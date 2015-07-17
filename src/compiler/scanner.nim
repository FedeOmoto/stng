import macros, strutils, pegs, token, bignum, ../base/stream, ../base/obj

type
  CharType = enum
    undefined
    eof
    alphabetic
    digit
    binary
    special
    separator

  SmalltalkScanner* = ref object
    stream: UnicodeStringStream
    buffer: UnicodeStringStream
    tokenStart: int
    currentCharacter: Rune
    characterType: CharType
    separatorsInLiterals*: bool
    extendedLiterals: bool
    comments: seq[array[0 .. 1, int]]
    nameSpaceCharacter: char
    line: Positive

macro declareClassificationTable: stmt =
  var ct: array[0 .. 255, CharType]

  proc initializeChars(s: string, to: CharType) =
    for c in s: ct[c.int] = to

  initializeChars("_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz",
                  alphabetic)
  initializeChars("0123456789", digit)
  initializeChars(r"!%&*+,-/<=>?@\~|", binary)
  initializeChars("().:;[]^", special)
  initializeChars("\0\t\n\v\f\r ", separator)

  result = parseStmt("const ClassificationTable = " & repr(ct))

declareClassificationTable()

proc initializeForStNG(scanner: SmalltalkScanner) =
  scanner.extendedLiterals = true
  scanner.nameSpaceCharacter = '.'
  scanner.line = 1

proc on(scanner: SmalltalkScanner, stream: UnicodeStringStream) =
  scanner.buffer = newUnicodeStringStream()
  scanner.stream = stream
  scanner.tokenStart = stream.getPosition
  scanner.comments = @[]
  scanner.initializeForStNG

proc classify(scanner: SmalltalkScanner, c: Rune): CharType =
  if c == (-1).Rune: return separator
  if c.int < 256: return ClassificationTable[c.int]

proc step(scanner: SmalltalkScanner): Rune =
  scanner.currentCharacter = scanner.stream.readRune

  if scanner.currentCharacter == (-1).Rune:
    scanner.characterType = eof
    return scanner.currentCharacter

  scanner.characterType = scanner.classify(scanner.currentCharacter)
  if scanner.currentCharacter == 10.Rune: scanner.line += 1

  return scanner.currentCharacter

proc recoverFromUnterminatedComment(scanner: SmalltalkScanner, i: int) =
  scanner.characterType = eof
  scanner.currentCharacter = (-1).Rune
  scanner.comments.add([i, scanner.stream.getPosition - 1])

proc unterminatedComment(scanner: SmalltalkScanner, i: int) =
  stderr.writeln("Error: Object>>unknown at line " & $scanner.line &
                 ": comment not closed")
  scanner.recoverFromUnterminatedComment(i)

proc stripComment(scanner: SmalltalkScanner) =
  let start = scanner.stream.getPosition - 1

  while true:
    if scanner.stream.atEnd:
      scanner.unterminatedComment(start)
      return

    let ch = scanner.stream.readRune
    if ch == 10.Rune: scanner.line += 1 elif ch == '"'.Rune: break

  let stop = scanner.stream.getPosition - 1
  discard scanner.step
  scanner.comments.add([start, stop])

proc stripSeparators(scanner: SmalltalkScanner) =
  while scanner.characterType == separator: discard scanner.step
  while scanner.currentCharacter == '"'.Rune: scanner.stripComment

proc scanName(scanner: SmalltalkScanner) =
  while scanner.characterType == alphabetic or scanner.characterType == digit:
    scanner.buffer.write($scanner.currentCharacter)
    discard scanner.step

proc scanNameSpaceName(scanner: SmalltalkScanner): StIdentifierToken =
  if scanner.nameSpaceCharacter == ':':
    if scanner.stream.peek != ':'.Rune: return nil
    scanner.buffer.write("::")
    discard scanner.step
  else:
    if scanner.stream.atEnd or scanner.classify(scanner.stream.peek) !=
       alphabetic: return nil
    scanner.buffer.write('.')

  discard scanner.step
  scanner.scanName

  if scanner.currentCharacter == scanner.nameSpaceCharacter.Rune:
    return scanner.scanNameSpaceName

  return newStIdentifierToken(newObject(scanner.buffer.contents),
                              scanner.tokenStart)

proc scanKeyword(scanner: SmalltalkScanner): StValueToken =
  var words, outputPosition, inputPosition: int

  while true:
    words += 1
    scanner.buffer.write($scanner.currentCharacter)
    outputPosition = scanner.buffer.getPosition
    inputPosition = scanner.stream.getPosition
    discard scanner.step
    if scanner.characterType != alphabetic: break
    scanner.scanName
    if scanner.currentCharacter != ':'.Rune: break

  # Restore to position immediately after last colon
  scanner.buffer.setPosition(outputPosition)
  scanner.stream.setPosition(inputPosition)
  discard scanner.step

  if words == 1:
    return newStKeywordToken(newObject(scanner.buffer.contents),
                             scanner.tokenStart)

  let name = scanner.buffer.contents

  return newStMultiKeywordLiteralToken(newObject(name, true),
                                       scanner.tokenStart,
                                       scanner.tokenStart + name.len - 1)

proc previousStepPosition*(scanner: SmalltalkScanner): int =
  if scanner.characterType == eof: return scanner.stream.getPosition - 1
  return scanner.stream.getPosition - 2

proc scanIdentifierOrKeyword(scanner: SmalltalkScanner): StValueToken =
  scanner.scanName

  if scanner.currentCharacter == scanner.nameSpaceCharacter.Rune:
    var token = scanner.scanNameSpaceName
    if not token.isNil: return token

  if scanner.currentCharacter == ':'.Rune and scanner.stream.peek != '='.Rune:
    return scanner.scanKeyword

  let name = scanner.buffer.contents

  case name:
    of "true":
      return newStLiteralToken(True, scanner.tokenStart,
                               scanner.previousStepPosition)
    of "false":
      return newStLiteralToken(False, scanner.tokenStart,
                               scanner.previousStepPosition)
    of "nil":
      return newStLiteralToken(nil, scanner.tokenStart,
                               scanner.previousStepPosition)
    else:
      return newStIdentifierToken(newObject(name), scanner.tokenStart)

proc scanBinary(scanner: SmalltalkScanner, tk: StValueToken): StValueToken =
  result = tk
  scanner.buffer.write($scanner.currentCharacter)
  discard scanner.step

  while scanner.characterType == binary and scanner.currentCharacter != '-'.Rune:
    scanner.buffer.write($scanner.currentCharacter)
    discard scanner.step

  result.value = newObject(scanner.buffer.contents, true)
  result.start = scanner.tokenStart

proc scanSpecialCharacter(scanner: SmalltalkScanner): StToken =
  if scanner.currentCharacter == ':'.Rune:
    discard scanner.step

    if scanner.currentCharacter == '='.Rune:
      discard scanner.step
      return newStAssignmentToken(scanner.tokenStart)

    return newStSpecialCharacterToken(newObject(':'.Rune), scanner.tokenStart)

  var character = scanner.currentCharacter
  discard scanner.step

  return newStSpecialCharacterToken(newObject(character), scanner.tokenStart)

proc readRadixInteger(scanner: SmalltalkScanner, base: int,
                      isNegative: bool): Object =
  let start = scanner.stream.getPosition

  while not scanner.stream.atEnd:
    var digitValue: int
    let c = scanner.stream.readRune.int

    if c >= '0'.int and c <= '9'.int:
      digitValue = c - '0'.int
    elif c >= 'A'.int and c <= 'Z'.int:
      digitValue = c - 55
    else:
      digitValue = -1

    if digitValue == -1 or digitValue >= base:
      scanner.stream.setPosition(scanner.stream.getPosition -
                                 c.Rune.toUTF8.runeLenAt(0))
      break

  let stop = scanner.stream.getPosition
  var s = scanner.stream.data.substr(start, stop - 1)
  if isNegative: s = '-' & s
  let i = newInt(s, base.cint)

  return if i.fitsInt: newObject(i.toInt) else: newObject(i)

proc readNumberSmalltalkSyntax(scanner: SmalltalkScanner): Object =
  let numberPeg = peg"({^{\-}?{\d\d?}{r}{[0-9A-Z]+}})/({^{{\-}?{\d+}{{\.}\d+}?}{{{{e}{\-/\+}?}/{s}}{\d+}}?})"
  var matches: array[0 .. 11, string]
  let start = scanner.stream.getPosition
  let numberStr = scanner.stream.data.substr(start)
  let numberLen = numberStr.matchLen(numberPeg, matches)

  scanner.stream.setPosition(start + numberLen)

  if matches[2] == "r" or matches[3] == "r":
    let negative = matches[1] == "-"
    var base = if negative: matches[2].parseInt else: matches[1].parseInt

    if base < 2 or base > 36:
      if negative:
        scanner.stream.setPosition(start + ($base).len + 1)
        base = -base
      else:
        scanner.stream.setPosition(start + ($base).len)

      return newObject(base)
    else:
      if negative:
        scanner.stream.setPosition(start + ($base).len + 2)
      else:
        scanner.stream.setPosition(start + ($base).len + 1)

      return scanner.readRadixInteger(base, negative)

  if matches[5] == "s" or matches[6] == "s" or matches[7] == "s" or
     matches[8] == "s":
    # TODO: ScaledDecimal
    return nil

  if matches[4] == "." or matches[5] == ".":
    return newObject(matches[0].parseFloat)

  let i = newInt(matches[0], 10)

  return if i.fitsInt: newObject(i.toInt) else: newObject(i)

proc readNumber(scanner: SmalltalkScanner): Object =
  scanner.stream.setPosition(scanner.stream.getPosition - 1)
  result = scanner.readNumberSmalltalkSyntax
  discard scanner.step

proc illegalNumber(scaner: SmalltalkScanner, start: int) =
  # TODO: implement
  discard

proc scanNumber(scanner: SmalltalkScanner): StNumberLiteralToken =
  let start = scanner.stream.getPosition - 1
  let number = try: scanner.readNumber
               except:
                 scanner.illegalNumber(start)
                 newObject(0)
  let stop = scanner.previousStepPosition

  return newStNumberLiteralToken(number, start, stop,
                                 scanner.stream.data.substr(start, stop))

proc literalErrorToken(scanner: SmalltalkScanner, value: Object,
                       start: int): StLiteralToken =
  newStLiteralToken(value, scanner.tokenStart, start)

proc recoverFromUnterminatedLiteral(scanner: SmalltalkScanner): StLiteralToken =
  scanner.literalErrorToken(newObject(scanner.buffer.contents),
                            scanner.previousStepPosition)

proc unterminatedString(scanner: SmalltalkScanner): StLiteralToken =
  stderr.writeln("Error: Object>>unknown at line " & $scanner.line &
                 ": literal string not closed")
  scanner.recoverFromUnterminatedLiteral

proc scanLiteralString(scanner: SmalltalkScanner): StLiteralToken =
  discard scanner.step

  while true:
    if scanner.currentCharacter == (-1).Rune: return scanner.unterminatedString
    if scanner.currentCharacter == '\''.Rune and scanner.step != '\''.Rune: break
    scanner.buffer.write($scanner.currentCharacter)
    discard scanner.step

  return newStLiteralToken(newObject(scanner.buffer.contents),
                           scanner.tokenStart, scanner.previousStepPosition)

proc scanSymbol(scanner: SmalltalkScanner): StLiteralToken =
  var hasColon: bool
  var lastPosition = scanner.stream.getPosition
  let startPosition = lastPosition

  while scanner.characterType == alphabetic:
    scanner.scanName

    if scanner.currentCharacter == ':'.Rune:
      scanner.buffer.write(':')
      hasColon = true
      lastPosition = scanner.stream.getPosition
      discard scanner.step

  var value = scanner.buffer.contents

  if hasColon and value[^1] != ':':
    scanner.stream.setPosition(lastPosition)
    discard scanner.step
    value = value.substr(0, lastPosition - startPosition)

  newStLiteralToken(newObject(value, true), scanner.tokenStart,
                    scanner.previousStepPosition)

proc scanStringSymbol(scanner: SmalltalkScanner): StLiteralToken =
  result = scanner.scanLiteralString
  result.value.kind = obj.symbol

proc scanLiteralArrayToken(scanner: SmalltalkScanner): StLiteralArrayToken =
  result = newStLiteralArrayToken(newObject("#" & $scanner.currentCharacter),
                                  scanner.tokenStart)
  discard scanner.step

proc scanQualifiedReference(scanner: SmalltalkScanner): StValueToken =
  # TODO: implement
  discard

proc scanOptimized(scanner: SmalltalkScanner): StOptimizedToken =
  discard scanner.step
  newStOptimizedToken(scanner.tokenStart, scanner.previousStepPosition)

proc extendedLiteralExpected(scanner: SmalltalkScanner): StLiteralToken =
  stderr.writeln("Error: Object>>unknown at line " & $scanner.line &
                 ": extended literal expected")
  scanner.recoverFromUnterminatedLiteral

proc scanExtendedLiterals(scanner: SmalltalkScanner): StToken =
  discard scanner.step
  if scanner.separatorsInLiterals: scanner.stripSeparators

  case scanner.characterType:
    of alphabetic:
      result = scanner.scanSymbol
    of binary:
      result = scanner.scanBinary(newStLiteralToken())
      result.stop = scanner.previousStepPosition
    else:
      if scanner.currentCharacter == '\''.Rune:
        result = scanner.scanStringSymbol
      elif scanner.currentCharacter == '('.Rune:
        return scanner.scanOptimized

  if result.isNil: return scanner.extendedLiteralExpected
  result.value.kind = obj.symbol

proc constantExpected(scanner: SmalltalkScanner): StLiteralToken =
  stderr.writeln("Error: Object>>unknown at line " & $scanner.line &
                 ": constant expected")
  scanner.recoverFromUnterminatedLiteral

proc scanLiteral(scanner: SmalltalkScanner): StToken =
  discard scanner.step
  if scanner.separatorsInLiterals: scanner.stripSeparators
  if scanner.characterType == alphabetic: return scanner.scanSymbol

  if scanner.characterType == binary:
    let tk = scanner.scanBinary(newStLiteralToken())
    tk.stop = scanner.previousStepPosition
    return tk

  if scanner.currentCharacter == '\''.Rune: return scanner.scanStringSymbol

  if scanner.currentCharacter == '('.Rune or scanner.currentCharacter == '['.Rune:
    return scanner.scanLiteralArrayToken

  if scanner.separatorsInLiterals and scanner.currentCharacter == '{'.Rune:
    return scanner.scanQualifiedReference

  if scanner.extendedLiterals and scanner.currentCharacter == '#'.Rune:
    return scanner.scanExtendedLiterals

  return scanner.constantExpected

proc recoverFromExpectChar(scanner: SmalltalkScanner): StLiteralToken =
  scanner.literalErrorToken(newUndefinedObject(), scanner.stream.getPosition - 1)

proc characterExpected(scanner: SmalltalkScanner): StLiteralToken =
  stderr.writeln("Error: Object>>unknown at line " & $scanner.line &
                 ": character expected")
  scanner.recoverFromExpectChar

proc atEnd*(scanner: SmalltalkScanner): bool =
  scanner.characterType == eof

proc scanLiteralCharacter(scanner: SmalltalkScanner): StLiteralToken =
  discard scanner.step
  if scanner.atEnd: return scanner.characterExpected
  result = newStLiteralToken(newObject(scanner.currentCharacter),
                             scanner.tokenStart, scanner.stream.getPosition - 1)
  discard scanner.step

proc recoverFromIllegalCharacter(scanner: SmalltalkScanner):
                                 StIllegalCharacterToken =
  let badChar = scanner.currentCharacter
  let start = scanner.tokenStart
  discard scanner.step
  newStIllegalCharacterToken(newObject(badChar), start)

proc illegalCharacter(scanner: SmalltalkScanner): StIllegalCharacterToken =
  stderr.writeln("Error: Object>>unknown at line " & $scanner.line &
                 ": illegal character '" & $scanner.currentCharacter & "'")
  scanner.recoverFromIllegalCharacter

proc newSmalltalkScanner*(on: UnicodeStringStream): SmalltalkScanner =
  new(result)
  result.on(on)
  discard result.step
  result.stripSeparators

proc scanToken*(scanner: SmalltalkScanner): StToken =
  if scanner.characterType == digit or (scanner.currentCharacter == '-'.Rune and
     scanner.classify(scanner.stream.peek) == digit): return scanner.scanNumber

  case scanner.characterType:
    of alphabetic: return scanner.scanIdentifierOrKeyword
    of binary: return scanner.scanBinary(newStBinarySelectorToken())
    of special: return scanner.scanSpecialCharacter
    else:
      if scanner.currentCharacter == '\''.Rune: return scanner.scanLiteralString
      if scanner.currentCharacter == '#'.Rune: return scanner.scanLiteral
      if scanner.currentCharacter == '$'.Rune: return scanner.scanLiteralCharacter

  return scanner.illegalCharacter

proc getComments*(scanner: SmalltalkScanner): seq[array[0 .. 1, int]] =
  if scanner.comments.len == 0: return nil
  result = scanner.comments
  scanner.comments = @[]

proc next*(scanner: SmalltalkScanner): StToken =
  scanner.buffer.setPosition(0)
  scanner.tokenStart = scanner.stream.getPosition

  if scanner.characterType == eof:
    result = newStToken(scanner.tokenStart)
  else:
    scanner.tokenStart -= scanner.stream.lastRuneLen
    result = scanner.scanToken

  scanner.stripSeparators
  result.comments = scanner.getComments


when isMainModule:
  var s: SmalltalkScanner
  var tk: StToken

  # classify
  s = newSmalltalkScanner(newUnicodeStringStream())
  assert s.classify((-1).Rune) == separator
  assert s.classify(256.Rune) == undefined

  # step
  assert s.step == (-1).Rune
  assert s.characterType == eof

  s = newSmalltalkScanner(newUnicodeStringStream("\n"))
  assert s.line == 2

  # stripSeparators
  s = newSmalltalkScanner(newUnicodeStringStream("    "))
  assert s.stream.getPosition == 4

  # stripComment
  s = newSmalltalkScanner(newUnicodeStringStream("\"comment\""))
  assert s.stream.getPosition == 9
  assert s.comments[0] == [0, 8]

  s = newSmalltalkScanner(newUnicodeStringStream("\"コメント\""))
  assert s.stream.getPosition == 14
  assert s.comments[0] == [0, 13]

  s = newSmalltalkScanner(newUnicodeStringStream("\"line1\nline2\""))
  assert s.line == 2

  # unterminatedComment / recoverFromUnterminatedComment
  s = newSmalltalkScanner(newUnicodeStringStream("\"unterminated comment"))
  assert s.characterType == eof
  assert s.currentCharacter == (-1).Rune
  assert s.comments[0] == [0, 20]

  # next
  s = newSmalltalkScanner(newUnicodeStringStream(""))
  assert s.next.start == 0

  s = newSmalltalkScanner(newUnicodeStringStream("."))
  discard s.next
  assert s.next.start == 1

  # getComments
  s = newSmalltalkScanner(newUnicodeStringStream("\"comment1\"\"comment2\""))
  let comments = s.next.comments
  assert comments[0] == [0, 9]
  assert comments[1] == [10, 19]

  # scanToken -> scanNumber / readNumber / readNumberSmalltalkSyntax /
  #              readRadixInteger
  s = newSmalltalkScanner(newUnicodeStringStream("37rFF"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 1
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == 37
  assert tk.source == "37"

  s = newSmalltalkScanner(newUnicodeStringStream("-37rFF"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 2
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == -37
  assert tk.source == "-37"

  s = newSmalltalkScanner(newUnicodeStringStream("16rFF"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 4
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == 255
  assert tk.source == "16rFF"

  s = newSmalltalkScanner(newUnicodeStringStream("-16rFF"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 5
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == -255
  assert tk.source == "-16rFF"

  s = newSmalltalkScanner(newUnicodeStringStream("16rFFFFFFFFFFFFFFFFFF"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 20
  assert tk.value.kind == largeInteger
  assert primitiveValue[Int](tk.value) == newInt("FFFFFFFFFFFFFFFFFF", 16)
  assert tk.source == "16rFFFFFFFFFFFFFFFFFF"

  s = newSmalltalkScanner(newUnicodeStringStream("-16rFFFFFFFFFFFFFFFFFF"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 21
  assert tk.value.kind == largeInteger
  assert primitiveValue[Int](tk.value) == newInt("-FFFFFFFFFFFFFFFFFF", 16)
  assert tk.source == "-16rFFFFFFFFFFFFFFFFFF"

  s = newSmalltalkScanner(newUnicodeStringStream("16rFG"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 3
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == 15
  assert tk.source == "16rF"

  s = newSmalltalkScanner(newUnicodeStringStream("-16rFG"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 4
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == -15
  assert tk.source == "-16rF"

  s = newSmalltalkScanner(newUnicodeStringStream("5.6789"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 5
  assert tk.value.kind == fp
  assert primitiveValue[float](tk.value) == 5.6789
  assert tk.source == "5.6789"

  s = newSmalltalkScanner(newUnicodeStringStream($int.high))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == ($int.high).len - 1
  assert tk.value.kind == smallInteger
  assert primitiveValue[int](tk.value) == int.high
  assert tk.source == $int.high

  s = newSmalltalkScanner(newUnicodeStringStream("123456789012345678901234567890"))
  tk = s.next
  assert tk of StNumberLiteralToken
  assert tk.start == 0
  assert tk.stop == 29
  assert tk.value.kind == largeInteger
  assert primitiveValue[Int](tk.value) == newInt("123456789012345678901234567890", 10)
  assert tk.source == "123456789012345678901234567890"

  # scanToken -> scanIdentifierOrKeyword / scanName / scanNameSpaceName /
  #              scanKeyword
  s = newSmalltalkScanner(newUnicodeStringStream("true"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 3
  assert tk.value.kind == boolean
  assert primitiveValue[bool](tk.value)

  s = newSmalltalkScanner(newUnicodeStringStream("false"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 4
  assert tk.value.kind == boolean
  assert primitiveValue[bool](tk.value) == false

  s = newSmalltalkScanner(newUnicodeStringStream("nil"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 2
  assert tk.value.isUndefinedObject

  s = newSmalltalkScanner(newUnicodeStringStream("identifier"))
  tk = s.next
  assert tk of StIdentifierToken
  assert tk.start == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "identifier"

  s = newSmalltalkScanner(newUnicodeStringStream("name.space.name"))
  tk = s.next
  assert tk of StIdentifierToken
  assert tk.start == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "name.space.name"

  s = newSmalltalkScanner(newUnicodeStringStream("keyword:"))
  tk = s.next
  assert tk of StKeywordToken
  assert tk.start == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "keyword:"

  s = newSmalltalkScanner(newUnicodeStringStream("multi:keyword:"))
  tk = s.next
  assert tk of StMultiKeywordLiteralToken
  assert tk.start == 0
  assert tk.stop == 13
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "multi:keyword:"

  # scanToken -> scanBinary
  s = newSmalltalkScanner(newUnicodeStringStream("--"))
  tk = s.next
  assert tk of StBinarySelectorToken
  assert tk.start == 0
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "-"

  s = newSmalltalkScanner(newUnicodeStringStream("+-"))
  tk = s.next
  assert tk of StBinarySelectorToken
  assert tk.start == 0
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "+"

  s = newSmalltalkScanner(newUnicodeStringStream("-="))
  tk = s.next
  assert tk of StBinarySelectorToken
  assert tk.start == 0
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "-="

  # scanToken -> scanSpecialCharacter
  s = newSmalltalkScanner(newUnicodeStringStream(":)"))
  tk = s.next
  assert tk of StSpecialCharacterToken
  assert tk.start == 0
  assert tk.value.kind == character
  assert primitiveValue[Rune](tk.value) == ':'.Rune

  s = newSmalltalkScanner(newUnicodeStringStream("."))
  tk = s.next
  assert tk of StSpecialCharacterToken
  assert tk.start == 0
  assert tk.value.kind == character
  assert primitiveValue[Rune](tk.value) == '.'.Rune

  s = newSmalltalkScanner(newUnicodeStringStream(":="))
  tk = s.next
  assert tk of StAssignmentToken
  assert tk.start == 0

  # scanToken -> scanLiteralString / unterminatedString /
  #              recoverFromUnterminatedLiteral
  s = newSmalltalkScanner(newUnicodeStringStream("'unterminated string"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 19
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "unterminated string"

  s = newSmalltalkScanner(newUnicodeStringStream("'string'"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 7
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "string"

  s = newSmalltalkScanner(newUnicodeStringStream("'文字列'"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 10
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "文字列"

  s = newSmalltalkScanner(newUnicodeStringStream("'string1''string2'"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 17
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "string1'string2"

  # scanToken -> scanLiteral / scanSymbol / scanStringSymbol /
  #              scanLiteralArrayToken / scanExtendedLiterals / scanOptimized /
  #              extendedLiteralExpected / constantExpected
  s = newSmalltalkScanner(newUnicodeStringStream("#symbol"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 6
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "symbol"

  s = newSmalltalkScanner(newUnicodeStringStream("#+="))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 2
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "+="

  s = newSmalltalkScanner(newUnicodeStringStream("#'stringSymbol'"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 14
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "stringSymbol"

  s = newSmalltalkScanner(newUnicodeStringStream("#("))
  tk = s.next
  assert tk of StLiteralArrayToken
  assert tk.start == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "#("

  s = newSmalltalkScanner(newUnicodeStringStream("#["))
  tk = s.next
  assert tk of StLiteralArrayToken
  assert tk.start == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == "#["

  s = newSmalltalkScanner(newUnicodeStringStream("##extendedLiteral"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 16
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "extendedLiteral"

  s = newSmalltalkScanner(newUnicodeStringStream("##!="))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 3
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "!="

  s = newSmalltalkScanner(newUnicodeStringStream("##'extendedStringSymbol'"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 23
  assert tk.value.kind == obj.symbol
  assert primitiveValue[string](tk.value) == "extendedStringSymbol"

  s = newSmalltalkScanner(newUnicodeStringStream("##("))
  tk = s.next
  assert tk of StOptimizedToken
  assert tk.start == 0
  assert tk.length == 3

  s = newSmalltalkScanner(newUnicodeStringStream("##"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 1
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == ""

  s = newSmalltalkScanner(newUnicodeStringStream("#"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == ""

  s = newSmalltalkScanner(newUnicodeStringStream("#."))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 0
  assert tk.value.kind == str
  assert primitiveValue[string](tk.value) == ""

  # scanToken -> scanLiteralCharacter / characterExpected
  s = newSmalltalkScanner(newUnicodeStringStream("$a"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 1
  assert tk.value.kind == character
  assert primitiveValue[Rune](tk.value) == 'a'.Rune

  s = newSmalltalkScanner(newUnicodeStringStream("$あ"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 3
  assert tk.value.kind == character
  assert primitiveValue[Rune](tk.value) == "あ".runeAt(0)

  s = newSmalltalkScanner(newUnicodeStringStream("$"))
  tk = s.next
  assert tk of StLiteralToken
  assert tk.start == 0
  assert tk.stop == 0
  assert tk.value.isUndefinedObject

  # illegalCharacter
  s = newSmalltalkScanner(newUnicodeStringStream("`"))
  tk = s.next
  assert tk of StIllegalCharacterToken
  assert tk.start == 0
  assert tk.value.kind == character
  assert primitiveValue[Rune](tk.value) == '`'.Rune

  s = newSmalltalkScanner(newUnicodeStringStream("る"))
  tk = s.next
  assert tk of StIllegalCharacterToken
  assert tk.start == 0
  assert tk.value.kind == character
  assert primitiveValue[Rune](tk.value) == "る".runeAt(0)

  assert s.next.start == "る".runeLenAt(0)
