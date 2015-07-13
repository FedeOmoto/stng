import strutils, unicode, ../base/obj

type
  StToken* = ref object of RootObj
    sourcePointer: int
    comments*: seq[array[0 .. 1, int]]

  StAssignmentToken* = ref object of StToken

  StOptimizedToken* = ref object of StToken
    stopPosition: int

  StValueToken* = ref object of StToken
    FValue: Object

  StBinarySelectorToken* = ref object of StValueToken

  StIdentifierToken* = ref object of StValueToken

  StIllegalCharacterToken* = ref object of StValueToken

  StKeywordToken* = ref object of StValueToken

  StLiteralArrayToken* = ref object of StValueToken

  StSpecialCharacterToken* = ref object of StValueToken

  StLiteralToken* = ref object of StValueToken
    stopPosition: int

  StMultiKeywordLiteralToken* = ref object of StLiteralToken

  StNumberLiteralToken* = ref object of StLiteralToken
    FSource: string

proc newStToken*(start: int): StToken =
  new(result)
  result.sourcePointer = start

proc toString(tk: StToken, name: string, indent: int): string =
  name & " --> [\n  " & indent.spaces & "sourcePointer: " &
    $tk.sourcePointer & ",\n  " & indent.spaces & "comments: " &
    repr(tk.comments)

method typeName(tk: StToken): string =
  if tk of StAssignmentToken:
    result = "StAssignmentToken"
  else:
    result = "StToken"

method `$`*(tk: StToken, indent: int = 0): string =
  tk.toString(tk.typeName, indent) & "]"

method start*(tk: StToken): int {.inline.} = tk.sourcePointer

method `start=`*(tk: StToken, i: int) {.inline.} = tk.sourcePointer = i

method isEof*(tk: StToken): bool = true

method isBinary*(tk: StToken): bool = false

method isBinary*(tk: StToken, s: string): bool = false

method isIdentifier*(tk: StToken): bool = false

method isKeyword*(tk: StToken): bool = false

method isLiteralToken*(tk: StToken): bool = false

method isLiteralArrayToken*(tk: StToken): bool = false

method isSpecial*(tk: StToken): bool = false

method isSpecial*(tk: StToken, c: Rune): bool = false

method isAssignment*(tk: StToken): bool = false

method isOptimized*(tk: StToken): bool = false

method length*(tk: StToken): int {.inline.} = 0

method value*(tk: StToken): Object {.inline.} =
  doesNotUnderstand(tk.typeName, "value")

method `value=`*(tk: StToken, o: Object) {.inline.} =
  doesNotUnderstand(tk.typeName, "value=")

method stop*(tk: StToken): int {.inline.} = tk.sourcePointer + tk.length - 1

method `stop=`*(tk: StToken, i: int) {.inline.} =
  doesNotUnderstand(tk.typeName, "stop=")

method source*(tk: StToken): string {.inline.} =
  doesNotUnderstand(tk.typeName, "source")

method `source=`*(tk: StToken, s: string) {.inline.} =
  doesNotUnderstand(tk.typeName, "source=")

proc newStAssignmentToken*(start: int): StAssignmentToken =
  new(result)
  result.sourcePointer = start

method isEof*(tk: StAssignmentToken): bool = false

method isAssignment*(tk: StAssignmentToken): bool = true

method length*(tk: StAssignmentToken): int {.inline.} = 2

proc newStOptimizedToken*(start: int, stop: int): StOptimizedToken =
  new(result)
  result.sourcePointer = start
  result.stopPosition = stop

method typeName(tk: StOptimizedToken): string =
  "StOptimizedToken"

method `$`*(tk: StOptimizedToken, indent: int = 0): string =
  cast[StToken](tk).toString(tk.typeName, indent) & "  " & indent.spaces &
    "stopPosition: " & $tk.stopPosition & "\n" & indent.spaces & "]"

method length*(tk: StOptimizedToken): int {.inline.} =
  tk.stopPosition - tk.sourcePointer + 1

method isEof*(tk: StOptimizedToken): bool = false

method isOptimized*(tk: StOptimizedToken): bool = true

proc toString(tk: StValueToken, name: string, indent: int): string =
  cast[StToken](tk).toString(name, indent) & "  " & indent.spaces & "value: " &
    `$`(tk.value, 2 + indent)

method typeName(tk: StValueToken): string =
  if tk of StBinarySelectorToken:
    result = "StBinarySelectorToken"
  elif tk of StIdentifierToken:
    result = "StIdentifierToken"
  elif tk of StIllegalCharacterToken:
    result = "StIllegalCharacterToken"
  elif tk of StKeywordToken:
    result = "StKeywordToken"
  elif tk of StLiteralArrayToken:
    result = "StLiteralArrayToken"
  elif tk of StSpecialCharacterToken:
    result = "StSpecialCharacterToken"
  else:
    result = "StValueToken"

method `$`*(tk: StValueToken, indent: int = 0): string =
  tk.toString(tk.typeName, indent) & "\n" & indent.spaces & "]"

method value*(tk: StValueToken): Object {.inline.} = tk.FValue

method `value=`*(tk: StValueToken, o: Object) {.inline.} = tk.FValue = o

method isEof*(tk: StValueToken): bool = false

method length*(tk: StValueToken): int {.inline.} = tk.value.size

proc newStBinarySelectorToken*: StBinarySelectorToken =
  new(result)

proc newStBinarySelectorToken*(value: Object, start: int): StBinarySelectorToken =
  new(result)
  result.value = value
  result.sourcePointer = start

method isBinary*(tk: StBinarySelectorToken): bool = true

method isBinary*(tk: StBinarySelectorToken, s: string): bool = primitiveValue[string](tk.value) == s

proc newStIdentifierToken*(value: Object, start: int): StIdentifierToken =
  new(result)
  result.value = value
  result.sourcePointer = start

method isIdentifier*(tk: StIdentifierToken): bool = true

proc newStIllegalCharacterToken*(value: Object, start: int):
                                 StIllegalCharacterToken =
  new(result)
  result.value = value
  result.sourcePointer = start

method length*(tk: StIllegalCharacterToken): int {.inline.} =
  `$`(primitiveValue[Rune](tk.value)).runeLenAt(0)

proc newStKeywordToken*(value: Object, start: int): StKeywordToken =
  new(result)
  result.value = value
  result.sourcePointer = start

method isKeyword*(tk: StKeywordToken): bool = true

proc newStLiteralArrayToken*(value: Object, start: int): StLiteralArrayToken =
  new(result)
  result.value = value
  result.sourcePointer = start

method isLiteralArrayToken*(tk: StLiteralArrayToken): bool = true

method isForByteArray*(tk: StLiteralArrayToken): bool =
  primitiveValue[string](tk.value)[^1] == '['

proc newStSpecialCharacterToken*(value: Object, start: int):
                                 StSpecialCharacterToken =
  new(result)
  result.value = value
  result.sourcePointer = start

method isSpecial*(tk: StSpecialCharacterToken): bool = true

method isSpecial*(tk: StSpecialCharacterToken, c: Rune): bool =
  primitiveValue[Rune](tk.value) == c

method length*(tk: StSpecialCharacterToken): int {.inline.} = 1

proc newStLiteralToken*: StLiteralToken =
  new(result)

proc newStLiteralToken*(value: Object, start, stop: int): StLiteralToken =
  new(result)
  result.value = value
  result.sourcePointer = start
  result.stopPosition = stop

proc toString(tk: StLiteralToken, name: string, indent: int): string =
  cast[StValueToken](tk).toString(name, indent) & ",\n  " & indent.spaces &
    "stopPosition: " & $tk.stopPosition

method typeName(tk: StLiteralToken): string =
  if tk of StMultiKeywordLiteralToken:
    result = "StMultiKeywordLiteralToken"
  else:
    result = "StLiteralToken"

method `$`*(tk: StLiteralToken, indent: int = 0): string =
  tk.toString(tk.typeName, indent) & "\n" & indent.spaces & "]"

method stop*(tk: StLiteralToken): int {.inline.} = tk.stopPosition

method `stop=`*(tk: StLiteralToken, i: int) {.inline.} = tk.stopPosition = i

method realValue*(tk: StLiteralToken): Object {.inline.} = tk.value

method isLiteralToken*(tk: StLiteralToken): bool = true

method isMultiKeyword*(tk: StLiteralToken): bool = false

method length*(tk: StLiteralToken): int {.inline.} =
  tk.stopPosition - tk.sourcePointer + 1

proc newStMultiKeywordLiteralToken*(value: Object, start, stop: int):
                                    StMultiKeywordLiteralToken =
  new(result)
  result.value = value
  result.sourcePointer = start
  result.stopPosition = stop

method isMultiKeyword*(tk: StMultiKeywordLiteralToken): bool = true

method source*(tk: StNumberLiteralToken): string {.inline.} = tk.FSource

method `source=`*(tk: StNumberLiteralToken, s: string) {.inline.} = tk.FSource = s

proc newStNumberLiteralToken*(value: Object, start, stop: int, source: string):
                              StNumberLiteralToken =
  new(result)
  result.value = value
  result.sourcePointer = start
  result.stopPosition = stop
  result.source = source

method typeName(tk: StNumberLiteralToken): string =
  "StNumberLiteralToken"

method `$`*(tk: StNumberLiteralToken, indent: int = 0): string =
  cast[StLiteralToken](tk).toString(tk.typeName, indent) & ",\n  " &
    indent.spaces & "source: " & $tk.source & "\n" & indent.spaces & "]"
