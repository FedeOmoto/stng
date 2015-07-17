import strutils, tables, bignum, scanner, token, ast, ../base/stream, ../base/obj

type
  SmalltalkParser* = ref object
    FScanner: SmalltalkScanner
    currentToken: StToken
    FNextToken: StToken
    emptyStatements: bool
    errorBlock: proc ()
    methodNode: StMethodNode
    st80Syntax: bool
    comments: seq[array[0 .. 1, int]]
    methodClass: Class
    source: string

  # TODO
  ExtCallArgTypes = enum
    ExtCallArgINVALID = 0
    ExtCallArgVOID = 1
    ExtCallArgLPVOID = 2
    ExtCallArgLPPVOID = 16
    ExtCallArgSTRUCT = 27

const MaxPrimIndex = 256
const MaxVfn = 1024

# TODO: align with LLVM CCs?
const ExternalCallingConventions = ["stdcall:", "cdecl:", "fastcall:", "thiscall:"]

# TODO
let ExternalValueTypes = newTable([("void*", ExtCallArgLPVOID)])

# TODO
let ExternalReferenceTypes = newTable([(ExtCallArgVOID.int + 1, ExtCallArgLPVOID),
                                       (ExtCallArgLPPVOID.int + 1, ExtCallArgLPPVOID)])

# Declarations
proc parseLiteralArray(parser: SmalltalkParser): StLiteralArrayNode
# TODO: Uncomment when https://github.com/nim-lang/Nim/issues/3055 is fixed
#proc parseStatementsOf(parser: SmalltalkParser, node: StBlockNode | StMethodNode | StOptimizedNode, allowTag: bool): StSequenceNode
proc parseStatementsOf(parser: SmalltalkParser, node: StProgramNode, allowTag: bool): StSequenceNode
proc parseOptimizedExpression(parser: SmalltalkParser): StOptimizedNode
proc parseAssignment(parser: SmalltalkParser): StValueNode

proc initialize*(parser: SmalltalkParser) =
  parser.comments = @[]

proc newSmalltalkParser*: SmalltalkParser =
  new(result)
  result.initialize

proc step(parser: SmalltalkParser) =
  if parser.currentToken != nil and parser.currentToken.comments != nil:
    parser.comments.add(parser.currentToken.comments)

  if parser.FNextToken != nil:
    parser.currentToken = parser.FNextToken
    parser.FNextToken = nil
  else:
    parser.currentToken = parser.FScanner.next

proc `scanner=`*(parser: SmalltalkParser, scanner: SmalltalkScanner) {.inline.} =
  parser.FScanner = scanner
  scanner.separatorsInLiterals = parser.st80Syntax
  parser.step

proc initializeParserWith(parser: SmalltalkParser, s: string) =
  parser.source = s
  parser.scanner = newSmalltalkScanner(newUnicodeStringStream(s))

proc addCommentsTo(parser: SmalltalkParser, node: StProgramNode) =
  if parser.comments.len == 0: return
  node.comments = node.comments & parser.comments
  parser.comments = @[]

proc parsePrimitiveIdentifier(parser: SmalltalkParser): StVariableNode =
  let token = parser.currentToken
  parser.step
  result = newStVariableNode(cast[StIdentifierToken](token))
  parser.addCommentsTo(result)

proc parseVariableNode(parser: SmalltalkParser): StVariableNode =
  if parser.currentToken.isIdentifier: return parser.parsePrimitiveIdentifier
  # TODO: print error?
  let token = newStIdentifierToken(newObject(parser.source.substr(
                                               parser.currentToken.start,
                                               parser.currentToken.stop)),
                                   parser.currentToken.start)
  parser.step
  result = newStVariableNode(token)
  parser.addCommentsTo(result)

proc isSpecialVariable(parser: SmalltalkParser, node: StVariableNode): bool =
  let identifier = node.name
  return "self" == identifier or "super" == identifier or "thisContext" == identifier

# TODO: Uncomment when https://github.com/nim-lang/Nim/issues/3055 is fixed
#proc validateTemp(parser: SmalltalkParser, node: StVariableNode, ofNode: StBlockNode | StMethodNode | StOptimizedNode, temps: seq[StVariableNode]): bool =
proc validateTemp(parser: SmalltalkParser, node: StVariableNode, ofNode: StProgramNode, temps: seq[StVariableNode]): bool =
  if temps.contains(node):
    # TODO: print error?
    discard
  else:
    if parser.isSpecialVariable(node):
      # TODO: print error?
      discard
    else:
      # TODO: Uncomment when https://github.com/nim-lang/Nim/issues/3055 is fixed
      #if ofNode.arguments.contains(node):
      #  # TODO: print error?
      #  discard

      return true

  return false

# TODO: Uncomment when https://github.com/nim-lang/Nim/issues/3055 is fixed
#proc parseTempsOf(parser: SmalltalkParser, node: StBlockNode | StMethodNode | StOptimizedNode): seq[StVariableNode] =
proc parseTempsOf(parser: SmalltalkParser, node: StProgramNode): seq[StVariableNode] =
  result = @[]

  while parser.currentToken.isIdentifier:
    let temp = parser.parseVariableNode
    if parser.validateTemp(temp, node, result): result.add(temp)

proc parseExternalCallingConvention(parser: SmalltalkParser): int =
  if parser.currentToken.isKeyword:
    result = ExternalCallingConventions.find(primitiveValue[string](parser.currentToken.value))

    if result == -1: result = 0 # TODO: default to stdcall?
    parser.step
    return

  # TODO: print error?
  discard

proc lookupExternalStructType(parser: SmalltalkParser, tk: StToken): Class =
  # TODO
  ObjectClass

proc patchExternalDescriptorClose(parser: SmalltalkParser): string =
  # Hack needed because scanner treats trailing *> and **> as binary selectors rather than two tokens.
  assert parser.FNextToken == nil
  result = primitiveValue[string](parser.currentToken.value)
  result = result.substr(0, result.high - 1)
  parser.FNextToken = parser.currentToken
  parser.currentToken = newStBinarySelectorToken(newObject(result, true), parser.FNextToken.start)
  parser.FNextToken.value = newObject(">", true)
  # TODO: result.len or result.high?
  parser.FNextToken.start = parser.FNextToken.start + result.len

proc parseExternalArgTypeQualifier(parser: SmalltalkParser): int =
  if not parser.currentToken.isBinary: return 0
  var qualifier = primitiveValue[string](parser.currentToken.value)
  if qualifier == ">": return 0
  if qualifier == "*>" or qualifier == "**>": qualifier = parser.patchExternalDescriptorClose
  if qualifier == "*": return 1
  if qualifier == "**": return 2

  # TODO: print error? / Support more than 2 indirections?
  parser.step

proc externalReferenceTypeFor(parser: SmalltalkParser,
                              valueType: ExtCallArgTypes): ExtCallArgTypes =
  ExternalReferenceTypes[valueType.int + 1]

proc parseExternalArgType(parser: SmalltalkParser): StExternalArgTypeNode =
  let start = parser.currentToken.start

  if not parser.currentToken.isIdentifier:
    # TODO: print error?
    parser.step
    result = newStExternalArgTypeNode()
    result.start = parser.currentToken.start
    result.stop = parser.currentToken.stop
    result.typeOrdinal = ExternalValueTypes["void*"].int
    result.indirections = 0
    return

  var structClass: Class
  var valueType = ExternalValueTypes[primitiveValue[string](parser.currentToken.value)]
  if valueType == ExtCallArgINVALID: valueType = ExtCallArgSTRUCT
  if valueType == ExtCallArgSTRUCT: structClass = parser.lookupExternalStructType(parser.currentToken)
  let typeToken = parser.currentToken
  parser.step
  var stop: int
  var indirections = parser.parseExternalArgTypeQualifier

  if indirections > 0:
    var refType = valueType

    if indirections > 1:
      if valueType == ExtCallArgSTRUCT:
        # TODO
        discard
      else:
        refType = parser.externalReferenceTypeFor(valueType)

    stop = parser.currentToken.stop

    if refType == ExtCallArgLPPVOID:
      # TODO: print error?
      indirections = 0

    parser.step
  else:
    stop = typeToken.stop

  result = newStExternalArgTypeNode()
  result.start = typeToken.start
  result.stop = stop
  result.typeOrdinal = valueType.int
  result.structClass = structClass

proc parseExternalCallName(parser: SmalltalkParser): Object =
  if parser.currentToken.isLiteralToken:
    let token = parser.currentToken.value

    if token.isInteger:
      if token.isSmallInteger:
        if primitiveValue[int](token) >= 0: result = token
      else:
        if primitiveValue[Int](token).positive: result = token
    elif token.isString:
      result = token
  else:
    if parser.currentToken.isIdentifier: result = parser.currentToken.value

  if result.isNil:
    # TODO: print error?
    result = newObject("")
  else:
    parser.step

proc atEnd*(parser: SmalltalkParser): bool =
  parser.currentToken.isEof

proc parseExternalCallArgList(parser: SmalltalkParser): seq[StExternalArgTypeNode] =
  result = @[]

  while not (parser.atEnd or parser.currentToken.isBinary(">")):
    let arg = parser.parseExternalArgType

    if arg.typeOrdinal == ExtCallArgVOID.int and arg.indirections == 0:
      # TODO: print error?
      discard

    result.add(arg)

proc parseExternalCallArgs(parser: SmalltalkParser, node: StExternalCallNode): seq[StExternalArgTypeNode] =
  #let start = parser.currentToken.start
  result = parser.parseExternalCallArgList
  node.argumentTypes = result
  let got = result.len
  let expected = parser.methodNode.argumentCount

  if got < expected:
    # TODO: print error?
    discard
  elif got > expected:
    # TODO: print error?
    discard

proc parseExternalCall(parser: SmalltalkParser, start: int, isVirtual: bool): StExternalCallNode =
  result = newStExternalCallNode()
  result.start = start
  result.callingConvention = parser.parseExternalCallingConvention
  result.returnType = parser.parseExternalArgType

  if isVirtual:
    result.isVirtual = true
    var vfnIndex = newObject(1)

    if not parser.currentToken.isLiteralToken or not parser.currentToken.value.isInteger:
      # TODO: print error?
      discard

    vfnIndex = parser.currentToken.value

    if primitiveValue[int](vfnIndex) notin (1 .. MaxVfn):
      # TODO: print error?
      discard

    result.nameOrOrdinal = vfnIndex
    parser.step
  else:
    result.nameOrOrdinal = parser.parseExternalCallName

  discard parser.parseExternalCallArgs(result)

proc parseOverlappedCall(parser: SmalltalkParser, start: int): StExternalCallNode =
  result = parser.parseExternalCall(start, false)
  result.isOverlapped = true

proc parseExtendedExternalCall(parser: SmalltalkParser, start: int): StExternalCallNode =
  if parser.currentToken.isIdentifier:
    let modifier = primitiveValue[string](parser.currentToken.value)

    if modifier == "virtual":
      parser.step
      return parser.parseExternalCall(start, true)
    elif modifier == "overlap":
      parser.step
      return parser.parseOverlappedCall(start)

  # TODO: print error?
  parser.step
  return parser.parseExternalCall(start, false)

proc parseTag(parser: SmalltalkParser, start: int): StTagNode =
  if parser.currentToken.isKeyword:
    if primitiveValue[string](parser.currentToken.value) == "primitive:":
      parser.step

      if not parser.currenttoken.isLiteralToken or not parser.currentToken.value.isInteger:
        # TODO: print error?
        if not parser.currentToken.isBinary(">"): parser.step
        result = newStPrimitiveNode(0)
        result.start = start
        return

      let primitiveIndex = primitiveValue[int](parser.currentToken.value)
      if primitiveIndex notin (1 .. MaxPrimIndex): discard  # TODO: print error?
      parser.step
      result = newStPrimitiveNode(primitiveIndex)
      result.start = start
    else:
      result = parser.parseExternalCall(start, false)
  else:
    result = parser.parseExtendedExternalCall(start)

proc parseTag(parser: SmalltalkParser) =
  if not parser.currentToken.isBinary("<"): return
  let start = parser.currentToken.start
  parser.step
  let tag = parser.parseTag(start)
  parser.methodNode.tag = tag

  if not parser.currentToken.isBinary(">"):
    # TODO: print error?
    tag.stop = parser.currentToken.start
    return

  tag.stop = parser.currentToken.start
  parser.step

proc parsePrimitiveLiteral(parser: SmalltalkParser): StLiteralNode =
  let token = parser.currentToken
  parser.step
  newStLiteralNode(cast[StLiteralToken](token))

proc isByte(o: Object): bool =
  if o.isInteger:
    let value = primitiveValue[int](o)
    return value >= 0 and value <= 255

  let value = primitiveValue[Int](o)
  return value.positive and value <= 255

proc parseLiteralByteArrayObject(parser: SmalltalkParser): StLiteralNode =
  if not (parser.currentToken.isLiteralToken and parser.currentToken.value.isInteger and parser.currentToken.value.isByte):
    # TODO: print error?
    let token = newStLiteralToken(newObject(0), parser.currentToken.start, parser.currentToken.stop)
    parser.step
    return newStLiteralNode(token)

  return parser.parsePrimitiveLiteral

proc parseLiteralByteArray(parser: SmalltalkParser): StLiteralArrayNode =
  let start = parser.currentToken.start
  var stop: int
  var contents: seq[StValueNode] = @[]
  parser.step

  while not (parser.atEnd or (parser.currentToken.isSpecial and primitiveValue[Rune](parser.currentToken.value) == ']'.Rune)):
    contents.add(parser.parseLiteralByteArrayObject)

  if parser.currentToken.isSpecial and primitiveValue[Rune](parser.currentToken.value) == ']'.Rune:
    stop = parser.currentToken.stop
  else:
    # TODO: print error?
    if contents.len == 0:
      stop = start + 1
    else:
      stop = stop(contents[^1])
      discard

  parser.step

  newStLiteralArrayNode(start, contents, stop, true)

proc patchLiteralArrayToken(parser: SmalltalkParser) =
  var value: string
  let tk = parser.currentToken

  if tk.isIdentifier or tk.isBinary or tk.isKeyword:
    value = primitiveValue[string](tk.value)
  else:
    value = parser.source.substr(tk.start, tk.stop)

  parser.currentToken = newStLiteralToken(newObject(value, true), tk.start, tk.stop)

proc parseLiteralArrayObject(parser: SmalltalkParser): StValueNode =
  if parser.currentToken.isSpecial:
    let tkValue = primitiveValue[Rune](parser.currentToken.value)
    if tkValue == '('.Rune: return parser.parseLiteralArray
    if tkValue == '['.Rune: return parser.parseLiteralByteArray

  if parser.currentToken.isLiteralArrayToken:
    if cast[StLiteralArrayToken](parser.currentToken).isForByteArray:
      return parser.parseLiteralByteArray

    return parser.parseLiteralArray

  if parser.currentToken.isOptimized: return parser.parseOptimizedExpression
  if not parser.currentToken.isLiteralToken: parser.patchLiteralArrayToken

  return parser.parsePrimitiveLiteral

proc evaluateStatements(parser: SmalltalkParser, stmts: StSequenceNode): Object =
  # TODO
  newUndefinedObject()

proc parseOptimizedExpression(parser: SmalltalkParser): StOptimizedNode =
  let position = parser.currentToken.start
  parser.step
  result = newStOptimizedNode()
  let body = parser.parseStatementsOf(result, false)
  let isClosed = parser.currentToken.isSpecial(')'.Rune)
  result.left = position
  result.body = body
  result.right = if isClosed: parser.currentToken.start else: parser.FScanner.previousStepPosition

  if not isClosed:
    # TODO: print error?
    discard

  result.value = parser.evaluateStatements(body)
  parser.step

proc parseLiteralArray(parser: SmalltalkParser): StLiteralArrayNode =
  let start = parser.currentToken.start
  var contents: seq[StValueNode] = @[]
  parser.step

  while not (parser.atEnd or (parser.currentToken.isSpecial and primitiveValue[Rune](parser.currentToken.value) == ')'.Rune)):
    contents.add(parser.parseLiteralArrayObject)

  if not (parser.currentToken.isSpecial and primitiveValue[Rune](parser.currentToken.value) == ')'.Rune):
    # TODO: print error?
    discard

  let stop = parser.currentToken.stop
  parser.step

  return newStLiteralArrayNode(start, contents, stop, false)

proc validateArg(parser: SmalltalkParser, arg: StVariableNode, args: seq[StVariableNode]): bool =
  if args.contains(arg):
    # TODO: print error?
    discard
  else:
    if parser.isSpecialVariable(arg):
      # TODO: print error?
      discard
    else:
      return true

  return false

proc parseBlockArgsInto(parser: SmalltalkParser, node: StBlockNode): StBlockNode =
  var args: seq[StVariableNode] = @[]
  var colons: seq[int] = @[]
  var verticalBar: bool
  result = node

  while parser.currentToken.isSpecial(':'.Rune):
    colons.add(parser.currentToken.start)
    parser.step
    verticalBar = true
    let arg = parser.parseVariableNode
    if parser.validateArg(arg, args): args.add(arg)

  if verticalBar:
    if parser.currentToken.isBinary:
      result.bar = parser.currentToken.start

      if primitiveValue[string](parser.currentToken.value) == "|":
        parser.step
      else:
        if primitiveValue[string](parser.currentToken.value) == "||":
          # Hack the current token to be the start of temps bar
          parser.currentToken.value = newObject("|", true)
          parser.currentToken.start = parser.currentToken.start + 1
        else:
          # TODO: print error?
          discard
    else:
      # St-80 allows, for example, [:a] as a valid block, ANSI requires the arg
      # list termination bar, i.e. [:a|]
      if not parser.st80Syntax and parser.currentToken.isSpecial(']'.Rune):
        # TODO: print error?
        discard

  result.arguments = args
  result.colons = colons

proc parseBlock(parser: SmalltalkParser): StBlockNode =
  let position = parser.currentToken.start
  parser.step
  result = newStBlockNode()
  result.left = position
  discard parser.parseBlockArgsInto(result)
  result.body = parser.parseStatementsOf(result, false)

  if not parser.currentToken.isSpecial(']'.Rune):
    # TODO: print error?
    var blockEnd = result.body.stop

    if blockEnd == 0:
      if result.bar == 0: blockEnd = position

    result.right = blockEnd
    return

  result.right = parser.currentToken.start
  parser.step

proc errorPosition*(parser: SmalltalkParser): int = parser.currentToken.start

proc parseParenthesizedExpression(parser: SmalltalkParser): StValueNode =
  let leftParen = parser.currentToken.start
  var rightParen: int
  parser.step
  result = parser.parseAssignment

  if parser.currentToken.isSpecial(')'.Rune):
    rightParen = parser.currentToken.start
    parser.step
  else:
    # TODO: print error?
    rightParen = parser.errorPosition - 1

  result.addParenthesis([leftParen, rightParen])

proc parsePrimitiveObject(parser: SmalltalkParser): StValueNode =
  if parser.currentToken.isIdentifier: return parser.parsePrimitiveIdentifier

  if parser.currentToken.isLiteralToken and not cast[StLiteralToken](parser.currentToken).isMultiKeyword:
    return parser.parsePrimitiveLiteral

  if parser.currentToken.isLiteralArrayToken:
    if cast[StLiteralArrayToken](parser.currentToken).isForByteArray:
      return parser.parseLiteralByteArray
    else:
      return parser.parseLiteralArray

  if parser.currentToken.isSpecial:
    if primitiveValue[Rune](parser.currentToken.value) == '['.Rune: return parser.parseBlock
    if primitiveValue[Rune](parser.currentToken.value) == '('.Rune: return parser.parseParenthesizedExpression

  if parser.currentToken.isOptimized: return parser.parseOptimizedExpression

  # TODO: print error?
  let token = newStLiteralToken(newObject(parser.source.substr(
                                            parser.currentToken.start,
                                            parser.currentToken.stop)),
                                parser.currentToken.start,
                                parser.currentToken.stop)
  parser.step
  result = newStLiteralNode(token)
  parser.addCommentsTo(result)

proc patchLiteralMessage(parser: SmalltalkParser): StIdentifierToken =
  let value = parser.currentToken.value

  if value.isBoolean:
    parser.currentToken = newStIdentifierToken(newObject($primitiveValue[bool](value)), parser.currentToken.start)

  if value.isUndefinedObject:
    parser.currentToken = newStIdentifierToken(newObject("nil"), parser.currentToken.start)

  return cast[StIdentifierToken](parser.currentToken)

proc parseUnaryMessageWith(parser: SmalltalkParser, node: StValueNode): StMessageNode =
  let selector = parser.currentToken
  parser.step
  newStMessageNode(node, @[cast[StValueToken](selector)], @[])

proc parseUnaryMessage(parser: SmalltalkParser): StValueNode =
  result = parser.parsePrimitiveObject
  parser.addCommentsTo(result)

  while true:
    if parser.currentToken.isLiteralToken: discard parser.patchLiteralMessage

    if parser.currentToken.isIdentifier:
      result = parser.parseUnaryMessageWith(result)
    else:
      break

  parser.addCommentsTo(result)

proc patchNegativeLiteral(parser: SmalltalkParser) =
  # Handle the special negative number case for binary message sends.
  let tkValue = parser.currentToken.value
  if not tkValue.isNumber: return
  var eq0: bool
  var tkValueNegated: Object

  case tkValue.kind:
    of fp:
      let primValue = primitiveValue[float](tkValue)
      if not (primValue <= 0): return
      eq0 = primValue == 0
      tkValueNegated = newObject(-primValue)
    of fraction:
      let primValue = primitiveValue[Rat](tkValue)
      if not (primValue <= 0): return
      eq0 = primValue == 0
      tkValueNegated = newObject(-primValue)
    of smallInteger:
      let primValue = primitiveValue[int](tkValue)
      if not (primValue <= 0): return
      eq0 = primValue == 0
      tkValueNegated = newObject(-primValue)
    of largeInteger:
      let primValue = primitiveValue[Int](tkValue)
      if not (primValue <= 0): return
      eq0 = primValue == 0
      tkValueNegated = newObject(-primValue)
    of scaledDecimal:
      # TODO
      #let primValue = primitiveValue[](tkValue)
      #if not (primValue <= 0): return
      #eq0 = primValue == 0
      #tkValueNegated = newObject(-primValue)
      discard
    else: discard

  if eq0:
    if not (parser.source.isNil and parser.source != "" and parser.source[parser.currentToken.start.min(parser.source.len)] == '-'):
      return

  parser.FNextToken = parser.currentToken
  parser.currentToken = newStBinarySelectorToken(newObject("-", true), parser.FNextToken.start)
  parser.FNextToken.value = tkValueNegated

  if parser.FNextToken of StNumberLiteralToken:
    parser.FNextToken.source = parser.FNextToken.source.substr(1, parser.FNextToken.source.high)

  parser.FNextToken.start = parser.FNextToken.start + 1

proc parseBinaryMessageWith(parser: SmalltalkParser, node: StValueNode): StMessageNode =
  let binaryToken = parser.currentToken
  parser.step
  newStMessageNode(node, @[cast[StValueToken](binaryToken)], @[parser.parseUnaryMessage])

proc parseBinaryMessage(parser: SmalltalkParser): StValueNode =
  result = parser.parseUnaryMessage

  while true:
    if parser.currentToken.isLiteralToken: parser.patchNegativeLiteral

    if parser.currentToken.isBinary:
      result = parser.parseBinaryMessageWith(result)
    else:
      break

proc parseKeywordMessageWith(parser: SmalltalkParser, node: StValueNode): StValueNode =
  var args: seq[StValueNode] = @[]
  var keywords: seq[StValueToken] = @[]
  var isKeyword: bool
  result = node

  if parser.currentToken.isKeyword:
    keywords.add(cast[StValueToken](parser.currentToken))
    parser.step
    args.add(parser.parseBinaryMessage)
    isKeyword = true

  if isKeyword:
    return newStMessageNode(result, keywords, args)
  else:
    if result.isVariable:
      if cast[StVariableNode](result).name == "super":
        # TODO: print error?
        discard

proc parseKeywordMessage(parser: SmalltalkParser): StValueNode =
  parser.parseKeywordMessageWith(parser.parseBinaryMessage)

proc parseCascadedMessageFor(parser: SmalltalkParser, node: StValueNode): StValueNode =
  if parser.currentToken.isIdentifier: return parser.parseUnaryMessageWith(node)
  if parser.currentToken.isKeyword: return parser.parseKeywordMessageWith(node)
  if parser.currentToken.isLiteralToken: parser.patchNegativeLiteral

  if parser.currentToken.isBinary:
     # TODO: print error?
     return nil

  return parser.parseBinaryMessageWith(node)

proc parseCascadeMessage(parser: SmalltalkParser): StValueNode =
  result = parser.parseKeywordMessage

  if parser.currentToken.isKeyword: result = parser.parseKeywordMessageWith(result)
  if parser.currentToken.isBinary: result = parser.parseBinaryMessageWith(result)
  if parser.currentToken.isIdentifier: result = parser.parseUnaryMessageWith(result)

  while parser.currentToken.isSpecial(';'.Rune) and result.isMessage:
    let receiver = cast[StMessageNode](result).receiver
    var messages: seq[StMessageNode] = @[]
    var semicolons: seq[int] = @[]
    messages.add(cast[StMessageNode](result))

    while parser.currentToken.isSpecial(';'.Rune):
      semicolons.add(parser.currentToken.start)
      parser.step
      let msg = parser.parseCascadedMessageFor(receiver)
      if not msg.isNil: messages.add(cast[StMessageNode](msg))

    result = newStCascadeNode(messages, semicolons)

proc nextToken(parser: SmalltalkParser): StToken =
  if parser.FNextToken.isNil: parser.FNextToken = parser.FScanner.next
  parser.FNextToken

proc parseAssignment(parser: SmalltalkParser): StValueNode =
  # Need one token lookahead to see if we have a ":=". This method could make it
  # possible to assign the literals true, false and nil.
  if not (parser.currentToken.isIdentifier and parser.nextToken.isAssignment):
    return parser.parseCascadeMessage

  let node = parser.parseVariableNode
  let position = parser.currentToken.start
  parser.step

  return newStAssignmentNode(node, parser.parseAssignment, position)

proc canHaveStatementsAfterReturn*(parser: SmalltalkParser): bool = true

proc parseStatementList(parser: SmalltalkParser, allowTag: bool,
                        into: StSequenceNode): StSequenceNode =
  var stmtEnd: bool
  var statements: seq[StProgramNode] = @[]
  var periods: seq[int] = @[]
  var firstReturn = -1

  parser.addCommentsTo(into)
  if allowTag: parser.parseTag

  while not (parser.atEnd or (parser.currentToken.isSpecial and "])}".contains(primitiveValue[Rune](parser.currentToken.value).char))):
    var node: StProgramNode

    if stmtEnd:
      # TODO: print error?
      stmtEnd = false

    if parser.currentToken.isSpecial('^'.Rune):
      let returnPosition = parser.currentToken.start
      parser.step
      node = newStReturnNode(returnPosition, parser.parseAssignment)
      statements.add(node)
      if firstReturn == -1: firstReturn = statements.len
      stmtEnd = not parser.canHaveStatementsAfterReturn
    else:
      node = parser.parseAssignment
      statements.add(node)

    if parser.currentToken.isSpecial('.'.Rune):
      periods.add(parser.currentToken.start)
      parser.step
      parser.addCommentsTo(node)
    else:
      # An unterminated statement can only occur at the end of a statement list
      stmtEnd = true

    if parser.emptyStatements:
      while parser.currentToken.isSpecial('.'.Rune):
        periods.add(parser.currentToken.start)
        parser.step

  if firstReturn != -1 and firstReturn < statements.len:
    # TODO: print error?
    discard

  if statements.len != 0: parser.addCommentsTo(statements[^1])
  result = into
  result.statements = statements
  result.periods = periods

# TODO: Uncomment when https://github.com/nim-lang/Nim/issues/3055 is fixed
#proc parseStatementsOf(parser: SmalltalkParser, node: StBlockNode | StMethodNode | StOptimizedNode, allowTag: bool): StSequenceNode =
proc parseStatementsOf(parser: SmalltalkParser, node: StProgramNode, allowTag: bool): StSequenceNode =
  var temps: seq[StVariableNode] = @[]
  var leftBar, rightBar: int

  if parser.currentToken.isBinary:
    if primitiveValue[string](parser.currentToken.value) == "|":
      leftBar = parser.currentToken.start
      parser.step
      temps = parser.parseTempsOf(node)

      if parser.currentToken.isBinary("|"):
        rightBar = parser.currentToken.start
        parser.step
      else:
        # TODO: print error?
        discard
    else:
      if primitiveValue[string](parser.currentToken.value) == "||":
        leftBar = parser.currentToken.start
        rightBar = leftBar + 1
        parser.step

  parser.parseStatementList(allowTag, newStSequenceNode(leftBar, temps, rightBar))

proc parseExpression(parser: SmalltalkParser, s: string): StSequenceNode =
  let mth = newStMethodNode(cast[seq[StValueToken]](@[newStIdentifierToken(newObject("doIt"), 0)]), @[])
  mth.source = s
  result = parser.parseStatementsOf(mth, false)
  mth.body = result

  if not parser.atEnd:
    # TODO: print error?
    discard

proc parseExpression*(s: string, inClass: Class, onError: proc ()): StProgramNode =
  let parser = newSmalltalkParser()
  parser.errorBlock = onError
  parser.methodClass = inClass
  parser.initializeParserWith(s)
  let node = parser.parseExpression(s)

  if node.statements.len == 1 and node.temporaries.len == 0:
    return node.statements[0]
  else:
    return node

proc parseExpression*(s: string, onError: proc () = nil): StProgramNode =
  parseExpression(s, ObjectClass, onError)

proc parseUnaryPattern(parser: SmalltalkParser): StMethodNode =
  let selector = parser.currentToken
  parser.step
  newStMethodNode(@[cast[StValueToken](selector)], @[])

proc parseKeywordPattern(parser: SmalltalkParser): StMethodNode =
  var keywords: seq[StValueToken] = @[]
  var args: seq[StVariableNode] = @[]

  while parser.currentToken.isKeyword:
    keywords.add(cast[StValueToken](parser.currentToken))
    parser.step
    let arg = parser.parseVariableNode
    if parser.validateArg(arg, args): args.add(arg)

  result = newStMethodNode(keywords, args)
  result.comments = result.comments & args[^1].comments
  args[^1].comments = nil

proc parseBinaryPattern(parser: SmalltalkParser): StMethodNode =
  let binaryToken = parser.currentToken
  parser.step
  let args: seq[StVariableNode] = @[parser.parseVariableNode]
  result = newStMethodNode(@[cast[StValueToken](binaryToken)], args)
  result.comments = result.comments & args[^1].comments
  args[^1].comments = nil

proc parseMessagePattern(parser: SmalltalkParser): StMethodNode =
  if parser.currentToken.isLiteralToken: discard parser.patchLiteralMessage

  if parser.currentToken.isIdentifier:
    return parser.parseUnaryPattern
  elif parser.currentToken.isKeyword:
    return parser.parseKeywordPattern
  elif parser.currentToken.isBinary:
    return parser.parseBinaryPattern
  else:
    # TODO: print error?
    return newStMethodNode(@[cast[StValueToken](newStIdentifierToken(newObject(""), 0))], @[])

proc parseMethod(parser: SmalltalkParser): StMethodNode =
  parser.methodNode = parser.parseMessagePattern
  parser.addCommentsTo(parser.methodNode)
  parser.methodNode.body = parser.parseStatementsOf(parser.methodNode, true)
  return parser.methodNode

proc parseMethod*(parser: SmalltalkParser, s: string): StMethodNode =
  result = parser.parseMethod

  if not parser.atEnd:
    # TODO: print error?
    discard

  result.source = s

proc parseMethod*(parser: SmalltalkParser, s: string, inClass: Class,
                  errorBlock: proc()): StMethodNode =
  parser.errorBlock = errorBlock
  parser.methodClass = inClass
  parser.initializeParserWith(s)
  parser.parseMethod(s)

proc parseMethod*(s: string, inClass: Class, onError: proc () = nil): StMethodNode =
  newSmalltalkParser().parseMethod(s, inClass, onError)

proc parseMethod*(s: string): StMethodNode =
  parseMethod(s, UndefinedObjectClass)
