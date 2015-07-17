import strutils, token, ../base/obj

type
  StProgramNode* = ref object of RootObj
    parent: StProgramNode
    FComments*: seq[array[0 .. 1, int]]

  StExternalArgTypeNode* = ref object of StProgramNode
    typeOrdinal*: int
    structClass*: Class
    indirections*: int
    start*: int
    FStop: int

  StMethodNode* = ref object of StProgramNode
    FSelector: string
    selectorParts: seq[StValueToken]
    FBody: StSequenceNode
    source*: string
    FArguments*: seq[StVariableNode]
    tag*: StTagNode

  StReturnNode* = ref object of StProgramNode
    returnPosition: int
    FValue: StValueNode

  StSequenceNode* = ref object of StProgramNode
    leftBar: int
    rightBar: int
    FStatements: seq[StProgramNode]
    periods*: seq[int]
    FTemporaries: seq[StVariableNode]

  StTagNode* = ref object of StProgramNode
    start*: int
    FStop: int

  StExternalCallNode* = ref object of StTagNode
    nameOrOrdinal*: Object
    returnType*: StExternalArgTypeNode
    argumentTypes*: seq[StExternalArgTypeNode]
    isOverlapped*: bool
    isVirtual*: bool
    callingConvention*: int

  StPrimitiveNode* = ref object of StTagNode
    primitiveIndex: int

  StValueNode* = ref object of StProgramNode
    parentheses: seq[array[0 .. 1, int]]

  StAssignmentNode* = ref object of StValueNode
    FVariable: StVariableNode
    assignment*: int
    FValue: StValueNode

  StBlockNode* = ref object of StValueNode
    left*: int
    right*: int
    colons*: seq[int]
    FBody: StSequenceNode
    FArguments: seq[StVariableNode]
    bar*: int

  StCascadeNode* = ref object of StValueNode
    FMessages: seq[StMessageNode]
    semicolons*: seq[int]

  StLiteralNode* = ref object of StValueNode

  StMessageNode* = ref object of StValueNode
    FReceiver: StValueNode
    FSelector: string
    selectorParts: seq[StValueToken]
    FArguments: seq[StValueNode]

  StLiteralArrayNode* = ref object of StLiteralNode
    isByteArray: bool
    FStop: int
    FContents: seq[StValueNode]
    FStart: int

  StLiteralValueNode* = ref object of StLiteralNode
    token: StLiteralToken

  StOptimizedNode* = ref object of StValueNode
    left*: int
    right*: int
    FBody: StSequenceNode
    value*: Object

  StVariableNode* = ref object of StValueNode
    token: StIdentifierToken

# Declarations
method `$`*(node: StProgramNode, indent: int = 0, expandParent: bool = true): string

method typeName(node: StProgramNode): string =
  if node of StExternalArgTypeNode:
    "StExternalArgTypeNode"
  elif node of StMethodNode:
    "StMethodNode"
  elif node of StReturnNode:
    "StReturnNode"
  elif node of StSequenceNode:
    "StSequenceNode"
  elif node of StExternalCallNode:
    "StExternalCallNode"
  elif node of StPrimitiveNode:
    "StPrimitiveNode"
  elif node of StTagNode:
    "StTagNode"
  elif node of StAssignmentNode:
    "StAssignmentNode"
  elif node of StBlockNode:
    "StBlockNode"
  elif node of StCascadeNode:
    "StCascadeNode"
  elif node of StMessageNode:
    "StMessageNode"
  elif node of StOptimizedNode:
    "StOptimizedNode"
  elif node of StVariableNode:
    "StVariableNode"
  elif node of StLiteralArrayNode:
    "StLiteralArrayNode"
  elif node of StLiteralValueNode:
    "StLiteralValueNode"
  elif node of StLiteralNode:
    "StLiteralNode"
  elif node of StValueNode:
    "StValueNode"
  else:
    "StProgramNode"

proc toString(node: StProgramNode, name: string, indent: int,
              expandParent: bool): string =
  var parent: string

  if node.parent.isNil:
    parent = "nil"
  else:
    parent = if expandParent: `$`(node.parent, indent) else: node.parent.typeName

  name & " --> [\n" & indent.spaces & "parent: " & parent & ",\n" &
    indent.spaces & "comments: " & repr(node.FComments)

method `$`*(node: StProgramNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & indent.spaces & "]"

proc comments*(node: StProgramNode): seq[array[0 .. 1, int]] =
  return if node.FComments.isNil: @[] else: node.FComments

proc `comments=`*(node: StProgramNode, comments: seq[array[0 .. 1, int]]) =
  node.FComments = comments

method stop*(node: StProgramNode): int {.inline.} =
  subclassResponsibility("stop")

method isVariable*(node: StProgramNode): bool = false

method isMessage*(node: StProgramNode): bool = false

method isBlock*(node: StProgramNode): bool = false

method isMethod*(node: StProgramNode): bool = false

proc newStExternalArgTypeNode*: StExternalArgTypeNode =
  new(result)

proc toString(node: StExternalArgTypeNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StProgramNode](node).toString(name, indent, expandParent) &
    indent.spaces &
    "typeOrdinal: " & $node.typeOrdinal & ",\n" & indent.spaces &
    # TODO
    "structClass: " & (if node.structClass.isNil: "nil" else: "ObjectClass") &
      ",\n" & indent.spaces &
    "indirections: " & $node.indirections & ",\n" & indent.spaces &
    "start: " & $node.start & ",\n" & indent.spaces &
    "stop: " & $node.FStop

method `$`*(node: StExternalArgTypeNode, indent: int = 0,
            expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stop*(node: StExternalArgTypeNode): int {.inline.} = node.FStop

method `stop=`*(node: StExternalArgTypeNode, i: int) {.inline.} = node.FStop = i

proc `arguments`*(node: StMethodNode): seq[StVariableNode] = node.FArguments

proc `arguments=`*(node: StMethodNode, args: seq[StVariableNode]) =
  node.FArguments = args
  for arg in args: arg.parent = node

proc newStMethodNode*(selectorParts: seq[StValueToken],
                      arguments: seq[StVariableNode]): StMethodNode =
  new(result)
  result.selectorParts = selectorParts
  result.arguments = arguments

proc body*(node: StMethodNode): StSequenceNode = node.FBody

proc `body=`*(node: StMethodNode, body: StSequenceNode) =
  node.FBody = body
  body.parent = node

proc argumentCount*(node: StMethodNode): int =
  node.FArguments.len

proc buildSelector(node: StMethodNode | StMessageNode): string =
  result = ""

  for selectorPart in node.selectorParts:
    result &= primitiveValue[string](selectorPart.value)

proc selector*(node: StMethodNode | StMessageNode): string =
  if node.FSelector.isNil: node.FSelector = node.buildSelector
  node.FSelector

proc toString(collection: seq[StValueToken], indent: int): string =
  result = "@["

  for item in collection:
    result &= "\n" & (indent + 2).spaces & `$`(item, indent + 2)

  if not collection.isNil and not (collection.len == 0):
    result &= "\n" & indent.spaces

  result &= "]"

proc toString[T](collection: seq[T], indent: int): string =
  result = "@["

  for item in collection:
    result &= "\n" & (indent + 2).spaces & `$`(item, indent + 2, false)

  if not collection.isNil and not (collection.len == 0):
    result &= "\n" & indent.spaces

  result &= "]"

proc toString(node: StMethodNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StProgramNode](node).toString(name, indent, expandParent) &
    indent.spaces &
    "selector: " & node.selector & ",\n" & indent.spaces &
    "selectorParts: " & node.selectorParts.toString(indent) & ",\n" &
      indent.spaces &
    "body: " & `$`(node.body, indent, false) & ",\n" & indent.spaces &
    "source: " & node.source & ",\n" & indent.spaces &
    "arguments: " & node.FArguments.toString(indent) & ",\n" & indent.spaces &
    "tag: " & (if node.tag.isNil: "nil" else: `$`(node.tag, indent))

method `$`*(node: StMethodNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

proc `==`*(node1: StMethodNode, node2: StMethodNode): bool =
  if node1[].addr == node2[].addr: return true
  if not (node1.selector == node2.selector and node1.body == node2.body): return false

  for i in 0 .. node1.arguments.high:
    if not (node1.arguments[i] == node2.arguments[i]): return false

  return node1.tag == node2.tag

method stop*(node: StMethodNode): int {.inline.} = node.source.len

method isMethod*(node: StMethodNode): bool = true

proc `value=`*(node: StReturnNode, value: StValueNode) =
  node.FValue = value
  value.parent = node

proc newStReturnNode*(returnPosition: int, value: StValueNode): StReturnNode =
  new(result)
  result.returnPosition = returnPosition
  result.`value=`(value)

proc toString(node: StReturnNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StProgramNode](node).toString(name, indent, expandParent) &
    indent.spaces &
    "returnPosition: " & $node.returnPosition & ",\n" & indent.spaces &
    "value: " & `$`(node.FValue, indent, false)

method `$`*(node: StReturnNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stop*(node: StReturnNode): int {.inline.} = node.FValue.stop

proc temporaries*(node: StSequenceNode): seq[StVariableNode] = node.FTemporaries

proc `temporaries=`*(node: StSequenceNode, temps: seq[StVariableNode]) =
  node.FTemporaries = temps
  for temp in temps: temp.parent = node

proc newStSequenceNode*(leftBar: int, temporaries: seq[StVariableNode],
                        rightBar: int): StSequenceNode =
  new(result)
  result.leftBar = leftBar
  result.`temporaries=`(temporaries)
  result.rightBar = rightBar

proc toString(node: StSequenceNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StProgramNode](node).toString(name, indent, expandParent) &
    indent.spaces &
    "leftBar: " & $node.leftBar & ",\n" & indent.spaces &
    "rightBar: " & $node.rightBar & ",\n" & indent.spaces &
    "statements: " & node.FStatements.toString(indent) & ",\n" & indent.spaces &
    "periods: " & $node.periods & ",\n" & indent.spaces &
    "temporaries: " & node.FTemporaries.toString(indent)

method `$`*(node: StSequenceNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

proc statements*(node: StSequenceNode): seq[StProgramNode] = node.FStatements

proc `statements=`*(node: StSequenceNode, stmts: seq[StProgramNode]) =
  node.FStatements = stmts
  for statement in stmts: statement.parent = node

#proc `==`*(node1: StSequenceNode, node2: StSequenceNode): bool =
#  # TODO
#  discard

method stop*(node: StSequenceNode): int {.inline.} =
  (if node.periods.len == 0: 0 else: node.periods[^1]).max(if node.statements.len == 0: 0 else: node.statements[^1].stop)

proc toString(node: StTagNode, name: string, indent: int, expandParent: bool): string =
  var indent = indent + 2
  result = cast[StProgramNode](node).toString(name, indent, expandParent) &
    indent.spaces &
    "start: " & $node.start & ",\n" & indent.spaces &
    "stop: " & $node.FStop & ",\n"

method `$`*(node: StTagNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stop*(node: StTagNode): int {.inline.} = node.FStop

method `stop=`*(node: StTagNode, i: int) {.inline.} = node.FStop = i

method `==`*(node: StTagNode): bool =
  subclassResponsibility("==")

proc newStExternalCallNode*: StExternalCallNode =
  new(result)

proc toString(node: StExternalCallNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StTagNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "nameOrOrdinal: " & `$`(node.nameOrOrdinal, indent) & ",\n" & indent.spaces &
    "returnType: " & `$`(node.returnType, indent) & ",\n" & indent.spaces &
    "argumentTypes: " & node.argumentTypes.toString(indent) & ",\n" &
      indent.spaces &
    "isOverlapped: " & $node.isOverlapped & ",\n" & indent.spaces &
    "isVirtual: " & $node.isVirtual & ",\n" & indent.spaces &
    "callingConvention: " & $node.callingConvention

method `$`*(node: StExternalCallNode, indent: int = 0,
            expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

#method `==`*(node1: StExternalCallNode, node2: StExternalCallNode): bool =
#  # TODO
#  discard

proc newStPrimitiveNode*(index: int): StPrimitiveNode =
  new(result)
  result.primitiveIndex = index

proc toString(node: StPrimitiveNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StTagNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "primitiveIndex: " & $node.primitiveIndex

method `$`*(node: StPrimitiveNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

#method `==`*(node1: StPrimitiveNode, node2: StPrimitiveNode): bool =
#  # TODO
#  discard

proc toString(node: StValueNode, name: string, indent: int, expandParent: bool): string =
  var indent = indent + 2
  result = cast[StProgramNode](node).toString(name, indent, expandParent) &
    indent.spaces &
    "parentheses: " & repr(node.parentheses)

method `$`*(node: StValueNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stopWithoutParentheses*(node: StValueNode): int {.inline.} =
  subclassResponsibility("stopWithoutParentheses")

method stop*(node: StValueNode): int {.inline.} =
  if node.parentheses.isNil: return node.stopWithoutParentheses
  return node.parentheses[^1][1]

method addParenthesis*(node: StValueNode, parenthesis: array[0 .. 1, int]) =
  if node.parentheses.isNil: node.parentheses = @[]
  node.parentheses.add(parenthesis)

method `==`*(node: StValueNode): bool =
  subclassResponsibility("==")

proc `variable=`*(node: StAssignmentNode, variable: StVariableNode) =
  node.FVariable = variable
  variable.parent = node

proc `value=`*(node: StAssignmentNode, value: StValueNode) =
  node.FValue = value
  value.parent = node

proc newStAssignmentNode*(variable: StVariableNode, value: StValueNode,
                          position: int): StAssignmentNode =
  new(result)
  result.variable = variable
  result.value = value
  result.assignment = position

proc toString(node: StAssignmentNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "variable: " & `$`(node.FVariable, indent, false) & ",\n" & indent.spaces &
    "assignment: " & $node.assignment & ",\n" & indent.spaces &
    "value: " & `$`(node.FValue, indent, false)

method `$`*(node: StAssignmentNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stopWithoutParentheses*(node: StAssignmentNode): int {.inline.} =
  node.FValue.stop

proc newStBlockNode*: StBlockNode =
  new(result)

proc toString(node: StBlockNode, name: string, indent: int, expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "left: " & $node.left & ",\n" & indent.spaces &
    "righ: " & $node.right & ",\n" & indent.spaces &
    "colons: " & $node.colons & ",\n" & indent.spaces &
    "body: " & `$`(node.FBody, indent, false) & ",\n" & indent.spaces &
    "arguments: " & node.FArguments.toString(indent) & ",\n" & indent.spaces &
    "bar: " & $node.bar

method `$`*(node: StBlockNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

proc body*(node: StBlockNode): StSequenceNode = node.FBody

proc `body=`*(node: StBlockNode, body: StSequenceNode) =
  node.FBody = body
  body.parent = node

proc `arguments`*(node: StBlockNode): seq[StVariableNode] = node.FArguments

proc `arguments=`*(node: StBlockNode, args: seq[StVariableNode]) =
  node.FArguments = args
  for arg in args: arg.parent = node

#proc `==`*(node1: StBlockNode, node2: StBlockNode): bool =
#  # TODO
#  discard

method stopWithoutParentheses*(node: StBlockNode): int {.inline.} = node.right

method isBlock*(node: StBlockNode): bool = true

proc `messages=`*(node: StCascadeNode, messages: seq[StMessageNode]) =
  node.FMessages = messages
  for message in messages: message.parent = node

proc newStCascadeNode*(messages: seq[StMessageNode], semicolons: seq[int]): StCascadeNode =
  new(result)
  result.messages = messages
  result.semicolons = semicolons

proc toString(node: StCascadeNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "messages: " & node.FMessages.toString(indent) & ",\n" & indent.spaces &
    "semicolons: " & $node.semicolons

method `$`*(node: StCascadeNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stopWithoutParentheses*(node: StCascadeNode): int {.inline.} =
  node.FMessages[^1].stop

proc newStOptimizedNode*: StOptimizedNode =
  new(result)

proc toString(node: StOptimizedNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "left: " & $node.left & ",\n" & indent.spaces &
    "right: " & $node.right & ",\n" & indent.spaces &
    "body: " & `$`(node.FBody, indent, false) #& ",\n" & indent.spaces &

method `$`*(node: StOptimizedNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

proc arguments*(node: StOptimizedNode): seq[StVariableNode] = @[]

proc body*(node: StOptimizedNode): StSequenceNode = node.FBody

proc `body=`*(node: StOptimizedNode, body: StSequenceNode) =
  node.FBody = body
  body.parent = node

proc `==`*(node1: StOptimizedNode, node2: StOptimizedNode): bool =
  if node1[].addr == node2[].addr: return true
  return node1.body == node2.body

method stopWithoutParentheses*(node: StOptimizedNode): int {.inline.} = node.right

proc newStLiteralValueNode*(tk: StLiteralToken): StLiteralValueNode =
  new(result)
  result.token = tk

proc toString(node: StLiteralValueNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "token: " & `$`(node.token, indent)

method `$`*(node: StLiteralValueNode, indent: int = 0,
            expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stopWithoutParentheses*(node: StLiteralValueNode): int {.inline.} = node.token.stop

proc newStLiteralNode*(tk: StLiteralToken): StLiteralNode =
  # TODO: Array / ByteArray ?
  newStLiteralValueNode(tk)

proc receiver*(node: StMessageNode): StValueNode = node.FReceiver

proc `receiver=`*(node: StMessageNode, receiver: StValueNode) =
  node.FReceiver = receiver
  receiver.parent = node

proc arguments*(node: StMessageNode): seq[StValueNode] =
  if node.FArguments.isNil: return @[]
  return node.FArguments

proc `arguments=`*(node: StMessageNode, arguments: seq[StValueNode]) =
  node.FArguments = arguments
  for argument in arguments: argument.parent = node

proc newStMessageNode*(receiver: StValueNode, selectorParts: seq[StValueToken],
                       arguments: seq[StValueNode]): StMessageNode =
  new(result)
  result.receiver = receiver
  result.selectorParts = selectorParts
  result.arguments = arguments

proc toString(node: StMessageNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "receiver: " & `$`(node.FReceiver, indent, false) & ",\n" & indent.spaces &
    "selector: " & $node.selector & ",\n" & indent.spaces &
    "selectorParts: " & node.selectorParts.toString(indent) & ",\n" &
    indent.spaces &
    "arguments: " & node.FArguments.toString(indent)

method `$`*(node: StMessageNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

method stopWithoutParentheses*(node: StMessageNode): int {.inline.} =
  if node.arguments.len == 0: return node.selectorParts[0].stop
  return node.arguments[^1].stop

method isMessage*(node: StMessageNode): bool = true

proc `contents=`*(node: StLiteralArrayNode, contents: seq[StValueNode]) =
  node.FContents = contents
  for content in contents: content.parent = node

proc newStLiteralArrayNode*(start: int, contents: seq[StValueNode], stop: int,
                            isByteArray: bool): StLiteralArrayNode =
  new(result)
  result.FStart = start
  result.contents = contents
  result.FStop = stop
  result.isByteArray = isByteArray

proc toString(node: StLiteralArrayNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "isByteArray: " & $node.isByteArray & ",\n" & indent.spaces &
    "stop: " & $node.stop & ",\n" & indent.spaces &
    "contents: " & node.FContents.toString(indent) #& ",\n" & indent.spaces &
    # TODO
    #"start: " & $node.start

method `$`*(node: StLiteralArrayNode, indent: int = 0,
            expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

proc contents*(node: StLiteralArrayNode): seq[StValueNode] =
  node.FContents

method stopWithoutParentheses*(node: StLiteralArrayNode): int {.inline.} = node.FStop

proc newStVariableNode*(tk: StIdentifierToken): StVariableNode =
  new(result)
  result.token = tk

proc toString(node: StVariableNode, name: string, indent: int,
              expandParent: bool): string =
  var indent = indent + 2
  result = cast[StValueNode](node).toString(name, indent - 2, expandParent) &
    indent.spaces &
    "token: " & `$`(node.token, indent)

method `$`*(node: StVariableNode, indent: int = 0, expandParent: bool = true): string =
  node.toString(node.typeName, indent, expandParent) & "\n" & indent.spaces & "]"

proc name*(node: StVariableNode): string = primitiveValue[string](node.token.value)

proc `==`*(node1: StVariableNode, node2: StVariableNode): bool =
  if node1[].addr == node2[].addr: return true
  return node1.name == node2.name

method stopWithoutParentheses*(node: StVariableNode): int {.inline.} = node.token.stop

method isVariable*(node: StVariableNode): bool = true
