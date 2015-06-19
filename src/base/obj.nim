import unicode, strutils, bignum

type
  MessageNotUnderstood = object of SystemError

  ObjectKind* = enum
    obj
    undefinedObject
    boolean
    character
    str
    symbol
    smallInteger
    largeInteger
    fp
    fraction
    scaledDecimal

  Object* = ref object of RootObj
    kind: ObjectKind
    data: int64

proc finalizeObject(o: Object) =
  case o.kind:
    of str: GC_unref(cast[string](o.data))
    of largeInteger: GC_unref(cast[Int](o.data))
    of fraction: GC_unref(cast[Rat](o.data))
    else: discard

proc doesNotUnderstand*(receiver: string, message: string) =
  raise newException(MessageNotUnderstood, receiver & " does not understand #" &
                     message)

proc newObject*(): Object =
  new(result, finalizeObject)

proc newUndefinedObject*(): Object =
  new(result)
  result.kind = undefinedObject
  result.data = cast[int64](nil)

proc newObject*(data: bool): Object =
  new(result)
  result.kind = boolean
  result.data = cast[int64](data)

proc newObject*(data: Rune): Object =
  new(result)
  result.kind = character
  result.data = cast[int64](data)

proc newObject*(data: string, isSymbol = false): Object =
  var s = data
  GC_ref(s)
  result = newObject()
  if isSymbol: result.kind = symbol else: result.kind = str
  result.data = cast[int64](s)

proc newObject*(data: int): Object =
  new(result)
  result.kind = smallInteger
  result.data = cast[int64](data)

proc newObject*(data: Int): Object =
  var i = data.clone
  GC_ref(i)
  result = newObject()
  result.kind = largeInteger
  result.data = cast[int64](i)

proc newObject*(data: float): Object =
  new(result)
  result.kind = fp
  result.data = cast[int64](data)

proc newObject*(data: Rat): Object =
  var r = data.clone
  GC_ref(r)
  result = newObject()
  result.kind = fraction
  result.data = cast[int64](r)

proc boolValue*(o: Object): bool =
  cast[bool](o.data)

proc charValue*(o: Object): Rune =
  cast[Rune](o.data)

proc stringValue*(o: Object): string =
  cast[string](o.data)

proc symbolValue*(o: Object): string =
  o.stringValue

proc intValue*(o: Object): int =
  cast[int](o.data)

proc bigIntValue*(o: Object): Int =
  cast[Int](o.data)

proc floatValue*(o: Object): float =
  cast[float](o.data)

proc ratValue*(o: Object): Rat =
  cast[Rat](o.data)

proc `$`*(o: Object, indent: int = 0): string =
  result = "Object --> "

  if o == nil:
    result &= "nil"
    return

  result &= "[\n  " & indent.spaces & "kind: " & $o.kind

  case o.kind:
    of undefinedObject:
      result &= ",\n" & (2 + indent).spaces & "data: nil"
    of boolean:
      result &= ",\n" & (2 + indent).spaces & "data: " & $o.boolValue
    of character:
      result &= ",\n" & (2 + indent).spaces & "data: " & $o.charValue
    of str:
      result &= ",\n" & (2 + indent).spaces & "data: " & o.stringValue
    of symbol:
      result &= ",\n" & (2 + indent).spaces & "data: " & o.symbolValue
    of smallInteger:
      result &= ",\n" & (2 + indent).spaces & "data: " & $o.intValue
    of largeInteger:
      result &= ",\n" & (2 + indent).spaces & "data: " & $o.bigIntValue
    of fp:
      result &= ",\n" & (2 + indent).spaces & "data: " & $o.floatValue
    of fraction:
      result &= ",\n" & (2 + indent).spaces & "data: " & $o.ratValue
    of scaledDecimal:
      # TODO
      discard
    else:
      discard

  result &= "\n" & indent.spaces & "]"

proc kind*(o: Object): ObjectKind {.inline.} = o.kind

proc `kind=`*(o: Object, k: ObjectKind) {.inline.} = o.kind = k
