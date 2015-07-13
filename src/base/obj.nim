import unicode, strutils, bignum

type
  MessageNotUnderstood = object of SystemError

  SubclassResponsibilityError = object of SystemError

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
    arr
    byteArray

  Object* = ref object of RootObj
    case kind*: ObjectKind
      of boolean:
        booleanValue: bool
      of character:
        characterValue: Rune
      of str, symbol:
        strValue: string
      of smallInteger:
        smallIntegerValue: int
      of largeInteger:
        largeIntegerValue: Int
      of fp:
        fpValue: float
      of fraction:
        fractionValue: Rat
      # TODO
      #of scaledDecimal:
      #  scaledDecimalValue:
      of arr:
        arrValue: seq[Object]
      of byteArray:
        byteArrayValue: seq[byte]
      else:
        nil

  ObjectClass* = ref object of RootObj

proc doesNotUnderstand*(receiver: string, message: string) =
  raise newException(MessageNotUnderstood, receiver & " does not understand #" &
                     message)

proc subclassResponsibility*(selector: string) =
  raise newException(SubclassResponsibilityError, selector & " is the responsibility of the subclass")

proc newUndefinedObject*: Object =
  Object(kind: undefinedObject)

proc newObject*: Object =
  Object(kind: obj)

proc newObject*[T](data: T, isSymbol: bool = false): Object =
  when T is bool:
    Object(kind: boolean, booleanValue: data)
  elif T is Rune:
    Object(kind: character, characterValue: data)
  elif T is string:
    if isSymbol: Object(kind: symbol, strValue: data) else: Object(kind: str, strValue: data)
  elif T is int:
    Object(kind: smallInteger, smallIntegerValue: data)
  elif T is Int:
    Object(kind: largeInteger, largeIntegerValue: data)
  elif T is float:
    Object(kind: fp, fpValue: data)
  elif T is Rat:
    Object(kind: fraction, fractionValue: data)
  # TODO
  #elif T is :
  #  Object(kind: scaledDecimal, scaledDecimalValue: data)
  elif T is seq[Object]:
    Object(kind: arr, arrValue: data)
  elif T is seq[byte]:
    Object(kind: byteArray, byteArrayValue: data)
  else:
    raise newException(ObjectConversionError, "Invalid primitive type")

proc primitiveValue*[T](o: Object): var T =
  when T is bool:
    o.booleanValue
  elif T is Rune:
    o.characterValue
  elif T is string:
    o.strValue
  elif T is int:
    o.smallIntegerValue
  elif T is Int:
    o.largeIntegerValue
  elif T is float:
    o.fpValue
  elif T is Rat:
    o.fractionValue
  # TODO
  #elif T is :
  #  return o.scaledDecimalValue
  elif T is seq[Object]:
    o.arrValue
  elif T is seq[byte]:
    o.byteArrayValue
  else:
    nil

proc `$`*(o: Object, indent: int = 0): string =
  result = "Object --> "

  if o == nil:
    result &= "nil"
    return

  result &= "[\n  " & indent.spaces & "kind: " & $o.kind

  case o.kind:
    of boolean:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[bool](o)
    of character:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[Rune](o)
    of str, symbol:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[string](o)
    of smallInteger:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[int](o)
    of largeInteger:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[Int](o)
    of fp:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[float](o)
    of fraction:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[Rat](o)
    of arr:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[seq[Object]](o)
    of byteArray:
      result &= ",\n" & (2 + indent).spaces & "data: " & $primitiveValue[seq[byte]](o)
    else:
      result &= ",\n" & (2 + indent).spaces & "data: nil"

  result &= "\n" & indent.spaces & "]"

method size*(o: Object): int {.inline.} =
  case o.kind:
    of str:
      o.strValue.len
    of arr:
      o.arrValue.len
    of byteArray:
      o.byteArrayValue.len
    else:
      0

proc isSmallInteger*(o: Object): bool {.inline.} = o.kind == smallInteger

proc isLargeInteger*(o: Object): bool {.inline.} = o.kind == largeInteger

proc isInteger*(o: Object): bool {.inline.} =
  o.kind == smallInteger or o.kind == largeInteger

proc isString*(o: Object): bool {.inline.} = o.kind == str

proc isArray*(o: Object): bool {.inline.} = o.kind == arr

proc isByteArray*(o: Object): bool {.inline.} = o.kind == byteArray

proc isBoolean*(o: Object): bool {.inline.} = o.kind == boolean

proc isUndefinedObject*(o: Object): bool {.inline.} = o.kind == undefinedObject

proc isNumber*(o: Object): bool {.inline.} =
  case o.kind:
    of fp, fraction, smallInteger, largeInteger, scaledDecimal: true
    else: false

proc newObjectClass*(): ObjectClass =
  new(result)
