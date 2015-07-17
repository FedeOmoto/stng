import unicode, strutils, bignum

type
  MessageNotUnderstood = object of SystemError

  SubclassResponsibilityError = object of SystemError

  Primitive = bool | Rune | string | int | Int | float | Rat | seq[Object] | seq[byte]

  ObjectKind* = enum
    obj
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
    FClass: ClassDescription

  Behavior = ref object of Object
    superclass: ClassDescription

  ClassDescription = ref object of Behavior

  Class* = ref object of ClassDescription
    FName: string

  Metaclass = ref object of ClassDescription
    FInstanceClass: Class

# Declarations
proc basicNewClass(name: string): Class
proc newClass*(superclass: ClassDescription, name: string): Class
proc newMetaclass*(superclass: ClassDescription, instanceClass: Class): Metaclass
method name*(behavior: Behavior): string
method `$`*(o: Object, indent: int = 0): string

let ObjectClass* = basicNewClass("Object")
ObjectClass.superclass = nil

let BehaviorClass = basicNewClass("Behavior")
BehaviorClass.superclass = ObjectClass

let ClassDescriptionClass = basicNewClass("ClassDescription")
ClassDescriptionClass.superclass = BehaviorClass

let ClassClass = basicNewClass("Class")
ClassClass.superclass = ClassDescriptionClass

let MetaclassClass = basicNewClass("Metaclass")
MetaclassClass.superclass = ClassDescriptionClass

ObjectClass.FClass = newMetaclass(ClassClass, ObjectClass)
BehaviorClass.FClass = newMetaclass(BehaviorClass.superclass.FClass, BehaviorClass)
ClassDescriptionClass.FClass = newMetaclass(ClassDescriptionClass.superclass.FClass, ClassDescriptionClass)
ClassClass.FClass = newMetaclass(ClassClass.superclass.FClass, ClassClass)
MetaclassClass.FClass = newMetaclass(MetaclassClass.superclass.FClass, MetaclassClass)

let UndefinedObjectClass* = newClass(ObjectClass, "UndefinedObject")

let BooleanClass = newClass(ObjectClass, "Boolean")
let TrueClass = newClass(BooleanClass, "True")
let FalseClass = newClass(BooleanClass, "False")

let True* = Object(kind: boolean, booleanValue: true, FClass: TrueClass)
let False* = Object(kind: boolean, booleanValue: false, FClass: FalseClass)

let MagnitudeClass = newClass(ObjectClass, "Magnitude")
let CharacterClass = newClass(MagnitudeClass, "Character")

let CollectionClass = newClass(ObjectClass, "Collection")
let SequenceableCollectionClass = newClass(CollectionClass, "SequenceableCollection")
let ArrayedCollectionClass = newClass(SequenceableCollectionClass, "ArrayedCollection")
let StringClass = newClass(ArrayedCollectionClass, "String")
let SymbolClass = newClass(StringClass, "Symbol")

let ArithmeticValueClass = newClass(MagnitudeClass, "ArithmeticValue")
let NumberClass = newClass(ArithmeticValueClass, "Number")
let IntegerClass = newClass(NumberClass, "Integer")
let SmallIntegerClass = newClass(IntegerClass, "SmallInteger")
let LargeIntegerClass = newClass(IntegerClass, "LargeInteger")

let FloatClass = newClass(NumberClass, "Float")

let FractionClass = newClass(NumberClass, "Fraction")

let ArrayClass = newClass(ArrayedCollectionClass, "Array")

let ByteArrayClass = newClass(ArrayedCollectionClass, "ByteArray")

proc doesNotUnderstand*(receiver: string, message: string) =
  raise newException(MessageNotUnderstood, receiver & " does not understand #" &
                     message)

proc subclassResponsibility*(selector: string) =
  raise newException(SubclassResponsibilityError, selector &
    " is the responsibility of the subClassClass")

proc `==`*(o1: Object | Behavior | ClassDescription | Class | Metaclass,
           o2: Object | Behavior | ClassDescription | Class | Metaclass): bool =
  o1[].addr == o2[].addr

proc newUndefinedObject*: Object = nil

proc newObject*(class: Class): Object =
  if class.isNil or class == UndefinedObjectClass:
    nil
  else:
    Object(kind: obj, FClass: class)

proc newObject*[T: Primitive](data: T, isSymbol: bool = false): Object =
  when T is bool:
    if data == true: True else: False
  elif T is Rune:
    Object(kind: character, characterValue: data, FClass: CharacterClass)
  elif T is string:
    if isSymbol: Object(kind: symbol, strValue: data, FClass: SymbolClass) else: Object(kind: str, strValue: data, FClass: StringClass)
  elif T is int:
    Object(kind: smallInteger, smallIntegerValue: data, FClass: SmallIntegerClass)
  elif T is Int:
    Object(kind: largeInteger, largeIntegerValue: data, FClass: LargeIntegerClass)
  elif T is float:
    Object(kind: fp, fpValue: data, FClass: FloatClass)
  elif T is Rat:
    Object(kind: fraction, fractionValue: data, FClass: FractionClass)
  # TODO
  #elif T is :
  #  Object(kind: scaledDecimal, scaledDecimalValue: data)
  elif T is seq[Object]:
    Object(kind: arr, arrValue: data, FClass: ArrayClass)
  elif T is seq[byte]:
    Object(kind: byteArray, byteArrayValue: data, FClass: ByteArrayClass)
  else:
    raise newException(ObjectConversionError, "Invalid primitive type")

proc primitiveValue*[T: Primitive](o: Object): var T =
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

proc class*(o: Object): ClassDescription =
  if o.isNil: UndefinedObjectClass else: o.FClass

proc toString(collection: seq[Object], indent: int): string =
  result = "@["

  for item in collection:
    result &= "\n" & (indent + 2).spaces & `$`(item, indent + 2)

  if not collection.isNil and not (collection.len == 0):
    result &= "\n" & indent.spaces

  result &= "]"

proc toString(o: Object, name: string, indent: int): string =
  result = name & " --> "

  if o.isNil:
    result &= "nil"
    return

  let indent = indent + 2
  result &= "[\n" & indent.spaces & "kind: " & $o.kind

  case o.kind:
    of boolean:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[bool](o)
    of character:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[Rune](o)
    of str, symbol:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[string](o)
    of smallInteger:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[int](o)
    of largeInteger:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[Int](o)
    of fp:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[float](o)
    of fraction:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[Rat](o)
    of arr:
      result &= ",\n" & indent.spaces & "data: " & primitiveValue[seq[Object]](o).toString(indent)
    of byteArray:
      result &= ",\n" & indent.spaces & "data: " & $primitiveValue[seq[byte]](o)
    else:
      discard

  result &= "\n"

proc prefix(o: Object): string =
  if o.FClass.name[0].toLower in ['a', 'e', 'i', 'o', 'u']: "an " else: "a "

method `$`*(o: Object, indent: int = 0): string =
  o.toString(o.prefix & o.FClass.name, indent) & indent.spaces & "]"

proc toString(o: Behavior, name: string, indent: int): string =
  result = cast[Object](o).toString(name, indent) & (indent + 2).spaces &
    "superclass: " & (if o.superclass.isNil: "nil" else: o.superclass.name)

method `$`*(o: Behavior, indent: int = 0): string =
  o.toString(o.prefix & o.FClass.name, indent) & "\n" & indent.spaces & "]"

proc toString(o: ClassDescription, name: string, indent: int): string =
  result = cast[Behavior](o).toString(name, indent)

method `$`*(o: ClassDescription, indent: int = 0): string =
  o.toString(o.name, indent) & "\n" & indent.spaces & "]"

proc toString(o: Metaclass, name: string, indent: int): string =
  result = cast[ClassDescription](o).toString(name, indent) & ",\n" & (indent + 2).spaces &
    "instanceClass: " & o.FInstanceClass.name

method `$`*(o: Metaclass, indent: int = 0): string =
  o.toString(o.name, indent) & "\n" & indent.spaces & "]"

method size*(o: Object): int {.inline.} =
  if o.isNil: return 0
  return case o.kind:
    of str, symbol:
      o.strValue.len
    of arr:
      o.arrValue.len
    of byteArray:
      o.byteArrayValue.len
    else:
      0

proc isPrimitive*(o: Object): bool {.inline.} = o.isNil or o.kind != obj

proc isSmallInteger*(o: Object): bool {.inline.} =
  not o.isNil and o.kind == smallInteger

proc isLargeInteger*(o: Object): bool {.inline.} =
  not o.isNil and o.kind == largeInteger

proc isInteger*(o: Object): bool {.inline.} =
  not o.isNil and (o.kind == smallInteger or o.kind == largeInteger)

proc isString*(o: Object): bool {.inline.} = not o.isNil and o.kind == str

proc isArray*(o: Object): bool {.inline.} = not o.isNil and o.kind == arr

proc isByteArray*(o: Object): bool {.inline.} = not o.isNil and o.kind == byteArray

proc isBoolean*(o: Object): bool {.inline.} = not o.isNil and o.kind == boolean

proc isUndefinedObject*(o: Object): bool {.inline.} = o.isNil

proc isNumber*(o: Object): bool {.inline.} =
  if o.isNil: return false
  return case o.kind:
    of fp, fraction, smallInteger, largeInteger, scaledDecimal: true
    else: false

method name*(behavior: Behavior): string =
  subclassResponsibility("name")

method name*(behavior: Class): string = behavior.FName

method name*(behavior: Metaclass): string = behavior.FInstanceClass.name & " class"

method instanceClass(cd: ClassDescription): Class =
  subclassResponsibility("instanceClass")

method instanceClass(cd: Class): Class = cd

method instanceClass(cd: Metaclass): Class = cd.FInstanceClass

method isMeta*(behavior: Behavior): bool = false

proc basicNewMetaclass: Metaclass =
  result = Metaclass(kind: obj)

proc basicNewClass(name: string): Class =
  result = Class(kind: obj, FName: name)

proc newMetaclass*(superclass: ClassDescription, instanceClass: Class): Metaclass =
  result = basicNewMetaclass()
  result.FClass = MetaclassClass
  result.superclass = superclass
  result.FInstanceClass = instanceClass

method isMeta*(behavior: Metaclass): bool = true

proc newClass*(superclass: ClassDescription, name: string): Class =
  result = basicNewClass(name)
  result.superclass = superclass

  if result.superclass.isNil:
    result.FClass = newMetaclass(ClassClass, result)
  else:
    result.FClass = newMetaclass(result.superclass.FClass, result)


when isMainModule:
  assert ObjectClass.kind == obj
  assert ObjectClass.superclass.isNil
  assert ObjectClass.superclass.class == UndefinedObjectClass
  assert ObjectClass.class.isMeta
  assert ObjectClass.class.kind == obj
  assert ObjectClass.class.superclass == ClassClass
  assert ObjectClass.class.instanceClass == ObjectClass
  assert ObjectClass.class.class == MetaclassClass
  assert (not ObjectClass.class.class.isMeta)
  assert ObjectClass.class.class.class == MetaclassClass.class
  assert ObjectClass.class.class.class.class == MetaclassClass

  assert BehaviorClass.kind == obj
  assert BehaviorClass.superclass == ObjectClass

  assert ClassDescriptionClass.kind == obj
  assert ClassDescriptionClass.superclass == BehaviorClass

  assert ClassClass.kind == obj
  assert ClassClass.superclass == ClassDescriptionClass

  assert MetaclassClass.kind == obj
  assert MetaclassClass.superclass == ClassDescriptionClass

  assert UndefinedObjectClass.kind == obj
  assert UndefinedObjectClass.superclass == ObjectClass
  assert nil.class == UndefinedObjectClass
  assert nil.isPrimitive
  assert nil.isUndefinedObject
  assert newUndefinedObject().isNil

  assert True.kind == boolean
  assert primitiveValue[bool](True)
  assert True.isPrimitive
  assert True.isBoolean

  assert False.kind == boolean
  assert (not primitiveValue[bool](False))
  assert False.isPrimitive
  assert False.isBoolean

  assert newObject(true) == True
  assert newObject(false) == False

  let c = newObject('c'.Rune)
  assert c.kind == character
  assert primitiveValue[Rune](c) == 'c'.Rune
  assert c.isPrimitive

  let s = newObject("string")
  assert s.kind == str
  assert primitiveValue[string](s) == "string"
  assert s.size == primitiveValue[string](s).len
  assert s.isPrimitive
  assert s.isString

  let sym = newObject("symbol", true)
  assert sym.kind == symbol
  assert primitiveValue[string](sym) == "symbol"
  assert sym.size == primitiveValue[string](sym).len
  assert sym.isPrimitive

  let si = newObject(1234)
  assert si.kind == smallInteger
  assert primitiveValue[int](si) == 1234
  assert si.isPrimitive
  assert si.isNumber
  assert si.isInteger
  assert si.isSmallInteger

  let li = newObject(newInt(int.high) + 1)
  assert li.kind == largeInteger
  assert primitiveValue[Int](li) == newInt(int.high) + 1
  assert li.isPrimitive
  assert li.isNumber
  assert li.isInteger
  assert li.isLargeInteger

  let f = newObject(1.234)
  assert f.kind == fp
  assert primitiveValue[float](f) == 1.234
  assert f.isPrimitive
  assert f.isNumber

  let r = newObject(newRat(1, 2))
  assert r.kind == fraction
  assert primitiveValue[Rat](r) == newRat(1, 2)
  assert r.isPrimitive
  assert r.isNumber

  let arrayData = @[newObject(1), newObject("s")]
  let a = newObject(arrayData)
  assert a.kind == arr
  assert primitiveValue[seq[Object]](a) == arrayData
  assert a.size == arrayData.len
  assert a.isPrimitive

  let baData = @[1.byte, 15, 255]
  let ba = newObject(baData)
  assert ba.kind == byteArray
  assert ($primitiveValue[seq[byte]](ba) == $baData)
  assert ba.size == baData.len
  assert ba.isPrimitive

  let o = newObject(ObjectClass)
  assert o.kind == obj
  assert o.class == ObjectClass

  assert newObject(UndefinedObjectClass).isNil
  assert newObject(nil).isNil

  let animalClass = newClass(ObjectClass, "Animal")
  assert animalClass.kind == obj
  assert animalClass.superclass == ObjectClass
  let animalMetaclass = animalClass.class
  assert animalMetaclass.name == "Animal class"
  assert animalMetaclass.kind == obj
  assert animalMetaclass.superclass == ObjectClass.class
  assert animalMetaclass.instanceClass == animalClass

  let catClass = newClass(animalClass, "Cat")
  assert catClass.kind == obj
  assert catClass.superclass == animalClass
  assert catClass.class.name == "Cat class"
  assert catClass.class.superclass == animalMetaclass
  assert catClass.class.superclass.superclass == ObjectClass.class
  assert catClass.class.superclass.superclass.superclass == ClassClass

  let cat = newObject(catClass)
  assert cat.kind == obj
  assert cat.class == catClass
  assert cat.class.class == catClass.class
  assert cat.class.class.instanceClass == catClass
  assert cat.class.class.superclass == animalMetaclass
  assert cat.class.class.superclass.instanceClass == animalClass
  assert cat.class.class.superclass.superclass == ObjectClass.class
  assert cat.class.class.superclass.superclass.instanceClass == ObjectClass
  assert cat.class.class.superclass.superclass.superclass == ClassClass
  assert cat.class.superclass == animalClass
  assert cat.class.superclass.superclass == ObjectClass
  assert cat.class.superclass.superclass.superclass.isNil

  let protoClass = newClass(nil, "ProtoObject")
  assert protoClass.kind == obj
  assert protoClass.class.name == "ProtoObject class"
  assert protoClass.class.superclass == ClassClass
  assert protoClass.class.class == MetaclassClass
  assert protoClass.class.class.class == MetaclassClass.class
  assert protoClass.class.class.class.class == MetaclassClass
  assert protoClass.superclass.isNil

  let protoObject = newObject(protoClass)
  assert protoObject.kind == obj
  assert protoObject.class == protoClass
  assert protoObject.class.superclass.isNil
  assert protoObject.class.class.superclass == ClassClass
