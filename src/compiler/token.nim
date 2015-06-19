import ../base/obj

type
  StToken* = ref object of RootObj
    sourcePointer: int
    comments: seq[array[0 .. 1, int]]

  StAssignmentToken* = ref object of StToken

  StOptimizedToken* = ref object of StToken
    stopPosition: int

  StValueToken* = ref object of StToken
    value: Object

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
    source: string

proc newStToken*(start: int): StToken =
  new(result)
  result.sourcePointer = start

proc toString(tk: StToken, name: string): string =
  result = name & " --> [\n  sourcePointer: " & $tk.sourcePointer &
           ",\n  comments: " & repr(tk.comments)

method typeName(tk: StToken): string =
  if tk of StAssignmentToken:
    result = "StAssignmentToken"
  else:
    result = "StToken"

method `$`*(tk: StToken): string =
  tk.toString(tk.typeName) & "]"

method start*(tk: StToken): int {.inline.} = tk.sourcePointer

method `start=`*(tk: StToken, i: int) {.inline.} = tk.sourcePointer = i

method comments*(tk: StToken): seq[array[0 .. 1, int]] {.inline.} = tk.comments

method `comments=`*(tk: StToken, c: seq[array[0 .. 1, int]]) {.inline.} =
  tk.comments = c

method length*(tk: StToken): int {.inline.} =
  doesNotUnderstand(tk.typeName, "length")

method value*(tk: StToken): Object {.inline.} =
  doesNotUnderstand(tk.typeName, "value")

method `value=`*(tk: StToken, o: Object) {.inline.} =
  doesNotUnderstand(tk.typeName, "value=")

method stop*(tk: StToken): int {.inline.} =
  doesNotUnderstand(tk.typeName, "stop")

method `stop=`*(tk: StToken, i: int) {.inline.} =
  doesNotUnderstand(tk.typeName, "stop=")

method source*(tk: StToken): string {.inline.} =
  doesNotUnderstand(tk.typeName, "source")

method `source=`*(tk: StToken, s: string) {.inline.} =
  doesNotUnderstand(tk.typeName, "source=")

proc newStAssignmentToken*(start: int): StAssignmentToken =
  new(result)
  result.sourcePointer = start

proc newStOptimizedToken*(start: int, stop: int): StOptimizedToken =
  new(result)
  result.sourcePointer = start
  result.stopPosition = stop

method typeName(tk: StOptimizedToken): string =
  "StOptimizedToken"

method `$`*(tk: StOptimizedToken): string =
  result = cast[StToken](tk).toString(tk.typeName) &
           "  stopPosition: " & $tk.stopPosition & "\n]"

method length*(tk: StOptimizedToken): int {.inline.} =
  tk.stopPosition - tk.sourcePointer + 1

proc toString(tk: StValueToken, name: string): string =
  cast[StToken](tk).toString(name) & "  value: " & `$`(tk.value, 2)

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

method `$`*(tk: StValueToken): string =
  tk.toString(tk.typeName) & "\n]"

method value*(tk: StValueToken): Object {.inline.} = tk.value

method `value=`*(tk: StValueToken, o: Object) {.inline.} = tk.value = o

proc newStBinarySelectorToken*: StBinarySelectorToken =
  new(result)

proc newStIdentifierToken*(value: Object, start: int): StIdentifierToken =
  new(result)
  result.value = value
  result.sourcePointer = start

proc newStIllegalCharacterToken*(value: Object, start: int):
                                 StIllegalCharacterToken =
  new(result)
  result.value = value
  result.sourcePointer = start

proc newStKeywordToken*(value: Object, start: int): StKeywordToken =
  new(result)
  result.value = value
  result.sourcePointer = start

proc newStLiteralArrayToken*(value: Object, start: int): StLiteralArrayToken =
  new(result)
  result.value = value
  result.sourcePointer = start

proc newStSpecialCharacterToken*(value: Object, start: int):
                                 StSpecialCharacterToken =
  new(result)
  result.value = value
  result.sourcePointer = start

proc newStLiteralToken*: StLiteralToken =
  new(result)

proc newStLiteralToken*(value: Object, start, stop: int): StLiteralToken =
  new(result)
  result.value = value
  result.sourcePointer = start
  result.stopPosition = stop

proc toString(tk: StLiteralToken, name: string): string =
  cast[StValueToken](tk).toString(name) & ",\n  stopPosition: " & $tk.stopPosition

method typeName(tk: StLiteralToken): string =
  if tk of StMultiKeywordLiteralToken:
    result = "StMultiKeywordLiteralToken"
  else:
    result = "StLiteralToken"

method `$`*(tk: StLiteralToken): string =
  tk.toString(tk.typeName) & "\n]"

method stop*(tk: StLiteralToken): int {.inline.} = tk.stopPosition

method `stop=`*(tk: StLiteralToken, i: int) {.inline.} = tk.stopPosition = i

proc newStMultiKeywordLiteralToken*(value: Object, start, stop: int):
                                    StMultiKeywordLiteralToken =
  new(result)
  result.value = value
  result.sourcePointer = start
  result.stopPosition = stop

proc newStNumberLiteralToken*(value: Object, start, stop: int, source: string):
                              StNumberLiteralToken =
  new(result)
  result.value = value
  result.sourcePointer = start
  result.stopPosition = stop
  result.source = source

method typeName(tk: StNumberLiteralToken): string =
  "StNumberLiteralToken"

method `$`*(tk: StNumberLiteralToken): string =
  result = cast[StLiteralToken](tk).toString(tk.typeName) &
           ",\n  source: " & $tk.source & "\n]"

method source*(tk: StNumberLiteralToken): string {.inline.} = tk.source

method `source=`*(tk: StNumberLiteralToken, s: string) {.inline.} = tk.source = s
