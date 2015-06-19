import unicode, streams
export unicode, streams

type
  UnicodeStringStream* = ref UnicodeStringStreamObj
  UnicodeStringStreamObj* = object of StringStreamObj
    lastRuneLen: int

proc newUnicodeStringStream*(s: string = ""): UnicodeStringStream =
  let ss = newStringStream(s)
  new(result)
  result.data = s
  result.closeImpl = ss.closeImpl
  result.atEndImpl = ss.atEndImpl
  result.setPositionImpl = ss.setPositionImpl
  result.getPositionImpl = ss.getPositionImpl
  result.readDataImpl = ss.readDataImpl
  result.writeDataImpl = ss.writeDataImpl

proc lastRuneLen*(stream: UnicodeStringStream): int = stream.lastRuneLen

proc readRune*(stream: UnicodeStringStream): Rune =
  if stream.atEnd: return (-1).Rune
  var old, new = stream.getPosition
  fastRuneAt(stream.data, new, result)
  stream.lastRuneLen = new - old
  stream.setPosition(new)

proc peek*(stream: UnicodeStringStream): Rune =
  if stream.atEnd: return (-1).Rune
  let i = stream.getPosition
  result = stream.readRune
  stream.setPosition(i)

proc contents*(stream: UnicodeStringStream): string =
  stream.data.substr(0, stream.getPosition - 1)
