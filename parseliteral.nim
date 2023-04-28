from std/parseutils import parseInt, parseHex, parseOct, parseBin

proc isPosInteger(input: string): bool =
  var value = 0

  if input.len() <= 2:
    return parseInt(input, value) == input.len()

  case input[0..1]:
  of "0B", "0b":  (if input.parseBin(value) == input.len(): return true)
  of "0X", "0x":  (if input.parseHex(value) == input.len(): return true)
  of "0O", "0o":  (if input.parseOct(value) == input.len(): return true)
  else:           (if input.parseInt(value) == input.len(): return true)

proc isInteger*(input: string): bool =
  if input[0] == '-':
    return input[1..high input].isPosInteger()
  else:
    return input[0..high input].isPosInteger()

proc asPosInteger(input: string): int =
  if input.len() <= 2:
    return if parseInt(input, result) < input.len(): 0 else: result

  case input[0..1]:
  of "0B", "0b":  (if input.parseBin(result) < input.len(): return 0)
  of "0X", "0x":  (if input.parseHex(result) < input.len(): return 0)
  of "0O", "0o":  (if input.parseOct(result) < input.len(): return 0)
  else:           (if input.parseInt(result) < input.len(): return 0)

proc asInteger*(input: string): int =
  if input[0] == '-':
    return -input[1..high input].asPosInteger()
  else:
    return  input[0..high input].asPosInteger()

proc isCharacter*(input: string): bool =
  if not (input[0] == '\'' and input[high input] == '\''):
    return false
  elif input[1] == '\\':
    case input[2]:
    of '0'..'9':
      return (input[2..(high input) - 1]).isInteger()
    of 'x':
      return ("0x" & input[3..(high input) - 1]).isInteger()
    of {'\\', '\'', '\"', 'n', 'r', 't', 'b', 'f'}:
      return true
    else:
      return false
  elif input[1] in {'\x20'..'\x7E'}:
    return true

proc asCharacter*(input: string): char =
  if not (input[0] == '\'' and input[high input] == '\''):
    return '\x0'

  if input[1] == '\\':
    case input[2]:
    of '0'..'9':  return char((input[2..(high input) - 1]).asInteger())
    of 'x':       return char(("0x" & input[3..(high input) - 1]).asInteger())
    of '\\':      return '\\'
    of '\'':      return '\''
    of '\"':      return '\"'
    of 'n':       return '\n'
    of 'r':       return '\r'
    of 't':       return '\t'
    of 'b':       return '\b'
    of 'f':       return '\f'
    else:         return '\0'

  if input[1] in {'\x20'..'\x7E'}:
    return input[1]
