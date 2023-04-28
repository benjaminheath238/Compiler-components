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

proc isInteger(input: string): bool =
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

proc asInteger(input: string): int =
  if input[0] == '-':
    return -input[1..high input].asPosInteger()
  else:
    return  input[0..high input].asPosInteger()

type TokenKind = enum
  TK_INTEGER
  
  TK_LPAREN
  TK_RPAREN
  TK_LBRACK
  TK_RBRACK

  TK_COMMA

type Token = tuple[kind: TokenKind, lexeme: string]

type Lexer = ref object
  start: int
  index: int

  input: string
  output: seq[Token]
  errors: seq[(string, int)]

proc newLexer(
  input: string
): Lexer = Lexer(index: 0, input: input, output: newSeq[Token](), errors: newSeq[(string, int)]())

proc tokenize(this: Lexer): void =
  while this.index <= high this.input:
    this.start = this.index

    case this.input[this.index]:
    of {' ', '\t'}:
      this.index.inc()
    of '(':
      this.index.inc()
      this.output.add((TK_LPAREN, this.input[this.start..this.index-1]))
    of ')':
      this.index.inc()
      this.output.add((TK_RPAREN, this.input[this.start..this.index-1]))
    of '[':
      this.index.inc()
      this.output.add((TK_LBRACK, this.input[this.start..this.index-1]))
    of ']':
      this.index.inc()
      this.output.add((TK_RBRACK, this.input[this.start..this.index-1]))
    of ',':
      this.index.inc()
      this.output.add((TK_COMMA, this.input[this.start..this.index-1]))
    of {'-', '+', '0'..'9'}:
      this.index.inc()
      
      while (this.index <= high this.input) and (this.input[this.index] in {'0'..'9', 'o', 'O', 'x', 'X', 'b', 'B', 'a'..'f', 'A'..'F'}):
        this.index.inc()
    
      this.output.add((TK_INTEGER, this.input[this.start..this.index-1]))
    else:
      this.errors.add(("Unexpected character '" & this.input[this.index] & "'", this.index))
      this.index.inc()

type Interval* = tuple[closed: tuple[l, r: bool], bounds: tuple[l, r: int]]

type Parser = ref object
  index: int

  input: seq[Token]
  output: Interval
  errors: seq[(string, int)]

proc newParser(
  input: seq[Token]
): Parser = Parser(index: 0, input: input, output: ((false, false), (0, 0)), errors: newSeq[(string, int)]())

proc parse(this: Parser): void =
  var lclosed = false
  var rclosed = false

  var lbounds = 0
  var rbounds = 0

  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_LBRACK, TK_RBRACK, TK_LPAREN}):
    case this.input[this.index].kind:
    of {TK_RBRACK, TK_LPAREN}:  lclosed = false
    of {TK_LBRACK}:             lclosed = true
    else:                       discard

    this.index.inc()
  else:
    this.errors.add(("Expected one of '[', ']' or '(' at begining of interval", this.index))

  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_INTEGER}):
    if this.input[this.index].lexeme.isInteger():
      lbounds = this.input[this.index].lexeme.asInteger()
    else:
      this.errors.add(("Invalid integer for first bound of interval", this.index))
    
    this.index.inc()
  else:
    this.errors.add(("Expected an integer for first bound of interval", this.index))

  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_COMMA}):
    this.index.inc()
  else:
    this.errors.add(("Expected a comma between interval bounds", this.index))

  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_INTEGER}):
    if this.input[this.index].lexeme.isInteger():
      rbounds = this.input[this.index].lexeme.asInteger()
    else:
      this.errors.add(("Invalid integer for second bound of interval", this.index))

    this.index.inc()
  else:
    this.errors.add(("Expected an integer for second bound of interval", this.index))

  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_LBRACK, TK_RBRACK, TK_RPAREN}):
    case this.input[this.index].kind:
    of {TK_LBRACK, TK_RPAREN}:  rclosed = false
    of {TK_RBRACK}:             rclosed = true
    else:                       discard

    this.index.inc()
  else:
    this.errors.add(("Expected one of '[', ']' or ')' at end of interval", this.index))

  this.output = ((lclosed, rclosed), (lbounds, rbounds))

proc parseInterval*(input: string): (Interval, seq[(string, int)]) =
  var errors = newSeq[(string, int)]()
  
  let lexer = newLexer(input)
  
  lexer.tokenize()
  
  if lexer.errors.len() > 0:
    errors.add(lexer.errors)

  let parser = newParser(lexer.output)

  parser.parse()

  if parser.errors.len() > 0:
    errors.add(parser.errors)

  return (parser.output, errors)

proc newInterval*(input: string): Interval = parseInterval(input)[0]

proc isClosed*(this: Interval): bool = this.closed.l and this.closed.r
proc isOpen*(this: Interval): bool = not this.isClosed()
proc isDegenerate*(this: Interval): bool = this.bounds.l == this.bounds.r and this.isClosed()
proc isEmpty*(this: Interval): bool = (this.bounds.l > this.bounds.r) or
                                      (this.bounds.l == this.bounds.r and this.isOpen())

converter toSlice*(this: Interval): Slice[int] = 
  this.bounds.l+(if this.closed.l: 0 else: 1)..this.bounds.r-(if this.closed.r: 0 else: 1)

when isMainModule:
  # ISO 31-11
  assert newInterval("[-1, 1)") == newInterval("[-1, 1[")
  assert newInterval("(-1, 1]") == newInterval("]-1, 1]")
  assert newInterval("(-1, 1)") == newInterval("]-1, 1[")

  assert newInterval("[-1, 1]") == ((true,   true),  (-1, 1))
  assert newInterval("[-1, 1)") == ((true,   false), (-1, 1))
  assert newInterval("(-1, 1]") == ((false,  true),  (-1, 1))
  assert newInterval("(-1, 1)") == ((false,  false), (-1, 1))

  # Slice converter tests
  assert -1 in    newInterval("[-1, 1]")
  assert  1 in    newInterval("[-1, 1]")
  assert  0 in    newInterval("[-1, 1]")

  assert -1 in    newInterval("[-1, 1)")
  assert  1 notin newInterval("[-1, 1)")
  assert  0 in    newInterval("[-1, 1)")

  assert -1 notin newInterval("(-1, 1]")
  assert  1 in    newInterval("(-1, 1]")
  assert  0 in    newInterval("(-1, 1]")

  assert -1 notin newInterval("(-1, 1)")
  assert  1 notin newInterval("(-1, 1)")
  assert  0 in    newInterval("(-1, 1)")

  # Classification tests
  assert not newInterval("[1,  1]").isEmpty()
  assert     newInterval("[1, -1]").isEmpty()
  assert     newInterval("[1, -1)").isEmpty()
  assert     newInterval("(1, -1]").isEmpty()
  assert     newInterval("(1, -1)").isEmpty()
  assert     newInterval("(1,  1)").isEmpty()
  assert     newInterval("(1,  1]").isEmpty()
  assert     newInterval("[1,  1)").isEmpty()

  assert     newInterval("[1,  1]").isDegenerate()
  assert not newInterval("[1, -1]").isDegenerate()
  assert not newInterval("[1, -1)").isDegenerate()
  assert not newInterval("(1, -1]").isDegenerate()
  assert not newInterval("(1, -1)").isDegenerate()
  assert not newInterval("(1,  1)").isDegenerate()
  assert not newInterval("(1,  1]").isDegenerate()
  assert not newInterval("[1,  1)").isDegenerate()

  assert newInterval("[1,  1]").isClosed()
  assert newInterval("(1,  1)").isOpen()

  # Quirks of the method
  assert newInterval("[]") ==      ((true,   true),  (0, 0))
  assert newInterval("()") ==      ((false,  false), (0, 0))
  assert newInterval("-1") ==      ((false,  false), (-1, 0))
  assert newInterval("-1, 0") ==   ((false,  false), (-1, 0))
  assert newInterval("-1, 1") ==   ((false,  false), (-1, 1))
  assert newInterval("-1 1") ==    ((false,  false), (-1, 1))
