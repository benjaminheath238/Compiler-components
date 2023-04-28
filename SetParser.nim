from std/algorithm import sort
from std/sequtils import deduplicate

import parseliteral

type TokenKind = enum
  TK_INTEGER

  TK_LBRACE
  TK_RBRACE
  
  TK_COMMA

type Token = tuple[kind: TokenKind, lexeme: string, index: int]

type Lexer = ref object
  index: int
  start: int

  input: string
  output: seq[Token]
  errors: seq[(string, int)]

proc newLexer(
  input: string
): Lexer = Lexer(index: 0, start: 0, input: input, output: newSeq[Token](), errors: newSeq[(string, int)]())

proc tokenize(this: Lexer): void =
  while this.index <= high this.input:
    this.start = this.index

    case this.input[this.index]:
    of {' ', '\t'}:    
      this.index.inc()
    of '{':
      this.index.inc()
      this.output.add((TK_LBRACE, this.input[this.start..this.index-1], this.index))
    of '}':
      this.index.inc()
      this.output.add((TK_RBRACE, this.input[this.start..this.index-1], this.index))
    of ',':
      this.index.inc()
      this.output.add((TK_COMMA, this.input[this.start..this.index-1], this.index))
    of {'0'..'9', '+', '-'}:
      while (this.index <= high this.input)  and (this.input[this.index] in {'0'..'9', 'o', 'O', 'x', 'X', 'b', 'B', 'a'..'f', 'A'..'F'}):
        this.index.inc()

      this.output.add((TK_INTEGER, this.input[this.start..this.index-1], this.index))
    else:
      this.errors.add(("Unexpected character '" & $this.input[this.index] & "'", this.index))
      this.index.inc()

type Set* = seq[int]

type Parser = ref object
  index: int

  input: seq[Token]
  output: Set
  errors: seq[(string, int)]

proc newParser(
  input: seq[Token]
): Parser = Parser(index: 0, input: input, output: newSeq[int](), errors: newSeq[(string, int)]())

proc parse(this: Parser): void =
  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_LBRACE}):
    this.index.inc()
  else:
    this.errors.add(("Expected an '{' at begining of set", this.index))

  while (this.index <= high this.input) and not (this.input[this.index].kind in {TK_RBRACE}):
    if (this.index <= high this.input) and (this.input[this.index].kind in {TK_INTEGER}) and this.input[this.index].lexeme.isInteger():
      this.output.add(this.input[this.index].lexeme.asInteger())
      this.index.inc()
    else:
      this.errors.add(("Expected an integer for set element", this.index))

    if (this.index <= high this.input) and not (this.input[this.index].kind in {TK_RBRACE}) and this.input[this.index].kind in {TK_COMMA}:
      this.index.inc()
    else:
      this.errors.add(("Expected a comma between set elements", this.index))

  if (this.index <= high this.input) and (this.input[this.index].kind in {TK_RBRACE}):
    this.index.inc()
  else:
    this.errors.add(("Expected an '}' at begining of set", this.index))

  this.output.sort()
  this.output = this.output.deduplicate(isSorted=true)

proc parseSet*(input: string): (Set, seq[(string, int)]) =
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

proc newSet*(input: string): Set = parseSet(input)[0]

proc isEmpty*(this: Set): bool = this.len() == 0
proc isSingleton*(this: Set): bool = this.len() == 1
proc card*(this: Set): int = this.len()

when isMainModule:
  assert newSet("{0, 1, 2}") == @[0, 1, 2]

  assert newSet("{0, 1, 2}") == newSet("{0, 1, 2}")
  assert newSet("{2, 1, 0}") == newSet("{0, 1, 2}")
  assert newSet("{1, 2, 3}") != newSet("{0, 1, 2}")
  assert newSet("{1, 1, 1}") == newSet("{1}")

  assert -1 notin newSet("{0, 1, 2}")
  assert  0 in    newSet("{0, 1, 2}")
  assert  1 in    newSet("{0, 1, 2}")
  assert  2 in    newSet("{0, 1, 2}")
  assert  3 notin newSet("{0, 1, 2}")

  assert     newSet("{}").isEmpty()
  assert not newSet("{1}").isEmpty()

  assert     newSet("{1}").isSingleton()
  assert not newSet("{}").isSingleton()
  assert not newSet("{1, 2}").isSingleton()

  assert newSet("{}").card() == 0
  assert newSet("{1}").card() == 1
  assert newSet("{1, 2}").card() == 2

