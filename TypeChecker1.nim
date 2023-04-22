from std/tables import TableRef, newTable, toTable, contains, `[]`, `[]=`, pairs
from std/strutils import parseInt
from std/json import `%`, pretty

type Group = enum
  GK_ERROR
  GK_VOID
  GK_INTEGER
  GK_STRING

const GROUPS = toTable({
  "Int":    GK_INTEGER,
  "String": GK_STRING,
})

type ValueKind = enum
  VK_INTEGER
  VK_STRING

type Value = ref object
  case kind: ValueKind:
  of VK_INTEGER:
    integerValue: int
  of VK_STRING:
    stringValue: string

type Variable = ref object
  group: Group

type Environment = ref object
  super: Environment

  variables: TableRef[string, Variable]

proc newEnvironment(super: Environment = nil): Environment =
  result = Environment()

  result.super = super
  
  result.variables = newTable[string, Variable]()

proc hasVariable(this: Environment, identifier: string): bool = (identifier in this.variables) or (this.super != nil and this.super.hasVariable(identifier))

proc getVariable(this: Environment, identifier: string): Variable =
  if identifier in this.variables:
    result = this.variables[identifier]
  elif this.super != nil:
    result = this.super.getVariable(identifier)
  else:
    echo "No such variable"

proc addVariable(this: Environment, identifier: string, variable: Variable): void =
  if identifier notin this.variables:
    this.variables[identifier] = variable
  else:
    echo "Variable already defined"

type State = ref object
  gEnv: Environment
  lEnv: Environment

proc newState(): State =
  result = State()

  result.gEnv = newEnvironment()
  result.lEnv = newEnvironment(result.gEnv)

type TokenKind = enum
  TK_ASSIGN
  TK_ADD
  TK_SUB
  TK_MUL
  TK_DIV
  TK_MOD
  TK_POW

  TK_INTEGER
  TK_STRING

  TK_COLON
  TK_SEMI

  TK_LPAREN
  TK_RPAREN

  TK_IDENTIFIER

  TK_LET

type Token = tuple[kind: TokenKind, lexeme: string, line: int, column: int]

type Lexer = ref object
  index: int
  start: int

  line: int
  column: int

  input: string
  output: seq[Token]

proc newLexer(input: string): Lexer =
  result = Lexer()

  result.index = 0
  result.start = 0

  result.line = 1
  result.column = 1

  result.input = input
  result.output = newSeq[Token]()

template eos(this: Lexer, l: int = 0): bool = this.index + l > high this.input

template reset(this: Lexer): void = this.start = this.index

template read(this: Lexer, l: int = 0): char = this.input[this.index + l]

template nextLine(this: Lexer): void =
  this.index.inc()
  this.line.inc()
  this.column = 1
  
template nextColumn(this: Lexer): void =
  this.index.inc()
  this.column.inc()

template lexeme(this: Lexer, soffset: int = 0, eoffset: int = -1): string =
  this.input[this.start + soffset..this.index + eoffset]

proc addToken(this: Lexer, kind: TokenKind, lexeme: string = this.lexeme, xoffset: int = -lexeme.len(), yoffset: int = 0): void =
  this.output.add((kind, lexeme, this.line + yoffset, this.column + xoffset))

const RESERVED_WORDS = toTable({
  "let":    TK_LET,
})

proc tokenize(this: Lexer): void =
  while not this.eos():
    this.reset()
    
    case this.read():
    of {' ', '\r', '\t'}:
      this.nextColumn()
    of '\n':
      this.nextLine()
    of '=':
      this.nextColumn()
      this.addToken(TK_ASSIGN)
    of '+':
      this.nextColumn()
      this.addToken(TK_ADD)
    of '-':
      this.nextColumn()
      this.addToken(TK_SUB)
    of '*':
      this.nextColumn()
      this.addToken(TK_MUL)
    of '/':
      this.nextColumn()
      this.addToken(TK_DIV)
    of '%':
      this.nextColumn()
      this.addToken(TK_MOD)
    of '^':
      this.nextColumn()
      this.addToken(TK_POW)
    of {'0'..'9'}:
      while not this.eos() and this.read() in {'0'..'9'}:
        this.nextColumn()

      this.addToken(TK_INTEGER)
    of '\"':
      this.nextColumn()
      while not this.eos() and this.read() != '\"':
        this.nextColumn()
      this.nextColumn()
        
      this.addToken(TK_STRING)
    of ';':
      this.nextColumn()
      this.addToken(TK_SEMI)
    of ':':
      this.nextColumn()
      this.addToken(TK_COLON)
    of '(':
      this.nextColumn()
      this.addToken(TK_LPAREN)
    of ')':
      this.nextColumn()
      this.addToken(TK_RPAREN)
    of {'a'..'z', 'A'..'Z'}:
      while not this.eos() and this.read() in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
        this.nextColumn()
      
      if this.lexeme() in RESERVED_WORDS:
        this.addToken(RESERVED_WORDS[this.lexeme()])
      else:
        this.addToken(TK_IDENTIFIER)
    else: discard

type NodeKind = enum
  NK_PROGRAM
  NK_BINARY
  NK_UNARY
  NK_GROUPING
  NK_REFERENCE
  NK_CONSTANT
  NK_DEFINE
  NK_EXPR

type Node = ref object
  line: int
  column: int

  group: Group

  case kind: NodeKind:
  of NK_PROGRAM:
    programBody: seq[Node]
  of NK_BINARY:
    binaryOperand1: Node
    binaryOperator: TokenKind
    binaryOperand2: Node
  of NK_UNARY:
    unaryOperand1: Node
    unaryOperator: TokenKind
  of NK_GROUPING:
    groupingBody: Node
  of NK_REFERENCE:
    referenceIdentifier: string
  of NK_CONSTANT:
    constantValue: Value
    constantGroup: Group
  of NK_DEFINE:
    defineIdentifier: string
    defineGroup: Group
    defineBody: Node
  of NK_EXPR:
    exprBody: Node

type Parser = ref object
  state: State

  index: int

  input: seq[Token]
  output: Node

proc newParser(input: seq[Token]): Parser =
  result = Parser()

  result.state = newState()

  result.index = 0

  result.input = input
  result.output = Node(kind: NK_PROGRAM, programBody: newSeq[Node]())

template eos(this: Parser): bool = this.index > high this.input

template read(this: Parser, l: int = 0): Token = this.input[this.index + l]

template next(this: Parser, l: int = 1): void = this.index.inc(l)

template matches(this: Parser, kinds: set[TokenKind], l: int = 0): bool = this.read(l).kind in kinds

template expects(this: Parser, kinds: set[TokenKind], body: untyped): void =
  if this.matches(kinds):
    this.next()
  else:
    body

const OPERATORS = toTable({
  TK_ASSIGN:      (bop: (precedence: 0, isRightAssociative: true),  uop: (precedence: 0)),
  TK_POW:         (bop: (precedence: 1, isRightAssociative: true),  uop: (precedence: 0)),
  TK_ADD:         (bop: (precedence: 2, isRightAssociative: false), uop: (precedence: 0)),
  TK_SUB:         (bop: (precedence: 2, isRightAssociative: false), uop: (precedence: 4)),
  TK_MUL:         (bop: (precedence: 3, isRightAssociative: false), uop: (precedence: 0)),
  TK_DIV:         (bop: (precedence: 3, isRightAssociative: false), uop: (precedence: 0)),
  TK_MOD:         (bop: (precedence: 3, isRightAssociative: false), uop: (precedence: 0)),
})

const BINARY_OPERATORS = {TK_ASSIGN, TK_POW, TK_ADD, TK_SUB, TK_MUL, TK_DIV, TK_MOD}
const UNARY_OPERATORS = {TK_SUB}

proc parseExpr(this: Parser, precedence: int = 0, primary: bool = false): Node =
  if primary:
    case this.read().kind:
    of UNARY_OPERATORS:
      result = Node(kind: NK_UNARY, line: this.read().line, column: this.read().column)

      this.expects(UNARY_OPERATORS): echo "Expected an unary operator"

      result.unaryOperator = this.read(-1).kind

      result.unaryOperand1 = this.parseExpr(precedence=OPERATORS[result.unaryOperator].uop.precedence, primary=true)
    of TK_LPAREN:
      result = Node(kind: NK_GROUPING, line: this.read().line, column: this.read().column)

      this.expects({TK_LPAREN}): echo "Expected an opening bracket"

      result.groupingBody = this.parseExpr()

      this.expects({TK_RPAREN}): echo "Expected a closing bracket"
    of TK_IDENTIFIER:
      result = Node(kind: NK_REFERENCE, line: this.read().line, column: this.read().column)

      this.expects({TK_IDENTIFIER}): echo "Expected an identifier"

      result.referenceIdentifier = this.read(-1).lexeme
    of TK_INTEGER:
      result = Node(kind: NK_CONSTANT, line: this.read().line, column: this.read().column)
    
      this.expects({TK_INTEGER}): echo "Expected an integer"

      result.constantValue = Value(kind: VK_INTEGER, integerValue: this.read(-1).lexeme.parseInt())
      
      result.constantGroup = GK_INTEGER
    of TK_STRING:
      result = Node(kind: NK_CONSTANT, line: this.read().line, column: this.read().column)
    
      this.expects({TK_STRING}): echo "Expected a string"

      result.constantValue = Value(kind: VK_STRING, stringValue: this.read(-1).lexeme)
      
      result.constantGroup = GK_STRING
    else:
      echo "Unexpected token"
  else:
    result = this.parseExpr(primary=true)

    while not this.eos() and this.matches(BINARY_OPERATORS) and OPERATORS[this.read().kind].bop.precedence >= precedence:
      result = Node(kind: NK_BINARY, line: this.read().line, column: this.read().column, binaryOperand1: result)

      this.expects(BINARY_OPERATORS): echo "Expected a binary operator"

      result.binaryOperator = this.read(-1).kind

      result.binaryOperand2 = this.parseExpr(precedence=
        if OPERATORS[result.binaryOperator].bop.isRightAssociative:
          OPERATORS[result.binaryOperator].bop.precedence + 0
        else:
          OPERATORS[result.binaryOperator].bop.precedence + 1
      )

proc parseStmt(this: Parser): Node =
  case this.read().kind:
  of TK_LET:
    result = Node(kind: NK_DEFINE, line: this.read().line, column: this.read().column)
  
    this.expects({TK_LET}): echo "Expected key word 'let'"
    this.expects({TK_IDENTIFIER}): echo "Expected an variable identifier"

    result.defineIdentifier = this.read(-1).lexeme

    this.expects({TK_COLON}): echo "Expected a colon between identifier and type"
    this.expects({TK_IDENTIFIER}): echo "Expected type identifer"
    
    result.defineGroup = GROUPS[this.read(-1).lexeme]
    
    this.expects({TK_ASSIGN}): echo "Expected an assignment operator between variable and value"
    
    result.defineBody = this.parseExpr()

    this.expects({TK_SEMI}): echo "Expected a semicolon after statement"

    this.state.lEnv.addVariable(result.defineIdentifier, Variable())
  of TK_IDENTIFIER:
    result = Node(kind: NK_EXPR, line: this.read().line, column: this.read().column)

    result.exprBody = this.parseExpr()

    this.expects({TK_SEMI}): echo "Expected a semicolon after statement"
  else: discard

proc parse(this: Parser): void =
  while not this.eos():
    this.output.programBody.add(this.parseStmt())

type TypeChecker = ref object
  state: State

  output: Node

proc newTypeChecker(input: Node, state: State): TypeChecker =
  result = TypeChecker()

  result.state = state

  result.output = input

proc check(this: TypeChecker, node: Node): Group =
  case node.kind:
  of NK_BINARY:
    let lhs = this.check(node.binaryOperand1)
    let rhs = this.check(node.binaryOperand2)
    
    if lhs == rhs:
      result = lhs
    else:
      echo "Type mismatch"
  of NK_UNARY:
    result = this.check(node.unaryOperand1)
  of NK_GROUPING:
    result = this.check(node.groupingBody)
  of NK_REFERENCE:
    if this.state.lEnv.hasVariable(node.referenceIdentifier):
      result = this.state.lEnv.getVariable(node.referenceIdentifier).group
  of NK_CONSTANT:
    result = node.constantGroup
  of NK_DEFINE:
    let rhs = this.check(node.defineBody)

    if rhs == node.defineGroup:
      result = rhs
    else:
      echo "Type mismatch"

    this.state.lEnv.getVariable(node.defineIdentifier).group = result
  of NK_EXPR:
    result = this.check(node.exprBody)
  else:
    discard

  node.group = result

proc check(this: TypeChecker): void =
  for child in this.output.programBody:
    discard this.check(child)

  this.output.group = GK_VOID

when isMainModule:
  let lexer = newLexer("let a: Int = (2 + (1 * 24)) - 52; let b: String = \"awdwad\"; let c: String = b + \"Str\";")

  lexer.tokenize()

  let parser = newParser(lexer.output)

  parser.parse()

  echo (%parser.output).pretty()

  let typeChecker = newTypeChecker(parser.output, parser.state)

  typeChecker.check()

  echo (%typeChecker.output).pretty()

