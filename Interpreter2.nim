from std/tables import TableRef, toTable, newTable, toOrderedTable, contains, pairs, `[]`, `[]=`
from std/terminal import ForegroundColor, styledWrite
from std/strutils import toUpper, split, join, alignLeft, indent, isEmptyOrWhitespace, toHex, contains
from std/sequtils import toSeq, filter, map, concat, deduplicate
from std/setutils import toSet
from std/os import commandLineParams, changeFileExt, splitFile, fileExists
from std/strformat import fmt
from std/sugar import `=>`
from std/json import `%`, `$`

import parseliteral

type TokenKind = enum
  TK_C_IDENTIFIER
  TK_C_INTEGER
  TK_C_CHARACTER
  TK_C_BOOLEAN

  # Keywords
  TK_K_MACRO
  TK_K_LABEL
  TK_K_IF
  TK_K_ELSE
  
  TK_K_WHILE

  # Arithmetic
  TK_O_ADD
  TK_O_SUB
  TK_O_MUL
  TK_O_DIV
  TK_O_MOD
  
  # Equality
  TK_O_EQUAL
  TK_O_NOT_EQUAL
  TK_O_MORE
  TK_O_MORE_THAN_EQUAL
  TK_O_LESS
  TK_O_LESS_THAN_EQUAL

  # Shift
  TK_O_SHL
  TK_O_SHR
  
  # Bitwise or Logical
  TK_O_NOT
  TK_O_AND
  TK_O_XOR
  TK_O_OR

  # Memory, Register and Sides
  TK_I_MOVE
  TK_I_READ
  TK_I_WRITE
  TK_I_LOAD
  TK_I_STORE
  
  # Arithmetic
  TK_I_ADD
  TK_I_SUB
  TK_I_MUL
  TK_I_DIV
  TK_I_MOD
  
  # Equality
  TK_I_EQUAL
  TK_I_NOT_EQUAL
  TK_I_MORE
  TK_I_MORE_THAN_EQUAL
  TK_I_LESS
  TK_I_LESS_THAN_EQUAL

  # Shift
  TK_I_SHL
  TK_I_SHR
  
  # Bitwise or Logical
  TK_I_NOT
  TK_I_AND
  TK_I_XOR
  TK_I_OR

  # Control Flow
  TK_I_GOTO
  TK_I_JUMP_FALSE
  TK_I_JUMP_TRUE

  # VM Control
  TK_I_HALT
  TK_I_NOOP

  TK_P_RBRACK
  TK_P_LBRACK
  TK_P_RBRACE
  TK_P_LBRACE
  TK_P_RPAREN
  TK_P_LPAREN

  TK_P_COMMA

type Token = ref object
  kind: TokenKind
  
  lexeme: string
  
  pos: tuple[line: int, column: int]

type Lexer = ref object
  index: int
  start: int

  line: int
  column: int

  input: string
  output: seq[Token]
  errors: int

proc newToken(
  kind: TokenKind,
  
  lexeme: string,
  
  pos: tuple[line: int, column: int]
): Token = Token(kind: kind, lexeme: lexeme, pos: pos)

proc `$`(this: Token): string = fmt("({this.kind}, {this.lexeme}, {this.pos.line}:{this.pos.column})")

proc newLexer(
  input: string
): Lexer = Lexer(index: 0, start: 0, line: 1, column: 1, input: input, output: newSeq[Token](), errors: 0)

template eos(this: Lexer, offset: int = 0): bool = (this.index + offset > high this.input)
template get(this: Lexer, offset: int = 0): char = (if this.eos(offset): '\0' else: this.input[this.index + offset])
template jyn(this: Lexer): void = (this.start = this.index)
template nxl(this: Lexer): void = (this.index.inc(); this.line.inc(); this.column = 1)
template nxc(this: Lexer): void = (this.index.inc(); this.column.inc())
template bak(this: Lexer): void = (this.index.dec(); this.column.dec())
template txt(this: Lexer, offset: int = -1): string = (this.input[this.start..this.index + offset])
template add(this: Lexer, kind: TokenKind): void = (this.output.add(newToken(kind, this.txt(), (this.line, this.column - this.txt().len()))))
template err(this: Lexer, msg: string = "Unexpected character"): void = (stderr.styledWrite(fgRed, "[Lexer]: ", msg, " '", $this.get(), "' at ", $this.line, ":", $this.column, "\n"); this.errors.inc())

const RESERVED_LEXEMES = toTable({
  "TRUE":             TK_C_BOOLEAN,
  "FALSE":            TK_C_BOOLEAN,
  "MACRO":            TK_K_MACRO,
  "LABEL":            TK_K_LABEL,
  "IF":               TK_K_IF,
  "ELSE":             TK_K_ELSE,
  "WHILE":            TK_K_WHILE,
  "+":                TK_O_ADD,
  "-":                TK_O_SUB,
  "*":                TK_O_MUL,
  "/":                TK_O_DIV,
  "%":                TK_O_MOD,
  "==":               TK_O_EQUAL,
  "!=":               TK_O_NOT_EQUAL,
  ">":                TK_O_MORE,
  ">=":               TK_O_MORE_THAN_EQUAL,
  "<":                TK_O_LESS,
  "<=":               TK_O_LESS_THAN_EQUAL,
  "<<":               TK_O_SHL,
  ">>":               TK_O_SHR,
  "!":                TK_O_NOT,
  "&":                TK_O_AND,
  "^":                TK_O_XOR,
  "|":                TK_O_OR,
  "MOVE":             TK_I_MOVE,
  "READ":             TK_I_READ,
  "WRITE":            TK_I_WRITE,
  "LOAD":             TK_I_LOAD,
  "STORE":            TK_I_STORE,
  "ADD":              TK_I_ADD,
  "SUB":              TK_I_SUB,
  "MUL":              TK_I_MUL,
  "DIV":              TK_I_DIV,
  "MOD":              TK_I_MOD,
  "EQUAL":            TK_I_EQUAL,
  "NOT_EQUAL":        TK_I_NOT_EQUAL,
  "MORE":             TK_I_MORE,
  "MORE_THAN_EQUAL":  TK_I_MORE_THAN_EQUAL,
  "LESS":             TK_I_LESS,
  "LESS_THAN_EQUAL":  TK_I_LESS_THAN_EQUAL,
  "SHL":              TK_I_SHL,
  "SHR":              TK_I_SHR,
  "NOT":              TK_I_NOT,
  "AND":              TK_I_AND,
  "XOR":              TK_I_XOR,
  "OR":               TK_I_OR,
  "GOTO":             TK_I_GOTO,
  "JUMP_FALSE":       TK_I_JUMP_FALSE,
  "JUMP_TRUE":        TK_I_JUMP_TRUE,
  "HALT":             TK_I_HALT,
  "NOOP":             TK_I_NOOP,
  "[":                TK_P_LBRACK,
  "]":                TK_P_RBRACK,
  "{":                TK_P_LBRACE,
  "}":                TK_P_RBRACE,
  "(":                TK_P_LPAREN,
  ")":                TK_P_RPAREN,
  ",":                TK_P_COMMA,
})

const SYMBOL_CHARACTERS = RESERVED_LEXEMES.pairs()
                                          .toSeq()
                                          .filter(x => not x[0].contains({'a'..'z', 'A'..'Z', '0'..'9'}))
                                          .map(x => x[0].toSeq())
                                          .concat()
                                          .deduplicate()
                                          .toSet()

proc tokenize(this: Lexer): void =
  while not this.eos():
    this.jyn()

    case this.get():
    of {'\n'}:
      this.nxl()
    of {' ', '\r', '\t'}:
      this.nxc()
    of '#':
      while this.get() != '\n':
        this.nxc()
    of '\'':
      this.nxc()
      while this.get() notin {'\x00'..'\x1F', '\x7F'..'\xFF', '\''}:
        if this.get() == '\\':
          this.nxc()

          case this.get():
          of {'\\', '\'', '\"', 'n', 'r', 't', 'b', 'f'}:
            this.nxc()
          of {'x', '0'..'9'}:
            this.nxc()

            while this.get() in {'0'..'9', 'a'..'f', 'A'..'F'}:
              this.nxc()
          else:
            this.err()
            this.nxc()
        elif this.get() in {'\x20'..'\x7E'}:
          this.nxc()
        else:
            this.err()
            this.nxc()
      this.nxc()

      this.add(TK_C_CHARACTER)
    of SYMBOL_CHARACTERS:
      while this.get() in SYMBOL_CHARACTERS:
        this.nxc()

      while this.txt() notin RESERVED_LEXEMES and this.start < this.index:
        this.bak()

      if this.txt() in RESERVED_LEXEMES:
        this.add(RESERVED_LEXEMES[this.txt()])
      else:
        this.err()
        this.nxc()
    of {'0'..'9'}:
      while this.get() in {'0'..'9', 'o', 'O', 'x', 'X', 'b', 'B', 'a'..'f', 'A'..'F', '_'}:
        this.nxc()

      this.add(TK_C_INTEGER)
    of {'a'..'z', 'A'..'Z'}:
      while this.get() in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
        this.nxc()

      if this.txt().toUpper() in RESERVED_LEXEMES:
        this.add(RESERVED_LEXEMES[this.txt().toUpper()])
      else:
        this.add(TK_C_IDENTIFIER)
    else:
      this.err()
      this.nxc()

type NodeKind = enum
  NK_BLOCK

  NK_MACRO
  NK_LABEL
  NK_IF
  NK_WHILE

  NK_INSTRUCTION
  
  NK_BINARY
  NK_UNARY

  NK_CONSTANT
  NK_REGISTER

  NK_MACRO_CALL
  NK_LABEL_REF

type Node = ref object
  pos: tuple[line: int, column: int]

  case kind: NodeKind:
  of NK_BLOCK:
    blockBody: seq[Node]
  of NK_MACRO:
    macroName: string
    macroBody: Node
  of NK_LABEL:
    labelName: string
  of NK_IF:
    ifCondition: Node
    thenBody: Node
    elseBody: Node
  of NK_WHILE:
    whileCondition: Node
    whileBody: Node
  of NK_INSTRUCTION:
    instructionIdentifier: TokenKind
    instructionArguments: seq[Node]
  of NK_BINARY:
    bopOperand1: Node
    bopOperator: TokenKind
    bopOperand2: Node
  of NK_UNARY:
    uopOperator: TokenKind
    uopOperand1: Node
  of NK_CONSTANT:
    case constantKind: TokenKind:
    of TK_C_INTEGER:
      integerValue: int
    of TK_C_CHARACTER:
      characterValue: char
    of TK_C_BOOLEAN:
      booleanValue: bool
    else: discard
  of NK_REGISTER:
    registerAddress: int
  of NK_MACRO_CALL:
    macroCallName: string
  of NK_LABEL_REF:
    labelRefName: string

type State = ref object
  env: TableRef[string, int]
  macros: seq[Node]

type Parser = ref object
  index: int

  state: State

  last: Token

  input: seq[Token]
  output: Node
  errors: int

proc newBlockNode(
  pos: tuple[line: int, column: int] = (0, 0),
  
  body: seq[Node] = newSeq[Node]()
): Node = Node(pos: pos, kind: NK_BLOCK, blockBody: body)

proc newMacroNode(
  pos: tuple[line: int, column: int],
  
  name: string = "",
  body: Node = nil,
): Node = Node(pos: pos, kind: NK_MACRO, macroName: name, macroBody: body)

proc newLabelNode(
  pos: tuple[line: int, column: int],
  
  name: string = "",
): Node = Node(pos: pos, kind: NK_LABEL, labelName: name)

proc newIfNode(
  pos: tuple[line: int, column: int],
  
  condition: Node = nil,
  thenBody: Node = nil,
  elseBody: Node = nil,
): Node = Node(pos: pos, kind: NK_IF, ifCondition: condition, thenBody: thenBody, elseBody: elseBody)

proc newWhileNode(
  pos: tuple[line: int, column: int],
  
  condition: Node = nil,
  body: Node = nil,
): Node = Node(pos: pos, kind: NK_WHILE, whileCondition: condition, whileBody: body)

proc newInstructionNode(
  pos: tuple[line: int, column: int],
  
  identifier: TokenKind = TK_I_NOOP,
  arguments: seq[Node] = newSeq[Node]()
): Node = Node(pos: pos, kind: NK_INSTRUCTION, instructionIdentifier: identifier, instructionArguments: arguments)

proc newBinaryNode(
  pos: tuple[line: int, column: int],

  operand1: Node = nil,
  operator: TokenKind = TK_O_ADD,
  operand2: Node = nil
): Node = Node(pos: pos, kind: NK_BINARY, bopOperand1: operand1, bopOperator: operator, bopOperand2: operand2)

proc newUnaryNode(
  pos: tuple[line: int, column: int],

  operator: TokenKind = TK_O_ADD,
  operand1: Node = nil,
): Node = Node(pos: pos, kind: NK_UNARY, uopOperator: operator, uopOperand1: operand1)

proc newIntegerConstantNode(
  pos: tuple[line: int, column: int],
  
  value: int = 0
): Node = Node(pos: pos, kind: NK_CONSTANT, constantKind: TK_C_INTEGER, integerValue: value)

proc newCharacterConstantNode(
  pos: tuple[line: int, column: int],
  
  value: char = '\0'
): Node = Node(pos: pos, kind: NK_CONSTANT, constantKind: TK_C_CHARACTER, characterValue: value)

proc newBooleanConstantNode(
  pos: tuple[line: int, column: int],
  
  value: bool = false
): Node = Node(pos: pos, kind: NK_CONSTANT, constantKind: TK_C_BOOLEAN, booleanValue: value)

proc newRegisterNode(
  pos: tuple[line: int, column: int],
  
  address: int = 0
): Node = Node(pos: pos, kind: NK_REGISTER, registerAddress: address)

proc newMacroCallNode(
  pos: tuple[line: int, column: int],
  
  name: string = ""
): Node = Node(pos: pos, kind: NK_MACRO_CALL, macroCallName: name)

proc newLabelRefNode(
  pos: tuple[line: int, column: int],
  
  name: string = ""
): Node = Node(pos: pos, kind: NK_LABEL_REF, labelRefName: name)

proc newState(): State = State(env: newTable[string, int](), macros: newSeq[Node]())

proc newParser(
  input: seq[Token]
): Parser = Parser(index: 0, last: input[0], state: newState(), input: input, output: newBlockNode(), errors: 0)

template eos(this: Parser, offset: int = 0): bool = (this.index + offset > high this.input)
template get(this: Parser, offset: int = 0): Token = (this.input[this.index + offset])
template nxt(this: Parser): void = (this.index.inc(); (if not this.eos(): this.last = this.get()))
template err(this: Parser, msg: string = "Unexpected token"): void = (stderr.styledWrite(fgRed, "[Parser]: ", $msg, ", received '", $this.last.lexeme, "' at ", $this.last.pos.line, ":", $this.last.pos.column, "\n"); this.errors.inc())
template mch(this: Parser, kinds: set[TokenKind]): bool = (this.get().kind in kinds)
template pnk(this: Parser, kinds: set[TokenKind]): void = (while not this.eos() and not this.mch(kinds): this.nxt())
template xpc(this: Parser, kinds: set[TokenKind], msg: string): void = (if not this.eos() and this.mch(kinds): (this.nxt()) else: (this.err(msg); this.pnk(kinds)))

const INSTRUCTION_ARITIES = toTable({
  TK_I_MOVE:            2,
  TK_I_READ:            2,
  TK_I_WRITE:           2,
  TK_I_LOAD:            2,
  TK_I_STORE:           2,
  TK_I_ADD:             3,
  TK_I_SUB:             3,
  TK_I_MUL:             3,
  TK_I_DIV:             3,
  TK_I_MOD:             3,
  TK_I_EQUAL:           3,
  TK_I_NOT_EQUAL:       3,
  TK_I_MORE:            3,
  TK_I_MORE_THAN_EQUAL: 3,
  TK_I_LESS:            3,
  TK_I_LESS_THAN_EQUAL: 3,
  TK_I_SHL:             3,
  TK_I_SHR:             3,
  TK_I_NOT:             2,
  TK_I_AND:             3,
  TK_I_XOR:             3,
  TK_I_OR:              3,
  TK_I_GOTO:            1,
  TK_I_JUMP_FALSE:      2,
  TK_I_JUMP_TRUE:       2,
  TK_I_HALT:            0,
  TK_I_NOOP:            0,
})

const OPERATOR_PROPERTIES = toTable({
  TK_O_ADD:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_SUB:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_MUL:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_DIV:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_MOD:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_EQUAL:             (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_NOT_EQUAL:         (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_MORE:              (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_MORE_THAN_EQUAL:   (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_LESS:              (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_LESS_THAN_EQUAL:   (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_SHL:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_SHR:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_NOT:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_AND:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_XOR:               (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
  TK_O_OR:                (bop: (isRightAssociative: false, precedence: 0), uop: (precedence: 0)),
})

const INSTRUCTIONS = {TK_I_MOVE..TK_I_NOOP}
const BOPERATORS = {TK_O_ADD..TK_O_OR}
const UOPERATORS = {TK_O_ADD, TK_O_SUB, TK_O_NOT}

proc parseExpr(this: Parser, precedence: int = 0, primary: bool = false): Node =
  if this.eos():
    this.err("Could not parse expression, End of stream")
    return nil

  if primary:
    case this.get().kind:
    of {TK_C_INTEGER}:
      result = newIntegerConstantNode(this.last.pos)

      this.xpc({TK_C_INTEGER}, "Expected an integer literal")

      if this.get(-1).lexeme.isInteger():
        result.integerValue = this.get(-1).lexeme.asInteger()
      else:
        this.err("Failed to parse integer")
        result.integerValue = 0
    of TK_C_CHARACTER:
      result = newCharacterConstantNode(this.last.pos)

      this.xpc({TK_C_CHARACTER}, "Expected an character literal")
      
      if this.get(-1).lexeme.isCharacter():
        result.characterValue = this.get(-1).lexeme.asCharacter()
      else:
        this.err("Failed to parse character")
        result.characterValue = '\0'
    of TK_C_BOOLEAN:
      result = newBooleanConstantNode(this.last.pos)

      this.xpc({TK_C_BOOLEAN}, "Expected an boolean literal")
      
      result.booleanValue = this.get(-1).lexeme.toUpper() == "TRUE"
    of UOPERATORS:
      result = newUnaryNode(this.last.pos)

      this.xpc(UOPERATORS, "Expected an unary operator")

      result.uopOperator = this.get(-1).kind

      result.uopOperand1 = this.parseExpr(precedence=OPERATOR_PROPERTIES[result.uopOperator].uop.precedence)
    of TK_P_LBRACK:
      result = newRegisterNode(this.last.pos)

      this.xpc({TK_P_LBRACK}, "Expected an opening bracket before register")
      this.xpc({TK_C_INTEGER}, "Expected a register address")

      if this.get(-1).lexeme.isInteger():
        result.registerAddress = this.get(-1).lexeme.asInteger()
      else:
        this.err("Failed to parse integer")
        result.registerAddress = 0

      this.xpc({TK_P_RBRACK}, "Expected a closing bracket after register")
    of TK_C_IDENTIFIER:
      result = newLabelRefNode(this.last.pos)

      this.xpc({TK_C_IDENTIFIER}, "Expected an identifier")
      
      result.labelRefName = this.get(-1).lexeme
    else:
      this.err()
      this.pnk({TK_C_INTEGER, TK_C_CHARACTER, TK_C_BOOLEAN, TK_P_LBRACK})
      return this.parseExpr()
  else:
    result = this.parseExpr(primary=true)

    while not this.eos() and this.mch(BOPERATORS) and OPERATOR_PROPERTIES[this.get().kind].bop.precedence >= precedence:
      result = newBinaryNode(this.last.pos, operand1=result)

      this.xpc(BOPERATORS, "Expected a binary operator")

      result.bopOperator = this.get(-1).kind

      result.bopOperand2 = this.parseExpr(precedence=
        if OPERATOR_PROPERTIES[result.bopOperator].bop.isRightAssociative:
          OPERATOR_PROPERTIES[result.bopOperator].bop.precedence + 0
        else:
          OPERATOR_PROPERTIES[result.bopOperator].bop.precedence + 1
      )

proc parseStmt(this: Parser): Node =
  if this.eos():
    this.err("Could not parse statement, End of stream")
    return nil

  case this.get().kind:
  of TK_P_LBRACE:
    result = newBlockNode(this.last.pos)

    this.xpc({TK_P_LBRACE}, "Expected an opening bracket")

    while not this.eos() and not this.mch({TK_P_RBRACE}):
      result.blockBody.add(this.parseStmt())

    this.xpc({TK_P_RBRACE}, "Expected a closing bracket")
  of TK_K_MACRO:
    result = newMacroNode(this.last.pos)

    this.xpc({TK_K_MACRO}, "Expected keyword 'macro'")
    this.xpc({TK_C_IDENTIFIER}, "Expected keyword 'identifier'")

    result.macroName = this.get(-1).lexeme
    
    this.state.macros.add(result)
    this.state.env[result.macroName] = high this.state.macros

    result.macroBody = this.parseStmt()
  of TK_K_LABEL:
    result = newLabelNode(this.last.pos)

    this.xpc({TK_K_LABEL}, "Expected keyword 'label'")
    this.xpc({TK_C_IDENTIFIER}, "Expected keyword 'identifier'")

    result.labelName = this.get(-1).lexeme
    
    this.state.env[result.labelName] = 0
  of TK_K_IF:
    result = newIfNode(this.last.pos)

    this.xpc({TK_K_IF}, "Expected keyword 'if'")
    
    result.ifCondition = this.parseExpr()
    result.thenBody = this.parseStmt()

    if this.mch({TK_K_ELSE}):
      this.xpc({TK_K_ELSE}, "Expected keyword 'else'")

      result.elseBody = this.parseStmt()
    else:
      result.elseBody = newBlockNode()
  of TK_K_WHILE:
    result = newWhileNode(this.last.pos)

    this.xpc({TK_K_WHILE}, "Expected keyword 'while'")
    
    result.whileCondition = this.parseExpr()
    result.whileBody = this.parseStmt()
  of INSTRUCTIONS:
    result = newInstructionNode(this.last.pos)

    this.xpc(INSTRUCTIONS, "Expected an instruction name")

    result.instructionIdentifier = this.get(-1).kind

    let arity = INSTRUCTION_ARITIES[result.instructionIdentifier]

    for i in 1..arity:
      result.instructionArguments.add(this.parseExpr())

      if i < arity:
        this.xpc({TK_P_COMMA}, "Expected a comma between arguments")
  of TK_C_IDENTIFIER:
    result = newMacroCallNode(this.last.pos)

    this.xpc({TK_C_IDENTIFIER}, "Expected an identifier")
    
    result.macroCallName = this.get(-1).lexeme

    this.xpc({TK_P_LPAREN}, "Expected an opening parenthesis before macro arguments")
    this.xpc({TK_P_RPAREN}, "Expected a closing parenthesis after macro arguments")
  else:
    this.err()
    this.pnk(INSTRUCTIONS)
    return this.parseStmt()

proc parse(this: Parser): void =
  while not this.eos():
    this.output.blockBody.add(this.parseStmt())

type Compiler = ref object
  address: int

  state: State

  input: Node
  output: Node
  errors: int

proc newCompiler(
  state: State,

  input: Node
): Compiler = Compiler(address: 0, state: state, input: input, output: newBlockNode(), errors: 0)

template nxt(this: Compiler): void = (this.address.inc())
template err(this: Compiler, pos: tuple[line: int, column: int], msg: string = "Unexpected node"): void = (stderr.styledWrite(fgRed, "[Compiler]: ", msg, " at ", $pos.line, ":", $pos.column, "\n"); this.errors.inc())

proc compile(this: Compiler, node: Node): Node =
  result = node

  case node.kind:
  of NK_BLOCK:
    result = newBlockNode(node.pos)

    for child in node.blockBody:
      let compiled = this.compile(child)
      
      if compiled != nil:
        result.blockBody.add(compiled)
  of NK_LABEL:
    result = nil

    this.state.env[node.labelName] = this.address
  of NK_MACRO:
    result = nil
  of NK_IF:
    result = newBlockNode(node.pos)    

    let ifCondition = this.compile(node.ifCondition)
    
    let ifJump = newInstructionNode(node.ifCondition.pos, TK_I_JUMP_FALSE)
    ifJump.instructionArguments.add(ifCondition)
    this.nxt()
    
    let thenBody = this.compile(node.thenBody)

    let ifGoto = newInstructionNode(node.ifCondition.pos, TK_I_GOTO)
    this.nxt()

    ifJump.instructionArguments.add(newIntegerConstantNode(ifCondition.pos, this.address))

    let elseBody = this.compile(node.elseBody)
    
    ifGoto.instructionArguments.add(newIntegerConstantNode(ifCondition.pos, this.address))
    
    result.blockBody.add(ifJump)
    result.blockBody.add(thenBody)
    result.blockBody.add(ifGoto)
    result.blockBody.add(elseBody)
  of NK_WHILE:
    result = newBlockNode(node.pos)
    
    let condition = this.compile(node.whileCondition)
    
    let label = newLabelNode(node.whileCondition.pos, $this.address)
    this.state.env[label.labelName] = this.address
    this.nxt()

    let jump = newInstructionNode(node.whileCondition.pos, TK_I_JUMP_FALSE)
    jump.instructionArguments.add(condition)
    this.nxt()

    let goto = newInstructionNode(node.whileCondition.pos, TK_I_GOTO)
    goto.instructionArguments.add(newLabelRefNode(node.whileCondition.pos, label.labelName))
    this.nxt()
    
    let main = this.compile(node.whileBody)

    jump.instructionArguments.add(newIntegerConstantNode(condition.pos, this.address))
    
    result.blockBody.add(jump)
    result.blockBody.add(main)
    result.blockBody.add(goto)

    result = this.compile(result)
  of NK_INSTRUCTION:
    if node.instructionIdentifier == TK_I_NOOP:
      result = nil
    else:
      this.nxt()

    for i, arg in node.instructionArguments:
      node.instructionArguments[i] = this.compile(arg)
  of NK_MACRO_CALL:
    result = this.compile(this.state.macros[this.state.env[node.macroCallName]].macroBody)
  of NK_LABEL_REF:
    result = newIntegerConstantNode(node.pos)

    result.integerValue = this.state.env[node.labelRefName]
  else: discard

proc compile(this: Compiler): void = this.output = this.compile(this.input)

type Assembler = ref object
  input: Node
  output: seq[byte]
  errors: int

proc newAssembler(
  input: Node
): Assembler = Assembler(input: input, output: newSeq[byte](), errors: 0)

template err(this: Assembler, pos: tuple[line: int, column: int], msg: string = "Unexpected node"): void = (stderr.styledWrite(fgRed, "[Assembler]: ", msg, " at ", $pos.line, ":", $pos.column, "\n"); this.errors.inc())

const OPCODES = toTable({
  TK_I_MOVE:            byte(0x00),
  TK_I_READ:            byte(0x01),
  TK_I_WRITE:           byte(0x02),
  TK_I_LOAD:            byte(0x03),
  TK_I_STORE:           byte(0x04),
  TK_I_ADD:             byte(0x10),
  TK_I_SUB:             byte(0x11),
  TK_I_MUL:             byte(0x12),
  TK_I_DIV:             byte(0x13),
  TK_I_MOD:             byte(0x14),
  TK_I_EQUAL:           byte(0x40),
  TK_I_NOT_EQUAL:       byte(0x41),
  TK_I_MORE:            byte(0x42),
  TK_I_MORE_THAN_EQUAL: byte(0x43),
  TK_I_LESS:            byte(0x44),
  TK_I_LESS_THAN_EQUAL: byte(0x45),
  TK_I_SHL:             byte(0x20),
  TK_I_SHR:             byte(0x21),
  TK_I_NOT:             byte(0x22),
  TK_I_AND:             byte(0x23),
  TK_I_XOR:             byte(0x24),
  TK_I_OR:              byte(0x25),
  TK_I_GOTO:            byte(0x30),
  TK_I_JUMP_FALSE:      byte(0x31),
  TK_I_JUMP_TRUE:       byte(0x32),
  TK_I_HALT:            byte(0xFE),
  TK_I_NOOP:            byte(0xFF),
})

const INSTRUCTION_WIDTH = 4
const BOOLEAN_FALSE = byte(0)
const BOOLEAN_TRUE = byte(1)

proc assemble(this: Assembler, node: Node): seq[byte] =
  result = newSeq[byte]()

  case node.kind:
  of NK_BLOCK:
    for child in node.blockBody:
      result.add(this.assemble(child))
  of NK_INSTRUCTION:
    result.add(OPCODES[node.instructionIdentifier])
    
    for arg in node.instructionArguments:
      result.add(this.assemble(arg))
    
    while result.len() < INSTRUCTION_WIDTH:
      result.add(0x00)
  of NK_CONSTANT:
    case node.constantKind:
    of TK_C_INTEGER:    result.add(byte(node.integerValue))
    of TK_C_CHARACTER:  result.add(byte(node.characterValue))
    of TK_C_BOOLEAN:    result.add(if node.booleanValue: BOOLEAN_TRUE else: BOOLEAN_FALSE)
    else:               discard
  of NK_REGISTER:
    result.add(byte(node.registerAddress))
  else: this.err(node.pos)

proc assemble(this: Assembler): void = this.output = this.assemble(this.input)

type VirtualMachine = ref object
  memory: seq[byte]
  registers: seq[byte]
  sides: seq[byte]

  input: seq[byte]
  errors: int

proc newVirtualMachine(
  input: seq[byte]
): VirtualMachine = VirtualMachine(memory: newSeq[byte](0xFF), registers: newSeq[byte](0x0F), sides: newSeq[byte](0x06), input: input, errors: 0)

const RIP = byte(0x0E)

template rset(this: VirtualMachine, reg: SomeInteger, val: SomeInteger): void = this.registers[reg] = byte(val)
template mset(this: VirtualMachine, mem: SomeInteger, val: SomeInteger): void = this.memory[mem] = byte(val)
template sset(this: VirtualMachine, sid: SomeInteger, val: SomeInteger): void = this.sides[sid] = byte(val)

template rget(this: VirtualMachine, reg: SomeInteger): SomeInteger = this.registers[reg]
template mget(this: VirtualMachine, mem: SomeInteger): SomeInteger = this.memory[mem]
template sget(this: VirtualMachine, sid: SomeInteger): SomeInteger = this.sides[sid]
template iget(this: VirtualMachine, ind: SomeInteger): SomeInteger = this.input[ind]

template eos(this: VirtualMachine): bool = int(this.rget(RIP)) > high this.input

template opcode(this: VirtualMachine): SomeInteger = this.iget(this.rget(RIP) - 4)
template opand1(this: VirtualMachine): SomeInteger = this.iget(this.rget(RIP) - 3)
template opand2(this: VirtualMachine): SomeInteger = this.iget(this.rget(RIP) - 2)
template opand3(this: VirtualMachine): SomeInteger = this.iget(this.rget(RIP) - 1)

template err(this: VirtualMachine, msg: string): void = (stderr.styledWrite(fgRed, "[Interpreter]: ", msg, ", received", $this.opcode(), " at ", $this.rget(RIP), "\n"); this.errors.inc())
template say(this: VirtualMachine, msg: varargs[string]): void = (stdout.styledWrite(fgBlue, "[Debugger]: ", msg.join(), "\n"))

template execute(this: VirtualMachine): void =
  case this.opcode():
  of OPCODES[TK_I_MOVE]:            this.rset(this.opand1(), this.opand2())
  of OPCODES[TK_I_READ]:            this.rset(this.opand1(), this.sget(this.opand2()))
  of OPCODES[TK_I_WRITE]:           this.sset(this.opand1(), this.rget(this.opand2()))
  of OPCODES[TK_I_LOAD]:            this.rset(this.opand1(), this.mget(this.opand2()))
  of OPCODES[TK_I_STORE]:           this.mset(this.opand1(), this.rget(this.opand2()))
  of OPCODES[TK_I_ADD]:             this.rset(this.opand1(), this.rget(this.opand2())  +  this.rget(this.opand3())) 
  of OPCODES[TK_I_SUB]:             this.rset(this.opand1(), this.rget(this.opand2())  -  this.rget(this.opand3()))   
  of OPCODES[TK_I_MUL]:             this.rset(this.opand1(), this.rget(this.opand2())  *  this.rget(this.opand3()))   
  of OPCODES[TK_I_DIV]:             this.rset(this.opand1(), this.rget(this.opand2()) div this.rget(this.opand3()))   
  of OPCODES[TK_I_MOD]:             this.rset(this.opand1(), this.rget(this.opand2()) mod this.rget(this.opand3()))   
  of OPCODES[TK_I_EQUAL]:           this.rset(this.opand1(), if this.rget(this.opand2()) == this.rget(this.opand3()): BOOLEAN_TRUE else: BOOLEAN_FALSE)
  of OPCODES[TK_I_NOT_EQUAL]:       this.rset(this.opand1(), if this.rget(this.opand2()) != this.rget(this.opand3()): BOOLEAN_TRUE else: BOOLEAN_FALSE)
  of OPCODES[TK_I_MORE]:            this.rset(this.opand1(), if this.rget(this.opand2()) >  this.rget(this.opand3()): BOOLEAN_TRUE else: BOOLEAN_FALSE)
  of OPCODES[TK_I_MORE_THAN_EQUAL]: this.rset(this.opand1(), if this.rget(this.opand2()) >= this.rget(this.opand3()): BOOLEAN_TRUE else: BOOLEAN_FALSE)
  of OPCODES[TK_I_LESS]:            this.rset(this.opand1(), if this.rget(this.opand2()) <  this.rget(this.opand3()): BOOLEAN_TRUE else: BOOLEAN_FALSE)
  of OPCODES[TK_I_LESS_THAN_EQUAL]: this.rset(this.opand1(), if this.rget(this.opand2()) <= this.rget(this.opand3()): BOOLEAN_TRUE else: BOOLEAN_FALSE)
  of OPCODES[TK_I_SHL]:             this.rset(this.opand1(), this.rget(this.opand2()) shl this.rget(this.opand3()))
  of OPCODES[TK_I_SHR]:             this.rset(this.opand1(), this.rget(this.opand2()) shr this.rget(this.opand3()))
  of OPCODES[TK_I_NOT]:             this.rset(this.opand1(),                          not this.rget(this.opand2()))
  of OPCODES[TK_I_AND]:             this.rset(this.opand1(), this.rget(this.opand1()) and this.rget(this.opand3()))
  of OPCODES[TK_I_XOR]:             this.rset(this.opand1(), this.rget(this.opand1()) xor this.rget(this.opand3()))
  of OPCODES[TK_I_OR]:              this.rset(this.opand1(), this.rget(this.opand1()) or  this.rget(this.opand3()))
  of OPCODES[TK_I_GOTO]:            this.rset(RIP, this.opand1() * INSTRUCTION_WIDTH)
  of OPCODES[TK_I_JUMP_FALSE]:      this.rset(RIP, if this.rget(this.opand1()) == BOOLEAN_FALSE: this.opand2() * INSTRUCTION_WIDTH else: this.rget(RIP))
  of OPCODES[TK_I_JUMP_TRUE]:       this.rset(RIP, if this.rget(this.opand1()) == BOOLEAN_TRUE:  this.opand2() * INSTRUCTION_WIDTH else: this.rget(RIP))
  of OPCODES[TK_I_HALT]:            this.rset(RIP, 0xFF)
  of OPCODES[TK_I_NOOP]:            discard
  else: this.err("No such opcode")

proc interpret(this: VirtualMachine): void =
  while not this.eos():
    this.rget(RIP).inc(INSTRUCTION_WIDTH)
    this.execute()

const OPNAMES = toTable({
  OPCODES[TK_I_MOVE]:             "MOVE",
  OPCODES[TK_I_READ]:             "READ",
  OPCODES[TK_I_WRITE]:            "WRITE",
  OPCODES[TK_I_LOAD]:             "LOAD",
  OPCODES[TK_I_STORE]:            "STORE",
  OPCODES[TK_I_ADD]:              "ADD",
  OPCODES[TK_I_SUB]:              "SUB",
  OPCODES[TK_I_MUL]:              "MUL",
  OPCODES[TK_I_DIV]:              "DIV",
  OPCODES[TK_I_MOD]:              "MOD",
  OPCODES[TK_I_EQUAL]:            "EQUAL",
  OPCODES[TK_I_NOT_EQUAL]:        "NOT_EQUAL",
  OPCODES[TK_I_MORE]:             "MORE",
  OPCODES[TK_I_MORE_THAN_EQUAL]:  "MORE_THAN_EQUAL",
  OPCODES[TK_I_LESS]:             "LESS",
  OPCODES[TK_I_LESS_THAN_EQUAL]:  "LESS_THAN_EQUAL",
  OPCODES[TK_I_SHL]:              "SHL",
  OPCODES[TK_I_SHR]:              "SHR",
  OPCODES[TK_I_NOT]:              "NOT",
  OPCODES[TK_I_AND]:              "AND",
  OPCODES[TK_I_XOR]:              "XOR",
  OPCODES[TK_I_OR]:               "OR",
  OPCODES[TK_I_GOTO]:             "GOTO",
  OPCODES[TK_I_JUMP_FALSE]:       "JUMP_FALSE",
  OPCODES[TK_I_JUMP_TRUE]:        "JUMP_TRUE",
  OPCODES[TK_I_HALT]:             "HALT",
  OPCODES[TK_I_NOOP]:             "NOOP",
})

proc debug(this: VirtualMachine): void =
  while true:
    let input = stdin.readLine().split(" ")
  
    case input[0]:
    of "r", "run":
      if not this.eos():
        this.rget(RIP).inc(INSTRUCTION_WIDTH)
        this.execute()
      else:
        this.say("End of stream, use (s)tart to return to start")
    of "R", "Run":
      this.interpret()
    of "t", "start":
      this.rset(RIP, 0)
    of "d", "disassemble":
      if not this.eos():
        this.rget(RIP).inc(INSTRUCTION_WIDTH)
        this.say("Instruction at ", $((this.rget(RIP) - 1) div INSTRUCTION_WIDTH).toHex(), ": ", OPNAMES[this.opcode()].alignLeft(12), " ", $this.opand1().toHex(), " ", $this.opand2().toHex(), " ", $this.opand3().toHex())
        this.rget(RIP).dec(INSTRUCTION_WIDTH)
    of "D", "Disassemble":
      let ip = this.rget(RIP)
      this.rset(RIP, 0)
      for i in 0..(high this.input) div INSTRUCTION_WIDTH:
        this.rget(RIP).inc(INSTRUCTION_WIDTH)
        this.say($((this.rget(RIP) - 1) div INSTRUCTION_WIDTH).toHex(), " = ", OPNAMES[this.opcode()].alignLeft(12), " ", $this.opand1().toHex(), " ", $this.opand2().toHex(), " ", $this.opand3().toHex())
      this.rset(RIP, ip)
    of "g", "get":
      if input.len() < 3 or not input[2].isInteger():
        this.say("Expected form (g)et [(r)egister | (m)emory | (s)ide] [index:byte]")
        continue

      case input[1]:
      of "r", "register":
        let i = input[2].asInteger()

        if i in {0x00..0x0E}:
          this.say("Value of register ", byte(i).toHex(), " = ", $this.rget(byte(i)).toHex())
        else:
          this.say("Index must be in interval [0x00, 0x0F)")
      of "m", "memory":
        let i = input[2].asInteger()
          
        if i in {0x00..0xFE}:
          this.say("Value of memory at ", byte(i).toHex(), " = ", $this.mget(byte(i)).toHex())
        else:
          this.say("Index must be in interval [0x00, 0xFF)")
      of "s", "side":
        let i = input[2].asInteger()

        if i in {0x00..0x05}:
          this.say("Value of side ", byte(i).toHex(), " = ", $this.sget(byte(i)).toHex())
        else:
          this.say("Index must be in interval [0x00, 0x06)")
      else:
        this.say("Expected form (g)et [(r)egister | (m)emory | (s)ide] [index:byte]")
    of "s", "set":
      if input.len() < 4 or not input[2].isInteger() or not input[3].isInteger():
        this.say("Expected form (s)et [(r)egister | (m)emory | (s)ide] [index:byte] [value:byte]")
        continue

      case input[1]:
      of "r", "register":
        let i = input[2].asInteger()
        let v = input[3].asInteger()
        
        if i in {0x00..0x0E} and v in {0x00..0xFE}:
          this.rset(byte(i), byte(v))
        else:
          this.say("Index must be in interval [0x00, 0x0F), value must be in interval [0x00, 0xFF)")
      of "m", "memory":
        let i = input[2].asInteger()
        let v = input[3].asInteger()
        
        if i in {0x00..0xFE} and v in {0x00..0xFE}:
          this.mset(byte(i), byte(v))
        else:
          this.say("Index must be in interval [0x00, 0xFF), value must be in interval [0x00, 0xFF)")
      of "s", "side":
        let i = input[2].asInteger()
        let v = input[3].asInteger()

        if i in {0x00..0x05} and v in {0x00..0xFE}:
          this.sset(byte(i), byte(v))
        else:
          this.say("Index must be in interval [0x00, 0x06), value must be in interval [0x00, 0xFF)")
      else:
        this.say("Expected form (s)et [(r)egister | (m)emory | (s)ide] [index:byte] [value:byte]")
    of "q", "quit":
      break
    else:
      this.say("Expected form [(r | R)un | s(t)art | (d | D)isassemble | (g)et | (s)et | (q)uit]")

type OptionParameter = enum
  OP_INPUT_FILE
  OP_OUTPUT_FILE

  OP_RUN
  OP_COMPILE
  OP_EXECUTE
  OP_DEBUG

  OP_OUTPUT_TARGET

  OP_HELP

type Options = ref object
  values: TableRef[OptionParameter, string]

  input: string

proc newOptions(
  input: string
): Options = Options(values: newTable[OptionParameter, string](), input: input)

const OPTIONS_PARAMETERS = toOrderedTable({
  "in":         (OP_INPUT_FILE,       "Implicit, can be given without --in=[...]"),
  "out":        (OP_OUTPUT_FILE,      "Output file name"),

  "r":          (OP_RUN,              "Compile and execute a file"),
  "run":        (OP_RUN,              "Compile and execute a file"),
  "c":          (OP_COMPILE,          "Compile a file"),
  "compile":    (OP_COMPILE,          "Compile a file"),
  "e":          (OP_EXECUTE,          "Execute a file"),
  "execute":    (OP_EXECUTE,          "Execute a file"),
  "d":          (OP_DEBUG,            "Debug a file"),
  "debug":      (OP_DEBUG,            "Debug a file"),

  "target":     (OP_OUTPUT_TARGET,    "Compiler output target"),

  "?":          (OP_HELP,             "Display this help"),
  "h":          (OP_HELP,             "Display this help"),
  "help":       (OP_HELP,             "Display this help"),
})

proc add(this: Options, p: OptionParameter, n: string = $p, v: string): void =
  if p in this.values:
    echo "Duplicate parameter ", n
  else:
    this.values[p] = v

proc add(this: Options, n: string, v: string): void =
  if n in OPTIONS_PARAMETERS:
    this.add(OPTIONS_PARAMETERS[n][0], n, v)
  else:
    echo "Unexpected parameter ", n

proc update(this: Options, p: OptionParameter, n: string = $p, v: string): void =
  this.values[p] = v

proc update(this: Options, n: string, v: string): void =
  if n in OPTIONS_PARAMETERS:
    this.update(OPTIONS_PARAMETERS[n][0], n, v)
  else:
    echo "Unexpected parameter ", n

proc get(this: Options, p: OptionParameter, n: string = $p): string =
  if p in this.values:
    return this.values[p]
  else:
    return "false"

proc get(this: Options, n: string): string =
  if n in OPTIONS_PARAMETERS:
    return this.get(OPTIONS_PARAMETERS[n][0], n)
  else:
    return "false"

template has(this: Options, n: string): bool = (this.get(n) != "false")

proc parseLong(this: Options, item: string): void =
  if '=' in item:
    let pair = item.split("=")

    this.add(pair[0], pair[1])
  else:
    this.add(item, "true")

proc parseShort(this: Options, item: string): void =
  if '=' in item:
    let pair = item.split("=")

    for name in pair[0]:
      this.add($name, pair[1])
  elif item.len() == 1:
    this.add(item, "true")
  else:
    for i in 0..high item:
      this.add($item[i], "true")

proc help(this: Options): void =
  stdout.write("Usage:\n")
  stdout.write(("Interpreter2 [options...] <in>").indent(2), "\n")
  stdout.write(("Note: the parameter in can be given in form --in=[...] or [...]").indent(2), "\n")
  stdout.write("\n")
  stdout.write("Options:\n")

  for (k, v) in OPTIONS_PARAMETERS.pairs():
    stdout.write((k).indent(2).alignLeft(12), ":", (v[1]).indent(2), "\n")

proc parse(this: Options): void =
  if this.input.isEmptyOrWhitespace():
    this.add("?", "true")
  else:
    for item in this.input.split(" "):
      if item.len() > 2 and item[0..1] == "--":
        this.parseLong(item[2..high item])
      elif item.len() > 1 and item[0] == '-':
        this.parseShort(item[1..high item])
      else:
        this.add("in", item)

proc exists(input: string): (string, bool, string) =
  if input.fileExists():
    return (input, false, "")
  else:
    return ("", true, "File " & input & " does not exist")

proc lex(input: (string, bool, string)): (seq[Token], bool, string) =
  if input[1]: return (newSeq[Token](), input[1], input[2])
  
  let lexer = newLexer(input[0])
  
  lexer.tokenize()

  return (lexer.output, lexer.errors > 0, "Failed to tokenize")

proc parse(input: (seq[Token], bool, string)): (Node, State, bool, string) =
  if input[1]: return (newBlockNode(), newState(), input[1], input[2])
  
  let parser = newParser(input[0])

  parser.parse()

  return (parser.output, parser.state, parser.errors > 0, "Failed to parse")

proc compile(input: (Node, State, bool, string)): (Node, bool, string) =
  if input[2]: return (newBlockNode(), input[2], input[3])

  let compiler = newCompiler(input[1], input[0])

  compiler.compile()

  return (compiler.output, compiler.errors > 0, "Failed to compile")

proc assemble(input: (Node, bool, string)): (seq[byte], bool, string) =
  if input[1]: return (newSeq[byte](), input[1], input[2])

  let assembler = newAssembler(input[0])

  assembler.assemble()

  return (assembler.output, assembler.errors > 0, "Failed to assemble")

proc disassemble(input: (seq[byte], bool, string)): (string, bool, string) =
  if input[1]: return ("", input[1], input[2])
 
  let bytes = input[0]
  
  var output = ""
  
  var opname = ""
  var opcode = TK_I_NOOP
  var arity = 0
  
  for i in countup(0, high input[0], INSTRUCTION_WIDTH):
    opname = OPNAMES[bytes[i + 0]]
    opcode = RESERVED_LEXEMES[opname]    
    arity = INSTRUCTION_ARITIES[opcode]    
    
    output &= opname.alignLeft(12)

    for j in 1..arity:
      output &= " 0x" & $bytes[i + j].toHex()

      if j < arity:
        output &= ","

    output &= "\n"

  return (output, false, "")

proc readBytes(input: (string, bool, string)): (seq[byte], bool, string) =
  if input[1]: return (newSeq[byte](), input[1], input[2])
  
  let file = input[0].open()

  var bytes = newSeq[byte](file.getFileSize())

  if file.readBytes(bytes, 0, file.getFileSize()) < file.getFileSize():
    result = (bytes, true, "Failed to read from input file")
  else:
    result = (bytes, false, "")

  file.close()

proc readString(input: (string, bool, string)): (string, bool, string) =
  if input[1]: return ("", input[1], input[2])

  return (input[0].readFile(), false, "")

proc writeBytes(input: (seq[byte], bool, string), output: string): (bool, string) =
  if input[1]: return (input[1], input[2])
  
  let file = output.open(fmWrite)

  if file.writeBytes(input[0], 0, input[0].len()) < input[0].len():
    result = (true, "Failed to write to output file")
  else:
    result = (false, "")

  file.close()

proc writeString(input: (string, bool, string), output: string): (bool, string) =
  if input[1]: return (input[1], input[2])

  output.writeFile(input[0])

  return (false, "")

proc interpret(input: (seq[byte], bool, string)): (bool, string) =
  if input[1]: return (input[1], input[2])
  
  let vm = newVirtualMachine(input[0])

  vm.interpret()

proc debug(input: (seq[byte], bool, string)): (bool, string) =
  if input[1]: return (input[1], input[2])
  
  let vm = newVirtualMachine(input[0])

  vm.debug()

proc display(input: (bool, string)): void =
  if input[0]:
    stderr.styledWrite(fgRed, "[Failure]: ", input[1], "\n")
  else:
    stderr.styledWrite(fgGreen, "[Success]\n")

when isMainModule:
  let options = newOptions(commandLineParams().join(" "))

  options.parse()

  if not options.has("out"):
    options.update("out", options.get("in").changeFileExt("rsec"))
  
  if options.has("?") or not options.has("in"):
    options.help()
  elif options.has("r"):
    options.get("in")
           .exists()
           .readString()
           .lex()
           .parse()
           .compile()
           .assemble()
           .interpret()
           .display()
  elif options.has("c"):
    if not options.has("target") or options.get("target") == "rsec":
      options.get("in")
            .exists()
            .readString()
            .lex()
            .parse()
            .compile()
            .assemble()
            .writeBytes(options.get("out"))
            .display()
    elif options.has("target") and options.get("target") == "rasm":
      options.get("in")
            .exists()
            .readString()
            .lex()
            .parse()
            .compile()
            .assemble()
            .disassemble()
            .writeString(options.get("out"))
            .display()
  elif options.has("e"):
    options.get("in")
           .exists()
           .readBytes()
           .interpret()
           .display()
  elif options.has("d"):
    case options.get("in").splitFile().ext:
    of ".rasm":
      options.get("in")
             .exists()
             .readString()
             .lex()
             .parse()
             .compile()
             .assemble()
             .debug()
             .display()
    of ".rsec":
      options.get("in")
             .exists()
             .readBytes()
             .debug()
             .display()
    else:
      (true, "File must be of type [rasm | rsec]").display()
  else:
    options.help()
