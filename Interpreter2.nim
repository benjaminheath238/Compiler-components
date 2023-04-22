from std/tables import toTable, getOrDefault, `[]`
from std/terminal import ForegroundColor, styledWrite
from std/strutils import toUpper
from std/parseutils import parseInt, parseHex, parseOct, parseBin
from std/os import commandLineParams
from std/strformat import fmt

proc parseInteger(input: string): int =
  if len(input) <= 2:
    if parseInt(input, result) < len(input):
      echo "Invalid base 10 literal"
    return

  case input[0..1]:
  of "0B":
    if parseBin(input, result) < len(input):
      echo "Invalid base 2 literal"
  of "0b":
    if parseBin(input, result) < len(input):
      echo "Invalid base 2 literal"
  of "0X":
    if parseHex(input, result) < len(input):
      echo "Invalid base 16 literal"
  of "0x":
    if parseHex(input, result) < len(input):
      echo "Invalid base 16 literal"
  of "0O":
    if parseOct(input, result) < len(input):
      echo "Invalid base 8 literal"
  of "0o":
    if parseOct(input, result) < len(input):
      echo "Invalid base 8 literal"
  else:
    if parseInt(input, result) < len(input):
      echo "Invalid base 10 literal"

type TokenKind = enum
  TK_INTEGER

  TK_MOVE
  TK_READ
  TK_WRITE
  TK_LOAD
  TK_STORE
  
  TK_ADD
  TK_SUB
  TK_MUL
  TK_DIV
  TK_MOD
  
  TK_SHL
  TK_SHR
  
  TK_NOT
  TK_AND
  TK_XOR
  TK_OR

type Token = ref object
  kind: TokenKind
  
  lexeme: string
  
  line: int
  column: int

type Lexer = ref object
  index: int
  start: int

  line: int
  column: int

  input: string
  output: seq[Token]

proc newToken(
  kind: TokenKind,
  
  lexeme: string,
  
  line: int,
  column: int,
): Token = Token(kind: kind, lexeme: lexeme, line: line, column: column)

proc `$`(this: Token): string = fmt("({this.kind}, {this.lexeme}, {this.line}:{this.column})")

proc newLexer(
  input: string
): Lexer = Lexer(index: 0, start: 0, line: 1, column: 1, input: input, output: newSeq[Token]())

template eos(this: Lexer, offset: int = 0): bool = (this.index + offset > high this.input)
template get(this: Lexer, offset: int = 0): char = (if this.eos(offset): '\0' else: this.input[this.index + offset])
template jyn(this: Lexer): void = (this.start = this.index)
template nxl(this: Lexer): void = (this.index.inc(); this.line.inc(); this.column = 1)
template nxc(this: Lexer): void = (this.index.inc();this.column.inc())
template txt(this: Lexer): string = (this.input[this.start..this.index - 1])
template add(this: Lexer, kind: TokenKind): void = (this.output.add(newToken(kind, this.txt(), this.line, this.column - this.txt().len())))
template err(this: Lexer, msg: string = "Unexpected character"): void = (stderr.styledWrite(fgRed, "[Lexer]: ", msg, " '", $this.get(), "' at ", $this.line, ":", $this.column, "\n"))

const RESERVED_LEXEMES = toTable({
  "MOVE":         TK_MOVE,
  "READ":         TK_READ,
  "WRITE":        TK_WRITE,
  "LOAD":         TK_LOAD,
  "STORE":        TK_STORE,
  "ADD":          TK_ADD,
  "SUB":          TK_SUB,
  "MUL":          TK_MUL,
  "DIV":          TK_DIV,
  "MOD":          TK_MOD,
  "SHL":          TK_SHL,
  "SHR":          TK_SHR,
  "NOT":          TK_NOT,
  "AND":          TK_AND,
  "XOR":          TK_XOR,
  "OR":           TK_OR,
})

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
    of {'0'..'9'}:
      while this.get() in {'0'..'9', 'o', 'O', 'x', 'X', 'b', 'B'}:
        this.nxc()

      this.add(TK_INTEGER)
    of {'a'..'z', 'A'..'Z'}:
      while this.get() in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
        this.nxc()

      this.add(RESERVED_LEXEMES.getOrDefault(this.txt().toUpper()))
    else:
      this.err()

type NodeKind = enum
  NK_PROGRAM

  NK_INSTRUCTION

  NK_BINARY_ARGS
  NK_UNARY_ARGS
  NK_NULLARY_ARGS

  NK_CONSTANT

type Node = ref object
  line: int
  column: int

  case kind: NodeKind:
  of NK_PROGRAM:
    programBody: seq[Node]
  of NK_INSTRUCTION:
    instructionIdentifier: tuple[name: string, kind: TokenKind]
    instructionArguments: Node
  of NK_BINARY_ARGS:
    binaryArgs1: Node
    binaryArgs2: Node
  of NK_UNARY_ARGS:
    unaryArgs1: Node
  of NK_NULLARY_ARGS:
    discard
  of NK_CONSTANT:
    case constantKind: TokenKind:
    of TK_INTEGER:
      intValue: int
    else: discard

type Parser = ref object
  index: int

  input: seq[Token]
  output: Node

proc newProgramNode(
  line: int,
  column: int,
  
  body: seq[Node] = newSeq[Node]()
): Node = Node(line: line, column: column, kind: NK_PROGRAM, programBody: body)

proc newInstructionNode(
  line: int,
  column: int,

  identifier: tuple[name: string, kind: TokenKind] = ("", TK_MOVE),
  arguments: Node = nil
): Node = Node(line: line, column: column, kind: NK_INSTRUCTION, instructionIdentifier: identifier, instructionArguments: arguments)

proc newBinaryArgsNode(
  line: int,
  column: int,
  
  args1: Node = nil,
  args2: Node = nil
): Node = Node(line: line, column: column, kind: NK_BINARY_ARGS, binaryArgs1: args1, binaryArgs2: args2)

proc newUnaryArgsNode(
  line: int,
  column: int,

  args1: Node = nil,
): Node = Node(line: line, column: column, kind: NK_UNARY_ARGS, unaryArgs1: args1)

proc newNullaryArgsNode(
  line: int,
  column: int
): Node = Node(line: line, column: column, kind: NK_NULLARY_ARGS)

proc newIntegerConstant(
  line: int,
  column: int,
  
  value: int = 0
): Node = Node(line: line, column: column, kind: NK_CONSTANT, constantKind: TK_INTEGER, intValue: value)

proc newParser(
  input: seq[Token]
): Parser = Parser(index: 0, input: input, output: newProgramNode(0, 0))

template eos(this: Parser, offset: int = 0): bool = (this.index + offset > high this.input)
template get(this: Parser, offset: int = 0): Token = (this.input[this.index + offset])
template nxt(this: Parser): void = (this.index.inc())
template err(this: Parser, msg: string = "Unexpected token"): void = (stderr.styledWrite(fgRed, "[Parser]: ", $msg, ", received '", $this.get().lexeme, "' at ", $this.get().line, ":", $this.get().column, "\n"))
template mch(this: Parser, kinds: set[TokenKind]): bool = (this.get().kind in kinds)
template xpc(this: Parser, kinds: set[TokenKind], msg: string): void = (if this.mch(kinds): this.nxt() else: this.err(msg))

const INSTRUCTION_ARITIES = toTable({
  TK_MOVE:    2,
  TK_READ:    2,
  TK_WRITE:   2,
  TK_LOAD:    2,
  TK_STORE:   2,
  
  TK_ADD:     2,
  TK_SUB:     2,
  TK_MUL:     2,
  TK_DIV:     2,
  TK_MOD:     2,
  
  TK_SHL:     2,
  TK_SHR:     2,

  TK_NOT:     2,
  TK_AND:     2,
  TK_XOR:     2,
  TK_OR:      2,
})

proc parseExpr(this: Parser): Node =
  case this.get().kind:
  of TK_INTEGER:
    result = newIntegerConstant(this.get().line, this.get().column)

    this.xpc({TK_INTEGER}, "Expected an integer literal")

    result.intValue = parseInteger(this.get(-1).lexeme)
  else: this.err()

proc parseStmt(this: Parser): Node =
  case this.get().kind:
  of {TK_MOVE..TK_OR}:
    result = newInstructionNode(this.get().line, this.get().column)

    this.xpc({TK_MOVE..TK_OR}, "Expected an opcode name")

    result.instructionIdentifier = (this.get(-1).lexeme, this.get(-1).kind)

    let arity = INSTRUCTION_ARITIES[result.instructionIdentifier.kind]

    case arity:
    of 0:
      result.instructionArguments = newNullaryArgsNode(this.get().line, this.get().column)
    of 1:
      result.instructionArguments = newUnaryArgsNode(this.get().line, this.get().column)

      result.instructionArguments.unaryArgs1 = this.parseExpr()
    of 2:
      result.instructionArguments = newBinaryArgsNode(this.get().line, this.get().column)

      result.instructionArguments.binaryArgs1 = this.parseExpr()
      result.instructionArguments.binaryArgs2 = this.parseExpr()
    else: this.err("Problematic Arity")
  else: this.err()

proc parse(this: Parser): void =
  while not this.eos():
    this.output.programBody.add(this.parseStmt())

type Assembler = ref object
  input: Node
  output: seq[byte]

proc newAssembler(
  input: Node
): Assembler = Assembler(input: input, output: newSeq[byte]())

template err(this: Assembler, line: int, column: int, msg: string = "Unexpected Node"): void = (stderr.styledWrite(fgRed, "[Assembler]: ", msg, " at ", $line, ":", $column, "\n"))

const OPCODES = toTable({
  TK_MOVE:    byte(0x00),
  TK_READ:    byte(0x01),
  TK_WRITE:   byte(0x02),
  TK_LOAD:    byte(0x03),
  TK_STORE:   byte(0x04),

  TK_ADD:     byte(0x10),
  TK_SUB:     byte(0x11),
  TK_MUL:     byte(0x12),
  TK_DIV:     byte(0x13),
  TK_MOD:     byte(0x14),
  
  TK_SHL:     byte(0x20),
  TK_SHR:     byte(0x21),
  TK_NOT:     byte(0x22),
  TK_AND:     byte(0x23),
  TK_XOR:     byte(0x24),
  TK_OR:      byte(0x25),
})

proc assemble(this: Assembler, node: Node): seq[byte] =
  result = newSeq[byte]()

  case node.kind:
  of NK_INSTRUCTION:
    result.add(OPCODES[node.instructionIdentifier.kind])
    result.add(this.assemble(node.instructionArguments))
  of NK_BINARY_ARGS:
    result.add(this.assemble(node.binaryArgs1))
    result.add(this.assemble(node.binaryArgs2))
  of NK_UNARY_ARGS:
    result.add(this.assemble(node.unaryArgs1))
    result.add(0xFF)
  of NK_NULLARY_ARGS:
    result.add(0xFF)
    result.add(0xFF)
  of NK_CONSTANT:
    result.add(byte(node.intValue))
  else: this.err(node.line, node.column)

proc assemble(this: Assembler): void =
  for child in this.input.programBody:
    this.output.add(this.assemble(child))

type Interpreter = ref object
  memory: seq[byte]
  registers: seq[byte]
  sides: seq[byte]

  input: seq[byte]

proc newInterpreter(
  input: seq[byte]
): Interpreter = Interpreter(memory: newSeq[byte](0xFF), registers: newSeq[byte](0x0F), sides: newSeq[byte](0x06), input: input)

template err(this: Interpreter, msg: string): void = (stderr.styledWrite(fgRed, "[Interpreter]: ", msg, ", received", $this.input[this.registers[0x0F]], " at ", $this.registers[0x0F], "\n"))

proc interpret(this: Interpreter): void =
  const IP = 0x0F

  while int(this.registers[IP]) < high this.input:
    case this.input[this.registers[IP]]
    of OPCODES[TK_MOVE]:  this.registers[this.input[this.registers[IP] + 1]] = this.input[this.registers[IP] + 2]
    of OPCODES[TK_READ]:  this.registers[this.input[this.registers[IP] + 1]] = this.sides[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_WRITE]: this.sides[this.input[this.registers[IP] + 1]] = this.registers[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_LOAD]:  this.registers[this.input[this.registers[IP] + 1]] = this.memory[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_STORE]: this.memory[this.input[this.registers[IP] + 1]] = this.registers[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_ADD]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]]  +  this.registers[this.input[this.registers[IP] + 2]]   
    of OPCODES[TK_SUB]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]]  -  this.registers[this.input[this.registers[IP] + 2]]   
    of OPCODES[TK_MUL]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]]  *  this.registers[this.input[this.registers[IP] + 2]]   
    of OPCODES[TK_DIV]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]] div this.registers[this.input[this.registers[IP] + 2]]   
    of OPCODES[TK_MOD]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]] mod this.registers[this.input[this.registers[IP] + 2]]   
    of OPCODES[TK_SHL]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]] shl this.registers[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_SHR]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]] shr this.registers[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_NOT]:   this.registers[0] =                                                      not this.registers[this.input[this.registers[IP] + 1]]
    of OPCODES[TK_AND]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]] and this.registers[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_XOR]:   this.registers[0] = this.registers[this.input[this.registers[IP] + 1]] xor this.registers[this.input[this.registers[IP] + 2]]
    of OPCODES[TK_OR]:    this.registers[0] = this.registers[this.input[this.registers[IP] + 1]]  or this.registers[this.input[this.registers[IP] + 2]]
    else: this.err("No such opcode")

    this.registers[0x0F].inc(3)

when isMainModule:
  let args = commandLineParams()

  let lexer = newLexer(readFile(args[0]))

  lexer.tokenize()

  let parser = newParser(lexer.output)

  parser.parse()

  let assembler = newAssembler(parser.output)

  assembler.assemble()

  let interpreter = newInterpreter(assembler.output)

  interpreter.interpret()

  let file = args[1].open(fmWrite)

  file.write("Registers:\n" & $interpreter.registers & "\n")
  file.write("Memory:\n" & $interpreter.memory & "\n")
  file.write("Sides:\n" & $interpreter.sides & "\n")

  file.close()
