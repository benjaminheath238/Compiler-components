from std/tables import TableRef, toTable, newTable, toOrderedTable, contains, pairs, `[]`, `[]=`
from std/terminal import ForegroundColor, styledWrite
from std/strutils import toUpper, split, join, alignLeft, indent, isEmptyOrWhitespace, toHex
from std/sequtils import toSeq, filter, map, concat, deduplicate
from std/setutils import toSet
from std/os import commandLineParams, changeFileExt, splitFile, fileExists
from std/parseutils import parseInt, parseHex, parseOct, parseBin
from std/strformat import fmt
from std/sugar import `=>`

proc isInteger(input: string): bool =
  var value = 0

  if input.len() <= 2:
    return parseInt(input, value) == input.len()

  case input[0..1]:
  of "0B", "0b":  (if input.parseBin(value) == input.len(): return true)
  of "0X", "0x":  (if input.parseHex(value) == input.len(): return true)
  of "0O", "0o":  (if input.parseOct(value) == input.len(): return true)
  else:           (if input.parseInt(value) == input.len(): return true)

proc asInteger(input: string): int =
  if input.len() <= 2:
    return if parseInt(input, result) < input.len(): 0 else: result

  case input[0..1]:
  of "0B", "0b":  (if input.parseBin(result) < input.len(): return 0)
  of "0X", "0x":  (if input.parseHex(result) < input.len(): return 0)
  of "0O", "0o":  (if input.parseOct(result) < input.len(): return 0)
  else:           (if input.parseInt(result) < input.len(): return 0)

proc isCharacter(input: string): bool =
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

proc asCharacter(input: string): char =
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

type TokenKind = enum
  TK_IDENTIFIER
  
  TK_INTEGER
  TK_CHARACTER
  TK_BOOLEAN

  TK_MOVE
  TK_READ
  TK_WRITE
  TK_LOAD
  TK_STORE
  
  # Arithmetic
  TK_ADD
  TK_SUB
  TK_MUL
  TK_DIV
  TK_MOD
  
  # Shift
  TK_SHL
  TK_SHR
  
  # Bitwise
  TK_NOT
  TK_AND
  TK_XOR
  TK_OR

  # Control Flow
  TK_GOTO
  TK_JUMP

  TK_RBRACK
  TK_LBRACK

  TK_COMMA

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

const RESERVED_SYMBOLS = toTable({
  "[":            TK_LBRACK,
  "]":            TK_RBRACK,

  ",":            TK_COMMA,
})

const RESERVED_LEXEMES = toTable({
  "TRUE":         TK_BOOLEAN,
  "FALSE":        TK_BOOLEAN,
  
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

  "GOTO":         TK_GOTO,
  "JUMP":         TK_JUMP,
})

const SYMBOL_CHARACTERS = RESERVED_SYMBOLS.pairs()
                                            .toSeq()
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

      this.add(TK_CHARACTER)
    of SYMBOL_CHARACTERS:
      while this.get() in SYMBOL_CHARACTERS:
        this.nxc()

      while this.txt() notin RESERVED_SYMBOLS and this.start < this.index:
        this.bak()

      if this.txt() in RESERVED_SYMBOLS:
        this.add(RESERVED_SYMBOLS[this.txt()])
      else:
        this.err()
        this.nxc()
    of {'0'..'9'}:
      while this.get() in {'0'..'9', 'o', 'O', 'x', 'X', 'b', 'B', 'a'..'f', 'A'..'F'}:
        this.nxc()

      this.add(TK_INTEGER)
    of {'a'..'z', 'A'..'Z'}:
      while this.get() in {'a'..'z', 'A'..'Z', '0'..'9', '_'}:
        this.nxc()

      if this.txt().toUpper() in RESERVED_LEXEMES:
        this.add(RESERVED_LEXEMES[this.txt().toUpper()])
      else:
        this.add(TK_IDENTIFIER)
    else:
      this.err()
      this.nxc()

type NodeKind = enum
  NK_PROGRAM

  NK_INSTRUCTION

  NK_CONSTANT
  NK_REGISTER

type Node = ref object
  pos: tuple[line: int, column: int]

  case kind: NodeKind:
  of NK_PROGRAM:
    programBody: seq[Node]
  of NK_INSTRUCTION:
    instructionIdentifier: tuple[name: string, kind: TokenKind]
    instructionArguments: seq[Node]
  of NK_CONSTANT:
    case constantKind: TokenKind:
    of TK_INTEGER:
      integerValue: int
    of TK_CHARACTER:
      characterValue: char
    of TK_BOOLEAN:
      booleanValue: bool
    else: discard
  of NK_REGISTER:
    registerAddress: int

type Parser = ref object
  index: int

  last: Token

  input: seq[Token]
  output: Node
  errors: int

proc newProgramNode(
  pos: tuple[line: int, column: int] = (0, 0),
  
  body: seq[Node] = newSeq[Node]()
): Node = Node(pos: pos, kind: NK_PROGRAM, programBody: body)

proc newInstructionNode(
  pos: tuple[line: int, column: int],
  
  identifier: tuple[name: string, kind: TokenKind] = ("", TK_MOVE),
  arguments: seq[Node] = newSeq[Node]()
): Node = Node(pos: pos, kind: NK_INSTRUCTION, instructionIdentifier: identifier, instructionArguments: arguments)

proc newIntegerConstantNode(
  pos: tuple[line: int, column: int],
  
  value: int = 0
): Node = Node(pos: pos, kind: NK_CONSTANT, constantKind: TK_INTEGER, integerValue: value)

proc newCharacterConstantNode(
  pos: tuple[line: int, column: int],
  
  value: char = '\0'
): Node = Node(pos: pos, kind: NK_CONSTANT, constantKind: TK_CHARACTER, characterValue: value)

proc newBooleanConstantNode(
  pos: tuple[line: int, column: int],
  
  value: bool = false
): Node = Node(pos: pos, kind: NK_CONSTANT, constantKind: TK_BOOLEAN, booleanValue: value)

proc newRegisterNode(
  pos: tuple[line: int, column: int],
  
  address: int = 0
): Node = Node(pos: pos, kind: NK_REGISTER, registerAddress: address)

proc newParser(
  input: seq[Token]
): Parser = Parser(index: 0, last: input[0], input: input, output: newProgramNode(), errors: 0)

template eos(this: Parser, offset: int = 0): bool = (this.index + offset > high this.input)
template get(this: Parser, offset: int = 0): Token = (this.input[this.index + offset])
template nxt(this: Parser): void = (this.index.inc(); (if not this.eos(): this.last = this.get()))
template err(this: Parser, msg: string = "Unexpected token"): void = (stderr.styledWrite(fgRed, "[Parser]: ", $msg, ", received '", $this.last.lexeme, "' at ", $this.last.pos.line, ":", $this.last.pos.column, "\n"); this.errors.inc())
template mch(this: Parser, kinds: set[TokenKind]): bool = (this.get().kind in kinds)
template pnk(this: Parser, kinds: set[TokenKind]): void = (while not this.eos() and not this.mch(kinds): this.nxt())
template xpc(this: Parser, kinds: set[TokenKind], msg: string): void = (if this.mch(kinds): (this.nxt()) else: (this.err(msg); this.pnk(kinds)))

const INSTRUCTION_ARITIES = toTable({
  TK_MOVE:    2,
  TK_READ:    2,
  TK_WRITE:   2,
  TK_LOAD:    2,
  TK_STORE:   2,
  
  TK_ADD:     3,
  TK_SUB:     3,
  TK_MUL:     3,
  TK_DIV:     3,
  TK_MOD:     3,
  
  TK_SHL:     3,
  TK_SHR:     3,

  TK_NOT:     2,
  TK_AND:     3,
  TK_XOR:     3,
  TK_OR:      3,

  TK_GOTO:    1,
  TK_JUMP:    2,
})

const INSTRUCTIONS = {TK_MOVE..TK_JUMP}

proc parseExpr(this: Parser): Node =
  if this.eos():
    this.err("Could not parse expression, End of stream")
    return nil

  case this.get().kind:
  of TK_INTEGER:
    result = newIntegerConstantNode(this.last.pos)

    this.xpc({TK_INTEGER}, "Expected an integer literal")

    if this.get(-1).lexeme.isInteger():
      result.integerValue = this.get(-1).lexeme.asInteger()
    else:
      this.err("Failed to parse integer")
      result.integerValue = 0
  of TK_CHARACTER:
    result = newCharacterConstantNode(this.last.pos)

    this.xpc({TK_CHARACTER}, "Expected an character literal")
    
    if this.get(-1).lexeme.isCharacter():
      result.characterValue = this.get(-1).lexeme.asCharacter()
    else:
      this.err("Failed to parse character")
      result.characterValue = '\0'
  of TK_BOOLEAN:
    result = newBooleanConstantNode(this.last.pos)

    this.xpc({TK_BOOLEAN}, "Expected an boolean literal")
    
    result.booleanValue = this.get(-1).lexeme.toUpper() == "TRUE"
  of TK_LBRACK:
    result = newRegisterNode(this.last.pos)

    this.xpc({TK_LBRACK}, "Expected an opening bracket before register")
    this.xpc({TK_INTEGER}, "Expected a register address")

    if this.get(-1).lexeme.isInteger():
      result.registerAddress = this.get(-1).lexeme.asInteger()
    else:
      this.err("Failed to parse integer")
      result.registerAddress = 0

    this.xpc({TK_RBRACK}, "Expected a closing bracket after register")

  else:
    this.err()
    this.pnk({TK_INTEGER, TK_CHARACTER, TK_BOOLEAN, TK_LBRACK})
    return this.parseExpr()

proc parseStmt(this: Parser): Node =
  if this.eos():
    this.err("Could not parse statement, End of stream")
    return nil

  case this.get().kind:
  of INSTRUCTIONS:
    result = newInstructionNode(this.last.pos)

    this.xpc(INSTRUCTIONS, "Expected an instruction name")

    result.instructionIdentifier = (this.get(-1).lexeme, this.get(-1).kind)

    let arity = INSTRUCTION_ARITIES[result.instructionIdentifier.kind]

    for i in 0..<arity:
      result.instructionArguments.add(this.parseExpr())

      if i < arity - 1:
        this.xpc({TK_COMMA}, "Expected a comma between arguments")
  else:
    this.err()
    this.pnk(INSTRUCTIONS)
    return this.parseStmt()

proc parse(this: Parser): void =
  while not this.eos():
    this.output.programBody.add(this.parseStmt())

type Assembler = ref object
  input: Node
  output: seq[byte]
  errors: int

proc newAssembler(
  input: Node
): Assembler = Assembler(input: input, output: newSeq[byte](), errors: 0)

template err(this: Assembler, pos: tuple[line: int, column: int], msg: string = "Unexpected Node"): void = (stderr.styledWrite(fgRed, "[Assembler]: ", msg, " at ", $pos.line, ":", $pos.column, "\n"); this.errors.inc())

const OPCODES = toTable({
  TK_MOVE:        byte(0x00),
  TK_READ:        byte(0x01),
  TK_WRITE:       byte(0x02),
  TK_LOAD:        byte(0x03),
  TK_STORE:       byte(0x04),

  TK_ADD:         byte(0x10),
  TK_SUB:         byte(0x11),
  TK_MUL:         byte(0x12),
  TK_DIV:         byte(0x13),
  TK_MOD:         byte(0x14),
  
  TK_SHL:         byte(0x20),
  TK_SHR:         byte(0x21),
  TK_NOT:         byte(0x22),
  TK_AND:         byte(0x23),
  TK_XOR:         byte(0x24),
  TK_OR:          byte(0x25),

  TK_GOTO:        byte(0x30),
  TK_JUMP:        byte(0x31),
})

const INSTRUCTION_WIDTH = 4

proc assemble(this: Assembler, node: Node): seq[byte] =
  result = newSeq[byte]()

  case node.kind:
  of NK_INSTRUCTION:
    result.add(OPCODES[node.instructionIdentifier.kind])
    
    for arg in node.instructionArguments:
      result.add(this.assemble(arg))
    
    while result.len() < INSTRUCTION_WIDTH:
      result.add(0x00)
  of NK_CONSTANT:
    case node.constantKind:
    of TK_INTEGER:    result.add(byte(node.integerValue))
    of TK_CHARACTER:  result.add(byte(node.characterValue))
    of TK_BOOLEAN:    result.add(if node.booleanValue: byte(1) else: byte(0))
    else: discard
  of NK_REGISTER:
    result.add(byte(node.registerAddress))
  else: this.err(node.pos)

proc assemble(this: Assembler): void =
  for child in this.input.programBody:
    this.output.add(this.assemble(child))

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

template rget(this: VirtualMachine, reg: SomeInteger): byte = this.registers[reg]
template mget(this: VirtualMachine, mem: SomeInteger): byte = this.memory[mem]
template sget(this: VirtualMachine, sid: SomeInteger): byte = this.sides[sid]
template iget(this: VirtualMachine, ind: SomeInteger): byte = this.input[ind]

template eos(this: VirtualMachine): bool = int(this.rget(RIP)) > high this.input

template opcode(this: VirtualMachine): byte = this.iget(this.rget(RIP) - 4)
template opand1u(this: VirtualMachine): byte = this.iget(this.rget(RIP) - 3)
template opand2u(this: VirtualMachine): byte = this.iget(this.rget(RIP) - 2)
template opand3u(this: VirtualMachine): byte = this.iget(this.rget(RIP) - 1)
template opand1i(this: VirtualMachine): int8 = int8(this.iget(this.rget(RIP) - 3))
template opand2i(this: VirtualMachine): int8 = int8(this.iget(this.rget(RIP) - 2))
template opand3i(this: VirtualMachine): int8 = int8(this.iget(this.rget(RIP) - 1))

template err(this: VirtualMachine, msg: string): void = (stderr.styledWrite(fgRed, "[Interpreter]: ", msg, ", received", $this.opcode(), " at ", $this.rget(RIP), "\n"); this.errors.inc())
template say(this: VirtualMachine, msg: varargs[string]): void = (stdout.styledWrite(fgBlue, "[Debugger]: ", msg.join(), "\n"))

template execute(this: VirtualMachine): void =
  case this.opcode():
  of OPCODES[TK_MOVE]:  this.rset(this.opand1u(), this.opand2u())
  of OPCODES[TK_READ]:  this.rset(this.opand1u(), this.sget(this.opand2u()))
  of OPCODES[TK_WRITE]: this.sset(this.opand1u(), this.rget(this.opand2u()))
  of OPCODES[TK_LOAD]:  this.rset(this.opand1u(), this.mget(this.opand2u()))
  of OPCODES[TK_STORE]: this.mset(this.opand1u(), this.rget(this.opand2u()))
  of OPCODES[TK_ADD]:   this.rset(this.opand1u(), this.rget(this.opand2i())  +  this.rget(this.opand3i())) 
  of OPCODES[TK_SUB]:   this.rset(this.opand1u(), this.rget(this.opand2i())  -  this.rget(this.opand3i()))   
  of OPCODES[TK_MUL]:   this.rset(this.opand1u(), this.rget(this.opand2i())  *  this.rget(this.opand3i()))   
  of OPCODES[TK_DIV]:   this.rset(this.opand1u(), this.rget(this.opand2i()) div this.rget(this.opand3i()))   
  of OPCODES[TK_MOD]:   this.rset(this.opand1u(), this.rget(this.opand2i()) mod this.rget(this.opand3i()))   
  of OPCODES[TK_SHL]:   this.rset(this.opand1u(), this.rget(this.opand2i()) shl this.rget(this.opand3i()))
  of OPCODES[TK_SHR]:   this.rset(this.opand1u(), this.rget(this.opand2i()) shr this.rget(this.opand3i()))
  of OPCODES[TK_NOT]:   this.rset(this.opand1u(),                           not this.rget(this.opand2i()))
  of OPCODES[TK_AND]:   this.rset(this.opand1u(), this.rget(this.opand1i()) and this.rget(this.opand3i()))
  of OPCODES[TK_XOR]:   this.rset(this.opand1u(), this.rget(this.opand1i()) xor this.rget(this.opand3i()))
  of OPCODES[TK_OR]:    this.rset(this.opand1u(), this.rget(this.opand1i())  or this.rget(this.opand3i()))
  of OPCODES[TK_GOTO]:  this.rset(RIP, this.opand1u() * INSTRUCTION_WIDTH)
  of OPCODES[TK_JUMP]:  (if this.rget(this.opand1u()) == 1: this.rset(RIP, this.opand2u() * INSTRUCTION_WIDTH))
  else: this.err("No such opcode")

proc interpret(this: VirtualMachine): void =
  while not this.eos():
    this.rget(RIP).inc(INSTRUCTION_WIDTH)
    this.execute()

const OPNAMES = toTable({
  OPCODES[TK_MOVE]:     "MOVE",
  OPCODES[TK_READ]:     "READ",
  OPCODES[TK_WRITE]:    "WRITE",
  OPCODES[TK_LOAD]:     "LOAD",
  OPCODES[TK_STORE]:    "STORE",
  OPCODES[TK_ADD]:      "ADD",
  OPCODES[TK_SUB]:      "SUB",
  OPCODES[TK_MUL]:      "MUL",
  OPCODES[TK_DIV]:      "DIV",
  OPCODES[TK_MOD]:      "MOD",
  OPCODES[TK_SHL]:      "SHL",
  OPCODES[TK_SHR]:      "SHR",
  OPCODES[TK_NOT]:      "NOT",
  OPCODES[TK_AND]:      "AND",
  OPCODES[TK_XOR]:      "XOR",
  OPCODES[TK_OR]:       "OR",
  OPCODES[TK_GOTO]:     "GOTO",
  OPCODES[TK_JUMP]:     "JUMP",

  byte(0xFF):           "NOOP",
})

proc debug(this: VirtualMachine): void =
  while true:
    let input = stdin.readLine().split(" ")
  
    case input[0]:
    of "r", "run":
      if not this.eos():
        this.rget(RIP).inc(3)
        this.execute()
      else:
        this.say("End of stream, use (s)tart to return to start")
    of "R", "Run":
      this.interpret()
    of "s", "start":
      this.rset(RIP, 0)
    of "d", "disassemble":
      if not this.eos():
        this.rget(RIP).inc(INSTRUCTION_WIDTH)
        this.say("Instruction at ", $(this.rget(RIP) div INSTRUCTION_WIDTH).toHex(), ": ", OPNAMES[this.opcode()].alignLeft(8), " ", $this.opand1u().toHex(), " ", $this.opand2u().toHex(), " ", $this.opand3u().toHex())
      this.rset(RIP, 0)
    of "D", "Disassemble":
      for i in 0..(high this.input) div INSTRUCTION_WIDTH:
        this.rget(RIP).inc(INSTRUCTION_WIDTH)
        this.say($(this.rget(RIP) div INSTRUCTION_WIDTH).toHex(), " = ", OPNAMES[this.opcode()].alignLeft(8), " ", $this.opand1u().toHex(), " ", $this.opand2u().toHex(), " ", $this.opand3u().toHex())
      this.rset(RIP, 0)
    of "g", "get":
      if input.len() < 2:
        this.say("Expected form (g)et [(r)egister | (m)emory | (s)ide]")
        continue

      case input[1]:
      of "r", "register":
        if input.len() == 3 and input[2].isInteger():
          let b = byte(input[2].asInteger())

          this.say("Value of register ", b.toHex(), " = ", $this.rget(b).toHex())
        else:
          this.say("Expected form (g)et (r)egister [register:byte]")
      of "m", "memory":
        if input.len() == 3 and input[2].isInteger():
          let b = byte(input[2].asInteger())
          
          this.say("Value of memory at ", b.toHex(), " = ", $this.mget(b).toHex())
        else:
          this.say("Expected form (g)et m(emory [address:byte]")
      of "s", "side":
        if input.len() == 3 and input[2].isInteger():
          let b = byte(input[2].asInteger())

          this.say("Value of side ", b.toHex(), " = ", $this.sget(b).toHex())
        else:
          this.say("Expected form (g)et (s)ide [side:byte]")
      else:
        this.say("Expected form (g)et [(r)egister | (m)emory | (s)ide]")
    of "q", "quit":
      break
    else:
      this.say("Expected form [(r | R)un | (s)tart | (d | D)isassemble | (g)et | (q)uit]")

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
  if item.len() == 1:
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

proc parse(input: (seq[Token], bool, string)): (Node, bool, string) =
  if input[1]: return (newProgramNode(), input[1], input[2])
  
  let parser = newParser(input[0])

  parser.parse()

  return (parser.output, parser.errors > 0, "Failed to parse")

proc assemble(input: (Node, bool, string)): (seq[byte], bool, string) =
  if input[1]: return (newSeq[byte](), input[1], input[2])

  let assembler = newAssembler(input[0])

  assembler.assemble()

  return (assembler.output, assembler.errors > 0, "Failed to assemble")

proc read(input: (string, bool, string)): (seq[byte], bool, string) =
  if input[1]: return (newSeq[byte](), input[1], input[2])
  
  let file = input[0].open()

  var bytes = newSeq[byte](file.getFileSize())

  if file.readBytes(bytes, 0, file.getFileSize()) < file.getFileSize():
    result = (bytes, true, "Failed to read from input file")
  else:
    result = (bytes, false, "")

  file.close()

proc text(input: (seq[byte], bool, string)): (string, bool, string) =
  if input[1]: return ("", input[1], input[2])

  result = (input[0].map(x => char(x)).join(""), input[1], input[2])

proc write(input: (seq[byte], bool, string), output: string): (bool, string) =
  if input[1]: return (input[1], input[2])
  
  let file = output.open(fmWrite)

  if file.writeBytes(input[0], 0, input[0].len()) < input[0].len():
    result = (true, "Failed to write to output file")
  else:
    result = (false, "")

  file.close()

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
           .read()
           .text()
           .lex()
           .parse()
           .assemble()
           .interpret()
           .display()
  elif options.has("c"):
    options.get("in")
           .exists()
           .read()
           .text()
           .lex()
           .parse()
           .assemble()
           .write(options.get("out"))
           .display()
  elif options.has("e"):
    options.get("in")
           .exists()
           .read()
           .interpret()
           .display()
  elif options.has("d"):
    case options.get("in").splitFile().ext:
    of ".rasm":
      options.get("in")
             .exists()
             .read()
             .text()
             .lex()
             .parse()
             .assemble()
             .debug()
             .display()
    of ".rsec":
      options.get("in")
             .exists()
             .read()
             .debug()
             .display()
    else:
      (true, "File must be of type [rasm | rsec]").display()
  else:
    options.help()
