#[
  import io

  proc name(Int i, Int j): Void
    while true do
      io::print("Text: \n")
    
      val input: String = io::read()

      case input
        of "next"
          io::print("Advancing\n")
        of "back"
          io::print("Retreeting\n")
        of "stop"
          io::print("Stopping\n")
          break
      end
    end

    var index: Int = 0
    do
      io::print("TEXT\n")
  
      index++
    until index >= 32
  end
]#

from tables import TableRef, newTable, toTable, contains, pairs, `[]`, `[]=`
from json import JsonNode, newJObject, pretty, `%`, `[]=`
from sugar import `=>`, `->`
from sequtils import map
from strutils import indent, replace, toUpper, contains

type
  TokenKind = enum
    TK_LPAREN
    TK_RPAREN
    TK_LBRACK
    TK_RBRACK

    TK_QUALIFIER

    TK_DOT
    TK_COLON
    TK_COMMA

    TK_ASSIGN

    TK_ADD
    TK_SUB
    TK_MUL
    TK_DIV
    TK_MOD

    TK_EQUAL
    TK_NOT_EQUAL

    TK_GREATER
    TK_GREATER_EQUAL
    TK_LESSER
    TK_LESSER_EQUAL

    TK_INTEGER
    TK_FLOAT
    TK_CHARACTER
    TK_STRING
    TK_BOOLEAN

    TK_IDENTIFIER

    TK_PACKAGE
    TK_IMPORT
    
    TK_VAR
    TK_VAL

    TK_PROC
    TK_STRUCT
    TK_UNION  
    
    TK_IF
    TK_THEN
    TK_ELIF
    TK_ELSE

    TK_CASE
    TK_OF

    TK_WHILE
    TK_DO
    TK_FOR
    TK_UNTIL

    TK_BREAK
    TK_CONTINUE
    TK_RETURN

    TK_END

  Token = tuple[kind: TokenKind, lexeme: string, line: int, column: int]

  Lexer = ref object
    index: int
    start: int
    
    line: int
    column: int

    input: string
    output: seq[Token]

const RESERVED_WORDS = toTable({
  "(":            TK_LPAREN,
  ")":            TK_RPAREN,
  "[":            TK_LBRACK,
  "]":            TK_RBRACK,

  "::":           TK_QUALIFIER,

  ".":            TK_DOT,
  ":":            TK_COLON,
  ",":            TK_COMMA,

  "=":            TK_ASSIGN,

  "+":            TK_ADD,
  "-":            TK_SUB,
  "*":            TK_MUL,
  "/":            TK_DIV,
  "%":            TK_MOD,

  "==":           TK_EQUAL,
  "!=":           TK_NOT_EQUAL,

  ">":            TK_GREATER,
  ">=":           TK_GREATER_EQUAL,
  "<":            TK_LESSER,
  "<=":           TK_LESSER_EQUAL,

  "false":        TK_BOOLEAN,
  "true":         TK_BOOLEAN,
  
  "package":      TK_PACKAGE,
  "import":       TK_IMPORT,
    
  "var":          TK_VAR,
  "val":          TK_VAL,

  "proc":         TK_PROC,
  "struct":       TK_STRUCT,
  "union":        TK_UNION,
  
  "if":           TK_IF,
  "then":         TK_THEN,
  "elif":         TK_ELIF,
  "else":         TK_ELSE,

  "case":         TK_CASE,
  "of":           TK_OF,

  "while":        TK_WHILE,
  "do":           TK_DO,
  "for":          TK_FOR,
  "until":        TK_UNTIL,

  "break":        TK_BREAK,
  "continue":     TK_CONTINUE,
  "return":       TK_RETURN,

  "end":          TK_END,
})

const ALPHA_CHARS =       {'a'..'z', 'A'..'Z'}
const DIGIT_CHARS =       {'0'..'9'}
const ALPHA_DIGIT_CHARS = ALPHA_CHARS + DIGIT_CHARS
const IDENT_CHARS =       ALPHA_DIGIT_CHARS + {'_'}

const SYMBLE_CHARS = {'(', ')', '.', ':', ',', '=', '+', '-', '*', '/', '%', '!', '>', '<'}

proc newLexer(input: string): Lexer =
  result = Lexer()

  result.index = 0
  result.start = 0

  result.line = 1
  result.column = 1
  
  result.input = input
  result.output = newSeq[Token]()

template eos(this: Lexer, l: int = 0): bool = (this.index + l > high this.input)
template rst(this: Lexer): void = (this.start = this.index)
template get(this: Lexer, l: int = 0): char = (if this.eos(l): '\x00' else: this.input[this.index + l])
template txt(this: Lexer, l: int = -1): string = (this.input[this.start..this.index + l])
template add(this: Lexer, kind: TokenKind): void = (this.output.add((kind, this.txt(), this.line, this.column - this.txt().len())))
template nxc(this: Lexer): void = (this.index.inc(); this.column.inc())
template nxl(this: Lexer): void = (this.index.inc(); this.line.inc(); this.column = 1)
template err(this: Lexer, msg: string = "Unexpected character"): void = (echo "[Lexer]: " & msg & ", received '" & this.get() & "'" & $this.line & ":" & $this.column)

proc tokenize(this: Lexer): void =
  while not this.eos():
    this.rst()
    
    case this.get():
    of '\n':
      this.nxl()
    of {'\t', '\r', ' '}:
      this.nxc()
    of '#':
      while not this.eos() and this.get() != '\n':
        this.nxc()
      this.nxl()
    of {'\'', '\"'}:
      let delim = this.get()
      
      this.nxc()

      while not this.eos() and this.get() notin {'\x00'..'\x1F', '\x7F', delim}:
        this.nxc()

      this.nxc()

      case delim:
      of '\'':
        this.add(TK_CHARACTER)
      of '\"':
        this.add(TK_STRING)
      else:
        this.err()
    of SYMBLE_CHARS:
      this.nxc()
      
      while not this.eos() and this.get() in SYMBLE_CHARS:
        if this.txt(0) in RESERVED_WORDS:
          this.nxc()
        else:
          break

      if this.txt() in RESERVED_WORDS:
        this.add(RESERVED_WORDS[this.txt()])
      else:
        this.err()      
    of DIGIT_CHARS:
      while not this.eos() and this.get() in DIGIT_CHARS:
        this.nxc()
        
      if this.get() == '.':
        while not this.eos() and this.get() in DIGIT_CHARS:
          this.nxc()
        
        this.add(TK_FLOAT)
      else:
        this.add(TK_INTEGER)
    of ALPHA_CHARS:
      while not this.eos() and this.get() in IDENT_CHARS:
        this.nxc()

      if this.txt() in RESERVED_WORDS:
        this.add(RESERVED_WORDS[this.txt()])
      else:
        this.add(TK_IDENTIFIER)
    else:
      this.err()
      this.nxc()

type
  GroupKind = enum
    GK_ERROR
    
    GK_VOID
    
    GK_INTEGER
    GK_FLOAT
    GK_CHARACTER
    GK_STRING
    GK_BOOLEAN
    
    GK_STRUCTURE
    GK_PROCEDURE

  Group = tuple[kind: GroupKind, identifier: string]

  Variable = tuple[identifier: string, group: Group]

  Structure = tuple[identifier: string, fields: seq[Variable]]

  Procedure = tuple[identifier: string, parameters: seq[Variable], group: Group]

  Environment = ref object
    super: Environment
    
    id: int

    groups: TableRef[string, Group]
    variables: TableRef[string, Variable]
    structures: TableRef[string, Structure]
    procedures: TableRef[string, Procedure]

  State = ref object
    ids: int

    package: string

    gEnv: Environment
    lEnv: Environment

  NodeKind = enum
    NK_BLOCK

    NK_PACKAGE_STMT
    NK_IMPORT_STMT
    NK_PROC_DEF_STMT
    NK_STRUCT_DEF_STMT
    NK_VAR_DEF_STMT
    NK_IF_STMT
    NK_MATCH_STMT
    NK_WHILE_STMT
    NK_FOR_STMT
    NK_UNTIL_STMT
    NK_BREAK_STMT
    NK_CONTINUE_STMT
    NK_RETURN_STMT
    NK_EXPR_STMT

    NK_GROUPING_EXPR
    NK_BINARY_EXPR
    NK_UNARY_EXPR
    NK_PROC_CALL_EXPR
    NK_REFERENCE_EXPR
    NK_LITERAL_EXPR

  Node = ref object
    group: Group

    case kind: NodeKind
    of NK_BLOCK:
      blockEnv: Environment
      blockBody: seq[Node]
    of NK_PACKAGE_STMT:
      packageIdentifier: string
    of NK_IMPORT_STMT:
      importIdentifier: string
    of NK_PROC_DEF_STMT:
      procIdentifier: string
      procParameters: seq[Variable]
      procReturnGroup: Group
      procBody: Node
    of NK_STRUCT_DEF_STMT:
      structIdentifier: string
      structFields: seq[Variable]
      structKind: TokenKind
    of NK_VAR_DEF_STMT:
      varIsGlobal: bool
      varIsConst: bool
      varIdentifier: string
      varGroup: Group
      varValue: Node
    of NK_IF_STMT:
      ifCondition: Node
      ifBody: Node
      elifs: seq[tuple[condition: Node, body: Node]]
      elseBody: Node
    of NK_MATCH_STMT:
      matchVariable: string
      matchCases: seq[tuple[matches: Node, body: Node]]
      matchDefault: Node
    of NK_WHILE_STMT:
      whileCondition: Node
      whileBody: Node
    of NK_FOR_STMT:
      discard
    of NK_UNTIL_STMT:
      untilBody: Node
      untilCondition: Node
    of NK_BREAK_STMT:
      breakIdentifier: string
    of NK_CONTINUE_STMT:
      continueIdentifier: string
    of NK_RETURN_STMT:
      returnValue: Node
    of NK_EXPR_STMT:
      stmtExpr: Node
    of NK_GROUPING_EXPR:
      groupingExpr: Node
    of NK_BINARY_EXPR:
      bopOperandL: Node
      bopOperator: TokenKind
      bopOperandR: Node
    of NK_UNARY_EXPR:
      uopOperator: TokenKind
      uopOperandR: Node
    of NK_PROC_CALL_EXPR:
      callIdentifier: string
      callArguments: seq[Node]
    of NK_REFERENCE_EXPR:
      referenceIdentifier: string
    of NK_LITERAL_EXPR:
      literalKind: TokenKind
      literalValue: string

  Parser = ref object
    state: State

    index: int

    input: seq[Token]
    output: Node

const GROUP_ERROR =     (GK_ERROR,          "Error")
const GROUP_VOID =      (GK_VOID,           "Void")
const GROUP_INTEGER =   (GK_INTEGER,        "I32")
const GROUP_FLOAT =     (GK_FLOAT,          "F32")
const GROUP_CHARACTER = (GK_CHARACTER,      "Char")
const GROUP_STRING =    (GK_STRING,         "String")
const GROUP_BOOLEAN =   (GK_BOOLEAN,        "Bool")

const OPERATORS = toTable({
    TK_MUL:             (bop: (prec: 0x04, rAssoc: false), uop: (prec: 0x00)),
    TK_DIV:             (bop: (prec: 0x04, rAssoc: false), uop: (prec: 0x00)),
    TK_MOD:             (bop: (prec: 0x04, rAssoc: false), uop: (prec: 0x00)),

    TK_ADD:             (bop: (prec: 0x03, rAssoc: false), uop: (prec: 0x05)),
    TK_SUB:             (bop: (prec: 0x03, rAssoc: false), uop: (prec: 0x05)),

    TK_GREATER:         (bop: (prec: 0x02, rAssoc: false), uop: (prec: 0x00)),
    TK_GREATER_EQUAL:   (bop: (prec: 0x02, rAssoc: false), uop: (prec: 0x00)),
    TK_LESSER:          (bop: (prec: 0x02, rAssoc: false), uop: (prec: 0x00)),
    TK_LESSER_EQUAL:    (bop: (prec: 0x02, rAssoc: false), uop: (prec: 0x00)),

    TK_EQUAL:           (bop: (prec: 0x01, rAssoc: false), uop: (prec: 0x00)),
    TK_NOT_EQUAL:       (bop: (prec: 0x01, rAssoc: false), uop: (prec: 0x00)),

    TK_ASSIGN:          (bop: (prec: 0x00, rAssoc: true),  uop: (prec: 0x00)),
})

const BINARY_OPERATORS =  {TK_ADD..TK_MOD, TK_EQUAL..TK_NOT_EQUAL, TK_GREATER..TK_LESSER_EQUAL, TK_ASSIGN}
const UNARY_OPERATORS =   {TK_ADD, TK_SUB}
const LITERAL_KINDS =     {TK_INTEGER..TK_BOOLEAN}

proc `$`(this: Group): string = this.identifier
proc `$`(this: Variable): string = this.identifier & ":" & $this.group
proc `$`(this: Structure): string = this.identifier
proc `$`(this: Procedure): string = this.identifier & ":" & $this.parameters & ":" & $this.group

proc newEnvironment(super: Environment, id: int): Environment =
  result = Environment()
  
  result.super = super

  result.id = id

  result.groups = newTable[string, Group]()
  result.variables = newTable[string, Variable]()
  result.structures = newTable[string, Structure]()
  result.procedures = newTable[string, Procedure]()

proc hasGroup(this: Environment, identifier: string): bool =     (identifier in this.groups) or (this.super != nil and this.super.hasGroup(identifier))
proc hasVariable(this: Environment, identifier: string): bool =  (identifier in this.variables) or (this.super != nil and this.super.hasVariable(identifier))
proc hasStructure(this: Environment, identifier: string): bool = (identifier in this.structures) or (this.super != nil and this.super.hasStructure(identifier))
proc hasProcedure(this: Environment, identifier: string): bool = (identifier in this.procedures) or (this.super != nil and this.super.hasProcedure(identifier))

proc addGroup(this: Environment, identifier: string, element: Group): void =
  if identifier in this.groups:
    echo "Redefinition of group for type '" & $identifier & "'"
  else:
    this.groups[identifier] = element

proc addVariable(this: Environment, identifier: string, element: Variable): void =
  if identifier in this.variables:
    echo "Redefinition of variable with name '" & $identifier & "'"
  else:
    this.variables[identifier] = element

proc addStructure(this: Environment, identifier: string, element: Structure): void =
  if identifier in this.structures:
    echo "Redefinition of structure with name '" & $identifier & "'"
  else:
    this.structures[identifier] = element

proc addProcedure(this: Environment, identifier: string, element: Procedure): void =
  if identifier in this.procedures:
    echo "Redefinition of procedure for type '" & $identifier & "'"
  else:
    this.procedures[identifier] = element

proc getGroup(this: Environment, identifier: string): Group =
  if identifier in this.groups:
    return this.groups[identifier]
  elif this.super != nil:
    return this.super.getGroup(identifier)
  else:
    echo "No such group for type '" & $identifier & "'"

proc getVariable(this: Environment, identifier: string): Variable =
  if identifier in this.variables:
    return this.variables[identifier]
  elif this.super != nil:
    return this.super.getVariable(identifier)
  else:
    echo "No such variable with name '" & $identifier & "'"

proc getStructure(this: Environment, identifier: string): Structure =
  if identifier in this.structures:
    return this.structures[identifier]
  elif this.super != nil:
    return this.super.getStructure(identifier)
  else:
    echo "No such structure with name '" & $identifier & "'"

proc getProcedure(this: Environment, identifier: string): Procedure =
  if identifier in this.procedures:
    return this.procedures[identifier]
  elif this.super != nil:
    return this.super.getProcedure(identifier)
  else:
    echo "No such procedure for type '" & $identifier & "'"

proc `%`(this: Environment): JsonNode =
  result = newJObject()

  result["id"] = %this.id

  result["groups"] = %this.groups
  result["variables"] = %this.variables
  result["structures"] = %this.structures
  result["procedures"] = %this.procedures
  
proc newState(): State =
  result = State()

  result.ids = 0

  result.gEnv = newEnvironment(nil, result.ids)
  result.lEnv = result.gEnv

proc newParser(input: seq[Token]): Parser =
  result = Parser()

  result.state = newState()

  result.index = 0

  result.input = input
  result.output = Node(kind: NK_BLOCK, blockBody: newSeq[Node]())

template eos(this: Parser, l: int = 0): bool = (this.index + l > high this.input)
template get(this: Parser, l: int = 0): Token = (this.input[this.index + l])
template nxt(this: Parser): void = this.index.inc()
template err(this: Parser, msg: string = "Unexpected token"): void = (echo "[Parser]: " & msg & ", received '" & this.get().lexeme & "' at " & $this.get().line & ":" & $this.get(-1).column)
template mtc(this: Parser, kinds: set[TokenKind], l: int = 0): bool = (this.get(l).kind in kinds)
template pnk(this: Parser, kinds: set[TokenKind], l: int = 0): void = (while not this.eos(l) and not this.mtc(kinds, l): this.nxt())
template xpc(this: Parser, kinds: set[TokenKind], msg: string): void = (if this.mtc(kinds): this.nxt() else: (this.err(msg); this.pnk(kinds)))
template scp(this: Parser, body: untyped): void =
  this.state.ids.inc()

  this.state.lEnv = newEnvironment(this.state.lEnv, this.state.ids)
  
  body

  this.state.lEnv = this.state.lEnv.super

proc parseQualifiedName(this: Parser): string =
  result = ""

  while not this.eos(1) and this.mtc({TK_QUALIFIER}, 1):
    this.xpc({TK_IDENTIFIER}, "Expected an identifier for qualified name")
    
    result &= this.get(-1).lexeme

    this.xpc({TK_QUALIFIER}, "Expected a qualifier between identifiers in qualified name")
    
    result &= this.get(-1).lexeme
  
  this.xpc({TK_IDENTIFIER}, "Expected an identifier for qualified name")

  result &= this.get(-1).lexeme

proc parseExpr(this: Parser, prec: int = 0, primary: bool = false): Node =
  if primary:
    case this.get().kind:
    of TK_LPAREN:
      result = Node(kind: NK_GROUPING_EXPR)

      this.xpc({TK_LPAREN}, "Expected an opening parenthesis before grouping expression")
      
      result.groupingExpr = this.parseExpr()

      this.xpc({TK_LPAREN}, "Expected a closing parenthesis after grouping expression")
    of UNARY_OPERATORS:
      result = Node(kind: NK_UNARY_EXPR)

      this.xpc(UNARY_OPERATORS, "Expected an unary operator for expression")
      
      result.uopOperator = this.get(-1).kind

      result.uopOperandR = this.parseExpr(prec=OPERATORS[result.uopOperator].uop.prec)
    of TK_IDENTIFIER:
      let name = this.parseQualifiedName()
      
      case this.get().kind:
      of TK_LPAREN:
        result = Node(kind: NK_PROC_CALL_EXPR, callArguments: newSeq[Node]())
       
        if "::" in name:
          result.callIdentifier = name
        else:
          result.callIdentifier = this.state.package & "::" & name
       
        this.xpc({TK_LPAREN}, "Expected an opening parenthesis before arguments")
          
        while not this.eos() and not this.mtc({TK_RPAREN}):
          result.callArguments.add(this.parseExpr())
          
          if not this.mtc({TK_RPAREN}):
            this.xpc({TK_COMMA}, "Expected a comma between arguments")

        this.xpc({TK_RPAREN}, "Expected a closing parenthesis after arguments")
      of TK_DOT:
        let reference = Node(kind: NK_REFERENCE_EXPR, referenceIdentifier: name)
        
        this.xpc({TK_DOT}, "Expected a dot between variable and field")
        
        result = this.parseExpr(primary=true)

        case result.kind:
        of NK_PROC_CALL_EXPR:
          result.callArguments.insert(reference, 0)
        else: this.err("Unexpected token for field access")
      else:
        result = Node(kind: NK_REFERENCE_EXPR, referenceIdentifier: name)
    of LITERAL_KINDS:
      result = Node(kind: NK_LITERAL_EXPR)

      this.xpc(LITERAL_KINDS, "Expected a literal type for expression")

      result.literalKind = this.get(-1).kind
      result.literalValue = this.get(-1).lexeme
    else: this.err("Unexpected token for expression")
  else:
    result = this.parseExpr(primary=true)

    while not this.eos() and this.get().kind in BINARY_OPERATORS and OPERATORS[this.get().kind].bop.prec >= prec:
      result = Node(kind: NK_BINARY_EXPR, bopOperandL: result)

      this.xpc(BINARY_OPERATORS, "Expected a binary operator for expression")

      result.bopOperator = this.get(-1).kind

      result.bopOperandR = this.parseExpr(prec=
        if OPERATORS[result.bopOperator].bop.rAssoc:
          OPERATORS[result.bopOperator].bop.prec + 0
        else:
          OPERATORS[result.bopOperator].bop.prec + 1
      )

proc parseStmt(this: Parser, isGlobal: bool = false, isBlock: bool = false, blockPredicate: () -> bool = () => false): Node =
  if isBlock:
    result = Node(kind: NK_BLOCK, blockBody: newSeq[Node]())
    
    this.scp():
      while not this.eos() and blockPredicate():
        result.blockBody.add(this.parseStmt(isGlobal))

      result.blockEnv = this.state.lEnv
  elif isGlobal:
    case this.get().kind:
    of TK_PACKAGE:
      result = Node(kind: NK_PACKAGE_STMT)

      this.xpc({TK_PACKAGE}, "Expected key word 'package'")
      
      result.packageIdentifier = this.parseQualifiedName()

      if this.state.package == "":
        this.state.package = result.packageIdentifier
      else:
        this.err("Redefinition of package")
    of TK_IMPORT:
      result = Node(kind: NK_IMPORT_STMT)
      
      this.xpc({TK_IMPORT}, "Expected key word 'import'")
      
      result.importIdentifier = this.parseQualifiedName()
    of TK_PROC:
      result = Node(kind: NK_PROC_DEF_STMT, procParameters: newSeq[Variable]())

      this.xpc({TK_PROC}, "Expected key word 'proc'")
      this.xpc({TK_IDENTIFIER}, "Expected an identifier")

      result.procIdentifier = this.state.package & "::" & this.get(-1).lexeme

      this.xpc({TK_LPAREN}, "Expected an opening parenthesis before procedure parameters")

      while not this.eos() and not this.mtc({TK_RPAREN}):
        this.xpc({TK_IDENTIFIER}, "Expected an identifier")

        let identifier = this.get(-1).lexeme

        this.xpc({TK_COLON}, "Expected a colon procedure parameter identifier and kind")
        
        let kind = this.state.lEnv.getGroup(this.parseQualifiedName())

        result.procParameters.add((identifier, kind))

        if not this.mtc({TK_RPAREN}):
          this.xpc({TK_COMMA}, "Expected a comma between procedure parameters")

      this.xpc({TK_RPAREN}, "Expected an closing parenthesis after procedure parameters")
      this.xpc({TK_COLON}, "Expected a colon")
      this.xpc({TK_IDENTIFIER}, "Expected an identifier")
        
      result.procReturnGroup = this.state.lEnv.getGroup(this.get(-1).lexeme)

      result.procBody = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_END}))
      
      this.xpc({TK_END}, "Expected key word 'end'")

      for param in result.procParameters:
        result.procBody.blockEnv.addVariable(param.identifier, (param.identifier, param.group))

      this.state.lEnv.addProcedure(
        (
          $result.procIdentifier &
          $result.procParameters.map(x => x.group)
        ),
        (
          result.procIdentifier,
          result.procParameters.map(x => (x.identifier, x.group)),
          result.procReturnGroup
        )
      )
    of {TK_STRUCT, TK_UNION}:
      result = Node(kind: NK_STRUCT_DEF_STMT, structFields: newSeq[Variable]())

      this.xpc({TK_STRUCT, TK_UNION}, "Expected key word 'struct' or 'union'")

      result.structKind = this.get(-1).kind

      this.xpc({TK_IDENTIFIER}, "Expected an identifier")

      result.structIdentifier = this.get(-1).lexeme

      while not this.eos() and not this.mtc({TK_END}):
        this.xpc({TK_IDENTIFIER}, "Expected an identifier")

        let identifier = this.get(-1).lexeme

        this.xpc({TK_COLON}, "Expected a colon")
        
        let kind = this.state.lEnv.getGroup(this.parseQualifiedName())

        result.structFields.add((identifier, kind))

      this.xpc({TK_END}, "Expected key word 'end'")

      this.state.lEnv.addStructure(
        result.structIdentifier,
        (
          result.structIdentifier,
          result.structFields
        )
      )
    of {TK_VAL, TK_VAR}:
      result = Node(kind: NK_VAR_DEF_STMT)

      result.varIsGlobal = isGlobal

      if this.mtc({TK_VAL}):
        result.varIsConst = true
      else:
        result.varIsConst = false
      
      this.xpc({TK_VAL, TK_VAR}, "Expected key word 'var' or 'val'")
      this.xpc({TK_IDENTIFIER}, "Expected an identifier")

      result.varIdentifier = this.get(-1).lexeme

      this.xpc({TK_COLON}, "Expected a colon")
        
      result.varGroup = this.state.lEnv.getGroup(this.parseQualifiedName())

      if result.varIsConst or this.mtc({TK_ASSIGN}):
        this.xpc({TK_ASSIGN}, "Expected an assignment operator")

        result.varValue = this.parseExpr()
      
      this.state.lEnv.addVariable(
        result.varIdentifier,
        (
          result.varIdentifier,
          result.varGroup
        )
      )
    else: this.err("Unexpected token for global statement")
  else:
    case this.get().kind:
    of {TK_VAL, TK_VAR}:
      result = Node(kind: NK_VAR_DEF_STMT)

      result.varIsGlobal = isGlobal

      if this.mtc({TK_VAL}):
        result.varIsConst = true
      else:
        result.varIsConst = false
      
      this.xpc({TK_VAL, TK_VAR}, "Expected key word 'var' or 'val'")
      this.xpc({TK_IDENTIFIER}, "Expected an identifier")

      result.varIdentifier = this.get(-1).lexeme

      this.xpc({TK_COLON}, "Expected a colon")
     
      result.varGroup = this.state.lEnv.getGroup(this.parseQualifiedName())

      if result.varIsConst or this.mtc({TK_ASSIGN}):
        this.xpc({TK_ASSIGN}, "Expected an assignment operator")

        result.varValue = this.parseExpr()
      
      this.state.lEnv.addVariable(
        result.varIdentifier,
        (
          result.varIdentifier,
          result.varGroup
        )
      )
    of TK_IF:
      result = Node(kind: NK_IF_STMT, elifs: newSeq[tuple[condition: Node, body: Node]]())

      this.xpc({TK_IF}, "Expected key word 'if'")
      
      result.ifCondition = this.parseExpr()

      this.xpc({TK_THEN}, "Expected key word 'then'")

      result.ifBody = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_ELIF, TK_ELSE, TK_END}))
      
      while not this.eos() and this.mtc({TK_ELIF}):
        this.xpc({TK_ELIF}, "Expected key word 'elif'")

        let condition = this.parseExpr()
        
        this.xpc({TK_THEN}, "Expected key word 'then'")

        let body = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_ELIF, TK_ELSE, TK_END}))

        result.elifs.add((condition, body))
      
      if not this.eos() and this.mtc({TK_ELSE}):
        this.xpc({TK_ELSE}, "Expected key word 'else'")

        result.elseBody = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_END}))

      this.xpc({TK_END}, "Expected key word 'end'")
    of TK_CASE:
      result = Node(kind: NK_MATCH_STMT, matchCases: newSeq[tuple[matches: Node, body: Node]]())
      
      this.xpc({TK_CASE}, "Expected key word 'case'")
      this.xpc({TK_IDENTIFIER}, "Expected an identifier")

      result.matchVariable = this.get(-1).lexeme

      while not this.eos() and this.mtc({TK_OF}):
        this.xpc({TK_OF}, "Expected key word 'of'")

        if this.eos() or not this.mtc(LITERAL_KINDS):
          this.err("Case of statement must be a literal type")

        let matches = this.parseExpr()

        this.xpc({TK_THEN}, "Expected key word 'then'")

        let body = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_OF, TK_ELSE, TK_END}))
        
        result.matchCases.add((matches, body))
        
      this.xpc({TK_ELSE}, "Expected key word 'else'")

      result.matchDefault = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_END}))

      this.xpc({TK_END}, "Expected key word 'end'")
    of TK_WHILE:
      result = Node(kind: NK_WHILE_STMT)

      this.xpc({TK_WHILE}, "Expected key word 'while'")

      result.whileCondition = this.parseExpr()
      
      this.xpc({TK_DO}, "Expected key word 'do'")
      
      result.whileBody = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_END}))
      
      this.xpc({TK_END}, "Expected key word 'end'")
    of TK_FOR: this.err("For statements not supported")
    of TK_DO:
      result = Node(kind: NK_UNTIL_STMT)

      this.xpc({TK_DO}, "Expected key word 'do'")

      result.untilBody = this.parseStmt(isBlock=true, blockPredicate=() => not this.mtc({TK_UNTIL}))
      
      this.xpc({TK_UNTIL}, "Expected key word 'until'")
      
      result.untilCondition = this.parseExpr()
    of TK_BREAK:
      result = Node(kind: NK_BREAK_STMT)
      
      this.xpc({TK_BREAK}, "Expected key word 'break'")

      if this.mtc({TK_IDENTIFIER}):
        this.xpc({TK_IDENTIFIER}, "Expected an identifier for break statement")

        result.breakIdentifier = this.get(-1).lexeme
    of TK_CONTINUE:
      result = Node(kind: NK_CONTINUE_STMT)

      this.xpc({TK_CONTINUE}, "Expected key word 'continue'")

      if this.mtc({TK_IDENTIFIER}):
        this.xpc({TK_IDENTIFIER}, "Expected an identifier for continue statement")

        result.continueIdentifier = this.get(-1).lexeme
    of TK_RETURN:
      result = Node(kind: NK_RETURN_STMT)

      this.xpc({TK_RETURN}, "Expected key word 'return'")

      result.returnValue = this.parseExpr()
    of TK_IDENTIFIER:
      result = Node(kind: NK_EXPR_STMT)

      result.stmtExpr = this.parseExpr()
    else: this.err("Unexpected token for local statement")

proc parse(this: Parser): void =
  while not this.eos():
    this.output.blockBody.add(this.parseStmt(isGlobal=true))

  this.output.blockEnv = this.state.gEnv

type
  GroupChecker = ref object
    state: State
    
    output: Node

proc newGroupChecker(state: State, input: Node): GroupChecker =
  result = GroupChecker()

  result.state = state

  result.output = input

template err(this: GroupChecker, msg: string = "Type mismatch"): void = (echo "[Group Checker]: " & msg)

proc check(this: GroupChecker, node: Node, env: Environment): Group =
  result = GROUP_ERROR
  
  case node.kind:
  of NK_BLOCK:
    for child in node.blockBody:
      result = this.check(child, node.blockEnv)
  of NK_PACKAGE_STMT:
    result = GROUP_VOID
  of NK_IMPORT_STMT:
    result = GROUP_VOID
  of NK_PROC_DEF_STMT:
    discard this.check(node.procBody, env)

    result = GROUP_VOID
  of NK_STRUCT_DEF_STMT:
    result = GROUP_VOID
  of NK_VAR_DEF_STMT:
    let group = this.check(node.varValue, env)
    
    if group != node.varGroup:
      this.err()

    result = GROUP_VOID
  of NK_IF_STMT:
    result = GROUP_VOID
  of NK_MATCH_STMT:
    for match in node.matchCases:
      discard this.check(match.matches, env)
      discard this.check(match.body, env)

    discard this.check(node.matchDefault, env)

    result = GROUP_VOID
  of NK_WHILE_STMT:
    discard this.check(node.whileCondition, env)
    discard this.check(node.whileBody, env)

    result = GROUP_VOID
  of NK_FOR_STMT:
    result = GROUP_VOID
  of NK_UNTIL_STMT:
    discard this.check(node.untilBody, env)
    discard this.check(node.untilCondition, env)

    result = GROUP_VOID
  of NK_BREAK_STMT:
    result = GROUP_VOID
  of NK_CONTINUE_STMT:
    result = GROUP_VOID
  of NK_RETURN_STMT:
    result = this.check(node.returnValue, env)
  of NK_EXPR_STMT:
    result = this.check(node.stmtExpr, env)
  of NK_GROUPING_EXPR:
    result = this.check(node.groupingExpr, env)
  of NK_BINARY_EXPR:
    let lhs = this.check(node.bopOperandL, env)
    let rhs = this.check(node.bopOperandr, env)
  
    if lhs == rhs:
      result = lhs
  of NK_UNARY_EXPR:
    let rhs = this.check(node.uopOperandR, env)

    result = rhs
  of NK_PROC_CALL_EXPR:
    let procedure = env.getProcedure(($node.callIdentifier & $node.callArguments.map(x => this.check(x, env))))
    
    for i, child in node.callArguments:
      if this.check(child, env) != procedure.parameters[i].group:
        this.err()

    result = procedure.group
  of NK_REFERENCE_EXPR:
    result = env.getVariable(node.referenceIdentifier).group
  of NK_LITERAL_EXPR:
    result = case node.literalKind:
    of TK_INTEGER:    GROUP_INTEGER
    of TK_FLOAT:      GROUP_FLOAT
    of TK_CHARACTER:  GROUP_CHARACTER
    of TK_STRING:     GROUP_STRING
    of TK_BOOLEAN:    GROUP_BOOLEAN
    else:             GROUP_ERROR
  
  node.group = result

proc check(this: GroupChecker): void =
  for child in this.output.blockBody:
    discard this.check(child, this.output.blockEnv)

  this.output.group = GROUP_VOID

type
  Interpreter = ref object
    input: seq[byte]

type
  Transpiler = ref object
    phase: int

    temps: int

    state: State

    input: Node
    output: array[2, string]

const OPERATOR_LEXEMES = toTable({
  TK_ASSIGN:              "=",

  TK_ADD:                 "+",
  TK_SUB:                 "-",
  TK_MUL:                 "*",
  TK_DIV:                 "/",
  TK_MOD:                 "%",

  TK_EQUAL:               "==",
  TK_NOT_EQUAL:           "!=",

  TK_GREATER:             ">",
  TK_GREATER_EQUAL:       ">=",
  TK_LESSER:              "<",
  TK_LESSER_EQUAL:        "<=",
})

const PHASE_HEADER = 1
const PHASE_SOURCE = 2
const PHASE_ANY = PHASE_HEADER or PHASE_SOURCE

proc newTranspiler(state: State, input: Node): Transpiler =
  result = Transpiler()
  
  result.phase = PHASE_HEADER

  result.temps = 0
  
  result.state = state

  result.input = input
  result.output = ["", ""]

template during(this: Transpiler, phases: int, body: untyped): void =
  if (this.phase and phases) > 0:
    body

template getTemp(this: Transpiler): string =
  result = "_temp_" & this.temps

  this.temps.inc()

proc transpile(this: Transpiler, node: Node, indentation: int = 0): string =
  result = ""
  
  case node.kind:
  of NK_BLOCK:
    result &= "{\n"
    
    for child in node.blockBody:
      result &= (this.transpile(child) & "\n").indent(indentation, "\t")
    
    result &= "}"
  of NK_PACKAGE_STMT: discard
  of NK_IMPORT_STMT: discard
  of NK_PROC_DEF_STMT:
    result &= node.procReturnGroup.identifier & " "

    result &= node.procIdentifier

    for param in node.procParameters:
      result &= "_" & param.group.identifier

    result = result.replace("::", "_")

    result &= "("

    for i, param in node.procParameters:
      result &= param.group.identifier.replace("::", "_") & " " & param.identifier
    
      if i < high node.procParameters:
        result &= ", "

    result &= ")"

    this.during(PHASE_HEADER):
      result &= ";\n"

    this.during(PHASE_SOURCE):
      result &= " " & this.transpile(node.procBody, indentation + 1) & "\n"
  of NK_STRUCT_DEF_STMT:
    this.during(PHASE_HEADER):
      result &= "typedef "
      
      case node.structKind:
      of TK_STRUCT: result &= "struct"
      of TK_UNION: result &= "union"
      else: result &= "ERROR"

      result &= " " & node.structIdentifier & " {\n"

      for field in node.structFields:
        result &= (field.group.identifier.replace("::", "_") & " " & field.identifier).indent(indentation + 1, "\t") & ";\n"

      result &= "} " & node.structIdentifier & ";\n"
  of NK_VAR_DEF_STMT:
    if node.varIsConst:
      result &= "const "

    result &= node.varGroup.identifier.replace("::", "_") & " " & node.varIdentifier

    if node.varValue != nil:
      result &= " = " & this.transpile(node.varValue)
    
    result &= ";"
  of NK_IF_STMT:
    result &= "if (" & this.transpile(node.ifCondition) & ") " & this.transpile(node.ifBody, indentation + 1)

    for elseif in node.elifs:
      result &= " else if (" & this.transpile(elseif.condition) & ") " & this.transpile(elseif.body, indentation + 1)
    
    if node.elseBody != nil:
      result &= " else " & this.transpile(node.elseBody, indentation + 1)
  of NK_MATCH_STMT:
    result &= "switch (" & node.matchVariable & ") {\n"

    for match in node.matchCases:
      result &= "case " & this.transpile(match.matches) & ":" & this.transpile(match.body, indentation + 1) & "break;\n"
    
    result &= "default:" & this.transpile(node.matchDefault, indentation + 1) & "break;\n"

    result &= "}"
  of NK_WHILE_STMT:
    result &= "while (" & this.transpile(node.whileCondition) & ") " & this.transpile(node.whileBody)
  of NK_FOR_STMT: discard
  of NK_UNTIL_STMT:
    result &= "do " & this.transpile(node.untilBody) & " while (" & this.transpile(node.untilCondition) & ");"
  of NK_BREAK_STMT:
    if node.breakIdentifier != "":
      result &= "break " & node.breakIdentifier & ";"
  of NK_CONTINUE_STMT:
    if node.continueIdentifier != "":
      result &= "continue " & node.continueIdentifier & ";"
  of NK_RETURN_STMT:
    result &= "return " & this.transpile(node.returnValue) & ";"
  of NK_EXPR_STMT:
    result &= this.transpile(node.stmtExpr) & ";"
  of NK_GROUPING_EXPR:
    result &= "(" & this.transpile(node.groupingExpr) & ")"
  of NK_BINARY_EXPR:
    result &= this.transpile(node.bopOperandL) & " " & OPERATOR_LEXEMES[node.bopOperator] & " " & this.transpile(node.bopOperandR)
  of NK_UNARY_EXPR:
    result &= OPERATOR_LEXEMES[node.uopOperator] & this.transpile(node.uopOperandR)
  of NK_PROC_CALL_EXPR:
    result &= node.callIdentifier

    for arg in node.callArguments:
      result &= "_" & arg.group.identifier
    
    result = result.replace("::", "_")

    result &= "("

    for i, arg in node.callArguments:
      result &= this.transpile(arg)

      if i < high node.callArguments:
        result &= ", "

    result &= ")"
  of NK_REFERENCE_EXPR:
    result &= node.referenceIdentifier
  of NK_LITERAL_EXPR:
    result &= node.literalValue

proc transpile(this: Transpiler): void =
  this.phase = PHASE_HEADER
  
  this.output[0] &= "#ifndef " & this.state.package.replace("::", "_").toUpper() & "_H\n"
  this.output[0] &= "#define " & this.state.package.replace("::", "_").toUpper() & "_H\n"

  for child in this.input.blockBody:
    this.output[0] &= this.transpile(child)
  
  this.output[0] &= "#endif\n"

  this.phase = PHASE_SOURCE
  
  this.output[1] &= "#include \"" & this.state.package.replace("::", "_") & ".h\"\n"

  for child in this.input.blockBody:
    this.output[1] &= this.transpile(child)

when isMainModule:
  let lexer = newLexer(readFile("test.i1"))

  lexer.tokenize()

  echo (%lexer.output).pretty()

  let parser = newParser(lexer.output)

  parser.state.lEnv.addGroup("Void",    GROUP_VOID)
  parser.state.lEnv.addGroup("I32",     GROUP_INTEGER)
  parser.state.lEnv.addGroup("F32",     GROUP_FLOAT)
  parser.state.lEnv.addGroup("Char",    GROUP_CHARACTER)
  parser.state.lEnv.addGroup("String",  GROUP_STRING)
  parser.state.lEnv.addGroup("Bool",    GROUP_BOOLEAN)

  parser.state.lEnv.addProcedure(("io::print@[String]"),  ("io::print", @[("string", GROUP_STRING)],  GROUP_VOID))
  parser.state.lEnv.addProcedure(("io::read@[]"),         ("io::read",  @[],                          GROUP_STRING))

  parser.parse()

  echo (%parser.output).pretty()
  
  let groupChecker = newGroupChecker(parser.state, parser.output)

  groupChecker.check()

  echo (%groupChecker.output).pretty()

  let transpiler = newTranspiler(groupChecker.state, groupChecker.output)

  transpiler.transpile()

  echo transpiler.output[0]
  echo transpiler.output[1]
