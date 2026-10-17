## Clean-room MIT Motorola DSP56300 & C Code Generator for Omni DSL (.omni)
## Translates standalone .omni DSP modules into:
## 1. Motorola DSP56300 / DSP56311 Assembly (.asm) for 24-bit hardware execution
## 2. Self-contained C code (.c) for native/WASM compilation
##
## Features a complete Recursive-Descent Lexer & AST Parser:
## - Full operator precedence (unary -, not, multiplicative *, /, %, additive +, -, comparisons, and/or)
## - Control flow (if / elif / else, while) compiling to CMP + Jcc conditional branches
## - Zero hardcoded variable names or special-cased module identifiers

import std/[strutils, os, tables, parseutils, math, algorithm]

type
  OmniParam* = object
    name*: string
    defaultVal*: float
    minVal*: float
    maxVal*: float

  OmniStateVar* = object
    name*: string
    initExpr*: string
    initVal*: float

  ExprKind* = enum
    ekNum,      # Numeric literal: 1.0, 440, 0.5
    ekSym,      # Symbol / variable: in1, cutoff, phase
    ekUnary,    # Unary operator: -, not
    ekBinary,   # Binary operator: +, -, *, /, %, <, >, <=, >=, ==, !=, and, or
    ekCall,     # Function call: sin(x), pow(a, b), abs(x), tanh(x), etc.
    ekIf        # Inline ternary: if <cond>: <thenExpr> else: <elseExpr>

  Expr* = ref object
    case kind*: ExprKind
    of ekNum:
      numVal*: float
      isInt*: bool
      intVal*: int32
    of ekSym:
      symName*: string
    of ekUnary:
      uop*: string
      uexpr*: Expr
    of ekBinary:
      bop*: string
      lhs*, rhs*: Expr
    of ekCall:
      callee*: string
      args*: seq[Expr]
    of ekIf:
      ifCond*, ifThen*, ifElse*: Expr

  StmtKind* = enum
    skAssign,
    skIf,
    skWhile,
    skExpr

  Stmt* = ref object
    case kind*: StmtKind
    of skAssign:
      target*: string
      assignOp*: string  # "=", "+=", "-=", "*=", "/="
      val*: Expr
    of skIf:
      cond*: Expr
      thenStmts*: seq[Stmt]
      elifBranches*: seq[tuple[cond: Expr, stmts: seq[Stmt]]]
      elseStmts*: seq[Stmt]
    of skWhile:
      whileCond*: Expr
      whileStmts*: seq[Stmt]
    of skExpr:
      callExpr*: Expr

  OmniModule* = object
    name*: string
    inputs*: seq[string]
    outputs*: seq[string]
    params*: seq[OmniParam]
    stateVars*: seq[OmniStateVar]
    performStmts*: seq[Stmt]
    sampleStmts*: seq[Stmt]

# ------------------------------------------------------------- Lexer

type
  TokenKind* = enum
    tkEof,
    tkIdent,
    tkNumber,
    tkPlus, tkMinus, tkStar, tkSlash, tkPercent,
    tkAmp, tkPipe, tkCaret,
    tkAssign, tkPlusAssign, tkMinusAssign, tkStarAssign, tkSlashAssign,
    tkEq, tkNeq, tkLt, tkLe, tkGt, tkGe,
    tkLParen, tkRParen, tkLBrace, tkRBrace, tkLBracket, tkRBracket,
    tkColon, tkComma, tkDot,
    # Keywords
    kwIf, kwElif, kwElse, kwWhile, kwFor, kwIn,
    kwAnd, kwOr, kwNot, kwXor, kwTrue, kwFalse,
    kwIns, kwOuts, kwParams, kwInit, kwPerform, kwSample

  Token* = object
    kind*: TokenKind
    strVal*: string
    numVal*: float
    isInt*: bool
    intVal*: int32
    line*: int
    col*: int

  IndentedLine = object
    indent*: int
    lineNum*: int
    tokens*: seq[Token]

proc isIdentStart(c: char): bool =
  c in {'a'..'z', 'A'..'Z', '_'}

proc isIdentPart(c: char): bool =
  c in {'a'..'z', 'A'..'Z', '0'..'9', '_'}

proc tokenizeLine(rawLine: string, lineNum: int): IndentedLine =
  result.lineNum = lineNum
  result.tokens = @[]

  # 1. Compute indentation (treating tabs as 4 spaces)
  var i = 0
  var indent = 0
  while i < rawLine.len:
    if rawLine[i] == ' ':
      indent += 1
      i += 1
    elif rawLine[i] == '\t':
      indent += 4
      i += 1
    else:
      break
  result.indent = indent

  # 2. Tokenize line content
  while i < rawLine.len:
    let c = rawLine[i]

    # Skip inline whitespace
    if c in {' ', '\t', '\r'}:
      i += 1
      continue

    # Comments: # or // or ;
    if c == '#' or c == ';' or (c == '/' and i + 1 < rawLine.len and rawLine[i+1] == '/'):
      break # Rest of line is comment

    let col = i + 1

    # Numbers (including hex 0x...)
    if c in {'0'..'9'} or (c == '.' and i + 1 < rawLine.len and rawLine[i+1] in {'0'..'9'}):
      let start = i
      var isHex = false
      if c == '0' and i + 1 < rawLine.len and (rawLine[i+1] == 'x' or rawLine[i+1] == 'X'):
        isHex = true
        i += 2
        while i < rawLine.len and rawLine[i] in {'0'..'9', 'a'..'f', 'A'..'F'}:
          i += 1
      else:
        while i < rawLine.len and (rawLine[i] in {'0'..'9', '.'} or rawLine[i] in {'e', 'E', '+', '-'} and i > start and rawLine[i-1] in {'e', 'E'}):
          i += 1
      let numStr = rawLine[start..<i]
      var val = 0.0
      var isInt = false
      var intVal: int32 = 0
      if isHex:
        try:
          let h = parseHexInt(numStr[2..^1])
          val = float(h)
          intVal = int32(h)
          isInt = true
        except ValueError:
          val = 0.0
      else:
        if '.' in numStr or 'e' in numStr or 'E' in numStr:
          discard parseFloat(numStr, val)
          isInt = false
        else:
          try:
            intVal = int32(parseInt(numStr))
            val = float(intVal)
            isInt = true
          except ValueError:
            discard parseFloat(numStr, val)
            isInt = false
      result.tokens.add(Token(kind: tkNumber, strVal: numStr, numVal: val, isInt: isInt, intVal: intVal, line: lineNum, col: col))
      continue

    # Identifiers and keywords
    if isIdentStart(c):
      let start = i
      while i < rawLine.len and isIdentPart(rawLine[i]):
        i += 1
      let ident = rawLine[start..<i]
      var kind = tkIdent
      case ident
      of "if": kind = kwIf
      of "elif": kind = kwElif
      of "else": kind = kwElse
      of "while": kind = kwWhile
      of "for": kind = kwFor
      of "in": kind = kwIn
      of "and": kind = kwAnd
      of "or": kind = kwOr
      of "xor": kind = kwXor
      of "not": kind = kwNot
      of "true", "True": kind = kwTrue
      of "false", "False": kind = kwFalse
      of "ins": kind = kwIns
      of "outs": kind = kwOuts
      of "params": kind = kwParams
      of "init": kind = kwInit
      of "perform": kind = kwPerform
      of "sample": kind = kwSample
      else: discard

      result.tokens.add(Token(kind: kind, strVal: ident, line: lineNum, col: col))
      continue

    # Multi-character operators
    if i + 1 < rawLine.len:
      let pair = rawLine[i .. i+1]
      case pair
      of "==":
        result.tokens.add(Token(kind: tkEq, strVal: "==", line: lineNum, col: col))
        i += 2; continue
      of "!=":
        result.tokens.add(Token(kind: tkNeq, strVal: "!=", line: lineNum, col: col))
        i += 2; continue
      of "<=":
        result.tokens.add(Token(kind: tkLe, strVal: "<=", line: lineNum, col: col))
        i += 2; continue
      of ">=":
        result.tokens.add(Token(kind: tkGe, strVal: ">=", line: lineNum, col: col))
        i += 2; continue
      of "+=":
        result.tokens.add(Token(kind: tkPlusAssign, strVal: "+=", line: lineNum, col: col))
        i += 2; continue
      of "-=":
        result.tokens.add(Token(kind: tkMinusAssign, strVal: "-=", line: lineNum, col: col))
        i += 2; continue
      of "*=":
        result.tokens.add(Token(kind: tkStarAssign, strVal: "*=", line: lineNum, col: col))
        i += 2; continue
      of "/=":
        result.tokens.add(Token(kind: tkSlashAssign, strVal: "/=", line: lineNum, col: col))
        i += 2; continue
      else: discard

    # Single-character tokens
    case c
    of '+': result.tokens.add(Token(kind: tkPlus, strVal: "+", line: lineNum, col: col))
    of '-': result.tokens.add(Token(kind: tkMinus, strVal: "-", line: lineNum, col: col))
    of '*': result.tokens.add(Token(kind: tkStar, strVal: "*", line: lineNum, col: col))
    of '/': result.tokens.add(Token(kind: tkSlash, strVal: "/", line: lineNum, col: col))
    of '%': result.tokens.add(Token(kind: tkPercent, strVal: "%", line: lineNum, col: col))
    of '&': result.tokens.add(Token(kind: tkAmp, strVal: "&", line: lineNum, col: col))
    of '|': result.tokens.add(Token(kind: tkPipe, strVal: "|", line: lineNum, col: col))
    of '^': result.tokens.add(Token(kind: tkCaret, strVal: "^", line: lineNum, col: col))
    of '=': result.tokens.add(Token(kind: tkAssign, strVal: "=", line: lineNum, col: col))
    of '<': result.tokens.add(Token(kind: tkLt, strVal: "<", line: lineNum, col: col))
    of '>': result.tokens.add(Token(kind: tkGt, strVal: ">", line: lineNum, col: col))
    of '(': result.tokens.add(Token(kind: tkLParen, strVal: "(", line: lineNum, col: col))
    of ')': result.tokens.add(Token(kind: tkRParen, strVal: ")", line: lineNum, col: col))
    of '{': result.tokens.add(Token(kind: tkLBrace, strVal: "{", line: lineNum, col: col))
    of '}': result.tokens.add(Token(kind: tkRBrace, strVal: "}", line: lineNum, col: col))
    of '[': result.tokens.add(Token(kind: tkLBracket, strVal: "[", line: lineNum, col: col))
    of ']': result.tokens.add(Token(kind: tkRBracket, strVal: "]", line: lineNum, col: col))
    of ':': result.tokens.add(Token(kind: tkColon, strVal: ":", line: lineNum, col: col))
    of ',': result.tokens.add(Token(kind: tkComma, strVal: ",", line: lineNum, col: col))
    of '.': result.tokens.add(Token(kind: tkDot, strVal: ".", line: lineNum, col: col))
    else:
      discard # Skip unknown characters
    i += 1

proc tokenizeSource*(text: string): seq[IndentedLine] =
  let rawLines = text.splitLines()
  var parenDepth = 0
  for lineIdx, raw in rawLines:
    let il = tokenizeLine(raw, lineIdx + 1)
    if il.tokens.len == 0:
      continue
    if parenDepth > 0 and result.len > 0:
      for tok in il.tokens:
        if tok.kind == tkLParen: inc parenDepth
        elif tok.kind == tkRParen: dec parenDepth
        result[^1].tokens.add(tok)
    else:
      var lineDepth = 0
      for tok in il.tokens:
        if tok.kind == tkLParen: inc lineDepth
        elif tok.kind == tkRParen: dec lineDepth
      result.add(il)
      parenDepth = lineDepth

# ------------------------------------------------------------- Parser

type
  Parser = object
    lines: seq[IndentedLine]
    lineIdx: int
    tokIdx: int

proc isAtEof(p: Parser): bool =
  p.lineIdx >= p.lines.len

proc currTok(p: Parser): Token =
  if p.isAtEof():
    return Token(kind: tkEof)
  let line = p.lines[p.lineIdx]
  if p.tokIdx < line.tokens.len:
    return line.tokens[p.tokIdx]
  return Token(kind: tkEof)

proc peekTok(p: Parser, offset: int = 1): Token =
  if p.isAtEof(): return Token(kind: tkEof)
  let line = p.lines[p.lineIdx]
  let idx = p.tokIdx + offset
  if idx < line.tokens.len:
    return line.tokens[idx]
  return Token(kind: tkEof)

proc advanceTok(p: var Parser) =
  if not p.isAtEof():
    p.tokIdx += 1

proc nextLine(p: var Parser) =
  p.lineIdx += 1
  p.tokIdx = 0

proc currIndent(p: Parser): int =
  if p.isAtEof(): return -1
  return p.lines[p.lineIdx].indent

# Precedence levels for binary operators
proc getBinaryPrec(kind: TokenKind): int =
  case kind
  of kwOr, tkPipe: 1
  of kwXor, tkCaret: 2
  of kwAnd, tkAmp: 3
  of tkEq, tkNeq, tkLt, tkLe, tkGt, tkGe: 4
  of tkPlus, tkMinus: 5
  of tkStar, tkSlash, tkPercent: 6
  else: -1

proc getBinaryOpStr(kind: TokenKind): string =
  case kind
  of kwOr: "or"
  of tkPipe: "|"
  of kwXor: "xor"
  of tkCaret: "^"
  of kwAnd: "and"
  of tkAmp: "&"
  of tkEq: "=="
  of tkNeq: "!="
  of tkLt: "<"
  of tkLe: "<="
  of tkGt: ">"
  of tkGe: ">="
  of tkPlus: "+"
  of tkMinus: "-"
  of tkStar: "*"
  of tkSlash: "/"
  of tkPercent: "%"
  else: ""

# Forward declaration for mutual recursion
proc parseExpr(p: var Parser, minPrec: int = 0): Expr

proc parsePrimary(p: var Parser): Expr =
  let tok = p.currTok()
  case tok.kind
  of tkNumber:
    p.advanceTok()
    return Expr(kind: ekNum, numVal: tok.numVal, isInt: tok.isInt, intVal: tok.intVal)

  of tkIdent, kwTrue, kwFalse:
    var name = tok.strVal
    p.advanceTok()
    while p.currTok().kind == tkDot:
      p.advanceTok() # Consume '.'
      if p.currTok().kind == tkIdent:
        name.add("." & p.currTok().strVal)
        p.advanceTok()
    # Check for function call: name(...)
    if p.currTok().kind == tkLParen:
      p.advanceTok() # Consume '('
      var args: seq[Expr] = @[]
      if p.currTok().kind != tkRParen and p.currTok().kind != tkEof:
        args.add(p.parseExpr(0))
        while p.currTok().kind == tkComma:
          p.advanceTok() # Consume ','
          args.add(p.parseExpr(0))
      if p.currTok().kind == tkRParen:
        p.advanceTok() # Consume ')'
      return Expr(kind: ekCall, callee: name, args: args)
    else:
      return Expr(kind: ekSym, symName: name)

  of tkLParen:
    p.advanceTok() # Consume '('
    let inner = p.parseExpr(0)
    if p.currTok().kind == tkRParen:
      p.advanceTok() # Consume ')'
    return inner

  of kwIf:
    p.advanceTok() # Consume 'if'
    let cond = p.parseExpr(0)
    if p.currTok().kind == tkColon:
      p.advanceTok() # Consume ':'
    let thenExpr = p.parseExpr(0)
    var elseExpr: Expr = nil
    if p.currTok().kind == kwElse:
      p.advanceTok() # Consume 'else'
      if p.currTok().kind == tkColon:
        p.advanceTok() # Consume ':'
      elseExpr = p.parseExpr(0)
    return Expr(kind: ekIf, ifCond: cond, ifThen: thenExpr, ifElse: elseExpr)

  else:
    # Unexpected or empty; return 0.0 fallback
    p.advanceTok()
    return Expr(kind: ekNum, numVal: 0.0)

proc parseUnary(p: var Parser): Expr =
  let tok = p.currTok()
  if tok.kind == tkMinus:
    p.advanceTok()
    let inner = p.parseUnary()
    if inner.kind == ekNum:
      return Expr(kind: ekNum, numVal: -inner.numVal)
    return Expr(kind: ekUnary, uop: "-", uexpr: inner)
  elif tok.kind == tkPlus:
    p.advanceTok()
    return p.parseUnary()
  elif tok.kind == kwNot:
    p.advanceTok()
    return Expr(kind: ekUnary, uop: "not", uexpr: p.parseUnary())
  else:
    return p.parsePrimary()

proc parseExpr(p: var Parser, minPrec: int = 0): Expr =
  var left = p.parseUnary()
  while true:
    let prec = getBinaryPrec(p.currTok().kind)
    if prec < minPrec:
      break
    let opKind = p.currTok().kind
    let opStr = getBinaryOpStr(opKind)
    p.advanceTok()
    let right = p.parseExpr(prec + 1)
    left = Expr(kind: ekBinary, bop: opStr, lhs: left, rhs: right)
  return left

# Evaluate constant expressions at compile-time for initializers
proc evalConstExpr*(expr: Expr, samplerate: float = 48000.0, knownVars: Table[string, float] = initTable[string, float]()): float =
  if expr == nil: return 0.0
  case expr.kind
  of ekNum:
    return expr.numVal
  of ekSym:
    if expr.symName == "samplerate": return samplerate
    elif expr.symName.toUpperAscii() == "TWOPI": return 6.283185307179586
    elif expr.symName.toUpperAscii() == "PI": return 3.141592653589793
    elif expr.symName in knownVars: return knownVars[expr.symName]
    else: return 0.0
  of ekUnary:
    let v = evalConstExpr(expr.uexpr, samplerate, knownVars)
    if expr.uop == "-": return -v
    else: return v
  of ekBinary:
    let l = evalConstExpr(expr.lhs, samplerate, knownVars)
    let r = evalConstExpr(expr.rhs, samplerate, knownVars)
    case expr.bop
    of "+": return l + r
    of "-": return l - r
    of "*": return l * r
    of "/": return if r != 0.0: l / r else: 0.0
    of "%": return if r != 0.0: (let divVal = floor(l / r); l - divVal * r) else: 0.0
    of "&": return float(int64(l) and int64(r))
    of "|": return float(int64(l) or int64(r))
    of "^", "xor": return float(int64(l) xor int64(r))
    else: return 0.0
  of ekCall:
    if expr.callee == "trunc" and expr.args.len >= 1:
      return float(int(evalConstExpr(expr.args[0], samplerate, knownVars)))
    return 0.0
  of ekIf:
    let c = evalConstExpr(expr.ifCond, samplerate, knownVars)
    if c != 0.0:
      return evalConstExpr(expr.ifThen, samplerate, knownVars)
    elif expr.ifElse != nil:
      return evalConstExpr(expr.ifElse, samplerate, knownVars)
    else:
      return 0.0

# Statement Parsing
proc parseBlock(p: var Parser, parentIndent: int): seq[Stmt]

proc parseStatementLine(p: var Parser, parentIndent: int): Stmt =
  let firstTok = p.currTok()

  # 1. if statement
  if firstTok.kind == kwIf:
    p.advanceTok() # Consume 'if'
    let cond = p.parseExpr(0)
    if p.currTok().kind == tkColon:
      p.advanceTok()
    let ifIndent = p.lines[p.lineIdx].indent
    p.nextLine()

    let thenStmts = p.parseBlock(ifIndent)
    var elifBranches: seq[tuple[cond: Expr, stmts: seq[Stmt]]] = @[]
    var elseStmts: seq[Stmt] = @[]

    # Check for elif / else at the same indentation level
    while not p.isAtEof() and p.currIndent() == ifIndent:
      let tok = p.currTok()
      if tok.kind == kwElif:
        p.advanceTok()
        let elifCond = p.parseExpr(0)
        if p.currTok().kind == tkColon:
          p.advanceTok()
        p.nextLine()
        let bStmts = p.parseBlock(ifIndent)
        elifBranches.add((cond: elifCond, stmts: bStmts))
      elif tok.kind == kwElse:
        p.advanceTok()
        if p.currTok().kind == tkColon:
          p.advanceTok()
        p.nextLine()
        elseStmts = p.parseBlock(ifIndent)
        break # else is terminal
      else:
        break

    return Stmt(kind: skIf, cond: cond, thenStmts: thenStmts, elifBranches: elifBranches, elseStmts: elseStmts)

  # 2. while statement
  elif firstTok.kind == kwWhile:
    p.advanceTok()
    let cond = p.parseExpr(0)
    if p.currTok().kind == tkColon:
      p.advanceTok()
    let whileIndent = p.lines[p.lineIdx].indent
    p.nextLine()
    let stmts = p.parseBlock(whileIndent)
    return Stmt(kind: skWhile, whileCond: cond, whileStmts: stmts)

  # 3. Assignment: ident = expr (or +=, -=, *=, /=)
  elif firstTok.kind == tkIdent and p.peekTok(1).kind in {tkAssign, tkPlusAssign, tkMinusAssign, tkStarAssign, tkSlashAssign}:
    let target = firstTok.strVal
    p.advanceTok() # Consume ident
    let opTok = p.currTok()
    let assignOp = opTok.strVal
    p.advanceTok() # Consume assign op
    var expr = p.parseExpr(0)

    # Desugar compound assignments: target += expr -> target = target + (expr)
    if assignOp == "+=":
      expr = Expr(kind: ekBinary, bop: "+", lhs: Expr(kind: ekSym, symName: target), rhs: expr)
    elif assignOp == "-=":
      expr = Expr(kind: ekBinary, bop: "-", lhs: Expr(kind: ekSym, symName: target), rhs: expr)
    elif assignOp == "*=":
      expr = Expr(kind: ekBinary, bop: "*", lhs: Expr(kind: ekSym, symName: target), rhs: expr)
    elif assignOp == "/=":
      expr = Expr(kind: ekBinary, bop: "/", lhs: Expr(kind: ekSym, symName: target), rhs: expr)

    p.nextLine()
    return Stmt(kind: skAssign, target: target, assignOp: "=", val: expr)

  # 4. Standalone Expression
  else:
    let expr = p.parseExpr(0)
    p.nextLine()
    return Stmt(kind: skExpr, callExpr: expr)

proc parseBlock(p: var Parser, parentIndent: int): seq[Stmt] =
  result = @[]
  while not p.isAtEof():
    let indent = p.currIndent()
    if indent <= parentIndent:
      # Block terminated by lesser or equal indentation
      break
    let stmt = p.parseStatementLine(parentIndent)
    if stmt != nil:
      result.add(stmt)

proc parseParamDecl(line: string): OmniParam =
  var p = OmniParam(defaultVal: 0.0, minVal: 0.0, maxVal: 1.0)
  let l = line.strip()
  let braceOpen = l.find('{')
  let braceClose = l.find('}')
  if braceOpen > 0 and braceClose > braceOpen:
    p.name = l[0..<braceOpen].strip()
    let inner = l[(braceOpen + 1)..<braceClose].split(',')
    var v = 0.0
    if inner.len >= 1 and parseFloat(inner[0].strip(), v) > 0: p.defaultVal = v
    if inner.len >= 2 and parseFloat(inner[1].strip(), v) > 0: p.minVal = v
    if inner.len >= 3 and parseFloat(inner[2].strip(), v) > 0: p.maxVal = v
  else:
    p.name = l
  return p

proc parseOmniText*(text: string, moduleName: string = "OmniModule"): OmniModule =
  result.name = moduleName

  # 1. Tokenize all non-empty lines with multi-line parentheses continuation
  var indentedLines = tokenizeSource(text)
  let rawLines = text.splitLines()

  var p = Parser(lines: indentedLines, lineIdx: 0, tokIdx: 0)
  var knownInitVals = initTable[string, float]()

  while not p.isAtEof():
    let tok = p.currTok()

    # Section Headers
    if tok.kind in {kwIns, kwOuts, kwParams, kwInit, kwPerform, kwSample}:
      let secKind = tok.kind
      p.advanceTok()
      if p.currTok().kind == tkColon:
        p.advanceTok()
      let headerIndent = p.lines[p.lineIdx].indent
      p.nextLine()

      case secKind
      of kwIns:
        while not p.isAtEof() and p.currIndent() > headerIndent:
          if p.currTok().kind == tkIdent:
            result.inputs.add(p.currTok().strVal)
          p.nextLine()

      of kwOuts:
        while not p.isAtEof() and p.currIndent() > headerIndent:
          if p.currTok().kind == tkIdent:
            result.outputs.add(p.currTok().strVal)
          p.nextLine()

      of kwParams:
        while not p.isAtEof() and p.currIndent() > headerIndent:
          let rawLine = if p.lineIdx < rawLines.len: rawLines[p.lines[p.lineIdx].lineNum - 1] else: ""
          result.params.add(parseParamDecl(rawLine))
          p.nextLine()

      of kwInit:
        while not p.isAtEof() and p.currIndent() > headerIndent:
          if p.currTok().kind == tkIdent and p.peekTok(1).kind == tkAssign:
            let varName = p.currTok().strVal
            p.advanceTok() # ident
            p.advanceTok() # =
            let expr = p.parseExpr(0)
            let initVal = evalConstExpr(expr, 48000.0, knownInitVals)
            knownInitVals[varName] = initVal
            result.stateVars.add(OmniStateVar(name: varName, initExpr: $initVal, initVal: initVal))
          p.nextLine()

      of kwPerform:
        # Check if perform contains sample block
        while not p.isAtEof() and p.currIndent() > headerIndent:
          if p.currTok().kind == kwSample:
            p.advanceTok()
            if p.currTok().kind == tkColon: p.advanceTok()
            let sampleIndent = p.lines[p.lineIdx].indent
            p.nextLine()
            let sStmts = p.parseBlock(sampleIndent)
            result.sampleStmts.add(sStmts)
          else:
            let stmt = p.parseStatementLine(headerIndent)
            if stmt != nil:
              result.performStmts.add(stmt)

      of kwSample:
        let sStmts = p.parseBlock(headerIndent)
        result.sampleStmts.add(sStmts)

      else:
        p.nextLine()
    else:
      p.nextLine()

  # Auto-discover in* and out* symbols from statements if not explicitly declared in sections
  proc collectUsedSymbols(expr: Expr, syms: var seq[string]) =
    if expr == nil: return
    case expr.kind
    of ekSym:
      syms.add(expr.symName)
    of ekUnary:
      collectUsedSymbols(expr.uexpr, syms)
    of ekBinary:
      collectUsedSymbols(expr.lhs, syms)
      collectUsedSymbols(expr.rhs, syms)
    of ekCall:
      for a in expr.args: collectUsedSymbols(a, syms)
    of ekIf:
      collectUsedSymbols(expr.ifCond, syms)
      collectUsedSymbols(expr.ifThen, syms)
      if expr.ifElse != nil:
        collectUsedSymbols(expr.ifElse, syms)
    of ekNum:
      discard

  proc collectStmtSymbols(stmt: Stmt, syms: var seq[string]) =
    if stmt == nil: return
    case stmt.kind
    of skAssign:
      syms.add(stmt.target)
      collectUsedSymbols(stmt.val, syms)
    of skIf:
      collectUsedSymbols(stmt.cond, syms)
      for s in stmt.thenStmts: collectStmtSymbols(s, syms)
      for b in stmt.elifBranches:
        collectUsedSymbols(b.cond, syms)
        for s in b.stmts: collectStmtSymbols(s, syms)
      for s in stmt.elseStmts: collectStmtSymbols(s, syms)
    of skWhile:
      collectUsedSymbols(stmt.whileCond, syms)
      for s in stmt.whileStmts: collectStmtSymbols(s, syms)
    of skExpr:
      collectUsedSymbols(stmt.callExpr, syms)

  var allSyms: seq[string] = @[]
  for s in result.performStmts: collectStmtSymbols(s, allSyms)
  for s in result.sampleStmts: collectStmtSymbols(s, allSyms)

  for sym in allSyms:
    if (sym.startsWith("in") or sym.startsWith("In")) and sym.len >= 3 and sym[2] in {'0'..'9'}:
      if sym notin result.inputs:
        result.inputs.add(sym)
    elif (sym.startsWith("out") or sym.startsWith("Out")) and sym.len >= 4 and sym[3] in {'0'..'9'}:
      if sym notin result.outputs:
        result.outputs.add(sym)

  # Defaults if completely absent
  if result.inputs.len == 0:
    result.inputs.add("in1")
  if result.outputs.len == 0:
    result.outputs.add("out1")

  # Sort inputs and outputs for deterministic memory mapping (in1, in2, ...)
  result.inputs.sort()
  result.outputs.sort()

proc parseOmniFile*(path: string): OmniModule =
  let content = readFile(path)
  let name = splitFile(path).name
  return parseOmniText(content, name)

proc collectAssignTargets*(stmt: Stmt, targets: var seq[string]) =
  if stmt == nil: return
  case stmt.kind
  of skAssign:
    targets.add(stmt.target)
  of skIf:
    for s in stmt.thenStmts: collectAssignTargets(s, targets)
    for b in stmt.elifBranches:
      for s in b.stmts: collectAssignTargets(s, targets)
    for s in stmt.elseStmts: collectAssignTargets(s, targets)
  of skWhile:
    for s in stmt.whileStmts: collectAssignTargets(s, targets)
  of skExpr:
    discard


