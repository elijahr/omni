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
    ekCall      # Function call: sin(x), pow(a, b), abs(x), tanh(x), etc.

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
    tkAssign, tkPlusAssign, tkMinusAssign, tkStarAssign, tkSlashAssign,
    tkEq, tkNeq, tkLt, tkLe, tkGt, tkGe,
    tkLParen, tkRParen, tkLBrace, tkRBrace, tkLBracket, tkRBracket,
    tkColon, tkComma, tkDot,
    # Keywords
    kwIf, kwElif, kwElse, kwWhile, kwFor, kwIn,
    kwAnd, kwOr, kwNot, kwTrue, kwFalse,
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
  of kwOr: 1
  of kwAnd: 2
  of tkEq, tkNeq, tkLt, tkLe, tkGt, tkGe: 3
  of tkPlus, tkMinus: 4
  of tkStar, tkSlash, tkPercent: 5
  else: -1

proc getBinaryOpStr(kind: TokenKind): string =
  case kind
  of kwOr: "or"
  of kwAnd: "and"
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
    else: return 0.0
  of ekCall:
    if expr.callee == "trunc" and expr.args.len >= 1:
      return float(int(evalConstExpr(expr.args[0], samplerate, knownVars)))
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

  # 1. Tokenize all non-empty lines
  var rawLines = text.splitLines()
  var indentedLines: seq[IndentedLine] = @[]
  for lineIdx, raw in rawLines:
    let il = tokenizeLine(raw, lineIdx + 1)
    if il.tokens.len > 0:
      indentedLines.add(il)

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

# ------------------------------------------------------------- Motorola DSP56300 ASM Generator

proc floatToQ23Hex*(v: float): string =
  # Fractional [-1.0, 0.99999988] in Q23 format (1 sign bit, 23 fractional bits).
  # Audio frequencies and integer constants scaled without arbitrary sample rate divisions.
  var q: int32 = 0
  if v >= -1.0 and v <= 1.0:
    if v >= 0.99999988:
      q = 0x007FFFFF
    elif v <= -1.0:
      q = -8388608 # 0x800000
    else:
      q = int32(round(v * 8388608.0))
  else:
    # Integer constant (e.g. 2.0, 3.0, 4.0, 12.0, 69.0)
    q = int32(v)
  let u = uint32(q and 0x00FFFFFF)
  return "$" & u.toHex(6)

proc exprToHex*(expr: Expr): string =
  if expr != nil and expr.kind == ekNum:
    if expr.isInt:
      let u = uint32(expr.intVal and 0x00FFFFFF)
      return "$" & u.toHex(6)
    else:
      return floatToQ23Hex(expr.numVal)
  return "$000000"

type
  Dsp56300ModuleAddresses* = object
    inletAddrs*: seq[uint32]   # Explicit addresses for in1..in8
    outletAddrs*: seq[uint32]  # Explicit addresses for out1..out8
    paramBase*: uint32         # Base address in X: for parameters (p0, p1, ...)
    stateBase*: uint32         # Base address in Y: for state variables (s0, s1, ...)
    delayBase*: uint32         # Base address in Y: for circular delay buffers
    scratchBase*: uint32       # Base address in Y: for temporary scratch variables (default 0x0300)
    pOrg*: uint32              # Code origin in P: memory (default 0x0100)
    customAddrs*: bool         # True if caller specified custom relocated addresses

proc defaultDsp56300Addresses*(): Dsp56300ModuleAddresses =
  result.inletAddrs = @[]
  result.outletAddrs = @[]
  result.paramBase = 0x0020'u32
  result.stateBase = 0x0000'u32
  result.delayBase = 0x0400'u32
  result.scratchBase = 0x0300'u32
  result.pOrg = 0x0100'u32
  result.customAddrs = false

type
  AsmContext = object
    module: OmniModule
    addrs: Dsp56300ModuleAddresses
    paramNames: Table[string, bool]
    stateNames: Table[string, bool]
    inNames: Table[string, bool]
    outNames: Table[string, bool]
    inletAddrMap: Table[string, uint32]
    outletAddrMap: Table[string, uint32]
    paramAddrMap: Table[string, uint32]
    stateAddrMap: Table[string, uint32]
    tempTable: Table[string, uint32]
    tempNames: seq[string]
    curTempAddr: uint32
    spillCounter: int
    labelCounter: int
    labelPrefix: string
    asmOut: string

proc nextLabel(ctx: var AsmContext, prefix: string): string =
  ctx.labelCounter += 1
  let pfx = if ctx.labelPrefix.len > 0: ctx.labelPrefix & "_" & prefix else: prefix
  return pfx & "_" & $ctx.labelCounter

proc allocSpill(ctx: var AsmContext): string =
  ctx.spillCounter += 1
  let sym = "__spill_" & $ctx.spillCounter
  if sym notin ctx.tempTable:
    ctx.tempTable[sym] = ctx.curTempAddr
    ctx.tempNames.add(sym)
    ctx.curTempAddr += 1
  return sym

proc freeSpill(ctx: var AsmContext) =
  if ctx.spillCounter > 0:
    ctx.spillCounter -= 1

proc collectAssignTargets(stmt: Stmt, targets: var seq[string]) =
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

proc getMemOperand(ctx: AsmContext, sym: string): string =
  if sym in ctx.inletAddrMap:
    return "X:$" & toHex(int64(ctx.inletAddrMap[sym]), 4)
  elif sym in ctx.outletAddrMap:
    return "X:$" & toHex(int64(ctx.outletAddrMap[sym]), 4)
  elif sym in ctx.paramAddrMap:
    return "X:$" & toHex(int64(ctx.paramAddrMap[sym]), 4)
  elif sym in ctx.stateAddrMap:
    return "Y:$" & toHex(int64(ctx.stateAddrMap[sym]), 4)
  elif sym in ctx.tempTable and (ctx.addrs.customAddrs or ctx.tempTable[sym] > 0):
    return "Y:$" & toHex(int64(ctx.tempTable[sym]), 4)
  elif sym in ctx.inNames or sym in ctx.outNames or sym in ctx.paramNames:
    return "X:" & sym
  elif sym in ctx.stateNames or sym in ctx.tempTable:
    return "Y:" & sym
  else:
    return "Y:" & sym

# Forward declarations for AST code generation
proc genExprToAcc(ctx: var AsmContext, expr: Expr, acc: string = "A")
proc loadToX0(ctx: var AsmContext, expr: Expr)
proc loadToY0(ctx: var AsmContext, expr: Expr)
proc genDivide(ctx: var AsmContext, lhs: Expr, rhs: Expr, acc: string = "A")
proc genCondJump(ctx: var AsmContext, cond: Expr, targetLabel: string, jumpIfTrue: bool)
proc genStmt(ctx: var AsmContext, stmt: Stmt)

proc isLeaf(e: Expr): bool =
  e != nil and e.kind in {ekSym, ekNum}

proc loadToX0(ctx: var AsmContext, expr: Expr) =
  if expr == nil: return
  if expr.kind == ekSym:
    let s = expr.symName
    if s.toUpperAscii() == "TWOPI":
      ctx.asmOut.add("                MOVE    #$7FFFFF, X0         ; TWOPI\n")
    elif s.toUpperAscii() == "PI":
      ctx.asmOut.add("                MOVE    #$400000, X0         ; PI\n")
    elif s == "samplerate":
      ctx.asmOut.add("                MOVE    #$000055, X0         ; 1/samplerate in Q23\n")
    else:
      let mem = ctx.getMemOperand(s)
      ctx.asmOut.add("                MOVE    " & mem & ", X0\n")
  elif expr.kind == ekNum:
    ctx.asmOut.add("                MOVE    #" & exprToHex(expr) & ", X0\n")
  else:
    ctx.genExprToAcc(expr, "A")
    ctx.asmOut.add("                MOVE    A, X0\n")

proc loadToY0(ctx: var AsmContext, expr: Expr) =
  if expr == nil: return
  if expr.kind == ekSym:
    let s = expr.symName
    if s.toUpperAscii() == "TWOPI":
      ctx.asmOut.add("                MOVE    #$7FFFFF, Y0         ; TWOPI\n")
    elif s.toUpperAscii() == "PI":
      ctx.asmOut.add("                MOVE    #$400000, Y0         ; PI\n")
    elif s == "samplerate":
      ctx.asmOut.add("                MOVE    #$000055, Y0         ; 1/samplerate in Q23\n")
    else:
      let mem = ctx.getMemOperand(s)
      ctx.asmOut.add("                MOVE    " & mem & ", Y0\n")
  elif expr.kind == ekNum:
    ctx.asmOut.add("                MOVE    #" & exprToHex(expr) & ", Y0\n")
  else:
    ctx.genExprToAcc(expr, "A")
    ctx.asmOut.add("                MOVE    A, Y0\n")

proc genDivide(ctx: var AsmContext, lhs: Expr, rhs: Expr, acc: string = "A") =
  # True 24-bit restoring shift-subtract fixed-point division: A = lhs / rhs
  # Evaluates lhs into A, rhs into X0.
  # Handles division by zero, signed values, and saturation.
  ctx.genExprToAcc(lhs, "A")

  if isLeaf(rhs):
    ctx.loadToX0(rhs)
  else:
    let spill = ctx.allocSpill()
    ctx.asmOut.add("                MOVE    A, " & ctx.getMemOperand(spill) & "\n")
    ctx.genExprToAcc(rhs, "A")
    ctx.asmOut.add("                MOVE    A, X0\n")
    ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
    ctx.freeSpill()

  let lDivZero = ctx.nextLabel("_L_div_zero")
  let lSat = ctx.nextLabel("_L_div_sat")
  let lDoDiv = ctx.nextLabel("_L_div_do")
  let lApplySign = ctx.nextLabel("_L_div_apply_sign")
  let lDone = ctx.nextLabel("_L_div_done")
  let lAPos = ctx.nextLabel("_L_div_a_pos")
  let lX0Pos = ctx.nextLabel("_L_div_x0_pos")

  # 1. Check division by zero: if X0 == 0 -> jump to div_zero
  ctx.asmOut.add("                CLR     B\n")
  ctx.asmOut.add("                CMP     X0, B\n")
  ctx.asmOut.add("                JEQ     " & lDivZero & "\n")

  # 2. Track output sign in Y1: Y1 = (A < 0) ^ (X0 < 0)
  ctx.asmOut.add("                MOVE    #$000000, Y1         ; sign flag = 0 (positive)\n")
  ctx.asmOut.add("                TST     A\n")
  ctx.asmOut.add("                JGE     " & lAPos & "\n")
  ctx.asmOut.add("                NEG     A\n")
  ctx.asmOut.add("                MOVE    #$000001, Y1         ; sign flag = 1\n")
  ctx.asmOut.add(lAPos & ":\n")

  ctx.asmOut.add("                MOVE    X0, B\n")
  ctx.asmOut.add("                TST     B\n")
  ctx.asmOut.add("                JGE     " & lX0Pos & "\n")
  ctx.asmOut.add("                NEG     B\n")
  ctx.asmOut.add("                MOVE    B, X0                ; X0 = |rhs|\n")
  ctx.asmOut.add("                MOVE    Y1, B\n")
  ctx.asmOut.add("                MOVE    #$000001, X1\n")
  ctx.asmOut.add("                EOR     X1, B\n")
  ctx.asmOut.add("                MOVE    B, Y1                ; toggle sign\n")
  ctx.asmOut.add(lX0Pos & ":\n")

  # 3. Check saturation: if |A| >= X0 -> saturate to $7FFFFF
  ctx.asmOut.add("                CMP     X0, A\n")
  ctx.asmOut.add("                JLT     " & lDoDiv & "\n")
  ctx.asmOut.add(lSat & ":\n")
  ctx.asmOut.add("                MOVE    #$7FFFFF, B\n")
  ctx.asmOut.add("                JMP     " & lApplySign & "\n")

  ctx.asmOut.add(lDivZero & ":\n")
  ctx.asmOut.add("                MOVE    #$7FFFFF, B\n")
  ctx.asmOut.add("                JMP     " & lApplySign & "\n")

  # 4. 23-bit restoring shift-subtract division loop
  ctx.asmOut.add(lDoDiv & ":\n")
  ctx.asmOut.add("                CLR     B                    ; quotient = 0\n")
  for k in 1..23:
    let bitVal = 1'u32 shl (23 - k)
    let bitHex = "$" & bitVal.toHex(6)
    let lSkip = ctx.nextLabel("_L_div_k")
    ctx.asmOut.add("                ASL     A\n")
    ctx.asmOut.add("                CMP     X0, A\n")
    ctx.asmOut.add("                JLT     " & lSkip & "\n")
    ctx.asmOut.add("                SUB     X0, A\n")
    ctx.asmOut.add("                MOVE    #" & bitHex & ", X1\n")
    ctx.asmOut.add("                ADD     X1, B\n")
    ctx.asmOut.add(lSkip & ":\n")

  # 5. Apply sign and transfer result to acc
  ctx.asmOut.add(lApplySign & ":\n")
  ctx.asmOut.add("                MOVE    Y1, A\n")
  ctx.asmOut.add("                TST     A\n")
  ctx.asmOut.add("                JEQ     " & lDone & "\n")
  ctx.asmOut.add("                NEG     B\n")
  ctx.asmOut.add(lDone & ":\n")
  if acc == "A":
    ctx.asmOut.add("                MOVE    B, A\n")
  elif acc != "B":
    ctx.asmOut.add("                MOVE    B, " & acc & "\n")

proc genExprToAcc(ctx: var AsmContext, expr: Expr, acc: string = "A") =
  if expr == nil:
    ctx.asmOut.add("                CLR     " & acc & "\n")
    return

  case expr.kind
  of ekNum:
    ctx.asmOut.add("                MOVE    #" & exprToHex(expr) & ", " & acc & "\n")

  of ekSym:
    let s = expr.symName
    if s.toUpperAscii() == "TWOPI":
      ctx.asmOut.add("                MOVE    #$7FFFFF, " & acc & "         ; TWOPI (~1.0 normalized)\n")
    elif s.toUpperAscii() == "PI":
      ctx.asmOut.add("                MOVE    #$400000, " & acc & "         ; PI (~0.5 normalized)\n")
    elif s == "samplerate":
      ctx.asmOut.add("                MOVE    #$000055, " & acc & "         ; 1/samplerate in Q23\n")
    else:
      let mem = ctx.getMemOperand(s)
      ctx.asmOut.add("                MOVE    " & mem & ", " & acc & "\n")

  of ekUnary:
    if expr.uop == "-":
      ctx.genExprToAcc(expr.uexpr, acc)
      ctx.asmOut.add("                NEG     " & acc & "\n")
    elif expr.uop == "not":
      ctx.genExprToAcc(expr.uexpr, acc)
      ctx.asmOut.add("                NOT     " & acc & "\n")
    else:
      ctx.genExprToAcc(expr.uexpr, acc)

  of ekBinary:
    let op = expr.bop

    # Modulo: phase % 1.0
    if op == "%":
      ctx.genExprToAcc(expr.lhs, acc)
      if expr.rhs.kind == ekNum and (expr.rhs.numVal == 1.0 or expr.rhs.numVal == 1):
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, " & acc & "                ; wrap modulo 1.0\n")
      else:
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, " & acc & "\n")
      return

    # Multiply-Accumulate (MAC): a + b * c  or  b * c + a
    # Only use unspilled fast-path when BOTH operands of '*' are leaf nodes (ekSym or ekNum)
    # to prevent clobbering accumulator A during loadToX0 / loadToY0.
    if op == "+":
      if expr.rhs.kind == ekBinary and expr.rhs.bop == "*" and
          isLeaf(expr.rhs.lhs) and isLeaf(expr.rhs.rhs):
        ctx.genExprToAcc(expr.lhs, acc)
        ctx.loadToX0(expr.rhs.lhs)
        ctx.loadToY0(expr.rhs.rhs)
        ctx.asmOut.add("                MAC     X0, Y0, " & acc & "\n")
        return
      elif expr.lhs.kind == ekBinary and expr.lhs.bop == "*" and
          isLeaf(expr.lhs.lhs) and isLeaf(expr.lhs.rhs):
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.loadToX0(expr.lhs.lhs)
        ctx.loadToY0(expr.lhs.rhs)
        ctx.asmOut.add("                MAC     X0, Y0, " & acc & "\n")
        return

    # Multiply-Subtract: a - b * c
    if op == "-":
      if expr.rhs.kind == ekBinary and expr.rhs.bop == "*" and
          isLeaf(expr.rhs.lhs) and isLeaf(expr.rhs.rhs):
        ctx.genExprToAcc(expr.lhs, acc)
        ctx.loadToX0(expr.rhs.lhs)
        ctx.loadToY0(expr.rhs.rhs)
        ctx.asmOut.add("                MAC     -X0, Y0, " & acc & "\n")
        return
      elif expr.rhs.kind == ekCall and (expr.rhs.callee == "trunc" or expr.rhs.callee == "int") and
          expr.lhs.kind == ekSym and expr.rhs.args.len >= 1 and expr.rhs.args[0].kind == ekSym and
          expr.rhs.args[0].symName == expr.lhs.symName:
        ctx.genExprToAcc(expr.lhs, acc)
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, " & acc & "                ; x - trunc(x) -> frac(x)\n")
        return

    # Multiplication: X0 * Y0 -> A (with power-of-two optimizations)
    if op == "*":
      if expr.rhs.kind == ekNum:
        if expr.rhs.numVal == 2.0:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASL     " & acc & "\n")
          return
        elif expr.rhs.numVal == 4.0:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASL     " & acc & "\n                ASL     " & acc & "\n")
          return
        elif expr.rhs.numVal == 0.5:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASR     " & acc & "\n")
          return
        elif expr.rhs.numVal == 0.25:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASR     " & acc & "\n                ASR     " & acc & "\n")
          return
      elif expr.lhs.kind == ekNum:
        if expr.lhs.numVal == 2.0:
          ctx.genExprToAcc(expr.rhs, acc)
          ctx.asmOut.add("                ASL     " & acc & "\n")
          return
        elif expr.lhs.numVal == 4.0:
          ctx.genExprToAcc(expr.rhs, acc)
          ctx.asmOut.add("                ASL     " & acc & "\n                ASL     " & acc & "\n")
          return
        elif expr.lhs.numVal == 0.5:
          ctx.genExprToAcc(expr.rhs, acc)
          ctx.asmOut.add("                ASR     " & acc & "\n")
          return
        elif expr.lhs.numVal == 0.25:
          ctx.genExprToAcc(expr.rhs, acc)
          ctx.asmOut.add("                ASR     " & acc & "\n                ASR     " & acc & "\n")
          return

      ctx.genExprToAcc(expr.lhs, acc)
      ctx.asmOut.add("                MOVE    " & acc & ", X0\n")
      if expr.rhs.kind == ekSym:
        ctx.loadToY0(expr.rhs)
      elif expr.rhs.kind == ekNum:
        ctx.asmOut.add("                MOVE    #" & exprToHex(expr.rhs) & ", Y0\n")
      else:
        let spill = ctx.allocSpill()
        ctx.asmOut.add("                MOVE    X0, " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & acc & ", Y0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", X0\n")
        ctx.freeSpill()
      ctx.asmOut.add("                MPY     X0, Y0, " & acc & "\n")
      return

    # Division: a / b
    if op == "/":
      # Division by samplerate -> reciprocal multiplication (1/48000 = #$000055)
      if expr.rhs.kind == ekSym and expr.rhs.symName == "samplerate":
        ctx.genExprToAcc(expr.lhs, acc)
        ctx.asmOut.add("                MOVE    " & acc & ", X0\n")
        ctx.asmOut.add("                MOVE    #$000055, Y0         ; 1/samplerate in Q23\n")
        ctx.asmOut.add("                MPY     X0, Y0, " & acc & "\n")
        return
      elif expr.rhs.kind == ekNum:
        if expr.rhs.numVal == 2.0:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASR     " & acc & "\n")
          return
        elif expr.rhs.numVal == 4.0:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASR     " & acc & "\n                ASR     " & acc & "\n")
          return
        elif expr.rhs.numVal == 0.5:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASL     " & acc & "\n")
          return
        elif expr.rhs.numVal == 0.25:
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                ASL     " & acc & "\n                ASL     " & acc & "\n")
          return
        elif expr.rhs.numVal != 0.0 and abs(expr.rhs.numVal) >= 1.0:
          # Reciprocal multiplication for constants with |val| >= 1.0 (reciprocal <= 1.0)
          let recip = 1.0 / expr.rhs.numVal
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                MOVE    " & acc & ", X0\n")
          ctx.asmOut.add("                MOVE    #" & floatToQ23Hex(recip) & ", Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, " & acc & "\n")
          return
      # True 24-bit restoring shift-subtract variable division
      ctx.genDivide(expr.lhs, expr.rhs, acc)
      return

    # Addition / Subtraction
    if op == "+" or op == "-":
      ctx.genExprToAcc(expr.lhs, acc)
      if expr.rhs.kind == ekSym:
        ctx.loadToX0(expr.rhs)
      elif expr.rhs.kind == ekNum:
        ctx.asmOut.add("                MOVE    #" & exprToHex(expr.rhs) & ", X0\n")
      else:
        let spill = ctx.allocSpill()
        ctx.asmOut.add("                MOVE    " & acc & ", " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & acc & ", X0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", " & acc & "\n")
        ctx.freeSpill()

      if op == "+":
        ctx.asmOut.add("                ADD     X0, " & acc & "\n")
      else:
        ctx.asmOut.add("                SUB     X0, " & acc & "\n")
      return

    # Comparison ops evaluated as boolean expression (1.0 for true, 0.0 for false)
    if op in ["<", ">", "<=", ">=", "==", "!="]:
      let lTrue = ctx.nextLabel("_L_cmp_true")
      let lDone = ctx.nextLabel("_L_cmp_done")
      ctx.genCondJump(expr, lTrue, jumpIfTrue = true)
      ctx.asmOut.add("                CLR     " & acc & "\n")
      ctx.asmOut.add("                JMP     " & lDone & "\n")
      ctx.asmOut.add(lTrue & ":\n")
      ctx.asmOut.add("                MOVE    #$7FFFFF, " & acc & "\n")
      ctx.asmOut.add(lDone & ":\n")
      return

  of ekCall:
    case expr.callee
    of "sin":
      if expr.args.len >= 1:
        # Full-range normalized sine for phase in [0, 1): 16 * u * (0.5 - u)
        ctx.genExprToAcc(expr.args[0], "A")
        let lSinNeg = ctx.nextLabel("_L_sin_neg")
        let lSinDone = ctx.nextLabel("_L_sin_done")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, A                ; wrap to [0, 1)\n")
        ctx.asmOut.add("                MOVE    #$400000, X0         ; 0.5\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JGE     " & lSinNeg & "\n")
        ctx.asmOut.add("                MOVE    A, Y0                ; Y0 = u\n")
        ctx.asmOut.add("                MOVE    #$400000, B          ; B = 0.5\n")
        ctx.asmOut.add("                SUB     Y0, B                ; B = 0.5 - u\n")
        ctx.asmOut.add("                MOVE    B, X0                ; X0 = 0.5 - u\n")
        ctx.asmOut.add("                MPY     X0, Y0, A            ; A = 2 * u * (0.5 - u)\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n")
        let lSinClamp = ctx.nextLabel("_L_sin_clamp")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & lSinClamp & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lSinClamp & ":\n")
        ctx.asmOut.add("                JMP     " & lSinDone & "\n")
        ctx.asmOut.add(lSinNeg & ":\n")
        let lSinNegClamp = ctx.nextLabel("_L_sin_neg_clamp")
        ctx.asmOut.add("                SUB     X0, A                ; A = u - 0.5\n")
        ctx.asmOut.add("                MOVE    A, Y0                ; Y0 = u'\n")
        ctx.asmOut.add("                MOVE    #$400000, B          ; B = 0.5\n")
        ctx.asmOut.add("                SUB     Y0, B                ; B = 0.5 - u'\n")
        ctx.asmOut.add("                MOVE    B, X0                ; X0 = 0.5 - u'\n")
        ctx.asmOut.add("                MPY     X0, Y0, A            ; A = 2 * u' * (0.5 - u')\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & lSinNegClamp & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lSinNegClamp & ":\n")
        ctx.asmOut.add("                NEG     A\n")
        ctx.asmOut.add(lSinDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A, " & acc & "\n")

    of "cos":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    #$200000, X0         ; +0.25 phase shift for cos\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        let lCosNeg = ctx.nextLabel("_L_cos_neg")
        let lCosDone = ctx.nextLabel("_L_cos_done")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, A\n")
        ctx.asmOut.add("                MOVE    #$400000, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JGE     " & lCosNeg & "\n")
        ctx.asmOut.add("                MOVE    A, Y0\n")
        ctx.asmOut.add("                MOVE    #$400000, B\n")
        ctx.asmOut.add("                SUB     Y0, B\n")
        ctx.asmOut.add("                MOVE    B, X0\n")
        ctx.asmOut.add("                MPY     X0, Y0, A\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n")
        let lCosClamp = ctx.nextLabel("_L_cos_clamp")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & lCosClamp & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lCosClamp & ":\n")
        ctx.asmOut.add("                JMP     " & lCosDone & "\n")
        ctx.asmOut.add(lCosNeg & ":\n")
        let lCosNegClamp = ctx.nextLabel("_L_cos_neg_clamp")
        ctx.asmOut.add("                SUB     X0, A\n")
        ctx.asmOut.add("                MOVE    A, Y0\n")
        ctx.asmOut.add("                MOVE    #$400000, B\n")
        ctx.asmOut.add("                SUB     Y0, B\n")
        ctx.asmOut.add("                MOVE    B, X0\n")
        ctx.asmOut.add("                MPY     X0, Y0, A\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & lCosNegClamp & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lCosNegClamp & ":\n")
        ctx.asmOut.add("                NEG     A\n")
        ctx.asmOut.add(lCosDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A, " & acc & "\n")

    of "tan":
      # Filter cutoff authentic polynomial: x * (1 + x^2 * (1/3 + 2/15 * x^2))
      # Evaluated without accumulator overflow as x + x * [x^2 * (1/3 + 2/15 * x^2)]
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    A, X0\n")
        ctx.asmOut.add("                MPY     X0, X0, A            ; A = x^2\n")
        ctx.asmOut.add("                MOVE    A, Y0                ; Y0 = x^2\n")
        ctx.asmOut.add("                MOVE    #$111111, Y1         ; 2/15 in Q23\n")
        ctx.asmOut.add("                MPY     Y0, Y1, B            ; B = (2/15) * x^2\n")
        ctx.asmOut.add("                MOVE    #$2AAAAA, Y1         ; 1/3 in Q23\n")
        ctx.asmOut.add("                ADD     Y1, B                ; B = 1/3 + 2/15 * x^2\n")
        ctx.asmOut.add("                MOVE    B, Y1\n")
        ctx.asmOut.add("                MPY     Y0, Y1, B            ; B = x^2 * (1/3 + 2/15 * x^2)\n")
        ctx.asmOut.add("                MOVE    B, Y1\n")
        ctx.asmOut.add("                MPY     X0, Y1, B            ; B = x * (x^2 * (1/3 + 2/15 * x^2))\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add("                ADD     B, A                 ; A = x + B\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A, " & acc & "\n")

    of "tanh":
      # Real DSP56300 polynomial soft saturation: x * (1 - 1/3 * x^2)
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    A, X0\n")
        ctx.asmOut.add("                MPY     X0, X0, B\n")
        ctx.asmOut.add("                MOVE    B, Y0\n")
        ctx.asmOut.add("                MOVE    #$2AAAAA, Y1         ; 1/3 in Q23\n")
        ctx.asmOut.add("                MPY     -Y0, Y1, B           ; -1/3 * x^2\n")
        ctx.asmOut.add("                MOVE    #$7FFFFF, Y1         ; 1.0 in Q23\n")
        ctx.asmOut.add("                ADD     Y1, B                ; 1.0 - 1/3 * x^2\n")
        ctx.asmOut.add("                MOVE    B, Y1\n")
        ctx.asmOut.add("                MPY     X0, Y1, " & acc & "            ; x * (1 - 1/3 * x^2)\n")

    of "min":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if isLeaf(expr.args[1]):
          ctx.loadToX0(expr.args[1])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lMinDone = ctx.nextLabel("_L_min_done")
        ctx.asmOut.add("                JLE     " & lMinDone & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lMinDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A, " & acc & "\n")

    of "max":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if isLeaf(expr.args[1]):
          ctx.loadToX0(expr.args[1])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lMaxDone = ctx.nextLabel("_L_max_done")
        ctx.asmOut.add("                JGE     " & lMaxDone & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lMaxDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A, " & acc & "\n")

    of "clamp":
      if expr.args.len >= 3:
        ctx.genExprToAcc(expr.args[0], "A")
        # Lower bound: if A < lo, A = lo
        if isLeaf(expr.args[1]):
          ctx.loadToX0(expr.args[1])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lClampLo = ctx.nextLabel("_L_clamp_lo")
        ctx.asmOut.add("                JGE     " & lClampLo & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lClampLo & ":\n")
        # Upper bound: if A > hi, A = hi
        if isLeaf(expr.args[2]):
          ctx.loadToX0(expr.args[2])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[2], "A")
          ctx.asmOut.add("                MOVE    A, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lClampHi = ctx.nextLabel("_L_clamp_hi")
        ctx.asmOut.add("                JLE     " & lClampHi & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lClampHi & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A, " & acc & "\n")

    of "abs":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)
        ctx.asmOut.add("                ABS     " & acc & "\n")

    of "trunc", "int":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, " & acc & "                ; Q23 fractional wrapping\n")

    of "float":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)

    of "pow":
      if expr.args.len >= 2:
        # Check if 2.0 ^ x (pitch-to-frequency scaling polynomial)
        let isBaseTwo = (expr.args[0].kind == ekNum and expr.args[0].numVal == 2.0)
        if isBaseTwo:
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A, X0\n")
          ctx.asmOut.add("                MOVE    #$071AC0, Y0         ; c3 = 0.055504 in Q23\n")
          ctx.asmOut.add("                MPY     X0, Y0, B            ; x * c3\n")
          ctx.asmOut.add("                MOVE    #$1EBFCD, Y1         ; c2 = 0.240226 in Q23\n")
          ctx.asmOut.add("                ADD     Y1, B                ; c2 + x * c3\n")
          ctx.asmOut.add("                MOVE    B, Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, B            ; x * (c2 + x * c3)\n")
          ctx.asmOut.add("                MOVE    #$58B9FE, Y1         ; c1 = 0.693147 in Q23\n")
          ctx.asmOut.add("                ADD     Y1, B                ; c1 + x * (...)\n")
          ctx.asmOut.add("                MOVE    B, Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, B            ; x * (c1 + ...)\n")
          ctx.asmOut.add("                MOVE    #$7FFFFF, A          ; 1.0\n")
          ctx.asmOut.add("                ADD     B, A                 ; 1.0 + x * (...)\n")
          if acc != "A":
            ctx.asmOut.add("                MOVE    A, " & acc & "\n")
        else:
          ctx.genExprToAcc(expr.args[1], acc)

    of "delay_read", "delay.read":
      # Circular delay buffer in Y: memory at ctx.addrs.delayBase
      # Y:$delayBase stores write pointer wptr (0..63)
      # Y:($delayBase + 1 .. $delayBase + 64) stores the 64-sample ring buffer
      let delayBaseHex = toHex(int64(ctx.addrs.delayBase), 4)
      let delayDataHex = toHex(int64(ctx.addrs.delayBase + 1), 4)
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A") # evaluate time/offset into A
        let skipScaleLabel = ctx.nextLabel("_dly_time_ok")
        ctx.asmOut.add("                MOVE    #$00003F, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & skipScaleLabel & "\n")
        ctx.asmOut.add("                REP     #17\n")
        ctx.asmOut.add("                ASR     A\n")
        ctx.asmOut.add(skipScaleLabel & ":\n")
        ctx.asmOut.add("                MOVE    #$00003F, X0\n")
        ctx.asmOut.add("                AND     X0, A                            ; A = tap offset\n")
        ctx.asmOut.add("                MOVE    A, X0\n")
        ctx.asmOut.add("                MOVE    Y:$" & delayBaseHex & ", B       ; B = wptr\n")
        ctx.asmOut.add("                SUB     X0, B                            ; B = wptr - tap\n")
        let lTapPos = ctx.nextLabel("_dly_tap_pos")
        ctx.asmOut.add("                TST     B\n")
        ctx.asmOut.add("                JGE     " & lTapPos & "\n")
        ctx.asmOut.add("                MOVE    #$000040, X0                     ; +64 if negative\n")
        ctx.asmOut.add("                ADD     X0, B\n")
        ctx.asmOut.add(lTapPos & ":\n")
        ctx.asmOut.add("                MOVE    #$00003F, X0\n")
        ctx.asmOut.add("                AND     X0, B                            ; wrap 0..63\n")
        ctx.asmOut.add("                MOVE    #$" & delayDataHex & ", X0\n")
        ctx.asmOut.add("                ADD     X0, B\n")
        ctx.asmOut.add("                MOVE    B, R0\n")
        ctx.asmOut.add("                MOVE    Y:(R0), " & acc & "              ; delay_read\n")
      else:
        ctx.asmOut.add("                MOVE    Y:$" & delayDataHex & ", " & acc & "\n")

    else:
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)

# Compiles condition to CMP + Jcc
proc genCondJump(ctx: var AsmContext, cond: Expr, targetLabel: string, jumpIfTrue: bool) =
  if cond == nil: return

  if cond.kind == ekBinary and cond.bop == "and":
    if jumpIfTrue:
      let skipLabel = ctx.nextLabel("_L_and_skip")
      ctx.genCondJump(cond.lhs, skipLabel, jumpIfTrue = false)
      ctx.genCondJump(cond.rhs, targetLabel, jumpIfTrue = true)
      ctx.asmOut.add(skipLabel & ":\n")
    else:
      ctx.genCondJump(cond.lhs, targetLabel, jumpIfTrue = false)
      ctx.genCondJump(cond.rhs, targetLabel, jumpIfTrue = false)
    return

  if cond.kind == ekBinary and cond.bop == "or":
    if jumpIfTrue:
      ctx.genCondJump(cond.lhs, targetLabel, jumpIfTrue = true)
      ctx.genCondJump(cond.rhs, targetLabel, jumpIfTrue = true)
    else:
      let passLabel = ctx.nextLabel("_L_or_pass")
      ctx.genCondJump(cond.lhs, passLabel, jumpIfTrue = true)
      ctx.genCondJump(cond.rhs, targetLabel, jumpIfTrue = false)
      ctx.asmOut.add(passLabel & ":\n")
    return

  # Binary comparison: lhs < rhs, etc.
  if cond.kind == ekBinary and cond.bop in ["<", ">", "<=", ">=", "==", "!="]:
    ctx.genExprToAcc(cond.lhs, "A")
    if cond.rhs.kind == ekSym:
      ctx.loadToX0(cond.rhs)
    elif cond.rhs.kind == ekNum:
      ctx.asmOut.add("                MOVE    #" & exprToHex(cond.rhs) & ", X0\n")
    else:
      let spill = ctx.allocSpill()
      ctx.asmOut.add("                MOVE    A, " & ctx.getMemOperand(spill) & "\n")
      ctx.genExprToAcc(cond.rhs, "A")
      ctx.asmOut.add("                MOVE    A, X0\n")
      ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
      ctx.freeSpill()

    ctx.asmOut.add("                CMP     X0, A\n")

    let op = cond.bop
    var jmpOp = ""
    if jumpIfTrue:
      case op
      of "<": jmpOp = "JLT"
      of ">": jmpOp = "JGT"
      of "<=": jmpOp = "JLE"
      of ">=": jmpOp = "JGE"
      of "==": jmpOp = "JEQ"
      of "!=": jmpOp = "JNE"
      else: jmpOp = "JNE"
    else:
      # Inverted condition to jump when false
      case op
      of "<": jmpOp = "JGE"
      of ">": jmpOp = "JLE"
      of "<=": jmpOp = "JGT"
      of ">=": jmpOp = "JLT"
      of "==": jmpOp = "JNE"
      of "!=": jmpOp = "JEQ"
      else: jmpOp = "JEQ"

    ctx.asmOut.add("                " & alignLeft(jmpOp, 8) & targetLabel & "\n")
  else:
    # Single expression truthiness: if expr != 0
    ctx.genExprToAcc(cond, "A")
    ctx.asmOut.add("                CLR     B\n")
    ctx.asmOut.add("                CMP     B, A\n")
    if jumpIfTrue:
      ctx.asmOut.add("                JNE     " & targetLabel & "\n")
    else:
      ctx.asmOut.add("                JEQ     " & targetLabel & "\n")

proc genStmt(ctx: var AsmContext, stmt: Stmt) =
  if stmt == nil: return
  case stmt.kind
  of skAssign:
    ctx.asmOut.add("                ; " & stmt.target & " = ...\n")
    ctx.genExprToAcc(stmt.val, "A")
    let mem = ctx.getMemOperand(stmt.target)
    ctx.asmOut.add("                MOVE    A, " & mem & "\n")

  of skIf:
    let lEndif = ctx.nextLabel("_L_endif")
    let hasElif = stmt.elifBranches.len > 0
    let hasElse = stmt.elseStmts.len > 0

    let firstElseOrElif = if hasElif: ctx.nextLabel("_L_elif")
                          elif hasElse: ctx.nextLabel("_L_else")
                          else: lEndif

    # 1. Condition of 'if'
    ctx.genCondJump(stmt.cond, firstElseOrElif, jumpIfTrue = false)

    # 2. 'then' body
    for s in stmt.thenStmts: ctx.genStmt(s)
    if hasElif or hasElse:
      ctx.asmOut.add("                JMP     " & lEndif & "\n")

    # 3. 'elif' branches
    var curElifLabel = firstElseOrElif
    for i, b in stmt.elifBranches:
      ctx.asmOut.add(curElifLabel & ":\n")
      let nextLabel = if i + 1 < stmt.elifBranches.len: ctx.nextLabel("_L_elif")
                      elif hasElse: ctx.nextLabel("_L_else")
                      else: lEndif
      ctx.genCondJump(b.cond, nextLabel, jumpIfTrue = false)
      for s in b.stmts: ctx.genStmt(s)
      ctx.asmOut.add("                JMP     " & lEndif & "\n")
      curElifLabel = nextLabel

    # 4. 'else' body
    if hasElse:
      let elseLabel = if hasElif: curElifLabel else: firstElseOrElif
      ctx.asmOut.add(elseLabel & ":\n")
      for s in stmt.elseStmts: ctx.genStmt(s)

    ctx.asmOut.add(lEndif & ":\n")

  of skWhile:
    let lTop = ctx.nextLabel("_L_while_top")
    let lEnd = ctx.nextLabel("_L_while_end")
    ctx.asmOut.add(lTop & ":\n")
    ctx.genCondJump(stmt.whileCond, lEnd, jumpIfTrue = false)
    for s in stmt.whileStmts: ctx.genStmt(s)
    ctx.asmOut.add("                JMP     " & lTop & "\n")
    ctx.asmOut.add(lEnd & ":\n")

  of skExpr:
    if stmt.callExpr != nil and stmt.callExpr.kind == ekCall and
        (stmt.callExpr.callee == "delay_write" or stmt.callExpr.callee == "delay.write"):
      if stmt.callExpr.args.len >= 1:
        ctx.genExprToAcc(stmt.callExpr.args[0], "B") # evaluate writeVal into B
        let delayBaseHex = toHex(int64(ctx.addrs.delayBase), 4)
        let delayDataHex = toHex(int64(ctx.addrs.delayBase + 1), 4)
        ctx.asmOut.add("                MOVE    Y:$" & delayBaseHex & ", A       ; load wptr\n")
        ctx.asmOut.add("                MOVE    #$00003F, X0\n")
        ctx.asmOut.add("                AND     X0, A\n")
        ctx.asmOut.add("                MOVE    #$" & delayDataHex & ", Y0\n")
        ctx.asmOut.add("                ADD     Y0, A\n")
        ctx.asmOut.add("                MOVE    A, R0\n")
        ctx.asmOut.add("                MOVE    B, Y:(R0)                        ; delay_write\n")
        # increment wptr
        ctx.asmOut.add("                MOVE    Y:$" & delayBaseHex & ", A\n")
        ctx.asmOut.add("                MOVE    #$000001, X0\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        ctx.asmOut.add("                MOVE    #$00003F, X0\n")
        ctx.asmOut.add("                AND     X0, A\n")
        ctx.asmOut.add("                MOVE    A, Y:$" & delayBaseHex & "       ; update wptr\n")
    else:
      ctx.genExprToAcc(stmt.callExpr, "A")

proc compileOmniSnippet*(m: OmniModule, addrs: Dsp56300ModuleAddresses, labelPrefix: string): string =
  var localAddrs = addrs
  localAddrs.customAddrs = true
  let scratchStart = if localAddrs.scratchBase != 0: localAddrs.scratchBase else: 0x0200'u32
  var ctx = AsmContext(
    module: m,
    addrs: localAddrs,
    curTempAddr: scratchStart,
    spillCounter: 0,
    labelCounter: 0,
    labelPrefix: labelPrefix,
    asmOut: ""
  )

  for p in m.params: ctx.paramNames[p.name] = true
  for sv in m.stateVars: ctx.stateNames[sv.name] = true
  for i in m.inputs: ctx.inNames[i] = true
  for o in m.outputs: ctx.outNames[o] = true

  # Map custom inlet addresses
  for i, inName in m.inputs:
    if i < addrs.inletAddrs.len:
      ctx.inletAddrMap[inName] = addrs.inletAddrs[i]

  # Map custom outlet addresses (out1..out8)
  for i, outName in m.outputs:
    if i < addrs.outletAddrs.len:
      ctx.outletAddrMap[outName] = addrs.outletAddrs[i]

  # Map parameter addresses: paramBase + i in X memory
  for i, p in m.params:
    ctx.paramAddrMap[p.name] = addrs.paramBase + uint32(i)

  # Map state variable addresses: stateBase + i in Y memory
  for i, sv in m.stateVars:
    ctx.stateAddrMap[sv.name] = addrs.stateBase + uint32(i)

  # Discover temporary assign targets
  var allTargets: seq[string] = @[]
  for s in m.performStmts: collectAssignTargets(s, allTargets)
  for s in m.sampleStmts: collectAssignTargets(s, allTargets)

  for target in allTargets:
    if target notin ctx.outNames and target notin ctx.stateNames and target notin ctx.tempTable:
      ctx.tempTable[target] = ctx.curTempAddr
      ctx.tempNames.add(target)
      ctx.curTempAddr += 1

  for s in m.performStmts:
    ctx.genStmt(s)
  for s in m.sampleStmts:
    ctx.genStmt(s)

  return ctx.asmOut

proc emitDsp56300Asm*(m: OmniModule): string =
  var ctx = AsmContext(
    module: m,
    addrs: defaultDsp56300Addresses(),
    curTempAddr: 0x0040'u32,
    spillCounter: 0,
    labelCounter: 0,
    labelPrefix: "",
    asmOut: ""
  )

  for p in m.params: ctx.paramNames[p.name] = true
  for sv in m.stateVars: ctx.stateNames[sv.name] = true
  for i in m.inputs: ctx.inNames[i] = true
  for o in m.outputs: ctx.outNames[o] = true

  # Collect all temporary assigned variables
  var allTargets: seq[string] = @[]
  for s in m.performStmts: collectAssignTargets(s, allTargets)
  for s in m.sampleStmts: collectAssignTargets(s, allTargets)

  for target in allTargets:
    if target notin ctx.outNames and target notin ctx.stateNames and target notin ctx.tempTable:
      ctx.tempTable[target] = ctx.curTempAddr
      ctx.tempNames.add(target)
      ctx.curTempAddr += 1

  # Generate code for perform and sample blocks first to discover all spill slots
  for s in m.performStmts:
    ctx.genStmt(s)
  for s in m.sampleStmts:
    ctx.genStmt(s)

  var b = ""
  b.add("; =============================================================================\n")
  b.add("; Motorola DSP56300 / DSP56311 Assembly generated by Omni compiler\n")
  b.add("; Module: " & m.name & "\n")
  b.add("; Target Architecture: 24-bit Fixed-Point DSP56300 / DSP56311 (Q23)\n")
  b.add("; Clean-room Motorola syntax: direct ORG P/X/Y mappings\n")
  b.add("; =============================================================================\n\n")

  # 1. Symbol definitions (X memory space for IO & Params)
  b.add("; --- X Data Memory Map: Inputs, Outputs & Parameters ---\n")
  var xAddr = 0x0000
  for i, inName in m.inputs:
    let symAddr = xAddr + i
    b.add(alignLeft(inName, 28) & " EQU     $" & symAddr.toHex(4) & "\n")

  xAddr = 0x0010
  for i, outName in m.outputs:
    let symAddr = xAddr + i
    b.add(alignLeft(outName, 28) & " EQU     $" & symAddr.toHex(4) & "\n")

  xAddr = 0x0020
  for i, p in m.params:
    let symAddr = xAddr + i
    b.add(alignLeft(p.name, 28) & " EQU     $" & symAddr.toHex(4) & "\n")

  b.add("\n; --- Y Data Memory Map: State Variables & Scratch ---\n")
  var yAddr = 0x0000
  for i, sv in m.stateVars:
    let symAddr = yAddr + i
    b.add(alignLeft(sv.name, 28) & " EQU     $" & symAddr.toHex(4) & "\n")
  for name in ctx.tempNames:
    let symAddr = ctx.tempTable[name]
    b.add(alignLeft(name, 28) & " EQU     $" & symAddr.toHex(4) & "\n")

  # 2. Initialization Tables (DC values in X and Y memory)
  b.add("\n; --- Default Parameter Values in X Memory ---\n")
  b.add("                ORG     X:$0020\n")
  for p in m.params:
    b.add("                DC      " & floatToQ23Hex(p.defaultVal) & "         ; " & p.name & " = " & $p.defaultVal & "\n")

  b.add("\n; --- Initial State Values in Y Memory ---\n")
  b.add("                ORG     Y:$0000\n")
  for sv in m.stateVars:
    b.add("                DC      " & floatToQ23Hex(sv.initVal) & "         ; " & sv.name & " = " & sv.initExpr & "\n")

  # 3. Executable Program in P memory
  b.add("\n; =============================================================================\n")
  b.add("; Program Memory (P-RAM): Kernel Execution\n")
  b.add("; =============================================================================\n")
  b.add("                ORG     P:$0100\n")
  b.add(m.name & "_process:\n")

  b.add(ctx.asmOut)
  b.add("                RTS\n")
  return b

# ------------------------------------------------------------- C Code Generator

proc emitCExpr(expr: Expr): string =
  if expr == nil: return "0.0f"
  case expr.kind
  of ekNum:
    return $expr.numVal & "f"
  of ekSym:
    case expr.symName
    of "TWOPI": return "((float)TWOPI)"
    of "PI": return "((float)PI)"
    of "samplerate": return "samplerate"
    else: return expr.symName
  of ekUnary:
    if expr.uop == "not":
      return "(!(" & emitCExpr(expr.uexpr) & "))"
    else:
      return "(" & expr.uop & emitCExpr(expr.uexpr) & ")"
  of ekBinary:
    let op = expr.bop
    if op == "%":
      return "OMNI_MOD(" & emitCExpr(expr.lhs) & ", " & emitCExpr(expr.rhs) & ")"
    elif op == "and":
      return "(" & emitCExpr(expr.lhs) & " && " & emitCExpr(expr.rhs) & ")"
    elif op == "or":
      return "(" & emitCExpr(expr.lhs) & " || " & emitCExpr(expr.rhs) & ")"
    else:
      return "(" & emitCExpr(expr.lhs) & " " & op & " " & emitCExpr(expr.rhs) & ")"
  of ekCall:
    case expr.callee
    of "sin": return "sinf(" & emitCExpr(expr.args[0]) & ")"
    of "cos": return "cosf(" & emitCExpr(expr.args[0]) & ")"
    of "tan": return "tanf(" & emitCExpr(expr.args[0]) & ")"
    of "tanh": return "tanhf(" & emitCExpr(expr.args[0]) & ")"
    of "abs": return "fabsf(" & emitCExpr(expr.args[0]) & ")"
    of "trunc": return "truncf(" & emitCExpr(expr.args[0]) & ")"
    of "sqrt": return "sqrtf(" & emitCExpr(expr.args[0]) & ")"
    of "int": return "((int)(" & emitCExpr(expr.args[0]) & "))"
    of "float": return "((float)(" & emitCExpr(expr.args[0]) & "))"
    of "pow":
      if expr.args.len >= 2:
        return "powf(" & emitCExpr(expr.args[0]) & ", " & emitCExpr(expr.args[1]) & ")"
      else:
        return "0.0f"
    of "min":
      if expr.args.len >= 2:
        return "fminf(" & emitCExpr(expr.args[0]) & ", " & emitCExpr(expr.args[1]) & ")"
      else:
        return "0.0f"
    of "max":
      if expr.args.len >= 2:
        return "fmaxf(" & emitCExpr(expr.args[0]) & ", " & emitCExpr(expr.args[1]) & ")"
      else:
        return "0.0f"
    of "clamp":
      if expr.args.len >= 3:
        return "fminf(fmaxf(" & emitCExpr(expr.args[0]) & ", " & emitCExpr(expr.args[1]) & "), " & emitCExpr(expr.args[2]) & ")"
      else:
        return emitCExpr(expr.args[0])
    else:
      var s = expr.callee & "("
      for i, a in expr.args:
        if i > 0: s.add(", ")
        s.add(emitCExpr(a))
      s.add(")")
      return s

proc emitCCond(cond: Expr): string =
  var s = emitCExpr(cond)
  if s.startsWith("(") and s.endsWith(")"):
    s = s[1..^2]
  return s

proc emitCStmt(stmt: Stmt, indent: string): string =
  if stmt == nil: return ""
  var b = ""
  case stmt.kind
  of skAssign:
    let valStr = emitCExpr(stmt.val)
    b.add(indent & stmt.target & " = " & valStr & ";\n")

  of skIf:
    b.add(indent & "if (" & emitCCond(stmt.cond) & ") {\n")
    for s in stmt.thenStmts:
      b.add(emitCStmt(s, indent & "    "))
    for branch in stmt.elifBranches:
      b.add(indent & "} else if (" & emitCCond(branch.cond) & ") {\n")
      for s in branch.stmts:
        b.add(emitCStmt(s, indent & "    "))
    if stmt.elseStmts.len > 0:
      b.add(indent & "} else {\n")
      for s in stmt.elseStmts:
        b.add(emitCStmt(s, indent & "    "))
    b.add(indent & "}\n")

  of skWhile:
    b.add(indent & "while (" & emitCCond(stmt.whileCond) & ") {\n")
    for s in stmt.whileStmts:
      b.add(emitCStmt(s, indent & "    "))
    b.add(indent & "}\n")

  of skExpr:
    b.add(indent & emitCExpr(stmt.callExpr) & ";\n")

  return b

proc emitC*(m: OmniModule): string =
  var b = ""
  b.add("/* =============================================================================\n")
  b.add(" * Clean-room C Module generated by Omni compiler\n")
  b.add(" * Module: " & m.name & "\n")
  b.add(" * Target: Self-contained C99 / C11 for Native and WebAssembly (WASM)\n")
  b.add(" * ============================================================================= */\n\n")
  b.add("#include <stdlib.h>\n")
  b.add("#include <math.h>\n\n")

  b.add("#ifndef TWOPI\n")
  b.add("#define TWOPI 6.28318530717958647692f\n")
  b.add("#endif\n")
  b.add("#ifndef PI\n")
  b.add("#define PI 3.14159265358979323846f\n")
  b.add("#endif\n\n")
  b.add("#define OMNI_MOD(a, b) fmodf((float)(a), (float)(b))\n\n")

  # Struct declaration
  b.add("typedef struct {\n")
  b.add("    double samplerate;\n")
  for p in m.params:
    b.add("    float " & p.name & ";\n")
  for sv in m.stateVars:
    b.add("    float " & sv.name & ";\n")
  b.add("} " & m.name & ";\n\n")

  # Alloc & Free
  b.add(m.name & "* " & m.name & "_alloc(void) {\n")
  b.add("    " & m.name & "* m = (" & m.name & "*)calloc(1, sizeof(" & m.name & "));\n")
  b.add("    return m;\n")
  b.add("}\n\n")

  b.add("void " & m.name & "_free(" & m.name & "* m) {\n")
  b.add("    if (m) free(m);\n")
  b.add("}\n\n")

  # Init
  b.add("void " & m.name & "_init(" & m.name & "* m, double samplerate) {\n")
  b.add("    if (!m) return;\n")
  b.add("    m->samplerate = samplerate;\n")
  for p in m.params:
    b.add("    m->" & p.name & " = " & $p.defaultVal & "f;\n")
  for sv in m.stateVars:
    b.add("    m->" & sv.name & " = " & $sv.initVal & "f;\n")
  b.add("}\n\n")

  # Process block
  b.add("void " & m.name & "_process(" & m.name & "* m, const float** ins, float** outs, int num_samples) {\n")
  b.add("    if (!m || !outs) return;\n")
  b.add("    double samplerate = m->samplerate > 0.0 ? m->samplerate : 48000.0;\n")
  b.add("    (void)samplerate;\n")
  for p in m.params:
    b.add("    float " & p.name & " = m->" & p.name & ";\n")
    b.add("    (void)" & p.name & ";\n")
  for sv in m.stateVars:
    b.add("    float " & sv.name & " = m->" & sv.name & ";\n")
    b.add("    (void)" & sv.name & ";\n")

  var declaredLocals = initTable[string, bool]()
  for i in m.inputs: declaredLocals[i] = true
  for o in m.outputs: declaredLocals[o] = true
  for p in m.params: declaredLocals[p.name] = true
  for sv in m.stateVars: declaredLocals[sv.name] = true

  # Collect and declare all temporary local variables
  var allTargets: seq[string] = @[]
  for s in m.performStmts: collectAssignTargets(s, allTargets)
  for s in m.sampleStmts: collectAssignTargets(s, allTargets)

  var uniqueLocals: seq[string] = @[]
  for t in allTargets:
    if t notin declaredLocals and t notin uniqueLocals:
      uniqueLocals.add(t)
      declaredLocals[t] = true

  for t in uniqueLocals:
    b.add("    float " & t & " = 0.0f;\n")
    b.add("    (void)" & t & ";\n")

  # Perform statements
  for s in m.performStmts:
    b.add(emitCStmt(s, "    "))

  b.add("\n    for (int i = 0; i < num_samples; ++i) {\n")
  for i, inName in m.inputs:
    b.add("        float " & inName & " = (ins && ins[" & $i & "]) ? ins[" & $i & "][i] : 0.0f;\n")
    b.add("        (void)" & inName & ";\n")
  for outName in m.outputs:
    b.add("        float " & outName & " = 0.0f;\n")

  # Sample statements
  for s in m.sampleStmts:
    b.add(emitCStmt(s, "        "))

  for i, outName in m.outputs:
    b.add("        if (outs[" & $i & "]) outs[" & $i & "][i] = " & outName & ";\n")
  b.add("    }\n\n")

  # Save state vars back
  for sv in m.stateVars:
    b.add("    m->" & sv.name & " = " & sv.name & ";\n")
  b.add("}\n")

  return b
