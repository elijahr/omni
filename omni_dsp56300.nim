## MIT Motorola DSP56300 Assembly Code Generator for Omni DSL (.omni)
## Translates standalone .omni DSP modules into Motorola DSP56300 / DSP56311 Assembly (.asm)
## for 24-bit hardware execution.

import std/[strutils, tables, math]
import omni_ast, omni_c

export omni_ast, omni_c

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
  OmniStage* = enum
    stageAll,
    stagePerform,
    stageSample

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

proc accReg1(acc: string): string {.inline.} =
  if acc == "A": "A1"
  elif acc == "B": "B1"
  else: acc


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
    ctx.asmOut.add("                MOVE    A1, X0\n")

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
    ctx.asmOut.add("                MOVE    A1, Y0\n")

proc genDivide(ctx: var AsmContext, lhs: Expr, rhs: Expr, acc: string = "A") =
  # True 24-bit restoring shift-subtract fixed-point division: A = lhs / rhs
  # Evaluates lhs into A, rhs into X0.
  # Handles division by zero, signed values, and saturation.
  ctx.genExprToAcc(lhs, "A")

  if isLeaf(rhs):
    ctx.loadToX0(rhs)
  else:
    let spill = ctx.allocSpill()
    ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
    ctx.genExprToAcc(rhs, "A")
    ctx.asmOut.add("                MOVE    A1, X0\n")
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
  ctx.asmOut.add("                MOVE    B1, X0                ; X0 = |rhs|\n")
  ctx.asmOut.add("                MOVE    Y1, B\n")
  ctx.asmOut.add("                MOVE    #$000001, X1\n")
  ctx.asmOut.add("                EOR     X1, B\n")
  ctx.asmOut.add("                MOVE    B1, Y1                ; toggle sign\n")
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
    ctx.asmOut.add("                MOVE    B1, " & acc & "\n")

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
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")
      else:
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, " & acc & "\n")
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")
      return

    # Multiply-Accumulate (MAC): a + b * c  or  b * c + a
    # Only use unspilled fast-path when BOTH operands of '*' are leaf nodes (ekSym or ekNum)
    # to prevent clobbering accumulator A during loadToX0 / loadToY0.
    if op == "+":
      if expr.rhs.kind == ekBinary and expr.rhs.bop == "*" and
          isLeaf(expr.rhs.lhs) and isLeaf(expr.rhs.rhs) and
          not (expr.rhs.lhs.kind == ekNum and abs(expr.rhs.lhs.numVal) > 1.0) and
          not (expr.rhs.rhs.kind == ekNum and abs(expr.rhs.rhs.numVal) > 1.0):
        ctx.genExprToAcc(expr.lhs, acc)
        ctx.loadToX0(expr.rhs.lhs)
        ctx.loadToY0(expr.rhs.rhs)
        ctx.asmOut.add("                MAC     X0, Y0, " & acc & "\n")
        return
      elif expr.lhs.kind == ekBinary and expr.lhs.bop == "*" and
          isLeaf(expr.lhs.lhs) and isLeaf(expr.lhs.rhs) and
          not (expr.lhs.lhs.kind == ekNum and abs(expr.lhs.lhs.numVal) > 1.0) and
          not (expr.lhs.rhs.kind == ekNum and abs(expr.lhs.rhs.numVal) > 1.0):
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.loadToX0(expr.lhs.lhs)
        ctx.loadToY0(expr.lhs.rhs)
        ctx.asmOut.add("                MAC     X0, Y0, " & acc & "\n")
        return

    # Multiply-Subtract: a - b * c
    if op == "-":
      if expr.rhs.kind == ekBinary and expr.rhs.bop == "*" and
          isLeaf(expr.rhs.lhs) and isLeaf(expr.rhs.rhs) and
          not (expr.rhs.lhs.kind == ekNum and abs(expr.rhs.lhs.numVal) > 1.0) and
          not (expr.rhs.rhs.kind == ekNum and abs(expr.rhs.rhs.numVal) > 1.0):
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
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")
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
        elif abs(expr.rhs.numVal) > 1.0:
          var scaled = expr.rhs.numVal
          var k = 0
          while abs(scaled) > 0.99999988 and k < 16:
            scaled *= 0.5
            inc k
          ctx.genExprToAcc(expr.lhs, acc)
          ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
          ctx.asmOut.add("                MOVE    #" & floatToQ23Hex(scaled) & ", Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, " & acc & "\n")
          for _ in 0 ..< k:
            ctx.asmOut.add("                ASL     " & acc & "\n")
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
        elif abs(expr.lhs.numVal) > 1.0:
          var scaled = expr.lhs.numVal
          var k = 0
          while abs(scaled) > 0.99999988 and k < 16:
            scaled *= 0.5
            inc k
          ctx.genExprToAcc(expr.rhs, acc)
          ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
          ctx.asmOut.add("                MOVE    #" & floatToQ23Hex(scaled) & ", Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, " & acc & "\n")
          for _ in 0 ..< k:
            ctx.asmOut.add("                ASL     " & acc & "\n")
          return

      ctx.genExprToAcc(expr.lhs, acc)
      ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
      if expr.rhs.kind == ekSym:
        ctx.loadToY0(expr.rhs)
      elif expr.rhs.kind == ekNum:
        ctx.asmOut.add("                MOVE    #" & exprToHex(expr.rhs) & ", Y0\n")
      else:
        let spill = ctx.allocSpill()
        ctx.asmOut.add("                MOVE    X0, " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", Y0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", X0\n")
        ctx.freeSpill()
      ctx.asmOut.add("                MPY     X0, Y0, " & acc & "\n")
      return

    # Division: a / b
    if op == "/":
      # Division by samplerate -> reciprocal multiplication (1/48000 = #$000055)
      if expr.rhs.kind == ekSym and expr.rhs.symName == "samplerate":
        ctx.genExprToAcc(expr.lhs, acc)
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
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
          ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
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
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", " & acc & "\n")
        ctx.freeSpill()

      if op == "+":
        ctx.asmOut.add("                ADD     X0, " & acc & "\n")
      else:
        ctx.asmOut.add("                SUB     X0, " & acc & "\n")
      return

    # Bitwise AND: a & b
    if op == "&":
      ctx.genExprToAcc(expr.lhs, acc)
      if expr.rhs.kind == ekSym:
        ctx.loadToX0(expr.rhs)
      elif expr.rhs.kind == ekNum:
        ctx.asmOut.add("                MOVE    #" & exprToHex(expr.rhs) & ", X0\n")
      else:
        let spill = ctx.allocSpill()
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", " & acc & "\n")
        ctx.freeSpill()
      ctx.asmOut.add("                AND     X0, " & acc & "\n")
      ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")
      return

    # Bitwise OR: a | b
    if op == "|":
      ctx.genExprToAcc(expr.lhs, acc)
      if expr.rhs.kind == ekSym:
        ctx.loadToX0(expr.rhs)
      elif expr.rhs.kind == ekNum:
        ctx.asmOut.add("                MOVE    #" & exprToHex(expr.rhs) & ", X0\n")
      else:
        let spill = ctx.allocSpill()
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", " & acc & "\n")
        ctx.freeSpill()
      ctx.asmOut.add("                OR      X0, " & acc & "\n")
      ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")
      return

    # Bitwise XOR: a ^ b
    if op == "^" or op == "xor":
      ctx.genExprToAcc(expr.lhs, acc)
      if expr.rhs.kind == ekSym:
        ctx.loadToX0(expr.rhs)
      elif expr.rhs.kind == ekNum:
        ctx.asmOut.add("                MOVE    #" & exprToHex(expr.rhs) & ", X0\n")
      else:
        let spill = ctx.allocSpill()
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & ctx.getMemOperand(spill) & "\n")
        ctx.genExprToAcc(expr.rhs, acc)
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", X0\n")
        ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", " & acc & "\n")
        ctx.freeSpill()
      ctx.asmOut.add("                EOR     X0, " & acc & "\n")
      ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")
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
        ctx.asmOut.add("                MOVE    A1, A                ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$400000, X0         ; 0.5\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JGE     " & lSinNeg & "\n")
        ctx.asmOut.add("                MOVE    A1, Y0                ; Y0 = u\n")
        ctx.asmOut.add("                MOVE    #$400000, B          ; B = 0.5\n")
        ctx.asmOut.add("                SUB     Y0, B                ; B = 0.5 - u\n")
        ctx.asmOut.add("                MOVE    B1, X0                ; X0 = 0.5 - u\n")
        ctx.asmOut.add("                MPY     X0, Y0, A            ; A = 2 * u * (0.5 - u)\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n                ASL     A\n")
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
        ctx.asmOut.add("                MOVE    A1, Y0                ; Y0 = u'\n")
        ctx.asmOut.add("                MOVE    #$400000, B          ; B = 0.5\n")
        ctx.asmOut.add("                SUB     Y0, B                ; B = 0.5 - u'\n")
        ctx.asmOut.add("                MOVE    B1, X0                ; X0 = 0.5 - u'\n")
        ctx.asmOut.add("                MPY     X0, Y0, A            ; A = 2 * u' * (0.5 - u')\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n                ASL     A\n")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & lSinNegClamp & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lSinNegClamp & ":\n")
        ctx.asmOut.add("                NEG     A\n")
        ctx.asmOut.add(lSinDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "cos":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    #$200000, X0         ; +0.25 phase shift for cos\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        let lCosNeg = ctx.nextLabel("_L_cos_neg")
        let lCosDone = ctx.nextLabel("_L_cos_done")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, A\n")
        ctx.asmOut.add("                MOVE    A1, A                ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$400000, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JGE     " & lCosNeg & "\n")
        ctx.asmOut.add("                MOVE    A1, Y0\n")
        ctx.asmOut.add("                MOVE    #$400000, B\n")
        ctx.asmOut.add("                SUB     Y0, B\n")
        ctx.asmOut.add("                MOVE    B1, X0\n")
        ctx.asmOut.add("                MPY     X0, Y0, A\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n                ASL     A\n")
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
        ctx.asmOut.add("                MOVE    A1, Y0\n")
        ctx.asmOut.add("                MOVE    #$400000, B\n")
        ctx.asmOut.add("                SUB     Y0, B\n")
        ctx.asmOut.add("                MOVE    B1, X0\n")
        ctx.asmOut.add("                MPY     X0, Y0, A\n")
        ctx.asmOut.add("                ASL     A\n                ASL     A\n                ASL     A\n                ASL     A\n")
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & lCosNegClamp & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lCosNegClamp & ":\n")
        ctx.asmOut.add("                NEG     A\n")
        ctx.asmOut.add(lCosDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "tan":
      # Filter cutoff authentic polynomial: x * (1 + x^2 * (1/3 + 2/15 * x^2))
      # Evaluated without accumulator overflow as x + x * [x^2 * (1/3 + 2/15 * x^2)]
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    A1, X0\n")
        ctx.asmOut.add("                MPY     X0, X0, A            ; A = x^2\n")
        ctx.asmOut.add("                MOVE    A1, Y0                ; Y0 = x^2\n")
        ctx.asmOut.add("                MOVE    #$111111, Y1         ; 2/15 in Q23\n")
        ctx.asmOut.add("                MPY     Y0, Y1, B            ; B = (2/15) * x^2\n")
        ctx.asmOut.add("                MOVE    #$2AAAAA, Y1         ; 1/3 in Q23\n")
        ctx.asmOut.add("                ADD     Y1, B                ; B = 1/3 + 2/15 * x^2\n")
        ctx.asmOut.add("                MOVE    B1, Y1\n")
        ctx.asmOut.add("                MPY     Y0, Y1, B            ; B = x^2 * (1/3 + 2/15 * x^2)\n")
        ctx.asmOut.add("                MOVE    B1, Y1\n")
        ctx.asmOut.add("                MPY     X0, Y1, B            ; B = x * (x^2 * (1/3 + 2/15 * x^2))\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add("                ADD     B, A                 ; A = x + B\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "saturate", "cubic", "tanh":
      # Real DSP56300 polynomial cubic saturation: x * (1 - 1/3 * x^2) = x - x*x*x * 0.3333333
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    A1, X0\n")
        ctx.asmOut.add("                MPY     X0, X0, B\n")
        ctx.asmOut.add("                MOVE    B1, Y0\n")
        ctx.asmOut.add("                MOVE    #$2AAAAA, Y1         ; 1/3 in Q23\n")
        ctx.asmOut.add("                MPY     -Y0, Y1, B           ; -1/3 * x^2\n")
        ctx.asmOut.add("                MOVE    #$7FFFFF, Y1         ; 1.0 in Q23\n")
        ctx.asmOut.add("                ADD     Y1, B                ; 1.0 - 1/3 * x^2\n")
        ctx.asmOut.add("                MOVE    B1, Y1\n")
        ctx.asmOut.add("                MPY     X0, Y1, " & acc & "            ; x * (1 - 1/3 * x^2)\n")

    of "lfsr":
      # 24-bit Galois LFSR with standard Clavia polynomial 0x40001B (or custom poly in arg 1)
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        let polyHex = if expr.args.len >= 2 and expr.args[1].kind == ekNum:
                        exprToHex(expr.args[1])
                      else:
                        "40001B"
        let lNoCarry = ctx.nextLabel("_lfsr_no_carry")
        ctx.asmOut.add("                ASL     A                    ; shift left, bit 23 into carry\n")
        ctx.asmOut.add("                JCC     " & lNoCarry & "         ; skip XOR if carry clear\n")
        ctx.asmOut.add("                MOVE    #$" & polyHex & ", X0\n")
        ctx.asmOut.add("                EOR     X0, A                ; Galois feedback polynomial\n")
        ctx.asmOut.add(lNoCarry & ":\n")
        ctx.asmOut.add("                MOVE    A1, A                ; normalize extension A2 to A1 sign\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "poly5":
      # 5th-order minimax polynomial (Horner form): x * (c1 + x^2 * (c3 + c5 * x^2))
      # Scaled by 0.5 internally to maintain Q23 range for c1 > 1.0 (e.g. pi/2 = 1.5707963)
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    A1, X0                ; x\n")
        ctx.asmOut.add("                MPY     X0, X0, B            ; x^2\n")
        ctx.asmOut.add("                MOVE    B1, Y0                ; x^2\n")
        var c5Hex = "0519EE" # 0.0796926 / 2 in Q23
        var c3Hex = "D6A8CA" # -0.6459641 / 2 in Q23
        var c1Hex = "6487ED" # 1.5707963 / 2 in Q23
        if expr.args.len >= 4:
          if expr.args[1].kind == ekNum: c1Hex = floatToQ23Hex(expr.args[1].numVal * 0.5)
          if expr.args[2].kind == ekNum: c3Hex = floatToQ23Hex(expr.args[2].numVal * 0.5)
          if expr.args[3].kind == ekNum: c5Hex = floatToQ23Hex(expr.args[3].numVal * 0.5)
        ctx.asmOut.add("                MOVE    #$" & c5Hex & ", Y1         ; c5/2\n")
        ctx.asmOut.add("                MPY     Y0, Y1, B            ; (c5/2) * x^2\n")
        ctx.asmOut.add("                MOVE    #$" & c3Hex & ", Y1         ; c3/2\n")
        ctx.asmOut.add("                ADD     Y1, B                ; c3/2 + (c5/2) * x^2\n")
        ctx.asmOut.add("                MOVE    B1, Y1\n")
        ctx.asmOut.add("                MPY     Y0, Y1, B            ; x^2 * (c3/2 + ...)\n")
        ctx.asmOut.add("                MOVE    #$" & c1Hex & ", Y1         ; c1/2\n")
        ctx.asmOut.add("                ADD     Y1, B                ; c1/2 + x^2 * (...)\n")
        ctx.asmOut.add("                MOVE    B1, Y1\n")
        ctx.asmOut.add("                MPY     X0, Y1, A            ; x * (c1/2 + ...)\n")
        ctx.asmOut.add("                ASL     A                    ; double back to full scale\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "bitand":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if expr.args[1].kind == ekSym:
          ctx.loadToX0(expr.args[1])
        elif expr.args[1].kind == ekNum:
          ctx.asmOut.add("                MOVE    #" & exprToHex(expr.args[1]) & ", X0\n")
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                AND     X0, " & acc & "\n")
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")

    of "bitor":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if expr.args[1].kind == ekSym:
          ctx.loadToX0(expr.args[1])
        elif expr.args[1].kind == ekNum:
          ctx.asmOut.add("                MOVE    #" & exprToHex(expr.args[1]) & ", X0\n")
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                OR      X0, " & acc & "\n")
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")

    of "bitxor":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if expr.args[1].kind == ekSym:
          ctx.loadToX0(expr.args[1])
        elif expr.args[1].kind == ekNum:
          ctx.asmOut.add("                MOVE    #" & exprToHex(expr.args[1]) & ", X0\n")
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                EOR     X0, " & acc & "\n")
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")

    of "min":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if isLeaf(expr.args[1]):
          ctx.loadToX0(expr.args[1])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lMinDone = ctx.nextLabel("_L_min_done")
        ctx.asmOut.add("                JLE     " & lMinDone & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lMinDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "max":
      if expr.args.len >= 2:
        ctx.genExprToAcc(expr.args[0], "A")
        if isLeaf(expr.args[1]):
          ctx.loadToX0(expr.args[1])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lMaxDone = ctx.nextLabel("_L_max_done")
        ctx.asmOut.add("                JGE     " & lMaxDone & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lMaxDone & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "clamp":
      if expr.args.len >= 3:
        ctx.genExprToAcc(expr.args[0], "A")
        # Lower bound: if A < lo, A = lo
        if isLeaf(expr.args[1]):
          ctx.loadToX0(expr.args[1])
        else:
          let spill = ctx.allocSpill()
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
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
          ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
          ctx.genExprToAcc(expr.args[2], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    " & ctx.getMemOperand(spill) & ", A\n")
          ctx.freeSpill()
        ctx.asmOut.add("                CMP     X0, A\n")
        let lClampHi = ctx.nextLabel("_L_clamp_hi")
        ctx.asmOut.add("                JLE     " & lClampHi & "\n")
        ctx.asmOut.add("                MOVE    X0, A\n")
        ctx.asmOut.add(lClampHi & ":\n")
        if acc != "A":
          ctx.asmOut.add("                MOVE    A1, " & acc & "\n")

    of "abs":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)
        ctx.asmOut.add("                ABS     " & acc & "\n")

    of "trunc", "int":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)
        ctx.asmOut.add("                MOVE    #$7FFFFF, X0\n")
        ctx.asmOut.add("                AND     X0, " & acc & "                ; Q23 fractional wrapping\n")
        ctx.asmOut.add("                MOVE    " & accReg1(acc) & ", " & acc & "              ; normalize sign extension\n")

    of "float":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)

    of "pow":
      if expr.args.len >= 2:
        # Check if 2.0 ^ x (pitch-to-frequency scaling polynomial)
        let isBaseTwo = (expr.args[0].kind == ekNum and expr.args[0].numVal == 2.0)
        if isBaseTwo:
          ctx.genExprToAcc(expr.args[1], "A")
          ctx.asmOut.add("                MOVE    A1, X0\n")
          ctx.asmOut.add("                MOVE    #$071AC0, Y0         ; c3 = 0.055504 in Q23\n")
          ctx.asmOut.add("                MPY     X0, Y0, B            ; x * c3\n")
          ctx.asmOut.add("                MOVE    #$1EBFCD, Y1         ; c2 = 0.240226 in Q23\n")
          ctx.asmOut.add("                ADD     Y1, B                ; c2 + x * c3\n")
          ctx.asmOut.add("                MOVE    B1, Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, B            ; x * (c2 + x * c3)\n")
          ctx.asmOut.add("                MOVE    #$58B9FE, Y1         ; c1 = 0.693147 in Q23\n")
          ctx.asmOut.add("                ADD     Y1, B                ; c1 + x * (...)\n")
          ctx.asmOut.add("                MOVE    B1, Y0\n")
          ctx.asmOut.add("                MPY     X0, Y0, B            ; x * (c1 + ...)\n")
          ctx.asmOut.add("                MOVE    #$7FFFFF, A          ; 1.0\n")
          ctx.asmOut.add("                ADD     B, A                 ; 1.0 + x * (...)\n")
          if acc != "A":
            ctx.asmOut.add("                MOVE    A1, " & acc & "\n")
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
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    A1, X0\n")
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
        ctx.asmOut.add("                MOVE    B1, B                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$" & delayDataHex & ", X0\n")
        ctx.asmOut.add("                ADD     X0, B\n")
        ctx.asmOut.add("                MOVE    B1, R0\n")
        ctx.asmOut.add("                MOVE    Y:(R0), " & acc & "              ; delay_read\n")
      else:
        ctx.asmOut.add("                MOVE    Y:$" & delayDataHex & ", " & acc & "\n")

    of "lut_pitch":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        let skipScaleLabel = ctx.nextLabel("_lut_scale_ok")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & skipScaleLabel & "\n")
        ctx.asmOut.add("                REP     #16\n")
        ctx.asmOut.add("                ASR     A\n")
        ctx.asmOut.add(skipScaleLabel & ":\n")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                AND     X0, A                            ; mask 0..127\n")
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$001800, X0                     ; Clavia pitch table base\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        ctx.asmOut.add("                MOVE    A1, R0\n")
        ctx.asmOut.add("                MOVE    Y:(R0), " & acc & "              ; lut_pitch\n")

    of "lut_cent":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        let skipScaleLabel = ctx.nextLabel("_lut_scale_ok")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & skipScaleLabel & "\n")
        ctx.asmOut.add("                REP     #16\n")
        ctx.asmOut.add("                ASR     A\n")
        ctx.asmOut.add(skipScaleLabel & ":\n")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                AND     X0, A                            ; mask 0..127\n")
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$001880, X0                     ; Clavia cent table base\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        ctx.asmOut.add("                MOVE    A1, R0\n")
        ctx.asmOut.add("                MOVE    Y:(R0), " & acc & "              ; lut_cent\n")

    of "lut_env":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        let skipScaleLabel = ctx.nextLabel("_lut_scale_ok")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & skipScaleLabel & "\n")
        ctx.asmOut.add("                REP     #16\n")
        ctx.asmOut.add("                ASR     A\n")
        ctx.asmOut.add(skipScaleLabel & ":\n")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                AND     X0, A                            ; mask 0..127\n")
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$001900, X0                     ; Clavia env table base\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        ctx.asmOut.add("                MOVE    A1, R0\n")
        ctx.asmOut.add("                MOVE    Y:(R0), " & acc & "              ; lut_env\n")

    of "lut_wave":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        let skipScaleLabel = ctx.nextLabel("_lut_scale_ok")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                CMP     X0, A\n")
        ctx.asmOut.add("                JLE     " & skipScaleLabel & "\n")
        ctx.asmOut.add("                REP     #16\n")
        ctx.asmOut.add("                ASR     A\n")
        ctx.asmOut.add(skipScaleLabel & ":\n")
        ctx.asmOut.add("                MOVE    #$00007F, X0\n")
        ctx.asmOut.add("                AND     X0, A                            ; mask 0..127\n")
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$0019C0, X0                     ; Clavia wave table base\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        ctx.asmOut.add("                MOVE    A1, R0\n")
        ctx.asmOut.add("                MOVE    Y:(R0), " & acc & "              ; lut_wave\n")

    of "sat24":
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], "A")
        ctx.asmOut.add("                MOVE    A, X0                            ; pass through 56->24 limiter\n")
        ctx.asmOut.add("                MOVE    X0, " & acc & "\n")

    else:
      if expr.args.len >= 1:
        ctx.genExprToAcc(expr.args[0], acc)

  of ekIf:
    let lElse = ctx.nextLabel("_L_if_else")
    let lEnd = ctx.nextLabel("_L_if_end")
    ctx.genCondJump(expr.ifCond, lElse, jumpIfTrue = false)
    ctx.genExprToAcc(expr.ifThen, acc)
    ctx.asmOut.add("                JMP     " & lEnd & "\n")
    ctx.asmOut.add(lElse & ":\n")
    if expr.ifElse != nil:
      ctx.genExprToAcc(expr.ifElse, acc)
    else:
      ctx.asmOut.add("                CLR     " & acc & "\n")
    ctx.asmOut.add(lEnd & ":\n")

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
      ctx.asmOut.add("                MOVE    A1, " & ctx.getMemOperand(spill) & "\n")
      ctx.genExprToAcc(cond.rhs, "A")
      ctx.asmOut.add("                MOVE    A1, X0\n")
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
    ctx.asmOut.add("                TST     A\n")
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
    ctx.asmOut.add("                MOVE    A1, " & mem & "\n")

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
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    #$" & delayDataHex & ", Y0\n")
        ctx.asmOut.add("                ADD     Y0, A\n")
        ctx.asmOut.add("                MOVE    A1, R0\n")
        ctx.asmOut.add("                MOVE    B1, Y:(R0)                        ; delay_write\n")
        # increment wptr
        ctx.asmOut.add("                MOVE    Y:$" & delayBaseHex & ", A\n")
        ctx.asmOut.add("                MOVE    #$000001, X0\n")
        ctx.asmOut.add("                ADD     X0, A\n")
        ctx.asmOut.add("                MOVE    #$00003F, X0\n")
        ctx.asmOut.add("                AND     X0, A\n")
        ctx.asmOut.add("                MOVE    A1, A                            ; normalize sign extension\n")
        ctx.asmOut.add("                MOVE    A1, Y:$" & delayBaseHex & "       ; update wptr\n")
    else:
      ctx.genExprToAcc(stmt.callExpr, "A")

proc compileOmniSnippet*(m: OmniModule, addrs: Dsp56300ModuleAddresses, labelPrefix: string, stage: OmniStage = stageAll): string =
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
    elif addrs.inletAddrs.len > 0:
      ctx.inletAddrMap[inName] = addrs.inletAddrs[^1] + uint32(i - addrs.inletAddrs.len + 1)

  # Map custom outlet addresses (out1..out8)
  for i, outName in m.outputs:
    if i < addrs.outletAddrs.len:
      ctx.outletAddrMap[outName] = addrs.outletAddrs[i]
    elif addrs.outletAddrs.len > 0:
      ctx.outletAddrMap[outName] = addrs.outletAddrs[^1] + uint32(i - addrs.outletAddrs.len + 1)

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

  if stage in {stageAll, stagePerform} and m.performStmts.len > 0:
    ctx.asmOut.add("        ; --- Begin perform (control-rate uniforms) ---\n")
    for s in m.performStmts:
      ctx.genStmt(s)
    ctx.asmOut.add("        ; --- End perform ---\n")

  if stage in {stageAll, stageSample} and m.sampleStmts.len > 0:
    ctx.asmOut.add("        ; --- Begin sample (audio-rate DSP) ---\n")
    for s in m.sampleStmts:
      ctx.genStmt(s)
    ctx.asmOut.add("        ; --- End sample ---\n")

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

