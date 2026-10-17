# MIT License
# Copyright (c) 2020-2026 Francesco Cameli & Omni Contributors

import os, osproc

let omniBin = getAppDir() / "omni"
var cmd = (if omniBin.fileExists(): omniBin else: "omni") & " --target:max"

for arg in commandLineParams():
  cmd.add(" " & quoteShell(arg))

let exitCode = execCmd(cmd)
quit(exitCode)
