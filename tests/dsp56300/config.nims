import os

let envDir = if existsEnv("DSP56300_NIM_DIR"): getEnv("DSP56300_NIM_DIR") else: ""
if envDir != "" and dirExists(envDir):
  switch("path", envDir)
elif dirExists(thisDir() / "../../../dsp56300-nim/src"):
  switch("path", thisDir() / "../../../dsp56300-nim/src")
elif dirExists(thisDir() / "../../dsp56300-nim/src"):
  switch("path", thisDir() / "../../dsp56300-nim/src")
