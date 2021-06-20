# MIT License
# 
# Copyright (c) 2020-2021 Francesco Cameli
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import os, httpclient

when hostCPU == "amd64":
  let cpu = "64"
elif hostCPU == "i386":
  let cpu = "32"

let link = "https://github.com/vitreo12/windows-strip/releases/download/9.0.0/strip" & cpu & ".tar.gz"

#Check if link exists, perhaps os / cpu combo is wrong
var client = newHttpClient()
let response = client.request(link, httpMethod=HttpHead).code
if response.is4xx or response.is5xx:
  echo "Error: no connection or invalid link: " & link & "\n"
  quit 0

#Link exists and connection works, download it
quit execShellCmd("curl " & link & " -o ./strip.tar.gz")
