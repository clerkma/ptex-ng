# Copyright (c) 2020 Clerk Ma
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.

#
# ruby test.py
# mruby -r tag.rb -r otf.rb test.rb
# 
if Kernel.respond_to? :require
  require "./otf"
  require './tag'
end

if ARGV.length == 1
  File.open(ARGV[0], "rb") do |f|
    b = ParseBinary.new(f.read)
  end
elsif ARGV.length == 2
  File.open(ARGV[0], "rb") do |f|
    b = ParseBinary.new(f.read, ARGV[1].to_i)
  end
else
  puts("Usage: lsotfea font [index]")
end

