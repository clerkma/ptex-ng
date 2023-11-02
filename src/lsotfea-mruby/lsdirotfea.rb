# Copyright (c) 2023 Clerk Ma
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
# ruby lsdirotfea.py
# mruby -r tag.rb -r otf.rb lsdirotfea.rb
# 
if Kernel.respond_to? :require_relative
  require_relative "otf"
  require_relative 'tag'
end

def read_l(file, offset)
  file.seek offset
  file.read(4).unpack("L>")[0]
end

def parse(path)
  File.open(path, "rb") do |f|
    magic = read_l(f, 0)
    count = if [0x00010000, 0x4F54544F, 0x74727565].include?(magic)
      1
    elsif magic == 0x74746366
      read_l(f, 8)
    else
      0
    end
    if count > 1
      count.times do |font_index|
        puts "Task `#{path}`@#{font_index}"
        b = ParseBinary.new f, font_index
      end
    elsif count == 1
      puts "Task `#{path}`"
      b = ParseBinary.new f
    end
  end
end

def build_file_list(path, depth=0)
  if depth == 0
    path = path.gsub "\\", "/"
  end
  file_list = []
  Dir.open(path).each do |i|
    if not ['.', '..'].include?(i)
      cur = File.join path, i
      if File.directory? cur
        file_list += build_file_list(cur, depth+1)
      else
        file_list << cur
      end
    end
  end
  file_list
end

def parse_directory(path)
  if File.directory? path
    build_file_list(path).each do |one|
      parse(one)
    end
  else
    parse(path)
  end
end

if ARGV.length == 1
  parse_directory(ARGV[0])
else
  puts("Usage: lsdirotfea path")
end
