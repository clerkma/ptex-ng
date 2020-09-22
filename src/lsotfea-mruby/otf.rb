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

class GTabParser
  def unpack(start, length, format)
    if start + length > @length
      raise "Exceed Buffer."
    else
      @data[start, length].unpack(format)
    end
  end

  def parse_script_list(data, script_list_offset)
    script_list = []
    script_count = unpack(script_list_offset, 2, "S>")[0]
    script_count.times do |script_index|
      script = []
      script_one_offset = script_list_offset + 2 + 6 * script_index
      script_tag, script_offset = unpack(script_one_offset, 6, "a4S>")
      script_offset += script_list_offset
      default_lang_sys_offset, lang_sys_count = unpack(script_offset, 4, "S>*")
      if default_lang_sys_offset != 0
        offset = script_offset + default_lang_sys_offset
        lookup_order, required_feature_index, feature_index_count = unpack(offset, 6, "S>*")
        feature_index_list = unpack(offset + 6, 2 * feature_index_count, "S>*")
        default_lang_sys = [lookup_order, required_feature_index, feature_index_list]
      else
        default_lang_sys = nil
      end
      lang_sys_count.times do |lang_sys_index|
        offset = script_offset + 4 + 6 * lang_sys_index
        lang_sys_tag, lang_sys_offset = unpack(offset, 6, "a4S>")
        lang_sys_offset +=  script_offset
        lookup_order, required_feature_index, feature_index_count = unpack(lang_sys_offset, 6, "S>*")
        feature_index_list = unpack(lang_sys_offset + 6, 2 * feature_index_count, "S>*")
        script << [lang_sys_tag, lookup_order, required_feature_index, feature_index_list]
      end
      script_list << [script_tag, default_lang_sys, script]
    end
    script_list
  end

  def parse_feature_list(data, feature_list_offset)
    feature_list = []
    feature_count = unpack(feature_list_offset, 2, "S>")[0]
    feature_count.times do |feature_index|
      feature_tag, feature_offset = unpack(feature_list_offset + 2 + 6 * feature_index, 6, "a4S>")
      feature_offset += feature_list_offset
      feature_params, lookup_index_count = data[feature_offset, 4].unpack("S>2")
      lookup_index_list = unpack(feature_offset + 4, 2 * lookup_index_count, "S>*")
      feature_list << [feature_tag, feature_params, lookup_index_list]
    end
    feature_list
  end

  def initialize(data)
    @data = data
    @length = data.length
    @script_list = nil
    version = data[0, 4].unpack("S>2")
    if version == [1, 0]
      script_list_offset, feature_list_offset, lookup_list_offset = data[4, 6].unpack("S>*")
      @script_list = parse_script_list(data, script_list_offset)
      @feature_list = parse_feature_list(data, feature_list_offset)
    end
  rescue
    nil
  end

  def get_tag(tag)
    meaning = $ot_tag_hash[tag]
    if meaning == nil
      "'#{tag}'"
    else
      "'#{tag}' (#{meaning})"
    end
  end

  def list_feature(feature_index_list)
    lines = (feature_index_list.length + 3).div(4)
    lines.times do |line|
      one_line = feature_index_list[4 * line, 4].map {|i| @feature_list[i][0]}
      puts("    " + one_line.join(" "))
    end
  end

  def list_info(banner)
    puts(banner)
    @script_list.each do |script|
      puts("script #{get_tag(script[0])}:")
      puts("  default features:")
      if script[1] == nil
        puts("    (none)")
      else
        list_feature(script[1][2])
      end
      script[2].each do |lang_sys|
        puts("  language #{get_tag(lang_sys[0])}:")
        list_feature(lang_sys[3])
      end
    end
  end
end

class ParseBinary
  def calc_check_sum(src, offset, length)
    sum = 0
    padding_count = (4 - length & 3) & 3
    value_count = (length + padding_count) / 4
    src[offset, length + padding_count].unpack("L>*").each do |val|
      sum = (sum + val) & 0xFFFFFFFF
    end
    sum
  end

  def parse_one(src, one_offset)
    offset_table = src[one_offset, 12].unpack("L>S>4")
    offset_table[1].times do |tableIndex|
      tag, check_sum, offset, length = src[one_offset + 12 + 16 * tableIndex, 16].unpack("a4L>3")
      if tag == "GSUB"
        @gsub = GTabParser.new(src[offset, length])
        if @gsub != nil
          @gsub.list_info("Table 'GSUB'")
        end
      elsif tag == "GPOS"
        @gpos = GTabParser.new(src[offset, length])
        if @gpos != nil
          @gpos.list_info("Table 'GPOS'")
        end
      end
    end
  end

  def initialize(data, index=0)
    @gsub = nil
    @gpos = nil
    magic = data[0, 4].unpack("L>")[0]
    if [0x00010000, 0x4F54544F, 0x74727565].include?(magic)
      parse_one data, 0
    elsif magic == 0x74746366 then
      header = data[0, 12].unpack("a4S>2L>")
      offset_list = data[12, header[3] * 4].unpack("L>#{header[3]}")
      if index < offset_list.count
        parse_one data, offset_list[index]
      end
    end
  end
end
