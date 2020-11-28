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

  def u16(base_offset)
    unpack(base_offset, 2, "S>")[0]
  end

  def parse_coverage(base_offset)
    format, count = unpack(base_offset, 4, "S>2")
    if format == 1
      {format: format, data: unpack(base_offset + 4, count * 2, "S>*")}
    elsif format == 2
      {format: format, data: unpack(base_offset + 4, count * 6, "S>*")}
    end
  end

  def unpack_coverage(base_offset, coverage_offset)
    offset = u16(base_offset + coverage_offset)
    if offset != 0
      parse_coverage(base_offset + offset)
    end
  end

  def parse_class(base_offset)
    format = u16(base_offset)
    if format == 1
      start_glyph_id, count = unpack(base_offset + 2, 4, "S>2")
      class_value = unpack(base_offset + 6, count * 2, "S>*")
      {format: format, start_glyph_id: start_glyph_id, class_value: class_value}
    elsif format == 2
      count = u16(base_offset + 2)
      data = unpack(base_offset + 4, count * 6, "S>*")
      {format: format, data: data}
    end
  end

  def unpack_class(base_offset, coverage_offset)
    offset = u16(base_offset + coverage_offset)
    if offset != 0
      parse_class(base_offset + offset)
    end
  end

  def unpack_offset_list(base_offset)
    count = u16(base_offset)
    unpack(base_offset + 2, count * 2, "S>*")
  end

  def parse_lang_sys(base_offset)
    lookup_order, required_feature_index, feature_index_count = unpack(base_offset, 6, "S>*")
    feature_index_list = unpack(base_offset + 6, 2 * feature_index_count, "S>*")
    {lookup_order: lookup_order, required: required_feature_index, feature_index_list: feature_index_list}
  end

  def parse_script_list(base_offset)
    u16(base_offset).times.map do |script_index|
      script_one_offset = base_offset + 2 + 6 * script_index
      script_tag, script_offset = unpack(script_one_offset, 6, "a4S>")
      script_offset += base_offset
      default_lang_sys_offset, lang_sys_count = unpack(script_offset, 4, "S>*")
      if default_lang_sys_offset != 0
        offset = script_offset + default_lang_sys_offset
        default_lang_sys = parse_lang_sys(offset)
      else
        default_lang_sys = nil
      end
      script = lang_sys_count.times.map do |lang_sys_index|
        offset = script_offset + 4 + 6 * lang_sys_index
        tag, offset = unpack(offset, 6, "a4S>")
        offset += script_offset
        {tag: tag, lang_sys: parse_lang_sys(offset)}
      end
      {tag: script_tag, default: default_lang_sys, script: script}
    end
  end

  def parse_feature_list(base_offset)
    u16(base_offset).times.map do |feature_index|
      tag, offset = unpack(base_offset + 2 + 6 * feature_index, 6, "a4S>")
      offset += base_offset
      params, lookup_index_count = unpack(offset, 4, "S>2")
      lookup_index_list = unpack(offset + 4, 2 * lookup_index_count, "S>*")
      {tag: tag, params: params, lookup_index_list: lookup_index_list}
    end
  end

  def parse_lookup_context0(base_offset, offset)
    unpack_offset_list(base_offset + offset).map do |i|
      if i != 0
        i += base_offset
        unpack_offset_list(i).map do |j|
          if j != 0
            j += i
            glyph_count, seq_lookup_count = unpack(j, 4, "S>S>")
            input = unpack(j + 4, (glyph_count - 1) * 2, "S>*")
            seq_lookup_list = unpack(j + 4 + (glyph_count - 1) * 2, seq_lookup_count * 4,"S>*")
            {input: input, seq_lookup_list: seq_lookup_list}
          end
        end
      end
    end
  end

  def parse_lookup_context1(base_offset)
    coverage = unpack_coverage(base_offset, 2)
    data = parse_lookup_context0(base_offset, 4)
    {coverage: coverage, data: data}
  end

  def parse_lookup_context2(base_offset)
    coverage = unpack_coverage(base_offset, 2)
    class_def = unpack_class(base_offset, 4)
    data = parse_lookup_context0(base_offset, 6)
    {coverage: coverage, class_def: class_def, data: data}
  end

  def parse_lookup_context3(base_offset)
    glyph_count, seq_lookup_count = unpack(base_offset + 2, 4, "S>S>")
    coverage_list = glyph_count.times.map do |i|
       unpack_coverage(base_offset, 6 + 2 * i)
    end
    seq_lookup_list = unpack(base_offset + 6 + 2 * glyph_count, seq_lookup_count * 4, "S>")
    {coverage: coverage_list, seq_lookup_list: seq_lookup_list}
  end

  def parse_lookup_chain0(base_offset, offset)
    unpack_offset_list(base_offset + offset).map do |i|
      if i != 0
        i += base_offset
        unpack_offset_list(i).map do |j|
          if j != 0
            j += i
            backtrack_count = u16(j)
            j += 2
            backtrack = unpack(j, backtrack_count * 2, "S>*")
            j += 2 * backtrack_count
            input_count = u16(j)
            j += 2
            input = unpack(j, (input_count - 1) * 2, "S>*")
            j += (input_count - 1) * 2
            lookahead_count = u16(j)
            j += 2
            lookahead = unpack(j, lookahead_count * 2, "S>*")
            j += lookahead_count * 2
            seq_lookup_count = u16(j)
            j += 2
            seq_lookup_list = unpack(j, seq_lookup_count * 4, "S>*")
            {backtrack: backtrack, input: input, lookahead: lookahead, seq_lookup_list: seq_lookup_list}
          end
        end
      end
    end
  end

  def parse_lookup_chain1(base_offset)
    coverage = unpack_coverage(base_offset, 2)
    data = parse_lookup_chain0(base_offset, 4)
    {coverage: coverage, data: data}
  end

  def parse_lookup_chain2(base_offset)
    coverage = unpack_coverage(base_offset, 2)
    backtrack_class_def = unpack_class(base_offset, 4)
    input_class_def = unpack_class(base_offset, 6)
    lookahead_class_def = unpack_class(base_offset, 8)
    data = parse_lookup_chain0(base_offset, 10)
    {coverage: coverage, backtrack_class_def: backtrack_class_def,
     input_class_def: input_class_def, lookahead_class_def: lookahead_class_def, data: data}
  end

  def parse_lookup_chain3(base_offset)
    offset = base_offset + 2
    backtrack_list = unpack_offset_list(offset).map do |i|
       unpack_coverage(base_offset, i)
    end
    offset += 2 * backtrack_list.size + 2
    input_list = unpack_offset_list(offset).map do |i|
       unpack_coverage(base_offset, i)
    end
    offset += 2 * input_list.size + 2
    lookahead_list = unpack_offset_list(offset).map do |i|
       unpack_coverage(base_offset, i)
    end
    offset += 2 * lookahead_list.size + 2
    seq_lookup_count = u16(offset)
    seq_lookup_list = unpack(offset + 2, 2 * seq_lookup_count, "S>*")
    {backtrack: backtrack_list, input: input_list, lookahead_list: lookahead_list, seq_lookup_list: seq_lookup_list}
  end

  def parse_lookup_sub(base_offset, type)
    if type == 7
      format, type, offset = unpack(base_offset, 8, "S>S>L>")
      base_offset += offset
    end
    format = u16(base_offset)
    sig = type * 10 + format
    if sig == 11
      coverage = unpack_coverage(base_offset, 2)
      delta_glyph_id = u16(base_offset + 4)
      {sig: sig, coverage: coverage, delta_glyph_id: delta_glyph_id}
    elsif sig == 12
      coverage = unpack_coverage(base_offset, 2)
      count = u16(base_offset + 4)
      substitute_glyph_list = unpack(base_offset + 6, count * 2, "S>*")
      {sig: 12, coverage: coverage, substitute_glyph_list: substitute_glyph_list}
    elsif sig == 21 or sig == 31
      coverage = unpack_coverage(base_offset, 2)
      sequence_list = unpack_offset_list(base_offset + 4).map do |sequence_offset|
        count = u16(base_offset + sequence_offset)
        unpack(base_offset + sequence_offset + 2, count * 2, "S>*")
      end
      {sig: sig, coverage: coverage, sequence_list: sequence_list}
    elsif sig == 41
      coverage = unpack_coverage(base_offset, 2)
      ligature_set = unpack_offset_list(base_offset + 4).map do |i|
        if i != 0
          i += base_offset
          unpack_offset_list(i).map do |j|
            j += i
            ligature_glyph, component_count = unpack(j, 4, "S>S>")
            component = unpack(j + 4, component_count * 2, "S>*")
            {ligature_glyph: ligature_glyph, component: component}
          end
        end
      end
      {sig: 41, coverage: coverage, ligature_set: ligature_set}
    elsif sig == 51
      {sig: 51}.merge(parse_lookup_context1(base_offset))
    elsif sig == 52
      {sig: 52}.merge(parse_lookup_context2(base_offset))
    elsif sig == 53
      {sig: 53}.merge(parse_lookup_context3(base_offset))
    elsif sig == 61
      {sig: 61}.merge(parse_lookup_chain1(base_offset))
    elsif sig == 62
      {sig: 62}.merge(parse_lookup_chain2(base_offset))
    elsif sig == 63
      {sig: 63}.merge(parse_lookup_chain3(base_offset))
    elsif sig == 81
      coverage = unpack_coverage(base_offset, 2)
      offset = base_offset + 4
      backtrack_count = u16(offset)
      offset += 2
      backtrack_list = []
      unpack(offset, backtrack_count * 2, "S>*").map do |i|
        backtrack_list << unpack_coverage(base_offset, i)
      end
      offset += 2 * backtrack_list.size
      lookahead_count = u16(offset)
      offset += 2
      lookahead_list = []
      unpack(offset, lookahead_count * 2, "S>*").map do |i|
        lookahead_list << unpack_coverage(base_offset, i)
      end
      offset += lookahead_count * 2
      glyph_count = u16(offset)
      offset += 2
      substitute_glyph_list = unpack(offset, glyph_count * 2, "S>*")
      {sig: 81, coverage: coverage, backtrack: backtrack_list, lookahead: lookahead_list,
       substitute_glyph_list: substitute_glyph_list}
    end
  end

  def parse_lookup_pos(base_offset, type)
    if type == 9
      format, type, offset = unpack(base_offset, 8, "S>S>L>")
      base_offset += offset
    end
    format = u16(base_offset)
    sig = type * 10 + format
  end

  def parse_lookup(base_offset)
    type, flag = unpack(base_offset, 4, "S>2")
    table = unpack_offset_list(base_offset + 4).map do |i|
      if @tag == "GSUB"
        parse_lookup_sub(base_offset + i, type)
      elsif @tag == "GPOS"
        parse_lookup_pos(base_offset + i, type)
      end
    end
    {type: type, flag: flag, table: table}
  end

  def parse_lookup_list(base_offset)
    unpack_offset_list(base_offset).map do |lookup_offset|
      parse_lookup(base_offset + lookup_offset)
    end
  end

  def initialize(data, tag)
    @data = data
    @tag = tag
    @length = data.length
    @script_list = nil
    version = data[0, 4].unpack("S>2")
    if version == [1, 0]
      script_list_offset, feature_list_offset, lookup_list_offset = data[4, 6].unpack("S>*")
      @script_list = parse_script_list(script_list_offset)
      @feature_list = parse_feature_list(feature_list_offset)
      @lookup_list = parse_lookup_list(lookup_list_offset)
    end
  rescue
    nil
  end

  def get_tag(tag, category)
    meaning = nil
    if category == :feature
      meaning = $ot_tag_feature[tag]
    elsif category == :language
      meaning = $ot_tag_language[tag]
    elsif  category == :script
      meaning = $ot_tag_script[tag]
    end

    if meaning == nil
      "'#{tag}'"
    else
      "'#{tag}' (#{meaning})"
    end
  end

  def list_feature(feature_index_list)
    lines = (feature_index_list.length + 3).div(4)
    lines.times do |line|
      one_line = feature_index_list[4 * line, 4].map {|i| @feature_list[i][:tag]}
      puts("    " + one_line.join(" "))
    end
  end

  def list_info(banner)
    puts(banner)
    @script_list.each do |script|
      puts("script #{get_tag(script[:tag], :script)}:")
      puts("  default features:")
      if script[:default] == nil
        puts("    (none)")
      else
        list_feature(script[:default][:feature_index_list])
      end
      script[:script].each do |lang_sys|
        puts("  language #{get_tag(lang_sys[:tag], :language)}:")
        list_feature(lang_sys[:lang_sys][:feature_index_list])
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
        @gsub = GTabParser.new(src[offset, length], tag)
        if @gsub != nil
          @gsub.list_info("Table 'GSUB'")
        end
      elsif tag == "GPOS"
        @gpos = GTabParser.new(src[offset, length], tag)
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
