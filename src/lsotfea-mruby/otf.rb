# Copyright (c) 2020, 2021, 2022, 2023 Clerk Ma
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

$value_order = [
  [:x_placement, "s>"],
  [:y_placement, "s>"],
  [:x_advance, "s>"],
  [:y_advance, "s>"],
  [:x_placement_device, "S>"],
  [:y_placement_device, "S>"],
  [:x_advance_device, "S>"],
  [:y_advance_device, "S>"],
]

module OpenTypeDataParser
  def u16(base)
    unpack(base, 2, "S>")[0]
  end

  def u24(base)
    h, b = unpack(base, 3, "S>C")
    h * 256 + b
  end

  def u32(base)
    unpack(base, 4, "L>")[0]
  end

  def i16(base)
    unpack(base, 2, "s>")[0]
  end

  def parse_coverage(base)
    format, count = u16_list(base, 2)
    if format == 1
      {format: format, data: u16_list(base + 4, count)}
    elsif format == 2
      data = count.times.map do |i|
        u16_list(base + 4 + i * 6, 3)
      end
      {format: format, data: data}
    end
  end

  def unpack_coverage(base, delta)
    offset = u16(base + delta)
    if offset != 0
      parse_coverage(base + offset)
    end
  end

  def parse_class(base)
    format = u16(base)
    if format == 1
      start_glyph_id = u16(base + 2)
      class_value = unpack_u16_list(base + 4)
      {format: format, start_glyph_id: start_glyph_id, class_value: class_value}
    elsif format == 2
      data = u16(base + 2).times.map do |i|
        u16_list(base + 4 + i * 6, 3)
      end
      {format: format, data: data}
    end
  end

  def unpack_class(base, delta)
    offset = u16(base + delta)
    if offset != 0
      parse_class(base + offset)
    end
  end

  def u16_list(base, count)
    unpack(base, count * 2, "S>*")
  end

  def unpack_u16_list(base)
    count = u16(base)
    unpack(base + 2, count * 2, "S>*")
  end

  def value_size(value_format)
    2 * (value_format & 0xFF).to_s(2).bytes.sum { |x| x - 48 }
  end

  def parse_value(base, value_format)
    value = {}
    offset = base
    $value_order.each_with_index do |part, i|
      if value_format & (1 << i) > 0 and i < 4
        value[part[0]] = unpack(offset, 2, part[1])[0]
        offset += 2
      end
    end
    value
  end

  def parse_anchor(base, delta)
    if delta != 0
      format, x, y = unpack(base + delta, 6, "S>s>s>")
      {format: format, x: x, y: y}
    end
  end

  def parse_mark_array(base, delta)
    offset = base + u16(base + delta)
    u16(offset).times.map do |i|
      mark_class, anchor_offset = u16_list(offset + 2 + 4 * i, 2)
      anchor = parse_anchor(offset, anchor_offset)
      {mark_class: mark_class, anchor: anchor}
    end
  end

  def unpack(start, length, format)
    if start + length > @length
      puts("Exceed Buffer: #{@length} < #{start} + #{length}.")
      raise "Exceed Buffer."
    else
      @data[start, length].unpack(format)
    end
  end
end

class CMapParser
  include OpenTypeDataParser

  def f1(offset)
    length, language = unpack(offset + 2, 4, "S>S>")
    map = unpack(offset + 6, length - 6, "C*")
    {format: 1, language: language, map: map}
  end

  def f2(offset)
    length, language = unpack(offset + 2, 4, "S>S>")
    keys = unpack(offset + 6, 512, "S>*")
    key_max = keys.max + 8
    pos_d = 518 + key_max
    header = keys.each.map do |i|
      pos_h = 518 + i
      first, count, id_delta, id_offset = unpack(offset + pos_h, 8, "S>S>s>S>")
      pos_g = 518 + i + 6 + id_offset
      if id_offset != 0
        {first: first, count: count, id_delta: id_delta, id_offset: (pos_g - pos_d) / 2}
      else
        nil
      end
    end
    glyph = unpack(pos_d, length - pos_d, "S>*")
    {format: 2, language: language, header: header, glyph: glyph}
  end

  def f4(offset)
    length, language = unpack(offset + 2, 4, "S>S>")
    seg = u16(offset + 6)
    one_offset = 14
    end_code = unpack(offset + one_offset, seg, "S>*")
    one_offset += 2 + seg
    start_code = unpack(offset + one_offset, seg, "S>*")
    one_offset += seg
    id_delta = unpack(offset + one_offset, seg, "s>*")
    one_offset += seg
    id_offset = unpack(offset + one_offset, seg, "S>*")
    one_offset += seg
    glyph = unpack(offset + one_offset, length - one_offset, "S>*")
    id_offset = id_offset.each_with_index.map {|v, i| (v - seg) / 2 + i if v != 0 }
    {format: 4, language: language,
     start_code: start_code, end_code: end_code,
     id_delta: id_delta, id_offset: id_offset, glyph: glyph}
  end

  def f6(offset)
    length, language, first, count = unpack(offset + 2, 8, "S>*")
    glyph = unpack(offset + 10, count * 2, "S>*")
    {format: 6, language: language, first: first, glyph: glyph}
  end

  def f8(offset)
    length, language = unpack(offset + 4, 8, "L>L>")
    is32 = unpack(offset + 12, 8192, "C*")
    count = u32(offset + 8204)
    one_offset = offset + 8208
    glyph = count.times.map do |i|
      start_code, end_code, glyph = unpack(one_offset, 12, "L>*")
      one_offset += 12
      {start_code: start_code, end_code: end_code, glyph: glyph}
    end
    {format: 8, language: language, is32: is32, glyph: glyph}
  end

  def f10(offset)
    length, language, start_code, count = unpack(offset + 4, 16, "L>*")
    glyph = unpack(offset + 20, count * 2, "S>*")
    {format: 10, language: language,
     start_code: start_code, glyph: glyph}
  end

  def f12_13(offset)
    length, language, count = unpack(offset + 4, 12, "L>*")
    one_offset = offset +  16
    glyph = count.times.map do |i|
      start_code, end_code, glyph = unpack(one_offset, 12, "L>*")
      one_offset += 12
      {start_code: start_code, end_code: end_code, glyph: glyph}
    end
    {format: u16(offset), language: language, glyph: glyph}
  end

  def f14(offset)
    length, count = unpack(offset + 2, 8, "L>*")
    one_offset = offset + 10
    variation = count.times.map do |i|
      selector = u24(one_offset + i * 11)
      default_offset, non_default_offset = unpack(one_offset + i * 11 + 3, 8, "L>*")
      default = if default_offset != 0
        default_offset += offset
        count = u32(default_offset)
        count.times.map do |i|
          start_add = u32(default_offset + 4 + 4 * i)
          {start: start_add >> 8, add: start_add & 0xFF}
        end
      end
      non_default = if non_default_offset != 0
        non_default_offset += offset
        count = u32(non_default_offset)
        count.times.map do |i|
          code = u24(non_default_offset + 4 + 5 * i)
          glyph = u16(non_default_offset + 4 + 5 * i + 3)
          {code: code, glyph: glyph}
        end
      end
      {selector: selector, default: default, non_default: non_default}
    end
    {format: 14, variation: variation}
  end

  def parse_table(offset)
    format = u16(offset)
    case format
    when 1
      f1(offset)
    when 2
      f2(offset)
    when 4
      f4(offset)
    when 6
      f6(offset)
    when 8
      f8(offset)
    when 10
      f10(offset)
    when 12, 13
      f12_13(offset)
    when 14
      f14(offset)
    end
  end

  def collect_unicode_cmap
    maps = @encoding.select{|e| e[:platform] == 0 or (e[:platform] == 3 and [1, 10].include? e[:encoding])}
    maps.map {|x| x[:offset]}.uniq.sort.map {|x| @ctable[x]}
  end

  def to_glyph(codes)
    glyph = []
  end

  def initialize(data, tag)
    @data = data
    @tag = tag
    @length = data.length
    version = u16(0)
    if version == 0
      @encoding = u16(2).times.map do |i|
        platform, encoding, offset = unpack(4 + 8 * i, 8, "S>S>L>")
        {platform: platform, encoding: encoding, offset: offset}
      end
      @offset = @encoding.map {|e| e[:offset]}.uniq.sort
      @ctable = @offset.to_h do |i|
        [i, parse_table(i)]
      end
      @unicode = collect_unicode_cmap
    end
  rescue => error
    puts error
    nil
  end

  def inspect
    "OpenTypeParser(`cmap`)"
  end
end

class GTabParser
  include OpenTypeDataParser

  def parse_mark_glyph_set(base, delta)
    offset = u16(base + delta) + base
    if offset != base
      format = u16(offset)
      if format == 1
        u16(offset + 2).times.map do |i|
          parse_coverage(u32(offset + 4 + i * 4) + offset)
        end
      end
    end
  end

  def parse_lang_sys(base)
    lookup_order, required_feature_index = u16_list(base, 2)
    feature_index_list = unpack_u16_list(base + 4)
    {lookup_order: lookup_order, required: required_feature_index, feature_index_list: feature_index_list}
  end

  def parse_script_list(base, delta)
    offset = u16(base + delta) + base
    u16(offset).times.map do |i|
      tag, script_offset = unpack(offset + 2 + 6 * i, 6, "a4S>")
      script_offset += offset
      default_offset, count = u16_list(script_offset, 2)
      default = if default_offset != 0
        parse_lang_sys(script_offset + default_offset)
      end
      script = count.times.map do |j|
        tag, lang_sys_offset = unpack(script_offset + 4 + 6 * j, 6, "a4S>")
        {tag: tag, lang_sys: parse_lang_sys(script_offset + lang_sys_offset)}
      end
      {tag: tag, default: default, script: script}
    end
  end

  def parse_feature_list(base, delta)
    offset = u16(base + delta) + base
    u16(offset).times.map do |i|
      tag, feature_offset = unpack(offset + 2 + 6 * i, 6, "a4S>")
      feature_offset += offset
      params = u16(feature_offset)
      lookup_index_list = unpack_u16_list(feature_offset + 2)
      {tag: tag, params: params, lookup_index_list: lookup_index_list}
    end
  end

  def parse_lookup_context0(base, delta)
    unpack_u16_list(base + delta).map do |i|
      if i != 0
        i += base
        unpack_u16_list(i).map do |j|
          j += i
          glyph_count, seq_lookup_count = u16_list(j, 2)
          input = u16_list(j + 4, glyph_count - 1)
          seq_lookup_list = u16_list(j + 4 + (glyph_count - 1) * 2, seq_lookup_count * 2)
          {input: input, seq_lookup_list: seq_lookup_list}
        end
      end
    end
  end

  def parse_lookup_context1(base)
    coverage = unpack_coverage(base, 2)
    data = parse_lookup_context0(base, 4)
    {coverage: coverage, data: data}
  end

  def parse_lookup_context2(base)
    coverage = unpack_coverage(base, 2)
    class_def = unpack_class(base, 4)
    data = parse_lookup_context0(base, 6)
    {coverage: coverage, class_def: class_def, data: data}
  end

  def parse_lookup_context3(base)
    glyph_count, seq_lookup_count = u16_list(base + 2, 2)
    coverage_list = glyph_count.times.map do |i|
       unpack_coverage(base, 6 + 2 * i)
    end
    seq_lookup_list = unpack(base + 6 + 2 * glyph_count, seq_lookup_count * 4, "S>")
    {coverage: coverage_list, seq_lookup_list: seq_lookup_list}
  end

  def parse_lookup_chain0(base, offset)
    unpack_u16_list(base + offset).map do |i|
      if i != 0
        i += base
        unpack_u16_list(i).map do |j|
          j += i
          backtrack = unpack_u16_list(j)
          j += 2 * backtrack.size + 2
          input_count = u16(j)
          j += 2
          input = u16_list(j, input_count - 1)
          j += (input_count - 1) * 2
          lookahead = unpack_u16_list(j)
          j += lookahead.size * 2 + 2
          seq_lookup_count = u16(j)
          seq_lookup_list = u16_list(j + 2, seq_lookup_count * 2)
          {backtrack: backtrack, input: input, lookahead: lookahead, seq_lookup_list: seq_lookup_list}
        end
      end
    end
  end

  def parse_lookup_chain1(base)
    coverage = unpack_coverage(base, 2)
    data = parse_lookup_chain0(base, 4)
    {coverage: coverage, data: data}
  end

  def parse_lookup_chain2(base)
    coverage = unpack_coverage(base, 2)
    backtrack_class_def = unpack_class(base, 4)
    input_class_def = unpack_class(base, 6)
    lookahead_class_def = unpack_class(base, 8)
    data = parse_lookup_chain0(base, 10)
    {coverage: coverage, backtrack_class_def: backtrack_class_def,
     input_class_def: input_class_def, lookahead_class_def: lookahead_class_def, data: data}
  end

  def parse_lookup_chain3(base)
    offset = base + 2
    backtrack_list = unpack_u16_list(offset).map do |i|
      parse_coverage(base + i)
    end
    offset += 2 * backtrack_list.size + 2
    input_list = unpack_u16_list(offset).map do |i|
      parse_coverage(base + i)
    end
    offset += 2 * input_list.size + 2
    lookahead_list = unpack_u16_list(offset).map do |i|
      parse_coverage(base + i)
    end
    offset += 2 * lookahead_list.size + 2
    seq_lookup_count = u16(offset)
    seq_lookup_list = u16_list(offset + 2, seq_lookup_count * 2)
    {backtrack: backtrack_list, input: input_list, lookahead: lookahead_list, seq_lookup_list: seq_lookup_list}
  end

  def parse_lookup_sub(base, type)
    if type == 7
      _, type, offset = unpack(base, 8, "S>S>L>")
      base += offset
    end
    format = u16(base)
    sig = type * 10 + format
    case sig
    when 11
      coverage = unpack_coverage(base, 2)
      delta_glyph_id = i16(base + 4)
      {sig: sig, coverage: coverage, delta_glyph_id: delta_glyph_id}
    when 12
      coverage = unpack_coverage(base, 2)
      substitute_glyph_list = unpack_u16_list(base + 4)
      {sig: 12, coverage: coverage, substitute_glyph_list: substitute_glyph_list}
    when 21, 31
      coverage = unpack_coverage(base, 2)
      sequence_list = unpack_u16_list(base + 4).map do |sequence_offset|
        unpack_u16_list(base + sequence_offset)
      end
      {sig: sig, coverage: coverage, sequence_list: sequence_list}
    when 41
      coverage = unpack_coverage(base, 2)
      ligature_set = unpack_u16_list(base + 4).map do |i|
        i += base
        unpack_u16_list(i).map do |j|
          j += i
          ligature_glyph, component_count = u16_list(j, 2)
          component = u16_list(j + 4, component_count - 1)
          {ligature_glyph: ligature_glyph, component: component}
        end
      end
      {sig: sig, coverage: coverage, ligature_set: ligature_set}
    when 51
      {sig: sig}.merge(parse_lookup_context1(base))
    when 52
      {sig: sig}.merge(parse_lookup_context2(base))
    when 53
      {sig: sig}.merge(parse_lookup_context3(base))
    when 61
      {sig: sig}.merge(parse_lookup_chain1(base))
    when 62
      {sig: sig}.merge(parse_lookup_chain2(base))
    when 63
      {sig: sig}.merge(parse_lookup_chain3(base))
    when 81
      coverage = unpack_coverage(base, 2)
      offset = base + 4
      backtrack_list = unpack_u16_list(offset).map do |i|
        parse_coverage(base + i)
      end
      offset += 2 * backtrack_list.size + 2
      lookahead_list = unpack_u16_list(offset).map do |i|
        parse_coverage(base + i)
      end
      offset += 2 * lookahead_list.size + 2
      substitute_glyph_list = unpack_u16_list(offset)
      {sig: sig, coverage: coverage, backtrack: backtrack_list, lookahead: lookahead_list,
       substitute_glyph_list: substitute_glyph_list}
    else
      puts "??? GSUB #{sig}"
    end
  end

  def parse_lookup_pos(base, type)
    if type == 9
      _, type, offset = unpack(base, 8, "S>S>L>")
      base += offset
    end
    format = u16(base)
    sig = type * 10 + format
    case sig
    when 11
      coverage = unpack_coverage(base, 2)
      value_format = u16(base + 4)
      value = parse_value(base + 6, value_format)
      {sig: sig, coverage: coverage, value: value}
    when 12
      coverage = unpack_coverage(base, 2)
      value_format, value_count = u16_list(base + 4, 2)
      size = value_size(value_format)
      value_list = value_count.times.map do |i|
        parse_value(base + 8 + size * i, value_format)
      end
      {sig: sig, coverage: coverage, value_list: value_list}
    when 21
      coverage = unpack_coverage(base, 2)
      value_format1, value_format2 = u16_list(base + 4, 2)
      size1 = value_size(value_format1)
      size2 = value_size(value_format2)
      pair_size = 2 + size1 + size2
      pair_set_list = unpack_u16_list(base + 8).map do |i|
        i += base
        pair_count = u16(i)
        i += 2
        pair_count.times do
          second_glyph = u16(i)
          value1 = parse_value(i + 2, value_format1)
          value2 = parse_value(i + 2 + size1, value_format2)
          i += pair_size
          {second: second_glyph, value1: value1, value2: value2}
        end
      end
      {sig: sig, coverage: coverage, pair_set_list: pair_set_list}
    when 22
      coverage = unpack_coverage(base, 2)
      value_format1, value_format2 = u16_list(base + 4, 2)
      size1 = value_size(value_format1)
      size2 = value_size(value_format2)
      class_def1 = unpack_class(base, 8)
      class_def2 = unpack_class(base, 10)
      class1_count, class2_count = u16_list(base + 12, 2)
      offset = base + 16
      class_set_list = class1_count.times.map do
        class2_count.times.map do
          value1 = parse_value(offset, value_format1)
          offset += size1
          value2 = parse_value(offset, value_format2)
          offset += size2
          {value1: value1, value2: value2}
        end
      end
      {sig: sig, coverage: coverage,
       class_def1: class_def1, class_def2: class_def2,
       class_set_list: class_set_list}
    when 31
      coverage = unpack_coverage(base, 2)
      count = u16(base + 4)
      offset = base + 6
      entry_exit_list = count.times.map do
        entry_offset, exit_offset = u16_list(offset, 2)
        entry_anchor = parse_anchor(base, entry_offset)
        exit_anchor = parse_anchor(base, exit_offset)
        {entry: entry_anchor, exit: exit_anchor}
      end
      {sig: sig, coverage: coverage, entry_exit_list: entry_exit_list}
    when 41      
      mark_coverage = unpack_coverage(base, 2)
      base_coverage = unpack_coverage(base, 4)
      mark_class_count = u16(base + 6)
      mark_array = parse_mark_array(base, 8)
      base_array_offset = u16(base + 10) + base
      base_array = u16(base_array_offset).times.map do |i|
        offset = base_array_offset + 2 + 2 * mark_class_count * i
        u16_list(offset, mark_class_count).map do |j|
          parse_anchor(base_array_offset, j)
        end
      end

      {sig: sig, mark_coverage: mark_coverage,
       base_coverage: base_coverage,
       mark_array: mark_array, base_array: base_array}
    when 51
      mark_coverage = unpack_coverage(base, 2)
      ligature_coverage = unpack_coverage(base, 4)
      mark_class_count = u16(base + 6)
      mark_array = parse_mark_array(base, 8)
      ligature_array_offset = u16(base + 10) + base
      ligature_array = unpack_u16_list(ligature_array_offset).map do |i|
        ligature_offset = ligature_array_offset + i
        component_count = u16(ligature_offset)
        component_count.times.map do |j|
          offset = ligature_offset + 2 + 2 * mark_class_count * j
          u16_list(offset, mark_class_count).map do |k|
            parse_anchor(ligature_offset, k)
          end
        end
      end
      {sig: sig, mark_coverage: mark_coverage, ligature_coverage: ligature_coverage,
       mark_array: mark_array, ligature_array: ligature_array}
    when 61
      mark1_coverage = unpack_coverage(base, 2)
      mark2_coverage = unpack_coverage(base, 4)
      mark_class_count = u16(base + 6)
      mark_array = parse_mark_array(base, 8)
      mark2_array_offset = u16(base + 10) + base
      mark2_array = u16(mark2_array_offset).times.map do |i|
        offset = mark2_array_offset + 2 + i * 2 * mark_class_count
        u16_list(offset, mark_class_count).map do |j|
          parse_anchor(mark2_array_offset, j)
        end
      end
      {sig: sig, mark1_coverage: mark1_coverage,
       mark2_coverage: mark2_coverage,
       mark_array: mark_array, mark2_array: mark2_array}
    when 71
      {sig: sig}.merge(parse_lookup_context1(base))
    when 72
      {sig: sig}.merge(parse_lookup_context2(base))
    when 73
      {sig: sig}.merge(parse_lookup_context3(base))
    when 81
      {sig: sig}.merge(parse_lookup_chain1(base))
    when 82
      {sig: sig}.merge(parse_lookup_chain2(base))
    when 83
      {sig: sig}.merge(parse_lookup_chain3(base))
    else
      puts "??? GPOS #{sig}"
    end
  end

  def parse_lookup(base)
    type, flag = unpack(base, 4, "S>2")
    table = unpack_u16_list(base + 4).map do |i|
      if @tag == "GSUB"
        parse_lookup_sub(base + i, type)
      elsif @tag == "GPOS"
        parse_lookup_pos(base + i, type)
      end
    end
    if @tag == "GSUB" and type == 7
      type = table[0][:sig] / 10
    elsif @tag == "GPOS" and type == 9
      type = table[0][:sig] / 10
    end
    {type: type, flag: flag, table: table}
  end

  def parse_lookup_list(base, delta)
    offset = u16(base + delta) + base
    unpack_u16_list(offset).map do |lookup_offset|
      parse_lookup(offset + lookup_offset)
    end
  end

  def inspect
    "OpenTypeParser(`#{@tag}`)"
  end

  def initialize(data, tag)
    @data = data
    @tag = tag
    @length = data.length
    case tag
    when "GSUB", "GPOS"
      version = u16_list(0, 2)
      if [[1, 0], [1, 1]].include?(version)
        @script_list = parse_script_list(0, 4)
        @feature_list = parse_feature_list(0, 6)
        @lookup_list = parse_lookup_list(0, 8)
      end
    when "GDEF"
      version = u16_list(0, 2)
      if [[1, 0], [1, 2], [1, 3]].include?(version)
        @glyph_class = unpack_class(0, 4)
        @mark_attach_class = unpack_class(0, 10)
        if [2, 3].include?(version[1])
          @mark_glyph_set = parse_mark_glyph_set(0, 12)
        end
      end
    end
  rescue => error
    puts error
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

  def reduce_to_range(src)
    if src.count > 1
      range_index = [0]
      (1..src.count - 1).each do |i|
        if src[i] - src[i - 1] > 1
          range_index << i
        end
      end
      range_index << src.count
      range = []
      (0..range_index.count - 2).each do |i|
        l_pos = range_index[i]
        r_pos = range_index[i + 1] - 1
        if r_pos - l_pos > 1
            range << "#{src[l_pos]}-#{src[r_pos]}"
        else
            range << "#{src[l_pos]}"
        end
      end
      range.join(", ")
    else
      src[0]
    end
  end

  def list_feature(feature_index_list)
    lines = (feature_index_list.length + 3).div(4)
    lines.times do |line|
      one_line = feature_index_list[4 * line, 4].map {|i| @feature_list[i][:tag]}
      puts("    " + one_line.join(" "))
    end
    lookup_map = {}
    puts("\n    Feature -> LookupIndex")
    feature_index_list.each do |feature_index|
      feature = @feature_list[feature_index]
      tag = feature[:tag]
      lookup_index_list = feature[:lookup_index_list]
      puts("      #{tag} -> #{reduce_to_range(lookup_index_list.sort)}")
      lookup_index_list.each do |lookup_index|
        if lookup_map.has_key?(lookup_index)
          lookup_map[lookup_index].push(tag)
        else
          lookup_map[lookup_index] = [tag]
        end
      end
    end
    puts("\n    LookupIndex -> Feature")
    idx_len = lookup_map.keys.max.to_s.length
    lookup_map.keys.sort.each do |m|
      lookup = @lookup_list[m]
      puts("      #{m.to_s.rjust(idx_len)} (#{lookup[:type]}) #{lookup_map[m].join(', ')}")
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

  def list_lookup_sub(lookup)
    puts "{"
    puts "  flag=#{lookup[:flag]};"
    lookup[:table].each do |one|
      sig = one[:sig]
      if sig == 11
        format = one[:coverage][:format]
        if format == 1
          one[:coverage][:data].each do |cover|
            puts "  sub #{cover} by #{(one[:delta_glyph_id] + cover) & 0xFFFF};"
          end
        elsif format == 2
          one[:coverage][:data].each do |cover|
            start = cover[0]
            _end = cover[1]
            r_start = (start + one[:delta_glyph_id]) & 0xFFFF
            r_end = (_end + one[:delta_glyph_id]) & 0xFFFF
            puts "  sub #{start}..#{_end} by (#{r_start}, #{r_end});"
          end
        end
      elsif sig == 12
        format = one[:coverage][:format]
        if format == 1
          one[:coverage][:data].each_with_index do |cover, cover_index|
            puts "  sub #{cover} by #{one[:substitute_glyph_list][cover_index]};"
          end
        elsif format == 2
          one[:coverage][:data].each do |cover|
            r = cover[0]..cover[1]
            r.each_with_index do |glyph, index|
              puts "  sub #{glyph} by #{one[:substitute_glyph_list][cover[2] + index]};"
            end
          end
        end
      elsif sig == 21 or sig == 31
        format = one[:coverage][:format]
        if sig == 21
          prep = "by"
        else
          prep = "from"
        end
        if format == 1
          one[:coverage][:data].each_with_index do |cover, cover_index|
            puts "  sub #{cover} #{prep} #{one[:sequence_list][cover_index]};"
          end
        elsif format == 2
          one[:coverage][:data].each do |cover|
            r = cover[0]..cover[1]
            r.each_with_index do |glyph, index|
              puts "  sub #{glyph} #{prep} #{one[:sequence_list][cover[2] + index]};"
            end
          end
        end
      elsif sig == 41
        format = one[:coverage][:format]
        if format == 1
          one[:coverage][:data].each_with_index do |cover, cover_index|
            one[:ligature_set][cover_index].each do |one_ligature|
              seq = [cover] + one_ligature[:component]
              lig = one_ligature[:ligature_glyph]
              puts "  sub #{seq} by #{lig};"
            end
          end
        elsif format == 2
          one[:coverage][:data].each do |cover|
            r = cover[0]..cover[1]
            r.each_with_index do |glyph, index|
              one[:ligature_set][index].each do |one_ligature|
                seq = [glyph] + one_ligature[:component]
                lig = one_ligature[:ligature_glyph]
                puts "  sub #{seq} by #{lig};"
              end
            end
          end
        end
      else
        puts lookup
      end
    end
    puts "}"
  end
end

class ParseBinary
  def block(src, offset, length)
    if src.class == String
      src[offset, length]
    elsif src.class == File
      src.seek offset
      src.read length
    end
  end

  def unpack(src, offset, length, format)
    if src.class == String
      src[offset, length].unpack(format)
    elsif src.class == File
      src.seek offset
      src.read(length).unpack(format)
    end
  end

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
    offset_table = unpack(src, one_offset, 12, "L>S>4")
    offset_table[1].times do |tableIndex|
      tag, check_sum, offset, length = unpack(src, one_offset + 12 + 16 * tableIndex, 16, "a4L>3")
      if tag == "GSUB"
        @gsub = GTabParser.new(block(src, offset, length), tag)
        if @gsub != nil
          @gsub.list_info("Table 'GSUB'")
        end
      elsif tag == "GPOS"
        @gpos = GTabParser.new(block(src, offset, length), tag)
        if @gpos != nil
          @gpos.list_info("Table 'GPOS'")
        end
      elsif tag == "GDEF"
        @gdef = GTabParser.new(block(src, offset, length), tag)
      elsif tag == "cmap"
        @cmap = CMapParser.new(block(src, offset, length), tag)
      end
    end
  end

  def initialize(data, index=0)
    magic = unpack(data, 0, 4, "L>")[0]
    if [0x00010000, 0x4F54544F, 0x74727565].include?(magic)
      parse_one data, 0
    elsif magic == 0x74746366 then
      header = unpack(data, 0, 12, "a4S>2L>")
      offset_list = unpack(data, 12, header[3] * 4, "L>*")
      if index < offset_list.count
        parse_one data, offset_list[index]
      end
    end
  rescue => error
    puts error
  end
end
