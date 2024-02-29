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

module OpenTypeDataParser
  def u16(base)
    unpack(base, 2, "S>")[0]
  end

  def u32(base)
    unpack(base, 4, "L>")[0]
  end

  def u16_list(base, count)
    unpack(base, count * 2, "S>*")
  end

  def unpack_u16_list(base)
    count = u16(base)
    unpack(base + 2, count * 2, "S>*")
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

class GTabParser
  include OpenTypeDataParser

  def parse_lang_sys(base)
    lookup_order, required_feature_index = u16_list(base, 2)
    feature_index_list = unpack_u16_list(base + 4)
    {lookup_order: lookup_order, required: required_feature_index, feature_index_list: feature_index_list}
  end

  def parse_script_list(base, delta)
    offset = u16(base + delta) + base
    u16(offset).times.map do |i|
      script_tag, script_offset = unpack(offset + 2 + 6 * i, 6, "a4S>")
      script_offset += offset
      default_offset, count = u16_list(script_offset, 2)
      default = parse_lang_sys(script_offset + default_offset) if default_offset != 0
      script = count.times.map do |j|
        tag, lang_sys_offset = unpack(script_offset + 4 + 6 * j, 6, "a4S>")
        {tag: tag, lang_sys: parse_lang_sys(script_offset + lang_sys_offset)}
      end
      {tag: script_tag, default: default, script: script}
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

  def parse_lookup(base)
    type, flag = unpack(base, 4, "S>2")
    if (@tag == "GSUB" and type == 7) or (@tag == "GPOS" and type == 9)
      type = u16(u16(base + 6) + base + 2)
    end
    {type: type, flag: flag}
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
    end
  rescue => error
    puts error
    nil
  end

  def get_tag(tag, category)
    meaning = nil
    if category == :feature
      meaning = $OT_TAG_FEATURE[tag]
    elsif category == :language
      meaning = $OT_TAG_LANGUAGE[tag]
    elsif  category == :script
      meaning = $OT_TAG_SCRIPT[tag]
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
    feature_index_list.each_slice(4) do |x|
      line = x.map {|i| @feature_list[i][:tag]}
      puts("    " + line.join(" "))
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
      flag = (lookup[:flag] & 0x10).to_s(2).rjust(5, "0")
      info = sprintf("(%s, %s, %s)", lookup[:type], flag, (lookup[:flag] & 0xFF00) >> 8)
      puts("      #{m.to_s.rjust(idx_len)} #{info} #{lookup_map[m].join(', ')}")
    end
  end

  def list_info(banner)
    puts(banner)
    @script_list.each do |script|
      puts("script #{get_tag(script[:tag], :script)}:")
      puts("  default  'dflt':")
      if script[:default] == nil
        puts("    (none)")
      else
        list_feature(script[:default][:feature_index_list])
      end
      script[:script].each do |lang_sys|
        required = lang_sys[:lang_sys][:required]
        required_feature = if required != 0xFFFF
          " *(`" + @feature_list[required][:tag] + "`)"
        end
        puts("  language #{get_tag(lang_sys[:tag], :language)}#{required_feature}:")
        list_feature(lang_sys[:lang_sys][:feature_index_list])
      end
    end
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
    padding_count = (4 - length & 3) & 3
    block(src, offset, length + padding_count).unpack("L>*").sum & 0xFFFFFFFF
  end

  def parse_one(src, one_offset)
    offset_table = unpack(src, one_offset, 12, "L>S>4")
    gpos_seg = nil
    gsub_seg = nil
    directory = offset_table[1].times.to_h do |tableIndex|
      tag, check_sum, offset, length = unpack(src, one_offset + 12 + 16 * tableIndex, 16, "a4L>3")
      gpos_seg = [offset, length] if tag == "GPOS"
      gsub_seg = [offset, length] if tag == "GSUB"
      [tag, [check_sum, offset, length]]
    end
    values = directory.values
    offset_max = [values.max{|a, b| a[1] <=> b[1]}[1].to_s.length, 6].max
    length_max = [values.max{|a, b| a[2] <=> b[2]}[2].to_s.length, 6].max
    puts "Font Directory:"
    o = "OFFSET".rjust(offset_max, " ")
    l = "LENGTH".rjust(length_max, " ")
    puts "   TAG CHECKSUM #{o} #{l}"
    line = [4, 8, offset_max, length_max].map {|x| "-" * x}.join("+")
    puts "  #{line}"
    directory.each do |k, v|
      s = v[0].to_s(16).rjust(8, "0")
      o = v[1].to_s.rjust(offset_max, " ")
      l = v[2].to_s.rjust(length_max, " ")
      puts "  #{k} #{s} #{o} #{l}"
    end

    if gpos_seg != nil
      gpos = GTabParser.new(block(src, *gpos_seg), "GPOS")
      gpos.list_info("Table 'GPOS'")
    end

    if gsub_seg != nil
      gsub = GTabParser.new(block(src, *gsub_seg), "GSUB")
      gsub.list_info("Table 'GSUB'")
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
