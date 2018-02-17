module Puppet::Pops
module PN
  KEY_PATTERN = /^[A-Za-z_-][0-9A-Za-z_-]*$/

  def pnError(message)
    raise ArgumentError, message
  end

  def as_call(name)
    Call.new(name, self)
  end

  def as_parameters
    [self]
  end

  def to_s
    s = ''
    format(s)
    s
  end

  def with_name(name)
    Entry.new(name, self)
  end

  def double_quote(str, bld)
    bld << '"'
    str.each_codepoint do |codepoint|
      case codepoint
      when 0x09
        bld << '\\t'
      when 0x0a
        bld << '\\n'
      when 0x0d
        bld << '\\r'
      when 0x22
        bld << '\\"'
      when 0x5c
        bld << '\\\\'
      else
        if codepoint < 0x20
          bld << sprintf('\\o%3.3o', codepoint)
        elsif codepoint <= 0x7f
          bld << codepoint
        else
          bld << [codepoint].pack('U')
        end
      end
    end
    bld << '"'
  end

  def format_elements(elements, b)
    elements.each_with_index do |e, i|
      b << ' ' if i > 0
      e.format(b)
    end
  end

  class Call
    include PN
    attr_reader :name
    def initialize(name, *elements)
      @name = name
      @elements = elements
    end

    def as_call(name)
      Call.new(name, @elements)
    end

    def as_parameters
      List.new(@elements)
    end

    def format(b)
      b << '(' << @name
      if @elements.size > 0
        b << ' '
        format_elements(@elements, b)
      end
      b << ')'
    end

    def to_data
      { '^' => [@name] + @elements.map { |e| e.to_data } }
    end
  end

  class Entry
    attr_reader :key, :value
    def initialize(key, value)
      @key = key
      @value = value
    end
  end

  class List
    include PN
    def initialize(elements)
      @elements = elements
    end

    def as_call(name)
      Call.new(name, *@elements)
    end

    def as_parameters
      @elements
    end

    def format(b)
      b << '['
      format_elements(@elements, b)
      b << ']'
    end

    def to_data
      @elements.map { |e| e.to_data }
    end
  end

  class Literal
    include PN
    attr_reader :value
    def initialize(value)
      @value = value
    end

    def format(b)
      if @value.nil?
        b << 'nil'
      elsif value.is_a?(String)
        double_quote(value, b)
      else
        b << value.to_s
      end
    end

    def to_data
      @value
    end
  end

  class Map
    include PN
    def initialize(entries)
      entries.each { |e| pnError("key #{e.key} does not conform to pattern /#{KEY_PATTERN.source}/)") unless e.key =~ KEY_PATTERN }
      @entries = entries
    end

    def format(b)
      b << '{'
      @entries.each_with_index do |e,i|
        b << ' ' if i > 0
        b << ':' << e.key
        b << ' '
        e.value.format(b)
      end
      b << '}'
    end

    def to_data
      r = []
      @entries.each { |e| r << e.key << e.value.to_data }
      { '#' => r }
    end
  end
end
end