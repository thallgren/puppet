# The Lexer is responsbile for turning source text into tokens.
# This version is a performance enhanced lexer (in comparison to the 3.x and earlier "future parser" lexer.
#
# Old returns tokens [:KEY, value, { locator = }
# Could return [[token], locator]
# or Token.new([token], locator) with the same API x[0] = token_symbol, x[1] = self, x[:key] = (:value, :file, :line, :pos) etc

require 'strscan'
require 'puppet/pops/parser/lexer_support'
require 'puppet/pops/parser/heredoc_support'
require 'puppet/pops/parser/interpolation_support'
require 'puppet/pops/parser/epp_support'
require 'puppet/pops/parser/slurp_support'

module Puppet::Pops
module Parser
class Lexer3
  include LexerSupport
  include HeredocSupport
  include InterpolationSupport
  include SlurpSupport
  include EppSupport

  # ALl tokens have three slots, the token name (a Symbol), the token text (String), and a token text length.
  # All operator and punctuation tokens reuse singleton arrays Tokens that require unique values create
  # a unique array per token.
  #
  # PEFORMANCE NOTES:
  # This construct reduces the amount of object that needs to be created for operators and punctuation.
  # The length is pre-calculated for all singleton tokens. The length is used both to signal the length of
  # the token, and to advance the scanner position (without having to advance it with a scan(regexp)).
  #
  TOKEN_LBRACK       = [:LBRACK,       '['.freeze,   1].freeze
  TOKEN_LISTSTART    = [:LISTSTART,    '['.freeze,   1].freeze
  TOKEN_RBRACK       = [:RBRACK,       ']'.freeze,   1].freeze
  TOKEN_LBRACE       = [:LBRACE,       '{'.freeze,   1].freeze
  TOKEN_RBRACE       = [:RBRACE,       '}'.freeze,   1].freeze
  TOKEN_SELBRACE     = [:SELBRACE,     '{'.freeze,   1].freeze
  TOKEN_LPAREN       = [:LPAREN,       '('.freeze,   1].freeze
  TOKEN_WSLPAREN     = [:WSLPAREN,     '('.freeze,   1].freeze
  TOKEN_RPAREN       = [:RPAREN,       ')'.freeze,   1].freeze

  TOKEN_EQUALS       = [:EQUALS,       '='.freeze,   1].freeze
  TOKEN_APPENDS      = [:APPENDS,      '+='.freeze,  2].freeze
  TOKEN_DELETES      = [:DELETES,      '-='.freeze,  2].freeze

  TOKEN_ISEQUAL      = [:ISEQUAL,      '=='.freeze,  2].freeze
  TOKEN_NOTEQUAL     = [:NOTEQUAL,     '!='.freeze,  2].freeze
  TOKEN_MATCH        = [:MATCH,        '=~'.freeze,  2].freeze
  TOKEN_NOMATCH      = [:NOMATCH,      '!~'.freeze,  2].freeze
  TOKEN_GREATEREQUAL = [:GREATEREQUAL, '>='.freeze,  2].freeze
  TOKEN_GREATERTHAN  = [:GREATERTHAN,  '>'.freeze,   1].freeze
  TOKEN_LESSEQUAL    = [:LESSEQUAL,    '<='.freeze,  2].freeze
  TOKEN_LESSTHAN     = [:LESSTHAN,     '<'.freeze,   1].freeze

  TOKEN_FARROW       = [:FARROW,       '=>'.freeze,  2].freeze
  TOKEN_PARROW       = [:PARROW,       '+>'.freeze,  2].freeze

  TOKEN_LSHIFT       = [:LSHIFT,       '<<'.freeze,  2].freeze
  TOKEN_LLCOLLECT    = [:LLCOLLECT,    '<<|'.freeze, 3].freeze
  TOKEN_LCOLLECT     = [:LCOLLECT,     '<|'.freeze,  2].freeze

  TOKEN_RSHIFT       = [:RSHIFT,       '>>'.freeze,  2].freeze
  TOKEN_RRCOLLECT    = [:RRCOLLECT,    '|>>'.freeze, 3].freeze
  TOKEN_RCOLLECT     = [:RCOLLECT,     '|>'.freeze,  2].freeze

  TOKEN_PLUS         = [:PLUS,         '+'.freeze,   1].freeze
  TOKEN_MINUS        = [:MINUS,        '-'.freeze,   1].freeze
  TOKEN_DIV          = [:DIV,          '/'.freeze,   1].freeze
  TOKEN_TIMES        = [:TIMES,        '*'.freeze,   1].freeze
  TOKEN_MODULO       = [:MODULO,       '%'.freeze,   1].freeze

  TOKEN_NOT          = [:NOT,          '!'.freeze,   1].freeze
  TOKEN_DOT          = [:DOT,          '.'.freeze,   1].freeze
  TOKEN_PIPE         = [:PIPE,         '|'.freeze,   1].freeze
  TOKEN_AT           = [:AT ,          '@'.freeze,   1].freeze
  TOKEN_ATAT         = [:ATAT ,        '@@'.freeze,  2].freeze
  TOKEN_COLON        = [:COLON,        ':'.freeze,   1].freeze
  TOKEN_COMMA        = [:COMMA,        ','.freeze,   1].freeze
  TOKEN_SEMIC        = [:SEMIC,        ';'.freeze,   1].freeze
  TOKEN_QMARK        = [:QMARK,        '?'.freeze,   1].freeze
  TOKEN_TILDE        = [:TILDE,        '~'.freeze,   1].freeze # lexed but not an operator in Puppet

  TOKEN_REGEXP       = [:REGEXP,       nil,   0].freeze

  TOKEN_IN_EDGE      = [:IN_EDGE,      '->'.freeze,  2].freeze
  TOKEN_IN_EDGE_SUB  = [:IN_EDGE_SUB,  '~>'.freeze,  2].freeze
  TOKEN_OUT_EDGE     = [:OUT_EDGE,     '<-'.freeze,  2].freeze
  TOKEN_OUT_EDGE_SUB = [:OUT_EDGE_SUB, '<~'.freeze,  2].freeze
  TOKEN_ZERO         = [:NUMBER,       '0'.freeze,   1].freeze

  # Tokens that are always unique to what has been lexed
  TOKEN_STRING         =  [:STRING, nil,          0].freeze
  TOKEN_WORD           =  [:WORD, nil,            0].freeze
  TOKEN_DQPRE          =  [:DQPRE,  nil,          0].freeze
  TOKEN_DQMID          =  [:DQPRE,  nil,          0].freeze
  TOKEN_DQPOS          =  [:DQPRE,  nil,          0].freeze
  TOKEN_NUMBER         =  [:NUMBER, nil,          0].freeze
  TOKEN_VARIABLE       =  [:VARIABLE, nil,        1].freeze
  TOKEN_VARIABLE_EMPTY =  [:VARIABLE, ''.freeze,  1].freeze

  TOKEN_NL             =  [:NL,     "\n".freeze,  1].freeze

  # HEREDOC has syntax as an argument.
  TOKEN_HEREDOC        =  [:HEREDOC, nil, 0].freeze

  # EPP_START is currently a marker token, may later get syntax
  TOKEN_EPPSTART       =  [:EPP_START, nil, 0].freeze
  TOKEN_EPPEND         =  [:EPP_END, '%>', 2].freeze
  TOKEN_EPPEND_TRIM    =  [:EPP_END_TRIM, '-%>', 3].freeze

  # This is used for unrecognized tokens, will always be a single character.
  TOKEN_OTHER        = [:OTHER,  nil,  1].freeze

  # Keywords are all singleton tokens with pre calculated lengths.
  # Booleans are pre-calculated (rather than evaluating the strings "false" "true" repeatedly.
  #
  KW_case = 'case'.freeze
  KW_class = 'class'.freeze
  KW_default = 'default'.freeze
  KW_define = 'define'.freeze
  KW_if = 'if'.freeze
  KW_elsif = 'elsif'.freeze
  KW_else = 'else'.freeze
  KW_inherits = 'inherits'.freeze
  KW_node = 'node'.freeze
  KW_and = 'and'.freeze
  KW_or = 'or'.freeze
  KW_undef = 'undef'.freeze
  KW_false = 'false'.freeze
  KW_true = 'true'.freeze
  KW_in = 'in'.freeze
  KW_unless = 'unless'.freeze
  KW_function = 'function'.freeze
  KW_type = 'type'.freeze
  KW_attr = 'attr'.freeze
  KW_private = 'private'.freeze
  KW_application = 'application'.freeze
  KW_consumes = 'consumes'.freeze
  KW_produces = 'produces'.freeze
  KW_site = 'site'.freeze

  KEYWORDS = {
    KW_case        => [:CASE,        KW_case,         4].freeze,
    KW_class       => [:CLASS,       KW_class,        5].freeze,
    KW_default     => [:DEFAULT,     KW_default,      7].freeze,
    KW_define      => [:DEFINE,      KW_define,       6].freeze,
    KW_if          => [:IF,          KW_if,           2].freeze,
    KW_elsif       => [:ELSIF,       KW_elsif,        5].freeze,
    KW_else        => [:ELSE,        KW_else,         4].freeze,
    KW_inherits    => [:INHERITS,    KW_inherits,     8].freeze,
    KW_node        => [:NODE,        KW_node,         4].freeze,
    KW_and         => [:AND,         KW_and,          3].freeze,
    KW_or          => [:OR,          KW_or,           2].freeze,
    KW_undef       => [:UNDEF,       KW_undef,        5].freeze,
    KW_false       => [:BOOLEAN,     false,           5].freeze,
    KW_true        => [:BOOLEAN,     true,            4].freeze,
    KW_in          => [:IN,          KW_in,           2].freeze,
    KW_unless      => [:UNLESS,      KW_unless,       6].freeze,
    KW_function    => [:FUNCTION,    KW_function,     8].freeze,
    KW_type        => [:TYPE,        KW_type,         4].freeze,
    KW_attr        => [:ATTR,        KW_attr,         4].freeze,
    KW_private     => [:PRIVATE,     KW_private,      7].freeze,
    KW_consumes    => [:CONSUMES,    KW_consumes,     8].freeze,
    KW_produces    => [:PRODUCES,    KW_produces,     8].freeze,
    KW_site        => [:SITE,        KW_site,         4].freeze,
    KW_application => [:APPLICATION, KW_application, 11].freeze,
  }.freeze

  # Reverse lookup of keyword name to string
  KEYWORD_NAMES = {}
  KEYWORDS.each_pair {|k, v| KEYWORD_NAMES[v[0]] = k }
  KEYWORD_NAMES.freeze

  HEX_DIGIT = []
  LETTER = []
  LETTER_OR_DIGIT = []
  WORD = []
  WHITESPACE = [0x09, 0x0a, 0x0c, 0x20].freeze

  (0x30..0x39).each { |n| WORD[n] = true; LETTER_OR_DIGIT[n] = true; HEX_DIGIT[n] = true }
  (0x41..0x46).each { |n| WORD[n] = true; LETTER_OR_DIGIT[n] = true; LETTER[n] = true; HEX_DIGIT[n] = true }
  (0x47..0x5a).each { |n| WORD[n] = true; LETTER_OR_DIGIT[n] = true; LETTER[n] = true }
  (0x61..0x66).each { |n| WORD[n] = true; LETTER_OR_DIGIT[n] = true; LETTER[n] = true; HEX_DIGIT[n] = true }
  (0x67..0x7a).each { |n| WORD[n] = true; LETTER_OR_DIGIT[n] = true; LETTER[n] = true }
  WORD[0x5f] = true

  HEX_DIGIT.freeze
  LETTER.freeze
  LETTER_OR_DIGIT.freeze
  WORD.freeze

  # Invalid codepoint, used to denote end of stream. Saves checks for nil (nil does not respond to '<' '>')
  EOF = 0xffffffff

  def octal_digit?(c)
    c >= 0x30 && c <= 0x37
  end

  def upper_case_letter?(c)
    c >= 0x41 && c <= 0x5a
  end

  def lower_case_letter?(c)
    c >= 0x61 && c <= 0x7a
  end

  def letter?(c)
    LETTER[c]
  end

  def letter_or_digit?(c)
    LETTER_OR_DIGIT[c]
  end

  def digit?(c)
    c >= 0x30 && c <= 0x39
  end

  def hex_digit?(c)
    HEX_DIGIT[c]
  end

  def whitespace?(c)
    WHITESPACE.include?(c)
  end

  def word_char?(c)
    WORD[c]
  end

  attr_reader :locator

  def initialize()
    @token_queue = []
    @selector = []
    (0x00..0x7f).each { |c| @selector[c] = proc { |i,c| emit([:OTHER,  c.chr,  1], i) } }

    # TOKEN '.'
    @selector[0x2e] = proc { |i| emit(TOKEN_DOT, i) }

    # TOKEN ','
    @selector[0x2c] = proc { |i| emit(TOKEN_COMMA, i) }

    # TOKEN '['
    @selector[0x5b] = proc do |i|
      prev = @last
      emit(prev.nil? || prev.token_array == TOKEN_NL || prev.offset + prev.token_array[2] < i ? TOKEN_LISTSTART : TOKEN_LBRACK, i)
    end

    # TOKEN ']'
    @selector[0x5d] = proc { |i| emit(TOKEN_RBRACK, i) }

    # TOKEN '('
    @selector[0x28] = proc { |i| emit(@last.nil? || @last.token_array == TOKEN_NL ? TOKEN_WSLPAREN :  TOKEN_LPAREN, i) }

    # TOKEN ')'
    @selector[0x29] = proc { |i| emit(TOKEN_RPAREN, i) }

    # TOKEN ';'
    @selector[0x3b] = proc { |i| emit(TOKEN_SEMIC, i) }

    # TOKEN '?'
    @selector[0x3f] = proc { |i| emit(TOKEN_QMARK, i) }

    # TOKEN '*'
    @selector[0x2a] = proc { |i| emit(TOKEN_TIMES, i) }

    # TOKEN '%', '%>'
    @selector[0x25] = proc do |i|
      if @codepoints[@next] == 0x3e
        @next += 1
        emit(TOKEN_EPPEND, i)
      else
        emit(TOKEN_MODULO, i)
      end
    end

    # TOKEN '{'
    @selector[0x7b] = proc do |i|
      @brace_count += 1
      emit(@last && @last.token_array == TOKEN_QMARK ? TOKEN_SELBRACE : TOKEN_LBRACE, i)
    end

    # TOKEN '}'
    @selector[0x7d] = proc do |i|
      @brace_count -= 1
      emit(TOKEN_RBRACE, i)
    end


    # TOKENS '@', '@@', '@('
    @selector[0x40] = proc do |i|
      c = @codepoints[@next]
      if c == 0x40
        @next += 1
        emit(TOKEN_ATAT, i)
      elsif c == 0x28
        @next += 1
        emit(TOKEN_ATLP, i)
      else
        emit(TOKEN_AT, i)
      end
    end

    # TOKENS '|', '|>', '|>>'
    @selector[0x7c] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3e
        c = @codepoints[@next += 1]
        if c == 0x3e
          @next += 1
          emit(TOKEN_RRCOLLECT, i)
        else
          emit(TOKEN_RCOLLECT, i)
        end
      else
        emit(TOKEN_PIPE, i)
      end
    end

    # TOKENS '=', '=>', '==', '=~'
    @selector[0x3d] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3e
        @next += 1
        emit(TOKEN_FARROW, i)
      elsif c == 0x3d
        @next += 1
        emit(TOKEN_ISEQUAL, i)
      elsif c == 0x7e
        @next += 1
        emit(TOKEN_MATCH, i)
      else
        emit(TOKEN_EQUALS, i)
      end
    end

    # TOKENS '+', '+=', and '+>'
    @selector[0x2b] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3d
        @next += 1
        emit(TOKEN_APPENDS, i)
      elsif c == 0x3e
        @next += 1
        emit(TOKEN_PARROW, i)
      else
        emit(TOKEN_PLUS, i)
      end
    end

    # TOKENS '-', '->', '-=', and epp '-%>' (end of interpolation with trim)
    @selector[0x2d] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3e # '>'
        @next += 1
        emit(TOKEN_IN_EDGE, i)
      elsif c == 0x3d # '='
        @next += 1
        emit(TOKEN_DELETES, i)
      elsif c == 0x25 # '%'
        if @codepoints[@next + 1] == 0x3e # '>'
          @next += 2
          emit(TOKEN_EPPEND_TRIM, i)
        else
          emit(TOKEN_MINUS, i)
        end
      else
        emit(TOKEN_MINUS, i)
      end
    end

    # TOKENS '!', '!=', '!~'
    @selector[0x21] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3d
        @next += 1
        emit(TOKEN_NOTEQUAL, i)
      elsif c == 0x7e
        @next += 1
        emit(TOKEN_NOMATCH, i)
      else
        emit(TOKEN_NOT, i)
      end
    end

    # TOKENS '~>', '~'
    @selector[0x7e] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3e
        @next += 1
        emit(TOKEN_IN_EDGE_SUB, i)
      else
        emit(TOKEN_TILDE, i)
      end
    end

    # TOKEN '#'
    @selector[0x23] = proc do |i|
      loop do
        c = @codepoints[i += 1]
        if c == 0x0a || c.nil?
          @next = i + 1
          break
        end
      end
      nil
    end

    # TOKENS '/', '/*'
    @selector[0x2f] = proc do |i|
      c = @codepoints[@next]
      if c == 0x2a
        # scan for comment end
        c = @codepoints[@next += 1]
        ok = loop do
          c2 = @codepoints[@next += 1]
          break false if c2.nil?
          break true if c == 0x2a && c2 == 0x2f
          c = c2
        end
        lex_error(Issues::UNCLOSED_MLCOMMENT, EMPTY_HASH, i) unless ok
        @next += 1
        nil
      elsif regexp_acceptable?
        emit_regexp(i)
      else
        emit(TOKEN_DIV, i)
      end
    end

    # TOKENS '<', '<=', '<|', '<-', '<~', '<<|', '<<'
    @selector[0x3c] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3d # '='
        @next += 1
        emit(TOKEN_LESSEQUAL, i)
      elsif c == 0x7c # '|'
        @next += 1
        emit(TOKEN_LCOLLECT, i)
      elsif c == 0x2d # '-'
        @next += 1
        emit(TOKEN_OUT_EDGE, i)
      elsif c == 0x7e # '~'
        @next += 1
        emit(TOKEN_OUT_EDGE_SUB, i)
      elsif c == 0x3c # '<'
        if @codepoints[@next + 1] == 0x7c # '|'
          @next += 2
          emit(TOKEN_LLCOLLECT, i)
        else
          emit(TOKEN_LSHIFT, i)
        end
      else
        emit(TOKEN_LESSTHAN, i)
      end
    end

    # TOKENS '>', '>=', '>>'
    @selector[0x3e] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3d # '='
        @next += 1
        emit(TOKEN_GREATEREQUAL, i)
      elsif c == 0x3e # '>'
        @next += 1
        emit(TOKEN_RSHIFT, i)
      else
        emit(TOKEN_GREATERTHAN, i)
      end
    end

    # TOKENS ':', '::CLASSREF', '::NAME'
    @selector[0x3a] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3a
        c = @codepoints[@next += 1] || EOF
        if upper_case_letter?(c)
          emit_classref('::', i)
        elsif lower_case_letter?(c)
          emit_name('::', i, true)
        else
          lex_error(Issues::ILLEGAL_FULLY_QUALIFIED_NAME, EMPTY_HASH, i + 2)
        end
      else
        emit(TOKEN_COLON, i)
      end
    end

    # TOKENS '$', '$VAR'
    #   PATTERN_DOLLAR_VAR     = %r{\$(::)?(\w+::)*\w+}.freeze
    @selector[0x24] = proc do |i|
      c = @codepoints[@next]
      if c == 0x3a
        lex_error(Issues::ILLEGAL_VARIABLE, i, :value => '$:') unless  @codepoints[@next += 1] == 0x3a
        lex_error(Issues::ILLEGAL_VARIABLE, i, :value => '$::') unless word_char?(@codepoints[@next += 1] || EOF)
        emit_variable('::', i)
      elsif word_char?(c || EOF)
        emit_variable('', i)
      else
        emit(TOKEN_VARIABLE_EMPTY, i)
      end
    end

    # TOKEN '"'
    @selector[0x22] = proc { |i| emit_dqstring(i, true) }

    # TOKEN "'"
    @selector[0x27] = proc { |i| emit_sqstring(i) }

    # TOKEN LF (Line Feed)
    @selector[0x0a] = proc { |i| emit(TOKEN_NL, i); nil }

    # TOKEN WS (Space, Horizontal Tab, Carriage Return)
    skip_ws = proc do |i|
      loop do
        break nil unless whitespace?(@codepoints[@next] || EOF)
        @next += 1
      end
    end

    @selector[0x09] = skip_ws
    @selector[0x0d] = skip_ws
    @selector[0x20] = skip_ws

    # TOKEN '0'
    @selector[0x30] = proc do |i|
      c = @codepoints[@next] || EOF
      if c == 0x58 || c == 0x78 # 'X' or 'x'
        c2 = @codepoints[@next += 1] || EOF
        str = '0'
        str << c
        str << c2
        lex_error(Issues::INVALID_HEX_NUMBER, {:value => str}, i) unless hex_digit?(c2)
        loop do
          c = @codepoints[@next += 1] || EOF
          unless hex_digit?(c)
            lex_error(Issues::INVALID_HEX_NUMBER, {:value => str}, i) if word_char?(c)
            break
          end
          str << c
        end
        emit([:NUMBER, str, str.size], i)
      elsif c && octal_digit?(c)
        # Octal number
        str = '0'
        str << c
        loop do
          c = @codepoints[@next += 1] || EOF
          unless octal_digit?(c)
            lex_error(Issues::INVALID_OCTAL_NUMBER, {:value => str}, i) if letter_or_digit?(c)
            break
          end
          str << c
        end
        emit([:NUMBER, str, str.size], i)
      elsif c == 0x2e || c == 0x45 || c == 0x65 # dot, 'E', or 'e'
        str = parseFloat('0', c, i)
        emit([:NUMBER, str, str.size], i)
      else
        emit(TOKEN_ZERO, i)
      end
    end

    # TOKEN NUMBER (1 - 9)
    (0x31..0x39).each do |n|
      @selector[n] = proc do |i, c|
        str = c.chr
        loop do
          c = @codepoints[@next] || EOF
          unless digit?(c)
            if c == 0x2e || c == 0x45 || c == 0x65 # dot, 'E', or 'e'
              str = parseFloat(str, c, i)
            else
              lex_error(Issues::ILLEGAL_NUMBER, {:value => str}, i) if letter?(c)
            end
            break
          end
          @next += 1
          str << c
        end
        emit([:NUMBER, str, str.size], i)
      end
    end

    # TOKENS KEYWORD, NAME, or WORD
    @selector[0x5f] = proc { |i, c| emit_name(c.chr, i, false) }
    (0x61..0x7a).each { |n| @selector[n] = proc { |i, c| emit_name(c.chr, i, true) } }

    # CLASSREF
    #
    (0x41..0x5a).each do |n|
      @selector[n] = proc { |i, c| emit_classref(c.chr, i) }
    end
    @selector.freeze
  end

  # PATTERN_CLASSREF       = %r{((::){0,1}[A-Z][\w]*)+}.freeze
  def emit_classref(str, i)
    loop do
      c = @codepoints[@next] || EOF
      unless letter_or_digit?(c)
        if c != 0x3a # ':'
          break
        end

        if @codepoints[@next += 1] != 0x3a # '::'
          # only one colon is OK as next token
          @next -= 1
          break
        end

        # Must be followed by an upper case letter
        c = @codepoints[@next += 1]
        unless upper_case_letter?(c || EOF)
          lex_error(str.start_with?('::') ? Issues::ILLEGAL_FULLY_QUALIFIED_CLASS_REFERENCE : Issues::ILLEGAL_CLASS_REFERENCE, EMPTY_HASH, i)
        end
        str << '::'
      end
      @next += 1
      str << c
    end
    str.freeze
    emit([:CLASSREF, str, str.size], i)
  end

  # PATTERN_DOLLAR_VAR     = %r{\$(::)?(\w+::)*\w+}.freeze
  def emit_variable(str, i)
    loop do
      c = @codepoints[@next]
      break if c.nil?
      unless word_char?(c)
        unless c == 0x3a # ':'
          break
        end

        unless @codepoints[@next += 1] == 0x3a # '::'
          # only one colon is OK as next token
          @next -= 1
          break
        end

        # Must be followed by a word character
        c = @codepoints[@next += 1]
        lex_error(Issues::ILLEGAL_VARIABLE_NAME, EMPTY_HASH, i) unless word_char?(c || EOF)
        str << '::'
      end
      @next += 1
      str << c
    end
    str.freeze
    emit([:VARIABLE, str, str.size + 1], i)
  end

  def emit_sqstring(i)
    str = ''
    loop do
      c = @codepoints[@next]
      lex_error(Issues::UNCLOSED_QUOTE, { :after => '"\'"', :followed_by => '<eof>' }, i) if c.nil?
      @next += 1
      break if c == 0x27
      if c == 0x5c # '\'
        c = @codepoints[@next]
        lex_error(Issues::UNCLOSED_QUOTE, { :after => '"\'"', :followed_by => '<eof>' }, i) if c.nil?
        @next += 1
        str << '\\' unless c == 0x27 || c == 0x5c
      end
      str << c
    end
    emit([:STRING, str.freeze, @next - i], i)
  end

  #   PATTERN_REGEX     = %r{/[^/\n]*/}
  def emit_regexp(i)
    str = ''
    result = loop do
      c = @codepoints[@next]
      break false if c.nil?
      @next += 1
      if c == 0x5c && @codepoints[@next] == 0x5c # '\\'
        c = @codepoints[@next += 1]
        break false if c.nil? || c == 0x0a
        str << '\\\\' unless c == 0x2f
      else
        break false if c == 0x0a
        break true if c == 0x2f # '/'
      end
      str << c
    end
    if result
      emit([:REGEX, Regexp.new(str), (@next - 1) - i], i)
    else
      @next = i + 1
      emit(TOKEN_DIV, i)
    end
  end

  def hex_to_int(c)
    if c >= 0x41 && c <= 0x46
      c - 0x37 # 'A' => 10
    elsif c >= 0x61 && c <= 0x66
      c - 0x57 # 'a' => 10
    else
      c - 0x30 # '0' => 0
    end
  end

  def emit_dqstring(i, first)
    str = ''
    if first
      len = 1
    else
      len = 0
    end
    loop do
      c = @codepoints[@next]
      lex_error(Issues::UNCLOSED_QUOTE, { :after => "'\"'", :followed_by => '<eof>' }, i) unless c

      len += 1
      @next += 1
      if c == 0x22
        @token_queue << emit([first ? :STRING : :DQPOST, str.freeze, len], i)
        break
      end
      if c == 0x5c # '\'
        c = @codepoints[@next]
        lex_error(Issues::UNCLOSED_QUOTE, { :after => "'\"'", :followed_by => '<eof>' }, i) unless c

        len += 1
        @next += 1
        case c
        when 0x5c, 0x24, 0x27 # '\\', '$', '"'
          str << c
        when 0x6e # 'n'
          str << "\n"
        when 0x72 # 'r'
          str << "\r"
        when 0x73 # ' '
          str << ' '
        when 0x74 # 't'
          str << "\t"
        when 0x75 # 'u', unicode escape
          c = @codepoints[@next] || EOF
          len += 1
          if c == 0x7b # '{'
            # 1-6 hex-digits
            code = 0
            loop do
              c = @codepoints[@next += 1] || EOF
              len += 1
              break if c == 0x7d # '}'
              lex_error(Issues::ILLEGAL_UNICODE_ESCAPE, i) unless hex_digit?(c)
              code *= 0x10
              code += hex_to_int(c)
            end
            str << [code].pack('U')
          elsif hex_digit?(c)
            # exactly 4 hex dgits
            c2 = @codepoints[@next += 1] || EOF
            c3 = @codepoints[@next += 1] || EOF
            c4 = @codepoints[@next += 1] || EOF
            len += 3
            lex_error(Issues::ILLEGAL_UNICODE_ESCAPE, i) unless hex_digit?(c2) && hex_digit?(c3) && hex_digit?(c4)
            str.concat(((hex_to_int(c) * 0x10 + hex_to_int(c2)) * 0x10 + hex_to_int(c3)) * 0x10 + hex_to_int(c4))
          else
            lex_error(Issues::ILLEGAL_UNICODE_ESCAPE, i)
          end
          @next += 1
        else
          str << c
        end
      elsif c == 0x24 # '$'
        var_start = @next - 1
        n = @codepoints[@next] || EOF
        if n == 0x3a # ':'
          lex_error(Issues::ILLEGAL_VARIABLE, i, :value => '$:') unless  @codepoints[@next += 1] == 0x3a
          lex_error(Issues::ILLEGAL_VARIABLE, i, :value => '$::') unless word_char?(@codepoints[@next += 1] || EOF)
          @token_queue << emit([first ? :DQPRE : :DQMID, str, len - 1], i)
          @token_queue << emit_variable('::', var_start)
          emit_dqstring(@next, false)
          break
        elsif word_char?(n)
          @token_queue << emit([first ? :DQPRE : :DQMID, str, len - 1], i)
          @token_queue << emit_variable('', var_start)
          emit_dqstring(@next, false)
          break
        elsif n == 0x7b # '{'
          # Nested expression, queue tokens until brace count is back to current
          @next += 1
          current_brace_count = @brace_count
          @brace_count += 1

          queue = @token_queue
          selector = @selector
          new_queue = []
          loop do
            token = queue.shift
            if token
              new_queue << token
            else
              pos = @next
              @next += 1
              c = @codepoints[pos] || EOF
              if c < 0x80
                token = selector[c].call(pos, c)
                break if @brace_count == current_brace_count
                new_queue << token unless token.nil?
              elsif c == EOF
                break
              else
                new_queue << emit([:OTHER,  [c].pack('U'),  1], pos)
              end
            end
          end

          if new_queue.size == 1
            new_queue[0] = transform_to_variable(new_queue[0])
          elsif new_queue.size > 1
            second = new_queue[1][0]
            new_queue[0] = transform_to_variable(new_queue[0]) if second == :DOT || second == :LBRACK
          end

          queue << emit([first ? :DQPRE : :DQMID, str, len + 1], i)
          queue.concat(new_queue)
          emit_dqstring(@next, false)
          break
        else
          str << c
        end
      else
        str << c
      end
    end
    first ? @token_queue.shift : nil
  end

  # PATTERN_NAME           = %r{^((::)?[a-z][\w]*)(::[a-z][\w]*)*$}
  # PATTERN_BARE_WORD     = %r{((?:::){0,1}(?:[a-z_](?:[\w-]*[\w])?))+}
  def emit_name(str, i, name)
    loop do
      c = @codepoints[@next] || EOF
      unless word_char?(c)
        if c == 0x2d # '-'
          cnt = 1
          loop do
            c = @codepoints[@next += 1] || EOF
            break unless c == 0x2d
            cnt += 1
          end

          unless word_char?(c)
            cnt.times { @token_queue << TOKEN_MINUS }
            break
          end
          cnt.times { str << '-' }
          name = false
        else
          if c != 0x3a # ':'
            break
          end

          if @codepoints[@next += 1] != 0x3a # '::'
            # only one colon is OK as next token
            @next -= 1
            break
          end

          # Must be followed by a lower case letter
          c = @codepoints[@next += 1] || EOF
          if c == 0x5f
            name = false
          else
            lex_error(Issues::ILLEGAL_FULLY_QUALIFIED_NAME, EMPTY_HASH, i) unless lower_case_letter?(c)
          end
          str << '::'
        end
      end
      @next += 1
      str << c
    end
    str.freeze
    emit(name ? (KEYWORDS[str] || [:NAME, str, str.size]) : [:WORD, str, str.size], i)
  end

  # Parse what follows after '.', 'e', or 'E'.
  #
  # @param str [String] digits preceding the '.', 'e', or 'E'
  # @param floatchar [Integer] codepoint of the '.', 'e', or 'E'
  # @return [String] the parsed float
  def parseFloat(str, floatchar, i)
    c = @codepoints[@next += 1] || EOF
    if floatchar == 0x2e && !digit?(c)
      return str
    end

    str << floatchar
    if c == 0x2d && (floatchar == 0x45 || floatchar == 0x65) # 'e-' or 'E-'
      str << c
      c = @codepoints[@next += 1] || EOF
    end

    lex_error(Issues::INVALID_DECIMAL_NUMBER, {:value => str}, i) unless digit?(c)
    loop do
      str << c
      c = @codepoints[@next += 1] || EOF
      break unless c && digit?(c)
    end

    # check for 'e' or 'E' if we are in fractions
    if floatchar == 0x2e && (c == 0x45 || c == 0x65)
      str = parseFloat(str, c, i)
    else
      lex_error(Issues::INVALID_DECIMAL_NUMBER, {:value => str}, i) if c && letter?(c)
    end
    str
  end

  # Clears the lexer state (it is not required to call this as it will be garbage collected
  # and the next lex call (lex_string, lex_file) will reset the internal state.
  #
  def clear()
    # not really needed, but if someone wants to ensure garbage is collected as early as possible
    @locator = nil
    @lexing_context = nil
    @pushback = nil
    @codepoints = nil
    @token_queue = nil
  end

  # Convenience method, and for compatibility with older lexer. Use the lex_string instead which allows
  # passing the path to use without first having to call file= (which reads the file if it exists).
  # (Bad form to use overloading of assignment operator for something that is not really an assignment. Also,
  # overloading of = does not allow passing more than one argument).
  #
  def string=(string)
    lex_string(string, nil)
  end

  def lex_string(string, path=nil)
    initvars
    assert_not_bom(string)
    @codepoints = string.codepoints.freeze
    line_index = [0]
    @codepoints.each_with_index { |n, i| line_index << i + 1 if n == 0x0a }
    @locator = Locator.locator(string, '', line_index, true)
  end

  # Lexes an unquoted string.
  # @param string [String] the string to lex
  # @param locator [Locator] the locator to use (a default is used if nil is given)
  # @param escapes [Array<String>] array of character strings representing the escape sequences to transform
  # @param interpolate [Boolean] whether interpolation of expressions should be made or not.
  #
  def lex_unquoted_string(string, locator, escapes, interpolate)
    initvars
    assert_not_bom(string)
    @codepoints = string.codepoints.freeze
    unless locator
      line_index = [0]
      @codepoints.each_with_index { |n, i| line_index << i + 1 if n == 0x0a }
      @locator = Locator.locator(string, '', line_index, true)
    end
    @lexing_context[:escapes] = escapes || UQ_ESCAPES
    @lexing_context[:uq_slurp_pattern] = interpolate ? (escapes.include?('$') ? SLURP_UQ_PATTERN : SLURP_UQNE_PATTERN) : SLURP_ALL_PATTERN
  end

  # Convenience method, and for compatibility with older lexer. Use the lex_file instead.
  # (Bad form to use overloading of assignment operator for something that is not really an assignment).
  #
  def file=(file)
    lex_file(file)
  end

  # TODO: This method should not be used, callers should get the locator since it is most likely required to
  # compute line, position etc given offsets.
  #
  def file
    @locator ? @locator.file : nil
  end

  # Initializes lexing of the content of the given file. An empty string is used if the file does not exist.
  #
  def lex_file(file)
    initvars
    contents = Puppet::FileSystem.exist?(file) ? Puppet::FileSystem.read(file, :encoding => 'utf-8') : ''
    assert_not_bom(contents)
    @codepoints = contents.codepoints.freeze
    line_index = [0]
    @codepoints.each_with_index { |n, i| line_index << i + 1 if n == 0x0a }
    @locator = Locator.locator(contents, '', line_index, true)
  end

  def initvars
    @brace_count = 0
    @next = 0
    @token_queue = []
  end

  # Scans all of the content and returns it in an array
  # Note that the terminating [false, false] token is included in the result.
  #
  def fullscan
    result = []
    scan {|token| result.push(token) }
    result
  end

  # A block must be passed to scan. It will be called with two arguments, a symbol for the token,
  # and an instance of LexerSupport::TokenValue
  # PERFORMANCE NOTE: The TokenValue is designed to reduce the amount of garbage / temporary data
  # and to only convert the lexer's internal tokens on demand. It is slightly more costly to create an
  # instance of a class defined in Ruby than an Array or Hash, but the gain is much bigger since transformation
  # logic is avoided for many of its members (most are never used (e.g. line/pos information which is only of
  # value in general for error messages, and for some expressions (which the lexer does not know about).
  #

  def scan
    queue = @token_queue
    selector = @selector
    loop do
      token = queue.shift
      if token
        yield(token)
      else
        pos = @next
        @next += 1
        c = @codepoints[pos]
        break if c.nil?
        if c < 0x80
          token = selector[c].call(pos, c)
          yield(token) unless token.nil?
        else
          yield(emit([:OTHER,  [c].pack('U'),  1], pos))
        end
      end
    end

    # Signals end of input
    yield [false, false]
  end

  # Emits (produces) a token [:tokensymbol, TokenValue]
  #
  def emit(token, position)
    [token[0], @last = TokenValue.new(token, position, @locator)]
  end

  UNACCEPTABLE_BEFORE_REGEXP = {
    :RPAREN => true,
    :RBRACK => true,
    :RRCOLLECT => true,
    :RCOLLECT => true,
    :NAME => true,
    :CLASSREF => true,
    :NUMBER => true,
    :STRING => true,
    :BOOLEAN => true,
    :DQPRE => true,
    :DQMID => true,
    :DQPOST => true,
    :HEREDOC => true,
    :REGEX => true,
    :VARIABLE => true,
    :WORD => true
  }.freeze

  # Answers after which tokens it is acceptable to lex a regular expression.
  # PERFORMANCE NOTE:
  # It may be beneficial to turn this into a hash with default value of true for missing entries.
  # A case expression with literal values will however create a hash internally. Since a reference is
  # always needed to the hash, this access is almost as costly as a method call.
  #
  def regexp_acceptable?
    @last.nil? || !UNACCEPTABLE_BEFORE_REGEXP.include?(@last.token_array[0])
  end
end
end
end
