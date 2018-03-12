module Puppet::PSpec
  class Element
    include Puppet::Pops::Types::PuppetObject

    def self._pcore_type
      @type ||= Puppet::Pops::Types::TypeParser.singleton.parse("PSpec::#{name.split(/::/).last}")
    end

    def error(issue, args)
      raise Puppet::ParseError, issue.format(args)
    end

    def leaf_name
      self.class.name.split(/::/).last
    end
  end

  # Inputs


  # Abstract base class for all Input classes
  class Input < Element
    # @param result [Result] the result to create tests for
    # @return [Array<Lambda>] array of lambdas
    def create_tests(result)
      raise Puppet::Error('derived must implement')
    end
  end

  # Given is a list of inputs
  class Given < Element
    def self.create_new_function(t)
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        dispatch :create do
          repeated_param 'Variant[Get,String,Input]', :args
        end

        def create(*args)
          Given.new(args.map { |arg| arg.is_a?(String) || arg.is_a?(Get) ? Source.new([arg]) : arg })
        end
      end
    end

    attr_reader :inputs
    def initialize(inputs)
      @inputs = inputs
    end
  end

  # Named_source is a single source code string with a name (a path). Used when testing
  # that file is included in errors and warnings
  class Named_source < Input
    attr_reader :name, :source
    def initialize(name, source)
      @name = name
      @source = source
    end

    def create_tests(result)
      result.create_test(self)
    end
  end

  # Source is an Input that provides one or many puppet source code strings
  class Source < Input
    def self.create_new_function(t)
      impl_class = self
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        @impl_class = impl_class

        def self.create(args)
          @impl_class.new(args)
        end

        dispatch :create do
          repeated_param 'String', :args
        end

        def create(*args)
          self.class.create(args)
        end
      end
    end

    def initialize(sources)
      @sources = sources
    end

    def create_tests(result)
      @sources.map do |s|
        result.create_test(:source => s, :epp => epp)
      end
    end

    def epp
      false
    end
  end

  # Epp_source is an Input that provides one or many EPP source code strings
  class Epp_source < Source
    def epp
      true
    end
  end

  class Parser_options < Input
    attr_reader :options
    def initialize(options)
      @options = options
    end

    def create_tests(_)
      [->(ctx, _) { ctx.add_parser_options(@options) }]
    end
  end

  class Expectation < Element
    def self.create_new_function(t)
      impl_class = self
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        @impl_class = impl_class

        def self.create(args)
          @impl_class.new(args)
        end

        dispatch :create do
          repeated_param 'Variant[String,Regexp,Issue,IssueCode,Match,Include,Exclude]', :args
        end

        def create(*args)
          self.class.create(args)
        end
      end
    end

    def initialize(args)
      includes = []
      excludes = []
      args.each do |arg|
        case arg
        when Include
          includes.concat(arg.matches)
        when Exclude
          excludes.concat(arg.matches)
        when Issue,Match
          includes << arg
        when IssueCode
          includes << Issue.new(arg)
        else
          includes << Match.new(arg)
        end
      end
      @includes = includes
      @excludes = excludes
    end

    def is_log_entry_matched?(entry)
      @includes.any? { |m| m.match_log_entry?(entry) }
    end

    def is_diagnostic_matched?(diagnostic)
      @includes.any? { |m| m.match_diagnostic?(diagnostic)}
    end

    def match_entries(b, log, all_diagnostics)
      my_level = level.to_sym
      entries = log.select { |entry| entry.level == my_level }
      diagnostics = all_diagnostics.select do |diagnostic|
        case diagnostic.severity
        when :warning, :deprecation
          level == :warning
        when :error
          level == :err
        else
          false
        end
      end

      entries.each do |entry|
        unless is_log_entry_matched?(entry)
          @excludes.each { |m| b << "#{level}(#{entry}) matches exclusion #{m}\n" if m.match_log_entry?(entry) }
        end
      end

      diagnostics.each do |diagnostic|
        unless is_diagnostic_matched?(diagnostic)
          @excludes.each { |e| b << "#{diagnostic} matches exclusion #{m}\n" if m.match_diagnostic?(diagnostic) }
        end
      end

      @includes.each do |m|
        unless entries.any? { |entry| m.match_log_entry?(entry.message) }
          unless diagnostics.any? { |diagnostic| m.match_diagnostic(diagnostic) }
            b << "Expected #{level}(#{m.issue_code}) but it was not produced\n"
          end
        end
      end
    end
  end

  class Warning < Expectation
    def level
      'warning'
    end
  end

  class Error < Expectation
    def level
      'err'
    end
  end

  class Notice < Expectation
    def level
      'notice'
    end
  end

  class MatchContainer < Element
    attr_reader :matches

    def self.create_new_function(t)
      impl_class = self
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        @impl_class = impl_class

        def self.create(args)
          @impl_class.new(args)
        end

        dispatch :create do
          repeated_param 'Variant[String,Regexp,Issue,IssueCode,Match]', :args
        end

        def create(*args)
          self.class.create(args)
        end
      end
    end

    def initialize(args)
      matches = []
      args.each do |arg|
        case arg
        when Issue,Match
          matches << arg
        when IssueCode
          matches << Issue.new(arg)
        else
          matches << Match.new(arg)
        end
      end
      @matches = matches
    end
  end

  class Include < MatchContainer
  end

  class Exclude < MatchContainer
  end

  class Issue < Element
    def initialize(code, args_map = nil)
      @code = code
      @args_map = args_map || Puppet::Pops::EMPTY_HASH
    end

    # @param diagnostic [Puppet::Pops::Validation::Diagnostic] the diagnostic to match against
    def match_diagnostic?(diagnostic)
      return false unless @code == diagnostic.issue_code
      return true if @args_map.empty?
      args = diagnostic.arguments
      @args_map.each_pair do |k, v|
        return false unless args.include?(k) && v.match_string(args[k].to_s)
      end
      return true
    end

    def issue_code
      @code
    end

    def match_log_entry?(entry)
      match_diagnostic?(entry)
    end

    def match_string?(str)
      @code == str
    end

    def to_s
      @code
    end
  end

  class Match < Element
    def initialize(value)
      @value = value
    end

    def match_diagnostic?(diagnostic)
      match_string(Puppet::PSpec.formatter.format(diagnostic))
    end

    def match_log_entry?(entry)
      match_string(entry.message)
    end

    def match_string?(str)
      if @value.is_a?(String)
        @value == str
      else
        # Regexp works like Contain. It must be anchored in order to
        # loose that semantics
        !@value.match(str).nil?
      end
    end
  end

  class Contain < Match
    def match_string?(str)
      if @value.is_a?(String)
        str.include?(@value)
      else
        !@value.match(str).nil?
      end
    end
  end

  # Results

  class ErrorDiagnostic
    def initialize(error)
      @error = error
    end

    def arguments
      @error.arguments
    end

    def issue_code
      @error.issue_code
    end

    def message
      @error.message
    end

    def severity
      :err
    end
  end

  # Result is the abstract base class for the Evaluates_<ok/to/with> and Parses_<ok/to>, and Validates_with family of classes
  class Result < Element
    def create_test(actual)
      raise Puppet::Error('derived must implement')
    end

    def parse_and_validate(path, source, options)
      Puppet[:tasks] = !!options['tasks']
      pc = options['epp'] ? Puppet::Pops::Parser::EvaluatingParser::EvaluatingEppParser : Puppet::Pops::Parser::EvaluatingParser
      epc = pc.new

      begin
        result = epc.parser.parse_string(source, path)
      rescue Puppet::ParseErrorWithIssue => e
        return [nil, [ErrorDiagnostic.new(e)]]
      end

      if result['source_ref'].nil? || result['source_ref'] == ''
        result['source_ref'] = path
      end
      result = result.model
      validation_result = epc.validate(result)
      [result, validation_result.errors_and_warnings]
    end

    def path_content_epp(input)
      case input
      when Named_source
        [input.name, input.source, false]
      when Hash # Element from Source or Epp_source
        [input[:name], input[:source], input[:epp]]
      else
        raise ArgumentError, 'unknown input'
      end
    end
  end

  class Parses_to < Result
    attr_reader :pn_result
    def initialize(pn_result)
      @pn_result = pn_result
    end

    def create_test(actual)
      path, source, epp = path_content_epp(actual)
      -> (ctx, assertions) do
        source = ctx.resolve_lazy_value(source)
        error(VALUE_NOT_STRING, :name => 'Source', :type => source.class.name) unless source.is_a?(String)
        pn_result = ctx.resolve_lazy_value(@pn_result)
        error(VALUE_NOT_STRING, :name => leaf_name, :type => pn_result.class.name) unless pn_result.is_a?(String)

        # TODO: Pass the call expression locator and position to the PNParser somehow so that it can report errors with correct file, line, and position on line
        expected_pn = Puppet::Pops::Parser::PNParser.new.parse(pn_result)

        options = ctx.parser_options
        options['epp'] = true if epp
        actual, issues = parse_and_validate(path, source, options)
        error = issues.find { |issue| issue.severity == :error }
        assertions.assert_equal(nil, Puppet::PSpec.formatter.format(error)) unless error.nil?

        # Automatically strip off blocks that contain one statement
        actual = actual.body if actual.is_a?(Puppet::Pops::Model::Program)
        actual = actual.statements[0] if actual.is_a?(Puppet::Pops::Model::BlockExpression) && actual.statements.size == 1

        actual_pn = Puppet::Pops::Model::PNTransformer.transform(actual)
        assertions.assert_equal(expected_pn.to_s, actual_pn.to_s)
      end
    end
  end

  class ExpectedIssues < Result
    def validate_expectations(assertions, expectations, diagnostics, log_entries)
      bld = ''
      expectations.each { |e| e.match_entries(bld, log_entries, diagnostics) }
      diagnostics.each { |d| bld << "Unexpected #{d.severity}(#{d.issue_code})\n" unless expectations.any? { |e| e.is_diagnostic_matched?(d) } }
      log_entries.each { |l| bld << "Unexpected #{l.severity}(#{l.issue_code})\n" unless expectations.any? { |e| e.is_log_entry_matched?(l) } }
      if bld.size > 0
        assertions.fail(bld)
      end
    end
  end

  class Validates_with < ExpectedIssues
    def self.create_new_function(t)
      impl_class = self
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        @impl_class = impl_class

        def self.create(args)
          @impl_class.new(args)
        end

        dispatch :create do
          repeated_param 'Expectation', :args
        end

        def create(*args)
          self.class.create(args)
        end
      end
    end

    attr_reader :expectations
    def initialize(expectations)
      @expectations = expectations
    end

    def create_test(actual)
      path, source, epp = path_content_epp(actual)
      -> (ctx, assertions) do
        source = ctx.resolve_lazy_value(source)
        error(VALUE_NOT_STRING, :name => 'Source', :type => source.class.name) unless source.is_a?(String)
        options = ctx.parser_options
        options['epp'] = true if epp
        _, issues = parse_and_validate(path, source, options)
        validate_expectations(assertions, @expectations, issues, [])
      end
    end
  end

  # Lazy values

  class Get < Element
    attr_reader :key
    def initialize(key)
      @key = key
    end

    def get(tc)
      lv = tc.get_lazy_value(@key)
      error(GET_OF_UNKNOWN_VARIABLE, :name => key) if lv.nil?
      tc.get(lv)
    end
  end

  class Let < Element
    attr_reader :key, :value
    def initialize(key, value)
      @key = key
      @value = value
    end
  end

  # Lazy value. Abstract base class for things like File and Directory
  #
  class LazyValue < Element
    # @param ctx [Context] the test context
    # @return [Object] the result of evaluating the lazy value
    def get(ctx)
      ctx.to_s
    end
  end

  # Example

  # Node is the common abstract base class for {Example} and {Examples}
  class Node < Element
    attr_reader :description, :inputs, :values
    def initialize(description, values, inputs)
      @description = description
      @inputs = inputs
      @values = values
    end

    def collect_inputs(ctx, inputs)
      pc = ctx.parent
      pc.node.collect_inputs(pc, inputs) unless pc.nil?
      inputs.concat(@inputs)
      inputs
    end
  end

  class Example < Node
    def self.create_new_function(t)
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        dispatch :create do
          param 'String', :description
          repeated_param 'Variant[Let,Given,Result]', :args
        end

        def create(description, *args)
          values = []
          inputs = []
          results = []
          args.each do |arg|
            if arg.is_a?(Given)
              inputs.concat(arg.inputs)
            elsif arg.is_a?(Let)
              values << arg
            else
              results << arg
            end
          end
          Example.new(description, values, inputs, results)
        end
      end
    end

    attr_reader :results
    def initialize(description, values, inputs, results)
      super(description, values, inputs)
      @results = results
    end

    def create_test
      PSpecExecutable.new(self, ->(ctx, assertions) do
        tests = []
        collect_inputs(ctx, []).each { |input| @results.each { |result| tests.concat(input.create_tests(result)) } }
        tests.each { |test| test.call(ctx, assertions) }
      end)
    end
  end

  class Examples < Node
    def self.create_new_function(t)
      Puppet::Functions.create_loaded_function(:"new_#{t.name}", t.loader) do
        dispatch :create do
          param 'String', :description
          repeated_param 'Variant[Let,Given,Example,Examples]', :args
        end

        def create(description, *args)
          values = []
          inputs = []
          children = []
          args.each do |arg|
            if arg.is_a?(Given)
              inputs.concat(arg.inputs)
            elsif arg.is_a?(Let)
              values << arg
            else
              children << arg
            end
          end
          Examples.new(description, values, inputs, children)
        end
      end
    end

    def create_test
      PSpecGroup.new(self, @children.map { |child| child.create_test })
    end

    attr_reader :children
    def initialize(description, values, inputs, children)
      super(description, values, inputs)
      @children = children
    end
  end

  class IssueCode < Element
    attr_reader :code
    def initialize(code)
      @code = code
    end

    def to_s
      @code
    end
  end
end
