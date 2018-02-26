module Puppet::PSpec
  class Element
    include Puppet::Pops::Types::PuppetObject

    def self._pcore_type
      @type ||= Puppet::Pops::Types::TypeParser.singleton.parse("PSpec::#{name.split(/::/).last}")
    end

    def error(issue, args)
      fail_with(issue.format(args))
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

  # Results

  # Result is the abstract base class for the Evaluates_xxx and Parses_xxx family of classes
  class Result < Element
    def create_test(actual)
      raise Puppet::Error('derived must implement')
    end

    def parse_and_validate(path, source, options)
      Puppet[:tasks] = !!options['tasks']
      pc = options['epp'] ? Puppet::Pops::Parser::EvaluatingParser::EvaluatingEppParser : Puppet::Pops::Parser::EvaluatingParser
      epc = pc.new

      result = epc.parser.parse_string(source, path)
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
  end
end
