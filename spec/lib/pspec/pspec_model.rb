module Puppet::PSpec
  def init_mappings
    Puppet::Pops::Loaders.implementation_registry.register_implementation_namespace('PSpec', 'Puppet::PSpec')
  end

  def self.hard_issue(issue_code, *args, &block)
    Puppet::Pops::Issues::hard_issue(issue_code, *args, &block)
  end

  # PSpec issues
  VALUE_NOT_HASH = hard_issue :VALUE_NOT_HASH, :type do
    _('%{type} does not contain a Hash') % { type: type }
  end

  def error(issue_code, args)

  end

  def leaf_name
    self.class.name.split(/::/).last
  end

  class LazyValue

  end

  class Get < LazyValue

  end

  class LazyScopeCreator
    def initialize(ctx)
      @ctx = ctx
    end

    # Note that this is a #new method on the instance, not the class
    def new(parent)
      LazyScope.new(ctx, parent)
    end
  end

  class LazyScope < Puppet::Parser::Scope::LocalScope
    def initialize(ctx, parent=nil)
      super(parent)
      @ctx = ctx
    end

    def bound?(name)
      @ctx.get_lazy_value(name).nil? ? super : true
    end

    def [](name)
      lv = @ctx.get_lazy_value(name)
      lv.nil? ? super : @ctx.get(lv)
    end
  end

  # Context
  class TestContext
    def initialize(parent, node)
      @accessed_values = {}
      @parent = parent
      @parser_options = nil
      @node = node
      @teardowns = []
    end

    def run_test(test, assertions)
      test.needs_compiler? ? run_eval_test(test, assertions) : run_simple_test(test, assertions)
    end

    def run_eval_test(test, tf)
      node = Puppet::Node.new('testnode'))
      compiler = Puppet::Parser::Compiler.new(node)
      node.environment.check_for_reparse
      logs = []
      Puppet::Util::Log.with_destination(Puppet::Test::LogCollector.new(logs)) do
        yield(compiler)
      end

      logs = logs.select { |log| log.level == :notice }.map { |log| log.message }
       scope.with_local_scope({}, LazyScopeCreator.new(self)) do |local_scope|
        @local_scope = local_scope
        t.run(self, tf)
        @local_scope = nil
      end
    end

    def run_parser_test(test, tf)
      Puppet::Pal.in_tmp_environment('pal_env', modulepath: [], facts: Puppet::Pops::EMPTY_HASH) do |ctx|
        @pal_context = ctx

      end

      scope.with_local_scope({}, LazyScopeCreator.new(self)) do |local_scope|
        @local_scope = local_scope
        t.run(self, tf)
        @local_scope = nil
      end
    end

    def parser_options
      opts = @parent.nil? ? {} : @parent.parser_options
      opts = opts.merge(@parser_options) unless @parser_options.nil?
      opts
    end

    def resolve_lazy_value(v)
      if v.is_a?(LazyValue)
        get(v)
      elsif v.is_a?(LazyValueGet)
        v.get(self)
      elsif v.is_a?(Hash)
        h = {}
        v.each_pair { |k, e| h[k] = resolve_lazy_value(e) }
        h
      elsif v.is_a?(Array)
        v.map { |e| resolve_lazy_value(e) }
      else
        v
      end
    end

    def scope
      @local_scope
    end

    def get(lv)
      @accessed_values.fetch(lv.object_id) do |key|
        v = lv.get(self)
        @accessed_values[key] = v
        v
      end
    end

    def get_lazy_value(key)
      @node.get(key) || (@parent.nil? ? nil : @parent.get_lazy_value(key))
    end

    def register_tear_down(func)
      @teardowns << func
    end
  end

  # Inputs

  # Abstract base class for all Input classes
  class Input
    include Puppet::PSpec

    def create_tests(expectation)
      raise Puppet::Error('derived must implement')
    end
  end

  class Scope < Input
    def initialize(scope)
      @scope = scope
    end

    def create_tests(expectation)
      [ ->(testContext, _) do
        scope = testContext.resolve_lazy_value(@scope)
        error(VALUE_NOT_HASH, :type => leaf_name) unless scope.is_a?(Hash)
        testContext.scope = scope
      end ]
    end
  end

  class Settings < Input
    def initialize(settings)
      @settings = settings
    end

    def create_tests(expectation)
      [ ->(testContext, _) do
        settings = testContext.resolve_lazy_value(@settings)
        error(VALUE_NOT_HASH, :type => leaf_name) unless settings.is_a?(Hash)
        settings.each_pair { |k, v| Puppet[k] = v }
      end ]
    end
  end

  class Source < Input
    def initialize(sources)
      @sources = sources
    end

    def create_tests(expectation)
      @sources.map do |s|
        s = testContext.resolve_lazy_value(s)
        error(VALUE_NOT_STRING, :type => leaf_name) unless s.is_a?(String)
        expectation.create_test(:source => s, :epp => epp)
      end
    end

    def epp
      false
    end
  end

  class EppSource < Source
    def epp
      true
    end
  end

  class Result
    def create_test(actual)
      raise Puppet::Error('derived must implement')
    end

    def path_content_epp(actual)
      case actual
      when Pspec::Puppet::Named_source
      when Array
      else

      end
    end
  end

class ParseResult < Result
  def create_test(actual)
    path, source, epp = path_and_content(actual)
    raise Puppet::Error('derived must implement')
  end
end
end
=begin
type Pspec = TypeSet[{
  types => {
    Get => {
      attributes => {
        key => String
      }
    },
    Let => {
      attributes => {
        key => String,
        value => Any
      }
    },
    Input => {},
    Given => {
      attributes => {
        inputs => Array[Input]
      }
    },
    Result => {},
    Contain => {
      attributes => {
        match => Array[Variant[Get,String,Regexp]]
      }
    },
    Node => {
      attributes => {
        description => String,
        given => Given
      }
    },
    Example => {
      parent => Node,
      attributes => {
        results => Array[Result]
      }
    },
    Examples => {
      parent => Node,
      attributes => {
        children => Array[Node]
      }
    },
    Source => {
      parent => Input,
      attributes => {
        sources => Array[Variant[Get,String]]
      }
    },
    Epp_source => {
      parent => Source
    },
    Named_source => {
      parent => Input,
      attributes => {
        name => String,
        source => Variant[Get,String]
      }
    },
    Scope => {
      parent => Input,
      attributes => {
        entries => Variant[Get,Hash[Pattern[/\A[a-z_]\w*\z/],Any]]
      }
    },
    Settings => {
      parent => Input,
      attributes => {
        entries => Variant[Get,Hash[Pattern[/\A[a-z_]\w*\z/],Any]]
      }
    },
    Match => {
      attributes => {
        match => Array[Variant[Get,String,Regexp]]
      }
    },
    Issue => {
      attributes => {
        code => String,
        match => Variant[Get,Match,Contain,String,Regexp]
      }
    },
    Exclude => {
      attributes => {
        matches => Array[Variant[Get,Issue,Match,Contain,String,Regexp]]
      }
    },
    Include => {
      attributes => {
        matches => Array[Variant[Get,Issue,Match,Contain,String,Regexp]]
      }
    },
    Expectation => {
      attributes => {
        includes => Array[Include],
        excludes => Array[Exclude],
        level => { type => Enum[error,notice,warning], kind => derived }
      }
    },
    Error => { parent => Expectation },
    Notice => { parent => Expectation },
    Warning => { parent => Expectation },
    Evaluates_to => {
      parent => Result,
      attributes => {
        result => Any
      }
    },
    Evaluates_with => {
      parent => Result,
      attributes => {
        result => Array[Expectation]
      }
    },
    Parses_to => {
    parent => Result,
      attributes => {
        pn_result => String
      }
    },
    Parses_with => {
      parent => Result,
      attributes => {
        result => Array[Expectation]
      }
    },
  }
}]
=end