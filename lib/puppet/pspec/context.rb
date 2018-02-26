module Puppet::PSpec
  # Context
  class Context
    def self.run_test(pspec_test, parent_context, test_framework)
      new(parent_context, pspec_test).run_test(test_framework)
    end

    attr_reader :parent
    def initialize(parent, pspec_test)
      @parent = parent
      @pspec_test = pspec_test
      @accessed_values = {}
      @parser_options = nil
      @teardowns = []
    end

    def add_parser_options(options)
      if @parser_options.nil?
        @parser_options = options
      else
        @parser_options = @parser_options.merge(options)
      end
      nil
    end

    def run_test(test_framework)
      # NOTE! Test framework swaps self inside the block. One side effect of that is that references to attributes
      # are relative to the test framework. Therefore, the pspec_test and ctx_self must be set here
      pspec_test = @pspec_test
      ctx_self = self

      if pspec_test.is_a?(PSpecExecutable)
        test_framework.it(node.description) { pspec_test.run(ctx_self, TestFramework.create(self)) }
      else
        test_framework.context(node.description) { pspec_test.tests.each { |t| Context.run_test(t, ctx_self, TestFramework.create(self)) } }
      end
      nil
    end

    def get(lv)
      @accessed_values.fetch(lv.object_id) do |key|
        v = lv.get(self)
        @accessed_values[key] = v
        v
      end
    end

    def get_lazy_value(key)
      node.get(key) || (@parent.nil? ? nil : @parent.get_lazy_value(key))
    end

    def node
      @pspec_test.node
    end

    def parser_options
      opts = @parent.nil? ? {} : @parent.parser_options
      opts = opts.merge(@parser_options) unless @parser_options.nil?
      opts
    end

    def register_tear_down(func)
      @teardowns << func
      nil
    end

    def resolve_lazy_value(v)
      if v.is_a?(LazyValue)
        get(v)
      elsif v.is_a?(Get)
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
  end
end
