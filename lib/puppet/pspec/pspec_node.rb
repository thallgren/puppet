module Puppet::PSpec
  class PSpecNode
    attr_reader :node
    def initialize(node)
      @node = node
    end

    def needs_compiler?
      false
    end
  end

  class PSpecExecutable < PSpecNode
    attr_reader :test_function
    def initialize(node, test_function, needs_compiler = false)
      super(node)
      @test_function = test_function
      @needs_compiler = needs_compiler
    end

    def needs_compiler?
      @needs_compiler
    end

    def run(ctx, assertions)
      @test_function.call(ctx, assertions)
      nil
    end
  end

  class PSpecGroup < PSpecNode
    attr_reader :tests
    def initialize(node, tests)
      super(node)
      @tests = tests
    end

    def needs_compiler?
      @tests.any? { |t| t.needs_compiler? }
    end
  end
end
