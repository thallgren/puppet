module Puppet::PSpec
  class MochaGlue
    include TestFramework

    def initialize(mocha)
      @mocha = mocha
    end

    def context(label, &block)
      @mocha.context(label, &block)
    end

    def it(label, &block)
      @mocha.it(label, &block)
    end

    def assert_equal(expected, actual)
      @mocha.expect(actual).to @mocha.eql(expected)
    end

    def fail(message)
      @mocha.fail(message)
    end
  end
end
