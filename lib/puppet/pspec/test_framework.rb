module Puppet::PSpec
  module TestFramework

    # Factory method to create a test framework glue. Only Mocha is supported for now
    def self.create(native_framework)
      MochaGlue.new(native_framework)
    end
  end
end

require_relative 'mocha_glue'
