require 'spec_helper'
require 'puppet_spec/pspec'

context('parser.pspec') { PuppetSpec::PSpec.run_pspec(self, my_fixture('parser.pspec')) }
context('lexer_errors.pspec') { PuppetSpec::PSpec.run_pspec(self, my_fixture('lexer_errors.pspec')) }
