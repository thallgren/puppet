require 'spec_helper'
require 'puppet_spec/pspec'

context('validator.pspec') { PuppetSpec::PSpec.run_pspec(self, my_fixture('validator.pspec')) }
