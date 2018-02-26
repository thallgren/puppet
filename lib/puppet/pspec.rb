module Puppet::PSpec
  def self.hard_issue(issue_code, *args, &block)
    Puppet::Pops::Issues::hard_issue(issue_code, *args, &block)
  end

  # PSpec issues
  #
  GET_OF_UNKNOWN_VARIABLE = hard_issue :GET_OF_UNKNOWN_VARIABLE, :name do
    _("Get of unknown variable named '%{name}'") % { name: name }
  end

  VALUE_NOT_HASH = hard_issue :VALUE_NOT_HASH, :type do
    _('%{type} does not contain a Hash') % { type: type }
  end

  VALUE_NOT_STRING = hard_issue :VALUE_NOT_HASH, :name, :type do
    _('%{name} must be a String. Got %{type}') % { name: name, type: type }
  end

  def self.formatter
    @formatter ||= Puppet::Pops::Validation::DiagnosticFormatterPuppetStyle.new
  end

  def self.init(scope)
    loaders = scope.compiler.loaders
    loader = loaders.find_loader(nil)
    loaders.implementation_registry.register_implementation_namespace('PSpec', 'Puppet::PSpec', loader)
    ts_file = File.realpath(File.join(File.dirname(__FILE__), 'pspec', 'pspec.pp'))
    ts_code = Puppet::FileSystem.read(ts_file, :encoding => 'utf-8')
    tn = Puppet::Pops::Loader::TypedName.new(:type, 'pspec')
    pspec_typeset = Puppet::Pops::Loader::TypeDefinitionInstantiator.create(loader, tn, ts_file, ts_code)
    loader.set_entry(tn, pspec_typeset)

    @evaluator = Evaluator.new(pspec_typeset.resolve(loader).types)
    @parser = Puppet::Pops::Parser::EvaluatingParser.new(:backtick_strings => true)
    nil
  end

  def self.load_examples(scope, pspec_file)
    examples = @evaluator.evaluate_pspec(@parser.parse_file(pspec_file), scope)
    examples.map { |e| e.create_test }
  end

  def self.run_examples(tests, test_framework)
    tests.each { |pspec_test| Context.run_test(pspec_test, nil, test_framework) }
    nil
  end

  require 'puppet/pops/pn'

  require 'puppet/pspec/context'
  require 'puppet/pspec/evaluator'
  require 'puppet/pspec/model_impl'
  require 'puppet/pspec/test_framework'
  require 'puppet/pspec/pspec_node'
end
