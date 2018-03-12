module Puppet::PSpec
  class Evaluator < Puppet::Pops::Evaluator::EvaluatorImpl
    attr_reader :examples

    def self.eval_visitor
      @eval_visitor ||= Puppet::Pops::Visitor.new(nil, 'eval', 1, 1)
    end

    def initialize(name_mappings)
      @eval_visitor = self.class.eval_visitor
      @name_mappings = name_mappings
      @examples = []
    end

    def evaluate_pspec(pspec_ast, scope)
      @examples = []
      evaluate(pspec_ast, scope)
      examples
    end

    def evaluate(target, scope)
      @eval_visitor.visit_this_1(self, target, scope)
    end

    # Evaluates all statements and produces the last evaluated value
    #
    def eval_BlockExpression(o, scope)
      r = nil
      o.statements.each do |s|
        r = evaluate(s, scope)
        @examples << r if r.is_a?(Puppet::PSpec::Node)
      end
      r
    end

    def eval_QualifiedReference(o, scope)
      cn = o.cased_value
      pn = @name_mappings[cn]
      return pn unless pn.nil?
      cn =~ /^[A-Z][A-Z_]*$/ ? IssueCode.new(cn) : super
    end
  end
end
