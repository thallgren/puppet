type PSpec = TypeSet[{
  pcore_version => '1.0.0',

  types => {
    Contain => {
      parent => Match
    },
    Epp_source => {
      parent => Source
    },
    Error => { parent => Expectation },
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
    Exclude => {
      attributes => {
        matches => Array[Variant[Issue,IssueCode,Match,Contain,String,Regexp]]
      }
    },
    Expectation => {
      attributes => {
        includes => Array[Include],
        excludes => Array[Exclude],
        level => { type => Enum[error,notice,warning], kind => derived }
      }
    },
    Get => {
      attributes => {
        key => String
      }
    },
    Given => {
      attributes => {
        inputs => Array[Variant[String,Input]]
      }
    },
    Let => {
      attributes => {
        key => String,
        value => Any
      }
    },
    Include => {
      attributes => {
        matches => Array[Variant[Issue,IssueCode,Match,Contain,String,Regexp]]
      }
    },
    Input => {},
    Issue => {
      attributes => {
        code => IssueCode,
        match => Hash[String[1], Variant[Match,Contain,String,Regexp]]
      }
    },
    IssueCode => {
      attributes => {
        code => String
      }
    },
    Match => {
      attributes => {
        match => Array[Variant[String,Regexp]]
      }
    },
    Named_source => {
      parent => Input,
      attributes => {
        name => String,
        source => Variant[Get,String]
      }
    },
    Node => {
      attributes => {
        description => String,
        values => Array[Let],
        inputs => Array[Input]
      }
    },
    Notice => { parent => Expectation },
    Parser_options => {
      parent => Input,
      attributes => {
        options => Variant[Get,Hash[Pattern[/\A[a-z_]\w*\z/],Any]]
      }
    },
    Parses_to => {
      parent => Result,
      attributes => {
        pn_result => String
      }
    },
    Validates_with => {
      parent => Result,
      attributes => {
        result => Array[Expectation]
      }
    },
    Result => {},
    Scope => {
      parent => Input,
      attributes => {
        scope => Variant[Get,Hash[Pattern[/\A[a-z_]\w*\z/],Any]]
      }
    },
    Settings => {
      parent => Input,
      attributes => {
        settings => Variant[Get,Hash[Pattern[/\A[a-z_]\w*\z/],Any]]
      }
    },
    Source => {
      parent => Input,
      attributes => {
        sources => Array[Variant[Get,String]]
      }
    },
    Warning => { parent => Expectation },
  }
}]
