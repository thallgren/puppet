Puppet.features.add :jlexer do
  begin
    if RUBY_PLATFORM =~ /java/
      require 'jlexer'
      true
    else
      false
    end
  rescue LoadError
    false
  end
end
