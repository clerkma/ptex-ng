MRuby::Gem::Specification.new('mruby-ptexec') do |spec|
  spec.authors = 'Clerk Ma'
  spec.summary = 'PTEXENC Binding'
  spec.linker do |linker|
    linker.libraries = ['ptexenc', 'kpathsea']
  end
end
