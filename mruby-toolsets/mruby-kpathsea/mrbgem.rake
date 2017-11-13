MRuby::Gem::Specification.new('mruby-kpathsea') do |spec|
  spec.authors = 'Clerk Ma'
  spec.summary = 'Kpathsea Binding'
  spec.linker do |linker|
    linker.libraries = ['kpathsea']
  end
end
