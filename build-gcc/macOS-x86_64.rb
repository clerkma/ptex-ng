MRuby::CrossBuild.new('x86_64') do |conf|
  conf.toolchain :clang

  conf.gembox 'full-core'
  conf.cc.flags << "-arch x86_64"
  conf.linker.flags << "-arch x86_64"
end
