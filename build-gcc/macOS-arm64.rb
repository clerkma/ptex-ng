MRuby::CrossBuild.new('arm64') do |conf|
  conf.toolchain :clang

  conf.gembox 'full-core'
  conf.cc.flags << "-arch arm64"
  conf.linker.flags << "-arch arm64"
end
