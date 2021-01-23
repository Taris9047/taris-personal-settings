#!/usr/bin/env ruby

require 'etc'
require 'open3'

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'


$projects_to_enable = [
  'clang',
  'compiler-rt',
  'clang-tools-extra',
  'openmp',
  'lld',
]

class InstClang < InstallStuff

  def initialize(prefix, def_system, work_dirs, need_sudo, verbose_mode=false)
    super('clang', prefix, work_dirs, , verbose_mode=verbose_mode)
    @def_system = def_system

  end

  def install_clang
    puts ""
    puts "Working on Clang!!"
    puts ""

    @src_url = SRC_URL['llvm']

    # if self.CheckInfo
    #   return 0
    # end

    unless File.file?(File.join(@pkginfo_dir, 'gccold.info'))
      puts "Looks like we need to install gccold!!"
      require './install_gcc.rb'
      inst_gcc = InstGCCOld.new(@prefix, @def_system, @work_dirs, @need_sudo)
      inst_gcc.install
    end

    dn = Download.new(@src_url, destination=@src_dir, source_ctl='git')
    @src_dir = dn.GetPath

    # Let's build!!
    @build_dir = File.join(@build_dir, "llvm-clang")
    if Dir.exists?(@build_dir) == false
      puts "Build dir missing.. making one.."
    else
      puts "Build dir exists, cleaning up before work!!"
      self.Run( "rm -rf "+@build_dir )
    end
    self.Run( "mkdir -p "+@build_dir )

    if @need_sudo
      inst_cmd = "sudo ninja install"
    else
      inst_cmd = "ninja install"
    end

    # Setting up compilers
    compiler_path = File.join(@prefix, 'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    comp_settings = gc.get_cmake_settings
    @env = gc.get_env_settings

    # Some last minute changes.
    comp_settings[0] = '-DCMAKE_C_COMPILER=gcc-old'
    comp_settings[1] = '-DCMAKE_CXX_COMPILER=g++-old'

    # Setting up install prefix
    inst_prefix_opt = [ "-DCMAKE_INSTALL_PREFIX:PATH=#{@prefix}" ]

    cmake_opts = [
      "-Wno-dev",
      "-G Ninja",
      "-DLLVM_ENABLE_PROJECTS=\"#{$projects_to_enable.join(';')}\"",
      "-DLLVM_ENABLE_FFI=ON",
      "-DLLVM_BUILD_LLVM_DYLIB=ON",
      "-DLLVM_LINK_LLVM_DYLIB=ON",
      "-DLLVM_ENABLE_RTTI=ON",
      "-DLLVM_TARGETS_TO_BUILD=\"host\"",
      "-DCMAKE_BUILD_TYPE=Release",
    ]

    cmd = [
    	"cd",
    	@build_dir,
    	"&&",
    	"cmake",
    	inst_prefix_opt,
        cmake_opts.join(' '),
    	comp_settings.join(' '),
    	File.join(@src_dir, "llvm"),
    	"&&",
    	"ninja",
    	"&&",
    	inst_cmd
    ]

    # self.Run( cmd.join(" ") )

    system( cmd.join(' ') )

    @conf_options = [inst_prefix_opt]+cmake_opts+comp_settings

    self.WriteInfo

  end

end # InstClang
