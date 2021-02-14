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
    super('clang', prefix, work_dirs, verbose_mode=verbose_mode)
    @def_system = def_system

  end

  def install
    
    puts ""
    puts "Working on LLVM-Clang!! (git)"
    puts ""

    @src_url = SRC_URL['llvm']

    # if self.CheckInfo
    #   return 0
    # end

    # unless File.file?(File.join(@pkginfo_dir, 'gccold.info'))
    #   puts "Looks like we need to install gccold!!"
    #   require './install_gcc.rb'
    #   inst_gcc = InstGCCOld.new(@prefix, @def_system, @work_dirs, @need_sudo)
    #   inst_gcc.install
    # end

    dn = Download.new(@src_url, destination=@src_dir, source_ctl='git')
    @src_dir = dn.GetPath

    # Let's build!!
    @build_dir = File.join(@build_dir, "llvm-clang-build")
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

    config_cmd = [
    	"cd",
    	@build_dir,
    	"&&",
    	"cmake",
    	inst_prefix_opt,
        cmake_opts.join(' '),
    	comp_settings.join(' '),
    	File.join(@src_dir, "llvm"),
    ]

    compile_cmd = [
      "cd",
      @build_dir,
      "&&",
      "ninja",
      "&&",
      inst_cmd
    ]

    puts "Configuring with cmake"
    self.Run( config_cmd.join(' ') )

    # Fetching version info.
    ver_text = ''
    fp = File.open(File.join(@build_dir, 'llvm.spec'), 'r')
    llvm_spec = fp.readlines
    for l in llvm_spec
      if l.include?('Version:')
        ver_text = l.split(' ')[-1]
        break
      end
    end
    ver_text.delete! 'git'
    @Version = ver_text.split('.')
    puts ""
    puts "LLVM-Clang version detected: "+ver_text
    puts ""

    # self.Run( cmd.join(" ") )

    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.Run( compile_cmd.join(' ') )

    @conf_options = [inst_prefix_opt]+cmake_opts+comp_settings

    self.WriteInfo

  end

  def WriteInfo
    puts "Writing package info for #{@pkgname}..."
    fp = File.open(@pkginfo_file, 'w')
    compile_info_json = {
      "Package Name" => @pkgname,
      "Version" => @Version,
    }
    fp.write(compile_info_json.to_json)
    fp.close
  end

end # InstClang
