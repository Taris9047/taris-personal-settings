#!/usr/bin/env ruby

require 'etc'
require 'open3'

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

class InstClang < InstallStuff

  def initialize(prefix, def_system, work_dirs, need_sudo)
    super('clang', prefix, work_dirs)
    @def_system = def_system

  end

  def find_python3 (given_path='/usr/local')
    bin_path = File.join(given_path, 'bin')
    py3_exe = File.join(bin_path, 'python3')

    # Python3 default fallback
    unless File.file?(py3_exe)
      py3_exe = File.realpath("/usr/bin/python3")
    end

    return py3_exe
  end

  def install_clang
    puts ""
    puts "Working on Clang!!"
    puts ""

    url = SRC_URL.new()
    @src_url = url['llvm']

    if self.CheckInfo
      return 0
    end

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
      inst_cmd = "sudo make install"
    else
      inst_cmd = "make install"
    end

    # setup correct python path
    py3_path = find_python3(@prefix)
    py3_exe_option = [
      "-DPYTHON_EXECUTABLE:FILEPATH=\"",
      py3_path,
      "\"" ].join('')

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
      "-DCMAKE_BUILD_TYPE=Release",
      "-DLLVM_BUILD_DOCS=OFF",
      "-DLIBCXX_CXX_ABI=libstdc++",
    ]

    cmd = [
    	"cd",
    	@build_dir,
    	"&&",
    	"cmake",
    	inst_prefix_opt,
    	py3_exe_option,
    	comp_settings.join(' '),
    	File.join(@src_dir, "llvm"),
    	"&&",
    	"make",
    	"&&",
    	inst_cmd
    ]

    self.Run( cmd.join(" ") )

    @conf_options = [inst_prefix_opt,py3_exe_option]+cmake_opts+comp_settings

    self.WriteInfo

  end

end # InstClang
