#!/usr/bin/env ruby

require 'etc'
require 'open3'

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

$root_version = ["6", "22", "06"]


class InstROOT < InstallStuff

  def initialize(prefix, def_system, work_dirs, need_sudo, verbose_mode=false)
    super('ROOT', prefix, work_dirs, verbose_mode=verbose_mode)
    @def_system = def_system

  end

  def install

    puts ""
    puts "Working on ROOT!! (git)"
    puts ""

    @src_url = SRC_URL['ROOT']
    branch_opts = '--branch v#{$root_version[0]}-#{$root_version[1]}-#{$root_version[2]}'
    dn = Download.new(@src_url, destination=@src_dir, source_ctl='git', srouce_ctl_opts=branch_opts)
    @src_dir = dn.GetPath

    # Let's build!!
    @build_dir = File.join(@build_dir, "ROOT-build")
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

    # Setting up compilers
    compiler_path = File.join(@prefix, 'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    comp_settings = gc.get_cmake_settings
    # @env = gc.get_env_settings
	# @env = { "LD_LIBRARY_PATH" => "-Wl,-rpath=#{@prefix}/lib -Wl,-rpath=#{@prefix}/lib64" }
	@env = {}

    # Setting up install prefix
    inst_prefix_opt = [ "-DCMAKE_INSTALL_PREFIX:PATH=#{@prefix}" ]

    py_src = SRC_URL['python3']
    fnp = FNParser.new(py_src)
    py_ver = fnp.version
    cmake_opts = [
      "-DCMAKE_BUILD_TYPE=Release",
      "-DLLVM_BUILD_TYPE=Release",
      "-DPYTHON_EXECUTABLE=#{@prefix}/bin/python#{py_ver[0]}",
	  "-Drpath=ON",
    ]

    config_cmd = [
      "cd",
      @build_dir,
      "&&",
      "cmake",
      @src_dir,
      inst_prefix_opt,
      cmake_opts.join(' '),
      comp_settings.join(' '),
    ]

    compile_cmd = [
      "cd",
      @build_dir,
      "&&",
      "make -j #{@Processors}",
      "&&",
      inst_cmd
    ]

    puts "Configuring with cmake"
    self.Run( @env, config_cmd.join(' ') )

    @Version = $root_version

    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.Run( @env, compile_cmd.join(' ') )

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
