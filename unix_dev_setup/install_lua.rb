#!/usr/bin/env ruby

# Installs Lua

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'

$lua_src_url = "https://www.lua.org/ftp/lua-5.4.0.tar.gz"


class InstLua < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false)
    super('lua', prefix, work_dirs)
    @need_sudo = need_sudo

    @source_url = $lua_src_url

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @CompilerSettings = gc.get_settings
    @env = gc.get_env_settings
    @conf_options = []
  end

  def install

    if self.CheckInfo
      return 0
    end

    dl = Download.new(@source_url, @src_dir)
    # src_tarball_path = dl.GetPath

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@build_dir), src_tarball_bname)

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      self.Run(
        "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )
    end

    if @need_sudo
      inst_cmd = "sudo make INSTALL_TOP=\""+@prefix+"\" install"
    else
      inst_cmd = "make INSTALL_TOP=\""+@prefix+"\" install"
    end

    # Ok let's roll!!
    cmds = [
      "cd", src_extract_folder, "&&",
      "make "+@CompilerSettings.join(" ")+" linux",
      "&&",
      inst_cmd
    ]

    self.Run( cmds.join(" ") )

    self.WriteInfo
  end

end # class InstLua
