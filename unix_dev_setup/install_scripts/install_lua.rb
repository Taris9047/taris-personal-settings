#!/usr/bin/env ruby

# Installs Lua

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

class InstLua < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super('lua', prefix, work_dirs, ver_check=true, verbose_mode=verbose_mode)
    @need_sudo = need_sudo

    @source_url = SRC_URL[@pkgname]

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @CompilerSettings = gc.get_settings
    @env = gc.get_env_settings
    @conf_options = []
  end

  def install
    
    self.GetSrcVer
    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

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

    puts "Installing Lua!!"
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
