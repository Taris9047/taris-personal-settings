#!/usr/bin/env ruby

# this will handle Node.js with NPM

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

$conf_options = [
  "--shared-zlib"
]

class InstNode < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super('node', prefix, work_dirs, verbose_mode=verbose_mode)

    @source_url = SRC_URL[@pkgname]
    @need_sudo = need_sudo
    @PythonCmd = "python3"
    @work_dirs = work_dirs
    @need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix, 'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @env = gc.get_env_settings

    @conf_options = $conf_options

  end

  def install

    self.GetSrcVer
    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

    if self.CheckInfo
      return 0
    end

    puts "Downloading source from ... "+@source_url
    dl = Download.new(@source_url, @src_dir)

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    src_extract_folder = File.join(@build_dir, src_tarball_bname)

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
      puts "Deleting it"
      self.Run( ['rm -rf', src_extract_folder].join(' ') )
    end
    puts "Extracting..."
    self.Run( "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )

    opts = ["--prefix="+@prefix]+@conf_options

    if @need_sudo
      inst_cmd = "sudo make install"
    else
      inst_cmd = "make install"
    end

    @env['CC'] = 'gcc'
    @env['CXX'] = 'g++'

    # Ok let's rock!
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    cmds = [
      "cd", src_extract_folder, "&&",
      File.join(src_extract_folder,"configure"),
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]
    self.Run( @env, cmds.join(" ") )

    self.WriteInfo

    puts "Let's install package manger!"
    self.Run( 'npm install -g n' )
    puts "Now you can use 'n' to manage Node.JS versions!"

  end # install

end # class InstNode



# Class InstNodeLTS
class InstNodeLTS < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super('node-lts', prefix, work_dirs, verbose_mode=verbose_mode)

    @source_url = SRC_URL[@pkgname]
    @need_sudo = need_sudo
    @PythonCmd = "python3"
    @work_dirs = work_dirs
    @need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix, 'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @env = gc.get_env_settings

    @conf_options = $conf_options

  end

  def install

    self.GetSrcVer
    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

    if self.CheckInfo
      return 0
    end

    puts "Downloading source from ... "+@source_url
    dl = Download.new(@source_url, @src_dir)

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    src_extract_folder = File.join(@build_dir, src_tarball_bname)

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
      puts "Deleting it"
      self.Run( ['rm -rf', src_extract_folder].join(' ') )
    end
    puts "Extracting..."
    self.Run( "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )

    opts = ["--prefix="+@prefix]+@conf_options

    if @need_sudo
      inst_cmd = "sudo make install"
    else
      inst_cmd = "make install"
    end

    @env['CC'] = 'gcc'
    @env['CXX'] = 'g++'

    # Ok let's rock!
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    cmds = [
      "cd", src_extract_folder, "&&",
      File.join(src_extract_folder,"configure"),
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]
    self.Run( @env, cmds.join(" ") )

    self.WriteInfo

  end # install

end # class InstNode