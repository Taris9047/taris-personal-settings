#!/usr/bin/env ruby

# this will handle Ruby 3.0.0 installation

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

$gems_to_install = [
    "rsense",
    "rails",
    "bundler",
    "open3",
    "json",
    "hjson",
  ]

$pkg_name = 'ruby3'

class InstRuby3 < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super($pkg_name, prefix, work_dirs,ver_check=true, verbose_mode=verbose_mode)

    @source_url = SRC_URL[@pkgname]

    # Ruby3 modules to install
    @ruby_gems = $gems_to_install

    # Ruby3 build options
    @conf_options = [
      "--enable-shared",
      "--program-suffix=3"
    ]
    @need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(
      cc_path=compiler_path, 
      cxx_path=compiler_path, 
      cflags='-fno-semantic-interposition', 
      cxxflags='-fno-semantic-interposition',
      clang=false)
    @env = gc.get_env_settings

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
    src_tarball_path = dl.GetPath

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      self.Run( "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      self.Run( "rm -rfv "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    self.Run( "mkdir -p "+src_build_folder )

    opts = ["--prefix="+@prefix]+@conf_options

    if @need_sudo
      inst_cmd = "sudo make install"
      mod_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      mod_sudo = ""
    end

    # Ok let's roll!!
    cmds = [
      "cd", src_build_folder, "&&",
      src_extract_folder+"/configure",
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]

    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.Run( @env, cmds.join(" ") )

    inst_module_cmds = [
      mod_sudo,
      File.join(@prefix,"bin/gem3"),
      "install",
      @ruby_gems.join(" ")
    ]

    puts "Installing additional gems..."
    self.Run( inst_module_cmds.join(" ") )

    self.WriteInfo
  end

end # class InstRuby
