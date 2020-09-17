#!/usr/bin/env ruby

# this will handle Ruby

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require 'etc'

class InstRuby
  @@source_url = "https://cache.ruby-lang.org/pub/ruby/2.7/ruby-2.7.1.tar.gz"

  @@Prefix = nil
  @@Build_dir = nil
  @@Src_dir = nil

  # Python2 modules to install
  @@ruby_gems = [
    "rsense", "rails", "bundler"
  ]

  # Python2 build options
  @@ruby_conf_opts = [
    "--enable-shared"
  ]

  @@Processors = nil

  def initialize(prefix, build_dir, src_dir, need_sudo=false)
    @@Prefix = prefix
    @@Build_dir = build_dir
    @@Src_dir = src_dir
    @@need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @@env = gc.get_env_settings

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  def install
    dl = Download.new(@@source_url, @@Src_dir)
    # src_tarball_path = dl.GetPath

    fp = FNParser.new(@@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@@Build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@@Build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      system( "tar xf "+File.realpath(File.join(@@Src_dir, src_tarball_fname))+" -C "+@@Build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      system( "rm -rfv "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    system( "mkdir "+src_build_folder )

    conf_opts = ["--prefix="+@@Prefix]+@@ruby_conf_opts

    if @@need_sudo
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
      conf_opts.join(" "), "&&",
      "make -j", @@Processors.to_s, "&&",
      inst_cmd
    ]

    system( @@env, cmds.join(" ") )

    inst_module_cmds = [
      mod_sudo,
      File.join(@@Prefix,"/bin/gem"),
      "install",
      @@ruby_gems.join(" ")
    ]

    system( inst_module_cmds.join(" ") )

  end
end # class InstRuby
