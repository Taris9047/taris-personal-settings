#!/usr/bin/env ruby

# this will handle Node.js with NPM

require './download.rb'
require './fname_parser.rb'
require 'etc'

class InstNode
  @@source_url = "https://nodejs.org/dist/v14.9.0/node-v14.9.0.tar.gz"

  @@Prefix = nil
  @@Build_dir = nil
  @@Src_dir = nil

  @@Processors = nil

  @@CompilerSettings = [
    "CC=\"gcc\"",
    "CXX=\"g++\"",
    "CFLAGS=\"-O3 -march=native -fomit-frame-pointer -pipe\"",
    "CXXFLAGS=\"-O3 -march=native -fomit-frame-pointer -pipe\"",
  ]

  def initialize(prefix, build_dir, src_dir, need_sudo=false)
    @@Prefix = prefix
    @@Build_dir = build_dir
    @@Src_dir = src_dir
    @@need_sudo = need_sudo

    @@PythonCmd = "python3"

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  @@node_conf_opts = [
    "--shared-zlib"
  ]

  def install
    dl = Download.new(@@source_url, @@Src_dir)
    # src_tarball_path = dl.GetPath

    fp = FNParser.new(@@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(@@Build_dir, src_tarball_bname)
    # src_build_folder = File.join(@@Build_dir, src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      system( "tar xf "+File.realpath(File.join(@@Src_dir, src_tarball_fname))+" -C "+@@Build_dir )
    end

    # if Dir.exists?(src_build_folder)
    #   puts "Build folder found!! Removing it for 'pure' experience!!"
    #   system( "rm -rfv "+src_build_folder )
    # else
    #   puts "Ok, let's make a build folder"
    # end
    # system( "mkdir "+src_build_folder )

    conf_opts = ["--prefix="+@@Prefix]+@@node_conf_opts

    if @@need_sudo
      inst_cmd = "sudo make install"
      mod_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      mod_sudo = ""
    end

    # Ok let's roll!!
    cmds = [
      "cd", src_extract_folder, "&&",
      @@CompilerSettings.join(" "),
      @@PythonCmd,
      src_extract_folder+"/configure.py",
      conf_opts.join(" "), "&&",
      "make -j", @@Processors.to_s, "&&",
      inst_cmd
    ]

    system( cmds.join(" ") )

  end # install

end # class InstRuby
