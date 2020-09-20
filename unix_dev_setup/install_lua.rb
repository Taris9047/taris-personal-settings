#!/usr/bin/env ruby

# Installs Lua

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require 'etc'
require 'open3'

class InstLua
  @@source_url = "https://www.lua.org/ftp/lua-5.4.0.tar.gz"

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

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @@CompilerSettings = gc.get_settings

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

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      Open3.capture3( "tar xf "+File.realpath(File.join(@@Src_dir, src_tarball_fname))+" -C "+@@Build_dir )
    end

    if @@need_sudo
      inst_cmd = "sudo make INSTALL_TOP=\""+@@Prefix+"\" install"
    else
      inst_cmd = "make INSTALL_TOP=\""+@@Prefix+"\" install"
    end

    # Ok let's roll!!
    cmds = [
      "cd", src_extract_folder, "&&",
      "make "+@@CompilerSettings.join(" ")+" linux",
      "&&",
      inst_cmd
    ]

    Open3.capture3( cmds.join(" ") )

  end
end # class InstLua
