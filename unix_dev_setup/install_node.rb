#!/usr/bin/env ruby

# this will handle Node.js with NPM

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require 'etc'

class InstNode
  @@source_url = "https://nodejs.org/dist/v14.11.0/node-v14.11.0.tar.gz"

  @@Prefix = nil
  @@Build_dir = nil
  @@Src_dir = nil

  @@Processors = nil

  def initialize(prefix, build_dir, src_dir, need_sudo=false)
    @@Prefix = prefix
    @@Build_dir = build_dir
    @@Src_dir = src_dir
    @@need_sudo = need_sudo

    @@PythonCmd = "python3"

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
      puts "Deleting it"
      system( ['rm -rf', src_extract_folder].join(' ') )
    end
    puts "Extracting"
    system( "tar xf "+File.realpath(File.join(@@Src_dir, src_tarball_fname))+" -C "+@@Build_dir )

    conf_opts = ["--prefix="+@@Prefix]+@@node_conf_opts

    if @@need_sudo
      inst_cmd = "sudo make install"
    else
      inst_cmd = "make install"
    end
    
    # A bit of last minute changes
    @@env['CFLAGS'] = @@env['CFLAGS'] + " -fPIE"
    @@env['CXXFLAGS'] = @@env['CXXFLAGS'] + " -fPIE"

    # Ok let's rock!
    cmds = [
      "cd", src_extract_folder, "&&",
      File.join(src_extract_folder,"configure"),
      conf_opts.join(" "), "&&",
      "make -j", @@Processors.to_s, "&&",
      inst_cmd
    ]
    puts cmds.join(' ')
    system( @@env, cmds.join(" ") )

  end # install

end # class InstNode
