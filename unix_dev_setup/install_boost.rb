#!/usr/bin/env ruby

# Installs Boost
# http://www.boost.org/

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'

class InstBoost

  @@source_url = "https://dl.bintray.com/boostorg/release/1.74.0/source/boost_1_74_0.tar.bz2"

  @@Prefix = nil
  @@Src_dir = nil
  @@Build_dir = nil
  
  @@b2_opts = [
    "address-model=64",
    "architecture=x86",
    "--build-dir=build",
    ]

  def initialize(prefix, src_dir, build_dir, need_sudo)
    @@Prefix = prefix
    @@Src_dir = src_dir
    @@Build_dir = build_dir
    @@need_sudo = need_sudo
  
    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @@env = gc.get_env_settings
  end

  def install
    dl = Download.new(@@source_url, @@Src_dir)
    src_tarball_path = dl.GetPath

    fp = FNParser.new(@@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    src_extracted_folder = File.join(@@Build_dir,src_tarball_bname)
    if File.exists?(src_extracted_folder)
      puts "Previous Boost installation exists"
    else
      system( ["tar xvf", File.realpath(src_tarball_path), "-C", File.realpath(@@Build_dir)].join(" ") )
    end

    # Boost is kinda simple. just build within the directory!
    if @@need_sudo
      inst_cmd = "sudo ./b2 install"
    else
      inst_cmd = "./b2 install"
    end
    
    @@b2_opts << "--prefix={prefix}".gsub('{prefix}', @@Prefix)
    @@b2_opts << "stage"
    
    cmds = [
      "cd",
      src_extracted_folder, "&&",
      "./bootstrap.sh", "--prefix="+@@Prefix, "&&",
      "./b2", @@b2_opts.join(" "), "&&",
      inst_cmd
    ]

    system( @@env, cmds.join(" ") )

  end # install


end # class InstBoost
