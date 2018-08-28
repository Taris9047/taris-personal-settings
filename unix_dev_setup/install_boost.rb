#!/usr/bin/env ruby

# Installs Boost
# http://www.boost.org/

require './download.rb'
require './fname_parser.rb'

class InstBoost

  @@source_url = "https://dl.bintray.com/boostorg/release/1.68.0/source/boost_1_68_0.tar.bz2"

  @@Prefix = nil
  @@Src_dir = nil
  @@Build_dir = nil

  def initialize(prefix, src_dir, build_dir)
    @@Prefix = prefix
    @@Src_dir = src_dir
    @@Build_dir = build_dir
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
    cmds = [
      "cd",
      src_extracted_folder, "&&",
      "./bootstrap.sh", "--prefix="+@@Prefix, "&&",
      "./b2", "&&",
      "sudo ./b2 install"
    ]

    system( cmds.join(" ") )

  end # install


end # class InstBoost
