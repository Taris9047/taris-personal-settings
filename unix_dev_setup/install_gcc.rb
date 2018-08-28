#!/usr/bin/env ruby

require 'etc'
require './download.rb'
require './fname_parser.rb'

class InstGCC
  @@gcc_source_url = "https://ftp.gnu.org/gnu/gcc/gcc-8.2.0/gcc-8.2.0.tar.xz"

  @@gcc_conf_options = [
    "--enable-languages=c,c++,fortran,objc,obj-c++,go",
    "--enable-shared" ,
    "--enable-linker-build-id" ,
    "--enable-threads=posix" ,
    "--enable-plugin" ,
    "--with-system-zlib" ,
    "--disable-multilib" ,
    "--build=x86_64-linux-gnu",
  ]

  @@Processors = nil

  @@CompilerSettings = [
    "CC=\"gcc\"",
    "CXX=\"g++\"",
    "CFLAGS=\"-O3 -march=native -fomit-frame-pointer -pipe\"",
    "CXXFLAGS=\"-O3 -march=native -fomit-frame-pointer -pipe\"",
  ]

  def initialize
    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  def install_gcc (prefix='/usr/local', os_type='Ubuntu', build_dir='./build', source_dir='./src')
    puts ""
    puts "Working on GCC!!"
    puts ""

    dl = Download.new(@@gcc_source_url, source_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir) == true
      puts "Extracted folder has been found!!"
    else
      puts "Extracting..."
      system( "tar xf "+source_file+" -C "+build_dir)
    end

    # Downloading prerequisites
    system( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites" )

    # Let's build!!
    if Dir.exists?(bld_dir) == false
      puts "Build dir missing.. making one.."
    else
      puts "Build dir exists, cleaning up before work!!"
      system( "rm -rf "+bld_dir )
    end
    system( "mkdir "+bld_dir )

    opts = Array.new(["--prefix="+prefix]+@@gcc_conf_options)
    cmd = [
      "cd",
      File.realpath(bld_dir),
      "&&",
      @@CompilerSettings.join(" "),
      File.realpath(extracted_src_dir)+"/configure",
      opts.join(" "),
      "&& make -j",@@Processors.to_s,"bootstrap",
      "&& make -j",@@Processors.to_s,
      "&& sudo make install"
    ]

    system( cmd.join(" ") )

  end

end # class InstGCC


class InstGCCCuda
  @@gcc_source_url = "https://ftp.gnu.org/gnu/gcc/gcc-5.4.0/gcc-5.4.0.tar.bz2"

  @@gcc_conf_options = [
    "--program-suffix=5",
    "--enable-languages=c,c++,fortran,objc,obj-c++,go",
    "--enable-shared" ,
    "--enable-linker-build-id" ,
    "--enable-threads=posix" ,
    "--enable-plugin" ,
    "--with-system-zlib" ,
    "--disable-multilib" ,
    "--build=x86_64-linux-gnu",
  ]

  @@Processors = nil

  @@CompilerSettings = [
    "CC=\"gcc -std=gnu89\"",
    "CXX=\"g++\"",
    "CFLAGS=\"-O3 -march=native -fomit-frame-pointer -pipe\"",
    "CXXFLAGS=\"-O3 -march=native -fomit-frame-pointer -pipe\"",
  ]

  def initialize
    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  def install_gcc (prefix='/usr/local', os_type='Ubuntu', build_dir='./build', source_dir='./src')
    puts ""
    puts "Working on GCC for Cuda!!"
    puts ""

    dl = Download.new(@@gcc_source_url, source_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir) == true
      puts "Extracted folder has been found!!"
    else
      puts "Extracting..."
      system( "tar xf "+source_file+" -C "+build_dir)
    end

    # Downloading prerequisites
    system( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites" )

    # Let's build!!
    if Dir.exists?(bld_dir) == false
      puts "Build dir missing.. making one.."
    else
      puts "Build dir exists, cleaning up before work!!"
      system( "rm -rf "+bld_dir )
    end
    system( "mkdir -f "+bld_dir )

    opts = Array.new(["--prefix="+prefix]+@@gcc_conf_options)
    cmd = [
      "cd",
      File.realpath(bld_dir),
      "&&",
      @@CompilerSettings.join(" "),
      File.realpath(extracted_src_dir)+"/configure",
      opts.join(" "),
      "&& make -j",@@Processors.to_s,"bootstrap",
      "&& make -j",@@Processors.to_s,
      "&& sudo make install"
    ]

    system( cmd.join(" ") )

  end

end # class InstGCCCuda
