#!/usr/bin/env ruby

require 'etc'
require './download.rb'
require './fname_parser.rb'

class InstGCC
  @@gcc_source_url = "https://ftp.gnu.org/gnu/gcc/gcc-10.2.0/gcc-10.2.0.tar.xz"

  @@gcc_conf_options = [
    "--enable-languages=c,c++,fortran,objc,obj-c++",
    "--enable-shared" ,
    "--enable-linker-build-id" ,
    "--enable-threads=posix" ,
    "--enable-plugin" ,
    "--with-system-zlib" ,
    "--disable-multilib" ,
    "--build=x86_64-linux-gnu",
  ]

  @@Processors = nil
  
  @@env = {
    "CC" => "gcc",
    "CXX" => "g++",
    "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
    "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
    "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
  }

  def initialize (prefix='/usr/local', os_type='Ubuntu', build_dir='./build', source_dir='./src', need_sudo=false)
    
    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  
    @@prefix=prefix
    @@os_type=os_type
    @@build_dir=build_dir
    @@source_dir=source_dir
    @@need_sudo=need_sudo
  
  end

  def install
    puts ""
    puts "Working on GCC!!"
    puts ""

    dl = Download.new(@@gcc_source_url, @@source_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(@@build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir) == true
      puts "Extracted folder has been found!!"
    else
      puts "Extracting..."
      system( "tar xf "+source_file+" -C "+@@build_dir)
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
    system( "mkdir -p "+bld_dir )

    if @@need_sudo
      inst_cmd = "&& sudo make install"
    else
      inst_cmd = "&& make install"
    end

    @@env['LDFLAGS'] = @@env['LDFLAGS'].gsub('{prefix}', @@prefix)

    opts = Array.new(["--prefix="+@@prefix]+@@gcc_conf_options)
    cmd = [
      "cd",
      File.realpath(bld_dir),
      "&&",
      File.realpath(extracted_src_dir)+"/configure",
      opts.join(" "),
      "&& make -j",@@Processors.to_s,"bootstrap",
      "&& make -j",@@Processors.to_s,
      inst_cmd
    ]

    system( @@env, cmd.join(" ") )

  end

end # class InstGCC


class InstGCCCuda
  @@gcc_source_url = "https://ftp.gnu.org/gnu/gcc/gcc-8.4.0/gcc-8.4.0.tar.gz"

  @@gcc_conf_options = [
    "--program-suffix=-cuda",
    "--enable-languages=c,c++,fortran,objc,obj-c++",
    "--enable-shared" ,
    "--enable-linker-build-id" ,
    "--enable-threads=posix" ,
    "--enable-plugin" ,
    "--with-system-zlib" ,
    "--disable-multilib" ,
    "--build=x86_64-linux-gnu",
  ]

  @@Processors = nil

  @@env = {
    "CC" => "gcc",
    "CXX" => "g++",
    "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
    "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
    "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
  }

  def initialize (prefix='/usr/local', os_type='Ubuntu', build_dir='./build', source_dir='./src', need_sudo=false)
    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
    
    @@prefix=prefix
    @@os_type=os_type
    @@build_dir=build_dir
    @@source_dir=source_dir
    @@need_sudo=need_sudo

  end

  def install
    puts ""
    puts "Working on GCC for Cuda!!"
    puts ""

    dl = Download.new(@@gcc_source_url, @@source_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(@@build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir) == true
      puts "Extracted folder has been found!!"
    else
      puts "Extracting..."
      system( "tar xf "+source_file+" -C "+@@build_dir)
    end

    # Downloading prerequisites
    system( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites" )

    # Let's build!!
    unless Dir.exists?(bld_dir)
      puts "Build dir missing.. making one.."
    else
      puts "Build dir exists, cleaning up before work!!"
      system( "rm -rf "+bld_dir )
    end
    system( "mkdir -p "+bld_dir )

    if @@need_sudo
      inst_cmd = "&& sudo make install"
    else
      inst_cmd = "&& make install"
    end

    @@env['LDFLAGS'] = @@env['LDFLAGS'].gsub('{prefix}', @@prefix)

    opts = Array.new(["--prefix="+@@prefix]+@@gcc_conf_options)
    cmd = [
      "cd",
      File.realpath(bld_dir),
      "&&",
      File.realpath(extracted_src_dir)+"/configure",
      opts.join(" "),
      "&& make -j",@@Processors.to_s,"bootstrap",
      "&& make -j",@@Processors.to_s,
      inst_cmd
    ]

    system( @@env, cmd.join(" ") )

  end

end # class InstGCCCuda
