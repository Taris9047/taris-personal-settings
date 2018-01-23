#!/usr/bin/env ruby

require 'etc'

class InstGCC
  @@gcc_source_url = "http://mirrors.concertpass.com/gcc/releases/gcc-7.2.0/gcc-7.2.0.tar.xz"

  @@gcc_conf_options = "--enable-languages=c,c++,fortran,objc,obj-c++ \
  --prefix=/usr/local \
  --enable-shared \
  --enable-linker-build-id \
  --enable-threads=posix \
  --enable-plugin \
  --with-system-zlib \
  --disable-multilib \
  --build=x86_64-linux-gnu"

  @@Processors = nil

  def initialize
    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  def install_gcc (prefix, os_type='Ubuntu', build_dir='./build', source_dir='./src')
    puts ""
    puts "Working on GCC!!"
    puts ""

    fbasename = File.basename @@gcc_source_url
    source_file = source_dir+"/"+fbasename
    sf_basename = File.basename(fbasename, '.tar.xz') # Gotta impelement better method to neglect the extensions later

    if File.exists?(source_file) == false
      puts "Downloading source..."
      system( "wget "+@@gcc_source_url+" -P "+source_dir+"/" )
    else
      puts fbasename+" found in "+source_file+" !!"
    end

    extracted_src_dir = build_dir+"/"+sf_basename
    bld_dir = build_dir+"/"+sf_basename+"-build"

    if Dir.exists?(extracted_src_dir) == true
      puts "Extracted folder has been found!!"
    else
      puts "Extracting..."
      system( "tar xf "+source_file+" -C "+build_dir)
    end

    # Downloading prerequisites
    system( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites")

    # Let's build!!
    if Dir.exists?(bld_dir) == false
      puts "Build dir missing.. making one.."
    else
      puts "Build dir exists, cleaning up before work!!"
      system( "rm -rf "+bld_dir )
    end
    system( "mkdir "+bld_dir )

    cmd = "cd "+File.realpath(bld_dir)+" && "+
    File.realpath(extracted_src_dir)+"/configure "+@@gcc_conf_options+
    "&& make -j"+@@Processors.to_s+" bootstrap "+
    "&& make -j"+@@Processors.to_s+" "+
    "&& sudo make install"

    system( cmd )

  end

end
