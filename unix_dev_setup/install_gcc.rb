#!/usr/bin/env ruby

require 'etc'
require './download.rb'
require './fname_parser.rb'
require './install_stuff.rb'
require './src_urls.rb'

class InstGCC < InstallStuff

  def initialize (
      prefix='/usr/local', os_type='Ubuntu',
      work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super('gcc', prefix, work_dirs, verbose_mode=verbose_mode)

    @source_url = SRC_URL['gcc']

    @conf_options = [
      "--enable-languages=c,c++,fortran,objc,obj-c++",
      "--enable-shared",
      "--enable-default-pie",
      "--enable-linker-build-id",
      "--enable-threads=posix",
      "--enable-plugin",
      "--with-system-zlib",
      "--disable-multilib",
      "--build=x86_64-linux-gnu",
    ]

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
    }

    @os_type=os_type
    @need_sudo=need_sudo

  end

  def install
    @pkginfo_file=File.join(@pkginfo_dir, @pkgname+'.info')

    self.GetSrcVer
    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

    if self.CheckInfo
      return 0
    end

    if File.file?(@pkginfo_file)
      puts "Oh, it seems gcc was already installed!! Skipping!!"
      return 0
    end

    dl = Download.new(@source_url, @src_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(@build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir) == true
      puts "Extracted folder has been found!!"
    else
      puts "Extracting..."
      self.Run( "tar xf "+source_file+" -C "+@build_dir)
    end

    # Downloading prerequisites
    self.Run( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites" )

    # Let's build!!
    if Dir.exists?(bld_dir) == false
      puts "Build dir missing... making one..."
    else
      puts "Build dir exists, cleaning up before work!!"
      self.Run( "rm -rf "+bld_dir )
    end
    self.Run( "mkdir -p "+bld_dir )

    if @need_sudo
      inst_cmd = "&& sudo make install"
    else
      inst_cmd = "&& make install"
    end

    @env['LDFLAGS'] = @env['LDFLAGS'].gsub('{prefix}', @prefix)

    opts = ["--prefix="+@prefix]+@conf_options
    cmd = [
      "cd",
      File.realpath(bld_dir),
      "&&",
      File.realpath(extracted_src_dir)+"/configure",
      opts.join(" "),
      "&& make -j",@Processors.to_s,"bootstrap",
      "&& make -j",@Processors.to_s,
      inst_cmd
    ]

    # Ok let's rock!
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.Run( @env, cmd.join(" ") )

    self.WriteInfo
  end

end # class InstGCC


class InstGCCCuda < InstGCC

  def initialize (prefix='/usr/local', os_type='Ubuntu', work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super(prefix, os_type, work_dirs, need_sudo, verbose_mode=verbose_mode)

    @pkgname = 'cudacc'
    @source_url = SRC_URL['cudacc']

    @conf_options = [
      "--program-suffix=-cuda",
      "--enable-languages=c,c++,fortran,objc,obj-c++",
      "--enable-shared",
      "--enable-default-pie",
      "--enable-linker-build-id",
      "--enable-threads=posix",
      "--enable-plugin",
      "--with-system-zlib",
      "--disable-multilib",
      "--build=x86_64-linux-gnu",
    ]

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
    }

    @need_sudo=need_sudo

  end

  def install
    super
  end

end # class InstGCCCuda


class InstGCCOld < InstGCC


  def initialize (prefix='/usr/local', os_type='Ubuntu', work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super(prefix, os_type, work_dirs, need_sudo, verbose_mode=verbose_mode)

    @pkgname = 'gccold'
    @source_url = SRC_URL[@pkgname]

    @conf_options = [
      "--program-suffix=-old",
      "--enable-languages=c,c++,fortran,objc,obj-c++",
      "--enable-shared",
      "--enable-default-pie",
      "--enable-linker-build-id",
      "--enable-threads=posix",
      "--enable-plugin",
      "--with-system-zlib",
      "--disable-multilib",
      "--build=x86_64-linux-gnu",
    ]

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
    }

    @need_sudo=need_sudo

  end

  def install
    super
  end

end # class InstGCCOld
