#!/usr/bin/env ruby

require 'etc'
require 'fileutils'

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

$gcc_conf_options = [
  "--enable-languages=c,c++,fortran,objc,obj-c++",
  "--enable-shared",
  "--enable-default-pie",
  "--enable-linker-build-id",
  "--enable-threads=posix",
  "--enable-plugin",
  "--enable-nls",
  "--enable-clocale=gnu",
  "--with-target-system-zlib=auto",
  "--with-system-zlib",
  "--with-default-libstdcxx-abi=new",
  "--enable-gnu-unique-object",
  "--enable-objc-gc=auto",
  "--disable-multilib",
  "--disable-werror",
  "--build={target_arch}",
  "--host={target_arch}",
  "--target={target_arch}",
  # "--libexecdir={prefix}/lib",
  # "--libdir={prefix}/lib"
]


class InstGCC < InstallStuff

  def initialize (
      prefix='/usr/local', os_type='x86_64-linux-gnu',
      work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super('gcc', prefix, work_dirs, ver_check=true, verbose_mode=verbose_mode)

    @source_url = SRC_URL[@pkgname]

    @conf_options = $gcc_conf_options

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
    }

    @os_type=os_type
    @need_sudo=need_sudo
    @verbose = verbose_mode

  end

  def get_env_str
    envstr = []
    @env.keys.each do |k|
      envstr += ["#{k}=\"#{@env[k]}\""]
    end
    return envstr.join(' ')
  end

  def do_install
    @pkginfo_file=File.join(@pkginfo_dir, @pkgname+'.info')

    if @pkgname == 'gcc'
      o, e, s = Open3.capture3('echo $(/usr/bin/gcc --version)')
      ver_system_gcc = Version.new(o.split(' ')[2])
      if ver_system_gcc >= @ver_source
        puts "Looks like system gcc is new enough! Skipping!"
        self.WriteInfo_system(ver_system_gcc.to_s)
        return 0
      end
    end

    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

    # Replace '{prefix}' on configure parameters.
    @conf_options.each_with_index do |co, ind|
      if co.include?'{prefix}'
        @conf_options[ind] = co.gsub('{prefix}', @prefix)
      end
      if co.include?'{target_arch}'
        @conf_options[ind] = co.gsub('{target_arch}', @os_type)
      end
    end
    @env.each do |key, flag|
      if flag.include? '{prefix}'
        @env[key] = flag.gsub('{prefix}', @prefix)
      end
    end


    if File.file?(@pkginfo_file)
      puts "Oh, it seems gcc was already installed!! Skipping!!"
      return 0
    end

    puts "Downloading src from #{@source_url}"
    dl = Download.new(@source_url, @src_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(@build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir)
      puts "Extracted folder has been found. Using it!" 
    else
      puts "Extracting..."
      self.Run( "tar xf "+source_file+" -C "+@build_dir)  
    end

    # Downloading prerequisites
    self.Run( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites" )

    # Let's build!!
    unless Dir.exists?(bld_dir)
      puts "Build dir missing... making one..."
    else
      puts "Build dir exists, cleaning up before work!!"
      FileUtils.rm_rf( bld_dir )
    end
    FileUtils.mkdir_p( bld_dir )

    if @need_sudo
      inst_cmd = "&& sudo make install"
    else
      inst_cmd = "&& make install"
    end

    opts = ["--prefix="+@prefix]+@conf_options
    cmd = [
      self.get_env_str,
      "cd",
      File.realpath(bld_dir),
      "&&",
      File.realpath(extracted_src_dir)+"/configure",
      opts.join(" "),
      "&& make -j", @Processors.to_s, "bootstrap",
      "&& make -j", @Processors.to_s,
      inst_cmd
    ]

    # Ok let's rock!
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.Run( @env, cmd.join(" ") )

    self.WriteInfo
  end

  def WriteInfo_system(ver_system_gcc)
    puts "Writing package info for #{@pkgname}..."
    fp = File.open(@pkginfo_file, 'w')
    env_str = @env.map{|k,v| "{k}={v}".gsub('{k}', k).gsub('{v}', v)}.join("\n")

    o, e, s = Open3.capture3('echo $(gcc -v)')
    conf_options_str = o

    fnp = FNParser.new(@source_url)
    compile_info_json = {
      "Package Name" => @pkgname,
      "Source file URL" => 'system_package_manager',
      "Version" => ver_system_gcc.split('.'),
      "Config options" => conf_options_str,
      "Env Variables" => 'system_package_manager',
    }
    fp.write(compile_info_json.to_json)
    # fp.puts(compile_info.join("\n"))
    fp.close
  end

end # class InstGCC


class InstGCC8 < InstGCC

  def initialize (prefix='/usr/local', os_type='x86_64-linux-gnu', work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super(prefix, os_type, work_dirs, need_sudo, verbose_mode=verbose_mode)

    @pkgname = 'gcc8'
    @source_url = SRC_URL[@pkgname]

    @conf_options = \
      $gcc_conf_options - ["--enable-languages=c,c++,fortran,objc,obj-c++"] \
      + ["--enable-languages=c,c++"] \
      + ["--program-suffix=-8"]

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
    }

    @need_sudo=need_sudo
    @verbose = verbose_mode

  end

  def do_install
    super
  end

end # class InstGCC8


class InstGCC9 < InstGCC

  def initialize (prefix='/usr/local', os_type='x86_64-linux-gnu', work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super(prefix, os_type, work_dirs, need_sudo, verbose_mode=verbose_mode)

    @pkgname = 'gcc9'
    @source_url = SRC_URL[@pkgname]

    @conf_options = \
      $gcc_conf_options - ["--enable-languages=c,c++,fortran,objc,obj-c++"] \
      + ["--enable-languages=c,c++"] \
      + ["--program-suffix=-9"]

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-O3 -march=native -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
    }

    @need_sudo=need_sudo
    @verbose = verbose_mode

  end

  def do_install
    super
  end

end # class InstGCC9

# Gcc4.8.5 --> matching version for cuda 6.5 (MBP 2008)
class InstGCC4 < InstGCC

  def initialize (prefix='/usr/local', os_type='x86_64-linux-gnu', work_dirs=['./build', './src', './pkginfo'], need_sudo=false, verbose_mode=false)

    super(prefix, os_type, work_dirs, need_sudo, verbose_mode=verbose_mode)

    @pkgname = 'gcc4'
    @source_url = SRC_URL[@pkgname]

    @conf_options = [ 
      "--enable-shared", 
      "--disable-boostrap",
      "--enable-threads=posix",       
      "--disable-nls",
      "--disable-multilib",
      "--enable-languages=c,c++", 
      "--build={target_arch}",
      "--host={target_arch}",
      "--target={target_arch}",
      "--program-suffix=-4" ]

    @env = {
      "CC" => "gcc",
      "CXX" => "g++",
      "CFLAGS" => "-w -O2 -std=gnu89 -fgnu89-inline -fomit-frame-pointer -pipe",
      "CXXFLAGS" => "-w -O2 -std=gnu++98 -fomit-frame-pointer -pipe",
      "LDFLAGS" => "-Wl,-rpath={prefix}/lib -Wl,-rpath={prefix}/lib64",
  #    "LD_LIBRARY_PATH" => "/usr/lib/x86_64-linux-gnu:/usr/lib:{prefix}/lib:{prefix}/lib64",
    }

    @need_sudo=need_sudo
    @verbose = verbose_mode

  end

  def do_install
    @pkginfo_file=File.join(@pkginfo_dir, @pkgname+'.info')

    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

    # Replace '{prefix}' on configure parameters.
    @conf_options.each_with_index do |co, ind|
      if co.include?'{prefix}'
        @conf_options[ind] = co.gsub('{prefix}', @prefix)
      end
      if co.include?'{target_arch}'
        @conf_options[ind] = co.gsub('{target_arch}', @os_type)
      end
    end
    @env.each do |key, flag|
      if flag.include? '{prefix}'
        @env[key] = flag.gsub('{prefix}', @prefix)
      end
    end

    if File.file?(@pkginfo_file)
      puts "Oh, it seems gcc was already installed!! Skipping!!"
      return 0
    end

    puts "Downloading src from #{@source_url}"
    dl = Download.new(@source_url, @src_dir)
    source_file = dl.GetPath()
    fp = FNParser.new(source_file)
    src_tarball_fname, src_tarball_bname = fp.name

    extracted_src_dir = File.join(@build_dir, src_tarball_bname)
    bld_dir = extracted_src_dir+"-build"

    if Dir.exists?(extracted_src_dir)
      puts "Extracted folder has been found. Using it!" 
    else
      puts "Extracting..."
      self.Run( "tar xf "+source_file+" -C "+@build_dir)  
    end
    
    # Downloading prerequisites
    puts extracted_src_dir
    self.Run( "cd "+File.realpath(extracted_src_dir)+" && "+"./contrib/download_prerequisites" )

    # Need to patch a file.
    puts ""
    puts "Patching bugged files..."
    puts ""
    arches = [
      "aarch64", "alpha", "bfin", "i386", "pa", "sh", "tilepro", "xtensa"
    ]
    patch_cmd = [
      "sed -i -e 's/__attribute__/\\/\\/__attribute__/g' #{extracted_src_dir}/gcc/cp/cfns.h",
      "sed -i 's/struct ucontext/ucontext_t/g' #{extracted_src_dir}/libgcc/config/i386/linux-unwind.h"
    ]
    # arches.each do |ar|
    #   patch_cmd += [
    #     "wget \"https://gcc.gnu.org/git/?p=gcc.git;a=blob_plain;f=libgcc/config/#{ar}/linux-unwind.h\" -O #{extracted_src_dir}/libgcc/config/#{ar}/linux-unwind.h"  ]
    # end
    self.Run( patch_cmd.join(' && ') )

    # Let's build!!
    unless Dir.exists?(bld_dir)
      puts "Build dir missing... making one..."
    else
      puts "Build dir exists, cleaning up before work!!"
      FileUtils.rm_rf( bld_dir )
    end
    FileUtils.mkdir_p( bld_dir )

    if @need_sudo
      inst_cmd = "sudo make install"
    else
      inst_cmd = "make install"
    end

    opts = ["--prefix="+@prefix]+@conf_options
    cmd = [
      self.get_env_str,
      "cd #{File.realpath(bld_dir)}",
      "#{File.join(File.realpath(extracted_src_dir), "configure")} #{opts.join(" ")}",
      "make -j #{@Processors.to_s}",
      inst_cmd
    ]

    puts "*** This is totally deprecated software! ***"
    puts "*** If it breaks, it breaks... ***"
    puts "*** (This package was added solely due to old cuda: 6.5) ***"
    puts ""
    puts "*** If Ubuntu based one... try installing ***"
    puts "    gcc-multilib libstdc++6:i386"
    puts ""
    sleep(2)

    # Ok let's rock!
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.Run( @env, cmd.join(" && ") )

    self.WriteInfo

  end

end # class InstGCCOld
