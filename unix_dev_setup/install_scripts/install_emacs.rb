#!/usr/bin/env ruby
# this will handle emacs

# TODO: Implement more dependencies.
# 1. gnutls
# 2. giflib
# 3. jesson json parsor
# 4. libotf
# 5. m17n-flt
# 6. libxft
# 7. libgmp

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

class InstEmacs < InstallStuff

  def initialize(args)

    args.each do |k,v|
      instance_variable_set("@#{k}", v) unless v.nil?
    end

    super(@pkgname, @prefix, @work_dirs, @ver_check, @verbose_mode)

    @source_url = SRC_URL[@pkgname]

    # Setting up compilers
    self.CompilerSet

    # build options
    @conf_options = []
    # Checking up qt5
    @conf_options += [
      # This may not work.. on 27
      '--with-modules',
      '--with-mailutils',
      '--with-pop',
    #  '--with-xwidgets'    # needs webkitgtk4-dev
    ]

    @env["CC"] = "gcc"
    @env["CXX"] = "g++"
    @env["CFLAGS"] = "-O3 -fomit-frame-pointer -march=native -pipe"
    @env["CXXLAGS"] = "-O3 -fomit-frame-pointer -march=native -pipe"
    @env["LDFLAGS"] = "-Wl,-rpath=#{@prefix}/lib -Wl,-rpath=#{@prefix}/lib64"

  end

  def do_install

    puts ""
    puts "*** Dependencies has not implemented yet... ***"
    puts "Usually, you need giflib and gnutls. Install them from "
    puts "your distribution's package manager."
    puts ""
    sleep (2)

    dl = Download.new(@source_url, @src_dir)
    src_tarball_path = dl.GetPath

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      self.Run( "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      self.Run( "rm -rf "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    self.Run( "mkdir -p "+src_build_folder )

    opts = ["--prefix="+@prefix]+@conf_options

    if @need_sudo
      inst_cmd = "sudo make install"
      mod_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      mod_sudo = ""
    end

    # Ok let's roll!!
    cmds = [
      "cd", src_build_folder, "&&",
      src_extract_folder+"/configure",
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.RunInstall( env: @env, cmd: cmds.join(" ") )
    self.WriteInfo
  end

end # class InstEmacs