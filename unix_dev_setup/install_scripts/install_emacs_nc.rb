#!/usr/bin/env ruby
# this will handle emacs with native-compiler

# Additional deps for Ubuntu
#
# libgccjit0 libgccjit10-dev texinfo
#

require 'fileutils'
require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

class InstEmacsNC < InstallStuff

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
      '--with-modules',
      '--with-mailutils',
      '--with-pop',
      '--with-native-compilation',
    #  '--with-xwidgets'    # needs webkitgtk4-dev
    ]

    @env["CC"] = "gcc-10"
    @env["CXX"] = "g++-10"
    @env["CFLAGS"] = "-O3 -fomit-frame-pointer -march=native -pipe"
    @env["CXXLAGS"] = "-O3 -fomit-frame-pointer -march=native -pipe"
    @env["LDFLAGS"] = "-Wl,-rpath=#{@prefix}/lib -Wl,-rpath=#{@prefix}/lib64 -L#{@prefix}/lib -L#{@prefix}/lib64"
    @env["C_INCLUDE_PATH"] = "#{@prefix}/include"
    @env["CPLUS_INCLUDE_PATH"] = "#{@prefix}/include"

  end

  def do_install

    puts ""
    
    warn_txt = %q{
Emacs native-compiler (GccEmacs) is an experiemental program.
Many rolling distros provide this version with repl or copr.
So, it's better to use them instead of this head bonking source compile.
}      
    puts warn_txt
    sleep (2)

    dl = Download.new(@source_url, @src_dir,
        source_ctl='git', mode='wget',
        source_ctl_opts="#{@pkgname} -b feature/native-comp")
    src_clone_path = dl.GetPath

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_clone_folder = File.join(File.realpath(@src_dir), "#{@pkgname}")
    src_build_folder = File.join(File.realpath(@build_dir), "#{@pkgname}-build")

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
    else
      inst_cmd = "make install"
    end

    # Ok let's roll!!
    cmds = [
      "cd #{src_clone_folder}", "&&",
      "./autogen.sh", "&&",
      "cd #{src_build_folder}", "&&",
      File.join(src_clone_folder,"configure"), opts.join(" "), "&&",
      "make -j#{@Processors.to_s}", "&&",
      inst_cmd
    ]
    puts "Compiling (with #{@Processors} processors) and Installing ..."
    self.RunInstall( env: @env, cmd: cmds.join(" ") )
    self.WriteInfo
  end

end # class InstEmacsNC
