#!/usr/bin/env ruby

# TODO: Gotta make it with 'class'
# --> Too much stuff happening here!
#

require 'fileutils'
require 'tty-spinner'
require_relative './utils/utils.rb'

# Note that installing old gcc (gcccuda) is disabled due to libc 2.26 issue.
# In fact, we need to apply patch to adopt old gcc source codes to
# follow up the newest changes in libc 2.26

# Version
$version = ['1', '0', '2']

# title
$title = "Unix Development Environment setup"

#
# The main stuff handler class!
#
class UnixDevSetup

  def initialize(op_mode_list = [])

    # Default parameters
    home_dir = ENV["HOME"]
    def_prefix = File.join(home_dir, "/.local")
    # def_prefix = File.join("/usr/local")
    @list_of_progs = SRC_LIST[]

    @not_so_stable_pkgs = ['pypy3', 'clang', 'ROOT', 'julia']
    @not_so_needed_pkgs = ['gccold', 'cudacc', 'node-lts', 'ruby3']
    @not_really_a_pkg = ['get_pip', 'golang-bootstrap']

    @list_of_all = \
      @list_of_progs - @not_so_stable_pkgs \
      - @not_so_needed_pkgs \
      - @not_really_a_pkg 

    @aliases = TABLES.ALIAS_TABLE
    @aliases['all'] = @list_of_all

    @permitted_list = @list_of_progs + @aliases.keys
    @opt_list = [
      '--use-clang', 'prereq', '-v', '--verbose',
      'purge', '--purge', 'clean', '--clean', '--version',
      '--use-system-gcc', '-sgcc',
      '--force', '-f',
    ]
    @permitted_list += @opt_list

    # Default system ==> usually it's linux
    # TODO: implement platform detection stuff here later.
    @def_system = 'x86_64-linux-gnu'

    # Getting CWD
    @current_dir = File.expand_path(File.dirname(__FILE__))
    
    # Verbose mode? Default: false
    @verbose = false

    # Directories
    @work_dir_root = File.join(@current_dir, 'workspace')
    @work_dir_path = File.join(@work_dir_root, 'build')
    @source_dir_path = File.join(@work_dir_root, 'download')
    @pkginfo_dir_path = File.join(@current_dir, 'pkginfo')
    @work_dir_log = File.join(@work_dir_root, 'log')
    @prefix_dir_path = def_prefix
    @def_inst_script_dir='./install_scripts'
    self.__setup_work_dirs__

    # Setting up operatnion mode and package lists.
    @op_mode_list = op_mode_list
    @pkgs_to_install = []
    @flag_wrong_pkg_given = false
    @wrong_pkgs = []
    @parameters = []
    @op_mode_list.each do |opm|
      if @list_of_all.include?(opm)
        @pkgs_to_install.append(opm)
      elsif @aliases.keys.include?(opm)
        @pkgs_to_install.append(@aliases[opm])
      elsif @opt_list.include?(opm)
        @parameters.append(opm)
      else
        @wrong_pkgs.append(opm)
      end
    end
    unless @wrong_pkgs.empty?
      @flag_wrong_pkg_given = true
    end
    if @pkgs_to_install.empty?
      @pkgs_to_install = @list_of_all
    end

    # Clang mode for some packages.
    @clang_mode = false
    # use system gcc instead of gcc included here.
    @use_system_gcc = false
    # Force install mode (no dep check.)
    @force_install_mode = false
    self.__parse_params__

    # Set up console
    require_relative './utils/run_console.rb'
    @Con = RunConsole.new(verbose: @verbose, logf_dir: @work_dir_log)
  
    # Resolve dependencies.
    require './utils/utils.rb'
    puts "Checking dependency for #{@pkgs_to_install.join(" ")}"
    dep_resolve = DepResolve.new(
      @pkgs_to_install, @pkginfo_dir_path, @force_install_mode, @use_system_gcc)
      @pkgs_to_install = dep_resolve.GetInstList()

    # List packages to install
    if !dep_resolve.GetDepList().empty?
      puts "Following packages are selected to satisfy dependency."
      puts ""
      puts dep_resolve.PrintDepList()
      puts ""
    end
    puts "List of packages to install..."
    puts ""
    puts dep_resolve.PrintInstList()
    puts ""

    # Checking if the destination directory is writable or not.
    @need_sudo = !File.writable?(@prefix_dir)

    # Check version
    @vercheck = !@force_install_mode

    # TODO: Change class init arguemnt to hash based one.
    @inst_args = {
      "pkgname" => '',
      "prefix" => @prefix_dir,
      "os_type" => @def_system,
      "work_dirs" => @work_dirs,
      "need_sudo" => @eed_sudo,
      "verbose_mode" => @verbose,
      "ver_check" => @vercheck,
      "use_clang" => @clang_mode,
    }

    self.install_pkgs

  end # UnixDevSetup::initialize

  # Remove default python cmd
  def remove_def_python_cmd
    puts "Removing 'python' command to preserve system native python..."
    sudo_cmd = ''
    if !File.writable?(@prefix_dir)
      sudo_cmd = "sudo"
    end
    del_python_cmd = [
      File.join($prefix_dir, "bin/python"),
      File.join($prefix_dir, "bin/ipython")
    ]
    del_python_cmd.each do |c|
      FileUtils.rm_rf(c)
    end
  end

  # Main title banner
  def main_title
    mt = %{
  ******************************************

  #{$title}"
  Version (#{$version.join('.')})"

  ******************************************
    }
    puts mt
  end

  # Help message
  def show_help
    main_title
    hlp = %{
  Usage: ./unix_dev_setup.rb <params_or_installable_pkgs>

  <params> can be:
  #{$opt_list.join(', ')}
  --use-clang: Some packages can be built with clang.
  -v,--verbose: Make it loud!
  --version: displays version info.
  --purge: deletes everything before installing any package including pkginfo dir.
  --clean: deletes working dirs before installing any package
  -sgcc,--use-system-gcc: uses system gcc instead of state-of-art one.
  -f,--force: ignores dependency check and install packages.
  clean: deletes working dirs
  purge: purges all the working dirs including pkginfo dir.

  <installable_pkgs> can be:
  #{($permitted_list-$opt_list).join(', ')}

  --> Note that node-lts replaces node and vice versa.
  --> Default installation is node(latest version)

  Some packages are not very stable at the moment:
  #{$not_so_stable_pkgs.join(', ')}

  More packages are coming!! Stay tuned!!
    }
    puts hlp
    exit(0)
  end

  def __parse_params__
    # Parsing operation parameters.
    # Get version
    if @parameters.include?('--version')
      puts "(UDE set) #{$title} Ver. #{$version.join('.')}"
      exit(0)
    end

    # Show help message and quit.
    if @parameters.include?('--help')
      self.show_help
    end

    # Handling Verbose mode
    if @parameters.include?('-v') or @parameters.include?('--verbose')
      @verbose = true
      puts ""
      puts "*** Verbose ON! It will be pretty loud! ***"
      puts "* Note that some compilation jobs hang up with Verbose ON. *"
      puts ""
    end

    # Use clang as compiler
    if @parameters.include?('--use-clang')
      puts "Clang mode turned on. Some packages will be compiled with system llvm-clang."
      @clang_mode = true
    end

    # Some edge cases... cleaning and installing prereq
    if @parameters.include?('purge')
      puts "Purging everything!!!"
      spinner = TTY::Spinner.new("[Purging] ... :spinner", format: :bouncing_ball)
      spinner.auto_spin
      FileUtils.rm_rf(@work_dir_root)
      FileUtils.rm_rf(@pkginfo_dir_path)
      prefix_kill_list = Dir.entries(@prefix_dir_path)
      prefix_kill_list -= [ ".", "..", "share" ]
      prefix_kill_list += [ ".opt" ]
      prefix_kill_list = prefix_kill_list.uniq
      prefix_kill_list.each do |k|
        FileUtils.rm_rf(File.join(@prefix_dir_path, k))
      end
      spinner.stop
      puts "Purged everything!! Now you are free of cruds."
      exit(0)
    end
    if @parameters.include?('--purge')
      spinner = TTY::Spinner.new("[Purging] .. :spinner", format: :bouncing_ball)
      puts "Performing purge install..."
      spinner.auto_spin
      FileUtils.rm_rf(@pkginfo_dir_path)
      FileUtils.rm_rf(@work_dir_root)
      spinner.stop
      puts "Deleted every build stuff!!"
    end

    if @parameters.include?('clean')
      puts "Cleaning up source files and build dirs..."
      spinner = TTY::Spinner.new("[Cleaning] ... :spinner", format: :bouncing_ball)
      spinner.auto_spin
      FileUtils.rm_rf(@work_dir_root)
      spinner.stop
      puts "Cleaned up source files to save space!!"
      exit(0)
    end
    if @parameters.include?('--clean')
      puts "Performing clean install..."
      puts "Cleaning up source files and build dirs..."
      spinner = TTY::Spinner.new("[Cleaning] ... :spinner", format: :bouncing_ball)
      spinner.auto_spin
      FileUtils.rm_rf(@work_dir_root)
      spinner.stop
      puts "Cleaned up source files to save space!!"
    end
    
    if @parameters.include?('--use-system-gcc') or @parameters.include?('-sgcc')
      puts "Using system gcc!! i.e. /usr/bin/gcc"
      @use_system_gcc = true
    end

    if @parameters.include?('--force') or @parameters.include?('-f')
      puts "Foce install mode!"
      @force_install_mode = true
    end

    if @parameters.include?('prereq')
      puts ""
      puts "========================================================="
      puts "| It's recommended to run prereq. installation script!  |"
      puts "|                                                       |"
      puts "| Prereq. installation script: install_prereq.sh        |"
      puts "========================================================="
      puts ""
      exit(0)
    end

  end # def __parse_params__

  def __setup_work_dirs__
    # Working directories
    unless File.directory?(@work_dir_path)
      puts @work_dir_path+" not found, making one..."
      FileUtils.mkdir_p(@work_dir_path)
    end
    @work_dir = File.realpath(@work_dir_path)
    puts "Working directory will be: #{@work_dir}"

    unless File.directory?(@source_dir_path)
      puts @source_dir_path+" not found, making one..."
      FileUtils.mkdir_p(@source_dir_path)
    end
    @source_dir = File.realpath(@source_dir_path)
    puts "Source directory will be: #{@source_dir}"

    unless File.directory?(@pkginfo_dir_path)
      puts @pkginfo_dir_path+" not found, making one..."
      FileUtils.mkdir_p(@pkginfo_dir_path)
    end
    @pkginfo_dir = File.realpath(@pkginfo_dir_path)
    puts "Package information directory will be: #{@pkginfo_dir}"

    @work_dirs = [@work_dir, @source_dir, @pkginfo_dir]

    unless File.directory?(@prefix_dir_path)
      puts @prefix_dir_path+" not found, making one..."
      FileUtils.mkdir_p(@prefix_dir_path)
    end
    @prefix_dir = File.realpath(@prefix_dir_path)
    puts "Prefix confirmed! Everything will be installed at..."
    puts @prefix_dir
    puts ""
      
  end # def __setup_work_dirs__


  def install_pkgs
    # The main installation loop
    @pkgs_to_install.each do |pkg|
      pid = fork do
        require "#{@def_inst_script_dir}/#{SRC_SCRIPT[pkg]}"
        @inst_args["pkgname"] = pkg
        inst = Object.const_get(SRC_CLASS[pkg]).new( @inst_args )
        inst.install
      end
      Process.wait

      # case op_mode
      # # Gcc stuffs
      # when 'gcc'
      #   require "./install_scripts/install_gcc.rb"
      #   inst = InstGCC.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install
      # when 'gcc8'
      #   require "./install_scripts/install_gcc.rb"
      #   inst = InstGCC8.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install
      # when 'gcc9'
      #   require "./install_scripts/install_gcc.rb"
      #   inst = InstGCC9.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install
      # when 'gcc4'
      #   require "./install_scripts/install_gcc.rb"
      #   inst = InstGCC4.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'cmake'
      #   require "./install_scripts/install_cmake.rb"
      #   inst = InstCmake.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'clang'
      #   require "./install_scripts/install_clang.rb"
      #   # puts ">>>>> There is some discrepency with clang now... it might fail <<<<<"
      #   sleep(2)
      #   inst = InstClang.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # # Python stuffs
      # when 'python'
      #   require "./install_scripts/install_python.rb"
      #   inst_python2 = InstPython2.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      #   inst_python2.install
      #   inst_python3 = InstPython3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      #   inst_python3.install
      #   remove_def_python_cmd
      # when 'python2'
      #   require "./install_scripts/install_python.rb"
      #   inst_python2 = InstPython2.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      #   inst_python2.install
      #   remove_def_python_cmd
      # when 'python3'
      #   require "./install_scripts/install_python.rb"
      #   inst_python3 = InstPython3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      #   inst_python3.install
      #   remove_def_python_cmd

      # when 'boost'
      #   require "./install_scripts/install_boost.rb"
      #   inst = InstBoost.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'lua'
      #   pid = fork do 
      #     require "./install_scripts/install_lua.rb"
      #     inst = InstLua.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #     inst.install
      #   end
      #   Process.wait
      # when 'ruby'
      #   require "./install_scripts/install_ruby.rb"
      #   inst = InstRuby.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install
      # when 'ruby3'
      #   require "./install_scripts/install_ruby3.rb"
      #   inst_lua = InstRuby3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst_lua.install

      # # Node stuffs
      # when 'node'
      #   require "./install_scripts/install_node.rb"
      #   inst = InstNode.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install
      # when 'node-lts'
      #   require "./install_scripts/install_node.rb"
      #   inst = InstNodeLTS.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'rust'
      #   require "./install_scripts/install_rust.rb"
      #   inst = InstRust.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'pypy3'
      #   require "./install_scripts/install_pypy.rb"
      #   inst = InstPyPy3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'golang'
      #   require "./install_scripts/install_golang.rb"
      #   inst = InstGolang.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'julia'
      #   require "./install_scripts/install_julia.rb"
      #   inst = InstJulia.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # when 'ROOT'
      #   require "./install_scripts/install_ROOT.rb"
      #   inst = InstROOT.new($prefix_dir, 'x86_64', work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # # MPICH stuffs
      # when 'mpich'
      #   require "./install_scripts/install_mpich.rb"
      #   inst = InstMPICH.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install
      # when 'hydra'
      #   require "./install_scripts/install_hydra.rb"
      #   inst = InstHydra.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
      #   inst.install

      # else
      #   puts "Looks like #{op_mode} has not implemented yet!"
      #   flag_wrong_pkg_given = true
      #   wrong_pkgs.append(op_mode)
      #   puts "Passing #{op_mode} for now..."
      #   puts ""
      # end # case op_mode

    end # @pkgs_to_install.each do |pkg|

    if @flag_wrong_pkg_given
      puts ""
      puts "Looks like there were some pkgs weren't recognized!!"
      puts ""
      puts "Wrong pkgs:"
      puts @wrong_pkgs.join(" ")
      puts ""
      puts "Available modules are..."
      puts ""
      @list_of_progs.each do |pkg|
        puts pkg
      end
    end

  end



end # UnisDevSetup

dev = UnixDevSetup.new(ARGV)

puts ""
puts "Jobs finished!!"
puts ""
