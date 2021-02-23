#!/usr/bin/env ruby

# TODO: Gotta make it with 'class'
# --> Too much stuff happening here!
#

require 'fileutils'

# Note that installing old gcc (gcccuda) is disabled due to libc 2.26 issue.
# In fact, we need to apply patch to adopt old gcc source codes to
# follow up the newest changes in libc 2.26

# Default parameters
home_dir = ENV["HOME"]
def_prefix = File.join(home_dir, "/.local")
# def_prefix = File.join("/usr/local")

#
# This could be
# i686-linux-gnu
# some arm type etc.
# --> Only applies to gcc.
# We are not building any cross-compilation stuff yet.
#
def_system = 'x86_64-linux-gnu'

# Getting CWD
current_dir = File.expand_path(File.dirname(__FILE__))

# Version
$version = ['1', '0', '1']

# title
$title = "Unix Development Environment setup"

# Verbose mode? Default: false
verbose = false

# Directories
work_dir_root = File.join(current_dir, 'workspace')
work_dir_path = File.join(work_dir_root, 'build')
source_dir_path = File.join(work_dir_root, 'download')
pkginfo_dir_path = File.join(current_dir, 'pkginfo')
work_dir_log = File.join(work_dir_root, 'log')
prefix_dir_path = def_prefix

list_of_progs = [
  'gcc',
  'gcc9',
  'gcc8',
  'python2',
  'python3',
  'boost',
  'lua',
  'ruby',
  'ruby3',
  'node',
  'node-lts',
  'clang',
  'rust',
  'pypy3',
  'ROOT',
  'mpich',
  'hydra',
  'golang',
  'julia',
]

$not_so_stable_pkgs = ['pypy3', 'clang', 'ROOT', 'julia']
$not_so_needed_pkgs = ['gccold', 'cudacc', 'node-lts', 'ruby3']

list_of_all = list_of_progs - $not_so_stable_pkgs - $not_so_needed_pkgs

aliases = {
  'gcc-old' => 'gcc9',
  'gcc-cuda' => 'gcc8',
  'python' => 'python3',
  'Boost' => 'boost',
  'ruby2' => 'ruby',
  'pypy' => 'pypy3',
  'Rust' => 'rust',
  'root' => 'ROOT',
  'MPICH' => 'mpich',
  'go' => 'golang',
  'nodejs' => 'node',
  'nodejslts' => 'node-lts',
  'node.js' => 'node',
  'all' => list_of_all,
}

$permitted_list = list_of_progs + aliases.keys
$opt_list = [
  '--use-clang', 'prereq', '-v', '--verbose', 
  'purge', '--purge', 'clean', '--clean', '--version', 
  '--use-system-gcc', '-sgcc'
]
$permitted_list += $opt_list

require_relative './utils/run_console.rb'
Con = RunConsole.new(verbose=true, logf_dir=work_dir_log)

# Main title banner
def main_title
puts "******************************************"
puts ""
puts " #{$title}"
puts " Version (#{$version.join('.')})"
puts ""
puts "******************************************"
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

# Included clang back into the list. Now it compiles fine!
# --enable-default-pie was the key!
flag_wrong_pkg_given = false
wrong_pkgs = []

# Setting up operatnion mode
op_mode_list = []
if ARGV.empty?
  op_mode_list = list_of_all
else
  op_mode_list = ARGV
end

# Cleaning up op_mode_list with aliases
for k in aliases.keys
  if op_mode_list.include?(k)
    if aliases[k].instance_of? String
      ind_of_k = op_mode_list.index(k)
      op_mode_list[ind_of_k] = aliases[k]
    elsif aliases[k].instance_of? Array
      op_mode_list.delete(k)
      op_mode_list += aliases[k]
    end
  end
end

if op_mode_list.include?('--version')
  puts "(UDE set) #{title} Ver. #{$version.join('.')}"
  exit(0)
end

if op_mode_list.include?('--help')
  show_help
end

# Handling Verbose mode
if op_mode_list.include?('-v') or op_mode_list.include?('--verbose')
  verbose = true
  puts ""
  puts "*** Verbose ON! It will be pretty loud! ***"
  puts "* Note that some compilation jobs hang up with Verbose ON. *"
  puts ""
  op_mode_list.delete('-v')
  op_mode_list.delete('--verbose')
end


# Use clang as compiler
# Currently, only Python accepts those stuff.
#
clang_mode = false
if op_mode_list.include?('--use-clang')
  puts "Clang mode turned on. Some packages will be compiled with system llvm-clang."
  clang_mode = true
  op_mode_list.delete('--use-clang')
end

# Some edge cases... cleaning and installing prereq
if op_mode_list.include?('purge')
  puts "Purging everything!!!"
  # system( "rm -rf #{work_dirs.join(' ')}" )
  FileUtils.rm_rf("#{work_dir_root}" )
  prefix_kill_list = [
    "bin", "lib", "lib64", "libexec", "include",
    "opt", ".opt", "man", "etc", "state",
  ]
  prefix_kill_list.each do |k|
    FileUtils.rm_rf(File.join(prefix_dir_path, k))
  end
  op_mode_list.delete('purge')
  puts "Cleaned up everything!!"
  exit(0)
end

if op_mode_list.include?('--purge')
  puts "Performing purge install..."
  Con.Run( 'rm -rf '+work_dirs.join(' ') )
  puts "Deleted every build stuff!!"
  op_mode_list.delete('--purge')
end


if op_mode_list.include?('clean')
  puts "Cleaning up source files and build dirs..."
  Con.Run( "rm -rf #{work_dir} #{source_dir}" )
  puts "Cleaned up source files to save space!!"
  op_mode_list.delete('clean')
  exit(0)
end

if op_mode_list.include?('--clean')
  puts "Performing clean install..."
  puts "Cleaning up source files and build dirs..."
  Con.Run( "rm -rf #{work_dir} #{source_dir}" )
  puts "Cleaned up source files to save space!!"
  op_mode_list.delete('--clean')
end

use_system_gcc = false
if op_mode_list.include?('--use-system-gcc') or op_mode_list.include?('sgcc')
  puts "Using system gcc!! i.e. /usr/bin/gcc"
  use_system_gcc = true
  op_mode_list.delete('--use-system-gcc').delete('-sgcc')
end


# Working directories

unless File.directory?(work_dir_path)
  puts work_dir_path+" not found, making one..."
  FileUtils.mkdir_p(work_dir_path)
end
work_dir = File.realpath(work_dir_path)
puts "Working directory will be: #{work_dir}"

unless File.directory?(source_dir_path)
  puts source_dir_path+" not found, making one..."
  FileUtils.mkdir_p(source_dir_path)
end
source_dir = File.realpath(source_dir_path)
puts "Source directory will be: #{source_dir}"

unless File.directory?(pkginfo_dir_path)
  puts pkginfo_dir_path+" not found, making one..."
  FileUtils.mkdir_p(pkginfo_dir_path)
end
pkginfo_dir = File.realpath(pkginfo_dir_path)
puts "Package information directory will be: #{pkginfo_dir}"

work_dirs = [work_dir, source_dir, pkginfo_dir]

unless File.directory?(prefix_dir_path)
  puts prefix_dir_path+" not found, making one..."
  FileUtils.mkdir_p(prefix_dir_path)
end
$prefix_dir = File.realpath(prefix_dir_path)
puts "Prefix confirmed! Everything will be installed at..."
puts $prefix_dir
puts ""


if op_mode_list.include?('prereq')
  puts ""
  puts "========================================================="
  puts "| It's recommended to run prereq. installation script!  |"
  puts "|                                                       |"
  puts "| Prereq. installation script: install_prereq.sh        |"
  puts "========================================================="
  puts ""
  exit(0)
end

# When some stupid parameter was given...
op_mode_list.each do |o|
  if !$permitted_list.include?(o)
    show_help
    exit(0)
  end
end

# Resolving dependencies
require './utils/utils.rb'
puts "Checking dependency for #{op_mode_list.join(" ")}"
dep_resolve = DepResolve.new(
  op_mode_list, pkginfo_dir_path, use_system_gcc)
op_mode_list = dep_resolve.GetInstList()

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
need_sudo = !File.writable?($prefix_dir)

# TODO: Change class init arguemnt to hash based one.
inst_args = {
  "pkgname" => '',
  "prefix" => $prefix_dir,
  "system_arch" => def_system,
  "work_dirs" => work_dirs,
  "need_sudo" => need_sudo,
  "verbose_mode" => verbose,
  "ver_check" => true,
  "use_clang" => clang_mode,
}

# Remove default python cmd
def remove_def_python_cmd
  puts "Removing 'python' command to preserve system native python..."
  sudo_cmd = ''
  if !File.writable?($prefix_dir)
    sudo_cmd = "sudo"
  end
  del_python_cmd = [
      sudo_cmd,
      "rm -rfv",
      File.join($prefix_dir, "bin/python"),
      File.join($prefix_dir, "bin/ipython")
  ]
  system( del_python_cmd.join(" ") )
end

# The main installation loop
op_mode_list.each do |op_mode|
  case op_mode

  # Gcc stuffs
  when 'gcc'
    require "./install_scripts/install_gcc.rb"
    inst = InstGCC.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  when 'gcc8'
    require "./install_scripts/install_gcc.rb"
    inst = InstGCC8.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  when 'gcc9'
    require "./install_scripts/install_gcc.rb"
    inst = InstGCC9.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'cmake'
    require "./install_scripts/install_cmake.rb"
    inst = InstCmake.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'clang'
    require "./install_scripts/install_clang.rb"
    # puts ">>>>> There is some discrepency with clang now... it might fail <<<<<"
    sleep(2)
    inst = InstClang.new($prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  # Python stuffs
  when 'python'
    require "./install_scripts/install_python.rb"
    inst_python2 = InstPython2.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
    inst_python2.install
    inst_python3 = InstPython3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
    inst_python3.install
    remove_def_python_cmd
  when 'python2'
    require "./install_scripts/install_python.rb"
    inst_python2 = InstPython2.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
    inst_python2.install
    remove_def_python_cmd
  when 'python3'
    require "./install_scripts/install_python.rb"
    inst_python3 = InstPython3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
    inst_python3.install
    remove_def_python_cmd

  when 'boost'
    require "./install_scripts/install_boost.rb"
    inst = InstBoost.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'lua'
    require "./install_scripts/install_lua.rb"
    inst = InstLua.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'ruby'
    require "./install_scripts/install_ruby.rb"
    inst = InstRuby.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  when 'ruby3'
    require "./install_scripts/install_ruby3.rb"
    inst_lua = InstRuby3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_lua.install
  
  # Node stuffs
  when 'node'
    require "./install_scripts/install_node.rb"
    inst = InstNode.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  when 'node-lts'
    require "./install_scripts/install_node.rb"
    inst = InstNodeLTS.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'rust'
    require "./install_scripts/install_rust.rb"
    inst = InstRust.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'pypy3'
    require "./install_scripts/install_pypy.rb"
    inst = InstPyPy3.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'golang'
    require "./install_scripts/install_golang.rb"
    inst = InstGolang.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'julia'
    require "./install_scripts/install_julia.rb"
    inst = InstJulia.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  when 'ROOT'
    require "./install_scripts/install_ROOT.rb"
    inst = InstROOT.new($prefix_dir, 'x86_64', work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  # MPICH stuffs
  when 'mpich'
    require "./install_scripts/install_mpich.rb"
    inst = InstMPICH.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  when 'hydra'
    require "./install_scripts/install_hydra.rb"
    inst = InstHydra.new($prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install

  else
    puts "Looks like #{op_mode} has not implemented yet!"
    flag_wrong_pkg_given = true
    wrong_pkgs.append(op_mode)
    puts "Passing #{op_mode} for now..."
    puts ""
  end # case op_mode

end # for op_mode in op_mode_list do

if flag_wrong_pkg_given
  puts ""
  puts "Looks like there were some pkgs weren't recognized!!"
  puts ""
  puts "Wrong pkgs:"
  puts wrong_pkgs.join(" ")
  puts ""
  puts "Available modules are..."
  puts ""
  list_of_progs.each do |pkg|
    puts pkg
  end
end

puts ""
puts "Jobs finished!!"
puts ""
