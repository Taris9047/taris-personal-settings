#!/usr/bin/env ruby

# Note that installing old gcc (gcccuda) is disabled due to libc 2.26 issue.
# In fact, we need to apply patch to adopt old gcc source codes to
# follow up the newest changes in libc 2.26

# Default parameters
home_dir = ENV["HOME"]
def_prefix = File.join(home_dir, "/.local")
# def_prefix = File.join("/usr/local")
def_system = "Linux"

# Version
$version = ['1', '0', '0']

# title
$title = "Unix Development Environment setup"

# Verbose mode? Default: false
verbose = false

list_of_progs = [
  'gcc',
  'cudacc',
  'gccold',
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
$not_so_needed_pkgs = ['gccold', 'cudacc', 'node-lts']

list_of_all = list_of_progs - $not_so_stable_pkgs - $not_so_needed_pkgs

aliases = {
  'gcc-old' => 'gccold',
  'gcc-cuda' => 'cudacc',
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
$opt_list = ['--use-clang', 'prereq', '-v', '--verbose', 'purge', '--purge', 'clean', '--clean', '--version']
$permitted_list += $opt_list

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


# Working directories
require 'fileutils'

work_dir_path = "./build"
unless File.directory?(work_dir_path)
  puts work_dir_path+" not found, making one..."
  FileUtils.mkdir_p(work_dir_path)
end
work_dir = File.realpath(work_dir_path)
puts "Working directory will be: #{work_dir}"

source_dir_path = "./src"
unless File.directory?(source_dir_path)
  puts source_dir_path+" not found, making one..."
  FileUtils.mkdir_p(source_dir_path)
end
source_dir = File.realpath(source_dir_path)
puts "Source directory will be: #{source_dir}"

pkginfo_dir_path = "./pkginfo"
unless File.directory?(pkginfo_dir_path)
  puts pkginfo_dir_path+" not found, making one..."
  FileUtils.mkdir_p(pkginfo_dir_path)
end
pkginfo_dir = File.realpath(pkginfo_dir_path)
puts "Package information directory will be: #{pkginfo_dir}"

work_dirs = [work_dir, source_dir, pkginfo_dir]

prefix_dir_path = def_prefix
unless File.directory?(prefix_dir_path)
  puts prefix_dir_path+" not found, making one..."
  FileUtils.mkdir_p(prefix_dir_path)
end
prefix_dir = File.realpath(prefix_dir_path)
puts "Prefix confirmed! Everything will be installed at..."
puts prefix_dir
puts ""

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
  system( "rm -rf #{work_dirs.join(' ')}" )
  system( "rm -rf #{prefix_dir}/bin #{prefix_dir}/lib* #{prefix_dir}/include #{prefix_dir}/opt #{prefix_dir}/.opt" )
  op_mode_list.delete('purge')
  puts "Cleaned up everything!!"
  exit(0)
end

if op_mode_list.include?('--purge')
  puts "Performing purge install..."
  system( 'rm -rf '+work_dirs.join(' ') )
  puts "Deleted every build stuff!!"
  op_mode_list.delete('--purge')
end


if op_mode_list.include?('clean')
  puts "Cleaning up source files and build dirs..."
  system( "rm -rf #{work_dir} #{source_dir}" )
  puts "Cleaned up source files to save space!!"
  op_mode_list.delete('clean')
  exit(0)
end

if op_mode_list.include?('--clean')
  puts "Performing clean install..."
  puts "Cleaning up source files and build dirs..."
  system( "rm -rf #{work_dir} #{source_dir}" )
  puts "Cleaned up source files to save space!!"
  op_mode_list.delete('--clean')
end

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
for o in op_mode_list
  if !$permitted_list.include?(o)
    show_help
    exit(0)
  end
end

# Resolving dependencies
require './dep_resolve.rb'
puts "Checking dependency for #{op_mode_list.join(" ")}"
dep_resolve = DepResolve.new(op_mode_list, pkginfo_dir_path)
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
need_sudo = !File.writable?(prefix_dir)

# TODO: Change class init arguemnt to hash based one.
inst_args = {
  "pkgname" => '',
  "prefix" => prefix_dir,
  "system_arch" => def_system,
  "work_dirs" => work_dirs,
  "need_sudo" => need_sudo,
  "verbose_mode" => verbose,
  "ver_check" => true,
  "use_clang" => clang_mode,
}

# The main installation loop
for op_mode in op_mode_list do
  if op_mode == 'gcc'
    require "./install_gcc.rb"
    inst = InstGCC.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end
  if op_mode == 'cudacc'
    require "./install_gcc.rb"
    inst = InstGCCCuda.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end
  if op_mode == 'gccold'
    require "./install_gcc.rb"
    inst = InstGCCOld.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'clang'
    require "./install_clang.rb"
    # puts ">>>>> There is some discrepency with clang now... it might fail <<<<<"
    sleep(2)
    inst = InstClang.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  # Then Python stuffs
  if op_mode.include?'python'
    require "./install_python.rb"
    if op_mode.include?'2'
      inst_python2 = InstPython2.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      inst_python2.install
    elsif op_mode.include?'3'
      inst_python3 = InstPython3.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      inst_python3.install
    else
      inst_python2 = InstPython2.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      inst_python2.install
      inst_python3 = InstPython3.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose, use_clang=clang_mode)
      inst_python3.install
    end

    puts "Removing 'python' command to preserve system native python..."
    sudo_cmd = ''
    if need_sudo
      sudo_cmd = "sudo"
    end
    del_python_cmd = [
        sudo_cmd,
        "rm -rfv",
        File.join(prefix_dir, "bin/python"),
        File.join(prefix_dir, "bin/ipython")
    ]
    system( del_python_cmd.join(" ") )
  end

  if op_mode == 'boost'
    require "./install_boost.rb"
    inst = InstBoost.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'lua'
    require "./install_lua.rb"
    inst = InstLua.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'ruby'
    require "./install_ruby.rb"
    inst = InstRuby.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'ruby3'
    require "./install_ruby3.rb"
    inst_lua = InstRuby3.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_lua.install
  end

  if op_mode == 'node'
    require "./install_node.rb"
    inst = InstNode.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'node-lts'
    require "./install_node.rb"
    inst = InstNodeLTS.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'rust'
    require "./install_rust.rb"
    inst = InstRust.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'pypy3'
    require "./install_pypy.rb"
    inst = InstPyPy3.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'golang'
    require "./install_golang.rb"
    inst = InstGolang.new(prefix, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'julia'
    require "./install_julia.rb"
    inst = InstJulia.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'ROOT'
    require "./install_ROOT.rb"
    inst = InstROOT.new(prefix_dir, 'x86_64', work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'mpich'
    require "./install_mpich.rb"
    inst = InstMPICH.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'hydra'
    require "./install_hydra.rb"
    inst = InstHydra.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if !list_of_progs.include?(op_mode)
    puts "Looks like #{op_mode} has not implemented yet!"
    flag_wrong_pkg_given = true
    wrong_pkgs.append(op_mode)
    puts "Passing #{op_mode} for now..."
    puts ""
  end

end

if flag_wrong_pkg_given
  puts ""
  puts "Looks like there were some pkgs weren't recognized!!"
  puts ""
  puts "Wrong pkgs:"
  puts wrong_pkgs.join(" ")
  puts ""
  puts "Available modules are..."
  puts ""
  for pkg in list_of_progs
    puts pkg
  end
end

puts ""
puts "Jobs finished!!"
puts ""
