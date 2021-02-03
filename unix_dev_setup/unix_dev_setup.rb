#!/usr/bin/env ruby

# Note that installing old gcc (gcccuda) is disabled due to libc 2.26 issue.
# In fact, we need to apply patch to adopt old gcc source codes to
# follow up the newest changes in libc 2.26

# Default parameters
home_dir = ENV["HOME"]
def_prefix = File.join(home_dir, "/.local")
# def_prefix = File.join("/usr/local")
def_system = "Linux"

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
  'clang',
  'rust',
  'pypy3',
  'ROOT',
  'mpich',
  'hydra',
]

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
}

# Included clang back into the list. Now it compiles fine!
# --enable-default-pie was the key!
exclude_for_all = [ 'pypy3' ]

flag_wrong_pkg_given = false
wrong_pkgs = []

all_install_list = list_of_progs
for excl in exclude_for_all
  all_install_list.delete(excl)
end
# Setting up operatnion mode
op_mode = nil
if ARGV.empty?
  op_mode_list = all_install_list
elsif ARGV.include?('all')
  op_mode_list = all_install_list
  op_mode_list.delete('all')
else
  op_mode_list = ARGV
end

# Cleaning up op_mode_list with aliases
alias_keys = aliases.keys
for k in alias_keys
  if op_mode_list.include?(k)
    ind = op_mode_list.index(k)
    op_mode_list[ind] = aliases[k]
    puts "Package #{k} was an alias of #{aliases[k]}. Refined the list."
  end
end

# Main title banner
puts "******************************************"
puts "***                                    ***"
puts "*** Unix development environment setup ***"
puts "*** Currently... only works on Ubuntu. ***"
puts "***                                    ***"
puts "******************************************"

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
puts work_dir_path
unless File.directory?(work_dir_path)
  puts work_dir_path+" not found, making one..."
  FileUtils.mkdir_p(work_dir_path)
end
work_dir = File.realpath(work_dir_path)

source_dir_path = "./src"
unless File.directory?(source_dir_path)
  puts source_dir_path+" not found, making one..."
  FileUtils.mkdir_p(source_dir_path)
end
source_dir = File.realpath(source_dir_path)

pkginfo_dir_path = "./pkginfo"
unless File.directory?(pkginfo_dir_path)
  puts pkginfo_dir_path+" not found, making one..."
  FileUtils.mkdir_p(pkginfo_dir_path)
end
pkginfo_dir = File.realpath(pkginfo_dir_path)

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
  clang_mode = true
  op_mode_list.delete('--use-clang')
end

# Some edge cases... cleaning and installing prereq
if op_mode_list.include?('purge')
  puts "Purging everything!!!"
  system( 'rm -rf '+work_dirs.join(' ') )
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

  if op_mode == 'rust'
    require "./install_rust.rb"
    inst = InstRust.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst.install
  end

  if op_mode == 'pypy3'
    require "./install_pypy.rb"
    inst = InstPyPy3.new(prefix_dir, work_dirs, need_sudo)
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
