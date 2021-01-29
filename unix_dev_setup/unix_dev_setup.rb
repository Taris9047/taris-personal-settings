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

# Make everything lowercase.. --> not really needed now.
# op_mode_list.each_with_index do |op_mode, i|
#   op_mode_list[i] = op_mode_list[i].downcase
# end

# Doing some re-organization
# TODO: This dependency stuff needs to be revised!
# Gcc
if op_mode_list.include?('gcc')
  op_mode_list.delete('gcc')
  op_mode_list.insert(0, 'gcc')
end

# In case of CudaCC
if op_mode_list.include?('cudacc') and op_mode_list.include?('gcc')
  op_mode_list.delete('cudacc')
  op_mode_list.insert(op_mode_list.index('gcc')+1, 'cudacc')
end

# In case of node
# if op_mode_list.include?('node') and op_mode_list.include?('gccold')
#   op_mode_list.delete('node')
#   op_mode_list.insert(op_mode_list.index('gccold')+1, 'node')
# end

# In case of Clang -- which is not in the list at this moment...
if op_mode_list.include?('clang') and op_mode_list.include?('python3')
  op_mode_list.delete('clang')
  op_mode_list.insert(op_mode_list.index('python3')+1, 'clang')
end

# In case of pypy -- We need hg from python2
# if op_mode_list.include?('python2')
#   if op_mode_list.include?('pypy3')
#     op_mode_list.delete('pypy3')
#     op_mode_list.insert(op_mode_list.index('python2')+1, 'pypy')
#   end
# end

# List packages to install
puts "List of packages to install..."
puts ""
for pkg in op_mode_list
  puts pkg
end
puts ""





# Checking if the destination directory is writable or not.
need_sudo = !File.writable?(prefix_dir)

# The main installation loop
for op_mode in op_mode_list do
  if op_mode == 'gcc'
    require "./install_gcc.rb"
    inst_gcc = InstGCC.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst_gcc.install
  end
  if op_mode == 'cudacc'
    require "./install_gcc.rb"
    inst_gcc = InstGCCCuda.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst_gcc.install
  end
  if op_mode == 'gccold'
    require "./install_gcc.rb"
    inst_gcc = InstGCCOld.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst_gcc.install
  end


  if op_mode == 'clang'
    require "./install_clang.rb"
    # puts ">>>>> There is some discrepency with clang now... it might fail <<<<<"
    sleep(2)
    inst_clang = InstClang.new(prefix_dir, def_system, work_dirs, need_sudo, verbose_mode=verbose)
    inst_clang.install
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
    inst_boost = InstBoost.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_boost.install
  end

  if op_mode == 'lua'
    require "./install_lua.rb"
    inst_lua = InstLua.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_lua.install
  end

  if op_mode == 'ruby'
    require "./install_ruby.rb"
    inst_lua = InstRuby.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_lua.install
  end

  if op_mode == 'ruby3'
    require "./install_ruby3.rb"
    inst_lua = InstRuby3.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_lua.install
  end


  if op_mode == 'node'
    require "./install_node.rb"
    inst_node = InstNode.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_node.install
  end

  if op_mode == 'rust'
    require "./install_rust.rb"
    inst_rust = InstRust.new(prefix_dir, work_dirs, need_sudo, verbose_mode=verbose)
    inst_rust.install
  end

  if op_mode == 'pypy3'
    require "./install_pypy.rb"
    inst_pypy = InstPyPy3.new(prefix_dir, work_dirs, need_sudo)
    inst_pypy.install
  end

  unless list_of_progs.include?(op_mode)
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
