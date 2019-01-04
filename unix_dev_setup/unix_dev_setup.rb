#!/usr/bin/env ruby

# Note that installing old gcc (gcccuda) is disabled due to libc 2.26 issue.
# In fact, we need to apply patch to adopt old gcc source codes to
# follow up the newest changes in libc 2.26

require "./install_gcc.rb"
require "./install_gcc8.rb"
require "./install_python.rb"
require "./install_boost.rb"
require "./install_lua.rb"
require "./install_ruby.rb"
require "./install_clang.rb"

# Default parameters
def_prefix = "/usr/local"
def_system = "Ubuntu"

# Operatnion mode
op_mode = nil
if ARGV.empty?
  op_mode = 'all'
else
  op_mode = ARGV[0]
end

# Prerequisites
ubuntu_pkgs = [
  "build-essential",
  "flex",
  "bison",
  "zlib1g",
  "zlib1g-dev",
  "openssl",
  "libssl-dev",
  "libsqlite3-dev",
  "libncursesw5-dev",
  "libreadline-dev",
  "libssl-dev",
  "libgdbm-dev",
  "libc6-dev",
  "libsqlite3-dev",
  "tk-dev",
  "libbz2-dev",
  "libicu-dev",
  "autotools-dev",
  "python3-dev",
  "libncurses5-dev",
  "libxml2-dev",
  "libedit-dev",
  "swig",
  "doxygen",
  "graphviz",
  "xz-utils",
  "ruby-dev",
]

# Other dev tools
ubuntu_some_more_tools = [
  "valgrind",
  "valkyrie",
  "cmake",
  "cmake-gui",
  "autoconf",
  "automake",
  "vim",
  "emacs",
  "ttf-bitstream-vera",
  "subversion",
  "git",
  "wget",
  "curl",
]

# Ruby gems
ubuntu_ruby_gems = [
  "rsense",
]

# Working directory
work_dir = "./build"
source_dir = "./src"

if op_mode == 'clean'
  system( 'rm -rvf '+work_dir+' '+source_dir )
  puts "Cleaned up everything!!"
  exit(0)
end

puts "Unix development environment setup for me..."
puts "Current only works on Ubuntu"
puts ""
puts "Installing Prerequisites!!"
system ( 'sudo apt-get -y update && sudo apt-get -y upgrade' )
cmd_ary = ["sudo apt-get -y install"] + ubuntu_pkgs + ubuntu_some_more_tools
cmd = cmd_ary.join(" ")
system( cmd )

# Installing some gems
puts "Installing some gems"
cmd = ["sudo", "gem", "install"]+ubuntu_ruby_gems
system( cmd.join(" ") )

# Check work working directory
if Dir.exist?(work_dir) == false
  puts "Generating build directory at "+work_dir
  system( "mkdir "+work_dir )
end
if Dir.exist?(source_dir) == false
  puts "Generating source directory at "+source_dir
  system( "mkdir "+source_dir )
end

# Let's install gcc first
if op_mode.downcase == 'gcc'
  inst_gcc = InstGCC.new
  inst_gcc.install_gcc(def_prefix, def_system, work_dir, source_dir)
end
if op_mode.downcase == 'gcc8'
  inst_gcc = InstGCC8.new
  inst_gcc.install_gcc(def_prefix, def_system, work_dir, source_dir)
end
if op_mode.downcase == 'cudacc'
  # inst_gcc = InstGCCCuda.new
  # inst_gcc.install_gcc(def_prefix, def_system, work_dir, source_dir)
end

if op_mode.downcase == 'clang'
  inst_clang = InstClang.new
  inst_clang.install_clang(def_prefix, def_system, work_dir, source_dir)
end

# Then Python stuffs
if op_mode.downcase.include?'python'
  if op_mode.include?'2'
    inst_python2 = InstPython2.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
    inst_python2.install
  elsif op_mode.include?'3'
    inst_python3 = InstPython3.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
    inst_python3.install
  else
    inst_python2 = InstPython2.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
    inst_python2.install
    inst_python3 = InstPython3.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
    inst_python3.install
  end

    puts "Removing 'python' to preserve system native python..."
  del_python_cmd = [
    "sudo",
    "rm -rfv",
    File.join(File.realpath(def_prefix), "bin/python"),
    File.join(File.realpath(def_prefix), "bin/ipython")
  ]
  system( del_python_cmd.join(" ") )
end

if op_mode.downcase == 'boost'
  inst_boost = InstBoost.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
  inst_boost.install
end

if op_mode.downcase == 'lua'
  inst_lua = InstLua.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
  inst_lua.install
end

if op_mode.downcase == 'ruby'
  inst_lua = InstRuby.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
  inst_lua.install
end

if op_mode.downcase == 'all'
  inst_gcc = InstGCC.new
  inst_gcc.install_gcc(def_prefix, def_system, work_dir, source_dir)
  inst_clang = InstClang.new
  inst_clang.install_clang(def_prefix, def_system, work_dir, source_dir)

  # inst_python2 = InstPython2.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
  # inst_python2.install

  inst_python3 = InstPython3.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
  inst_python3.install

  inst_boost = InstBoost.new(def_prefix, File.realpath(work_dir), File.realpath(source_dir))
  inst_boost.install

  puts "Removing 'python' to preserve system native python..."
  del_python_cmd = [
    "sudo",
    "rm -rfv",
    File.join(File.realpath(def_prefix), "bin/python"),
    File.join(File.realpath(def_prefix), "bin/ipython")
  ]
  system( del_python_cmd.join(" ") )
end
