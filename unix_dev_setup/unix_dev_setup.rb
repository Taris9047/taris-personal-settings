#!/usr/bin/env ruby

# Note that installing old gcc (gcccuda) is disabled due to libc 2.26 issue.
# In fact, we need to apply patch to adopt old gcc source codes to
# follow up the newest changes in libc 2.26



# Default parameters
home_dir = ENV["HOME"]
def_prefix = File.join(home_dir, "/.local")
# def_prefix = File.join("/usr/local")
def_system = "Ubuntu"

list_of_progs = [
    'gcc',
    'cudacc',
    'gccold',
    'python2',
    'python3',
    'boost',
    'lua',
    'ruby',
    'node'
    ]

# Operatnion mode
op_mode = nil
if ARGV.empty? or ARGV[0].downcase == 'all'
  op_mode_list = list_of_progs
else
  op_mode_list = ARGV
end

op_mode_list.each_with_index do |op_mode, i|
  op_mode_list[i] = op_mode_list[i].downcase
end

# Some edge cases... cleaning and installing prereq
if op_mode_list.include?('clean')
  system( 'rm -rvf '+work_dir+' '+source_dir )
  puts "Cleaned up everything!!"
  exit(0)
end

if op_mode_list.include?('prereq')
  require "./install_prereq.rb"
  inst_prereq
  exit(0)
end

# Doing some re-organization
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
if op_mode_list.include?('node') and op_mode_list.include?('gccold')
  op_mode_list.delete('node')
  op_mode_list.insert(op_mode_list.index('gccold')+1, 'node')
end

# In case of Clang -- which is not in the list at this moment...
if op_mode_list.include?('clang') and op_mode_list.include?('python3')
  op_mode_list.delete('clang')
  op_mode_list.insert(op_mode_list.index('python3')+1, 'clang')
end

# Working directories
require 'fileutils'

puts "Unix development environment setup for me..."
puts "Currently... only works on Ubuntu"
puts ""

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

prefix_dir_path = def_prefix
unless File.directory?(prefix_dir_path)
	puts prefix_dir_path+" not found, making one..."
	FileUtils.mkdir_p(prefix_dir_path)
end
prefix_dir = File.realpath(prefix_dir_path)
puts "Prefix confirmed! Everything will be installed at..."
puts prefix_dir
puts ""

# Checking if the destination directory is writable or not.
need_sudo = !File.writable?(prefix_dir)



# The main installation loop
for op_mode in op_mode_list do
  if op_mode == 'gcc'
    require "./install_gcc.rb"
    inst_gcc = InstGCC.new(prefix_dir, def_system, work_dir, source_dir, need_sudo)
    inst_gcc.install
  end
  if op_mode == 'cudacc'
    require "./install_gcc.rb"
    inst_gcc = InstGCCCuda.new(prefix_dir, def_system, work_dir, source_dir, need_sudo)
    inst_gcc.install
  end
  if op_mode == 'gccold'
    require "./install_gcc.rb"
    inst_gcc = InstGCCOld.new(prefix_dir, def_system, work_dir, source_dir, need_sudo)
    inst_gcc.install
  end


  if op_mode == 'clang'
    require "./install_clang.rb"
    puts ">>>>> There is some discrepency with clang now... it might fail <<<<<"
    sleep(2)
    inst_clang = InstClang.new
    inst_clang.install_clang(prefix_dir, def_system, work_dir, source_dir, need_sudo)
  end

  # Then Python stuffs
  if op_mode.include?'python'
    require "./install_python.rb"
    if op_mode.include?'2'
      inst_python2 = InstPython2.new(prefix_dir, work_dir, source_dir, need_sudo)
      inst_python2.install
    elsif op_mode.include?'3'
      inst_python3 = InstPython3.new(prefix_dir, work_dir, source_dir, need_sudo)
      inst_python3.install
    else
      inst_python2 = InstPython2.new(prefix_dir, work_dir, source_dir, need_sudo)
      inst_python2.install
      inst_python3 = InstPython3.new(prefix_dir, work_dir, source_dir, need_sudo)
      inst_python3.install
    end

    puts "Removing 'python' to preserve system native python..."
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
    inst_boost = InstBoost.new(prefix_dir, work_dir, source_dir, need_sudo)
    inst_boost.install
  end

  if op_mode == 'lua'
    require "./install_lua.rb"
    inst_lua = InstLua.new(prefix_dir, work_dir, source_dir, need_sudo)
    inst_lua.install
  end

  if op_mode == 'ruby'
    require "./install_ruby.rb"
    inst_lua = InstRuby.new(prefix_dir, work_dir, source_dir, need_sudo)
    inst_lua.install
  end

  if op_mode == 'node'
    require "./install_node.rb"
    inst_node = InstNode.new(prefix_dir, work_dir, source_dir, need_sudo)
    inst_node.install
  end

end

