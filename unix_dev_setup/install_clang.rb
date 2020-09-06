#!/usr/bin/env ruby

require 'etc'
require './download.rb'
require './fname_parser.rb'

class InstClang
  @@llvm_repo_url = "http://llvm.org/svn/llvm-project/llvm/trunk"
  @@clang_repo_url = "http://llvm.org/svn/llvm-project/cfe/trunk"
  @@clang_tools_url = "http://llvm.org/svn/llvm-project/clang-tools-extra/trunk"
  @@clang_lld_linker = "http://llvm.org/svn/llvm-project/lld/trunk"
  @@clang_polly_loop = "http://llvm.org/svn/llvm-project/polly/trunk"
  @@openmp_url = "http://llvm.org/svn/llvm-project/openmp/trunk"
  @@compilerRT_url = "http://llvm.org/svn/llvm-project/compiler-rt/trunk"
  @@libcxx_url = "http://llvm.org/svn/llvm-project/libcxx/trunk"
  @@libcxxabi_url = "http://llvm.org/svn/llvm-project/libcxxabi/trunk"

  def initialize
  	procs = Etc.nprocessors
  	if procs > 2
  		@@Processors = procs-1
  	else
  		@@Processors = procs
  	end
  end

  def install_clang (prefix='/usr/local', os_type='Ubuntu', build_dir='./build', source_dir='./src', need_sudo=false)
    puts ""
    puts "Working on Clang!!"
    puts ""

    system( "svn co "+@@llvm_repo_url+" "+source_dir+"/llvm" )
    system( "svn co "+@@clang_repo_url+" "+source_dir+"/llvm/tools/clang" )
    system( "svn co "+@@clang_tools_url+" "+source_dir+"/llvm/tools/clang/tools/extra" )
    system( "svn co "+@@clang_lld_linker+" "+source_dir+"/llvm/tools/lld" )
    system( "svn co "+@@clang_polly_loop+" "+source_dir+"/llvm/tools/polly" )
    system( "svn co "+@@openmp_url+" "+source_dir+"/llvm/projects/openmp" )
    system( "svn co "+@@compilerRT_url+" "+source_dir+"/llvm/projects/compiler-rt" )
    system( "svn co "+@@libcxx_url+" "+source_dir+"/llvm/projects/libcxx" )
    system( "svn co "+@@libcxxabi_url+" "+source_dir+"/llvm/projects/libcxxabi" )

    # Let's build!!
    bld_dir = build_dir+"/clang"
    if Dir.exists?(bld_dir) == false
      puts "Build dir missing.. making one.."
    else
      puts "Build dir exists, cleaning up before work!!"
      system( "rm -rf "+bld_dir )
    end
    system( "mkdir -p "+bld_dir )

    if need_sudo
      inst_cmd = "sudo make install"
    else
      inst_cmd = "make install"
    end
    cmd = [
    	"cd",
    	File.realpath(bld_dir),
    	"&&",
    	"cmake",
    	"-DCMAKE_BUILD_TYPE=Release",
    	"-DLLVM_BUILD_DOCS=OFF",
    	"-DLIBCXX_CXX_ABI=libstdc++",
    	"-DPYTHON_EXECUTABLE:FILEPATH=\"/usr/local/bin/python3\"",
    	File.realpath(source_dir+"/llvm"),
    	"&&",
    	"make -j", @@Processors,
    	"&&",
    	inst_cmd
    ]

    system ( cmd.join(" ") )

  end

end # InstClang

