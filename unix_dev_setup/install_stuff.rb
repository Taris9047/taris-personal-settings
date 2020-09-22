#!/usr/bin/env ruby

# Installation script template class

require 'etc'
require 'open3'

require './get_compiler.rb'

class InstallStuff

  @souce_url = 'some_url'
  @conf_options = []
  @env = {}

  @Processors = 1

  @pkgname=''
  @prefix=''
  @os_type=''
  @build_dir=''
  @src_dir=''
  @pkginfo_dir=''
  @pkginfo_file=''

  def initialize(pkgname, prefix, work_dirs=[])

    @pkgname=pkgname
    @prefix=File.realpath(prefix)
    @build_dir, @src_dir, @pkginfo_dir = work_dirs
    @pkginfo_file = File.join(
      @pkginfo_dir, '{pkgname}.info'.gsub('{pkgname}', @pkgname) )

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @Processors = procs
    end

  end

  def Run(*args)
    if args.size > 1
      env = args[0]
      cmds = args[1]
      o,e,s = Open3.capture3( env, cmds )
    else
      cmds = args[0]
      o,e,s = Open3.capture3( cmds )
    end

    unless s.success?
      puts "Execution ended up with an error!!"
      puts e
      # TODO: Implement some error handling stuff
      exit(-1)
    end

    return 0

  end

  def WriteInfo
    puts "Writing package info for {pkgname}...".gsub('{pkgname}', @pkgname)
    fp = File.open(@pkginfo_file, 'w')
    env_str = @env.map{|k,v| "{k}={v}".gsub('{k}', k).gsub('{v}', v)}.join("\n")

    unless @conf_options == []
      if @conf_options.join(' ').includes?('-DCMAKE_INSTALL_PREFIX')
        conf_options_str = @conf_options.join(' ')
      else
        conf_options_str = "--prefix="+@prefix+' '+@conf_options.join(' ')
      end
    else
      conf_options_str = "N/A --> Probably the package was not based on automake or cmake."
    end

    compile_info = [
      "Package Name:", @pkgname, "\n",
      "Source file URL:", @source_url, "\n",
      "Configure options:", conf_options_str, "\n",
      "Env Variables:",
      env_str,
    ]
    fp.puts(compile_info.join("\n"))
    fp.close
  end

  def CheckInfo
    if File.file?(@pkginfo_file)
      puts "Oh, it seems {pkgname} was already installed!! Skipping!!".gsub('{pkgname}', @pkgname)
      return true
    end
  end
end # class InstallStuff
