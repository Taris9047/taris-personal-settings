#!/usr/bin/env ruby

# Installation script template class

require 'etc'
require 'open3'
require 'json'
require './fname_parser.rb'

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
      @pkginfo_dir, "#{@pkgname}.info" )

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @Processors = procs
    end

  end

  def Run(*args)

    if args[0].class == Hash
      env = args[0]
      cmds = args[1]
      opts = Array(args[2...])
    elsif args[0].class == Array
      env = {}
      cmds = args[0].join(' ')
      opts = Array(args[1...])
    elsif args[0].class == String
      env = {}
      cmds = args[0]
      opts = Array(args[1...])
    end

    o, e, s = Open3.capture3( env, cmds )

    log_file_name = @pkgname+'.log'
    if opts.length() >= 1
      log_file_name = opts[0]+'.log'
    end

    log_file = File.join(@pkginfo_dir, log_file_name)
    unless File.file?(log_file)
      fp = File.open(log_file, 'w')
    else
      fp = File.open(log_file, 'a')
    end
    fp.puts(o)
    fp.close
    # puts "Log file for #{@pkgname} has been saved at #{log_file_name}"

    unless s.success?
      puts "Execution ended up with an error!!"
      puts e
      # TODO: Implement some error handling stuff
      exit(-1)
    end

    if opts.include?('-v') or opts.include?('verbose')
      puts o
    end

    return 0

  end

  def WriteInfo
    puts "Writing package info for #{@pkgname}..."
    fp = File.open(@pkginfo_file, 'w')
    env_str = @env.map{|k,v| "{k}={v}".gsub('{k}', k).gsub('{v}', v)}.join("\n")

    unless @conf_options == []
      if @conf_options.join(' ').include?('-DCMAKE_INSTALL_PREFIX')
        conf_options_str = @conf_options.join(' ')
      else
        conf_options_str = "--prefix=#{@prefix} "+@conf_options.join(' ')
      end
    else
      conf_options_str = "N/A --> Probably the package was not based on automake or cmake."
    end

    fnp = FNParser.new(@source_url)
    compile_info_json = {
      "Package Name" => @pkgname,
      "Source file URL" => @source_url,
      "Version" => fnp.version(),
      "Config options" => conf_options_str,
      "Env Variables" => env_str,
    }
    fp.write(compile_info_json.to_json)
    # fp.puts(compile_info.join("\n"))
    fp.close
  end

  def CheckInfo
    if File.file?(@pkginfo_file)
      puts "Oh, it seems {pkgname} was already installed!! Skipping!!".gsub('{pkgname}', @pkgname)
      return true
    end
  end
end # class InstallStuff
