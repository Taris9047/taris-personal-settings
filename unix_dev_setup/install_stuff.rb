#!/usr/bin/env ruby

# Installation script template class

require 'etc'
require 'open3'
require 'json'

require './fname_parser.rb'
require './get_compiler.rb'

class InstallStuff

  @souce_url = 'some_url-0.0.0'
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

  def initialize(pkgname, prefix, work_dirs=[], ver_check=true)

    @pkgname=pkgname
    @prefix=File.realpath(prefix)
    @build_dir, @src_dir, @pkginfo_dir = work_dirs
    @pkginfo_file = File.join(
      @pkginfo_dir, "#{@pkgname}.info" )
    @check_ver = ver_check

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @Processors = procs
    elsif procs <= 4
      @Processors = procs-1
    end
  end # initialize

  def VerCheck
    # Checking if newer version has rolled out
    if @check_ver
      # Do the version checking
      if File.file?(@pkginfo_file)
        pkf = File.read(@pkginfo_file)
        data_hash = JSON.parse(pkf)
        @ver_current = Version.new(data_hash['Version'].join('.'))
        fnp_dummy = FNParser.new(@source_url)
        @ver_source = Version.new(fnp_dummy.version().join('.'))
        if (@ver_current >= @ver_source)
          puts "===================================================="
          puts "It seems Current version of #{@pkgname} is not so behind!"
          puts "Current #{@pkgname}: "+@ver_current.to_s
          puts "Source database #{@pkgname}: "+@ver_source.to_s
          puts "Consider updating the urls.json or keep it this way!"
          puts "===================================================="
          puts ""
          return true
        else
          puts "===================================================="
          puts "It seems current urls.json has newer version!!"
          puts "Current #{@pkgname}: "+@ver_current.to_s
          puts "Source database #{@pkgname}: "+@ver_source.to_s
          puts "Working on the newer version of #{@pkgname}!!"
          puts "===================================================="
          puts ""
          return false
        end
      else
        puts "===================================================="
        puts "No previous installation info. found for #{@pkgname}"
        puts "Working on the stuff anyway!"
        puts "===================================================="
        puts ""
        return false
      end
    end # if @check_ver

    return false
  end # VerCheck

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
      return VerCheck()
    end
  end
end # class InstallStuff
