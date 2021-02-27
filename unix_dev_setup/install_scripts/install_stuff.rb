#!/usr/bin/env ruby

# Installation script template class

require 'find'
require 'etc'
require 'open3'
require 'json'
require 'fileutils'
require 'tty-spinner'

require_relative '../utils/utils.rb'

class InstallStuff < RunConsole

  @souce_url = 'https://some_site.net.org/some_url-0.0.0'

  def initialize(pkgname, prefix, work_dirs=[], ver_check=true, verbose_mode=false)

    @pkgname=pkgname
    @prefix=File.realpath(prefix)
    @build_dir, @src_dir, @pkginfo_dir = work_dirs
    @pkginfo_file = File.join(@pkginfo_dir, "#{@pkgname}.info" )
    @check_ver = ver_check
    @verbose = verbose_mode
    @Installed_files = []
    super(
      verbose: @verbose, 
      logf_dir: @pkginfo_dir, 
      logf_name: "#{@pkgname}.log")
    @run_install = true

    # Setting up processors
    @Processors = Etc.nprocessors

    @Version = SRC_VER[@pkgname].join('.')

  end # initialize

  def install
    self.ShowTitle
    self.SetURL
    if @run_install == true
      self.do_install
    end
  end

  def SetURL
    @source_url = SRC_URL[@pkgname]
    @ver_source = @Version
    # Version Checking
    if File.file?(@pkginfo_file) and self.VerCheck()
      @run_install = false
    else
      self.ShowInstallInfo
    end
  end

  def ShowTitle
    puts ""
    puts "Working on #{@pkgname} (#{@Version})!!"
    puts ""
  end

  def ShowInstallInfo
    env_txt = ''
    if @env == nil
      return 0
    end
    @env.each do |k, flag|
      env_txt += "#{k}: #{flag}\n"
    end
    
    info_txt = %{
>> Installation Destination:
#{@prefix}

>> Config options
--prefix=#{@prefix}
#{@conf_options.join("\n")}

>> Compiler options (env)
#{env_txt}

    }
    puts info_txt
    sleep(2.5)
  end

  def VerCheck
    # Checking if newer version has rolled out
    if @check_ver
      # Do the version checking
      @ver_source = SRC_VER[@pkgname]
      if File.file?(@pkginfo_file)
        data_hash = JSON.parse(File.read(@pkginfo_file))
        @ver_current = Version.new(data_hash['Version'].join('.'))
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

  # Invoke it only if you have a valid prefix
  def CompilerSet(cflags='', cxxflags='')
    if !File.directory? @prefix
      puts "Cannot set a correct compiler with give path!"
      puts "Given path: #{@prefix}"
      puts "Reverting to default search path... /usr/bin"
      compiler_path = '/usr/bin'
    else
      compiler_path = File.join(@prefix, 'bin')
    end
    gc = GetCompiler.new(
      cc_path=compiler_path, cxx_path=compiler_path,
      cflags=cflags, cxxflags=cxxflags)
    @cmake_comp_settings = gc.get_cmake_settings
    @env = gc.get_env_settings
  end

  # Qt5 existence check. (more likely qmake executable.)
  def qt5_qmake()
    qmake_cmd = nil
    qmake_cmd_candidates = ['qmake', 'qmake5', 'qmake-qt5', 'qt5-qmake']
    qmake_cmd_candidates.each do |qm_cmd|
      qmake_cmd = UTILS.which(qm_cmd)
      if qmake_cmd
        return qmake_cmd
        break
      end
    end
    return nil
  end

  # Collects the list of files installed
  # TODO: make it faster. This is super brute force now...
  def InstallFiles(build_dir, inst_cmd)
    files_before_install = Find.find(@prefix).collect { |_| _ if !_.include?'__pycache__'  }

    self.Run("cd #{File.join(build_dir)} && #{inst_cmd}")

    files_after_install = Find.find(@prefix).collect { |_| _ if !_.include?'__pycache__'  }
    
    @Installed_files = files_after_install - files_before_install
  end

  def uninstall
    puts "Uninstalling #{@pkgname} ... "
    spinner = TTY::Spinner("[Uninstalling] ... :spinner", format: :bouncing_ball)
    spinner.auto_rotate
    @Installed_files.each do |file|
      FileUtils.rm_rf(file)
    end
    FileUtils.rm_rf(@pkginfo_file)
    spinner.stop
    puts "#{@pkgname} uninstalled successfully!"
  def

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
      "Installed Files" => @Installed_files,
    }
    fp.write(compile_info_json.to_json)
    # fp.puts(compile_info.join("\n"))
    fp.close
  end
end # class InstallStuff
