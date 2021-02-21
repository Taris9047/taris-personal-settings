#!/usr/bin/env ruby

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

require 'open3'

$julia_version = ["1", "3", "0"]


class InstJulia < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super('julia', prefix, work_dirs, verbose_mode=verbose_mode)
    @source_url = SRC_URL[@pkgname]
    @target_dir = File.join(@prefix, '/.opt')
    @Version = $julia_version
    @need_sudo = need_sudo
  end

  def install
    puts ""
    puts "Working on #{@pkgname} (#{$julia_version.join('.')})!!"
    puts ""
    puts "Unfortunately, building julia isn't so stable!"
    puts "If it fails, it fails!"
    puts ""

    puts "Installing Julia"
    if !File.directory?(@target_dir)
      if !need_sudo
        self.Run( "mkdir -pv #{@target_dir}" )
      else
        self.Run( "sudo mkdir -pv #{@target_dir}" )
      end
    end
    @src_dir = File.join(@target_dir, '/julia-src')
    if File.directory?(@src_dir)
      puts "Julia src directory found! Deleting it!"
      self.Run( "rm -rf #{@src_dir}")
    end
    self.Run( "cd #{@target_dir} && git clone #{@source_url} #{@src_dir}" )
    self.Run( "cd #{@src_dir} && git checkout v#{@Version.join('.')} && make" )
    julia_bin = File.join(@src_dir, 'julia')

    puts "Compilation finished! Linking executable!"
    julia_bin = File.join(@prefix, '/bin')
    if !File.directory?(julia_bin)
      self.Run("mkdir -pv #{julia_bin}")
    end
    self.Run( "ln -sfv #{julia_bin} #{File.join(julia_bin, 'julia')}" )

    self.WriteInfo

  end

  def WriteInfo
    puts "Writing package info for #{@pkgname}..."
    fp = File.open(@pkginfo_file, 'w')
    compile_info_json = {
      "Package Name" => @pkgname,
      "Version" => @Version
    }
    fp.write(compile_info_json.to_json)
    fp.close
  end

end # class InstJulia
