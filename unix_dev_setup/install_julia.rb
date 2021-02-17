#!/usr/bin/env ruby

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

require 'open3'

$julia_version = ["1", "3", "0"]


class InstJulia < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super('julia', prefix, work_dirs, verbose_mode=verbose_mode)
    @source_url = SRC_URL[@pkgname]
    @target_dir = File.join(@prefix, '/opt')
    @Version = $julia_version
    @golang_ver = @Version.join('.')
    @need_sudo = need_sudo
  end

  def install
    puts ""
    puts "Working on #{@pkgname} (#{@golang_ver})!!"
    puts ""

    puts "Installing Julia"
    if !File.directory?(@target_dir)
      if !need_sudo
        self.Run( "mkdir -pv #{@target_dir}" )
      else
        self.Run( "sudo mkdir -pv #{@target_dir}" )
      end
    end
    self.Run( "cd #{@target_dir} && git clone #{@source_url} ./julia-src" )
    @src_dir = File.join(@target_dir, '/julia-src')
    self.Run( "cd #{@src_dir} && git checkout v#{@Version.join('.')} && make" )
    julia_bin = File.join(@src_dir, 'julia')

    puts "Compilation finished! Linking executable!"
    self.Run( "ln -sfv #{julia_bin} #{@prefix}/bin/julia" )

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

end # class InstGolang
