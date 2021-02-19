#!/usr/bin/env ruby

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

require 'open3'

$golang_version = ["1", "15", "7"]


class InstGolang < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false)
    super('golang', prefix, work_dirs, verbose_mode=verbose_mode)
    @source_url = SRC_URL[@pkgname]
    @bootstrap_url = SRC_URL['golang-bootstrap']
    @Version = $golang_version
    @golang_ver = @Version.join('.')
  end

  def install
    puts ""
    puts "Working on #{@pkgname} (#{@golang_ver})!!"
    puts ""

    puts "Bootstraping!!"
    self.Run( "cd #{@src_dir} && wget #{@bootstrap_url} -O ./golang-bootstrap.tgz" )
    self.Run( "cd #{@src_dir} && tar xvf ./golang-bootstrap.tgz")
    self.Run( {"CGO_ENABLED" => "0"}, "cd #{@src_dir}/go/src && ./make.bash")

    bootstrap_dir = File.join(@src_dir, "/go")
    go_dir = File.join(@prefix, '/.opt/go')

    puts "Let's build Golang version (#{@golang_ver})"
    self.Run( "cd #{@src_dir} && git clone #{@source_url} #{go_dir} && cd #{go_dir} && git checkout go#{@golang_ver}" )
    self.Run( {"GOROOT_BOOTSTRAP" => bootstrap_dir}, "cd #{go_dir}/src && ./all.bash" )

    self.WriteInfo

    puts ""
    puts "Ok, golang has been installed!! let's install it!"
    puts "Make sure you add env stuff in your bashrc"
    puts "GOPATH=#{go_dir}"
    puts "Also, make sure to add #{go_dir}/bin to your PATH"
    puts ""

  end

  def WriteInfo
    puts "Writing package info for #{@pkgname}..."
    fp = File.open(@pkginfo_file, 'w')
    compile_info_json = {
      "Package Name" => @pkgname,
      "Version" => $golang_version
    }
    fp.write(compile_info_json.to_json)
    fp.close
  end

end # class InstGolang
