#!/usr/bin/env ruby

# Installs Boost
# http://www.boost.org/

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

$b2_opts = [
  "address-model=64",
  "architecture=x86",
  "--build-dir=build",
  ]

class InstBoost < InstallStuff

  def initialize(prefix, work_dirs, need_sudo)
    super('boost', prefix, work_dirs)

    @need_sudo = need_sudo

    URL = SRC_URL.new
    @source_url = URL[@pkgname]
    @b2_opts = $b2_opts

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @env = gc.get_env_settings
  end

  def install

    if self.CheckInfo
      return
    end

    puts "Downloading the source from #{@source_url}"
    dl = Download.new(@source_url, @src_dir)
    src_tarball_path = dl.GetPath

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    src_extracted_folder = File.join(@build_dir,src_tarball_bname)
    if File.exists?(src_extracted_folder)
      puts "Previous Boost installation exists"
    else
      self.Run( ["tar xvf", src_tarball_path, "-C", @build_dir].join(' ') )
    end

    # Boost is kinda simple. just build within the directory!
    if @need_sudo
      inst_cmd = "sudo ./b2 install"
    else
      inst_cmd = "./b2 install"
    end

    @b2_opts << "--prefix=#{@prefix}"
    @b2_opts << "stage"

    cmds = [
      "cd",
      src_extracted_folder, "&&",
      "./bootstrap.sh", "--prefix="+@prefix, "&&",
      "./b2", @b2_opts.join(" "), "&&",
      inst_cmd
    ]

    self.Run( @env, cmds.join(" ") )

    @conf_options = @b2_opts

    self.WriteInfo

  end # install


end # class InstBoost
