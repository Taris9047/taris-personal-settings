#!/usr/bin/env ruby

# this will handle Rust

# Rust installation is rather simple and their package system actually wants to use
# own directory at $HOME/.cargo. Therefore, we will not even list any source file.
#
# Also, the .info file will not contain any useful info. since every installation
# step is handled by the install system.

require './install_stuff.rb'

require 'json'
require 'open3'

# Anyway, here's some useful tools to install
$rust_utils_to_install = [
  "exa", "bat", "rm-improved", "diskonaut", "lsd",
  "cargo-update", "starship", "tokei", "fd-find", "procs",
  "du-dust", "ripgrep", "hyperfine", "ytop", "grex", "zoxide",
  "gitui", "eureka", "ddh",
  "nu"
]

# The command installs rust with default option.
$rust_inst_cmd = "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y"

class InstRust < InstallStuff

  def initialize(prefix='', work_dirs=[], need_sudo=false, verbose_mode=false)
    super('rust', prefix=ENV["HOME"], work_dirs, verbose_mode=verbose_mode)
    @rust_utils_to_install = $rust_utils_to_install
  end

  def install
    puts ""
    puts "Working on #{@pkgname}!!"
    puts ""

    rustup_cmd = File.join(ENV["HOME"], '.cargo/bin/rustup')
    unless File.file?( rustup_cmd )
      # Installing the rust
      self.Run( $rust_inst_cmd, @pkgname )

      # path for cargo
      cargo_cmd = File.join(ENV["HOME"], '.cargo/bin/cargo')
      for pkg in @rust_utils_to_install do
        puts("Installing "+pkg)
        self.Run( [cargo_cmd, 'install', pkg].join(' ') )
      end
    else
      puts "Looks like the Rust is already installed!. Attempting to update"
      puts "Running rustup update"
      self.Run( ["rustup update"] )
      puts "Updating cargo packages"
      self.Run( ["cargo install-update -a"] )
      puts "Done working on Rust!"      
    end
    
    stdo, stde, stat = Open3.capture3("rustc --version")
    ver = stdo.split(" ")[1].split(".")
    @@Version_Info = ver

    self.WriteInfo()

  end

  def WriteInfo
    puts "Writing package info for #{@pkgname}..."
    fp = File.open(@pkginfo_file, 'w')
    compile_info_json = {
      "Package Name" => @pkgname,
      "Install CMD" => $rust_inst_cmd,
      "Version" => @@Version_Info,
    }
    fp.write(compile_info_json.to_json)
    fp.close
  end

end
