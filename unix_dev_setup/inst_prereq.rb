#!/usr/bin/env ruby

# Prerequisites
ubuntu_pkgs = [
  "build-essential",
  "flex",
  "bison",
  "zlib1g",
  "zlib1g-dev",
  "openssl",
  "libssl-dev",
  "libsqlite3-dev",
  "libncursesw5-dev",
  "libreadline-dev",
  "libssl-dev",
  "libgdbm-dev",
  "libc6-dev",
  "libsqlite3-dev",
  "tk-dev",
  "libbz2-dev",
  "libicu-dev",
  "libffi-dev",
  "autotools-dev",
  "python3-dev",
  "libncurses5-dev",
  "libxml2-dev",
  "libedit-dev",
  "swig",
  "doxygen",
  "graphviz",
  "xz-utils",
  "ruby-dev",
  "git-lfs"
]

# Other dev tools
ubuntu_some_more_tools = [
  "valgrind",
  "cmake",
  "cmake-gui",
  "autoconf",
  "automake",
  "vim",
  "emacs",
  "ttf-bitstream-vera",
  "subversion",
  "git",
  "wget",
  "curl",
]

# Ruby gems
ubuntu_ruby_gems = [
  "rsense",
]

# Install prereqs!
def inst_prereq
  puts "Installing Prerequisites!!"
  system ( 'sudo apt-get -y update && sudo apt-get -y upgrade' )
  cmd_ary = ["sudo apt-get -y install"] + ubuntu_pkgs + ubuntu_some_more_tools
  cmd = cmd_ary.join(" ")
  system( cmd )

  # Installing some gems
  puts "Installing some gems"
  cmd = ["sudo", "gem", "install"]+ubuntu_ruby_gems
  system( cmd.join(" ") )
end


