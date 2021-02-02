#!/usr/bin/env ruby

require 'open3'

# Install order changer.

# Dependency table. -- It seems simple now...
$dependency_table = {
  "gcc" => [],
  "gccold" => [],
  "cudacc" => [],
  "python2" => [ 'gcc' ],
  "python3" => [ 'gcc' ],
  "lua" => [ 'gcc' ],
  "pypy3" => [ 'python2' ],
  "ROOT" => [ 'gcc', 'python3' ],
  "ruby" => [ 'gcc' ],
  "ruby3" => [ 'gcc' ],
  "node" => [ 'gcc' ],
  "rust" => [],
  "clang" => [],
  "boost" => [ 'gcc' ],
}

# class dependency resolve
# Simply put, re-orders the installation list according to the dependency table.
class DepResolve
  def initialize(install_list, pkginfo_dir)

    if install_list.empty?
      puts "No install list given! Exiting"
      exit(0)
    end

    @Installed_pkg_list = \
      Dir.entries('./pkginfo').select { |f| f.include?('.info') }.map { |item| item.gsub('.info', '') }

    @Inst_list = install_list


  end

  def GetList

  end

  def PrintList
  end

end
