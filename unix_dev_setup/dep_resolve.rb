#!/usr/bin/env ruby

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

require 'open3'
require 'json'

# Install order changer.

# Dependency table. -- It seems simple now...
$dependency_table = {
  "gcc" => [],
  "gccold" => [],
  "cudacc" => [],
  "python2" => [ 'gcc' ],
  "python3" => [ 'gcc' ],
  "lua" => [ 'gcc' ],
  "pypy3" => [ 'python2', 'python3' ],
  "ROOT" => [ 'gcc', 'python3', 'golang' ],
  "ruby" => [ 'gcc' ],
  "ruby3" => [ 'gcc' ],
  "node" => [ 'gcc' ],
  "node-lts" => [ 'gcc' ],
  "rust" => [],
  "clang" => [],
  "boost" => [ 'gcc' ],
  "mpich" => [ 'gcc' ],
  "hydra" => [ 'mpich' ],
  "golang" => [],
  "julia" => [],
}

# class dependency resolve
# Simply put, re-orders the installation list according to the dependency table.
class DepResolve
  def initialize(install_list, pkginfo_dir, force_install=false)

    if install_list.empty?
      puts "No install list given! Exiting"
      exit(0)
    end

    if File.directory? pkginfo_dir
      @Installed_pkg_list = \
        Dir.entries(pkginfo_dir).select { |f| f.include?('.info') }.map { |item| item.gsub('.info', '') }
    else
      @Installed_pkg_list = []
    end

    @force_install = force_install

    @Inst_list = install_list
    @Inst_list = @Inst_list.uniq
    if !@force_install and !@Installed_pkg_list.empty?
      for ipkg in @Installed_pkg_list

        # Checking version for installed/database package info.
        src_url = SRC_URL[ipkg]
        fnp = FNParser.new(src_url)
        src_ver = Version.new(fnp.version())

        fp = File.open(File.join(pkginfo_dir, ipkg+'.info'), 'r')
        ipkg_txt = fp.read.strip
        ipkg_info = JSON.parse(ipkg_txt)
        ipkg_ver = Version.new(ipkg_info["Version"].join('.'))

        if src_ver <= ipkg_ver
          # puts "Looks like #{ipkg} Version (#{ipkg_ver.to_s} is new enough. Skipping it.)"
          @Inst_list.delete(ipkg)
        end

      end # for
    end

    @dep_list = self.__make_dep_list(@Inst_list)
    @Inst_list = @dep_list+@Inst_list
    @Inst_list = @Inst_list.uniq

  end # initialize

  def __make_dep_list (inst_list)
    dep_list = []
    for pkg in inst_list
      p_dep = $dependency_table[pkg]
      dep_list += p_dep
    end
    dep_list = dep_list.uniq
    if !@Installed_pkg_list.empty?
      for ipkg in @Installed_pkg_list
        dep_list.delete(ipkg)
      end
    end

    # Checking out dependency list
    not_flat_dep_list = []
    for pk in dep_list
      if !$dependency_table[pk].empty?
        not_flat_dep_list.append(pk)
      end
    end
    not_flat_dep_list = not_flat_dep_list.uniq
    if !not_flat_dep_list.empty?
      return self.__make_dep_list(not_flat_dep_list)+dep_list
    else
      return dep_list
    end
  end # def __make_dep_list

  def GetDepList
    return @dep_list
  end

  def GetInstList
    return @Inst_list
  end

  def PrintDepList
    if @dep_list.empty?
      return "Nothing!"
    else
      return @dep_list.join(" ")
    end
  end

  def PrintInstList
    if @Inst_list.empty?
      return "Nothing!"
    else
      return @Inst_list.join(" ")
    end
  end

end # class DepResolve
