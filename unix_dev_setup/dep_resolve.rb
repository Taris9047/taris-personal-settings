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

    if File.directory? pkginfo_dir
      @Installed_pkg_list = \
        Dir.entries(pkginfo_dir).select { |f| f.include?('.info') }.map { |item| item.gsub('.info', '') }
    else
      @Installed_pkg_list = []
    end

    @Inst_list = install_list
    @dep_list = self.__make_dep_list(@Inst_list)
    @Inst_list = @dep_list+@Inst_list

  end

  def __make_dep_list (inst_list)
    dep_list = []
    for pkg in inst_list
      p_dep = []
      unless $dependency_table[pkg].empty?
        p_dep = $dependency_table[pkg]+p_dep
      end
      dep_list += p_dep
    end
    dep_list = dep_list.uniq
    unless @Installed_pkg_list.empty?
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
    if !not_flat_dep_list.empty?
      return self.__make_dep_list(not_flat_dep_list)+dep_list
    else
      return dep_list
    end
  end

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
end
