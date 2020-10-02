#!/usr/bin/env ruby

# URL database interfacing part...
# --> JSON would be suffice, right?

require 'hjson'

class SRC_URL
  def initialize(json_path='./')
    json_fp = File.read(File.join(json_path, 'urls.json'))
    @URL_DB = Hjson.parse(json_fp)
  end

  def Get(pkg_name)
    return @URL_DB[pkg_name.downcase]
  end

  def [](pkg_name)
    return Get(pkg_name)
  end

end # class URL_DB
