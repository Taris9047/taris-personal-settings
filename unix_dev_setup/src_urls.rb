#!/usr/bin/env ruby

# URL database interfacing part...
# --> JSON would be suffice, right?

require 'hjson'

class ParseHjson
  def initialize(json_path='./')
    json_fp = File.read(File.join(json_path, 'urls.json'))
    @URL_DB = Hjson.parse(json_fp)
  end

  def Get(pkg_name)
    return @URL_DB[pkg_name]
  end
end # class parse_json

module SRC_URL

  def [](pkg_name)
    json_parse = ParseHjson.new
    return json_parse.Get(pkg_name)
  end

  module_function :[]
end # class URL_DB


