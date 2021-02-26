#!/usr/bin/env ruby

require_relative './fname_parser.rb'

# URL database interfacing part...
# --> JSON would be suffice, right?
#
# require 'hjson'
#
# class ParseHjson
#   def initialize(json_path='./')
#     json_fp = File.read(File.join(json_path, 'urls.json'))
#     @URL_DB = Hjson.parse(json_fp)
#   end
#
#   def Get(pkg_name)
#     return @URL_DB[pkg_name]
#   end
# end # class parse_json


##
## Due to deprecation of hjson on ruby platform(especially on newer ones),
## we decided to parse the json text by ourselves.
##

$def_db_json_name = 'urls.json'

require 'json'

class ParseHjson
  def initialize(json_path='../data/')

    # TODO: Gotta code this crap with more... parser like.
    @json_path = File.join(File.dirname(__FILE__), json_path)
    real_f_name = File.join(@json_path, $def_db_json_name)
    @json_data = File.readlines(real_f_name)
    @cleaned_up_data = []
    @json_data.each do |line|
      unless line.include?('#') or line.include?('//')
        cleaned_up_line = line.delete("\n").delete("\r").gsub(/( |(".*?"))/, "\\2")

        if cleaned_up_line.size > 0
          @cleaned_up_data.push(cleaned_up_line)
        end
      end
    end
    @cleaned_up_data = @cleaned_up_data.join('')
    @URL_DB = JSON.parse(@cleaned_up_data)
  end

  def Get(pkg_name)
    return @URL_DB[pkg_name]["url"]
  end

  def GetType(pkg_name)
    return @URL_DB[pkg_name]["type"]
  end

  def GetInfo(pkg_name)
    return [ @URL_DB[pkg_name]["url"], @URL_DB[pkg_name]["type"] ]
  end

  def GetPkgList()
    return Array( @URL_DB.keys )
  end

end # class ParseHjson


module SRC_URL

  def [](pkg_name)
    begin
      json_parse = ParseHjson.new()
      return json_parse.Get(pkg_name)
    rescue
      puts "Not a valid package name: \"#{pkg_name}\""
      exit(-1)
    end
  end

  module_function :[]
end # module SRC_URL

module SRC_TYPE

  def [](pkg_name)
    begin
      json_parse = ParseHjson.new()
      return json_parse.GetType(pkg_name)
    rescue
      puts "Not a valid package name: \"#{pkg_name}\""
      exit(-1)
    end
  end

  module_function :[]
end # module SRC_TYPE

module SRC_INFO

  def [](pkg_name)
    begin
      json_parse = ParseHjson.new()
      return json_parse.GetInfo(pkg_name)
    rescue
      puts "Not a valid package name: \"#{pkg_name}\""
      exit(-1)
    end
  end

  module_function :[]
end # module SRC_INFO

module SRC_VER

  def [](pkg_name)
    begin
      if pkg_name == 'golang'
        return 'git'
      end

      if SRC_TYPE[pkg_name] == "tarball"
        fnp = FNParser.new(SRC_URL[pkg_name])
        src_ver = Version.new(fnp.version().join('.'))
      else
        src_ver = Version.new(SRC_TYPE[pkg_name])
      end

      return src_ver
    rescue
      puts "Not a valid package name: \"#{pkg_name}\""
      exit(-1)
    end
  end

  module_function :[]
end # module SRC_TYPE

module SRC_LIST

  def [](filter='')
    json_parse = ParseHjson.new()
    list = json_parse.GetPkgList()
    filtered_list = []
    unless filter.empty?
      list.each do |pkg|
        if filter in pkg
          filtered_list.append(pkg)
        end
      end
      return filtered_list
    else
      return list
    end
  end

  module_function :[]
end # mdoule SRC_LIST