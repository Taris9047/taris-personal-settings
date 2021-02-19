#!/usr/bin/env ruby

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
  def initialize(json_path='./')
    real_f_name = File.join(json_path, $def_db_json_name)
    @json_data = File.readlines(real_f_name)
    @cleaned_up_data = []
    for line in @json_data
      unless line.include?('#')
        cleaned_up_line = line.delete("\n").delete("\r").delete(' ')
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

end # class ParseHjson


module SRC_URL

  def [](pkg_name)
    json_parse = ParseHjson.new()
    return json_parse.Get(pkg_name)
  end

  module_function :[]
end # module SRC_URL

module SRC_TYPE

  def [](pkg_name)
    json_parse = ParseHjson.new()
    return json_parse.GetType(pkg_name)
  end

  module_function :[]
end # module SRC_TYPE

module SRC_INFO

  def [](pkg_name)
    json_parse = ParseHjson.new()
    return json_parse.GetInfo(pkg_name)
  end

  module_function :[]
end # module SRC_INFO
