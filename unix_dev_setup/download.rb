#!/usr/bin/env ruby

# Download url designated file to designated location

# require 'open3'
require 'open-uri'

class Download
  @@URL = nil
  @@DEST = nil

  def initialize(url, destination='./')
    @@URL = url
    @@DEST = destination

    puts "Downloading {URL}".gsub('{URL}', @@URL)
    dn = URI.open(@@URL)
    IO.copy_stream(dn, File.join(@@DEST, "#{dn.base_uri.to_s.split('/')[-1]}") )

    # wget_cmd = command?'wget'
    # if wget_cmd == nil
    #   puts "Can't find wget!!"
    #   exit(-1)
    # else
    #   wget_cmd = 'wget'
    # end

    # Open3.popen3( wget_cmd+" "+@@URL+" -c -N -P "+@@DEST )
  end

  def GetPath
    bname = @@URL.split('/')[-1]
    ret_path = File.realpath(@@DEST+"/"+bname)

    ret_path
  end

  # def command?(cmd)
  #   Open3.popen3("which #{ cmd} > /dev/null 2>&1")
  # end

end
