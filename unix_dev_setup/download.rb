#!/usr/bin/env ruby

# Download url designated file to designated location

class Download
  @@URL = nil
  @@DEST = nil

  def initialize(url, destination='./')
    @@URL = url
    @@DEST = destination

    wget_cmd = command?'wget'
    if wget_cmd == nil
      puts "Can't find wget!!"
      exit(-1)
    else
      wget_cmd = 'wget'
    end

    system( wget_cmd+" "+@@URL+" -c -N -P "+@@DEST )
  end

  def GetPath
    bname = @@URL.split('/')[-1]
    ret_path = File.realpath(@@DEST+"/"+bname)

    ret_path
  end

  def command?(cmd)
    system("which #{ cmd} > /dev/null 2>&1")
  end

end
