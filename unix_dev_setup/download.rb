#!/usr/bin/env ruby

# Download url designated file to designated location

require 'open-uri'

class Download
  @URL = ''
  @DEST = ''

  def initialize(url, destination='./', source_ctl='')
    @URL = url
    @DEST = destination
    @source_ctl = source_ctl.downcase

    if @source_ctl == ''
      direct_download
    elsif @source_ctl == 'git'
      git_clone
    end


    # wget_cmd = command?'wget'
    # if wget_cmd == nil
    #   puts "Can't find wget!!"
    #   exit(-1)
    # else
    #   wget_cmd = 'wget'
    # end
  end

  def direct_download
    puts "Downloading {URL}".gsub('{URL}', @URL)
    dn = URI.open(@URL)
    IO.copy_stream(dn, File.join(@DEST, "#{dn.base_uri.to_s.split('/')[-1]}") )
  end

  def git_clone
    puts "Cloning from {URL}".gsub('{URL}', @URL)
    system('git clone {URL} {DEST}'.gsub('{URL}', @URL).gsub('{DEST}', @DEST))
  end

  def GetPath
    bname = @URL.split('/')[-1]
    ret_path = File.realpath(@DEST+"/"+bname)

    ret_path
  end

  # def command?(cmd)
  #   Open3.popen3("which #{ cmd} > /dev/null 2>&1")
  # end

end
