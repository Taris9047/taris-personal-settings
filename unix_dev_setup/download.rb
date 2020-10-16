#!/usr/bin/env ruby

# Download url designated file to designated location

require 'open-uri'

class Download

  def initialize(url='', destination='./', source_ctl='', mode='direct')
    @URL = url
    @DEST = File.realpath(destination)
    @source_ctl = source_ctl.downcase
    @dn_mode = mode.downcase

    unless @URL
      raise "No valid URL given!!"
      exit(-1)
    end

    if @source_ctl == ''
      @outf_name = "#{@URL.split('/')[-1]}"
      @outf_path = File.join(@DEST, @outf_name)
      if @dn_mode == 'direct'
        direct_download
      elsif @dn_mode == 'wget'
        puts "Downloading with external wget"
        wget_download
      end
    elsif @source_ctl == 'git'
      @outf_name = @URL.split('/')[-1].split('.')[0..-2].join('.')
      @outf_path = File.join(@DEST, @outf_name)
      git_clone
    end
  end

  def direct_download
    dn = URI.open(@URL)
    IO.copy_stream( dn, @outf_path )
  end

  def wget_download
    wget_cmd = [
      "wget",
      @URL,
      "-O",
      @outf_path,
    ].join(' ')
    system( wget_cmd )
  end

  def git_clone
    puts "Cloning from #{@URL} into #{@DEST}"
    system( "cd #{@DEST} && git clone #{@URL}" )
  end

  def GetPath
    @outf_path
  end

end
