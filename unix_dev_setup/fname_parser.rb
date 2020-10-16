#!/usr/bin/env ruby

# Filename parser
# Only works with XXXX-X.X.X.ext1.ext2 or XXXX_X.X.X.ext1.ext2 format

class FNParser
  @@fname = nil
  @@bname = nil

  # Initializer
  def initialize(fname_url)
    @@fname = File.basename fname_url

    split_f = @@fname.split(".")
    if @@fname.include?".tar."
      split_f.pop
      split_f.pop
      @@bname = split_f.join(".")
    else
      split_f.pop
      @@bname = split_f.join(".")
    end
  end

  # Returns whole file name and without extension.
  def name()
    fn = @@fname
    bn = @@bname
    [fn, bn]
  end

  # Returns version
  def version()
    major = "0"
    minor = "0"
    patch = "0"

    if @@bname.include?'_'
      # In case of boost
      delim = '_'
      tmp = @@bname.split(delim)
      ver_split = tmp[1..-1]
    else
      # In case of many other stuffs
      delim = '-'
      bname_split = @@bname.split(delim)[-1]
      ver_split = bname_split.split('.')
    end

    major = ver_split[0]
    minor = ver_split[1]

    if ver_split.length > 2
      patch = ver_split[2]
    end

    ret_stuff = nil
    if ver_split.length > 2
      ret_stuff = [major, minor, patch]
    else
      ret_stuff = [major, minor]
    end

    return ret_stuff
  end # def version()

end # class fnParser
