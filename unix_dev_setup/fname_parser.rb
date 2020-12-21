#!/usr/bin/env ruby

# Version handlig stuffs
# Referenced: https://stackoverflow.com/questions/2051229/how-to-compare-versions-in-ruby/2051427#2051427
#
class Version < Array
  def initialize(s)
    super( s.split('.').map{ |e| e.delete(',').delete('v').delete('V').to_i } )
  end

  # Version comparison operators
  def <(x)
    (self <=> x) < 0
  end
  def <=(x)
    (self <=> x) <= 0
  end
  def >(x)
    (self <=> x) > 0
  end
  def >=(x)
    (self <=> x) >= 0
  end
  def ==(x)
    (self <=> x) == 0
  end

  # Returning the version info. from integer array to ...
  def to_s
    return self.join('.')
  end

  def to_sA
    return self.map{ |e| e.to_s }
  end

  def major
    return self[0].to_s
  end

  def minor
    if self.length() > 1
      return self[1].to_s
    else
      return self[0].to_s
    end
  end

  def patch
    return self[-1].to_s
  end

end # class Version

# Filename parser
# Only works with XXXX-X.X.X.ext1.ext2 or XXXX_X.X.X.ext1.ext2 format

class FNParser
  @@fname = nil
  @@bname = nil
  @@version = nil

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

    @@version = Version.new(ver_split.join('.'))
    return @@version.to_sA
  end # def version()

end # class fnParser
