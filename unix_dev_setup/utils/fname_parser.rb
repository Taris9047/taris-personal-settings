#!/usr/bin/env ruby

# TODO: Need to handle alphabet based version number such as '6b'...
# Alpha <--> Numeric conversion classes
# Referenced: https://stackoverflow.com/questions/14632304/generate-letters-to-represent-number-using-ruby/31152792
# and https://stackoverflow.com/questions/10637606/doesnt-ruby-have-isalpha
#
class Numeric
  Alpha26 = ("a".."z").to_a
  def to_s26
    return "" if self < 1
    s, q = "", self
    loop do
      q, r = (q - 1).divmod(26)
      s.prepend(Alpha26[r]) 
      break if q.zero?
    end
    s
  end
end

class String
  Alpha26 = ("a".."z").to_a

  def to_i26
    result = 0
    downcased = downcase
    (1..length).each do |i|
      char = downcased[-i]
      result += 26**(i-1) * (Alpha26.index(char) + 1)
    end
    result
  end

  def isalpha?
    !match(/^[[:alnum:]]+$/)
  end
end

#
# Version handlig stuffs
# Referenced: https://stackoverflow.com/questions/2051229/how-to-compare-versions-in-ruby/2051427#2051427
#
class Version < Array
  @ver_string = ''
  @has_alphabet = false
  @alphabet_uppercase = false

  def initialize(s)
    if s.instance_of? String
      begin
        # puts "#{s}: #{s[-1]}: #{s[-1].isalpha?}"
        if s[-1].isalpha?
          tmp = s[-1]
          s[-1] = '.'
          s += tmp.to_i26.to_s
          @has_alphabet = true
          if /[[:upper:]]/.match(tmp)
            @alphabet_uppercase = true
          end
        end
        super( s.split('.').map{ |e| e.delete(',').delete('v').delete('V').to_i } )
        @ver_string = self.join('.')
      rescue
        super([0,0,0])
        @ver_string = s
      end
    elsif s.instance_of? Array
      super( s.map{ |e| e.delete(',').delete('v').delete('V').to_i } )
      @ver_string = self.join('.')
    end
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
    if @has_alphabet
      ver_str = self[0..-2].map{ |e| e.to_s }.join('.')
      unless @alphabet_uppercase
        ver_str += self[-1].to_s26
      else
        ver_str += self[-1].to_s26.upper
      end
      return ver_str
    else
      return self.map{ |e| e.to_s }.join('.')
    end
  end

  def to_sA
    if @has_alphabet
      ver_str = self[0..-2].map{ |e| e.to_s }.join('.')
      unless @alphabet_uppercase
        ver_str += self[-1].to_s26
      else
        ver_str += self[-1].to_s26.upcase
      end
      return ver_str.split('.')
    else
      return self.map{ |e| e.to_s }
    end
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

    if @has_alphabet
      unless @alphabet_uppercase
        patch_str = [self[-2].to_s, self[-1].to_s26].join('')
      else
        patch_str = [self[-2].to_s, self[-1].to_s26.upcase].join('')
      end
      return patch_str
    else
      return self[-1].to_s
    end
  end

  def __alpha_to_int__
    
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

    if !fname_url
      puts "FNParser: Wrong URL given!"
      puts fname_url
      exit(-1)
    end

    @repo_addr = false
    if fname_url.include?('.git')
      @repo_addr = true
    else
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
  end

  # Returns whole file name and without extension.
  def name()
    unless @repo_addr
      fn = @@fname
      bn = @@bname
      return [fn, bn]
    else
      return ['', '']
    end
  end

  # Returns version
  def version()

    if @repo_addr
      @@version = Version.new('0.0.0')
    else
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
    end
    return @@version.to_sA
  end # def version()

end # class fnParser
