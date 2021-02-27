#!/usr/bin/env ruby

require_relative './download.rb'
require_relative './fname_parser.rb'
require_relative './get_compiler.rb'
require_relative './src_urls.rb'
require_relative './dep_resolve.rb'
require_relative './run_console.rb'

# Some misc utils
#

module UTILS

  # Which command eqv.
  # https://stackoverflow.com/a/5471032
  def which(cmd)
    exts = ENV['PATHEXT'] ? ENV['PATHEXT'].split(';') : ['']
    ENV['PATH'].split(File::PATH_SEPARATOR).each do |path|
      exts.each do |ext|
        exe = File.join(path, "#{cmd}#{ext}")
        return exe if File.executable?(exe) && !File.directory?(exe)
      end
    end
    nil
  end

  module_function :which
end # module UTILS