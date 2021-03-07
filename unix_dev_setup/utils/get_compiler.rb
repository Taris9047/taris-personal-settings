#!/usr/bin/env ruby

# Let's set up compiler here

#$cflags = "-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe"
#$cxxflags = "-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe"

require_relative './src_urls.rb'
require_relative './misc_utils.rb'

$cflags = "-O3 -march=native -fomit-frame-pointer -pipe -I{env_path}/include"
$cxxflags = $cflags
$fallback_compiler_path = '/usr/bin'
$state_of_art_gcc_ver = SRC_VER['gcc'][0]

$rpath = "-Wl,-rpath={env_path}/lib -Wl,-rpath={env_path}/lib64 -L{env_path}/lib -L{env_path}/lib64"
$pkg_config_path = "{env_path}/lib/pkgconfig:/usr/local/lib64/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib64/pkgconfig:/usr/lib/pkgconfig"

class GetCompiler
  attr_accessor :cc_path, :cxx_path, :cflags, :cxxflags, :clang, :suffix, :env_path

  def initialize(
    cc_path='/usr/bin', 
    cxx_path='/usr/bin', 
    cflags='', 
    cxxflags='', 
    clang=false,
    suffix='',
    env_path='',
    verbose=false)

    @fallback_compiler_path = '/usr/bin/'

    @current_gcc_major = SRC_VER['gcc'].major

    @CC_PATH = @fallback_compiler_path
    @CXX_PATH = @fallback_compiler_path
    @CFLAGS = [$cflags, cflags].join(' ')
    @RPATH = $rpath
    if ENV['PKG_CONFIG_PATH']
      @PKG_CONFIG_PATH = "#{$pkg_config_path}:#{ENV['PKG_CONFIG_PATH']}"
    else
      @PKG_CONFIG_PATH = ''
    end
    @PATH=""
    @CXXFLAGS = [$cxxflags, cxxflags].join(' ')
    @CC = File.join(@CC_PATH, "gcc-#{@current_gcc_major}")
    @CXX = File.join(@CXX_PATH, "g++-#{@current_gcc_major}")
    @env_path = env_path

    @verbose = verbose

    @prefix = env_path
    if env_path == '' or !File.directory? env_path
      @prefix = File.dirname(cc_path)
    end
    @CFLAGS = @CFLAGS.gsub('{env_path}', @prefix)
    @CXXFLAGS = @CFLAGS.gsub('{env_path}', @prefix)
    @RPATH = @RPATH.gsub('{env_path}', @prefix)
    unless @PKG_CONFIG_PATH.empty?
      @PKG_CONFIG_PATH = @PKG_CONFIG_PATH.gsub('{env_path}', @prefix)
    end

    if clang
      c_compiler = 'clang'
      cxx_compiler = 'clang++'
      # Clang already has -fno-semantic-interposition
      if @CFLAGS.include? '-fno-semantic-interposition'
        @CFLAGS.slice! '-fno-semantic-interposition'
      end
      if @CXXFLAGS.include? '-fno-semantic-interposition'
        @CXXFLAGS.slice! '-fno-semantic-interposition'
      end  
    else
      if UTILS.which("gcc-#{$state_of_art_gcc_ver}")
        c_compiler = "gcc-#{$state_of_art_gcc_ver}"
      else
        c_compiler = "gcc"
      end
      if UTILS.which("g++-#{$state_of_art_gcc_ver}")
        cxx_compiler = "g++-#{$state_of_art_gcc_ver}"
      else
        cxx_compiler = 'g++'
      end
    end

    unless suffix.empty?
      c_compiler = 'gcc' + '-' + suffix
      cxx_compiler = 'g++' + '-' + suffix
    end

    if File.directory?(cc_path)
      if File.file?(File.join(cc_path, c_compiler))
        @CC = File.realpath(File.join(cc_path, c_compiler))
      else
        if File.file?(UTILS.which(c_compiler))
          @CC = UTILS.which(c_compiler)
        else
          @CC = File.join(@fallback_compiler_path, "gcc")
        end
      end
    end
    if File.directory?(cxx_path)
      if File.file?(File.join(cxx_path, cxx_compiler))
        @CXX = File.realpath(File.join(cc_path, cxx_compiler))
      else
        if File.file?(UTILS.which(cxx_compiler))
          @CXX = UTILS.which(cxx_compiler)
        else
          @CXX = File.join(@fallback_compiler_path, "g++")
        end
      end
    end

    unless File.file?(@CC)
      raise "C Compiler not found!!"
      exit(-1)
    end
    unless File.file?(@CXX)
      raise "CXX Compiler not found!!"
      exit(-1)
    end

    if @verbose
      puts "So, we're going to use those settings..."
      puts "C compiler: #{@CC}"
      puts "C++ compiler: #{@CXX}"
      puts "C flags: #{@CFLAGS}"
      puts "CXX flags: #{@CXXFLAGS}"
      puts "Linker flags: #{@RPATH}"
      unless @PKG_CONFIG_PATH.empty?
        puts "pkgconfig path: #{@PKG_CONFIG_PATH}"
      end
    end

  end


  def get_settings
    cc_env = "CC=\""+@CC+"\""
    cxx_env = "CXX=\""+@CXX+"\""
    cflags_env = ["CFLAGS=\"", @CFLAGS, "\""].join('')
    cxxflags_env = ["CXXFLAGS=\"", @CXXFLAGS, "\""].join('')
    ldflags_env = ["LDFLAGS=\"", @RPATH, "\""].join('')
    pkgconfig_env = "PKG_CONFIG_PATH=\"#{@PKG_CONFIG_PATH}\""

    unless @PKG_CONFIG_PATH.empty?
      return [cc_env, cxx_env, cflags_env, cxxflags_env, ldflags_env, pkgconfig_env]
    else
      return [cc_env, cxx_env, cflags_env, cxxflags_env, ldflags_env]
    end
  end

  def get_env_str
    return self.get_settings.join(' ')
  end

  def get_env_settings
    unless @PKG_CONFIG_PATH.empty?
      return {
        'CC' => @CC,
        'CXX' => @CXX,
        'CFLAGS' => @CFLAGS,
        'CXXFLAGS' => @CXXFLAGS,
        'LDFLAGS' => @RPATH,
        'PKG_CONFIG_PATH' => @PKG_CONFIG_PATH,
      }
    else
      return {
        'CC' => @CC,
        'CXX' => @CXX,
        'CFLAGS' => @CFLAGS,
        'CXXFLAGS' => @CXXFLAGS,
        'LDFLAGS' => @RPATH,
      }
    end
  end

  def get_cmake_settings
    cc_env = "-DCMAKE_C_COMPILER=\""+@CC+"\""
    cxx_env = "-DCMAKE_CXX_COMPILER=\""+@CXX+"\""
    cflags_env = "-DCMAKE_C_FLAGS=\""+@CFLAGS+" "+@RPATH+"\""
    cxxflags_env = "-DCMAKE_CXX_FLAGS=\""+@CXXFLAGS+" "+@RPATH+"\""
    ld_exe_env = "-DCMAKE_EXE_LINKER_FLAGS_INIT=\""+@RPATH+"\""
    ld_shared_env = "-DCMAKE_SHARED_LINKER_FLAGS_INIT=\""+@RPATH+"\""
    ld_module_env = "-DCMAKE_MODULE_LINKER_FLAGS_INIT=\""+@RPATH+"\""
    pkg_config_path = "-DCMAKE_PKG_CONFIG_PATH=\"#{@PKG_CONFIG_PATH}\""

    unless @PKG_CONFIG_PATH.empty?
      return [cc_env, cxx_env, cflags_env, cxxflags_env, ld_exe_env, ld_shared_env, ld_module_env, pkg_config_path]
    else
      return [cc_env, cxx_env, cflags_env, cxxflags_env, ld_exe_env, ld_shared_env, ld_module_env]
    end
  end

end
