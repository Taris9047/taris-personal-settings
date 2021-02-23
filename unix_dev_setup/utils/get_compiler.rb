#!/usr/bin/env ruby

# Let's set up compiler here

#$cflags = "-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe"
#$cxxflags = "-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe"

$cflags = "-O3 -march=native -fomit-frame-pointer -pipe"
$cxxflags = $cflags

$rpath = "-Wl,-rpath={env_path}/lib64 -Wl,-rpath={env_path}/lib"

class GetCompiler
  attr_accessor :cc_path, :cxx_path, :cflags, :cxxflags, :clang, :suffix, :env_path

  def initialize(
    cc_path='/usr/bin', 
    cxx_path='/usr/bin', 
    cflags='', 
    cxxflags='', 
    clang=false, suffix='', env_path='', verbose=false)

    @fallback_compiler_path = '/usr/bin/'

    @CC_PATH = @fallback_compiler_path
    @CXX_PATH = @fallback_compiler_path
    @CFLAGS = [$cflags, cflags].join(' ')
    @RPATH = "-Wl,-rpath={env_path}/lib64 -Wl,-rpath={env_path}/lib"
    @CXXFLAGS = [$cxxflags, cxxflags].join(' ')
    @CC = File.join(@CC_PATH, 'gcc')
    @CXX = File.join(@CXX_PATH, 'g++')

    @verbose = verbose

    r_path = '/usr/local'
    if env_path == ''
      r_path = File.dirname(cc_path)
    end
    @RPATH = @RPATH.gsub('{env_path}', r_path)

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
      c_compiler = 'gcc'
      cxx_compiler = 'g++'
    end

    unless suffix == ''
      c_compiler = c_compiler + '-' + suffix
      cxx_compiler = cxx_compiler + '-' + suffix
    end

    if File.directory?(cc_path)
      if File.file?(File.join(cc_path, c_compiler))
        @CC = File.realpath(File.join(cc_path, c_compiler))
      else
        @CC = File.join(@fallback_compiler_path, 'gcc')
      end
    end
    if File.directory?(cxx_path)
      if File.file?(File.join(cxx_path, cxx_compiler))
        @CXX = File.realpath(File.join(cc_path, cxx_compiler))
      else
        @CXX = File.join(@fallback_compiler_path, 'g++')
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
      puts "LL flags: #{@RPATH}"
    end

  end


  def get_settings
    cc_env = "CC=\""+@CC+"\""
    cxx_env = "CXX=\""+@CXX+"\""
    cflags_env = ["CFLAGS=\"", @CFLAGS, "\""].join('')
    cxxflags_env = ["CXXFLAGS=\"", @CXXFLAGS, "\""].join('')
    ldflags_env = ["LDFLAGS=\"", @RPATH, "\""].join('')

    return [cc_env, cxx_env, cflags_env, cxxflags_env, ldflags_env]
  end

  def get_env_str
    return self.get_settings.join(' ')
  end

  def get_env_settings
    return {
      'CC' => @CC,
      'CXX' => @CXX,
      'CFLAGS' => @CFLAGS,
      'CXXFLAGS' => @CXXFLAGS,
      'LDFLAGS' => @RPATH,
    }
  end

  def get_cmake_settings
    cc_env = "-DCMAKE_C_COMPILER=\""+@CC+"\""
    cxx_env = "-DCMAKE_CXX_COMPILER=\""+@CXX+"\""
    cflags_env = "-DCMAKE_C_FLAGS=\""+@CFLAGS+" "+@RPATH+"\""
    cxxflags_env = "-DCMAKE_CXX_FLAGS=\""+@CXXFLAGS+" "+@RPATH+"\""
    ld_exe_env = "-DCMAKE_EXE_LINKER_FLAGS_INIT=\""+@RPATH+"\""
    ld_shared_env = "-DCMAKE_SHARED_LINKER_FLAGS_INIT=\""+@RPATH+"\""
    ld_module_env = "-DCMAKE_MODULE_LINKER_FLAGS_INIT=\""+@RPATH+"\""

    return  [cc_env, cxx_env, cflags_env, cxxflags_env, ld_exe_env, ld_shared_env, ld_module_env]
  end

end
