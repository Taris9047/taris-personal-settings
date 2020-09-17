#!/usr/bin/env ruby

# Let's set up compiler here

class GetCompiler
  @@fallback_compiler_path = '/usr/bin/'
  
  @@CC_PATH = @@fallback_compiler_path
  @@CXX_PATH = @@fallback_compiler_path
  @@CFLAGS = "-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe"
  @@RPATH = "-Wl,-rpath=$HOMEBREW/lib64 -Wl,-rpath=$HOMEBREW/lib"
  @@CXXFLAGS = @@CFLAGS
  @@CC = File.join(@@CC_PATH, 'gcc')
  @@CXX = File.join(@@CXX_PATH, 'g++')
  
  def initialize(cc_path='/usr/bin', cxx_path='/usr/bin', cflags='', cxxflags='', clang=false, suffix='', env_path='')
    
    c_compiler = 'gcc'
    cxx_compiler = 'g++'
    
    r_path = '/usr/local'
    if env_path == ''
      r_path = File.dirname(cc_path)
    end
    @@RPATH = "-Wl,-rpath="+File.join(r_path, "lib")+" "+"-Wl,-rpath="+File.join(r_path, "lib64")
    
    if clang
      c_compiler = 'clang'
      cxx_compiler = 'clang++'
    end
    
    unless suffix == ''
      c_compiler = c_compiler + '-' + suffix
      cxx_compiler = cxx_compiler + '-' + suffix
    end
    
    @@compiler_path = @@fallback_compiler_path
    if File.directory?(cc_path)
      @@cc_path = File.realpath(cc_path)
    end
    if File.directory?(cxx_path)
      @@cxx_path = File.realpath(cxx_path)
    end
    @@CC_PATH = @@cc_path
    @@CXX_PATH = @@cxx_path
    
    cc_path = File.join(@@CC_PATH, c_compiler)
    cxx_path = File.join(@@CXX_PATH, cxx_compiler)
    if File.file?(cc_path)
      @@CC = cc_path
    end
    if File.file?(cxx_path)
      @@CXX = cxx_path
    end
    
    # doing sanity check!
    unless File.file?(@@CC) and File.file?(@@CXX)
      raise ">>>> Can't find suitable compilers!!"
      exit(-1)
    end
    
    unless cflags == ''
      @@CFLAGS = cflags
      @@CXXFLAGS = cxxflags
    end
  end
  
  
  def get_settings
    cc_env = "CC=\""+@@CC+"\""
    cxx_env = "CXX=\""+@@CXX+"\""
    cflags_env = "CFLAGS=\""+@@CFLAGS+"\""
    cxxflags_env = "CXXFLAGS\""+@@CXXFLAGS+"\""
    ldflags_env = "LDFLAGS=\""+@@RPATH+"\""
    
    return [cc_env, cxx_env, cflags_env, cxxflags_env, ldflags_env]
  end
  
  def get_cmake_settings
    cc_env = "-DCMAKE_C_COMPILER=\""+@@CC+"\""
    cxx_env = "-DCMAKE_CXX_COMPILER=\""+@@CXX+"\""
    cflags_env = "-DCMAKE_C_FLAGS=\""+@@CFLAGS+" "+@@RPATH+"\""
    cxxflags_env = "-DCMAKE_CXX_FLAGS=\""+@@CXXFLAGS+" "+@@RPATH+"\""
    
    return  [cc_env, cxx_env, cflags_env, cxxflags_env]
  end
  
end
    
