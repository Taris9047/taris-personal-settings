#!/usr/bin/env ruby

# this will handle both Python 2 and 3

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require 'etc'

$get_pip_url = "https://bootstrap.pypa.io/get-pip.py"

$python2_url = "https://www.python.org/ftp/python/2.7.15/Python-2.7.15.tgz"
$python3_url = "https://www.python.org/ftp/python/3.8.5/Python-3.8.5.tgz"


class InstPython2
  @@source_url = $python2_url

  @@Prefix = nil
  @@Build_dir = nil
  @@Src_dir = nil

  # Python2 modules to install
  @@py2_modules = []

  # Python2 build options
  @@py2_conf_opts = [
    "--enable-shared",
    "--enable-ipv6",
    "--enable-unicode=ucs4",
    "--with-threads",
    "--with-valgrind",
  ]

  @@Processors = nil

  @@CompilerSettings = [
    "CC=\"gcc\"",
    "CXX=\"g++\"",
    "CFLAGS=\"-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe\"",
    "CXXFLAGS=\"-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe\"",
    "LDFLAGS=\"-Wl,-rpath={env_path}/lib64 -Wl,-rpath={env_path}/lib\"",
  ]

  def initialize(prefix, build_dir, src_dir, need_sudo=false)
    @@Prefix = prefix
    @@Build_dir = build_dir
    @@Src_dir = src_dir
    @@need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @@CompilerSettings = gc.get_settings
    @@env = gc.get_env_settings

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  def install
    dl = Download.new(@@source_url, @@Src_dir)
    # src_tarball_path = dl.GetPath

    fp = FNParser.new(@@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@@Build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@@Build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      system( "tar xf "+File.realpath(File.join(@@Src_dir, src_tarball_fname))+" -C "+@@Build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      system( "rm -rfv "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    system( "mkdir "+src_build_folder )

    conf_opts = ["--prefix="+@@Prefix]+@@py2_conf_opts

    # Ok let's roll!!
    if @@need_sudo
      inst_cmd = "sudo make install"
      pip_inst_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      pip_inst_sudo = ""
    end
    cmds = [
      "cd", src_build_folder, "&&",
      File.join(src_extract_folder,"configure"),
      conf_opts.join(" "), "&&",
      "make -j", @@Processors.to_s, "&&",
      inst_cmd
    ]

    system( @@env, cmds.join(" ") )

    if File.exists?(File.join(@@Src_dir, 'get-pip.py'))
      puts "Found get-pip.py"
    else
      dl_pip = Download.new($get_pip_url, @@Src_dir)
    end

    inst_pip_cmds = [
      pip_inst_sudo,
      File.join(@@Prefix, "bin/python"+major.to_s+"."+minor.to_s),
      File.realpath(File.join(@@Src_dir, 'get-pip.py')),
      "&&",
      pip_inst_sudo,
      "mv -fv",
      File.join(@@Prefix,"/bin/pip"),
      File.join(@@Prefix,"/bin/pip"+major.to_s)
    ]
    system( inst_pip_cmds.join(" ") )

    inst_module_cmds = [
      pip_inst_sudo,
      File.join(@@Prefix,"/bin/pip"+major.to_s),
      "install -U",
      @@py2_modules.join(" ")
    ]

    system( inst_module_cmds.join(" ") )

  end
end # class InstPython2


class InstPython3
  @@source_url = $python3_url

  @@Prefix = nil
  @@Build_dir = nil
  @@Src_dir = nil

  # Python3 modules to install
  @@py3_modules = [
    "pexpect", "sphinx", "cython", "autopep8", "xlrd", "xlsxwriter",
    "pylint", "pyparsing", "pyopengl", "pyqt5==5.12", "pyqtwebengine==5.12",
    "numpy", "scipy", "matplotlib", "pandas", "ipython", "ipywidgets",
    "jedi==0.17.1","parso==0.7.0", "qtconsole", "sympy", "cytoolz",
    "spyder", "pyinstaller", "proio", "jupyter",
  ]

  # Python2 build options
  @@py3_conf_opts = [
    "--enable-shared",
    "--enable-ipv6",
    "--enable-unicode=ucs4",
    "--with-threads",
    "--with-valgrind",
    "--with-ensurepip=yes",
    "--with-system-ffi",
    "--with-system-expat",
    "--enable-optimizations",
  ]

  @@Processors = nil

  @@CompilerSettings = [
    "CC=\"gcc\"",
    "CXX=\"g++\"",
    "CFLAGS=\"-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe\"",
    "CXXFLAGS=\"-O3 -fno-semantic-interposition -march=native -fomit-frame-pointer -pipe\"",
    "LDFLAGS=\"-Wl,-rpath={env_path}/lib64 -Wl,-rpath={env_path}/lib\"",
  ]

  def initialize(prefix, build_dir, src_dir, need_sudo=false)
    @@Prefix = prefix
    @@Build_dir = build_dir
    @@Src_dir = src_dir
    @@need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @@CompilerSettings = gc.get_settings
    @@env = gc.get_env_settings

    # Setting up processors
    procs = Etc.nprocessors
    if procs > 2
      @@Processors = procs-1
    else
      @@Processors = procs
    end
  end

  def install
    dl = Download.new(@@source_url, @@Src_dir)
    src_tarball_path = dl.GetPath

    fp = FNParser.new(@@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@@Build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@@Build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      system( "tar xf "+File.realpath(File.join(@@Src_dir, src_tarball_fname))+" -C "+@@Build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      system( "rm -rfv "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    system( "mkdir "+src_build_folder )

    conf_opts = ["--prefix="+@@Prefix]+@@py3_conf_opts

    if @@need_sudo
      inst_cmd = "sudo make install"
      pip_inst_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      pip_inst_sudo = ""
    end
    # Ok let's roll!!
    cmds = [
      "cd", src_build_folder, "&&",
      File.join(src_extract_folder,"configure"),
      conf_opts.join(" "), "&&",
      "make -j", @@Processors.to_s, "&&",
      inst_cmd
    ]

    system( @@env, cmds.join(" ") )

    if File.exists?(File.join(@@Src_dir, 'get-pip.py'))
      puts "Found get-pip.py"
    else
      dl_pip = Download.new($get_pip_url, @@Src_dir)
    end

    inst_pip_cmds = [
      pip_inst_sudo,
      File.join(@@Prefix, "bin/python"+major.to_s+"."+minor.to_s),
      File.realpath(File.join(@@Src_dir, 'get-pip.py')),
      "&&",
      pip_inst_sudo,
      "mv -fv",
      File.join(@@Prefix,"/bin/pip"),
      File.join(@@Prefix,"/bin/pip"+major.to_s)
    ]
    system( inst_pip_cmds.join(" ") )

    inst_module_cmds = [
      pip_inst_sudo,
      File.join(@@Prefix,"/bin/pip"+major.to_s),
      "install -U",
      @@py3_modules.join(" ")
    ]

    system( inst_module_cmds.join(" ") )

  end
end # class InstPython3
