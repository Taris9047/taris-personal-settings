#!/usr/bin/env ruby

# this will handle both Python 2 and 3

require './download.rb'
require './fname_parser.rb'
require './get_compiler.rb'
require './install_stuff.rb'
require './src_urls.rb'

$py2_modules = ['numpy', 'scipy', 'matplotlib', 'mercurial']
$py2_conf_options = [
  "--enable-shared",
  "--enable-ipv6",
  "--enable-unicode=ucs4",
  "--with-threads",
  "--with-valgrind",
]

$py3_modules = [
  "pexpect", "sphinx", "cython", "autopep8", "xlrd", "xlsxwriter",
  "pylint", "pyparsing", "pyopengl", "pyqt5==5.12", "pyqtwebengine==5.12",
  "numpy", "scipy", "matplotlib", "pandas", "ipython", "ipywidgets",
  "jedi==0.17.1","parso==0.7.0", "qtconsole", "sympy", "cytoolz",
  "spyder", "pyinstaller", "proio", "jupyter",
]
$py3_conf_options = [
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


class InstPython2 < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false)
    super('python2', prefix, work_dirs)

    @source_url = SRC_URL[@pkgname]
    @get_pip_url = SRC_URL['get_pip']

    # Python2 modules to install
    @py2_modules = $py2_modules

    # Python2 build options
    @conf_options = $py2_conf_options

    @need_sudo = need_sudo

    # Checking up version
    VerCheck()

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @CompilerSettings = gc.get_settings
    @env = gc.get_env_settings

  end

  def install

    if self.CheckInfo
      return 0
    end

    dl = Download.new(@source_url, @src_dir)
    # src_tarball_path = dl.GetPath

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      self.Run( "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      self.Run( "rm -rfv "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    self.Run( "mkdir -p "+src_build_folder )

    opts = ["--prefix="+@prefix]+@conf_options

    # Ok let's roll!!
    if @need_sudo
      inst_cmd = "sudo make install"
      pip_inst_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      pip_inst_sudo = ""
    end

    puts "Compiling..."
    cmds = [
      "cd", src_build_folder, "&&",
      File.join(src_extract_folder,"configure"),
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]
    self.Run( @env, cmds.join(" ") )

    if File.exists?(File.join(@src_dir, 'get-pip.py'))
      puts "Found get-pip.py"
    else
      dl_pip = Download.new(@get_pip_url, @src_dir)
    end

    puts "Installing modules for python2"
    inst_pip_cmds = [
      pip_inst_sudo,
      File.join(@prefix, "bin/python"+major.to_s+"."+minor.to_s),
      File.realpath(File.join(@src_dir, 'get-pip.py')),
      "&&",
      pip_inst_sudo,
      "mv -fv",
      File.join(@prefix,"bin/pip"),
      File.join(@prefix,"bin/pip"+major.to_s)
    ]
    self.Run( inst_pip_cmds.join(" ") )

    inst_module_cmds = [
      pip_inst_sudo,
      File.join(@prefix,"bin/pip"+major.to_s),
      "install -U",
      @py2_modules.join(" ")
    ]
    self.Run( inst_module_cmds.join(" ") )

    self.WriteInfo

  end
end # class InstPython2


class InstPython3 < InstallStuff

  def initialize(prefix, work_dirs, need_sudo=false)
    super('python3', prefix, work_dirs)

    @source_url = SRC_URL[@pkgname]
    @get_pip_url = SRC_URL['get_pip']

    # Python3 modules to install
    @py3_modules = $py3_modules

    # Python2 build options
    @conf_options = $py3_conf_options

    @need_sudo = need_sudo

    # Setting up compilers
    compiler_path = File.join(prefix,'bin')
    gc = GetCompiler.new(cc_path=compiler_path, cxx_path=compiler_path)
    @CompilerSettings = gc.get_settings
    @env = gc.get_env_settings
  end

  def install

    if self.CheckInfo
      return 0
    end

    dl = Download.new(@source_url, @src_dir)
    src_tarball_path = dl.GetPath

    fp = FNParser.new(@source_url)
    src_tarball_fname, src_tarball_bname = fp.name
    major, minor, patch = fp.version

    # puts src_tarball_fname, src_tarball_bname, major, minor, patch
    src_extract_folder = File.join(File.realpath(@build_dir), src_tarball_bname)
    src_build_folder = File.join(File.realpath(@build_dir), src_tarball_bname+'-build')

    if Dir.exists?(src_extract_folder)
      puts "Source file folder exists in "+src_extract_folder
    else
      puts "Extracting"
      self.Run( "tar xf "+File.realpath(File.join(@src_dir, src_tarball_fname))+" -C "+@build_dir )
    end

    if Dir.exists?(src_build_folder)
      puts "Build folder found!! Removing it for 'pure' experience!!"
      self.Run( "rm -rfv "+src_build_folder )
    else
      puts "Ok, let's make a build folder"
    end
    self.Run( "mkdir -p "+src_build_folder )

    opts = ["--prefix="+@prefix]+@conf_options

    if @need_sudo
      inst_cmd = "sudo make install"
      pip_inst_sudo = "sudo -H"
    else
      inst_cmd = "make install"
      pip_inst_sudo = ""
    end

    # Ok let's roll!!
    puts "Compiling..."
    cmds = [
      "cd", src_build_folder, "&&",
      File.join(src_extract_folder,"configure"),
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]
    self.Run( @env, cmds.join(" ") )

    if File.exists?(File.join(@src_dir, 'get-pip.py'))
      puts "Found get-pip.py"
    else
      dl_pip = Download.new(@get_pip_url, @src_dir)
    end

    puts "Installing modules for python3"
    inst_pip_cmds = [
      pip_inst_sudo,
      File.join(@prefix, "bin/python"+major.to_s+"."+minor.to_s),
      File.realpath(File.join(@src_dir, 'get-pip.py')),
      "&&",
      pip_inst_sudo,
      "mv -fv",
      File.join(@prefix,"bin/pip"),
      File.join(@prefix,"bin/pip"+major.to_s)
    ]
    self.Run( inst_pip_cmds.join(" ") )

    inst_module_cmds = [
      pip_inst_sudo,
      File.join(@prefix,"bin/pip"+major.to_s),
      "install -U",
      @py3_modules.join(" ")
    ]
    self.Run( inst_module_cmds.join(" ") )

    self.WriteInfo

  end
end # class InstPython3
