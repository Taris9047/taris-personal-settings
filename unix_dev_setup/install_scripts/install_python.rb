#!/usr/bin/env ruby

# this will handle both Python 2 and 3

require_relative '../utils/utils.rb'
require_relative './install_stuff.rb'

$py2_modules = [
  'numpy', 'scipy', 'matplotlib', 
  'pycparser', 'sympy', 'nose'
]
$py2_conf_options = [
  "--enable-shared",
  "--enable-ipv6",
  "--enable-unicode=ucs4",
  "--with-threads",
  "--with-valgrind",
]

$py3_modules = [
  "pexpect", "sphinx", "autopep8", "xlrd", "xlsxwriter",
  "pylint", "pyparsing", "pyopengl",
  "numpy", "scipy", "matplotlib", "pandas", "nose",
  "ipython", "jedi", "parso", "sympy", "cytoolz",
  "spyder", "pyinstaller", "jupyter", "bpytop", "pycparser",
  "mercurial", "nose"
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

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false, use_clang=false)
    super('python2', prefix, work_dirs, ver_check=true, verbose_mode=verbose_mode)

    @source_url = SRC_URL[@pkgname]
    @get_pip_url = SRC_URL['get_pip']

    # Python2 modules to install
    @py2_modules = $py2_modules

    # Python2 build options
    @conf_options = $py2_conf_options

    @need_sudo = need_sudo

    # Setting up compilers
    @clang_mode = use_clang
    compiler_path = File.join(prefix, 'bin')
    gc = GetCompiler.new(
      cc_path=compiler_path, 
      cxx_path=compiler_path, 
      cflags='-fno-semantic-interposition', 
      cxxflags='-fno-semantic-interposition',
      clang=@clang_mode)
    @CompilerSettings = gc.get_settings
    @env = gc.get_env_settings

  end

  def do_install

    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

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

    puts "Compiling (with #{@Processors} processors) and Installing ..."
    cmds = [
      "cd", src_build_folder, "&&",
      File.join(src_extract_folder,"configure"),
      opts.join(" "), "&&",
      "make -j", @Processors.to_s, "&&",
      inst_cmd
    ]
    self.Run( @env, cmds.join(" ") )

    # It seems get-pip.py doesn't support python2 anymore
    puts "Running python2 -mensurepip"
    puts "Installing modules for #{@pkgname}"
    inst_pip_cmds = [
      pip_inst_sudo,
      File.join(@prefix, "bin/python"+major.to_s+"."+minor.to_s),
      "-mensurepip"
    ]
    # Changed to system instead of self.Run due to deprecation error message.
    system( inst_pip_cmds.join(" ") )
    pip_post_install_cmd = []
    if File.exists?(File.join(@prefix,"bin/pip"))
      pip_post_install_cmd = [
        pip_inst_sudo,
        'mv -fv',
        File.join(@prefix,"bin/pip"),
        File.join(@prefix,"bin/pip"+major.to_s)
      ]
    end
    system( pip_post_install_cmd.join(" ") )

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

  def initialize(prefix, work_dirs, need_sudo=false, verbose_mode=false, use_clang=false)
    super('python3', prefix, work_dirs, ver_check=true, verbose_mode=verbose_mode)

    @source_url = SRC_URL[@pkgname]
    @get_pip_url = SRC_URL['get_pip']

    # Python3 modules to install
    @py3_modules = $py3_modules

    # Python2 build options
    @conf_options = $py3_conf_options

    @need_sudo = need_sudo

    # Setting up compilers
    @clang_mode = use_clang
    compiler_path = File.join(prefix, 'bin')
    gc = GetCompiler.new(
      cc_path=compiler_path, 
      cxx_path=compiler_path, 
      cflags='-fno-semantic-interposition', 
      cxxflags='-fno-semantic-interposition',
      clang=@clang_mode)
    @CompilerSettings = gc.get_settings
    @env = gc.get_env_settings
  end

  def do_install

    puts ""
    puts "Working on #{@pkgname} (#{@ver_source.to_s})!!"
    puts ""

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
    puts "Compiling (with #{@Processors} processors) and Installing ..."
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

    puts "Installing modules for #{@pkgname}"
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
