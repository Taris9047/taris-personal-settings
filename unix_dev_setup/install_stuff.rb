#!/usr/bin/env ruby

# Installation script template class

class InstallStuff

    @@souce_url = 'some_url'
    @@conf_options []
    @@env = {}

    @@Processors = nil

    @@prefix=''
    @@os_type=''
    @@build_dir=''
    @@source_dir=''
    @@pkginfo_dir=''
    @@pkginfo_file=''
    @@need_sudo=false

    def WriteInfo
        fp = File.open(@@pkginfo_file, 'w')
        compile_info = [
            'Source file:\n'+.join(' ')
            'Configure options:\n'+@@prefix+' '+@@conf_options.join(' ')
            'Env Variables:\n'+@@env.map{|k,v| "#{k}=#{v}"}.join('\n')
        ]
        fp.write(compile_info.join('\n'))
        fp.close
    end
end
