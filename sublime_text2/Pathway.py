import os
import platform as plf

if plf.system().lower() == 'windows':
    delim = ';'
    os.environ['PATH'] += delim + 'C:\\WinPython64\\python-3.4.3.amd64'
    os.environ['PATH'] += delim + \
        'C:\\WinPython64\\python-3.4.3.amd64\\Scripts'
    os.environ['PATH'] += delim + 'C:\\WinPython64\\tools\\Julia\\bin'
    os.environ['PATH'] += delim + 'C:\\MinGW\\bin' + \
        delim + 'C:\\MinGW\\msys\\1.0\\bin'
    os.environ['PATH'] += delim + "C:\\Program Files\\Git\\bin"
else:
    delim = ':'
    os.environ['PATH'] = \
        '$HOME/bin' + delim + os.environ['PATH']
    os.environ['PATH'] = \
        '/usr/local/bin' + delim + os.environ['PATH']
