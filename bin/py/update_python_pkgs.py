#!/usr/bin/env python

import sys
import subprocess as sbp

python_ver = sys.version_info[0]

if python_ver < 3:
    pip_cmd = 'pip'
else:
    pip_cmd = 'pip3'

pip_ver_cmd = sbp.Popen([pip_cmd, '-V'], stdout=sbp.PIPE, stderr=sbp.STDOUT)
stdout, stderr = pip_ver_cmd.communicate()

pip_ver_text = stdout.decode('utf-8').split(" ")[1]
pip_ver_num = [int(_) for _ in pip_ver_text.split(".")]

print("PIP version seems to be... {}".format(pip_ver_text))

if pip_ver_num[0] < 10 and pip_ver_num[1] < 1 and pip_ver_num[2] < 1:
    import pip
    packages = [dist.project_name for dist in pip.get_installed_distributions()]
    sbp.call("{} install -U ".format(pip_cmd)+' '.join(packages), shell=True)
else:
    import pkg_resources
    packages = [dist.project_name for dist in pkg_resources.working_set]
    sbp.call("{} install -U ".format(pip_cmd)+' '.join(packages), shell=True)

print("Python package update completed!!")


