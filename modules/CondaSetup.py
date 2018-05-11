import os
import sys

from Util import *

class CondaSetup:
    def __init__(self, workdir, py_ver):
        if os.path.isdir(workdir) == True:
            self.conda_path = workdir + '/miniconda/bin/'
        else:
            self.conda_path = None
        self.workdir = workdir
        self.py_ver = py_ver
        os.system("uname -a")

    def install_miniconda(self):

        # create workdir if it does not exist
        workdir = self.workdir
        if os.path.isdir(workdir) == True:
            print('INFO: ' + workdir + ' already exists')
            if self.conda_path != None and os.path.isdir(self.conda_path) == True:
                return(SUCCESS, self.conda_path)
        else:
            os.mkdir(workdir)

        url = "https://repo.continuum.io/miniconda"
        conda_script = os.path.join(workdir, 'miniconda.sh')
        if self.py_ver == 'py2':
            conda_ver = 'Miniconda2'
        else:
            conda_ver = 'Miniconda3'

        if sys.platform == 'darwin':
            conda_script = "{c}-latest-MacOSX-x86_64.sh".format(c=conda_ver)
            conda_script_full_path = os.path.join(workdir, conda_script)
            source_script = os.path.join(url, conda_script)
            cmd = "curl {src} -o {dest}".format(src=source_script, dest=conda_script_full_path)
        else:
            conda_script = "{c}-latest-Linux-x86_64.sh".format(c=conda_ver)
            conda_script_full_path = os.path.join(workdir, conda_script)
            source_script = os.path.join(url, conda_script)
            cmd = "wget {src} -O {dest}".format(src=source_script, dest=conda_script_full_path)

        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL..." + cmd)
            return(ret_code, None)

        conda_dir = os.path.join(workdir, 'miniconda')
        cmd = "bash {script} -b -p {dir}".format(script=conda_script_full_path, 
                                                 dir=conda_dir)

        # run the command, set verbose=False 
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL...installing miniconda")
            return(ret_code, None)

        self.conda_path = os.path.join(conda_dir, 'bin')
        conda_path = self.conda_path

        conda_cmd = os.path.join(conda_path, 'conda')
        cmd = "{c} config --set always_yes yes --set changeps1 no".format(c=conda_cmd)
    
        ret_code = run_cmd(cmd)
        if ret_code != SUCCESS:
            print('FAILED: ' + cmd)
            return(ret_code, None)

        # I am not sure if I need the following
        if sys.platform == 'darwin':
            cmd = "{c} update -y -q conda".format(c=conda_cmd)
            ret_code = run_cmd(cmd, True, False, False)
            if ret_code != SUCCESS:
                return(ret_code, None)

        cmd = "{c} config --set anaconda_upload no".format(c=conda_cmd)
        ret_code = run_cmd(cmd)
        return(ret_code, conda_path)


