import os
import sys

from Util import *

class CondaSetup:
    def __init__(self, workdir):
        if os.path.isdir(workdir) == True:
            self.workdir = workdir
            self.conda_path = workdir + '/miniconda/bin/'

        else:
            self.conda_path = None
        os.system("uname -a")

    def install_miniconda(self, workdir):

        # create workdir if it does not exist         
        if os.path.isdir(workdir) == True:
            print('INFO: ' + workdir + ' already exists')
            if self.conda_path != None and os.path.isdir(self.conda_path) == True:
                return(SUCCESS, self.conda_path)
        else:
            os.mkdir(workdir)

        self.workdir = workdir

        url = "https://repo.continuum.io/miniconda/"
        conda_script = os.path.join(workdir, 'miniconda.sh')
        
        if sys.platform == 'darwin':
            source_script = url + 'Miniconda3-latest-MacOSX-x86_64.sh'
            cmd = "curl {src} -o {dest}".format(src=source_script, dest=conda_script)
        else:            
            source_script = url + 'Miniconda3-4.3.21-Linux-x86_64.sh'
            cmd = "wget {src} -O {dest}".format(src=source_script, dest=conda_script)

        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL..." + cmd)
            return(ret_code, None)

        conda_dir = os.path.join(workdir, 'miniconda')
        cmd = "bash {script} -b -p {dir}".format(script=conda_script, dir=conda_dir)
        # run the command, set verbose=False 
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL...installing miniconda")
            return(ret_code, None)

        self.conda_path = os.path.join(conda_dir, 'bin')
        conda_path = self.conda_path

        if sys.platform == 'linux':
            pinned_dir = os.path.join(conda_dir, 'conda-meta', 'pinned')
            cmd = "echo 'conda ==4.3.21' >> {dir}".format(dir=pinned_dir)
            ret_code = run_cmd(cmd)
            if ret_code != SUCCESS:
                print("FAIL...installing miniconda")
                return(ret_code, None)

        print("xxxx conda_path: " + conda_path)
        conda_cmd = os.path.join(conda_path, 'conda')
        cmd = "{c} config --set always_yes yes --set changeps1 no".format(c=conda_cmd)
    
        ret_code = run_cmd(cmd)
        if ret_code != SUCCESS:
            print('FAILED: ' + cmd)
            return(ret_code, None)

        cmd = "{c} install gcc future".format(c=conda_cmd)
        ret_code = run_cmd(cmd, True, False, True)
        if ret_code != SUCCESS:
            print('FAILED: ' + cmd)
            return(ret_code, None)

        if sys.platform == 'darwin':
            cmd = "{c} update -y -q conda".format(c=conda_cmd)
            ret_code = run_cmd(cmd, True, False, False)
            if ret_code != SUCCESS:
                return(ret_code, None)

        cmd = "{c} config --set anaconda_upload no".format(c=conda_cmd)
        ret_code = run_cmd(cmd)
        return(ret_code, conda_path)


