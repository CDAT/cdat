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
        if sys.platform == 'darwin':
            install_script = url + 'Miniconda3-latest-MacOSX-x86_64.sh'
            cmd = 'curl ' + install_script + ' -o ' + workdir + '/miniconda.sh'
        else:
            
            install_script = url + 'Miniconda3-4.3.21-Linux-x86_64.sh'
            cmd = 'wget ' + install_script + ' -O ' + workdir + '/miniconda.sh'

        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL..." + cmd)
            return(ret_code, None)

        cmd = 'bash ' + workdir + '/miniconda.sh -b -p ' + workdir + '/miniconda'
        # run the command, set verbose=False 
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL...installing miniconda")
            return(ret_code, None)

        # REVISIT : check for installation finished in output                                                         
        self.conda_path = workdir + '/miniconda/bin/'
        conda_path = self.conda_path

        if sys.platform == 'linux':
            cmd = "echo 'conda ==4.3.21' >> " + workdir + '/miniconda/conda-meta/pinned'
            ret_code = run_cmd(cmd)
            if ret_code != SUCCESS:
                print("FAIL...installing miniconda")
                return(ret_code, None)

        cmd = conda_path + 'conda config --set always_yes yes --set changeps1 no'
    
        ret_code = run_cmd(cmd)
        if ret_code != SUCCESS:
            print('FAILED: ' + cmd)
            return(ret_code, None)

        cmd = conda_path + 'conda install gcc future'
        ret_code = run_cmd(cmd, True, False, True)
        if ret_code != SUCCESS:
            print('FAILED: ' + cmd)
            return(ret_code, None)

        if sys.platform == 'darwin':
            cmd = conda_path + 'conda update -y -q conda'
            ret_code = run_cmd(cmd, True, False, False)
            if ret_code != SUCCESS:
                return(ret_code, None)

        cmd = conda_path + 'conda config --set anaconda_upload no'
        ret_code = run_cmd(cmd)
        print("DEBUG...returning from install_miniconda, ret_code: " + str(ret_code))
        return(ret_code, conda_path)


