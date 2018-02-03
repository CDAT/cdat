import os
import sys

from Util import *

class NightlySetup:

    def __init__(self, workdir):
        if os.path.isdir(workdir) == True:
            self.workdir = workdir
            self.conda_path = workdir + '/miniconda/bin/'

            env_dir = workdir + '/miniconda/envs/nightly2'
            if os.path.isdir(env_dir) == True:
                self.py2_env = 'nightly2'

            env_dir = workdir + '/miniconda/envs/nightly3'
            if os.path.isdir(env_dir) == True:
                self.py3_env = 'nightly3'
        else:
            self.conda_path = None
            self.py2_env = None
            self.py3_env = None


    def install_miniconda(self, workdir):

        print("xxx echoing SHELL environment variable: ")
        os.system("echo $SHELL")

        # create workdir if it does not exist         
        if os.path.isdir(workdir) == True:
            print('INFO: ' + workdir + ' already exists')
            print('INFO: ' + env + ' must have already been set up')
            return(SUCCESS)
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
            return ret_code

        cmd = 'bash ' + workdir + '/miniconda.sh -b -p ' + workdir + '/miniconda'
        # run the command, set verbose=False 
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL...installing miniconda")
            return ret_code

        # REVISIT : check for installation finished in output                                                         
        self.conda_path = workdir + '/miniconda/bin/'
        conda_path = self.conda_path

        if sys.platform == 'linux':
            cmd = "echo 'conda ==4.3.21' >> " + workdir + '/miniconda/conda-meta/pinned'
            ret_code = run_cmd(cmd)
            if ret_code != SUCCESS:
                print("FAIL...installing miniconda")
                return(ret_code)

        cmd = conda_path + 'conda install gcc future'
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            return(ret_code)

        cmd = conda_path + 'conda config --set always_yes yes --set changeps1 no'
    
        ret_code = run_cmd(cmd)
        if ret_code != SUCCESS:
            print('FAILED: ' + cmd)
            return(ret_code)

        if sys.platform == 'darwin':
            cmd = conda_path + 'conda update -y -q conda'
            ret_code = run_cmd(cmd, True, False, False)
            if ret_code != SUCCESS:
                return(ret_code)

        cmd = conda_path + 'conda config --set anaconda_upload no'
        ret_code = run_cmd(cmd)
        print("xxxx returning from install_miniconda")
        return(ret_code)

    def install_nightly(self, py_ver):
        """
        install_nightly <env> <py_ver>:
           create an environment for cdat nightly.
           <env>    : environment name to be created
           <py_ver> : python version that the environment is to be created for
        """

        conda_path = self.conda_path
        if py_ver == 'py3':
            self.py3_env = 'nightly3'
            cmd = conda_path + "conda create -n " + self.py3_env + " cdat \"python>3\" "
            cmd += '-c uvcdat/label/nightly -c conda-forge -c uvcdat '
            cmd += '-c nesii/channel/dev-esmf'
        else:
            self.py2_env = 'nightly2'
            cmd = conda_path + "conda create -n " + self.py2_env + " cdat \"python<3\" "
            cmd += '-c uvcdat/label/nightly -c conda-forge -c uvcdat'

        ret_code = run_cmd(cmd, True, False, False)
        # REVISIT: check for nightly really installed
        return(ret_code)

