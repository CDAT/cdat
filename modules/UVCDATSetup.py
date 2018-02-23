import os
import sys

from Util import *

class UVCDATSetup(object):
    def __init__(self, conda_path, workdir, env_prefix):
        """
        env_prefix can be set to either 'nightly' or '2.12' etc..
        """
        if os.path.isdir(workdir):
            env_dir = workdir + '/miniconda/envs/' + env_prefix + '_py2'
            if os.path.isdir(env_dir):
                self.py2_env = env_prefix + '_py2'

            env_dir = workdir + '/miniconda/envs/' + env_prefix + '_py3'
            if os.path.isdir(env_dir):
                self.py3_env = env_prefix + '_py3'
        else:
            self.py2_env = None
            self.py3_env = None


        self.env_prefix = env_prefix
        self.workdir = workdir
        self.conda_path = conda_path
        os.system("uname -a")

    def get_env(self, py_ver):
        if py_ver == 'py2':
            return(self.py2_env)
        else:
            return(self.py3_env)


class NightlySetup(UVCDATSetup):

    def __init__(self, conda_path, workdir):
        super(NightlySetup, self).__init__(conda_path, workdir, 'nightly')

    def install(self, py_ver):
        """
        install_nightly <env> <py_ver>:
           create an environment for cdat nightly.
           <env>    : environment name to be created
           <py_ver> : python version that the environment is to be created for
        """
        conda_path = self.conda_path
        
        if not os.path.isdir(conda_path):
            raise Exception('Conda path: ' + conda_path + ' does not exist')

        if py_ver == 'py3':
            self.py3_env = 'nightly_py3'
            cmd = conda_path + "conda create -n " + self.py3_env + " cdat \"python>3\" "
            cmd += '-c uvcdat/label/nightly -c conda-forge -c uvcdat '
            cmd += '-c nesii/channel/dev-esmf'
        else:
            self.py2_env = 'nightly_py2'
            cmd = conda_path + "conda create -n " + self.py2_env + " cdat \"python<3\" "
            cmd += '-c uvcdat/label/nightly -c conda-forge -c uvcdat'

        ret_code = run_cmd(cmd, True, False, False)
        # REVISIT: check for nightly really installed
        return(ret_code)


class EnvSetup(UVCDATSetup):

    def __init__(self, conda_path, workdir, env_prefix):
        super(EnvSetup, self).__init__(conda_path, workdir, env_prefix)

    def install(self, py_ver):
        """
        install_nightly <env> <py_ver>:
           create an environment for cdat nightly.
           <py_ver> : python version that the environment is to be created for
        """

        conda_path = self.conda_path
        if not os.path.isdir(conda_path):
            raise Exception('Conda path: ' + conda_path + ' does not exist')

        env_prefix = self.env_prefix
        if py_ver == 'py2':
            env_name = env_prefix + '_py2'
            self.py2_env = env_name
        elif py_ver == 'py3':
            env_name = env_prefix + '_py3'
            self.py3_env = env_name
        else:
            raise Exception('invalid python version: ' + py_ver)

        # check if env already exists
        if os.path.isdir(conda_path + '../envs/' + env_name):
            print('Environment ' + env_name + ' already exists')
            return SUCCESS


        # download the env file
        dir = conda_path + '../envs'

        url = 'https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/'
        if sys.platform == 'darwin':
            env_file = env_prefix + '-osx.yml'
            url += env_file
            cmd = 'curl ' + url + ' -o ' + dir + '/' + env_file
        else:
            env_file = env_prefix + '.yml'
            url += env_file
            cmd = 'wget ' + url + ' -O ' + dir + '/' + env_file

        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            raise Exception('FAIL: ' + cmd)

        the_env_file = dir + '/' + env_file

        cmd = conda_path + '/conda env create -n ' + env_name
        cmd += ' -f ' + the_env_file
        
        ret_code = run_cmd(cmd, True, False, False)
        # REVISIT: check for nightly really installed
        return(ret_code)

