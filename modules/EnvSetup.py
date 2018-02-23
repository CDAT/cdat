import os
import sys

from Util import *
import CondaSetup

class EnvSetup(UVCDATSetup):

    def __init__(self, conda_path, workdir, env_prefix):
        """
        env_prefix can be '2.12', '2.10', '2.8' or '2.6'
        """
        super(NightlySetup, self).__init__(conda_path, workdir, env_prefix)

    def install(self, conda_path, version):
        """
        install_nightly <env> <py_ver>:
           create an environment for cdat nightly.
           <env>    : environment name to be created
           <py_ver> : python version that the environment is to be created for
        """
        print("DEBUG...EnvSetup.install(), version: " + version)
        # check if env already exists
        if os.path.isdir(conda_path + '../envs/' + version) == True:
            print('Environment ' + version + ' already exists')
            return SUCCESS

        if os.path.isdir(conda_path) == False:
            raise Exception('Conda path: ' + conda_path + ' does not exist')

        # download the env file
        dir = conda_path + '../envs'
        self.env = version + '_env'
        url = 'https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/'
        if sys.platform == 'darwin':
            env_file = version + '-osx.yml'
            url += env_file
            cmd = 'curl ' + url + ' -o ' + dir + '/' + env_file
        else:
            env_file = version + '.yml'
            url += env_file
            cmd = 'wget ' + url + ' -O ' + dir + '/' + env_file

        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            raise Exception('FAIL: ' + cmd)

        the_env_file = dir + '/' + env_file

        cmd = conda_path + '/conda env create -f ' + the_env_file
        ret_code = run_cmd(cmd, True, False, False)
        # REVISIT: check for nightly really installed
        return(ret_code)

