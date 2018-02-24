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

    def install_packages(self, py_ver, packages, add_channels=[]):
        """
        installs the specified packages from standard channels and 
        any additional channels (if specified)
        packages - space separated package names
        add_channels - is a list of channels
        """
        env = self.get_env(py_ver)
        channels = " -c uvcdat/label/{} -c conda-forge -c uvcdat ".format(self.env_prefix)
        for channel in add_channels:
            channels += " -c {} ".format(channel)

        cmd = "conda install {} {} > /dev/null 2>&1".format(channels, packages)
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            raise Exception('Failed in installing packages')

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

        conda_cmd = os.path.join(conda_path, 'conda')
        if py_ver == 'py3':
            self.py3_env = 'nightly_py3'
            cmd = conda_cmd + " create -n " + self.py3_env + " cdat \"python>3\" "
            cmd += '-c uvcdat/label/nightly -c conda-forge -c uvcdat '
            cmd += '-c nesii/channel/dev-esmf'
        else:
            self.py2_env = 'nightly_py2'
            cmd = conda_cmd + " create -n " + self.py2_env + " cdat \"python<3\" "
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
        workdir = self.workdir
        url = 'https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/'
        if sys.platform == 'darwin':
            env_file = env_prefix + '-osx.yml'
            url += env_file
            full_path_env_file = os.path.join(workdir, env_file)
            cmd = "curl {u} -o {env_f}".format(u=url, env_f=full_path_env_file)
        else:
            env_file = env_prefix + '.yml'
            url += env_file
            full_path_env_file = os.path.join(workdir, env_file)
            cmd = "wget {u} -O {env_f}".format(u=url, env_f=full_path_env_file)

        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            raise Exception('FAIL: ' + cmd)

        conda_cmd = os.path.join(conda_path, 'conda')
        cmd = "{c} env create -n {e} -f {f}".format(c=conda_cmd, e=env_name, f=full_path_env_file)
        
        ret_code = run_cmd(cmd, True, False, False)
        # REVISIT: check for nightly really installed
        return(ret_code)

