import os
import sys

from Util import *

class UVCDATSetup(object):
    def __init__(self, conda_path, workdir, env_prefix, label):
        """
        env_prefix can be set to either 'nightly' or '2.12' etc..
        """
        if os.path.isdir(workdir):
            print("xxx HERE HERE HERE ")
            env_name = "{prefix}_py2".format(prefix=env_prefix)
            env_dir = os.path.join(workdir, 'miniconda', 'envs', env_name)
            if os.path.isdir(env_dir):
                self.py2_env = env_name

            env_name = "{prefix}_py3".format(prefix=env_prefix)
            env_dir = os.path.join(workdir, 'miniconda', 'envs', env_name)
            if os.path.isdir(env_dir):
                self.py3_env = env_name
            self.label = label
        else:
            self.py2_env = None
            self.py3_env = None
            self.label = None

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
        installs the specified packages from standard channels
        (conda-forge and uvcdat) and any additional channels (if specified)
        packages - space separated package names
        add_channels - is a list of channels
        """
        env = self.get_env(py_ver)
        channels = " -c uvcdat/label/{} -c conda-forge -c uvcdat ".format(self.label)
        for channel in add_channels:
            channels += " -c {} ".format(channel)

        cmd = "conda install {} {} > /dev/null 2>&1".format(channels, packages)
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            raise Exception('Failed in installing packages')
        return(ret_code)

class NightlySetup(UVCDATSetup):

    def __init__(self, conda_path, workdir):
        super(NightlySetup, self).__init__(conda_path, workdir, 'nightly', 'nightly')

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
            channels = '-c uvcdat/label/nightly -c conda-forge -c uvcdat -c nesii/channel/dev-esmf'
            cmd = "{c} create -n {e} cdat \"python>3\" {ch}".format(c=conda_cmd,
                                                                    e=self.py3_env,
                                                                    ch=channels)
            env = self.py3_env
        else:
            self.py2_env = 'nightly_py2'
            channels = '-c uvcdat/label/nightly -c conda-forge -c uvcdat'
            cmd = "{c} create -n {e} cdat \"python<3\" {ch}".format(c=conda_cmd,
                                                                    e=self.py2_env,
                                                                    ch=channels)
            env = self.py2_env
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            return(ret_code)

        cmds_list = ["conda list"]
        ret_code = run_in_conda_env(conda_path, env, cmds_list)

        return(ret_code)

    def install_packages(self, py_ver):
        packages = "nose mesalib image-compare \\\"matplotlib<2.1\\\" numpy=1.13 "
        packages += "vcs vcsaddons cdp mesalib flake8"
        ret_code = super(NightlySetup, self).install_packages(py_ver, packages)
        if ret_code != SUCCESS:
            return(ret_code)

        packages = 'pcmdi_metrics cia'
        channels_list = ['pcmdi/label/nightly', 'pcmdi']
        ret_code = super(NightlySetup, self).install_packages(py_ver, packages, channels_list)
        if ret_code != SUCCESS:
            return(ret_code)

        if py_ver == 'py2':
            env = self.py2_env
        else:
            env = self.py3_env
        cmds_list = ["conda list"]
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        return(ret_code)

class EnvSetup(UVCDATSetup):

    def __init__(self, conda_path, workdir, env_prefix, label):
        print("xxx env_prefix: " + env_prefix)
        print("xxx label: " + label)
        super(EnvSetup, self).__init__(conda_path, workdir, env_prefix, label)

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
            env_name = "{prefix}_py2".format(prefix=env_prefix)
            self.py2_env = env_name
        elif py_ver == 'py3':
            env_name = "{prefix}_py3".format(prefix=env_prefix)
            self.py3_env = env_name
        else:
            raise Exception('invalid python version: ' + py_ver)

        # check if env already exists
        env_dir = os.path.join(conda_path, '..', 'envs', env_name)
        if os.path.isdir(env_dir):
            print("INFO...environment {env} already exists".format(env=env_name))
            return SUCCESS

        # download the env file
        workdir = self.workdir
        url = 'https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/'
        if sys.platform == 'darwin':
            if env_prefix == '2.12':
                env_file = "{prefix}-osx.yml".format(prefix=env_prefix)
            else:
                env_file = "{prefix}_{py_ver}.Darwin.yaml".format(prefix=env_prefix, py_ver=py_ver)
            url += env_file
            full_path_env_file = os.path.join(workdir, env_file)
            cmd = "curl {u} -o {env_f}".format(u=url, env_f=full_path_env_file)
        else:
            if env_prefix == '2.12':
                env_file = "{prefix}.yml".format(prefix=env_prefix)
            else:
                env_file = "{prefix}_{py_ver}.Linux.yaml".format(prefix=env_prefix, py_ver=py_ver)
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

    def install_packages(self, py_ver):
        packages = "nose mesalib image-compare \\\"matplotlib<2.1\\\" numpy=1.13 "
        packages += "vcs vcsaddons cdp mesalib flake8=3.3.0"
        ret_code = super(EnvSetup, self).install_packages(py_ver, packages)
        if ret_code != SUCCESS:
            return(ret_code)

        packages = 'pcmdi_metrics cia'
        pcmdi_label = "pcmdi/label/{label}".format(label=self.label)
        channels_list = [pcmdi_label, 'pcmdi']
        ret_code = super(EnvSetup, self).install_packages(py_ver, packages, channels_list)
        return(ret_code)

