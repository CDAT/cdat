import os
import sys
import re
from datetime import datetime

from Util import *

# UVCDATSetup() __init__ added py_ver
# remove get_env_name()
# remove set_env_name()
# remove py_ver from install_packages() argument
class UVCDATSetup(object):
    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        """
        env_prefix can be set to either 'nightly' or '2.12' etc.
           This prefix will be concatenated with either '_py2' or '_py3'
           for the environment name, so that multiple environments can
           use same conda installation.
        """

        if os.path.isdir(workdir):
            if not os.path.isdir(conda_path):
                raise Exception('Conda path: ' + conda_path + ' does not exist')

        env_name = "{prefix}_{py_ver}".format(prefix=env_prefix,
                                              py_ver=py_ver)
        self.env_name = env_name
        self.label = label
        self.env_prefix = env_prefix
        self.py_ver = py_ver
        self.workdir = workdir
        self.conda_path = conda_path
        os.system("uname -a")

    def get_env_name(self):
        return(self.env_name)

    def check_if_env_exists(self, env_name):
        env_dir = os.path.join(self.conda_path, '..', 'envs', env_name)
        if os.path.isdir(env_dir):
            print("INFO...environment {env} already exists".format(env=env_name))
            return True
        else:
            return False

    def install_packages(self, py_ver, packages, add_channels=[]):
        """
        installs the specified packages from standard channels
           (conda-forge and uvcdat) and any additional channels (if specified)
           in the uvcdat environment created. 
        packages - space separated package names
        add_channels - is a list of channels
        """
        env = self.env_name
        channels = "-c cdat/label/{l} -c uvcdat/label/{l} -c conda-forge -c uvcdat ".format(l=self.label)
        for channel in add_channels:
            channels += " -c {} ".format(channel)

        cmd = "conda install {} {} > /dev/null 2>&1".format(channels, packages)
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            print("FAIL...{c}".format(c=cmd))
        return(ret_code)

    def conda_list(self):
        cmds_list = ["conda list"]
        ret_code = run_in_conda_env(self.conda_path, self.env_name, cmds_list)
        return(ret_code)

    def get_packages_version(self, py_ver, packages):
        """
        This function gets the version of each package listed in <packages>
           that is installed in the environment.

        py_ver  : python version - this is used to refer to the right
                  uvcdat environment to check.
        packages: list of packages

        Return value:
           a dictionary with package names as the keys, and the value of 
           each dict entry is the package version (in date format 
           month/day/year)
        """
        env = self.env_name
        package_versions_dict = {}
        for package in packages:
            cmds_list = ["conda list {pkg}".format(pkg=package)]

            ret_code, output = run_in_conda_env_capture_output(self.conda_path,
                                                               env,
                                                               cmds_list)
            # Output of 'conda list <pkg>' example:
            # vcs       2.12.2018.02.26.16.25.ge721529 py27_0    uvcdat/label/nightly
            # vcsaddons 2.12.2018.02.28.18.09.g17c1f38 py27_0    uvcdat/label/nightly

            for a_line in output:
                if not a_line.startswith("#"):
                    compressed_line_list = re.sub("\s+", " ", a_line).split(" ")
                    pkg_name = compressed_line_list[0]
                    version = compressed_line_list[1]
                    match_obj = re.match(r'.*(20\d\d).(\d\d).(\d\d).*', version)
                    if match_obj:
                        m = match_obj.group(2)
                        d = match_obj.group(3)
                        y = match_obj.group(1)
                        version_date = "{m}/{d}/{y}".format(m=m, d=d, y=y)
                        #version_datetime = datetime.date(day=int(d), year=int(y), month=int(m))
                        version_datetime = datetime.strptime(version_date, "%m/%d/%Y")
                        package_versions_dict[pkg_name] = {}
                        package_versions_dict[pkg_name]['date_str'] = version_date
                        package_versions_dict[pkg_name]['datetime'] = version_datetime


        for pkg in package_versions_dict.keys():
            print("pkg: {pkg}, version date: {v}".format(pkg=pkg,
                                                         v=package_versions_dict[pkg]['date_str']))
        return(ret_code, package_versions_dict)

class NightlySetup(UVCDATSetup):

    def __init__(self, conda_path, workdir, py_ver):
        super(NightlySetup, self).__init__(conda_path, workdir, 'nightly', py_ver, 'nightly')
        
    def install(self):
        """
        install_nightly <env> <py_ver>:
           create an environment for cdat nightly.
           <env>    : environment name to be created
           <py_ver> : python version that the environment is to be created for
        """
        conda_path = self.conda_path        
        env_name = self.env_name

        # check if env already exists
        if self.check_if_env_exists(env_name): 
            print("INFO...environment {env} already exists".format(env=env_name))
            return SUCCESS

        conda_cmd = os.path.join(conda_path, 'conda')
        ch1 = "-c cdat/label/nightly -c uvcdat/label/nightly"
        ch2 = "-c nesii/label/dev-esmf -c conda-forge -c uvcdat"
        ch3 = "-c pcmdi/label/nightly -c pcmdi"

        if self.py_ver == 'py3':
            py_str = "python>3"
        else:
            py_str = "python<3"

        pkgs = "nose mesalib image-compare pcmdi_metrics cia easydev nbsphinx"
        cmd = "{c} create -n {e} cdat {pkgs} \"{p}\" {c1} {c2} {c3}".format(c=conda_cmd,
                                                                            e=env_name,
                                                                            pkgs=pkgs,
                                                                            p=py_str,
                                                                            c1=ch1,
                                                                            c2=ch2,
                                                                            c3=ch3)

        ret_code = run_cmd(cmd, True, False, False)

        return(ret_code)


class EnvSetup(UVCDATSetup):

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        super(EnvSetup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def __download_env_file(self, env_file_name):
        """
        This function download the specified env_file_name
        to dest_file under workdir
        """
        url_location = 'https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/'
        env_file_url = "{u}/{f}".format(u=url_location, f=env_file_name)
        dest_file = os.path.join(self.workdir, env_file_name)

        if sys.platform == 'darwin':
            cmd = "curl {u} -o {env_f}".format(u=env_file_url, 
                                               env_f=dest_file)
        else:
            cmd = "wget {u} -O {env_f}".format(u=env_file_url, 
                                               env_f=dest_file)
        
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            print("FAIL: {c}".format(c=cmd))
        return(ret_code, dest_file)


    def install_from_env_file(self, py_ver, env_file_name):
        """
        This method creates a CDAT environment from a environment file
           <py_ver> : python version that the environment is to be created for
        """

        conda_path = self.conda_path
        env_name = self.env_name

        # check if env already exists
        if self.check_if_env_exists(env_name): 
            print("INFO...environment {env} already exists".format(env=env_name))
            return SUCCESS

        # download the env file
        workdir = self.workdir
        env_prefix = self.env_prefix
        thisDir = os.path.abspath(os.path.dirname(__file__))
        full_path_env_file = os.path.join(thisDir, '..', 'conda', env_file_name)
        print("DEBUG..full_path_env_file: " + full_path_env_file)

        conda_cmd = os.path.join(conda_path, 'conda')
        cmd = "{c} env create -n {e} -f {f}".format(c=conda_cmd, e=env_name, f=full_path_env_file)
        ret_code = run_cmd(cmd, True, False, True)

        return(ret_code)

    def install_packages_for_tests(self):

        print("DEBUG DEBUG...in install_packages_for_test()...")
        if "nox" in self.env_prefix:
            pkgs = "nose image-compare pcmdi_metrics cia easydev nbsphinx"
        else:
            pkgs = "nose mesalib image-compare pcmdi_metrics cia easydev nbsphinx"
        
        #cmds_list = ["conda install -c conda-forge -c uvcdat {p}".format(p=pkgs)]
        channels = "-c conda-forge -c uvcdat -c pcmdi/label/nightly -c pcmdi"
        cmds_list = ["conda install {channels} {pkgs}".format(channels=channels,
                                                              pkgs=pkgs)]
        env = self.env_name
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            print("FAIL...{cmd}".format(cmd=cmd))
            return(ret_code)

        return(ret_code)

class Env212Setup(EnvSetup):

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        super(Env212Setup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def install(self, py_ver):
        """
        This method installs 2.12 environment.
           <py_ver> : python version that the environment is to be created for
        """
        env_prefix = self.env_prefix
        if sys.platform == 'darwin':
            env_file = "{prefix}-osx.yml".format(prefix=env_prefix)
        else:
            env_file = "{prefix}.yml".format(prefix=env_prefix)

        ret_code = super(Env212Setup, self).install_from_env_file(py_ver, env_file)
        if ret_code != SUCCESS:
            print("FAIL..install_from_env_file(), env_file: {e}".format(e=env_file))

        return(ret_code)

    def install_packages_for_tests(self, py_ver):
        ret_code = super(Env212Setup, self).install_packages_for_tests(py_ver)
        if ret_code != SUCCESS:
            return(ret_code)

        packages = "flake8=3.3.0"
        ret_code = super(EnvSetup, self).install_packages(py_ver, packages)

        return(ret_code)

class Env30Setup(EnvSetup):

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        super(Env30Setup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def install(self):
        """
        This method installs 3.0 Beta environment.
           <py_ver> : python version that the environment is to be created for
        """
        env_prefix = self.env_prefix
        py_ver = self.py_ver
        if sys.platform == 'darwin':
            env_file = "{prefix}_{py_ver}.Darwin.yaml".format(prefix=env_prefix, py_ver=py_ver)
        else:
            env_file = "{prefix}_{py_ver}.Linux.yaml".format(prefix=env_prefix, py_ver=py_ver)

        ret_code = super(Env30Setup, self).install_from_env_file(py_ver, env_file)
        if ret_code != SUCCESS:
            print("FAIL..install_from_env_file(), env_file: {e}".format(e=env_file))

        return(ret_code)


