import os
import sys
import re
from datetime import datetime

from Util import *

class CDATSetup(object):

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        """
        CDATSetup is a base class fpr a uvcdat setup.

        conda_path - where conda is already installed.
           Note that multiple CDATSetup can share a conda setup.
        workdir - working directory
           This is where environment yaml file will be downloaded
           to, and this is also where testsuites will be cloned to.

        env_prefix - can be set to one of these:
           'nightly' for nightly install
           'cdat-3.0.beta2-nox' for install from env file
           'cdat-3.0.beta2' for install from env file
        py_ver - can be either 'py2' or 'py3'
        label - is a conda label for cdat and uvcdat.
           packages will be installed from the following channels:
           -c cdat/label/<label> -c uvcdat/label/<label>


        The cdat environment that will get created will be named 
        as <env_prefix>_<py_ver>.
        When installing from an environment file, the environment 
        file is expected to following the following naming
        convention:
           <env_prefix>_<py_ver>.[Darwin|Linux].yaml
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
           -c cdat/label/<label> -c uvcdat/label/<label> 
           -c conda-forge -c uvcdat
           and any additional channels specified <add_channels>
        packages - space separated package names
        add_channels - is a list of channels

        Return value: SUCCESS or FAILURE
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

    def get_packages_version(self, packages):
        """
        This function gets the version of each package listed in <packages>
           that is installed in the environment.
        packages: list of packages
        Return value:
           a dictionary with package names as the keys, and the value of 
           each dict entry is another dictionary with the following keys:
              'date_str': version of package in date format m/d/y.
                          This value is more for logging and debugging.
              'datetime': version of package in datetime form.
                          This is for comparison purpose.
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
                        version_datetime = datetime.strptime(version_date, "%m/%d/%Y")
                        package_versions_dict[pkg_name] = {}
                        package_versions_dict[pkg_name]['date_str'] = version_date
                        package_versions_dict[pkg_name]['datetime'] = version_datetime

        for pkg in package_versions_dict.keys():
            print("pkg: {pkg}, version date: {v}".format(pkg=pkg,
                                                         v=package_versions_dict[pkg]['date_str']))
        return(ret_code, package_versions_dict)

class NightlySetup(CDATSetup):
    """
    NightlySetup is a subclass of CDATSetup, specifically for 
    nightly install.
    """

    def __init__(self, conda_path, workdir, py_ver):
        super(NightlySetup, self).__init__(conda_path, workdir, 'nightly', py_ver, 'nightly')
        
    def install(self):
        """
        install_nightly <env> <py_ver>:
           creates an environment for cdat nightly.
           The envirnonment name is set to <env>_<py_ver>
             
        """
        conda_path = self.conda_path        
        env_name = self.env_name

        # check if env already exists
        if self.check_if_env_exists(env_name): 
            print("INFO...environment {env} already exists".format(env=env_name))
            return SUCCESS

        conda_cmd = os.path.join(conda_path, 'conda')
        #ch1 = "-c cdat/label/nightly -c uvcdat/label/nightly"
        #ch2 = "-c nesii/label/dev-esmf -c conda-forge -c uvcdat"
        #ch3 = "-c pcmdi/label/nightly -c pcmdi"

        ch1 = "-c cdat/label/nightly -c nesii/label/dev-esmf -c conda-forge -c cdat"

        if self.py_ver == 'py3':
            ch2 = "-c pcmdi"
            py_str = "python>3"
            pkgs = "nose mesalib image-compare cia easydev nbsphinx \"proj4<5\""
        else:
            ch2 = "-c pcmdi/label/nightly -c pcmdi"
            py_str = "python<3"
            pkgs = "nose mesalib image-compare pcmdi_metrics cia easydev nbsphinx \"proj4<5\""

            
        cmd = "{c} create -n {e} cdat {pkgs} \"{p}\" {c1} {c2}".format(c=conda_cmd,
                                                                       e=env_name,
                                                                       pkgs=pkgs,
                                                                       p=py_str,
                                                                       c1=ch1,
                                                                       c2=ch2)

        ret_code = run_cmd(cmd, True, False, True)

        return(ret_code)


class EnvSetup(CDATSetup):

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        super(EnvSetup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def construct_env_file_name(self, env_prefix, py_ver):
        if sys.platform == 'darwin':
            env_file = "{pr}_{py_ver}.Darwin.yaml".format(pr=env_prefix, 
                                                          py_ver=py_ver)
        else:
            env_file = "{pr}_{py_ver}.Linux.yaml".format(pr=env_prefix, 
                                                         py_ver=py_ver)
        return(env_file)

    def install_from_env_file(self, env_file_name):
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

    def install(self):
        """
        This method installs 3.0 Beta environment.
        """
        env_prefix = self.env_prefix
        py_ver = self.py_ver
        env_file = self.construct_env_file_name(env_prefix, py_ver)

        ret_code = self.install_from_env_file(env_file)
        if ret_code != SUCCESS:
            print("FAIL..install_from_env_file(), env_file: {e}".format(e=env_file))

        return(ret_code)


class Env30SetupOBSOLETE(EnvSetup):

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        super(Env30Setup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def install(self):
        """
        This method installs 3.0 Beta environment.
        """
        env_prefix = self.env_prefix
        py_ver = self.py_ver
        env_file = self.construct_env_file_name(env_prefix, py_ver)

        ret_code = super(Env30Setup, self).install_from_env_file(env_file)
        if ret_code != SUCCESS:
            print("FAIL..install_from_env_file(), env_file: {e}".format(e=env_file))

        return(ret_code)


