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

    def get_env_name(self):
        return(self.env_name)

    def check_if_env_exists(self, env_name):
        env_dir = os.path.join(self.conda_path, '..', 'envs', env_name)
        if os.path.isdir(env_dir):
            print("INFO...environment {env} already exists".format(env=env_name))
            return True
        else:
            return False

    def conda_list(self):
        cmds_list = ["conda list"]
        ret_code = run_in_conda_env(self.conda_path, self.env_name, cmds_list)
        return(ret_code)

    def conda_env_export(self):
        yaml_file = os.path.join(self.workdir, "{e}_env.yaml".format(e=self.env_name))
        cmd = "conda env export > {yaml_file}".format(yaml_file=yaml_file)
        cmds_list = [cmd]
        ret_code = run_in_conda_env(self.conda_path, self.env_name, cmds_list)
        return(ret_code)

    def install_packages_for_tests(self):

        pkgs = "nose coverage image-compare pcmdi_metrics cia easydev nbsphinx"
        if "nox" not in self.env_prefix:
            pkgs += " mesalib"
        
        channels = "-c conda-forge -c cdat -c pcmdi/label/nightly -c pcmdi"
        cmds_list = ["conda install {channels} {pkgs}".format(channels=channels,
                                                              pkgs=pkgs)]
        env = self.env_name
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            print("FAIL...{cmd}".format(cmd=cmd))
            return(ret_code)

        return(ret_code)

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
        if sys.platform == 'darwin':
            ch1 = "-c danlipsa -c cdat/label/nightly -c conda-forge -c cdat"
        else:
            ch1 = "-c cdat/label/nightly -c conda-forge -c cdat"
        ch2 = "-c pcmdi/label/nightly -c pcmdi"

        base_pkgs = "openblas mesalib image-compare pcmdi_metrics cia easydev nbsphinx myproxyclient testsrunner coverage"
        temp_settings = "\"libnetcdf >4.6\" \"hdf5 >=1.10.2\" numpy=1.14.5 \"proj4<5\" \"vtk-cdat>8.1\""
        if sys.platform == 'darwin':
            temp_settings = "{} \"ffmpeg>4\" \"libpng>1.6.34\"".format(temp_settings)

        pkgs = "{p} {t}".format(p=base_pkgs, t=temp_settings)

        if self.py_ver == 'py3':
            py_str = "python>3"
        else:
            py_str = "python<3"
            
        cmd = "{c} create -n {e} cdat {pkgs} \"{p}\" {c1} {c2}".format(c=conda_cmd,
                                                                       e=env_name,
                                                                       pkgs=pkgs,
                                                                       p=py_str,
                                                                       c1=ch1,
                                                                       c2=ch2)
        ret_code = run_cmd(cmd, True, False, True)

        # TEMPORARY TEMPORARY - remove the following install when changes
        # of testsrunner for coverage is in nightly
        # cmd = "{c} install -n {e} -c cdat/label/linatest testsrunner".format(c=conda_cmd,
        #                                                                     e=env_name)
        # ret_code1 = run_cmd(cmd, True, False, True)

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
            cmds_list = ["{c}/conda list {pkg}".format(c=self.conda_path,
                                                       pkg=package)]

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

class EnvSetup(CDATSetup):

    """
    EnvSetup is a subclass of CDATSetup for a CDAT environment 
    created from yaml file.
    """

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        super(EnvSetup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def __construct_env_file_name(self, env_prefix, py_ver):
        if sys.platform == 'darwin':
            env_file = "{pr}_{py_ver}.Darwin.yaml".format(pr=env_prefix, 
                                                          py_ver=py_ver)
        else:
            env_file = "{pr}_{py_ver}.Linux.yaml".format(pr=env_prefix, 
                                                         py_ver=py_ver)
        return(env_file)

    def __install_from_env_file(self, env_file_name):
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

        conda_cmd = os.path.join(conda_path, 'conda')
        cmd = "{c} env create -n {e} -f {f}".format(c=conda_cmd, e=env_name, f=full_path_env_file)
        ret_code = run_cmd(cmd, True, False, True)

        return(ret_code)

    def install(self):

        env_prefix = self.env_prefix
        py_ver = self.py_ver
        env_file = self.__construct_env_file_name(env_prefix, py_ver)

        ret_code = self.__install_from_env_file(env_file)
        if ret_code != SUCCESS:
            print("FAIL..install_from_env_file(), env_file: {e}".format(e=env_file))

        return(ret_code)



class EnvFromChannelSetup(CDATSetup):

    """
    EnvFromChannelSetup is a subclass of CDATSetup. 
    This class is for cdat env that is created by installing for conda channels.
    """

    def __init__(self, conda_path, workdir, env_prefix, py_ver, label):
        """
        py_ver: 'py2' or 'py3'
        label: 'v80' or 'latest'
           if label == 'v80', env will be created with this command:
               conda create -n <env_name> -c conda-forge -c cdat/label/v80 <python_version> cdat
           if label == 'latest', env will be created with this command:
               conda create -n <env_name>-c conda-forge -c cdat <python_version> cdat
        """
        super(EnvFromChannelSetup, self).__init__(conda_path, workdir, env_prefix, py_ver, label)

    def install(self):
        env_prefix = self.env_prefix
        py_ver = self.py_ver
        label = self.label

        env_name = "{pref}_{py_ver}".format(pref=env_prefix, py_ver=py_ver)
        #if label == 'v80':
        #    channel = "-c conda-forge -c cdat/label/{l}".format(l=label)
        #elif label == 'latest':
        #    channel = "-c cdat/label/v80 -c conda-forge -c cdat"
        #else:
            # TEMPORARY
            #channel = "-c cdat/label/{l} -c cdat/label/nightly -c conda-forge -c cdat \"cdms2>3.0.1\"".format(l=label)
        channel = "-c cdat/label/{l} -c conda-forge -c cdat".format(l=label)
        conda_path = self.conda_path
        conda_cmd = os.path.join(conda_path, "conda")
        if py_ver == 'py2':
            py_ver_str = "python=2.7"
        else:
            py_ver_str = "python=3.6"

        cmd = "{c} create -n {n} {channel} {ver} cdat".format(c=conda_cmd,
                                                              n=env_name,
                                                              channel=channel,
                                                              ver=py_ver_str)

        ret_code = run_cmd(cmd, True, False, True)
        return(ret_code)


