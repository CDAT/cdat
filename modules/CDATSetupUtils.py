import os
import sys
import re
from datetime import datetime

from Util import *
from Const import *

def check_if_env_exists(conda_path, env_name):
    env_dir = os.path.join(conda_path, '..', 'envs', env_name)
    if os.path.isdir(env_dir):
        print("INFO...environment {env} already exists".format(env=env_name))
        return True
    else:
        return False

def conda_list(conda_path, env_name):
    cmds_list = ["conda list"]
    ret_code = run_in_conda_env(conda_path, env_name, cmds_list)
    return(ret_code)

def conda_env_export(workdir, conda_path, env_name):
    yaml_file = os.path.join(workdir, "{e}_env.yaml".format(e=env_name))
    cmd = "conda env export > {yaml_file}".format(yaml_file=yaml_file)
    cmds_list = [cmd]
    ret_code = run_in_conda_env(conda_path, env_name, cmds_list)
    return(ret_code)

def install_packages_for_tests(conda_path, env_name, pcmdi_from_nightly=True):

    pkgs = "nose coverage pcmdi_metrics cia easydev nbsphinx testsrunner myproxyclient pytest"
    if "nox" not in env_name:
        pkgs += " mesalib"
    
    if pcmdi_from_nightly:
        #channels = "-c cdat/label/{l} -c conda-forge -c pcmdi/label/nightly -c pcmdi".format(l=CONDA_LABEL)
        channels = "-c conda-forge -c cdat/label/{l} -c pcmdi/label/nightly -c pcmdi".format(l=CONDA_LABEL)
    else:
        channels = "-c conda-forge -c cdat/label/{l} -c pcmdi".format(l=CONDA_LABEL)

    cmds_list = ["conda install {channels} {pkgs}".format(channels=channels,
                                                          pkgs=pkgs)]
    ret_code = run_in_conda_env(conda_path, env_name, cmds_list)
    if ret_code != SUCCESS:
        print("FAIL FAIL...{cmd}".format(cmd=cmd))
        return(ret_code)
        
    return(ret_code)

def get_env_name(env_prefix, py_ver):
    env_name = "{prefix}_{py_ver}".format(prefix=env_prefix,
                                          py_ver=py_ver)

    return env_name

def construct_conda_py_str(py_ver):
    if py_ver == 'py3.6':
        py_str = "python>=3.6,<3.7"
    elif py_ver == 'py3.7':
        py_str = "python>=3.7"
    else:
        py_str = "python<3"
    return py_str

def install_nightly(workdir, conda_path, env_prefix, py_ver):

    env_name = get_env_name(env_prefix, py_ver)

    env_exists = check_if_env_exists(conda_path, env_name)
    if env_exists:
        print("INFO...environment {env} already exists".format(env=env_name))
        return SUCCESS

    conda_cmd = os.path.join(conda_path, 'conda')
    if sys.platform == 'darwin':
        ch1 = "-c cdat/label/nightly -c conda-forge"
    else:
        ch1 = "-c cdat/label/nightly -c conda-forge"
    ch2 = "-c pcmdi/label/nightly -c pcmdi"

    base_pkgs = "mesalib pcmdi_metrics cia easydev nbsphinx myproxyclient testsrunner coverage pytest"
    #temp_settings = "\"libnetcdf >4.6\" \"hdf5 >=1.10.2\" \"proj4<5\" \"vtk-cdat>8.1\""
    #if sys.platform == 'darwin':
    #    temp_settings = "{} \"ffmpeg>4\" \"libpng>1.6.34\"".format(temp_settings)

    #pkgs = "{p} {t}".format(p=base_pkgs, t=temp_settings)
    pkgs = base_pkgs

    py_str = construct_conda_py_str(py_ver)
        
    cmd = "{c} create -n {e} cdat {pkgs} \"{p}\" {c1} {c2}".format(c=conda_cmd,
                                                                   e=env_name,
                                                                   pkgs=pkgs,
                                                                   p=py_str,
                                                                   c1=ch1,
                                                                   c2=ch2)
    print("xxx xxx CMD: {c}".format(c=cmd))

    ret_code = run_cmd(cmd, True, False, True)
    return ret_code, env_name

def install_from_env_file(workdir, conda_path, env_prefix, py_ver):
    """
    This method creates a CDAT environment from a environment file
    <py_ver> : python version that the environment is to be created for
    """
    env_name = get_env_name(env_prefix, py_ver)
    
    if sys.platform == 'darwin':
        env_file = "{pr}_{py_ver}.Darwin.yaml".format(pr=env_prefix, 
                                                      py_ver=py_ver)
    else:
        env_file = "{pr}_{py_ver}.Linux.yaml".format(pr=env_prefix, 
                                                     py_ver=py_ver)

    # check if env already exists
    if check_if_env_exists(conda_path, env_name): 
        print("INFO...environment {env} already exists".format(env=env_name))
        return SUCCESS

    # download the env file
    thisDir = os.path.abspath(os.path.dirname(__file__))
    full_path_env_file = os.path.join(thisDir, '..', 'conda', env_file)

    conda_cmd = os.path.join(conda_path, 'conda')
    cmd = "{c} env create -n {e} -f {f}".format(c=conda_cmd, e=env_name, f=full_path_env_file)
    ret_code = run_cmd(cmd, True, False, True)

    return ret_code, env_name


def install_from_channel(workdir, conda_path, env_prefix, py_ver, conda_label):

    env_name = get_env_name(env_prefix, py_ver)

    # channel = "-c cdat/label/{l} -c conda-forge".format(l=conda_label)
    channel = "-c conda-forge -c cdat/label/{l}".format(l=conda_label)
    conda_cmd = os.path.join(conda_path, "conda")

    py_str = construct_conda_py_str(py_ver)

    cmd = "{c} create -n {n} {channel} \"{p}\" cdat mesalib".format(c=conda_cmd,
                                                                    n=env_name,
                                                                    channel=channel,
                                                                    p=py_str)

    ret_code = run_cmd(cmd, True, False, True)
    return ret_code, env_name


def get_packages_version(conda_path, env_name, packages):
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
    package_versions_dict = {}
    for package in packages:
        cmds_list = ["{c}/conda list {pkg}".format(c=conda_path,
                                                   pkg=package)]

        ret_code, output = run_in_conda_env_capture_output(conda_path,
                                                           env_name,
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
    print("\n")
    return(ret_code, package_versions_dict)
