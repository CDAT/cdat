import sys
import os
import argparse
import datetime

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

import CondaSetup
import CDATSetup
import TestSetup
from Const import *
from Util import *

#
# This script compares the version of packages installed in the
# cdat environment against the last commited version in git.
#
valid_env_prefixes = ["nightly"]
valid_git_labels = ["master"]
valid_py_vers = ["py2", "py3"]

parser = argparse.ArgumentParser(description="validate nightly install - verify test packages are up-to-date",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir", required=True,
                    help="working directory -- miniconda will be installed here")
parser.add_argument('-v', '--env_prefix', nargs='?', default='nightly',
                    help="env_prefix of cdat environment", choices=valid_env_prefixes)
parser.add_argument("-p", "--py_ver", required=True,
                    help="python version", choices=valid_py_vers)
parser.add_argument('-b', '--branch', nargs='?', default='master',
                    help="git branch to check out testsuite from.")
parser.add_argument('-l', '--label', nargs='?', default='master',
                    help="git label", choices=valid_git_labels)

args = parser.parse_args()
workdir = args.workdir
py_ver = args.py_ver
env_prefix = args.env_prefix
branch = args.branch
label = args.label

#
# Get the conda environment that should be set up already
# install_miniconda() will not install conda again if the conda environment
# is already set up.

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
if status != SUCCESS:
    sys.exit(FAILURE)

conda_label = 'nightly'

cdat_setup = CDATSetup.NightlySetup(conda_path, workdir, py_ver)
packages = ["cdms", "cdutil", "genutil", "vcs", 
            "pcmdi_metrics", "dv3d", "thermo", "wk"]
ret_code, installed_pkgs_dict = cdat_setup.get_packages_version(packages)

for installed_pkg in installed_pkgs_dict.keys():

    if installed_pkg == 'cdms2':
        pkg = 'cdms'
    else:
        pkg = installed_pkg
    test_setup = TestSetup.TestSetup(cdat_setup, pkg, py_ver, branch, label)
    ret_code, last_commit_info = test_setup.get_last_commit()
    
    installed_pkg_datetime = installed_pkgs_dict[installed_pkg]['datetime']
    pkg_last_commit_datetime = last_commit_info['datetime']
    
    print("pkg: {p}".format(p=pkg))
    print("   installed: {i}, last_commit_date: {c}".format(i=installed_pkgs_dict[installed_pkg]['date_str'],
                                                            c=last_commit_info['date_str']))

    if installed_pkg_datetime > pkg_last_commit_datetime:
        print("WARNING WARNING...")
        print("  installed_pkg_datetime: {d} is later than".format(d=installed_pkg_datetime))
        print("  pkg last commit datetime: {d}".format(d=last_commit_info['datetime']))
    elif abs(installed_pkg_datetime - pkg_last_commit_datetime) > datetime.timedelta(days=2):
        print("ERROR ERROR ERROR ***************")
        print("   installed_pkg_datetime: {d} differs".format(d=installed_pkg_datetime))
        print("   more than 2 days from pkg last commit datetime: {d}".format(d=last_commit_info['datetime']))

