import sys
import os
import argparse
import datetime
thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

import CondaSetup
import CDATSetup
import TestSetup
from Const import *
from Util import *

#
# This script compares the version of packages installed in the
# cdat environment against the last commited version in git.
#
parser = argparse.ArgumentParser(description="validate nightly install - verify test packages are up-to-date",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")
parser.add_argument('-v', '--env_prefix', nargs='?', default='nightly',
                    help="env_prefix of cdat environment")
parser.add_argument("-p", "--py_ver",
                    help="python version, 'py2' or 'py3'")
parser.add_argument('-b', '--branch', nargs='?', default='master',
                    help="git branch to check out testsuite from.")
parser.add_argument('-l', '--label', nargs='?', default='master',
                    help="git label: 'master' or other git label, like 'v3.0'")

args = parser.parse_args()
workdir = args.workdir
py_ver = args.py_ver
env_prefix = args.env_prefix
branch = args.branch
label = args.label

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
if status != SUCCESS:
    sys.exit(FAILURE)

if env_prefix == 'nightly' or "cdat-3.0" in env_prefix:
    conda_label = 'nightly'
else:
    print("ERROR...invalid env_prefix: {v}".format(v=env_prefix))
    sys.exit(FAILURE)

if env_prefix == 'nightly':
    cdat_setup = CDATSetup.NightlySetup(conda_path, workdir, py_ver)
else:
    cdat_setup = CDATSetup.EnvSetup(conda_path, workdir, env_prefix, py_ver, conda_label)

## REVISIT - packages should be parameter
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

    if abs(installed_pkg_datetime - pkg_last_commit_datetime) > datetime.timedelta(days=2):
        print("ERROR ERROR...")
        print("   installed_pkg_datetime: {d} differs".format(d=installed_pkg_datetime))
        print("   more than 2 days from pkg last commit datetime: {d}".format(d=last_commit_info['datetime']))


