import sys
import os
import argparse
import datetime
thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

import CondaSetup
import UVCDATSetup
import TestSetup
from Const import *
from Util import *

parser = argparse.ArgumentParser(description="validate nightly install - verify test packages are up-to-date",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")

parser.add_argument("-p", "--py_ver",
                    help="python version, 'py2' or 'py3'")

parser.add_argument('-b', '--branch', nargs='?', default='master',
                    help="git branch of testsuite: 'master' or '2.12' or 'cdat-3.0.beta'")

args = parser.parse_args()
workdir = args.workdir
py_ver = args.py_ver
branch = args.branch

conda_setup = CondaSetup.CondaSetup(workdir)
status, conda_path = conda_setup.install_miniconda(workdir)
if status != SUCCESS:
    sys.exit(FAILURE)

if branch == 'master' or branch == '2.12':
    label = branch
else:
    label = 'v30beta'

if branch == 'master':
    uvcdat_setup = UVCDATSetup.NightlySetup(conda_path, workdir)
else:
    uvcdat_setup = UVCDATSetup.EnvSetup(conda_path, workdir, branch, label)

## REVISIT - packages should be parameter
packages = ["cdms", "cdutil", "genutil", "vcs", "pcmdi_metrics", "dv3d", "thermo", "wk"]
ret_code, installed_pkgs_dict = uvcdat_setup.get_packages_version(py_ver, packages)

for installed_pkg in installed_pkgs_dict.keys():
    
    if installed_pkg == 'cdms2':
        pkg = 'cdms'
    else:
        pkg = installed_pkg
    test_setup = TestSetup.TestSetup(uvcdat_setup, pkg, py_ver, branch)
    ret_code, last_commit_info = test_setup.get_last_commit()
    
    installed_pkg_datetime = installed_pkgs_dict[installed_pkg]['datetime']
    pkg_last_commit_datetime = last_commit_info['datetime']
    
    print("pkg: {p}".format(p=pkg))
    print("   installed: {i}, last_commit_date: {c}".format(i=installed_pkgs_dict[installed_pkg]['date_str'],
                                                            c=last_commit_info['date_str']))

    if abs(installed_pkg_datetime - pkg_last_commit_datetime) > datetime.timedelta(days=2):
        print("ERROR...installed_pkg_datetime: {d} differs".format(d=installed_pkg_datetime))
        print("        more than 2 days from pkg last commit datetime: {d}".format(d=last_commit_info['datetime']))
###
