import sys
import os
import argparse
import datetime

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Const import *
from Util import *
from CondaUtils import *
from CDATSetupUtils import *
from TestSetupUtils import *

#
# This script compares the version of packages installed in the
# cdat environment against the last commited version in git.
#
valid_env_prefixes = ["nightly"]
valid_git_labels = ["master"]
valid_py_vers = PYTHON_VERSIONS

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

conda_dir = get_conda_dir(workdir, py_ver)
conda_path = os.path.join(conda_dir, 'bin')

conda_label = 'nightly'

packages = ["cdms", "cdutil", "genutil", "vcs", 
            "pcmdi_metrics", "dv3d", "wk"]

env_name = get_env_name(env_prefix, py_ver)
ret_code, installed_pkgs_dict = get_packages_version(conda_path, env_name, packages)


for installed_pkg in installed_pkgs_dict.keys():

    if installed_pkg == 'cdms2':
        pkg = 'cdms'
    else:
        pkg = installed_pkg

    print("pkg: {p}".format(p=pkg))
    if pkg in packages:
        repo_dir = os.path.join(workdir, pkg)

        ret_code, last_commit_info = get_last_commit(repo_dir)
    
        installed_pkg_datetime = installed_pkgs_dict[installed_pkg]['datetime']
        pkg_last_commit_datetime = last_commit_info['datetime']
    
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


