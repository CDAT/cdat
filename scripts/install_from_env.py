
import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

import CondaSetup
import CDATSetup
from Const import *
from Util import *

valid_env_prefixes = ["cdat-v80-nox", "cdat-v80"]
release_conda_label = "v80"
valid_py_vers = ["py2", "py3"]

parser = argparse.ArgumentParser(description="install CDAT from env file",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir", required=True,
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-v", "--env_prefix", required=True,
                    help="environment prefix string", choices=valid_env_prefixes)
parser.add_argument("-p", "--py_ver", nargs='?', default='py2',
                    help="python version", choices=valid_py_vers)
args = parser.parse_args()

workdir = args.workdir
env_prefix = args.env_prefix
py_ver = args.py_ver

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
if status != SUCCESS:
    sys.exit(FAILURE)

conda_label = release_conda_label
env_setup = CDATSetup.EnvSetup(conda_path, workdir, env_prefix, py_ver, conda_label)

status = env_setup.install()
if status != SUCCESS:
    sys.exit(FAILURE)

status = env_setup.install_packages_for_tests()
if status != SUCCESS:
    sys.exit(FAILURE)

env_setup.conda_list()

sys.exit(status)

