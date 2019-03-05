
import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Const import *
from CondaUtils import *
from CDATSetupUtils import *

valid_py_vers = PYTHON_VERSIONS
valid_env_prefixes = ["cdat_{l}_latest_channel".format(l=CONDA_LABEL)]
parser = argparse.ArgumentParser(description="install CDAT from conda channel",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir", required=True,
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-v", "--env_prefix", required=True, choices=valid_env_prefixes,
                    help="environment prefix string", default='cdat')
parser.add_argument("-p", "--py_ver", nargs='?', default='py2.7',
                    help="python version", choices=valid_py_vers)
parser.add_argument("-c", "--conda_label", default='latest',
                    help="conda label")

args = parser.parse_args()

workdir = args.workdir
env_prefix = args.env_prefix
py_ver = args.py_ver
conda_label = args.conda_label

status, conda_path = install_miniconda(workdir, py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)

status, env_name = install_from_channel(workdir, conda_path, env_prefix, py_ver, conda_label)
if status != SUCCESS:
    sys.exit(FAILURE)

status = install_packages_for_tests(conda_path, env_name)
if status != SUCCESS:
    sys.exit(FAILURE)

status = conda_list(conda_path, env_name)
if status != SUCCESS:
    sys.exit(FAILURE)

status = conda_env_export(workdir, conda_path, env_name)
sys.exit(status)


