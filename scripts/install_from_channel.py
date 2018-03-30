
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

valid_py_vers = ["py2", "py3"]
valid_env_prefixes = ["cdat_latest_channel", "cdat_v80_channel"]
parser = argparse.ArgumentParser(description="install CDAT from conda channel",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir", required=True,
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-v", "--env_prefix", required=True, choices=valid_env_prefixes,
                    help="environment prefix string", default='cdat')
parser.add_argument("-p", "--py_ver", nargs='?', default='py2',
                    help="python version", choices=valid_py_vers)
parser.add_argument("-c", "--conda_label", default='latest',
                    help="conda label, can be 'v80' or 'latest'")

args = parser.parse_args()

workdir = args.workdir
env_prefix = args.env_prefix
py_ver = args.py_ver
conda_label = args.conda_label

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
if status != SUCCESS:
    sys.exit(FAILURE)

env_setup = CDATSetup.EnvFromChannelSetup(conda_path, workdir, env_prefix, py_ver, conda_label)

status = env_setup.install()
if status != SUCCESS:
    sys.exit(FAILURE)

env_setup.conda_list()

sys.exit(status)

