
import sys
import os
import argparse

thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

import CondaSetup
import CDATSetup
from Const import *
from Util import *

parser = argparse.ArgumentParser(description="install from env file",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-v", "--env_prefix",
                    help="environment prefix - this can be 'cdat-3.0.beta2-nox' or 'cdat-3.0.beta2'")
parser.add_argument("-p", "--py_ver", nargs='?', default='py2',
                    help="python version, 'py2' or 'py3'")
args = parser.parse_args()

workdir = args.workdir
env_prefix = args.env_prefix
py_ver = args.py_ver

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
if status != SUCCESS:
    sys.exit(FAILURE)

if "cdat-v80" in env_prefix:
    # TEMPORARY till all cdat packages are conda_labeled with 'v3.0' for v3.0 release.
    #conda_label = 'nightly'
    conda_label = 'v80'
    env_setup = CDATSetup.EnvSetup(conda_path, workdir, env_prefix, py_ver, conda_label)
else:
    print("ERROR...incorrect env_prefix: {v}".format(v=env_prefix))
    sys.exit(FAILURE)

status = env_setup.install()
if status != SUCCESS:
    sys.exit(FAILURE)

status = env_setup.install_packages_for_tests()
if status != SUCCESS:
    sys.exit(FAILURE)

env_setup.conda_list()

sys.exit(status)

