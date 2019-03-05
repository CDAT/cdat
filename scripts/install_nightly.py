import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Const import *
from Util import *
from CondaUtils import *
from CDATSetupUtils import *

valid_py_vers = PYTHON_VERSIONS

parser = argparse.ArgumentParser(description="install nightly",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-p", "--py_ver", choices=valid_py_vers,
                    help="python version, 'py2.7' or 'py3.6' or 'py3.7'")

args = parser.parse_args()
workdir = args.workdir
py_ver = args.py_ver

status, conda_path = install_miniconda(workdir, py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)

print("xxx conda_path: {p}".format(p=conda_path))

status, env_name = install_nightly(workdir, conda_path, 'nightly', py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)

status = conda_list(conda_path, env_name)

sys.exit(status)

    






