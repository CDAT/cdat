
import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Const import *
#from Util import *
from CondaUtils import *
from CDATSetupUtils import *


conda_label = CONDA_LABEL
valid_py_vers = PYTHON_VERSIONS
valid_env_prefixes = ["cdat-{l}-nox".format(l=conda_label), 
                      "cdat-{l}".format(l=conda_label)]

parser = argparse.ArgumentParser(description="install CDAT from env file",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir", required=True,
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-v", "--env_prefix", required=True,
                    help="environment prefix string", choices=valid_env_prefixes)
parser.add_argument("-p", "--py_ver", nargs='?', default='py2.7',
                    help="python version", choices=valid_py_vers)
args = parser.parse_args()

workdir = args.workdir
env_prefix = args.env_prefix
py_ver = args.py_ver

status, conda_path = install_miniconda(workdir, py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)

status, env_name = install_from_env_file(workdir, conda_path, env_prefix, py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)

status = install_packages_for_tests(conda_path, env_name)
if status != SUCCESS:
    sys.exit(FAILURE)

status = conda_list(conda_path, env_name)

sys.exit(status)

