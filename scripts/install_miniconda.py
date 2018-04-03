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

parser = argparse.ArgumentParser(description="install miniconda",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-p", "--py_ver", choices=valid_py_vers,
                    help="python version, 'py2' or 'py3'")

args = parser.parse_args()
workdir = args.workdir
py_ver = args.py_ver

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
