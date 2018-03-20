
import sys
import os
import argparse

thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

import CondaSetup
import UVCDATSetup
from Const import *
from Util import *

parser = argparse.ArgumentParser(description="install nightly",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")
parser.add_argument("-p", "--py_ver",
                    help="python version, 'py2' or 'py3'")

args = parser.parse_args()
workdir = args.workdir
py_ver = args.py_ver

conda_setup = CondaSetup.CondaSetup(workdir, py_ver)
status, conda_path = conda_setup.install_miniconda()
if status != SUCCESS:
    sys.exit(FAILURE)

nightly_setup = UVCDATSetup.NightlySetup(conda_path, workdir, py_ver)
status = nightly_setup.install()
if status != SUCCESS:
    sys.exit(FAILURE)

nightly_setup.conda_list()

sys.exit(status)

    






