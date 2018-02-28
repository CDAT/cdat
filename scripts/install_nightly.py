
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

conda_setup = CondaSetup.CondaSetup(workdir)
status, conda_path = conda_setup.install_miniconda(workdir)
if status != SUCCESS:
    sys.exit(FAILURE)

nightly_setup = UVCDATSetup.NightlySetup(conda_path, workdir)

status = nightly_setup.install(py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)

status = nightly_setup.install_packages(py_ver)
sys.exit(status)

# install packages needed to run testsuites

#packages = "nose mesalib image-compare \\\"matplotlib<2.1\\\" numpy=1.13 vcs vcsaddons cdp mesalib"
        
#if branch == '2.12':
#    packages += 'flake8=3.3.0'
#else:
#    packages += 'flake8'
        
#status = nightly_setup.install_packages(py_ver, packages)
#if status != SUCCESS:
#   sys.exit(FAILURE)
    






