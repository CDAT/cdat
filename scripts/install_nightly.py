
import sys
import os
import argparse

thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

import NightlySetup
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
nightly_setup = NightlySetup.NightlySetup(workdir)

status = nightly_setup.install_miniconda(workdir)
if status != SUCCESS:
    sys.exit(FAILURE)

conda_path = nightly_setup.conda_path
status = nightly_setup.install_nightly(args.py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)



