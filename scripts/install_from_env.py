
import sys
import os
import argparse

thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

import CondaSetup
import UVCDATSetup
from Const import *
from Util import *

parser = argparse.ArgumentParser(description="install from env file",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument("-w", "--workdir",
                    help="working directory -- miniconda will be installed here")

parser.add_argument("-v", "--env_version",
                    help="environment version, '2.12', '2.12-nox', '2.10', '2.10-nox', '2.8', '2.8-nox', '2.6'")

parser.add_argument("-p", "--py_ver", nargs='?', default='py2',
                    help="python version, 'py2' or 'py3' --- but only 'py2' for 2.12 or prior envs.")

args = parser.parse_args()

workdir = args.workdir
env_version = args.env_version

conda_setup = CondaSetup.CondaSetup(workdir)
status, conda_path = conda_setup.install_miniconda(workdir)
if status != SUCCESS:
    sys.exit(FAILURE)

if env_version == '2.12':
    env_setup = UVCDATSetup.EnvSetup(conda_path, workdir, env_version)
else:
    env_setup = UVCDATSetup.Beta3_0EnvSetup(conda_path, workdir, env_version)
status = env_setup.install(args.py_ver)
if status != SUCCESS:
    sys.exit(FAILURE)



