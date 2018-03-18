
import sys
import os
import argparse

thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

from Util import *
import UVCDATSetup
import TestSetup

parser = argparse.ArgumentParser(description="run cdms tests",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('-w', '--workdir',
                    help="working directory -- where miniconda was installed")
parser.add_argument('-p', '--py_ver',
                    help="python version to run the testsuite with, can be 'py2' or 'py3'")
parser.add_argument('-b', '--branch', nargs='?', default='master',
                    help="git branch to check out testsuite: 'master' or other git branch")
parser.add_argument('-l', '--label', nargs='?', default='master',
                    help="git label: 'master' or other git label, like 'v3.0'")
parser.add_argument('-v', '--env_prefix', nargs='?', default='nightly',
                    help="cdat env name to run the testsuite in.")
parser.add_argument('-t', '--testsuite',
                    help='testsuite to run')

args = parser.parse_args()


# This should be consistent with what is set in install_miniconda()
conda_path = os.path.join(args.workdir, 'miniconda', 'bin')

workdir = args.workdir
py_ver = args.py_ver
branch = args.branch
label = args.label
env_prefix = args.env_prefix

if env_prefix == 'nightly':
    uvcdat_setup = UVCDATSetup.NightlySetup(conda_path, workdir)
elif "cdat-3.0" in env_prefix:
    uvcdat_setup = UVCDATSetup.Env30Setup(conda_path, workdir, env_prefix, label)


if uvcdat_setup.get_env_name(py_ver) is None:
    print("Conda environment for python version {v} does not exist.".format(v=py_ver))
    sys.exit(FAILURE)

ts = args.testsuite
# default run_tests.py invocation command
run_tests_cmd = 'python run_tests.py -s -v2'

if ts == 'vcs':
    test_setup = TestSetup.VcsTestSetup(uvcdat_setup, ts, py_ver, branch, label)
else:
    test_setup = TestSetup.TestSetup(uvcdat_setup, ts, py_ver, branch, label)

if ts == 'cdms':
    cmds_list = ['python run_tests.py -s -v2 -p -H']
elif ts == 'dv3d':
    cmds_list = ['python run_tests.py -v2 -n2 -g -H']
elif ts == 'vcs':
    if py_ver == 'py2':
        cmds_list = ['python run_tests.py -v2 -n 2 --no-vtk-ui -g -H']
    else:
        cmds_list = ['python run_tests.py -v2 -n 2 --no-vtk-ui -g -H', 'cd docs', 'make doctest']
elif ts == 'genutil' or ts == 'cdutil' or ts == 'pcmdi_metrics': 
    cmds_list = ['python run_tests.py -v2 -H']
elif ts == 'vcsaddons' or ts == 'thermo' or ts == 'wk':
    cmds_list = ['python run_tests.py -v2 -n 2 -H']

status = test_setup.run_tests(uvcdat_setup, py_ver, cmds_list)
sys.exit(status)




