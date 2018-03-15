
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
                    help="git branch of testsuite: 'master' or '2.12' or 'cdat-3.0.beta'")
parser.add_argument('-t', '--testsuite',
                    help='testsuite to run')

args = parser.parse_args()


# This should be consistent with what is set in install_miniconda()
conda_path = args.workdir + '/miniconda/bin'

workdir = args.workdir
py_ver = args.py_ver
branch = args.branch

if branch == 'master' or branch == '2.12':
    label = branch
else:
    label = 'v30beta'

if branch == 'master':
    uvcdat_setup = UVCDATSetup.NightlySetup(conda_path, workdir)
else:

    uvcdat_setup = UVCDATSetup.EnvSetup(conda_path, workdir, branch, label)


if uvcdat_setup.get_env_name(py_ver) is None:
    print("Conda environment for python version {v} does not exist.".format(v=py_ver))
    sys.exit(FAILURE)

# TEMPORARY
#if branch == 'cdat-3.0.beta':
#    # clone the nightly tests
#    branch = 'master'

    
ts = args.testsuite
# default run_tests.py invocation command
run_tests_cmd = 'python run_tests.py -s -v2'

if ts == 'vcs':
    test_setup = TestSetup.VcsTestSetup(uvcdat_setup, ts, py_ver, branch)
else:
    test_setup = TestSetup.TestSetup(uvcdat_setup, ts, py_ver, branch)

if ts == 'cdms':
    cmds_list = ['python run_tests.py -s -v2 -p -H']
elif ts == 'dv3d':
    cmds_list = ['python run_tests.py -v2 -n2 -g']
elif ts == 'vcs':
    cmds_list = ['python run_tests.py -v2 -n 2 --no-vtk-ui -g', 'cd docs', 'make doctest']    
elif ts == 'genutil' or ts == 'cdutil' or ts == 'pcmdi_metrics': 
    cmds_list = ['python run_tests.py -v2']
elif ts == 'vcsaddons' or ts == 'thermo' or ts == 'wk':
    cmds_list = ['python run_tests.py -v2 -n 2']

status = test_setup.run_tests(uvcdat_setup, py_ver, cmds_list)
sys.exit(status)




