
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
parser.add_argument('-v', '--version', nargs='?', default='master',
                    help="version of testsuite: 'master' or '2.12' or 'cdat-3.0.beta'")
parser.add_argument('-t', '--testsuite',
                    help='testsuite to run')

args = parser.parse_args()


# This should be consistent with what is set in install_miniconda()
conda_path = args.workdir + '/miniconda/bin'

workdir = args.workdir
py_ver = args.py_ver
version = args.version

#
# label is the git label to checkout testsuite
#
if version == 'master' or version == '2.12':
    label = version
elif "cdat-3.0" in version:
    # for cdat 3.0 TEMPORARY
    label = 'nightly'

if version == 'master':
    uvcdat_setup = UVCDATSetup.NightlySetup(conda_path, workdir)
elif "cdat-3.0" in version:
    uvcdat_setup = UVCDATSetup.Env30Setup(conda_path, workdir, version, label)


if uvcdat_setup.get_env_name(py_ver) is None:
    print("Conda environment for python version {v} does not exist.".format(v=py_ver))
    sys.exit(FAILURE)

# TEMPORARY
#if version == 'cdat-3.0.beta':
#    # clone the nightly tests
#    version = 'master'

    
ts = args.testsuite
# default run_tests.py invocation command
run_tests_cmd = 'python run_tests.py -s -v2'

if ts == 'vcs':
    test_setup = TestSetup.VcsTestSetup(uvcdat_setup, ts, py_ver, version)
else:
    test_setup = TestSetup.TestSetup(uvcdat_setup, ts, py_ver, version)

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




