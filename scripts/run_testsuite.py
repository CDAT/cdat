
import sys
import os
import argparse

thisDir = os.path.abspath(os.path.dirname(__file__))
sys.path.append(thisDir + '/../modules/')

from Util import *
import NightlySetup
import TestSetup

parser = argparse.ArgumentParser(description="run cdms tests",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('-w', '--workdir', 
                    help="working directory -- where miniconda was installed")
parser.add_argument('-p', '--py_ver',
                    help="python version to run the testsuite with, can be 'py2' or 'py3'")
parser.add_argument('-t', '--testsuite',
                    help='testsuite to run')


args = parser.parse_args()

# This should be consistent with what is set in install_miniconda()
conda_path = args.workdir + '/miniconda/bin'

workdir = args.workdir
py_ver = args.py_ver

nightly_setup = NightlySetup.NightlySetup(workdir)

if py_ver == 'py2' and nightly_setup.py2_env == None:
    print("Error....nightly environment is not created")
    sys.exit(FAILURE)

if py_ver == 'py3' and nightly_setup.py3_env == None:
    print("Error....nightly environment is not created")
    sys.exit(FAILURE)


ts = args.testsuite
# default run_tests.py invocation command
run_tests_cmd = 'python run_tests.py -s -v2'

if ts == 'cdms':
    cdms_test_setup = TestSetup.CdmsTestSetup(nightly_setup, ts, py_ver)
    run_tests_cmd = 'python run_tests.py -s -v2 -p'
    status = cdms_test_setup.run_tests(nightly_setup, py_ver, run_tests_cmd)
    
elif ts == 'vcs':
    test_setup = TestSetup.VcsTestSetup(nightly_setup, ts, py_ver)
    run_tests_cmd = 'python run_tests.py -v2 -n 2 -g --no-vtk-ui'
    status = test_setup.run_tests(nightly_setup, py_ver, run_tests_cmd)
    
elif ts == 'genutil':
    test_setup = TestSetup.TestSetup(nightly_setup, ts, py_ver)
    run_tests_cmd = 'python run_tests.py -v2'
    status = test_setup.run_tests(nightly_setup, py_ver, run_tests_cmd)

elif ts == 'cdutil':
    test_setup = TestSetup.TestSetup(nightly_setup, ts, py_ver)
    run_tests_cmd = 'python run_tests.py -v2'
    status = test_setup.run_tests(nightly_setup, py_ver, run_tests_cmd)

elif ts == 'vcsaddons':
    test_setup = TestSetup.TestSetup(nightly_setup, ts, py_ver)
    run_tests_cmd = 'python run_tests.py -v2 -n 2'
    status = test_setup.run_tests(nightly_setup, py_ver, run_tests_cmd)

sys.exit(status)




