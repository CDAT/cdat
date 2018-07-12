
import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Util import *
import CDATSetup
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
parser.add_argument("-c", "--conda_label", default='latest',
                    help="conda label, can be 'v80' or 'latest'")
args = parser.parse_args()


# This should be consistent with what is set in install_miniconda()
conda_path = os.path.join(args.workdir, 'miniconda', 'bin')

workdir = args.workdir
py_ver = args.py_ver
branch = args.branch
label = args.label
env_prefix = args.env_prefix
conda_label = args.conda_label

if env_prefix == 'nightly':
    cdat_setup = CDATSetup.NightlySetup(conda_path, workdir, py_ver)
elif "cdat-v80" in env_prefix:
    cdat_setup = CDATSetup.EnvSetup(conda_path, workdir, env_prefix, py_ver, label)
elif "channel" in env_prefix:
    cdat_setup = CDATSetup.EnvFromChannelSetup(conda_path, workdir, env_prefix, py_ver, conda_label)
else:
    print("ERROR...incorrect env_prefix: {v}".format(v=env_prefix))
    sys.exit(FAILURE)

ts = args.testsuite

# default run_tests.py invocation command
run_tests_cmd = 'python run_tests.py -s -v2'

if env_prefix == 'nightly':
    baseline_opt = "--checkout-baseline"
else:
    baseline_opt = "-g"

if ts == 'vcs':
    test_setup = TestSetup.VcsTestSetup(cdat_setup, ts, py_ver, branch, label)
elif ts == 'cdms':
    test_setup = TestSetup.CdmsTestSetup(cdat_setup, ts, py_ver, branch, label)
else:
    test_setup = TestSetup.TestSetup(cdat_setup, ts, py_ver, branch, label)

if ts == 'cdms':
    cmds_list = ['python run_tests.py -s -v2 -p -H']
elif ts == 'dv3d':
        cmds_list = ["python run_tests.py -v2 -n2 {cb} -H".format(cb=baseline_opt)]
elif ts == 'vcs':
    if py_ver == 'py2':
        cmds_list = ["python run_tests.py -v2 -n 2 --no-vtk-ui {cb} -H".format(cb=baseline_opt)]
    else:
        cmds_list = ["python run_tests.py -v2 -n 2 --no-vtk-ui {cb} -H".format(cb=baseline_opt),
                     'cd docs', 'make doctest']
elif ts == 'genutil' or ts == 'cdutil' or ts == 'pcmdi_metrics': 
    cmds_list = ['python run_tests.py -v2 -H']
elif ts == 'vcsaddons' or ts == 'thermo' or ts == 'wk':
    cmds_list = ['python run_tests.py -v2 -n 2 -H']

status = test_setup.run_tests(cdat_setup, py_ver, cmds_list)
sys.exit(status)




