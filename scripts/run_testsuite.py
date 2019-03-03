
import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Util import *
from CondaUtils import *
from CDATSetupUtils import *
from TestSetupUtils import *

parser = argparse.ArgumentParser(description="run cdms tests",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)

parser.add_argument('-w', '--workdir',
                    help="working directory -- where miniconda was installed")
parser.add_argument('-p', '--py_ver',
                    help="python version to run the testsuite with, can be 'py2.7' or 'py3.6' or 'py3.7'")
parser.add_argument('-b', '--branch', nargs='?', default='master',
                    help="git branch to check out testsuite: 'master' or other git branch")
parser.add_argument('-l', '--label', nargs='?', default='master',
                    help="git label: 'master' or other git label, like 'v3.0'")
parser.add_argument('-v', '--env_prefix', nargs='?', default='nightly',
                    help="cdat env name to run the testsuite in.")
parser.add_argument('-t', '--testsuite',
                    help='testsuite to run')
parser.add_argument("-c", "--coverage", default=False, action="store_true",
                    help="run testsuite with code coverage")
args = parser.parse_args()


# This should be consistent with what is set in install_miniconda()


workdir = args.workdir
py_ver = args.py_ver
branch = args.branch
label = args.label
env_prefix = args.env_prefix
coverage = args.coverage
ts = args.testsuite

conda_dir = get_conda_dir(workdir, py_ver)
conda_path = os.path.join(conda_dir, 'bin')

# default run_tests.py invocation command
# NOTE that if launching run_tests.py with -s option, the html and png artifacts
# will not be stored in .circleci nightly runs.

if coverage:
    run_tests_cmd = 'python run_tests.py -v2 -H -c'
else:
    run_tests_cmd = 'python run_tests.py -v2 -H'

if env_prefix == 'nightly':
    baseline_opt = "--checkout-baseline"
else:
    baseline_opt = "-g"

if ts == 'vcs' or ts == 'dv3d':
    sample_data = True
else:
    sample_data = False

status = setup_tests(workdir, conda_path, ts, py_ver, branch, label, sample_data)

if ts == 'cdms':
    cmds_list = ["{cmd} --subdir --package".format(cmd=run_tests_cmd)]
elif ts == 'dv3d':
    if env_prefix == 'nightly':
        cmds_list = ["{cmd} -n2 {cb}".format(cmd=run_tests_cmd,
                                             cb=baseline_opt)]
    else:
        cmds_list = ["{cmd} -n2".format(cmd=run_tests_cmd)]
elif ts == 'vcs':
    if env_prefix == 'nightly':
        cmds_list = ["{cmd} -n 2 --no-vtk-ui {cb}".format(cmd=run_tests_cmd,
                                                          cb=baseline_opt)]
    else:
        cmds_list = ["{cmd} -n 2 --no-vtk-ui".format(cmd=run_tests_cmd)]

    if py_ver == 'py3.6' or py_ver == 'py3.7':
        cmds_list.append('cd docs')
        cmds_list.append('make doctest')

elif ts == 'genutil' or ts == 'cdutil' or ts == 'pcmdi_metrics': 
    cmds_list = [run_tests_cmd]
elif ts == 'vcsaddons' or ts == 'thermo' or ts == 'wk':
    cmds_list = ["{cmd} -n 2".format(cmd=run_tests_cmd)]

env_name = get_env_name(env_prefix, py_ver)
status = run_tests(workdir, conda_path, env_name, ts, cmds_list)
sys.exit(status)




