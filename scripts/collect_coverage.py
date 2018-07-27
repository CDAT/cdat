import sys
import os
import argparse

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Util import *

desc = "collect coverage information (htmls) for cdat packages"
all_test_suites = "cdms genutil cdutil vcs vcsaddons pcmdi_metrics dv3d thermo wk"
all_pkgs = "cdms2 genutil cdutil vcs vcsaddons pcmdi_metrics dv3d thermo wk"

#
# This script collects coverage information for the specified packages
# The .coverage under <workdir>/<ts> from running test suites are all 
# combined, and the script generates html for coverage info for each
# package.
#

parser = argparse.ArgumentParser(description=desc,
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('-w', '--workdir',
                    help="working directory -- where miniconda was installed")
parser.add_argument('-p', '--py_ver',
                    help="python version to run the testsuite with, can be 'py2' or 'py3'")
parser.add_argument('-v', '--env_prefix', nargs='?', default='nightly',
                    help="cdat env name to run the testsuite in.")
parser.add_argument('-t', '--test_suites', default=all_test_suites,
                    help="test suite names that ran that generated coverage info")
parser.add_argument('-P', '--packages', default=all_pkgs,
                    help="packages that we want to get coverage info on")

args = parser.parse_args()

workdir = args.workdir
py_ver = args.py_ver
ts_list = args.test_suites.split()
env_prefix = args.env_prefix
packages = args.packages.split()

# dir is where test suites repo resides and where run_tests.py were ran.

env_name = "{e}_{p}".format(e=env_prefix, p=py_ver)
env_path = os.path.join(workdir, 'miniconda', 'envs', env_name)
coverage_cmd = os.path.join(env_path, 'bin', 'coverage')

def combine_coverage(dir, ts_list):

    coverage_dir = os.path.join(dir, 'all_coverage')
    os.mkdir(coverage_dir)
    counter = 1
    for ts in ts_list:
        ts_dir = os.path.join(dir, ts)
        if os.path.isdir(ts_dir):
            source = os.path.join(ts_dir, '.coverage')
            if os.path.exists(source):
                dest = os.path.join(coverage_dir, 
                                    ".coverage.{n}".format(n=counter))
                cmd = "cp {s} {d}".format(s=source, d=dest)

                ret_code = run_cmd(cmd, True, False, True)
                if ret_code != SUCCESS:
                    return ret_code
                counter += 1

            else:
                print("WARNING: {s} file does not exist".format(s=source))
        print("WARNING: perhaps {ts} tests did not get run".format(ts=ts))

    cmd = "{c} combine".format(c=coverage_cmd)
    ret_code = run_cmd(cmd, True, False, True, coverage_dir)
    return ret_code

def get_package_files(pkg_dir):
    pkg_files = []
    for file in os.listdir(pkg_dir):
        if file.endswith(".py"):
            file_full_path = os.path.join(pkg_dir, file)
            pkg_files.append(file_full_path)
    return pkg_files

def get_coverage_html(dir, pkgs_list):
    ret_code = SUCCESS
    coverage_dir = os.path.join(dir, 'all_coverage')
    coverage_html_dir = os.path.join(dir, 'coverage_htmls')
    os.mkdir(coverage_html_dir)

    if py_ver == 'py2':
        py_ver_str = "python2.7"
    else:
        py_ver_str = "python3.6"

    pkgs_dir = os.path.join(env_path, 'lib', py_ver_str, 'site-packages')

    for pkg in pkgs_list:
        pkg_html_dir = os.path.join(coverage_html_dir,
                                    "{pkg}_coverage_html".format(pkg=pkg))
        pkg_dir = os.path.join(pkgs_dir, pkg)
        if not os.path.isdir(pkg_dir):
            print("WARNING...{d} does not exist".format(d=pkg_dir))
            continue
        pkg_files_list = get_package_files(pkg_dir)
        pkg_files = " ".join(pkg_files_list)
        cmd = "{c} html -d {html_dir} -i {pkg_files}".format(c=coverage_cmd,
                                                             html_dir=pkg_html_dir,
                                                             pkg_files=pkg_files)
        
        ret_code = run_cmd(cmd, True, False, True, coverage_dir)
        if ret_code != SUCCESS:
            return ret_code
        
    return ret_code

status = combine_coverage(workdir, ts_list)
if status != SUCCESS:
    sys.exit(status)

status = get_coverage_html(workdir, packages)
sys.exit(status)
    
                     
    
