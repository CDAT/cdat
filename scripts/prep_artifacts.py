import sys
import os
import argparse
import shutil

this_dir = os.path.abspath(os.path.dirname(__file__))
modules_dir = os.path.join(this_dir, '..', 'modules')
sys.path.append(modules_dir)

from Util import *

all_test_suites = "cdms genutil cdutil vcs vcsaddons pcmdi_metrics dv3d thermo wk"

parser = argparse.ArgumentParser(description="preparing artifacts for a circle ci job",
                                 formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('-w', '--workdir',
                    help="working directory -- where miniconda was installed")
parser.add_argument('-p', '--py_ver',
                    help="python version to run the testsuite with, can be 'py2' or 'py3'")
parser.add_argument('-v', '--env_prefix', nargs='?', default='nightly',
                    help="cdat env name to run the testsuite in.")
parser.add_argument('-j', '--circle_ci_job',
                    help="Circle CI job name that we are collecting artifacts for")
args = parser.parse_args()

workdir = args.workdir
py_ver = args.py_ver
env_prefix = args.env_prefix
ci_job = args.circle_ci_job

# test suites that generate htmls
html_ts_list = ['genutil', 'cdutil', 'vcs', 'vcsaddons', 'dv3d', 'thermo',
                'wk', 'cdms', 'pcmdi_metrics']
# test suites that generate pngs
png_ts_list = ['vcs', 'vcsaddons', 'pcmdi_metrics', 'thermo', 'wk']

#
# This scripts is only to be called from ./circleci/config.yml.
# It moves test suites' tests_html and tests_png to a destination directory
# which is <workdir>/<ci_job>
#
def get_artifacts(workdir, ci_job, html_or_png, ts_list):
    if html_or_png == 'html':
        artifact = "tests_html"
    else:
        artifact = "tests_png"

    print("DEBUG DEBUG xxx get_artifacts...")
    for ts in ts_list:
        source = os.path.join(workdir, ts, artifact)
        print("DEBUG DEBUG xxx checking if source {s} exists".format(s=source))
        if os.path.isdir(source):
            dest = os.path.join(ci_job, "{ts}-{a}".format(ts=ts,
                                                          a=artifact))
            try:
                print("DEBUG DEBUG xxx copying from {s} to {d}".format(s=source,
                                                                       d=dest))
                shutil.copytree(source, dest)
            except Error:
                print ("Fail in copying {s} to {d}".format(s=source, d=dest))
                return FAILURE
    return SUCCESS

def get_coverage_htmls(workdir, ci_job):
    """
    move <workdir>/coverage_htmls to coverage_htmls (under current dir)
    """
    source = os.path.join(workdir, 'coverage_htmls')
    dest = os.path.join(ci_job, 'coverage_htmls')

    print("DEBUG xxx renaming {s} to {d}".format(s=source, d=dest))
    try:
        os.rename(source, dest)
    except OSError:
        print("FAIL in copying {s} to {d}".format(s=source,
                                                  d=dest))
        return FAILURE
    return SUCCESS

status1 = get_artifacts(workdir, ci_job, 'html', html_ts_list)

status2 = get_artifacts(workdir, ci_job, 'png', png_ts_list)

status3 = get_coverage_htmls(workdir, ci_job)

sys.exit(status1 | status2 | status3)

            
                                         
