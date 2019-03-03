import sys
import os
import re
import shutil
from datetime import datetime
from Util import *

def get_uvcdat_testdata(workdir, repo_name, branch, label):
    """                                                                                                                    
    get uvcdat-testdata code that has same tag as <for_repo_dir>                                                           
    For example, if <for_repo_dir> is a vcs repo and is tagged '2.12',                                                     
    then this function will check out uvcdat-testdata with same tag.                                                       
    It clones uvcdat-testdata into <for_repo_dir>/uvcdat-testdata                                                          
    directory                                                                                                              
    """
    print("DEBUG...get_uvcdat_testdata...")
    for_repo_dir = os.path.join(workdir, repo_name)
    repo_dir = os.path.join(for_repo_dir, 'uvcdat-testdata')
    ret_code, repo_dir = git_clone_repo(workdir, 'uvcdat-testdata', branch, label, repo_dir)

    cmd = "git status"
    ret_code = run_cmd(cmd, True, False, True, repo_dir)
    return ret_code

def run_tests(workdir, conda_path, env_name, repo_name, run_cmds_list):

    cmds_list = []
    cmd = 'export UVCDAT_ANONYMOUS_LOG=False'
    cmds_list.append(cmd)

    repo_dir = os.path.join(workdir, repo_name)
    cmd = "cd {d}".format(d=repo_dir)
    cmds_list.append(cmd)

    for cmd in run_cmds_list:
        cmds_list.append(cmd)
    ret_code = run_in_conda_env(conda_path, env_name, cmds_list)
    return(ret_code)

def get_last_commit(repo_dir):
    print("repo_dir: {d}".format(d=repo_dir))
    cmd = "git log -n1 --pretty=format:%ad"
    ret_code, cmd_output = run_cmd_capture_output(cmd, True, False, True, repo_dir)
    # parse the cmd_output - 'Fri Mar 2 16:15:37 2018 -0800'                                                               
    match_obj = re.match(r'(\S+\s+\S+\s+\d+\s+\d+:\d+:\d+\s+\d+)\s+-\d+', cmd_output[0])
    last_commit_info = None
    if match_obj:
        last_commit_date = match_obj.group(1)
        # just get the month/date/year                                                                                     
        last_commit_datetime = datetime.strptime(last_commit_date, "%a %b %d %H:%M:%S %Y")
        d = last_commit_datetime.strftime("%m/%d/%Y")
        last_commit_info = {}
        last_commit_info['datetime'] = last_commit_datetime
        last_commit_info['date_str'] = d
    return(ret_code, last_commit_info)

def setup_tests(workdir, conda_path, repo_name, py_ver, branch='master', label='master', sample_data=False):

    ret_code, repo_dir = git_clone_repo(workdir, repo_name, branch, label)
    if ret_code != SUCCESS:
        print("FAIL in git_clone_repo()...")
        return ret_code

    if sample_data and label != 'master':
        get_uvcdat_testdata(workdir, repo_name, branch, label)

    return ret_code
