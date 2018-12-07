import sys
import os
import re
import shutil
from datetime import datetime
from Util import *

class TestSetup(object):

    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master', label='master'):
        """
        clone the specified <repo_name>
        install packages needed to run tests specified by <repo_name>
        in the environment specified in <uvcdat_setup>

        saves the following info:
           self.branch - can be 'master' other git branch name
           self.label - git label of the repo
           self.repo_name - testsuite repo name
           self.repo_dir  - testsuite repo directory
           self.conda_path - conda path
        """

        self.branch = branch
        workdir = uvcdat_setup.workdir
        conda_path = uvcdat_setup.conda_path
        self.conda_path = conda_path
        
        ret_code, repo_dir = git_clone_repo(workdir, repo_name, branch, label)
        if ret_code != SUCCESS:
            raise Exception('git_clone_repo() failed')

        self.repo_name = repo_name
        self.repo_dir = repo_dir
        

    def get_uvcdat_testdata(self, uvcdat_setup, for_repo_dir, branch, label):
        """
        get uvcdat-testdata code that has same tag as <for_repo_dir>
        For example, if <for_repo_dir> is a vcs repo and is tagged '2.12',
        then this function will check out uvcdat-testdata with same tag.
        It clones uvcdat-testdata into <for_repo_dir>/uvcdat-testdata 
        directory
        """

        workdir = uvcdat_setup.workdir
        repo_dir = os.path.join(for_repo_dir, 'uvcdat-testdata')
        ret_code, repo_dir = git_clone_repo(workdir, 'uvcdat-testdata', branch, label, repo_dir)

        cmd = "git status"
        run_cmd(cmd, True, False, True, repo_dir)

    def run_tests(self, uvcdat_setup, py_version, run_cmds_list):

        env = uvcdat_setup.get_env_name()

        conda_path = uvcdat_setup.conda_path
        workdir = uvcdat_setup.workdir

        cmds_list = []
        cmd = 'export UVCDAT_ANONYMOUS_LOG=False'
        cmds_list.append(cmd)

        cmd = 'cd ' + self.repo_dir 
        cmds_list.append(cmd)

        for cmd in run_cmds_list:
            cmds_list.append(cmd)
        ret_code = run_in_conda_env(conda_path, env, cmds_list)
        return(ret_code)

    def get_last_commit(self):
        cmd = "git log -n1 --pretty=format:%ad"
        ret_code, cmd_output = run_cmd_capture_output(cmd, True, False, True, self.repo_dir)
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

class TestSetupWithSampleData(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master', label='master'):
        super(TestSetupWithSampleData, self).__init__(uvcdat_setup, repo_name, py_version, branch, label)

        # get uvcdat-testdata
        if label != 'master':
            for_repo_dir = os.path.join(uvcdat_setup.workdir, repo_name)
            super(TestSetupWithSampleData, self).get_uvcdat_testdata(uvcdat_setup, for_repo_dir, branch, label)       
    
