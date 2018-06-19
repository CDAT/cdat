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
        
        # clone repo

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
        # get the tag 
        #ret_code, tag = get_tag_name_of_repo(for_repo_dir)
        #if ret_code != SUCCESS:
        #    raise Exception('FAILED...in getting branch name of: ' + for_repo_dir)

        workdir = uvcdat_setup.workdir
        repo_dir = "{dir}/uvcdat-testdata".format(dir=for_repo_dir)
        
        ret_code, repo_dir = git_clone_repo(workdir, 'uvcdat-testdata', branch, label, repo_dir)

        #cmd = 'git pull'
        #ret_code = run_cmd(cmd, True, False, True, repo_dir)
        #if ret_code != SUCCESS:
        #    raise Exception('FAIL...' + cmd)

        #if branch != 'master' and branch != 'cdat-3.0.beta':
        #    cmd = 'cd ' + repo_dir + '; git checkout ' + tag
        #    print("CMD: " + cmd)
            # may need to revisit -- I have to use os.system() here.
            # if I use run_cmd() specifying cwd, it does not work in circleci.
        #    ret_code = os.system(cmd)
        #    if ret_code != SUCCESS:
        #        raise Exception('FAILED...' + cmd)

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

class VcsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master', label='master'):
        super(VcsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch, label)

        # get uvcdat-testdata
        if label != 'master':
            for_repo_dir = os.path.join(uvcdat_setup.workdir, branch, repo_name)
            super(VcsTestSetup, self).get_uvcdat_testdata(uvcdat_setup, for_repo_dir, branch, label)       

class CdmsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master', label='master'):
        super(CdmsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch, label)

        repo_dir = self.repo_dir
        user_home = os.environ['HOME']
        cmd = "mkdir {d}/.esg".format(d=user_home)
        run_cmd(cmd, True, False, True)

        esgf_pwd = os.environ['ESGF_PWD']
        esgf_user = os.environ['ESGF_USER']
        
        # install MyProxyClient
        conda_path = self.conda_path
        env = uvcdat_setup.get_env_name()
        cmds_list = ["pip install MyProxyClient"]
        ret_code = run_in_conda_env(conda_path, env, cmds_list)
        myproxyclient_cmd = "myproxyclient logon -s esgf-node.llnl.gov -p 7512 -t 12 -S -b"
        cmds_list = ["echo {p} | {cmd} -l {u} -o {w}/.esg/esgf.cert".format(p=esgf_pwd,
                                                                            cmd=myproxyclient_cmd,
                                                                            u=esgf_user,
                                                                            w=user_home)]
        ret_code = run_in_conda_env(conda_path, env, cmds_list, False)
        dds_url = "https://aims3.llnl.gov/thredds/dodsC/cmip5_css02_data/cmip5/output1/CMCC/CMCC-CM/decadal2005/mon/atmos/Amon/r1i1p1/cct/1/cct_Amon_CMCC-CM_decadal2005_r1i1p1_202601-203512.nc.dds"
        #dds_url = "https://esgf-node.cmcc.it/thredds/dodsC/esg_dataroot/cmip5/output1/CMCC/CMCC-CM/decadal2005/mon/atmos/Amon/r1i1p1/v20170725/cct/cct_Amon_CMCC-CM_decadal2005_r1i1p1_202601-203512.nc.dds"

        #if py_version == 'py2':
        #    cacert = "--cacert {w}/miniconda/lib/python2.7/site-packages/certifi/cacert.pem".format(w=workdir)
        #else:
        #    cacert = "--cacert {w}/miniconda/lib/python3.6/site-packages/certifi/cacert.pem".format(w=workdir)

        cookies = "-c {w}/.esg/.dods_cookies".format(w=user_home)
        key = "--key {w}/.esg/esgf.cert".format(w=user_home)
        cert = "--cert {w}/.esg/esgf.cert".format(w=user_home)

        #cmd = "curl -L -v {cacert} {cookies} {key} {cert} {url}".format(cacert=cacert,
        #                                                                cookies=cookies,
        #                                                                key=key,
        #                                                                cert=cert,
        #                                                                url=dds_url)

        cmd = "curl -L -v {cookies} {key} {cert} {url}".format(cookies=cookies,
                                                               key=key,
                                                               cert=cert,
                                                               url=dds_url)
        run_cmd(cmd, True, False, True)

        cmd = "ls -al {workdir}/.esg".format(workdir=user_home)
        run_cmd(cmd, True, False, True)

        user_home = os.environ['HOME']
        print("xxx HOME: {h}".format(h=user_home))
        if sys.platform == 'darwin':
            cmd = "cp {d}/tests/dodsrccircleciDarwin {h}/.dodsrc".format(d=repo_dir,
                                                                         h=user_home)
        else:
            cmd = "cp {d}/tests/dodsrccircleciLinux {h}/.dodsrc".format(d=repo_dir,
                                                                        h=user_home)

        #run_cmd(cmd, True, False, True)

        # remove afterwards...

    
