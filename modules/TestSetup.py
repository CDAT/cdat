import shutil
from Util import *

class TestSetup(object):

    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        """
        clone the specified <repo_name>
        install packages needed to run tests specified by <repo_name>
        in the environment specified in <uvcdat_setup>

        saves the following info:
           self.label - label for getting packages from conda channel
           self.repo_name - testsuite repo name
           self.repo_dir  - testsuite repo directory
           self.conda_path - conda path
        """

        self.branch = branch
        if branch == 'master':
            self.label = 'nightly'
        else:
            self.label = branch

        workdir = uvcdat_setup.workdir
        conda_path = uvcdat_setup.conda_path
        self.conda_path = conda_path
        
        # clone repo
        ret_code, repo_dir = git_clone_repo(workdir, repo_name, branch)
        if ret_code != SUCCESS:
            raise Exception('git_clone_repo() failed')

        self.repo_name = repo_name
        self.repo_dir = repo_dir
        
        # install things needed for running tests.

        self.install_packages(uvcdat_setup, py_version, 'nose')
        if branch == 'master':
            self.install_packages(uvcdat_setup, py_version, 'flake8')
        else:
            self.install_packages(uvcdat_setup, py_version, 'flake8=3.3.0')

    def get_uvcdat_testdata(self, uvcdat_setup, for_repo_dir, branch):

        # get the tag 
        ret_code, tag = get_branch_name_of_repo(for_repo_dir)
        if ret_code != SUCCESS:
            raise Exception('FAILED...in getting branch name of: ' + for_repo_dir)

        workdir = uvcdat_setup.workdir
        
        ret_code, repo_dir = git_clone_repo(workdir, 'uvcdat-testdata', branch)
        current_dir = os.getcwd()
        print("DEBUG...cwd: " + current_dir)
        print("DEBUG...repo_dir: " + repo_dir)
        os.chdir(repo_dir)
        cmd = 'git pull'
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            raise Exception('FAIL...' + cmd)

        cmd = 'git checkout ' + tag
        ret_code = run_cmd(cmd, True, False, False)
        if ret_code != SUCCESS:
            raise Exception('FAIL...' + cmd)
        os.chdir(current_dir)

    def install_packages(self, uvcdat_setup, py_ver, packages, add_channels=[]):
        """
        installs the specified packages from standard channels and 
        any additional channels (if specified)
        packages - space separated package names
        add_channels - is a list of channels
        """
        env = uvcdat_setup.get_env(py_ver)
        channels = " -c uvcdat/label/{} -c conda-forge -c uvcdat ".format(self.label)
        for channel in add_channels:
            channels += " -c {} ".format(channel)

        cmd = "conda install {} {} > /dev/null 2>&1".format(channels, packages)
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(self.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            raise Exception('Failed in installing packages')
        

    def run_tests(self, uvcdat_setup, py_version, run_tests_invoke_cmd):

        env = uvcdat_setup.get_env(py_version)

        conda_path = uvcdat_setup.conda_path
        workdir = uvcdat_setup.workdir

        cmds_list = []
        cmd = 'export UVCDAT_ANONYMOUS_LOG=False'
        cmds_list.append(cmd)

        cmd = 'cd ' + self.repo_dir 
        cmds_list.append(cmd)

        cmd = run_tests_invoke_cmd
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(conda_path, env, cmds_list)
        return(ret_code)


class CdmsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        super(CdmsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch)

        if py_version == 'py2':
            env = uvcdat_setup.py2_env
        elif py_version == 'py3':
            env = uvcdat_setup.py3_env

        super(CdmsTestSetup, self).install_packages(uvcdat_setup, py_version, 'image-compare')
        
class VcsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        super(VcsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch)

        if py_version == 'py2':
            env = uvcdat_setup.py2_env
        elif py_version == 'py3':
            env = uvcdat_setup.py3_env

        packages = 'mesalib image-compare \\\"matplotlib<2.1\\\"'
        super(VcsTestSetup, self).install_packages(uvcdat_setup, py_version, packages)

        # get uvcdat-testdata
        for_repo_dir = os.path.join(uvcdat_setup.workdir, branch, repo_name)
        super(VcsTestSetup, self).get_uvcdat_testdata(uvcdat_setup, for_repo_dir, branch)
                    

class PcmdiTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        super(PcmdiTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch)

        if py_version == 'py2':
            env = uvcdat_setup.py2_env
        elif py_version == 'py3':
            env = uvcdat_setup.py3_env
        
        packages = 'vcs vcsaddons cdp mesalib image-compare'
        super(PcmdiTestSetup, self).install_packages(uvcdat_setup, py_version, packages)    

        channels_list = ['pcmdi']
        super(PcmdiTestSetup, self).install_packages(uvcdat_setup, py_version, packages, channels_list)


class VcsaddonsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        super(VcsaddonsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch)

        if py_version == 'py2':
            env = uvcdat_setup.py2_env
        elif py_version == 'py3':
            env = uvcdat_setup.py3_env

        packages = 'mesalib image-compare \\\"matplotlib<2.1\\\" numpy=1.13 vcs'
        super(VcsaddonsTestSetup, self).install_packages(uvcdat_setup, py_version, packages)

    
