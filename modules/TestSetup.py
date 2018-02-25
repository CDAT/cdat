import shutil
from Util import *

class TestSetup(object):

    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        """
        clone the specified <repo_name>
        install packages needed to run tests specified by <repo_name>
        in the environment specified in <uvcdat_setup>

        saves the following info:
           self.repo_name - testsuite repo name
           self.repo_dir  - testsuite repo directory
           self.conda_path - conda path
        """

        self.branch = branch
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

        uvcdat_setup.install_packages(py_version, 'nose')
        if branch == 'master':
            uvcdat_setup.install_packages(py_version, 'flake8')
        else:
            uvcdat_setup.install_packages(py_version, 'flake8=3.3.0')

    def get_uvcdat_testdata(self, uvcdat_setup, for_repo_dir, branch):
        """
        get uvcdat-testdata code that has same tag as <for_repo_dir>
        For example, iv <for_repo_dir> is a vcs repo and is tagged '2.12',
        then this function will check out uvcdat-testdata with same tag.
        """
        # get the tag 
        ret_code, tag = get_tag_name_of_repo(for_repo_dir)
        if ret_code != SUCCESS:
            raise Exception('FAILED...in getting branch name of: ' + for_repo_dir)

        workdir = uvcdat_setup.workdir
        
        ret_code, repo_dir = git_clone_repo(workdir, 'uvcdat-testdata', branch)
        current_dir = os.getcwd()
        os.chdir(repo_dir)
        cdir = os.getcwd()
        print("xxx current dir: " + cdir)
        cmd = 'git pull'
        ret_code = run_cmd(cmd, True, False, True)
        if ret_code != SUCCESS:
            raise Exception('FAIL...' + cmd)

        cdir = os.getcwd()
        print("xxx current dir: " + cdir)
        print("xxx xxx repo_dir: ", repo_dir)
        cmd = 'git checkout ' + tag
        os.chdir(repo_dir)
        ret_code = run_cmd(cmd, True, False, True)
        if ret_code != SUCCESS:
            raise Exception('FAIL...' + cmd)
        os.chdir(current_dir)

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

        uvcdat_setup.install_packages(py_version, 'image-compare')
        
class VcsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        super(VcsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch)

        if py_version == 'py2':
            env = uvcdat_setup.py2_env
        elif py_version == 'py3':
            env = uvcdat_setup.py3_env

        packages = 'mesalib image-compare \\\"matplotlib<2.1\\\"'
        uvcdat_setup.install_packages(py_version, packages)

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
        uvcdat_setup.install_packages(py_version, packages)    

        channels_list = ['pcmdi']
        uvcdat_setup.install_packages(py_version, packages, channels_list)


class VcsaddonsTestSetup(TestSetup):
    def __init__(self, uvcdat_setup, repo_name, py_version, branch='master'):
        super(VcsaddonsTestSetup, self).__init__(uvcdat_setup, repo_name, py_version, branch)

        if py_version == 'py2':
            env = uvcdat_setup.py2_env
        elif py_version == 'py3':
            env = uvcdat_setup.py3_env

        packages = 'mesalib image-compare \\\"matplotlib<2.1\\\" numpy=1.13 vcs'
        uvcdat_setup.install_packages(py_version, packages)

    
