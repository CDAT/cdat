from Util import *

class TestSetup(object):

    def __init__(self, nightly_setup, repo_name, py_version):
        """
        clone the specified <repo_name>
        install packages needed to run tests specified by <repo_name>
        in the environment specified in <nightly_setup>

        saves the following info:
           self.log -- to be implemented
           self.repo_name 
           self.repo_dir
        """

        # to be implemented
        self.log = None
        workdir = nightly_setup.workdir
        conda_path = nightly_setup.conda_path
        env = None
        # REVISIT - do assert here..
        if py_version == 'py2':
            env = nightly_setup.py2_env
        elif py_version == 'py3':
            env = nightly_setup.py3_env

        # clone repo
        full_path_dir = workdir + '/' + repo_name
        if os.path.exists(full_path_dir) == False:
            ret_code = git_clone_repo(workdir, repo_name)
            if ret_code != SUCCESS:
                raise Exception('git_clone_repo() failed')

        self.repo_name = repo_name
        self.repo_dir = full_path_dir

        # install things needed for running tests.
        # conda install -c uvcdat/label/nightly -c conda-forge -c uvcdat nose image-compare 
        cmd = 'conda install -c uvcdat/label/nightly -c conda-forge '
        cmd += '-c uvcdat nose'
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            raise Exception('installing needed packages from test failed')

    def run_tests(self, nightly_setup, py_version, run_tests_invoke_cmd):

        if py_version == 'py2':
            env = nightly_setup.py2_env
        elif py_version == 'py3':
            env = nightly_setup.py3_env
        conda_path = nightly_setup.conda_path
        workdir = nightly_setup.workdir

        cmds_list = []
        cmd = 'export UVCDAT_ANONYMOUS_LOG=False'
        cmds_list.append(cmd)

        cmd = 'cd ' + self.repo_dir 
        cmds_list.append(cmd)

        #cmd = 'python run_tests.py -s -v2'
        cmd = run_tests_invoke_cmd
        cmds_list.append(cmd)

        ret_code = run_in_conda_env(conda_path, env, cmds_list)
        return(ret_code)


class CdmsTestSetup(TestSetup):
    def __init__(self, nightly_setup, repo_name, py_version):
        super(CdmsTestSetup, self).__init__(nightly_setup, repo_name, py_version)

        if py_version == 'py2':
            env = nightly_setup.py2_env
        elif py_version == 'py3':
            env = nightly_setup.py3_env

        # install additional packages needed by this testsuite
        cmd = 'conda install -c uvcdat/label/nightly -c conda-forge '
        cmd += '-c uvcdat image-compare'
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(nightly_setup.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            raise Exception('Running ' + cmd + ' failed')


class VcsTestSetup(TestSetup):
    def __init__(self, nightly_setup, repo_name, py_version):
        super(VcsTestSetup, self).__init__(nightly_setup, repo_name, py_version)

        if py_version == 'py2':
            env = nightly_setup.py2_env
        elif py_version == 'py3':
            env = nightly_setup.py3_env

        # install additional packages needed by this testsuite
        cmd = 'conda install -c uvcdat/label/nightly -c conda-forge '
        cmd += '-c uvcdat mesalib image-compare \\\"matplotlib<2.1\\\"'
        cmds_list = []
        cmds_list.append(cmd)
        ret_code = run_in_conda_env(nightly_setup.conda_path, env, cmds_list)
        if ret_code != SUCCESS:
            raise Exception('Running ' + cmd + ' failed')

    
