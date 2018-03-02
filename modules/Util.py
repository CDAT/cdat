import os
import subprocess
import shlex
import shutil
from Const import *

#                                                                                                                 
# following code is mostly copied from cdms/run_tests.py                                                          
#                   

def run_command(cmd, join_stderr=True, shell_cmd=False, verbose=True, cwd=None):
    print("CMD: " + cmd)
    if isinstance(cmd, str):
        cmd = shlex.split(cmd)

    if join_stderr:
        stderr = subprocess.STDOUT
    else:
        stderr = subprocess.PIPE

    if cwd is None:
        current_wd = os.getcwd()
    else:
        current_wd = cwd

    P = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=stderr,
        bufsize=0, cwd=current_wd, shell=shell_cmd)
    out = []
    while P.poll() is None:
        read = P.stdout.readline().rstrip()
        out.append(read.decode('utf-8'))
        if verbose == True:
            print(read)

    ret_code = P.returncode
    return(ret_code, out)
                                                                                              
def run_cmd(cmd, join_stderr=True, shell_cmd=False, verbose=True, cwd=None):

    ret_code, output = run_command(cmd, join_stderr, shell_cmd, verbose, cwd)
    return(ret_code)

def run_cmd_get_output(cmd, join_stderr=True, shell_cmd=False, verbose=True, cwd=None):

    ret_code, output = run_command(cmd, join_stderr, shell_cmd, verbose, cwd)
    return(ret_code, output)

def git_clone_repo(workdir, repo_name, branch='master', repo_dir=None):
    """ git clone https://github.com/UV-CDAT/<repo_name> and place it in
        <repo_dir>.
        If <repo_dir> is not specified, place the repo in 
        <workdir>/<branch>/<repo_name> directory                                              
    """
    print("DEBUG...in git_clone_repo()")
    if repo_name == 'pcmdi_metrics':
        url = 'https://github.com/pcmdi/' + repo_name
    else:
        url = 'https://github.com/UV-CDAT/' + repo_name

    branch_dir = os.path.join(workdir, branch)
    if not os.path.isdir(branch_dir):
        os.mkdir(branch_dir)
    
    if repo_dir is None:
        repo_dir = os.path.join(workdir, branch, repo_name)
        if os.path.isdir(repo_dir):
            shutil.rmtree(repo_dir)

    cmd = "git clone {url} {repo_dir}".format(url=url, repo_dir=repo_dir)
    ret_code = run_cmd(cmd)

    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    cmd = 'git pull'
    ret_code = run_cmd(cmd, True, False, True, repo_dir)
    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    print("xxx branch: " + branch)
    if branch != 'master' and repo_name != 'uvcdat-testdata' and branch != 'cdat-3.0.beta':
        
        cmd = 'git describe --tags --abbrev=0'
        ret_code, cmd_output = run_cmd_get_output(cmd, True, False, True, repo_dir)
        version = cmd_output[0]

        cmd = "git checkout {}".format(version)
        ret_code = run_cmd(cmd, True, False, True, repo_dir)

    print("...returning from git_clone_repo()...")
    return(ret_code, repo_dir)

def run_in_conda_env(conda_path, env, cmds_list):
    """
    conda_path - path to comda command
    env - conda environment name
    cmds_list - a list of commands
    This function runs all the commands in cmds_list in the specified
    conda environment.
    """

    add_path = "export PATH={path}:$PATH".format(path=conda_path)
    cmds = "{add_path_cmd}; source activate {e}".format(add_path_cmd=add_path,
                                                       e=env)
    for a_cmd in cmds_list:
        cmds = "{existing}; {new}".format(existing=cmds, new=a_cmd)        
    cmds = "{existing}; source deactivate".format(existing=cmds)

    cmd = "bash -c \"{the_cmds}\"".format(the_cmds=cmds)
    print("CMD: " + cmd)
    ret_code = os.system(cmd)
    print(ret_code)
    return(ret_code)

def get_tag_name_of_repo(repo_dir):

    current_dir = os.getcwd()
    os.chdir(repo_dir)
    cmd = 'git describe --tags --abbrev=0'
    ret_code, cmd_output = run_cmd_get_output(cmd, True, False, True)
    branch = cmd_output[0]
    os.chdir(current_dir)

    return(ret_code, branch)
