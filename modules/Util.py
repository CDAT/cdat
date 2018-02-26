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

    if cwd == None:
        current_wd = os.getcwd()
    else:
        current_wd = cwd
    #P = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=stderr,
    #    bufsize=0, cwd=os.getcwd(), shell=shell_cmd)
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

def git_clone_repo(workdir, repo_name, branch='master'):
    """ git clone https://github.com/UV-CDAT/<repo_name> and place it in
        <workdir>/<branch>/<repo_name> directory                                              
    """
    if repo_name == 'pcmdi_metrics':
        url = 'https://github.com/pcmdi/' + repo_name
    else:
        url = 'https://github.com/UV-CDAT/' + repo_name

    branch_dir = os.path.join(workdir, branch)
    print("xxx branch_dir: " + branch_dir)
    if not os.path.isdir(branch_dir):
        os.mkdir(branch_dir)
    
    repo_dir = os.path.join(workdir, branch, repo_name)
    if os.path.isdir(repo_dir):
            shutil.rmtree(repo_dir)

    cmd = "git clone {url} {repo_dir}".format(url=url, repo_dir=repo_dir)
    ret_code = run_cmd(cmd)

    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    current_dir = os.getcwd()
    print("xxx current_dir: ", current_dir)
    print("xxx repo_dir: ", repo_dir)
    # TEMPORARY
    cmd = "ls "
    ret_code = run_cmd(cmd, True, False, True)
    ##os.chdir(repo_dir)
    cmd = 'git pull'
    ret_code = run_cmd(cmd, True, False, True, repo_dir)
    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    if branch != 'master' and repo_name != 'uvcdat-testdata':
        
        cmd = 'git describe --tags --abbrev=0'
        ret_code, cmd_output = run_cmd_get_output(cmd, True, False, True, repo_dir)
        version = cmd_output[0]

        cmd = "git checkout {}".format(version)
        ret_code = run_cmd(cmd, True, False, True, repo_dir)

    ##print("xxx chdir current_dir: " + current_dir)
    ##os.chdir(current_dir)
    # TEMPORARY
    cmd = "ls "
    ret_code = run_cmd(cmd, True, False, True)
    print("xxx returning from git_clone_repo")
    return(ret_code, repo_dir)

def run_in_conda_env(conda_path, env, cmds_list):
    """
    conda_path - path to comda command
    env - conda environment name
    cmds_list - a list of commands
    This function runs all the commands in cmds_list in the specified
    conda environment.
    """
    cmd = 'bash -c \"export PATH=' + conda_path + ':$PATH; '
    cmd += 'source activate ' + env + '; '
    
    for a_cmd in cmds_list:
        cmd += a_cmd + '; '
    cmd += 'source deactivate \"'
    print('CMD: ' + cmd)

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
