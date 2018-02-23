import os
import subprocess
import shlex
import shutil
from Const import *


# https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/2.12-osx.yml                                      
# https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/2.12.yml                                          

#                                                                                                                 
# following code is mostly copied from cdms/run_tests.py                                                          
#                   

def run_command(cmd, join_stderr=True, shell_cmd=False, verbose=True):
    print("CMD: " + cmd)
    if isinstance(cmd, str):
        cmd = shlex.split(cmd)

    if join_stderr:
        stderr = subprocess.STDOUT
    else:
        stderr = subprocess.PIPE

    P = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=stderr,
        bufsize=0, cwd=os.getcwd(), shell=shell_cmd)
    out = []
    while P.poll() is None:
        read = P.stdout.readline().rstrip()
        out.append(read.decode('utf-8'))
        if verbose == True:
            print(read)

    ret_code = P.returncode
    return(ret_code, out)
                                                                                              
def run_cmd(cmd, join_stderr=True, shell_cmd=False, verbose=True):

    ret_code, output = run_command(cmd, join_stderr, shell_cmd, verbose)
    return(ret_code)

def run_cmd_get_output(cmd, join_stderr=True, shell_cmd=False, verbose=True):

    ret_code, output = run_command(cmd, join_stderr, shell_cmd, verbose)
    return(ret_code, output)

def git_clone_repo(workdir, repo_name, branch='master'):
    """ git clone https://github.com/UV-CDAT/<repo_name> and place it in
        <workdir>/<repo_name> directory                                              
    """
    if repo_name == 'pcmdi_metrics':
        url = 'https://github.com/pcmdi/' + repo_name
    else:
        url = 'https://github.com/UV-CDAT/' + repo_name

    #repo_dir = workdir + '/' + repo_name
    repo_dir = os.path.join(workdir, repo_name)
    if os.path.isdir(repo_dir):
            shutil.rmtree(repo_dir)

    #cmd = 'git clone ' + url + ' ' + repo_dir
    #ret_code = run_cmd(cmd, True, False, False)
    cmd = "git clone {url} {repo_dir}".format(url=url, repo_dir=repo_dir)
    ret_code = run_cmd(cmd)

    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    current_dir = os.getcwd()
    print("DEBUG DEBUG DEBUG...git_clone_repo()")
    print("DEBUG DEBUG DEBUG...current_dir: " + current_dir)
    os.chdir(repo_dir)
    cmd = 'git pull'
    ret_code = run_cmd(cmd)
    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    if branch != 'master':
        cmd = 'git describe --tags --abbrev=0'
        ret_code, cmd_output = run_cmd_get_output(cmd, True, False, True)
        version = cmd_output[0]

        cmd = "git checkout {}".format(version)
        ret_code = run_cmd(cmd)

    os.chdir(current_dir)
    return(ret_code)

def run_in_conda_env(conda_path, env, cmds_list):
    cmd = 'bash -c \"export PATH=' + conda_path + ':$PATH; '
    cmd += 'source activate ' + env + '; '
    
    for a_cmd in cmds_list:
        cmd += a_cmd + '; '
    cmd += 'source deactivate \"'
    print('CMD: ' + cmd)

    ret_code = os.system(cmd)
    print(ret_code)
    return(ret_code)

def get_branch_name_of_repo(repo_dir):

    current_dir = os.getcwd()
    print("DEBUG DEBUG DEBUG...git_branch_name_of_repo")
    print("DEBUG DEBUG DEBUG...current_dir: " + current_dir)
    os.chdir(repo_dir)
    cmd = 'git describe --tags --abbrev=0'
    ret_code, cmd_output = run_cmd_get_output(cmd, True, False, True)
    #cmd_output = "".join(out)
    #branch = cmd_output.strip()
    branch = cmd_output[0]
    print("xxx debug, branch: " + branch)
    os.chdir(current_dir)

    return(ret_code, branch)
