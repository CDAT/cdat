import os
import subprocess
import shlex
from Const import *


# https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/2.12-osx.yml                                      
# https://raw.githubusercontent.com/UV-CDAT/uvcdat/master/conda/2.12.yml                                          

#                                                                                                                 
# following code is mostly copied from cdms/run_tests.py                                                          
#                                                                                                                 
def run_cmd(cmd, join_stderr=True, shell_cmd=False, verbose=True):
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
        out.append(read)
        if verbose == True:
            print(read)

    ret_code = P.returncode
    return(ret_code)

def git_clone_repo(workdir, repo_name):
    """ git clone https://github.com/UV-CDAT/<repo_name> and place it in
        <workdir>/<repo_name> directory                                              
    """
    url = 'https://github.com/UV-CDAT/' + repo_name

    
    cmd = 'git clone ' + url + ' ' + workdir + '/' + repo_name
        
    ret_code = run_cmd(cmd)
    if ret_code != SUCCESS:
        print("FAIL..." + cmd)
        return ret_code

    return(ret_code)

def run_in_conda_env(conda_path, env, cmds_list):
    cmd = 'export PATH=' + conda_path + ':$PATH; '
    cmd += 'conda activate ' + env + '; '
    
    for a_cmd in cmds_list:
        cmd += a_cmd + '; '
    cmd += 'conda deactivate'
    print('CMD: ' + cmd)
    ret_code = os.system(cmd)
    print(ret_code)
    
    return(ret_code)

