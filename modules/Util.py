import os
import subprocess
import shlex
import shutil
import time

from Const import *

def run_command(cmd, join_stderr=True, shell_cmd=False, verbose=True, cwd=None):
    print("CMD: {c}".format(c=cmd))
    if isinstance(cmd, str):
        cmd = shlex.split(cmd)

    if join_stderr:
        stderr_setting = subprocess.STDOUT
    else:
        stderr_setting = subprocess.PIPE

    if cwd is None:
        current_wd = os.getcwd()
    else:
        current_wd = cwd

    P = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=stderr_setting,
        bufsize=0, cwd=current_wd, shell=shell_cmd)
    out = []
    while P.poll() is None:
        read = P.stdout.readline().rstrip()
        decoded_str = read.decode('utf-8')
        out.append(decoded_str)
        if verbose == True:
            print(decoded_str)

    ret_code = P.returncode
    return(ret_code, out)
                                                                                              
def run_cmd(cmd, join_stderr=True, shell_cmd=False, verbose=True, cwd=None):

    ret_code, output = run_command(cmd, join_stderr, shell_cmd, verbose, cwd)
    return(ret_code)

def run_cmd_capture_output(cmd, join_stderr=True, shell_cmd=False, verbose=True, cwd=None):

    ret_code, output = run_command(cmd, join_stderr, shell_cmd, verbose, cwd)
    return(ret_code, output)

def git_clone_repo(workdir, repo_name, branch='master', label='master', repo_dir=None):
    """ git clone https://github.com/CDAT/<repo_name> and place it in
        <repo_dir>.
        If <repo_dir> is not specified, place the repo in 
        <workdir>/<branch>/<repo_name> directory                                              
    """
    if repo_name == 'pcmdi_metrics':
        url = 'https://github.com/pcmdi/' + repo_name
    else:
        url = 'https://github.com/CDAT/' + repo_name

    if repo_dir is None:
        branch_dir = os.path.join(workdir, "{b}-{l}".format(b=branch,
                                                            l=label))
        if not os.path.isdir(branch_dir):
            os.mkdir(branch_dir)
    
        repo_dir = os.path.join(branch_dir, repo_name)
        if os.path.isdir(repo_dir):
            shutil.rmtree(repo_dir)

    if not os.path.isdir(repo_dir):
        if branch == 'master':
            cmd = "git clone {url} {repo_dir}".format(url=url, repo_dir=repo_dir)
        else:
            cmd = "git clone -b {b} {url} {repo_dir}".format(b=branch,
                                                             url=url, 
                                                             repo_dir=repo_dir)
        ret_code = run_cmd(cmd, False, False, False)

        if ret_code != SUCCESS:
            print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
            return ret_code

    cmd = 'git pull'
    ret_code = run_cmd(cmd, True, False, True, repo_dir)
    if ret_code != SUCCESS:
        print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
        return ret_code

    if label != 'master':
        
        #if repo_name == 'pcmdi_metrics':
        #    label = "cdat_{label}".format(label=label)
        cmd = "git checkout {label}".format(label=label)
        ret_code = run_cmd(cmd, True, False, True, repo_dir)
        if ret_code != SUCCESS:
            print("FAIL...{failed_cmd}".format(failed_cmd=cmd))
            return ret_code

    cmd = "mkdir tests_png tests_html"
    ret_code = run_cmd(cmd, True, False, True, repo_dir)

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
    print("CMD: {c}".format(c=cmd))
    ret_code = os.system(cmd)
    print(ret_code)
    return(ret_code)

def get_tag_name_of_repo(repo_dir):

    current_dir = os.getcwd()
    os.chdir(repo_dir)
    cmd = 'git describe --tags --abbrev=0'
    ret_code, cmd_output = run_cmd_capture_output(cmd, True, False, True)
    branch = cmd_output[0]
    os.chdir(current_dir)

    return(ret_code, branch)

def run_in_conda_env_capture_output(conda_path, env, cmds_list):

    current_time = time.localtime(time.time())
    time_str = time.strftime("%b.%d.%Y.%H:%M:%S", current_time)
    tmp_file = "/tmp/conda_capture.{curr_time}".format(curr_time=time_str)

    add_path_cmd = "export PATH={path}:$PATH".format(path=conda_path)
    activate_cmd = "source activate {env}".format(env=env)
    cmds = None
    for a_cmd in cmds_list:
        if cmds == None:
            cmds = a_cmd
        else:
            cmds = "{existing}; {new_cmd}".format(existing=cmds, new_cmd=a_cmd)

    deactivate_cmd = 'source deactivate'

    cmd = "bash -c \"{add_path}; {act}; {cmds}; {deact}\"".format(add_path=add_path_cmd,
                                                                  act=activate_cmd,
                                                                  cmds=cmds,
                                                                  deact=deactivate_cmd)

    cmd = "{the_cmd} > {output_file}".format(the_cmd=cmd, output_file=tmp_file)
    print("CMD: {cmd}".format(cmd=cmd))
    ret_code = os.system(cmd)
    print(ret_code)
    if ret_code != SUCCESS:
        return(FAILURE, None)

    with open(tmp_file) as f:
        output = f.readlines()
    os.remove(tmp_file)
    return(ret_code, output)


