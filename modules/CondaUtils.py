import os
import sys

from Util import *

def get_conda_dir(workdir, py_ver):
    #if py_ver.startswith('py2'):
    #    conda_dir = os.path.join(workdir, 'miniconda2')
    #else:
    #    conda_dir = os.path.join(workdir, 'miniconda3')
    conda_dir = os.path.join(workdir, 'miniconda')
    return conda_dir

def install_miniconda(workdir, py_ver):
    if os.path.isdir(workdir) == True:
        print("INFO: {dir} already exists".format(dir=workdir))
    else:
        os.mkdir(workdir)

    conda_dir = get_conda_dir(workdir, py_ver)
    conda_path = os.path.join(conda_dir, 'bin')

    if os.path.isdir(conda_dir) == True:
        print("INFO: {dir} already exists".format(dir=conda_dir))
        print("Conda is already installed.")
        return(SUCCESS, conda_path)


    url = "https://repo.continuum.io/miniconda"
    conda_script = os.path.join(workdir, 'miniconda.sh')
    if py_ver.startswith('py2'):
        conda_ver = 'Miniconda2'
    else:
        conda_ver = 'Miniconda3'

    if sys.platform == 'darwin':
        conda_script = "{c}-latest-MacOSX-x86_64.sh".format(c=conda_ver)
        conda_script_full_path = os.path.join(workdir, conda_script)
        source_script = os.path.join(url, conda_script)
        cmd = "curl {src} -o {dest}".format(src=source_script, dest=conda_script_full_path)
    else:
        conda_script = "{c}-latest-Linux-x86_64.sh".format(c=conda_ver)
        conda_script_full_path = os.path.join(workdir, conda_script)
        source_script = os.path.join(url, conda_script)
        cmd = "wget {src} -O {dest}".format(src=source_script, dest=conda_script_full_path)

    print("DEBUG...cmd: {c}".format(c=cmd))

    ret_code = run_cmd(cmd, True, False, True)
    if ret_code != SUCCESS:
        print("FAIL..." + cmd)
        return(ret_code, None)

    cmd = "bash {script} -b -p {dir}".format(script=conda_script_full_path, 
                                             dir=conda_dir)

    # run the command, set verbose=False 
    ret_code = run_cmd(cmd, True, False, True)
    if ret_code != SUCCESS:
        print("FAIL...installing miniconda")
        return(ret_code, None)

    conda_cmd = os.path.join(conda_path, 'conda')
    cmd = "{c} config --set always_yes yes --set changeps1 no".format(c=conda_cmd)
    
    ret_code = run_cmd(cmd, True, False, True)
    if ret_code != SUCCESS:
        print('FAILED: ' + cmd)
        return(ret_code, None)

    cmd = "{c} config --set anaconda_upload no".format(c=conda_cmd)
    ret_code = run_cmd(cmd)
    if ret_code != SUCCESS:
        print('FAILED: ' + cmd)
        return(ret_code, None)

    cmd = "{c} config --add channels conda-forge".format(c=conda_cmd)
    ret_code = run_cmd(cmd)
    if ret_code != SUCCESS:
        print('FAILED: ' + cmd)
        return(ret_code, None)

    cmd = "{c} config --set channel_priority strict".format(c=conda_cmd)
    ret_code = run_cmd(cmd)

    print("DEBUG...install_miniconda() returning conda_path: {p}".format(p=conda_path))
    return(ret_code, conda_path)
