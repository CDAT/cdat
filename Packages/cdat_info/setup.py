from distutils.core import setup
import subprocess
import os
import tempfile
import shutil
import sys

src_dir = os.path.dirname(os.path.realpath(__file__))

os.chdir(src_dir)
GIT_DESCRIBE = subprocess.Popen(["git","describe","--tag"],stdout=subprocess.PIPE).stdout.read().strip()

tmp_dir = os.path.join(tempfile.gettempdir(),"cdat_info_build")
shutil.rmtree(tmp_dir,ignore_errors=True)
os.makedirs(tmp_dir)
os.chdir(tmp_dir)
lib_dir = os.path.join(tmp_dir,"cdat_info_Lib")
shutil.copytree(os.path.join(src_dir,"Lib"),lib_dir)



f=open(os.path.join(src_dir,"cdat_info.py.in"))
cdat_info = f.read().replace("GIT_DESCRIBE",GIT_DESCRIBE)
f.close()
f=open(os.path.join(lib_dir,"cdat_info.py"),"w")
f.write(cdat_info)
f.close()

setup (name = "cdat_info",
       packages = ['cdat_info'],
       package_dir = {'cdat_info': lib_dir},
      )
shutil.rmtree(lib_dir,ignore_errors=True)
