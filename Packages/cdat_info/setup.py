from distutils.core import setup
import subprocess

Version = "2.10"
p = subprocess.Popen(
    ("git",
     "describe",
     "--tags"),
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE)
try:
    descr = p.stdout.readlines()[0].strip()
    Version = "-".join(descr.split("-")[:-2])
    if Version == "":
        Version = descr
except:
    descr = Version
f = open("Lib/version.py", "w")
print >>f, "__version__ = '%s'" % Version
print >>f, "__describe__ = '%s'" % descr
f.close()
setup (name = "cdat_info",
       packages = ['cdat_info'],
       package_dir = {'cdat_info': "Lib"},
      )
