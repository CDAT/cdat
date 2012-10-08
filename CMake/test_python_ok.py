import sys, os
# The main installation script is installation/install.py
# However, we need to first check for problems using 1.5.2 syntax only.
trouble = 0
minimum_python_version = (2,5,0,'final',0)
if not hasattr(sys, 'version_info') or sys.version_info < minimum_python_version:
    sys.stderr.write("Your Python is too old; please see README.txt.\n")
    trouble = 1
for x in ["PYTHONHOME"]:
    if os.environ.has_key(x):
        sys.stderr.write('Please undefine ' + x + ' before installation.\n')
        trouble = 1
if not os.environ.has_key('HOME'):
    sys.stderr.write(\
"Caution: You'll need to set environment variable HOME before using CDAT.\n")
try:
   import Tkinter
except:
    if not "--enable-esg" in sys.argv: 
        sys.stderr.write("Your Python does not have support for Tkinter CDAT will not work\n")
        trouble=1
    else:
        sys.stderr.write("Your python does not have Tkinter, this means for example no GUI will be allowed on your system, or any kind of graphics")
if trouble:
    raise SystemExit, 1
print 'Your Python checked OK!'
