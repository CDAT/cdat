from test_python_ok import *

import os

# The main installation script is installation/install.py
# However, we need to first check for problems using 1.5.2 syntax only.

current_dir = os.path.dirname(__file__)
install_script_path = os.path.join(current_dir, '..', 'installation', 'install.py')

execfile(install_script_path)
