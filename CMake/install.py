import os

# The main installation script is installation/install.py
# However, we need to first check for problems using 1.5.2 syntax only.
current_dir = os.path.dirname(__file__)

execfile(os.path.join(current_dir, 'test_python_ok.py'))

install_script_path = os.path.join(current_dir, '..', 'installation', 'install.py')
execfile(install_script_path)
