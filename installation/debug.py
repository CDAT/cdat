## action['setup.py'] = sys.executable + ' setup.py build --debug install'
## action['install_script'] = './install_script --debug ' + sys.exec_prefix
## for k in ['makefile','Makefile','MAKEFILE']:
##     action[k] = make_code + " PREFIX='%s' DEBUG=1 install " % sys.exec_prefix

# matplotlib depends on pkg-config under install/bin
action['setup.py'] = 'PATH=%s/bin:$PATH && %s setup.py build --debug install --prefix=%s ; ' \
    % (sys.exec_prefix, sys.executable, target_prefix)
action['install_script'] = './install_script  %s %s --debug ; ' % (target_prefix, sys.exec_prefix)
for k in ['makefile','Makefile','MAKEFILE']:
    action[k] = make_code + " PYPREFIX='%s' PREFIX='%s' DEBUG=1 install ; " % (sys.exec_prefix,target_prefix)
action['autogen.sh'] = "autogen.sh ; ./configure --prefix=%s  --with-python=%s ; make ; make install ;" % (os.path.join(os.path.split(target_prefix)[0],'Externals'), os.path.join(sys.exec_prefix,'bin','python'))
