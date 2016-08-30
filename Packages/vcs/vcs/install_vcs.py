#############################################################################
#                                                                           #
# Create the $HOME/.uvcdat directory if it is does not exist.        #
# Copy the appropriate files from the $PYTHONHOME/bin directory to the      #
# user's $HOME/.uvcdat directory.                                    #
#                                                                           #
#############################################################################


def _files():
    import sys
    import os
    import vcs
    dotdir, dotdirenv = vcs.getdotdirectory()
    #
    # Create .uvcdat directory if it does not exist
    try:
        fn = os.environ[dotdirenv]
    except:
        try:
            fn = '%s/%s' % (os.path.expanduser("~"), dotdir)
        except:
            print "Could not find the $HOME or the %s directory." % (dotdir)
            print "Set your environment variable 'HOME' or '%s'" % (dotdirenv)
            print "to your home directory. (e.g., 'export HOME=/home/username')."
            sys.exit()
    if os.access(fn, os.X_OK) == 0:
        try:
            os.mkdir(fn)
        except:
            print "Do not have write permission for user's home directory. Must have write permissions."
            sys.exit()
