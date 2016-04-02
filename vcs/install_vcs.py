#############################################################################
#                                                                           #
# Create the $HOME/.uvcdat directory if it is does not exist.        #
# Copy the appropriate files from the $PYTHONHOME/bin directory to the      #
# user's $HOME/.uvcdat directory.                                    #
#                                                                           #
#############################################################################
import vcs


def _files():
    import sys
    import os
    import shutil
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
            print "Could not find the $HOME directory or the %s."
            print "Set your environment variable 'HOME' or '%s'" % (dotdir, dotdirenv)
            print "to your home directory. (e.g., 'setenv HOME /home/user')."
            sys.exit()
    if os.access(fn, os.X_OK) == 0:
        try:
            os.mkdir(fn)
        except:
            print "Do not have write permission for user's home directory. Must have write permissions."
            sys.exit()
    #
    # Copy the initial.attributes file to the user's $HOME/.uvcdat directory
    files = ["initial.attributes",
            "data_continent_states",
            "data_continent_political",
            "data_continent_river",
            "data_continent_other7",
            ]
    for file_name in files:
        file_cp = os.path.join(
            vcs.prefix,
            'share',
            'vcs',
            file_name)
        ofile = os.path.join(fn, file_name)
        print "Copy",file_cp,"to",ofile
        if (os.access(file_cp, os.F_OK) == 1) and (
                os.path.isfile(ofile) == 0):
            shutil.copyfile(file_cp, ofile)

    # Copy font files to the user's $HOME/.uvcdat
    for font in ['Adelon_Regular', 'Arabic', 'Athens_Greek', 'AvantGarde-Book_Bold',
                 'Chinese_Generic1', 'Clarendon', 'Courier', 'hebrew', 'HelvMono',
                 'Russian', 'Times_CG_ATT', 'jsMath-wasy10', 'blex', 'blsy', 'jsMath-msam10']:
        fnm_cp = os.path.join(
            vcs.prefix,
            'share',
            'vcs',
            font +
            '.ttf')
        fnm = os.path.join(fn, font + '.ttf')
        if (os.access(fnm_cp, os.F_OK) == 1) and (os.path.isfile(fnm) == 0):
            shutil.copyfile(fnm_cp, fnm)
