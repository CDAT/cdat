# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Animation module
#
############################################################################
#                                                                          #
# Module:       testanimate module                                         #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's animation capabilities.                 #
#                                                                          #
#               run with "-i" option:   python -i testboxfill.py           #
#                                                                          #
#               To test the animation capability from command line you     #
#               will need to do the following:                             #
#                  * issue the UNIX "more" command on the is script file   #
#                    to see the commented animation commands               #
#                  * now run this script with the -i option:               #
#                    python -i it.py                                       #
#                  * at the python command prompt issue the commented cdat #
#                    commands below                                        #
#                                                                          #
# Version:      4.0                                                        #
#                                                                          #
############################################################################
#
#
#
############################################################################
#                                                                          #
# Import: VCS  and cdms modules.                                           #
#                                                                          #
############################################################################

import vcs_legacy,cdms2 as cdms,sys,os,support
support.dogui = True
if support.dogui:

    f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
    s=f('clt')
    s=s[0:12,:]

    x=vcs_legacy.init()
#    y=vcs_legacy.init()

    b=x.createboxfill('new')
    x.plot(s,b)

    #x.animate.create(thread_it=1)
    #raw_input("press enter")
    x.animate.create()
    x.animate.run()
    import time
    time.sleep(2)
    x.animate.zoom(1.4)
    time.sleep(2)
    x.animate.horizontal(50)
    x.animate.vertical(-50)
    import time
    time.sleep(3)
#    x.animate.gui()
    x.animate.pause(1)
    import time
    time.sleep(9)
    x.animate.stop()
#    x.animate.zoom(1)
#    x.animate.frame(2)
#    x.animate.frame(5)
#    x.animate.close()
else:
    print 'You need to run this one by hand (turn support.dogui to True first)'
