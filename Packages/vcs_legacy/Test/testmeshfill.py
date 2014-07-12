# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Meshfill (Gfm) module
#
############################################################################
#                                                                          #
# Module:       testmeshfill module                                        #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's boxfill graphics method.                #
#                                                                          #
#               run with "-i" option:   python -i testmeshfill.py          #
#                                                                          #
#               This function runs a test for the mesh fill graphics       #
#               method.                                                    #
#                                                                          #
# Version:      5.0                                                        #
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

import vcs_legacy
import cdms2 as cdms
import sys
import os
import support

f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','meshfill.nc'))
M=f('Mesh')
s=f('Data')

x=vcs_legacy.init()
x.scriptrun(os.path.join(cdms.__path__[0],'..','..','..','..','bin','ASD.scr'))
m=x.getmeshfill('ASD')
m.wrap=[0,360]
m.mesh='y'
#mn,mx=vcs_legacy.minmax(s)
levs=vcs_legacy.mkscale(-10,30)
#print levs
levs=vcs_legacy.mkevenlevels(levs[0],levs[-1],256)
levs2=levs[::25]
if levs2[-1]!=levs[-1]: levs2.append(levs[-1])
lbls=vcs_legacy.mklabels(levs2)
#print levs
m.legend=lbls
m.levels=levs[::-1]
m.fillareacolors=vcs_legacy.getcolors(levs)
## m.list()
x.plot(s,M,m,bg=support.bg)
support.check_plot(x)
