#!/usr/bin/env python
#
# The PCMDI Data Browser Menu About Dialog -  gui_about module
#
###############################################################################
#                                                                             #
# Module:       gui_about module                                              #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter Main Menu About Dialog. #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Pmw, sys, string
import gui_support

#------------------------------------------------------------------------------
# Begin the creation of the "Help" menu's About dialog
#------------------------------------------------------------------------------
def create(parent):

    version = parent.Version
    p,c = gui_support.buildDate()
    Pmw.aboutversion("%s \n(Executed from %s)\n\nPython built on: %s\nCDAT   built on: %s" % (version,  sys.prefix, p, c))
    Pmw.aboutcopyright("""
Copyright:    2001-2008, Regents of the University of California
All rights reserved.\n\n
This release of CDAT incorporates modules from other organizations besides
LLNL some of which are distributed under open source licenses of their choosing,
and therefore may be subject to different terms and conditions.  We ask that users
respect these all terms and conditions as well as any copyright notices requirements. 
""")
    Pmw.aboutcontact( """ Go to http://cdat.sourceforge.net for documentation, support, 
bug reporting, and releases.

Program for Climate Model Diagnosis and Intercomparison
Lawrence Livermore National Laboratory
Livermore, CA 94550
""")
    about = Pmw.AboutDialog(parent, applicationname = 'The Visual Climate Data Analysis Tools')

    # Position dialog popup
    parent_geom = parent.geometry()
    geom = string.split(parent_geom, '+')
    d1 = string.atoi( geom[1] )
    d2 = string.atoi( geom[2] )
    about.geometry( "+%d+%d" % (d1, d2) )

#--------------------------------------------------------------------
# End of File
#--------------------------------------------------------------------
