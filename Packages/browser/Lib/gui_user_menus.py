#!/usr/bin/env python
#
# The PCMDI Data Browser Defined Data Access functions -  gui_user_menus module
#
#################################################################################
#                                                                               #
# Module:       gui_user_menus module                                           #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                  			        #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser Extend Toplevel Menus.            #
#                                                                               #
# Version:      3.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------


#---------------------------------------------------------------------
# NOTE: The PCMDI Data Browser allows the user to dynamically
#       create Menu's and Menu Items.
#
#       In addition, the user is allowed to attach functions
#       (which the user supplies) to these Menu Item's.
#
#       When these Menu Item's are selected these user
#       supplied functions are called.
#
#       This file contains the special functions which 
#       the user is allowed to import and call from within
#       their coding.  They are supplied to enable the
#       user to gain access to the 'Defined' data arrays
#       which recide in VCDAT's memory space.
#---------------------------------------------------------------------

import __main__
import gui_formulate
import gui_message


#---------------------------------------------------------------------
# Return pointer to Defined variable in Brower's memory
#---------------------------------------------------------------------
def user_menus_get( ):


   b = __main__.__dict__

   parent = __main__.__dict__['tk_root']
   from_selected = 0
   slab = []
   try:
      if (parent.panelDM.var3 is not None):
         slab.append( gui_formulate.data( parent, parent.panelDM.var3) )
      else:
         from_selected = 1
         if (len(parent.panelDV.selected) == 0):
            return slab
         for i in range(len(parent.panelDV.selected_list),0,-1):
            slab.append( gui_formulate.data( parent, var = parent.panelDV.lst1[ parent.panelDV.selected_list[i-1] ] ))
   except:
       pass

   return slab 


#---------------------------------------------------------------------
# Store user's variable into Defined variable of Brower's memory
#---------------------------------------------------------------------
def user_menus_put( vnam, var ):
   import gui_message

#   print 'user_menus_put vnam, var = ', vnam, var
   if (type( vnam ) != type( '' )) or (type( var ) == type( '' )):
      gui_message.error( 'user_menu_put must be called with variable name, pointer' )

   import gui_defined_variables
   a = __main__
   b = __main__.__dict__
   c = __main__.__dict__['tk_root']
   b[ vnam ] = var
   gui_defined_variables.selected_updated( )

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
