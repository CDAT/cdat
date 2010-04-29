#!/usr/bin/env python
#
# The PCMDI Data Browser Busy Cursor -  gui_busy module
#
#################################################################################
#                                                                               #
# Module:       gui_busy module                                                 #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser Tkinter "gui busy" panel          #
#               GUI.                                                            #
#                                                                               #
# Version:      3.0                                                             #
#                                                                               #
#################################################################################

#################################################################################
#                                                                               #
# Start the busy cursor prompt.                                                 #
#                                                                               #
#################################################################################
def busyStart(self, parent):
   try:
      busyEnd(self, parent) # If an error occurred, then end previous busy
   except:
      pass
   newcursor = parent.busyCursor
   newPreBusyCursors = {}
   for component in parent.busyWidgets:
       newPreBusyCursors[component] = component['cursor']
       component.configure(cursor=newcursor)
       component.update_idletasks()
   self.preBusyCursors = (newPreBusyCursors, parent.preBusyCursors)

#################################################################################
#                                                                               #
# End the busy cursor prompt.                                                   #
#                                                                               #
#################################################################################
def busyEnd(self, parent):
   if not self.preBusyCursors:
       return
   oldPreBusyCursors = self.preBusyCursors[0]
   self.preBusyCursors = self.preBusyCursors[1]
   for component in parent.busyWidgets:
       try:
           component.configure(cursor=oldPreBusyCursors[component])
       except KeyError:
           pass
       component.update_idletasks()
