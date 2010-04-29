#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Output -  gui_output module
#
#################################################################################
#                                                                               #
# Module:       gui_output module                                               #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser output.                           #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import sys, os, string
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import gui_message

#---------------------------------------------------------------------
#
# Start of Popup Dialogs
#
#---------------------------------------------------------------------------
#
#-------------------------------------------------------------------------------
# Save VCS Plot in a file. Can be either a postscript, EPS, GIF, or CGM file.
#-------------------------------------------------------------------------------
#
class create:
   def __init__(self, parent, type, title, text, entry_str = '', update_widget = None, gm_type = None):
        self.parent = parent
        self.type = type
        self.entry_str = entry_str
        self.update_widget = update_widget
        self.gm_type = gm_type
        self.dialog = Pmw.Dialog( parent,
            title = title,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = gui_control.Command(self.execute, parent) )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent

        lbl=Tkinter.Label(self.dialog.interior(),
            text = text,
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny1=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            entry_background = 'white',
            entry_foreground = 'black',
            value = entry_str,
            entry_width =  50,
            )
        self.eny1.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if self.type in ['template_save_as', 'gm_save_as']: self.eny1.setentry( "" )

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        if type != 'd' and type != 'operation':
	   self.dialog.geometry( "+%d+%d" % (d1, d2) )
        else:
           geom = "+%d+%d" % (d1, d2)
           self.dialog.activate(geometry = geom)

   def execute(self, parent, result):
      if result == 'OK':
         if self.eny1.get( ) == '':
            pass
         else:
            a = self.eny1.get()
            # Add tilde function to entry window
            if a == '~':
              a = os.environ['HOME']
            elif a[0:2] == '~/':
              a = os.environ['HOME'] + '/' + a[2:]
            elif a[0] == '~':
              a = os.environ['HOME'] + '/' + a[1:]

            # Get the page orientation (i.e., either l='landscape', or p=portrait)
            o = 'l'
            if self.parent.panelGC.page_orientation_flg == 0: o = 'p'

            if self.type == 'p':
               self.parent.vcs[ parent.vcs_id ].postscript( a, o )
               gui_control.record_command( parent, "\n# Save VCS Canvas in Postscript File" )
               gui_control.record_command( parent, "vcs.postscript( '%s', 'r' )" % (a,) )
            elif self.type == 'e':
               self.parent.vcs[ parent.vcs_id ].eps( a, 'r', o )
               gui_control.record_command(parent,"\n# Save VCS Canvas in Encapsulated Postscript File")
               gui_control.record_command( parent, "vcs.eps( '%s', 'r')" % (a,) )
            elif self.type == 'g':
               self.parent.vcs[ parent.vcs_id ].gif( a, 'r' )
               gui_control.record_command( parent, "\n# Save VCS Canvas in GIF File" )
               gui_control.record_command( parent, "vcs.gif( '%s', 'r' )" % (a, ) )
            elif self.type == 'c':
               self.parent.vcs[ parent.vcs_id ].cgm( a, 'r' )
               gui_control.record_command( parent, "\n# Save VCS Canvas in CGM File" )
               gui_control.record_command( parent, "vcs.cgm( '%s', 'r' )" % (a,) )
            elif self.type == 'd':
               if ((a == '') or (a == ' ') or
                  (a is None)):
                  self.parent.new_defined_var = None
               else:
                  self.parent.new_defined_var = a
            elif self.type == 'operation':
               try:
                  parent.ret_val = string.atof( self.eny1.get() )
               except:
                  gui_message.error("Not a floating point value.")
                  parent.ret_val = None
            elif self.type == 't':
               self.parent.vcs[ parent.vcs_id ].printer( a, o )
               gui_control.record_command(parent,"\n# Send VCS Canvas to %s Printer" % a )
               gui_control.record_command( parent, "vcs.printer( '%s', '%s' )" % (a, o) )
            elif self.type == 's':
               search_pattern = a
               parent.file_search_pattern =  search_pattern
               #  	re-write the new directory information and save directory list
               d,f = gui_functions._scn_a_dir( parent )
               #parent.panelDM.scl1.setlist( d )
               parent.panelDM.lst1 = d
               #  	display the data files in the directory
#               parent.panelDM.scl2.setlist( f )
               for i in range(len(f)): parent.panelSV.tin3.insert( i, f[i] )
               parent.panelDM.lst2 = f
               #
               parent.panelDM.fid2 = None
               parent.panelDM.var3 = parent.panelDM.remember_var3 = None
               #
#               parent.panelDM.scl3.setlist( () )
               #
               gui_functions._blank_dim1_to_ndim( parent )
            elif self.type == 'ss':
               parent.swap_size = string.atof( a ) * 1000000.
            elif self.type == 'tt':
                 if a not in parent.vcs[ parent.vcs_id ].listelements( 'template' ):
                    gui_message.error( 'Could not find the template name ( %s ) in the template list. Please check the template list to make sure it exist.' % a )
                    return
                 parent.template_name =  a
            elif self.type == 'gm':
                 parent.graphics_method_name = a
            elif self.type in ['template_rename', 'template_copy', 'template_remove', 'template_save_as']:
               new_t_name = a
               if ( (len(new_t_name) < 1) or (self.entry_str == new_t_name) ):
                  self.dialog.destroy()
                  return              			# do nothing
               try:
                  vcs = self.parent.vcs[ parent.vcs_id ]
                  if self.type == 'template_rename':
                     vcs.canvas.renameP(self.entry_str, new_t_name)
                  elif self.type == 'template_copy':
                     vcs.canvas.copyP(self.entry_str, new_t_name)
                  elif self.type == 'template_remove':
                     vcs.canvas.removeP(self.entry_str)
                  elif self.type == 'template_save_as':
                     if new_t_name[-4:] != ".scr": new_t_name += ".scr"
                     vcs.canvas.scriptP( self.entry_str, new_t_name, 'w' )
                  self.update_widget.setlist( vcs.listelements('template') )
               except:
                  gui_message.error( 'Cannot rename template (P_%s). Check to make sure that the old name exist in the template list and that the new name does not.' % self.entry_str )

            elif self.type in ['gm_rename', 'gm_copy', 'gm_remove', 'gm_save_as']:
               new_gm_name = a
               gm_type = string.lower( self.gm_type )
               if ( (len(new_gm_name) < 1) or (self.entry_str == new_gm_name) ):
                  self.dialog.destroy()
                  return              			# do nothing
               try:
                  vcs = self.parent.vcs[ parent.vcs_id ]
                  if self.type == 'gm_rename':
                     if gm_type == 'boxfill': vcs.canvas.renameGfb(self.entry_str, new_gm_name)
                     elif gm_type == 'continents': vcs.canvas.renameGcon(self.entry_str, new_gm_name)
                     elif gm_type == 'isofill': vcs.canvas.renameGfi(self.entry_str, new_gm_name)
                     elif gm_type == 'isoline': vcs.canvas.renameGi(self.entry_str, new_gm_name)
                     elif gm_type == 'outfill': vcs.canvas.renameGfo(self.entry_str, new_gm_name)
                     elif gm_type == 'outline': vcs.canvas.renameGo(self.entry_str, new_gm_name)
                     elif gm_type == 'scatter': vcs.canvas.renameGSp(self.entry_str, new_gm_name)
                     elif gm_type == 'taylordiagrams': vcs.canvas.renameGtd(self.entry_str, new_gm_name)
                     elif gm_type == 'vector': vcs.canvas.renameGv(self.entry_str, new_gm_name)
                     elif gm_type == 'xvsy': vcs.canvas.renameGXY(self.entry_str, new_gm_name)
                     elif gm_type == 'xyvsy': vcs.canvas.renameGXy(self.entry_str, new_gm_name)
                     elif gm_type == 'yxvsx': vcs.canvas.renameGYx(self.entry_str, new_gm_name)
                  elif self.type == 'gm_copy':
                     if gm_type == 'boxfill': vcs.canvas.copyGfb( self.entry_str, new_gm_name)
                     elif gm_type == 'continents': vcs.canvas.copyGcon( self.entry_str, new_gm_name)
                     elif gm_type == 'isofill': vcs.canvas.copyGfi( self.entry_str, new_gm_name)
                     elif gm_type == 'isoline': vcs.canvas.copyGfi( self.entry_str, new_gm_name)
                     elif gm_type == 'outfill': vcs.canvas.copyGfo( self.entry_str, new_gm_name)
                     elif gm_type == 'outline': vcs.canvas.copyGo( self.entry_str, new_gm_name)
                     elif gm_type == 'scatter': vcs.canvas.copyGSp( self.entry_str, new_gm_name)
                     elif gm_type == 'taylordiagrams': vcs.canvas.copyGtd( self.entry_str, new_gm_name)
                     elif gm_type == 'vector': vcs.canvas.copyGv( self.entry_str, new_gm_name)
                     elif gm_type == 'xvsy': vcs.canvas.copyGXY( self.entry_str, new_gm_name)
                     elif gm_type == 'xyvsy': vcs.canvas.copyGXy( self.entry_str, new_gm_name)
                     elif gm_type == 'yxvsx': vcs.canvas.copyGYx( self.entry_str, new_gm_name)
                  elif self.type == 'gm_save_as':
                     if new_gm_name[-4:] != ".scr": new_gm_name += ".scr"
                     if gm_type == 'boxfill': vcs.canvas.scriptGfb( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'continents': vcs.canvas.scriptGcon( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'isofill': vcs.canvas.scriptGfi( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'isoline': vcs.canvas.scriptGi( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'outfill': vcs.canvas.scriptGfo( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'outline': vcs.canvas.scriptGo( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'scatter': vcs.canvas.scriptGSp( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'taylordiagrams': vcs.canvas.scriptGtd( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'vector': vcs.canvas.scriptGv( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'xvsy': vcs.canvas.scriptXY( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'xyvsy': vcs.canvas.scriptXy( self.entry_str, new_gm_name, 'w' )
                     elif gm_type == 'yxvsx': vcs.canvas.scriptYx( self.entry_str, new_gm_name, 'w' )
                  self.update_widget.setlist( vcs.listelements(gm_type) )
               except:
                  gui_message.error( 'Cannot rename %s graphics method (%s). Check to make sure that the old name exist in the %s list and that the new name does not.' % (self.gm_type, self.entry_str, self.gm_type) )
         
      self.dialog.destroy()

#---------------------------------------------------------------------
#
# End output popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
