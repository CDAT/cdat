#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Write netCDF File -  gui_writenetCDF module
#
###############################################################################
#                                                                             #
# Module:       gui_writenetCDF module                                        #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                             	      #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser write netCDF file popup.        #
#                                                                             #
# Version:      3.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import os, sys, cdms2
import gui_menu
import gui_functions
import gui_control
import gui_message
import tkFileDialog
import string

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
# Write Variable to NetCDF File Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent):
        if len(parent.panelDV.selected) == 0:
           gui_message.error('No variable selected in the "Defined Variable" list window.')
           return
        parent.slab=parent.panelDV.lst1[ parent.panelDV.selected ]
        var_name = ''
        for x in parent.panelDV.selected_list.values():
            var_name += "%s, " % (string.split(x,' ')[0])
        var_name = var_name[:-2]
        str_title = ('Write variable(s) [ %s ] to NetCDF File' % var_name)
        self.dialog = Pmw.Dialog( parent,
            title = str_title,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = gui_control.Command(self.execute,parent) )

        self.dialog.transient( parent ) # draw widget on top of its parent

        str_lbl1 = ('Save variable(s) [ %s ] to a netCDF file: ' % var_name)
        lbl1=Tkinter.Label(self.dialog.interior(),
            text = str_lbl1,
            justify = 'left',
            anchor = 'w',
            )
        lbl1.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        frame=Tkinter.Frame(self.dialog.interior())
        frame.pack(expand=1,fill='both',padx=10,pady=5)
        file=Tkinter.Label(frame,text='NetCDF File Name:')
        file.pack(side='left')
        self.canvas_openicon = Tkinter.Canvas(frame, bd=0, highlightthickness=0,
                                              width = 27, height = 27)
        self.canvas_openicon.pack( side='left')
        parent.balloon.bind( self.canvas_openicon, 
                 "Display 'File Select' browser for 'Directory' and 'File' selection." )
        self.img = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'open.gif') )
        self.canvas_openicon.create_image(0,0, anchor=Tkinter.NW, image=self.img )
        self.canvas_openicon.bind( '<1>', gui_control.Command( self.evt_icon_open_file, parent, None ))
        self.eny1=Pmw.EntryField(frame,
            labelpos = 'w',
            label_text = '',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  39,
            )
        self.eny1.pack( side='left',expand = 1, fill = 'both', padx=20, pady=5 )

	self.rdo=Pmw.RadioSelect(self.dialog.interior(),
	    buttontype = 'radiobutton',
	    orient = 'vertical',
	    labelpos = 'wn',
            label_text = 'File save mode:',
	    hull_borderwidth = 0,
	    hull_relief = 'ridge',
	    )
	self.rdo.pack(side = 'left', padx = 20, pady = 5)

	# Add some buttons to the radiobutton RadioSelect.
	self.rdo.add('[Replace] by overwriting netCDF file')
	self.rdo.add('[Append] to existing netCDF file')
	self.rdo.invoke('[Replace] by overwriting netCDF file')

        entries = (self.eny1, self.rdo)
        Pmw.alignlabels(entries)

        # Position dialog popup
        parent_geom = parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def evt_icon_open_file(self, parent, dirfilename=None, event=None):
        datatypes = [
            ("Search for NetCDF files", "*.nc *.netcdf"),
            ("All files", "*")
            ]
        
        dialog_icon = tkFileDialog.SaveAs(master=parent,
                                        filetypes=datatypes, title = 'Save File')
        dirfilename=dialog_icon.show(initialdir=os.getcwd())
        self.eny1.setentry(dirfilename)
        
   def execute(self, parent, result):
        import string
        if result == 'OK':
           text_1 = self.eny1.get( )
           name = parent.slab.getattribute('name')
           if string.find( text_1, '.nc' ) == -1: text_1 = text_1 + '.nc' 
           mode = 'w'
           if self.rdo.getcurselection()[1:2] == 'A': mode = 'a'
           for x in parent.panelDV.selected_list.values():
              try:
                 f=cdms2.open( text_1, mode )
                 f.write( parent.panelDV.lst1[ x  ] )
                 f.close()
              except:
                 gui_message.error('Could not save [ %s ] in netCDF file [ %s ].' % (name,text_1))
              mode = 'a'

        self.dialog.destroy()
#
#---------------------------------------------------------------------
#
# End write netCDF file popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
