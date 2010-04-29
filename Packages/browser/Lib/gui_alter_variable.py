#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Alter Varialbe Attributes -  gui_alter_variable module
#
#################################################################################
#                                                                               #
# Module:       gui_alter_variable module                                       #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser alter variable attributes popup.  #
#                                                                               #
# Version:      3.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import os, sys, string
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import gui_message
import gui_defined_variables

#---------------------------------------------------------------------
#
# Start of Popup Dialogs
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------------
# Modify Variable Attribute Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__( self, parent ):
        if len(parent.panelDV.selected) == 0:
           gui_message.error('No variable selected in the "Defined Variable" list window.')
           return
        parent.slab=parent.panelDV.lst1[ parent.panelDV.selected ]
        var_name = string.split(parent.panelDV.selected)[0]
        str_title = ('Alter Variable [ %s ] Attributes ' % var_name)
        self.dialog = Pmw.Dialog( parent,
            title = str_title,
            buttons = ('Dismiss',),
#            defaultbutton = 'Dismiss',
            command = self.execute )

        self.dialog.transient( parent ) # draw widget on top of its parent

        fm0=Tkinter.Frame(self.dialog.interior())
        self.lbl0_text = Tkinter.StringVar()
        self.lbl0_text.set( ("Change the variable's name from [ %s ] to [ ? ]:" % var_name) )
        self.lbl0=Tkinter.Label(fm0,
            textvariable = self.lbl0_text,
            justify = 'left',
            anchor = 'w',
            )
        self.lbl0.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny0=Pmw.EntryField(fm0,
            labelpos = 'w',
            label_text = 'Rename Variable:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  30,
            )
        self.eny0.pack( expand=1, fill = 'both', padx=20, pady=5 )
        self.eny0.component('entry').bind( "<Return>", gui_control.Command(self.evt_getfocus1, parent) )
        #self.eny0.pack( side='left', fill = 'both', padx=20, pady=5 )

        self.btn0 = Tkinter.Button( fm0,
            text="Apply",
            background = gui_color.one,
            command = gui_control.Command(self.rename_variable_apply, parent)
            )
        self.btn0.pack( expand = 1, fill = 'both', padx=200, pady=5 )
        fm0.pack( side='top', fill='both', expand=1 )

        fm=Tkinter.Frame(self.dialog.interior())
        self.lbl1_text = Tkinter.StringVar()
        self.lbl1_text.set( ('Change or add variable attribute to [ %s ]:' % var_name) )
        self.lbl1=Tkinter.Label(fm,
            textvariable = self.lbl1_text,
            justify = 'left',
            anchor = 'w',
            )
        self.lbl1.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny1=Pmw.EntryField(fm,
            labelpos = 'w',
            label_text = 'Attribute Name:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  30,
            )
        self.eny1.pack( side='top', fill = 'both', padx=20, pady=5 )

        fm.pack( side='top', fill='both', expand=1 )

        self.fm2=Tkinter.Frame(self.dialog.interior())
        fm2=Tkinter.Frame( self.fm2 )
        fm2.pack( side='top', fill='both', expand=1 )
        self.eny2=Pmw.EntryField( fm2,
            labelpos = 'w',
            label_text = "Attribute's Values:",
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  30,
            )
        self.eny2.pack( side='left', expand = 1, fill = 'both', padx=20, pady=5 )
        self.eny2.component('entry').bind( "<Return>", gui_control.Command(self.evt_getfocus2, parent) )

        chlst = [ "str  String value",
                "int  Integer value",
                "flt  Float value" ]
	self.omu = Pmw.OptionMenu( fm2,
		items = chlst,
		menubutton_width = 3,
                menubutton_anchor = 'w',
                command = self.set_right
	        )
        self.omu.configure( menubutton_text=chlst[0][0:3] )
	self.omu.pack( side='left', fill = 'both', after=self.eny2 )

        self.btn1 = Tkinter.Button( self.fm2,
            text="Apply",
            background = gui_color.one,
            command = gui_control.Command(self.attribute_apply, parent)
            )
        self.btn1.pack( expand = 1, fill = 'both', padx=200, pady=5 )
        self.fm2.pack( side='top', fill='both', expand=1 )

        self.fm3=Tkinter.Frame(self.dialog.interior())
        self.lbl2_text = Tkinter.StringVar()
        self.lbl2_text.set( ('Remove variable attributes from [ %s ]:' % var_name) )
        self.lbl2=Tkinter.Label(self.fm3,
            textvariable = self.lbl2_text,
            justify = 'left',
            anchor = 'w',
            )
        self.lbl2.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny3=Pmw.EntryField(self.fm3,
            labelpos = 'w',
            label_text = 'Attribute Name:',
            entry_background = 'red',
            entry_foreground = 'black',
            entry_width =  30,
            )
        self.eny3.pack( expand=1, fill = 'both', padx=20, pady=5 )
        self.eny3.component('entry').bind( "<Return>", gui_control.Command(self.evt_getfocus3, parent) )

        self.btn2 = Tkinter.Button( self.fm3,
            text="Apply",
            background = gui_color.one,
            command = gui_control.Command(self.remove_apply, parent)
            )
        self.btn2.pack( expand = 1, fill = 'both', padx=200, pady=5 )
        self.fm3.pack( side='top', fill='both', expand=1 )

        self.lbl3_text = Tkinter.StringVar()
        if len(parent.slab.shape) > 0:
         self.fm4=Tkinter.Frame(self.dialog.interior())
         self.ctr = Pmw.Counter( self.fm4,
	    labelpos = 'w',
	    label_text = 'Set Dimension ',
	    entryfield_value = '1',
	    #datatype = {'counter' : 'integer'},
            entry_background = 'white',
            entry_foreground = 'black',
            entryfield_entry_width = 4,
            entryfield_entry_justify = 'center',
	    entryfield_validate = {'validator' : 'integer',
	                           'min' : '1', 'max' : str(len(parent.slab.shape))},
 	    increment = 1
           )
         self.ctr.pack(side='left',  padx=10, pady=5 )
       
         self.lbl3_text.set( ('attribute for [ %s ]:' % var_name) )
         lbl3=Tkinter.Label(self.fm4,
            textvariable = self.lbl3_text,
            justify = 'left',
            anchor = 'w',
            )
         lbl3.pack( expand = 1, fill = 'both', padx=0, pady=5 )
         self.fm4.pack( side='top', fill='both', expand=1 )

         fm5=Tkinter.Frame(self.dialog.interior())
         self.eny4=Pmw.EntryField(fm5,
            labelpos = 'w',
            label_text = 'Set Dimension Units:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  30,
            )
         self.eny4.pack( expand=1, fill = 'both', padx=20, pady=5 )
         self.eny4.component('entry').bind( "<Return>", gui_control.Command(self.evt_getfocus4, parent) )

         btn3 = Tkinter.Button( fm5,
            text="Apply",
            background = gui_color.one,
            command = gui_control.Command(self.dimension_apply, parent)
            )
         btn3.pack( expand = 1, fill = 'both', padx=200, pady=5 )
         fm5.pack( side='top', fill='both', expand=1 )
         entries = ( self.eny1, self.eny2, self.eny3, self.eny4 )
        else:
         entries = ( self.eny1, self.eny2, self.eny3 )
        Pmw.alignlabels(entries)

        # Position dialog popup
        parent_geom = parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def set_right(self, event):
      self.omu.configure( menubutton_text=event[0:3] )
 
   def evt_getfocus1( self, parent, event ):
      self.rename_variable_apply( parent )

   def rename_variable_apply( self, parent ):
      import __main__
      if parent.panelDV.selected == []:
	gui_message.error("Must select a variable in the 'Defined Variable' window.")
        return
      old_name = var_name = string.split(parent.panelDV.selected)[0]
      new_name = self.eny0.get( )
      selected = parent.panelDV.selected
      selected_list = parent.panelDV.selected_list
      __main__.__dict__[ new_name ] = __main__.__dict__[ old_name ]
      if old_name != new_name:
         del __main__.__dict__[ old_name ]
       
      for x in selected_list.keys():
        if string.split(selected_list[x], ' ')[0] == old_name:
           break
      selected_list[x]=string.replace(selected_list[x], old_name, new_name)
      selected = selected_list[x]

      hold_selected_list =  parent.panelDV.selected_list
      hold_selected = parent.panelDV.selected
      gui_defined_variables.update_defined( parent )
      parent.panelDV.selected_list = hold_selected_list
      parent.panelDV.selected = hold_selected

      # Update the selection numbering scheme
      ct = 0
      for x in parent.panelDV.selected_list.values():
         ct += 1
         if ct < 10:
            parent.panelDV.number_lst1[ x ] = ("%s%d " % (gui_control.dvholder[0], ct)) + x
         elif ct < 100:
            parent.panelDV.number_lst1[ x ] = ("%d " % ct) + x
         else:
            gui_message.error('Cannot exceed over 100 selections. See gui_control.py for modification instructions.')

      # Update the defined variable list
      gui_defined_variables.update_defined_variable_list( parent )
      #
      dvlt =  parent.panelDV.scl1.get()
      for x in selected_list.values():
         for i in range( len( dvlt ) ):
            if dvlt[ i ][gui_control.dvsize:] == x:
               parent.panelDV.scl1.select_set( i )
      #
      parent.panelDV.selected_list = selected_list
      parent.panelDV.selected = selected
      #
      gui_functions._srl_var_dim1_to_ndim( parent, 
                    slab=parent.panelDV.lst1[ parent.panelDV.selected ] )
      #
      self.lbl0_text.set( ("Change the variable's name from [ %s ] to [ ? ]:" % new_name) )
      self.lbl1_text.set( ('Add variable attribute to [ %s ]:' % new_name) )
      self.lbl2_text.set( ('Remove variable attributes from [ %s ]:' % new_name) )
      self.lbl3_text.set( ('attribute for [ %s ]:' % new_name) )
      #
      parent.slab.id = parent.slab.name = new_name # change the name of the id to new_name
      #
      slab_list = parent.slab.listall()
      parent.panelVI.scl1.configure( text_state = 'normal' )
      parent.panelVI.scl1.settext( '' )
      first_time = 1
      for x in slab_list:
         if string.find( x, " Dimension " ) != -1:
            if first_time:
               ln=len(parent.panelDV.lst1[ parent.panelDV.selected ].shape)
               num_dim = 'Number of dimensions: %d' % ln
               parent.panelVI.scl1.insert( 'end',( num_dim + '\n') )
               first_time = 0
            parent.panelVI.scl1.insert( 'end', '\n' )
         parent.panelVI.scl1.insert( 'end', (x + '\n') )
      parent.panelVI.scl1.configure( text_state = 'disabled' )

      # record as a new variable
      gui_control.record_command(parent, "\n# Record the name change of %s to %s" % (old_name, new_name) , 1 )
      gui_control.record_command( parent, "%s = %s" % (new_name, old_name) , 1)

   def evt_getfocus2( self, parent, event ):
      self.attribute_apply( parent )

   def attribute_apply( self, parent ):
      import string
      text_1 = self.eny1.get( )
      text_2 = self.eny2.get( )
      opt_1  = self.omu.getcurselection( )
      if opt_1 == 'str':
         pass
      elif opt_1 == 'int':
         try:
            text_2 = string.atoi(text_2)
         except:
            gui_message.error('The attribute value [ %s ] is not an integer.' % text_2)
            return
      elif opt_1 == 'flt':
         try:
            text_2 = string.atof(text_2)
         except:
            gui_message.error('The attribute value [ %s ] is not a float.' % text_2)
            return
      try:
         parent.slab.createattribute(text_1, text_2)
      except:
         parent.slab.setattribute(text_1, text_2)
      # record as a new variable
      gui_control.record_command(parent, "\n# Record the change for attribute '%s' on %s to '%s'" % (text_1, parent.slab.id, text_2) , 1 )
      gui_control.record_command( parent, "%s.%s = %s" % (parent.slab.id, text_1, repr(text_2)) , 1)
      #
      slab_list = parent.slab.listall()
      parent.panelVI.scl1.configure( text_state = 'normal' )
      parent.panelVI.scl1.settext( '' )
      first_time = 1
      for x in slab_list:
         if string.find( x, " Dimension " ) != -1:
            if first_time:
               ln=len(parent.panelDV.lst1[ parent.panelDV.selected ].shape)
               num_dim = 'Number of dimensions: %d' % ln
               parent.panelVI.scl1.insert( 'end',( num_dim + '\n') )
               first_time = 0
            parent.panelVI.scl1.insert( 'end', '\n' )
         parent.panelVI.scl1.insert( 'end', (x + '\n') )
      parent.panelVI.scl1.configure( text_state = 'disabled' )
      #
#      for x in slab_list:
#         parent.panelVI.scl1.insert( 'end', (x + '\n') )

   def evt_getfocus3( self, parent, event ):
      self.remove_apply( parent )

   def remove_apply( self, parent ):
      text_3 = self.eny3.get( )
      try:
         delattr (parent.slab, text_3)
         #
         slab_list = parent.slab.listall()
         parent.panelVI.scl1.configure( text_state = 'normal' )
         parent.panelVI.scl1.settext( '' )
         first_time = 1
         for x in slab_list:
            if string.find( x, " Dimension " ) != -1:
               if first_time:
                  ln=len(parent.panelDV.lst1[parent.panelDV.selected ].shape)
                  num_dim = 'Number of dimensions: %d' % ln
                  parent.panelVI.scl1.insert( 'end',( num_dim + '\n') )
                  first_time = 0
               parent.panelVI.scl1.insert( 'end', '\n' )
            parent.panelVI.scl1.insert( 'end', (x + '\n') )
         parent.panelVI.scl1.configure( text_state = 'disabled' )
         #
#         for x in slab_list:
#            parent.panelVI.scl1.insert( 'end', (x + '\n') )
      except:
         gui_message.error('The attribute %s was not found.' % text_3)

      # record as a new variable
      gui_control.record_command(parent, "\n# Record the deletion of attribute '%s' on %s " % (text_3, parent.slab.id) , 1 )
      gui_control.record_command( parent, "del(%s.%s)" % (parent.slab.id, text_3) , 1)
      #
   def evt_getfocus4( self, parent, event ):
      self.dimension_apply( parent )

   def dimension_apply( self, parent ):
      text_4 = self.eny4.get( )
      parent.slab.setdimattribute((string.atoi(self.ctr.get( ))-1),'units', text_4)
      #
      slab_list = parent.slab.listall()
      parent.panelVI.scl1.configure( text_state = 'normal' )
      parent.panelVI.scl1.settext( '' )
      first_time = 1
      for x in slab_list:
         if string.find( x, " Dimension " ) != -1:
            if first_time:
               ln=len(parent.panelDV.lst1[ parent.panelDV.selected ].shape)
               num_dim = 'Number of dimensions: %d' % ln
               parent.panelVI.scl1.insert( 'end',( num_dim + '\n') )
               first_time = 0
            parent.panelVI.scl1.insert( 'end', '\n' )
         parent.panelVI.scl1.insert( 'end', (x + '\n') )
      parent.panelVI.scl1.configure( text_state = 'disabled' )
      #
#      for x in slab_list:
#         parent.panelVI.scl1.insert( 'end', (x + '\n') )
      # record as a new variable
      gui_control.record_command(parent, "\n# Record the change of units on dimension %i for %s to %s " % (string.atoi(self.ctr.get( ))-1, parent.slab.id, text_4) , 1 )
      gui_control.record_command( parent, "%s.setdimattribute(%i,'units','%s')" % (parent.slab.id, string.atoi(self.ctr.get( ))-1, text_4) , 1)
      #


   def execute( self, result ):
        self.dialog.destroy()
#
#---------------------------------------------------------------------
#
# End alter variable attribute popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
