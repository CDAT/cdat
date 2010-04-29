#!/usr/bin/env python
#
# The PCMDI Data Browser Plot Annotation -  gui_annotate module
#
###############################################################################
#                                                                             #
# Module:       gui_annotate module                                           #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                      		      #
#               Lawrence Livermore National Laboratory:                       #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser VCS plot annotation.            #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import sys, string
import __main__
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import vcs_function
import gui_set_text_object

class EntryButton(Tkinter.Button):
   def __init__(self, master=None, cnf={}, **kw):
      self.__toggle = 0
      kw['background'] = gui_color.eight
      kw['activebackground'] = gui_color.nine
      apply(Tkinter.Button.__init__, (self, master, cnf), kw)

#---------------------------------------------------------------------
#
# Get the annotation infomation from the data variable
#
#---------------------------------------------------------------------------
def get_annotation_info( parent, data_name=None ):
   for i in parent.pl.form.keys():
       data1 = parent.pl.form[i].data1.get()
       try:
          if data_name == data1:
             #if (parent.pl.form[i].display.off == 0) and (data1 != ""):
             if (data1 != ""):
                attribute_list = __main__.__dict__[ data1 ].listall()
                found_source = 0
                found_title = 0
                for i in range(len(attribute_list)):
                   if string.lower(attribute_list[i][:6]) == 'source':
                      found_source = 1
                      try: parent.annotate_source =  __main__.__dict__[ data1 ].getattribute(attribute_list[i][:6])
                      except: pass
                   if string.lower(attribute_list[i][:9]) == 'long_name':
                      found_title = 1
                      try: parent.annotate_title =  __main__.__dict__[ data1 ].getattribute(attribute_list[i][:9])
                      except: pass
                   if string.lower(attribute_list[i][:5]) == 'title':
                      found_title = 1
                      try: parent.annotate_title =  __main__.__dict__[ data1 ].getattribute(attribute_list[i][:5])
                      except: pass
                if found_source == 0: parent.annotate_source = ''
                if found_title == 0: parent.annotate_title = ''

                parent.annotate_xlabel = None; parent.annotate_ylabel = None
                parent.annotate_legendlabel = None
   
                # Get file information
                try: var =parent.panelDM.var3
                except: var = None

                # Get the ordered dimension names from the VCDAT GUI
# 
#      The annotate x and y labels are now set in the vcs_function.py file. 
#      This way we can make sure that the dimension names are correct.
#
#                d = {}
#                for ii in range( parent.panelDM.ndim ): d[parent.panelDM.dim[ii].dim_name] = ii
#                gl = []
#                for ii in range( parent.panelDM.ndim ):
#                   for jj in range( parent.panelDM.ndim ):
#                      if (d[parent.panelDM.dim[jj].dim_name] == ii):
#                         gl.append(parent.panelDM.dim[jj].dim_name)
#                         break
   
                if var is None:
                   try: parent.annotate_name =  __main__.__dict__[ data1 ].id
                   except: pass
                   try:  parent.annotate_units =  __main__.__dict__[ data1 ].units
                   except: pass
#                   try: parent.hannotate_xlabel =  gl[len(__main__.__dict__[ data1 ].shape)-1]
#                   except: pass
#                   try: parent.hannotate_ylabel =  gl[len(__main__.__dict__[ data1 ].shape)-2]
#                   except: pass
                else:
                   g = parent.panelDM.fid2
                   try: parent.annotate_name =  g[var].id
                   except: pass
                   try: parent.annotate_units = g[var].units
                   except: pass
#                   try: parent.hannotate_xlabel = gl[len(g[var].shape)-1]
#                   except: pass
#                   try: parent.hannotate_ylabel = gl[len(g[var].shape)-2]
#                   except: pass
       except: pass

#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS Plot Annotation Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent):
        if parent.annotate_start_over == 1: parent.panelGC.evt_plot( parent )

        if (parent.panelDM.var3 is None) and (len(parent.panelDV.selected) == 0): return

        self.parent = parent
        self.preview_flg = 0
        self.dialog = Pmw.Dialog( parent,
            title = 'Annotate VCS Plot',
            buttons = ('Preview', 'Reset to\nInitial\nState', 'Use\nVariable\nDefaults', 'Apply\nCurrent\nSettings', 'Cancel'),
            defaultbutton = 'Apply\nCurrent\nSettings',
            command = gui_control.Command(self.execute, parent) )

        if parent.menu.popup_window_settings_flg == 1:
           self.dialog.transient( self.parent ) # Keep widget on top of its parent
        self.dialog.withdraw()                  # this is too slow

        # Save the incoming settings, just in case the user cancels.
        self.hold_annotate_cancel_settings( parent )

        # Get information from the data and the graphics methods
#        if self.parent.annotate_get_info == 1:
#           get_annotation_info( parent, self.parent.annotate_var_name )
        self.parent.annotate_view = 1

        # Setup group1 (Master annotations control)
        group1 = Pmw.Group(self.dialog.interior(),
                            tag_text = 'Master control',
                            tagindent = 10,
                            tag_font = ('times',14,'bold'))

        self.retain = Pmw.RadioSelect(group1.interior(),
                            labelpos = 'w',
                            label_text = 'Retain Annotation Settings:',
                            labelmargin = 20,
                            buttontype = 'radiobutton',
                            command = gui_control.Command(self.retain_switch, parent)
                            )
        self.parent.balloon.bind(self.retain, "Select whether or not to retain annotation settings for all variables selected.")

        self.retain.add('no')
        self.retain.add('yes')
        self.retain.invoke( self.parent.annotate_retain )
        self.retain.pack(side = 'top', expand = 1, fill = 'x',padx = 10,pady = 0)
        group1.pack(side = 'top', expand = 1, fill = 'x',padx = 10,pady = 10)

        self.master = Pmw.RadioSelect(group1.interior(),
                            labelpos = 'w',
                            label_text = 'Annotation Status:',
                            labelmargin = 75,
                            buttontype = 'radiobutton',
                            command = gui_control.Command(self.master_switch, parent) )
        self.parent.balloon.bind(self.master, "Toggle 'All Annotation' on or off.")

        self.master.add('on')
        self.master.add('off')
        self.master.invoke( self.parent.annotate_status )
        self.master.pack(side = 'left', expand = 1, fill = 'x',padx = 10,pady = 0)
        group1.pack(side = 'top', expand = 1, fill = 'x',padx = 10,pady = 10)

        # Setup group2 (Change plot labels)
        group2 = Pmw.Group(self.dialog.interior(),
                            tag_text = 'Change labels',
                            tagindent = 10,
                            tag_font = ('times',14,'bold'))
        tempframe = Tkinter.Frame(group2.interior())
        self.eny1 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'Source:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black')
        self.eny1.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        parent.balloon.bind(self.eny1,"Enter the 'Source' text to be displayed on the VCS Canvas")
        if self.parent.annotate_source is not None:
           self.eny1.setentry( self.parent.annotate_source )
        btn1 = Tkinter.Button(tempframe,
                                text = 'Properties',
                                command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'source', None))
        btn1.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x', pady = 1)

        tempframe = Tkinter.Frame(group2.interior())
        self.eny2 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'Name:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black',
                                    command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'dataname'))
        self.eny2.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        self.parent.balloon.bind(self.eny2, "Enter the 'Name' text to be displayed on the VCS Canvas")
        if self.parent.annotate_name is not None:
           self.eny2.setentry( self.parent.annotate_name )
        elif ((parent.annotate_name is None) or (parent.annotate_name == "QuickPlot")):
           try: self.eny2.setentry( parent.QuickPlot_name[ 'QuickPlot' ] )
           except: pass
        btn2 = Tkinter.Button(tempframe,
                                text = 'Properties',
                                command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'dataname', None))
        btn2.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x', pady = 1)

        tempframe = Tkinter.Frame(group2.interior())
        self.eny3 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'Title:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black')
        self.eny3.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        parent.balloon.bind(self.eny3,"Enter the 'Title' text to be displayed on the VCS Canvas")
        if self.parent.annotate_title is not None:
           self.eny3.setentry( self.parent.annotate_title )
        btn3 = Tkinter.Button(tempframe,
                                text = 'Properties',
                                command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'title', None))
        btn3.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x', pady = 1)

        tempframe = Tkinter.Frame(group2.interior())
        self.eny4 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'Units:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black')
        self.eny4.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        parent.balloon.bind(self.eny4,"Enter the 'Units' text to be displayed on the VCS Canvas")
        if self.parent.annotate_units is not None:
           self.eny4.setentry( self.parent.annotate_units )
        btn4 = Tkinter.Button(tempframe,
                                text = 'Properties',
                                command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'units', None))
        btn4.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x', pady = 1)

        tempframe = Tkinter.Frame(group2.interior())
        self.eny5 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'X-label:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black')
        self.eny5.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        gm_type = parent.panelGC.text_opt2.get()
        if gm_type in ['Xyvsy']:
            self.eny5.configure( label_state = 'disabled', entry_state = 'disabled')
            parent.balloon.bind(self.eny5,"'X-label' disabled for this graphic method, use Name instead")
        else:       
            parent.balloon.bind(self.eny5,"Enter the 'X-label' name to be displayed on the VCS Canvas")
        if self.parent.annotate_xlabel is not None:
           self.eny5.setentry( self.parent.annotate_xlabel )
        else:
           try: self.eny5.setentry( self.parent.hannotate_xlabel )
           except: pass
        self.store_annotate_xlabel = parent.annotate_xlabel
        btn5 = Tkinter.Button(tempframe,
                                text = 'Properties',
                                command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'xname', None))
        btn5.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x', pady = 1)

        tempframe = Tkinter.Frame(group2.interior())
        self.eny6 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'Y-label:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black')
        self.eny6.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        
        if gm_type in ['Yxvsx']:
            self.eny6.configure( label_state = 'disabled', entry_state = 'disabled')
            parent.balloon.bind(self.eny6,"'Y-label' disabled for this graphic method, use Name instead")
        else:
            parent.balloon.bind(self.eny6,"Enter the 'Y-label' name to be displayed on the VCS Canvas")
            
        if self.parent.annotate_ylabel is not None:
           self.eny6.setentry( self.parent.annotate_ylabel )
        else:
           try: self.eny6.setentry( self.parent.hannotate_ylabel )
           except: pass
        self.store_annotate_ylabel = parent.annotate_ylabel
        btn6 = Tkinter.Button(tempframe,
                                text = 'Properties',
                                command = gui_control.Command(gui_set_text_object.create, self.dialog, parent, parent.template_name, 'yname', None))
        btn6.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x',pady = 1)

        tempframe = Tkinter.Frame(group2.interior())
        self.eny7 = Pmw.EntryField(tempframe,
                                    labelpos = 'w',
                                    label_text = 'Legend:',
                                    entry_width = 40,
                                    entry_background = 'white',
                                    entry_foreground = 'black')
        self.eny7.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        parent.balloon.bind(self.eny7,"Enter the 'Legend' text to be displayed on the VCS Canvas")
        if self.parent.annotate_legendlabel is not None:
           self.eny7.setentry( self.parent.annotate_legendlabel )
        #btn7 = Tkinter.Button(tempframe,
        #                        text = 'Properties')
        #btn7.pack(side = 'left', expand = 1, fill = 'x', padx = 10)
        tempframe.pack(side = 'top', expand = 1, fill = 'x', pady = 1)

        labels = (self.eny1, self.eny2, self.eny3, self.eny4, self.eny5, self.eny6, self.eny7)
        Pmw.alignlabels(labels)
        group2.pack(side = 'top',expand = 1, fill = 'x',padx = 10, pady = 10)

        # Setup group3 (Turn labels on/off)
        group3 = Pmw.Group(self.dialog.interior(),
                            tag_text = 'Turn labels on/off',
                            tagindent = 10,
                            tag_font = ('times',14,'bold'))
        tempframe = Tkinter.Frame(group3.interior())
        self.chkbtn1 = Tkinter.Checkbutton(tempframe,
                            text = 'Date',
                            selectcolor=gui_color.one,
                            command = gui_control.Command(self.evt_set_toggle_state, parent,0) 
                           )
        self.chkbtn1.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
        self.chkbtn2 = Tkinter.Checkbutton(tempframe,
                            text = 'Time',
                            selectcolor=gui_color.one,
                            command = gui_control.Command(self.evt_set_toggle_state, parent,1) 
                           )
        self.chkbtn2.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
        self.chkbtn3 = Tkinter.Checkbutton(tempframe,
                            text = 'Mean',
                            selectcolor=gui_color.one,
                            command = gui_control.Command(self.evt_set_toggle_state, parent,2) 
                           )
        self.chkbtn3.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
        tempframe.pack(side = 'left', expand = 1, fill = 'x')

        tempframe = Tkinter.Frame(group3.interior())
        self.chkbtn4 = Tkinter.Checkbutton(tempframe,
                            text = 'Max',
                            selectcolor=gui_color.one,
                            command = gui_control.Command(self.evt_set_toggle_state, parent,3) 
                           )
        self.chkbtn4.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
        self.chkbtn5 = Tkinter.Checkbutton(tempframe,
                            text = 'Min',
                            selectcolor=gui_color.one,
                            command = gui_control.Command(self.evt_set_toggle_state, parent,4) 
                           )
        self.chkbtn5.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
        self.chkbtn6 = Tkinter.Checkbutton(tempframe,
                            text = 'Multiplier',
                            selectcolor=gui_color.one,
                            command = gui_control.Command(self.evt_set_toggle_state, parent,5) 
                           )
        self.chkbtn6.pack(side = 'top',anchor = 'nw', expand = 1, padx = 10,pady=1)
        tempframe.pack(side = 'left', expand = 1, fill = 'x')
        group3.pack(side = 'top', expand = 1, fill = 'x', padx = 10, pady = 10)

        if (self.parent.toggle_state[0]): self.chkbtn1.select()
        if (self.parent.toggle_state[1]): self.chkbtn2.select()
        if (self.parent.toggle_state[2]): self.chkbtn3.select()
        if (self.parent.toggle_state[3]): self.chkbtn4.select()
        if (self.parent.toggle_state[4]): self.chkbtn5.select()
        if (self.parent.toggle_state[5]): self.chkbtn6.select()

        # Hold the values of the original settings. This is needed for Reset and Cancel
        self.hold_annotate_original_settings( parent )

        # Position dialog popup on top of the main GUI
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )

#        self.dialog.activate(globalMode = 1, geometry= "+%d+%d" % (d1, d2) )
        self.dialog.geometry( "+%d+%d" % (d1, d2) )
        self.dialog.show()

   def evt_set_toggle_state(self, parent, id):
      if parent.toggle_state[id] == 0:
         parent.toggle_state[id] = 1
      else:
         parent.toggle_state[id] = 0

   def retain_switch(self, parent, result):
      self.parent.annotate_retain = result

   def master_switch(self, parent, result):
      if result == 'off':
        try:
          self.eny5.setentry( ' ' )     # Remove the X and Y axis labels on the plot
          self.eny6.setentry( ' ' )
          self.store_annotate_xlabel = parent.annotate_xlabel
          self.store_annotate_ylabel = parent.annotate_ylabel
        except:
           pass
      else:
        try:
           if self.store_annotate_xlabel is not None:
              self.eny5.setentry( self.store_annotate_xlabel )# Restore the X and Y axis labels
           else:
              self.eny5.setentry( '' )
           if self.store_annotate_ylabel is not None:
              self.eny6.setentry( self.store_annotate_ylabel )
           else:
              self.eny6.setentry( '' )
           parent.annotate_xlabel = self.store_annotate_xlabel
           parent.annotate_ylabel = self.store_annotate_ylabel
        except:
           pass

   def get_settings( self, parent ):
      if self.eny1.get( ) == '':
          parent.annotate_source = " "
      else:
          self.parent.annotate_source=self.eny1.get( )
      if self.eny2.get( ) == '':
           parent.annotate_name = " "
      else:
           parent.annotate_name=self.eny2.get( )
      if self.eny3.get( ) == '':
           parent.annotate_title = " "
      else:
           parent.annotate_title=self.eny3.get( )
      if self.eny4.get( ) == '':
           parent.annotate_units = " "
      else:
          parent.annotate_units=self.eny4.get( )
      if self.eny5.get( ) == '':
           parent.annotate_xlabel = " "
      else:
         parent.annotate_xlabel=self.eny5.get( )
      if self.eny6.get( ) == '':
           parent.annotate_ylabel = " "
      else:
          parent.annotate_ylabel=self.eny6.get( )
      if self.eny7.get( ) == '':
           parent.annotate_legendlabel = ""
      else:
          parent.annotate_legendlabel=self.eny7.get( )

      # Handle checkbutton selections
      parent.annotate_date = self.parent.toggle_state[0]
      parent.annotate_time = self.parent.toggle_state[1]
      parent.annotate_mean = self.parent.toggle_state[2]
      parent.annotate_max = self.parent.toggle_state[3]
      parent.annotate_min = self.parent.toggle_state[4]
      parent.annotate_multiplier = self.parent.toggle_state[5]

      # Master control to toggle everything off or on
      parent.annotate_status = self.master.getcurselection()

   def annotate_replot( self, parent ):
      self.get_settings(parent)
      parent.panelGC.evt_plot( parent )
#      vcs_function.re_plot( parent, 0 )

   def annotate_clear( self, parent ):
      self.master.invoke( 'on' )
      parent.annotate_source = None
      parent.annotate_name = None
      parent.annotate_title = None
      parent.annotate_units = None
      parent.annotate_xlabel = None
      parent.annotate_ylabel = None
      parent.annotate_legendlabel = None
#      self.chkbtn1.select()
#      self.chkbtn2.select()
#      self.chkbtn3.select()
#      self.chkbtn4.select()
#      self.chkbtn5.select()
#      self.chkbtn6.select()

      # Plot the settings
      parent.panelGC.evt_plot( parent )

   def annotate_cancel( self, parent ):
      self.parent.annotate_retain = self.cancel_annotate_retain
      self.retain.invoke( self.parent.annotate_retain )
      self.parent.annotate_status = self.cancel_annotate_status
      self.master.invoke( self.parent.annotate_status )

      parent.annotate_source = self.cancel_annotate_source
      parent.annotate_name = self.cancel_annotate_name
      parent.annotate_title = self.cancel_annotate_title
      parent.annotate_units = self.cancel_annotate_units
      parent.annotate_xlabel = self.cancel_annotate_xlabel
      parent.annotate_ylabel = self.cancel_annotate_ylabel
      parent.annotate_legendlabel = self.cancel_annotate_legendlabel

      self.parent.toggle_state = list( self.cancel_toggle_state )

      self.chkbtn1.deselect();self.chkbtn2.deselect();self.chkbtn3.deselect()
      self.chkbtn4.deselect();self.chkbtn5.deselect();self.chkbtn6.deselect()
      if (self.parent.toggle_state[0]): self.chkbtn1.select()
      if (self.parent.toggle_state[1]): self.chkbtn2.select()
      if (self.parent.toggle_state[2]): self.chkbtn3.select()
      if (self.parent.toggle_state[3]): self.chkbtn4.select()
      if (self.parent.toggle_state[4]): self.chkbtn5.select()
      if (self.parent.toggle_state[5]): self.chkbtn6.select()

      # Plot the settings
      parent.panelGC.evt_plot( parent )

   def annotate_reset( self, parent ):
      self.parent.annotate_retain = self.stored_annotate_retain
      self.retain.invoke( self.parent.annotate_retain )
      self.parent.annotate_status = self.stored_annotate_status
      self.master.invoke( self.parent.annotate_status )

      if self.stored_annotate_source is not None:
         self.parent.annotate_source = self.stored_annotate_source
         self.eny1.setentry( self.stored_annotate_source )
      self.eny2.setentry( self.parent.annotate_var_name )
      if self.stored_annotate_name is not None:
         self.parent.annotate_name = self.stored_annotate_name
      if self.stored_annotate_title is not None:
         self.parent.annotate_title = self.stored_annotate_title
         self.eny3.setentry( self.stored_annotate_title )
      if self.stored_annotate_units is not None:
         self.parent.annotate_units = self.stored_annotate_units
         self.eny4.setentry( self.stored_annotate_units )
      if self.stored_annotate_xlabel is not None:
         self.parent.annotate_xlabel = self.stored_annotate_xlabel
         self.eny5.setentry( self.stored_annotate_xlabel )
      elif self.parent.hannotate_xlabel is not None:
         self.eny5.setentry( self.parent.hannotate_xlabel )
      if self.stored_annotate_ylabel is not None:
         self.parent.annotate_ylabel = self.stored_annotate_ylabel
         self.eny6.setentry( self.stored_annotate_ylabel )
      else:
         try: self.eny6.setentry( self.parent.hannotate_ylabel )
         except: pass
      self.parent.annotate_legendlabel = self.stored_annotate_legendlabel
      if self.parent.annotate_legendlabel is not None:
         self.eny7.setentry( self.stored_annotate_legendlabel )
      else:
         self.eny7.setentry( '' )

      self.parent.toggle_state = list( self.stored_toggle_state )

      self.chkbtn1.deselect();self.chkbtn2.deselect();self.chkbtn3.deselect()
      self.chkbtn4.deselect();self.chkbtn5.deselect();self.chkbtn6.deselect()
      if (self.parent.toggle_state[0]): self.chkbtn1.select()
      if (self.parent.toggle_state[1]): self.chkbtn2.select()
      if (self.parent.toggle_state[2]): self.chkbtn3.select()
      if (self.parent.toggle_state[3]): self.chkbtn4.select()
      if (self.parent.toggle_state[4]): self.chkbtn5.select()
      if (self.parent.toggle_state[5]): self.chkbtn6.select()

      # Plot the settings
      self.annotate_replot(parent)

   def hold_annotate_cancel_settings(self, parent ):
      # Store current values
      self.cancel_annotate_retain = self.parent.annotate_retain
      self.cancel_annotate_status = self.parent.annotate_status
      self.cancel_annotate_source = self.parent.annotate_source
      self.cancel_annotate_name = self.parent.annotate_name
      self.cancel_annotate_title = self.parent.annotate_title
      self.cancel_annotate_units = self.parent.annotate_units
      self.cancel_annotate_xlabel = self.parent.annotate_xlabel
      self.cancel_annotate_ylabel = self.parent.annotate_ylabel
      self.cancel_annotate_legendlabel = self.parent.annotate_legendlabel
      self.cancel_toggle_state = list( self.parent.toggle_state )

   def hold_annotate_original_settings(self, parent ):
      # Store current values
      self.stored_annotate_retain = self.parent.annotate_retain
      self.stored_annotate_status = self.parent.annotate_status
      self.stored_annotate_source = self.parent.annotate_source
      self.stored_annotate_name = self.parent.annotate_name
      self.stored_annotate_title = self.parent.annotate_title
      self.stored_annotate_units = self.parent.annotate_units
      self.stored_annotate_xlabel = self.parent.annotate_xlabel
      self.stored_annotate_ylabel = self.parent.annotate_ylabel
      self.stored_annotate_legendlabel = self.parent.annotate_legendlabel
      self.stored_toggle_state = list( self.parent.toggle_state )

   def execute(self, parent, result):
      if result == 'Preview':
          self.preview_flg = 1
          self.annotate_replot( parent )
      if result == 'Reset to\nInitial\nState':
          self.preview_flg = 0
          self.annotate_reset( parent )
          parent.annotate_xlabel = None; parent.annotate_ylabel = None
      if result == 'Use\nVariable\nDefaults':
          self.preview_flg = 0
          #parent.annotate_xlabel = None; parent.annotate_ylabel = None
          self.parent.annotate_get_info = 1 # The Next time GUI comes up, enter data infomation
          self.parent.annotate_retain = 'no' # Use variable information for annotation
          self.parent.annotate_view = 0
          self.annotate_clear( parent )
          self.dialog.destroy()
      if result == 'Apply\nCurrent\nSettings':
          self.preview_flg = 0
          self.parent.annotate_var_name = self.eny2.get( )
          self.annotate_replot( parent ) 
          self.parent.annotate_get_info = 0 # The Next time GUI comes up, show previous settings
          self.parent.annotate_view = 1
          self.dialog.destroy()
      if (result == 'Cancel') or (result == None):
          if self.preview_flg: self.annotate_cancel( parent )
          self.parent.annotate_view = 0
          self.dialog.destroy()

#---------------------------------------------------------------------
#
# End VCS annotation popup dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
if __name__ == '__main__':
    root = Tkinter.Tk()
    Pmw.initialise(root)
    root.balloon = Pmw.Balloon()
    root.option_add('*Font',('helvetica',12))
    create(root)

