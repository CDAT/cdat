#!/usr/bin/env python
#
# The PCMDI Data Browser Plot Secondary Object -  gui_set_text_object module
#
###############################################################################
#                                                                             #
# Module:       gui_set_text_object                                           #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                      		      #
#               Lawrence Livermore National Laboratory:                       #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser to set the seconday text object.#
#                                                                             #
# Version:      3.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import sys, string
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import vcs_function
import gui_annotate
import gui_message
import gui_set_text_table
import gui_set_text_orientation
import gui_support
from vcs import fonteditorgui
#---------------------------------------------------------------------
#
# Start of Popup Dialog
#
#---------------------------------------------------------------------------
# VCS Text Object Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, dialog, parent, template_name, template_member, event):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = "Set template %s's -- %s attributes"%(template_name,string.capwords(template_member)),
            buttons = ('OK','Apply', 'Dismiss'),
            defaultbutton = 'Dismiss',
            command = gui_control.Command(self.execute, parent, template_name, template_member) )

        self.dialog.transient( dialog ) # draw widget on top of its parent


        self.template_name = template_name
        self.template_member = template_member
        
        template = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
        template_member = template.__dict__[ template_member ]

        self.template_member_priority = template_member.priority
        self.template_member_x = template_member.x
        self.template_member_y = template_member.y
        self.template_member_texttable = template_member.texttable
        self.template_member_textorientation = template_member.textorientation
        
        self.priority = Pmw.Counter(self.dialog.interior(),
             labelpos = 'w',
             label_text = 'Priority',
             entry_background = 'white',
             entry_foreground = 'black',
             entry_width=5,
             entryfield_value = str ( template_member.priority ),
             entryfield_validate = {'validator':'integer','min':0,'minstrict':1},
             downarrow_state = 'disabled',
             uparrow_state = 'disabled'
            )
        self.priority.pack(fill='both', expand=1)
        if template_name == 'ASD':
           self.priority.configure(label_state = 'disabled', entry_state='disabled')
           self.priority.component( 'downarrow').bind( '<1>', "break")
           self.priority.component( 'uparrow').bind( '<1>', "break")
        else:
           self.priority.component('entry').unbind("<Return>")

        self.x=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'X',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  28,
            value = str( template_member.x ) 
            )
        self.x.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if self.parent.vcg[ parent.vcs_id ][0] is not None:
           self.x.setentry( self.parent.vcg[ parent.vcs_id ][0] )

        self.y=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            label_text = 'Y',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  28,
            value = str( template_member.y )
            )
        self.y.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        if self.parent.vcg[ parent.vcs_id ][1] is not None:
           self.y.setentry( self.parent.vcg[ parent.vcs_id ][1] )
        if template_name == 'ASD':
           self.y.configure(label_state = 'disabled', entry_state='disabled')

           
        if self.parent.vcg[ parent.vcs_id ][3] is not None:
           tentry=self.parent.vcg[ parent.vcs_id ][3]
        else:
           tentry=template_member.texttable
        if self.parent.vcg[ parent.vcs_id ][2] is not None:
           oentry=self.parent.vcg[ parent.vcs_id ][2]
        else:
           oentry=template_member.textorientation


        self.ft=Tkinter.Frame(self.dialog.interior())

        self.bpf=Tkinter.Button(self.ft,
                         font= ('helvetica',10),
                         bg=gui_support.gui_color.Tt_color,
                         text='Table',
                         )
        self.bpf.configure(command=gui_control.Command(fonteditorgui.FontGUI,self.parent.vcs[ parent.vcs_id ] ,self.parent,self.dialog,self.ft,tentry,oentry,self))
        self.bpf.pack(side='left')
        self.parent.balloon.bind(self.bpf,"Modify 'text table' attributes")

        self.properties_font=Pmw.ComboBox(self.ft,
            scrolledlist_items = self.parent.vcs[parent.vcs_id].listelements('texttable'),
##             labelpos = 'w',
##             label_pyclass = gui_annotate.EntryButton,
##             label_text = 'Properties',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  28,
            entryfield_value = tentry,
            selectioncommand=self.update,
            )
        self.properties_font.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.ft.pack()
        
        self.fo=Tkinter.Frame(self.dialog.interior())
        self.bpo=Tkinter.Button(self.fo,
                         font= ('helvetica',10),
                         bg=gui_support.gui_color.To_color,
                         text='Orientation',
                         )
        self.bpo.configure(command=gui_control.Command(fonteditorgui.FontGUI,self.parent.vcs[ parent.vcs_id ] ,self.parent,self.dialog,self.fo,tentry,oentry,self))
        
        self.properties_orientation=Pmw.ComboBox(self.fo,
            scrolledlist_items = self.parent.vcs[parent.vcs_id].listelements('textorientation'),
##             labelpos = 'w',
##             label_pyclass = gui_annotate.EntryButton,
##             label_text = 'Orientation',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  28,
            entryfield_value = oentry,
            selectioncommand=self.update,
            )
        self.bpo.pack(side='left')
        self.properties_orientation.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.fo.pack()
        self.parent.balloon.bind(self.bpo,"Modify 'text orientation' attributes")
##         self.properties_orientation.component('label').bind('<1>', gui_control.Command(self.evt_orientation_widget, self.parent))
        entries = ( self.priority, self.x, self.y, self.properties_orientation, self.properties_font )
        Pmw.alignlabels(entries)

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )

   def get_template_attribute_settings( self, parent,template_name,template_member ):

      template = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
      template_member = template.__dict__[ template_member ]

      try:
          template_member.priority = eval( self.priority.get() )
      except:
          template_member.priority = 0

      try:
         template_member.x = eval( self.x.get() )
      except:
         template_member.x = 0

      try:
         template_member.y = eval( self.y.get() )
      except:
         template_member.y = 0

      try:
         hold = template_member.texttable
         template_member.texttable = self.properties_font.get()
      except:
         template_member.texttable = 'default'
      tlist = parent.vcs[ parent.vcs_id ].listelements('texttable')
      if template_member.texttable not in (tlist):
          template_member.texttable = hold
      self.properties_font.setentry( template_member.texttable )

      try:
         hold = template_member.textorientation
         template_member.textorientation = self.properties_orientation.get()
      except:
         template_member.textorientation = 'default'
      tlist = parent.vcs[ parent.vcs_id ].listelements('textorientation')
      if template_member.textorientation not in (tlist):
          template_member.textorientation = hold
      self.properties_orientation.setentry( template_member.textorientation )

   def evt_properties_widget(self, parent, event):
      if self.properties_font.get() == 'default':
         gui_message.error('You cannot modify default text table objects!')
      else:
         gui_set_text_table.create(self.dialog, parent, self.properties_font.get() )

   def evt_orientation_widget(self, parent, event):
      if self.properties_orientation.get()[0:3] == 'def':
         gui_message.error('You cannot modify default text orientation objects!')
      else:
         gui_set_text_orientation.create(self.dialog, parent, self.properties_orientation.get() )

   def update(self,*args):
      self.execute(self.parent,self.template_name,self.template_member,'Apply')
      
   def execute(self, parent, template_name, template_member, result):
      if result in [ 'Apply', 'OK']:
         self.get_template_attribute_settings( parent,template_name,template_member )
      else:
         template = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
         template_member = template.__dict__[ template_member ]
         
         template_member.priority = self.template_member_priority
         template_member.x = self.template_member_x
         template_member.y = self.template_member_y
         template_member.texttable = self.template_member_texttable
         template_member.textorientation = self.template_member_textorientation
         

      parent.panelGC.evt_plot( parent )
      if result == 'Apply':
         template = parent.vcs[ parent.vcs_id ].gettemplate( template_name )
         template_member = template.__dict__[ template_member ]
         
         if self.parent.vcg[ parent.vcs_id ][3] is not None:
            tentry=self.parent.vcg[ parent.vcs_id ][3]
         else:
            tentry=template_member.texttable
         if self.parent.vcg[ parent.vcs_id ][2] is not None:
            oentry=self.parent.vcg[ parent.vcs_id ][2]
         else:
            oentry=template_member.textorientation
           
         self.bpo.configure(command=gui_control.Command(fonteditorgui.FontGUI,self.parent.vcs[ parent.vcs_id ] ,self.parent,self.dialog,self.fo,tentry,oentry,self))
         self.bpf.configure(command=gui_control.Command(fonteditorgui.FontGUI,self.parent.vcs[ parent.vcs_id ] ,self.parent,self.dialog,self.ft,tentry,oentry,self))

      else:
         self.dialog.destroy()

#---------------------------------------------------------------------
#
# End text object setting dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
