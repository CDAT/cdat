#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Database Dialog Popup -  gui_database module
#
#################################################################################
#                                                                               #
# Module:       gui_database module                                             #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore National Laboratory:                         #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI Software System browser database dialog popup.            #
#                                                                               #
# Version:      3.0                                                             #
#                                                                               #
#################################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import re, os, sys, string, cdms2
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import gui_message

#---------------------------------------------------------------------
#
# Get the CDMS Database information
#
#---------------------------------------------------------------------------
def get_cdms_database_info():
   try:
      uri = os.environ['CDMSROOT']
   except KeyError:
      gui_message.error( 'Database connection error: ' + '\nSet environment variable CDMSROOT to default database location' )
      return None, None, None
      #raise CDMSError, ConnectError + '%s\nSet environment variable CDMSROOT to default database location'%uri
   (scheme,netloc,path,parameters,query,fragment)=cdms2.cdurlparse.urlparse(uri)

   host_port = string.split(netloc,':')
   if len(host_port) > 0:
     host = host_port[0]
   if len(host_port) > 1:
     port = host_port[1]
   else:
     port = '389'

   cpath = (path,)
   return host, port, cpath

#---------------------------------------------------------------------
#
# Start of Popup Dialogs
#
#---------------------------------------------------------------------------
# CDMS Database Dialog Popup
#---------------------------------------------------------------------------
#
class create:
   def __init__(self, parent, host, port, path):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
            title = 'Database Connection',
            buttons = ('Connect', 'Dismiss'),
            defaultbutton = 'Connect',
            command = self.execute_connect )

        self.dialog.transient( self.parent ) # draw widget on top of its parent

        # Create and pack the NoteBook
        notebook = Pmw.NoteBook( self.dialog.interior() )
        notebook.pack(fill = 'both', expand = 1, padx = 5, pady = 5)

        #----------------------------------------------------------------------
        # Add the "Host" page to the notebook.
        #----------------------------------------------------------------------
        page = notebook.add('Host')
        notebook.tab('Host').focus_set()

        # Create the "Host Info" contents of the page.
        group = Pmw.Group(page, tag_text = 'Host Info')
        group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
        #     1st - line in Host Info group
        fm=Tkinter.Frame(group.interior())
        self.eny1=Pmw.EntryField(fm,
            labelpos = 'w',
            label_text = 'Host:',
            entry_background = 'white',
            entry_foreground = 'black',
            value = host,
            entry_width =  23,
            )
        self.eny1.pack( side='left', fill = 'both', padx=5, pady=5 )
#	version_num = ( "2", "3")
#	cbx1 = Pmw.ComboBox(fm,
#                label_text = 'Version:',
#	        labelpos = 'w',
#                entry_width =  2,
#		scrolledlist_items = version_num)
#	cbx1.pack(side='right', fill = 'x', after=eny1, padx=5, pady=0 )
#	cbx1.selectitem(0)
        self.eny2=Pmw.EntryField(fm,
            labelpos = 'w',
            label_text = 'Port:',
            entry_background = 'white',
            entry_foreground = 'black',
            value = port,
            entry_width =  5,
            )
        self.eny2.pack( side='right', fill = 'both', padx=5, pady=5 )
#        eny2.pack( side='right', fill = 'both', after=cbx1, padx=5, pady=5 )
        fm.pack( side='top', fill='both', expand=1 )
        #     2nd - line in Host Info group
        self.cbx2 = Pmw.ComboBox(group.interior(),
                label_text = 'Base DN:',
                labelpos = 'w',
                entry_background = 'white',
                entry_foreground = 'black',
                entry_width =  10,
                scrolledlist_items = path,
                )
        self.cbx2.pack(expand=1, fill = 'both', padx=5, pady=5 )
        self.cbx2.selectitem( path[0] )
        #     3rd - line in Host Info group
        fm2=Tkinter.Frame(group.interior())
        lbl=Tkinter.Label(fm2,
            text = "",
            justify = 'center',
            anchor = 'w',
            )
        lbl.pack( side = 'left', fill = 'both', padx=5, pady=5 )

        self.cbn1 = Pmw.RadioSelect(fm2,
                buttontype = 'checkbutton',
                orient = 'vertical',
                labelpos = 'w',
                hull_borderwidth = 2,
                hull_relief = 'ridge',
                command = self.anonymous_bind
        )
        self.cbn1.pack( side = 'right', fill = 'both', after=lbl, padx = 5, pady = 5 )
        self.cbn1.add( 'Anonymous bind' )
        fm2.pack( side='top', fill='both', expand=1 )

        # Create the "User Info" contents of the page
        group = Pmw.Group(page, tag_text = 'User Info')
        group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
        self.parent.eny3=Pmw.EntryField(group.interior(),
            labelpos = 'w',
            label_text = 'User DN:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  45,
            )
        self.parent.eny3.pack( expand=1, fill = 'both', padx=5, pady=5 )
        self.parent.eny3.configure(label_state='disabled', entry_state='disabled')
        self.parent.eny4=Pmw.EntryField(group.interior(),
            labelpos = 'w',
            label_text = 'Password:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_show ='*',
            entry_width =  45,
            )
        self.parent.eny4.pack( expand=1, fill = 'both', padx=5, pady=5 )
        self.parent.eny4.configure(label_state='disabled', entry_state='disabled')
        entries = ( self.parent.eny3, self.parent.eny4 )
        Pmw.alignlabels(entries)
        self.cbn1.invoke('Anonymous bind')
        #----------------------------------------------------------------------
        # End the "Host" page of the notebook
        #----------------------------------------------------------------------

        #----------------------------------------------------------------------
        # Add the "Transfer Method" page to the notebook
        #----------------------------------------------------------------------
        page = notebook.add('Transfer Method')
        # Create the "LDAP Settings" contents of the page.
        group = Pmw.Group(page, tag_text = 'Transfer Method')
        group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
        self.cbn2 = Pmw.RadioSelect(group.interior(),
                buttontype = 'radiobutton',
                orient = 'vertical',
                command = self.transfer_method_setting,
                labelpos = 'w',
        )
        self.cbn2.pack( expand=1, fill = 'both', padx=5, pady=5 )
        for text in ('Python FTP', 'ESG Request Manager'): self.cbn2.add(text)
        self.transfer_toggle = 'Python'

        # Create the "Request Manager User Info" contents of the page
        group = Pmw.Group(page, tag_text = 'Request Manager Info')
        group.pack(fill = 'both', expand = 1, padx = 5, pady = 5)
        self.cbn3 = Tkinter.Checkbutton( group.interior(),
            state = 'disabled',
            anchor = 'w',
            text = 'Use Replica Catalog',
            command = self.replica_toggle_cb,
        )
        self.replica_toggle = 1
        self.cbn3.select( )
        self.cbn3.pack( expand=1, fill = 'both', padx=5, pady=5 )
        replicaDN = os.environ.get('CDMS_REPLICA_DN')
        if replicaDN is None:
           replicaDN = ''
        self.rqmeny1=Pmw.EntryField(group.interior(),
                                    labelpos = 'w',
                                    label_text = 'Replica DN:',
                                    entry_background = 'white',
                                    entry_foreground = 'black',
                                    entry_width =  45,
                                    value = replicaDN,
                                    )
        self.rqmeny1.pack( expand=1, fill = 'both', padx=5, pady=5 )
        self.rqmeny1.configure(label_state='disabled', entry_state='disabled')
        self.rqmeny2=Pmw.EntryField(group.interior(),
            labelpos = 'w',
            label_text = 'User ID:',
            entry_background = 'white',
            entry_foreground = 'black',
            entry_width =  45,
            )
        self.rqmeny2.pack( expand=1, fill = 'both', padx=5, pady=5 )
        self.rqmeny2.configure(label_state='disabled', entry_state='disabled')
        entries = ( self.rqmeny1, self.rqmeny2 )
        Pmw.alignlabels(entries)
        self.cbn2.invoke( 'Python FTP' )

        self.dialog.geometry( "+%d+%d" % (self.parent.dxwin, self.parent.dywin) )
        notebook.setnaturalsize()

   def anonymous_bind(self, tag, state):
      self.cbn1.anonymous_flag = state
      if state:
         self.parent.eny3.configure(label_state='disabled', entry_state='disabled')
         self.parent.eny4.configure(label_state='disabled', entry_state='disabled')
      else:
         self.parent.eny3.configure(label_state='normal', entry_state='normal')
         self.parent.eny4.configure(label_state='normal', entry_state='normal')

   def replica_toggle_cb(self):
      if self.replica_toggle == 0:
         self.replica_toggle = 1
         self.cbn3.select()
      else:
         self.replica_toggle = 0
         self.cbn3.deselect()

   def transfer_method_setting(self, tag):
      if tag == 'Python FTP':
         self.rqmeny1.configure(label_state='disabled', entry_state='disabled')
         self.rqmeny2.configure(label_state='disabled', entry_state='disabled')
         self.cbn3.configure(state='disabled')
         self.transfer_toggle = 'Python'
      else:
         self.rqmeny1.configure(label_state='normal', entry_state='normal')
         self.rqmeny2.configure(label_state='normal', entry_state='normal')
         self.cbn3.configure(state='normal')
         self.transfer_toggle = 'RM'
   
   def execute_connect(self, result):
      import cdms2
      #	self.transfer_toggle is either: "Python" or "RM"
      #	self.replica_toggle is either:  0 or 1: default is 1 for User Replica Catalog
      # self.rqmeny1.get() returns the "Replica DN" string
      #	self.rqmeny2.get() returns the "User ID" string
      if result == 'Connect':
         if self.cbn1.anonymous_flag:
            gui_control.directory_or_database = 'database'
            uri = 'ldap://'+self.eny1.get()+':'+self.eny2.get()+self.cbx2.get()
#            self.parent.panelSV.textv_lab3.set("  Dataset:  " )
            gui_control.db=cdms2.connect( uri )
         else:    # Must have "User Dn" and "Password"
            try:
               gui_control.db=cdms2.connect( user=self.parent.eny3.get(),
                                            password=self.parent.eny4.get() )
            except:
               gui_message.error(
                    "Invalid 'User DN' and 'Password' combination.\nTry again." )
               return
            uri = gui_control.db.uri
         # Set the transfer method (Python FTP or ESG Request Manager)
         if self.transfer_toggle=='Python':
            import cdms2.cache
            cdms2.cache.usePythonTransfer()
         elif self.transfer_toggle=='RM':
            gui_control.db.useRequestManager(self.rqmeny1.get(), self.replica_toggle, self.rqmeny2.get())
         
         # Display the URI in the "Database" entry window
         self.parent.panelSV.tin2.delete( 0, 'end' )
         self.parent.panelSV.tin2.insert( 0, uri  )
         self.parent.panelSV.tin3.delete( 0, 'end' )
         self.parent.panelSV.tin4.delete( 0, 'end' )

         # Display the URIs in the "Databases" window
         match_found = None
         for x in gui_control.db_connections:
             match_found = re.match( uri, x )
             if match_found: break
         if not match_found:
            gui_control.db_connections.append( uri )
         self.parent.panelSV.tin2.clear( )        # clear "directory" combo box entry and list
         self.parent.panelSV.tin2.setentry( gui_control.db_connections )
         self.parent.panelSV.tin2.insert(  0, gui_control.db_connections )
         self.parent.balloon.bind(self.parent.panelSV.tin2._arrowBtn, 'View and Select Databases')
         self.parent.balloon.bind(self.parent.panelSV.tin3._arrowBtn, 'View and Select Datasets in Database')
         self.parent.balloon.bind(self.parent.panelSV.tin4._arrowBtn, 'View and Select Variables in Dataset')

         # Display the datasets in the "Datasets" window
         ds_list = gui_control.db.listDatasets()
         ds_list.sort()
         self.parent.panelSV.tin3.ds_list = ds_list
         #
         self.parent.panelSV.tin3.clear( )   # clear "directory" combo box entry and list
         self.parent.panelSV.tin4.clear( )   # clear "directory" combo box entry and list
         self.parent.panelSV.tin3.component('scrolledlist').setlist( ds_list )
         self.parent.panelSV.textv_lab3.set("Datasets:" )
      else:
         self.parent.panelSV.opt9.invoke(0)
      #
      self.dialog.destroy()


#---------------------------------------------------------------------
#
# End Database Popup Dialog
#
#---------------------------------------------------------------------
#
#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
