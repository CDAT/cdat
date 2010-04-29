#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Dimensions Panel -  gui_dimensions module
#
###############################################################################
#                                                                             #
# Module:       gui_panelC module                                             #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser Tkinter "Dimensions" panel GUI. #
#                                                                             #
# Version:      3.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import os, sys, string, cdms2
import gui_functions
import gui_control
import gui_output
import gui_message
from gui_support import gui_color

#---------------------------------------------------------------------------
#
# Start of the "Dimensions" panel GUI Layout
#
#---------------------------------------------------------------------------
# Start the Tkinter/Pmw GUI layout. The layout is listed from top to bottom.
# Starting with: the menu bar; followed by the "Select Variable" panel, which
# allows the user to select data from a directory or a database; followed by
# the "Graphics Control" panel, which allows the user to plot the selected or
# defined variables; followed by the "Dimension" panel, which allows the user to
# select subsets of the selected variable before plotting or storing into memory;
# followed by the "Defined Variables" panel, which allows the user to modify the
# variables that are stored in memory; and finally followed by the "Variable
# Information" scroll window, which displays variable information.
#
# All panels are contained within a paned widget. Thus, allowing the size of
# each section to expand or constrict.
#



## Function to resize the scrolled box when the panel changes
def alterpaneDM(self,parent,event=None):

   ## OK this is total BS but there's no way to know
   ## the size of a character in pixel !
   ## or to resize menutoon and combobox in pixels....
   ## therefore i'm defined an completely random
   ## average-character-size-in-pixels !
   acs=8
   ## defbutton+padding size
   pad=130
   ## Available width
   width=parent.pane.pane('panelDM').winfo_width()-pad
   menubuttonwidth=width/(1.+gui_control.dim_scale_ratio/gui_control.dim_button_ratio)
   if menubuttonwidth<gui_control.dim_button_min:
      menubuttonwidth=gui_control.dim_button_min
   elif menubuttonwidth>gui_control.dim_button_max:
      menubuttonwidth=gui_control.dim_button_max
   listwidth=(width-menubuttonwidth)/(gui_control.dim_scale_ratio+1)

   for i in range(gui_control.ndim):
      adim=self.dim[i]
      adim.menu.configure(width=int(menubuttonwidth/acs))
      adim.comb.configure(entry_width=int(listwidth/acs))
      adim.sep1.configure(width=width)
      adim.first_scl.configure(length=listwidth*gui_control.dim_scale_ratio)
      adim.last_scl.configure(length=listwidth*gui_control.dim_scale_ratio)
   pass

#---------------------------------------------------------------------
# Begin the creation of "Dimension" panel
#---------------------------------------------------------------------
class create:
   def __init__( self, parent ):

      #-------------------------------------------
      # create 3rd sub-panel
      #-------------------------------------------
      self.PANEL = Pmw.ScrolledFrame( parent.pane.pane( 'panelDM' ),
##                                  usehullsize=1,
                                 horizscrollbar_width=gui_control.scl_width,
                                 vertscrollbar_width=gui_control.scl_width )
      self.PANEL.pack( side='top', pady=3, fill = 'both', expand=1 )
      parent.balloon.bind(self.PANEL, "This is the Dimension Manipulation Panel.\n\nThe Dimension Manipulation Panel displays a row for\neach dimension that is defined in the array data\nattribute set. This panel can be used to re-order\nor reverse dimensions, change the viewing style of\nindiviual dimensions, select sub-regions, and\nmanipulate dimension sub-ranges.\n\nNote: The white entry and slider background color\nindicates that the selected variable is from a file\nand the cyan background color indicates that the\nvariable is defined in memory.")
      INTERIOR = self.PANEL.interior()

      #-----------------------------------------------------------------
      # create -- 'Dimensions' row by row; the number of 
      #            dimensions are controlled by gui_control.ndim
      #-----------------------------------------------------------------
      self.dim = []
      for i in range(gui_control.ndim):
         self.dim.append( create_dim_row( INTERIOR, parent ) )
      parent.pane.pane( 'panelDM' ).bind('<Configure>',gui_control.Command(alterpaneDM, self, parent ))

                                         
# Create one row of the dimension class
class create_dim_row:
   def __init__( self, frameit, parent ):
      self.top_frame = Tkinter.Frame( frameit, borderwidth=2 )
      self.lst = ( )
      self.top_frame.pack( side='top' )

      framed0 = Tkinter.Frame( self.top_frame )
      framed0.pack( side='top', fill = 'x', expand=1 )

      framed00a = Tkinter.Frame( framed0 )
      framed00a.pack( side='left' )

      # Create the lock/unlock Icon button
      self.canvas_lockicon_toggle = 0
      self.canvas_lockicon = Tkinter.Canvas(framed00a, bd=0, highlightthickness=0,
                                        width = 20, height = 20)
      self.canvas_lockicon.pack( side='top', fill=Tkinter.BOTH, expand='no', padx=5 )
      parent.balloon.bind( self.canvas_lockicon, "(Un)Hold dimension setting." )
      self.unlock = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'outpin_red.gif') )
      self.lock = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'inpin_red.gif') )
      self.tiltedlock = Tkinter.PhotoImage( file = os.path.join(cdms2.__path__[0],'..','..','..','..', 'bin', 'tiltedpin_red.gif') )
      self.canvas_lockicon.create_image(0,0, anchor=Tkinter.NW, image=self.unlock )
      self.canvas_lockicon.bind( '<1>', gui_control.Command( self.evt_lock, parent ))

      # Create the dimension pull down menu
      self.menu = Tkinter.Menubutton( framed00a,
         text = 'Dimension',
         relief = 'raised',
         underline = 0,
         borderwidth = 2,
         width=10
         )
      parent.balloon.bind( self.menu, 'Dimension Menu to Change Coordinate Values to Index Values\nand to Re-Order Dimensions' )
      self.menu.pack( side='top', padx=2 )
      self.men1 = Tkinter.Menu( self.menu )
      self.menu['menu'] = self.men1
      self.index_lb = Tkinter.StringVar()
      self.index_lb.set('Index')
      self.men1.index_flg = 0
      self.men1.raw_flg = 0
      self.stride = 1
      # Added the Check Buttons items in gui_functions._srl_var_dim1_to_ndim

      # Create the dimension combo box
      framed0a = Tkinter.Frame( framed0 )
      framed0a.pack( side='left' )
      self.comb = Pmw.ComboBox( framed0a,
         scrolledlist_usehullsize = 1,
         entry_width = 15,
         entry_background = gui_color.dim,
         entry_foreground = 'black',
         selectioncommand = gui_control.Command( self.evt_select_command, parent ),
         scrolledlist_items=self.lst )
      self.comb.pack( side='top' )
      self.comb.component('entry').bind( "<Key>", gui_control.Command(self.evt_change_entry_color, parent, 0) )
      parent.balloon.bind( self.comb, "Dimension coordinate or index values entry window.\n\nDimension representation is 'first : last' or\n'first : last by stride'. The arrow button to the\nright can also be used to select the first and last\npoints. The top slider to the right of the arrow\nbutton, changes the first value, while the bottom\nslider changes the last value.\n\nNote: The 'Enter' key must be depressed in order\nregister changes." )
      self.dim_lab = Tkinter.StringVar()
      self.lab = Tkinter.Label( framed0a, textvariable=self.dim_lab )
      self.dim_lab.set(" " )
      self.lab.pack( side='top' )

      self.comb._entryWidget.bind( "<Return>", gui_control.Command(self.evt_enter_first_last, parent) )

      self.comb._arrowBtn.bind('<1>', gui_control.Command(self.evt_select_values, parent), self.comb._postList)
      parent.balloon.bind( self.comb._arrowBtn, 'View and selected first and last values.' )


      self.comb.component( 'scrolledlist' ).configure(
         horizscrollbar_width = gui_control.scl_width,
         vertscrollbar_width  = gui_control.scl_width)

      self.comb.component( 'listbox' ).configure(
         height           = 37,
         width            = gui_control.listbox_width,
         background       = gui_color.dim,
         foreground       = 'black',
         selectbackground = 'lightgrey',
         selectforeground = 'black',
         selectmode       = Tkinter.MULTIPLE,
         exportselection  = Tkinter.NO )

      self.srl = self.comb.component( 'scrolledlist' )

      framed1 = Tkinter.Frame( framed0 )
      framed1.pack( side='left', padx=2)
      self.first_scl = Tkinter.Scale( framed1,
         orient           = 'horizontal',
         width            = 9,
         showvalue        = 0,
         length           = 205,
         troughcolor	  = gui_color.dim,
         from_            = 0,
         to               = 100,
         command 	  = gui_control.Command( self.evt_first_scl, parent )
        )
      self.first_scl.pack( side='top', fill='x', expand=1 )
      self.first_scl.do_second_row=1
      parent.balloon.bind( self.first_scl, "Slider to change the first value." )

      self.last_scl = Tkinter.Scale( framed1,
         orient           = 'horizontal',
         width            = 9,
         showvalue        = 0,
         troughcolor	  = gui_color.dim,
         length           = 205,
         from_            = 0,
         to               = 100,
         command 	  = gui_control.Command( self.evt_last_scl, parent )
         )
      self.last_scl.pack( side='top', fill='x', expand=1 )
      self.last_scl.do_second_row=1
      parent.balloon.bind( self.last_scl, "Slider to change the last value." )
      
      # Enter the First and Last values values under the scale widgets
      framed2 = Tkinter.Frame( framed1 )
      framed2.pack( side='top')
      self.text_flb = Tkinter.StringVar()
      self.text_flb.set('0.0')
      self.slb1 = Tkinter.Label( framed2, justify='left',
#                                  font = ('time',12),
                                   width=12, textvariable=self.text_flb )
      self.slb1.pack( side='left' )
      self.text_llb = Tkinter.StringVar()
      self.text_llb.set('0.0')
      self.slb2 = Tkinter.Label( framed2, justify='right',
#                                  font = ('time',12),
                                   width=12, textvariable=self.text_llb )
      self.slb2.pack( side='right', padx=15 )

      # Finally, add the function option menu
      self.opt = Pmw.OptionMenu( framed0,
                                  menubutton_width=3,
                                  items=gui_control.dimchlst,
                                  command=gui_control.Command(self.evt_dim_function,parent) )
      self.opt.configure( menubutton_text=gui_control.dimchlst[0][0:3] )
      self.opt.pack( side='left' )
      parent.balloon.bind( self.opt, 'Operations Specific to this Dimension' )

      self.sep1 = Tkinter.Frame ( self.top_frame , relief = 'raised',
                    height = 4, width = 520, borderwidth = 2, background=gui_color.two)
      self.sep1.pack(side = 'top' )

   #
   # event to set background color of the entry window. This change of 
   # the background color lets the user know they are in edit mode and must
   # select the 'Enter' key to register changes.
   #
   def evt_change_entry_color( self, parent, num, event ):
      keycolor = Pmw.Color.changebrightness(parent, 'red', 0.85)
      self.comb.component('entry').configure( background = keycolor )

   #
   # event to set the first and last dimension value from the entry widget,
   # either in coordinates or index
   #
   def evt_enter_first_last( self, parent, event ):
      e = self.comb._entryfield.get()
      e2 = string.split( e, 'by' )
      e1 = string.split(e2[0], ':')
      self.comb.component('entry').configure( background = self.first_scl.cget( 'troughcolor' ) )
      if self.men1.raw_flg == 1: # view "time" raw values
         gui_message.error('Must be in index or raw mode.')
      elif self.men1.index_flg == 1: # view index values
         f = e1[0]
         try:
            l = e1[1]
         except:
            l = f
         try:
            self.stride = abs( string.atoi( e2[1] ) )
         except:
            self.stride = 1
         try:
            self.first_scl.set(string.atoi(f))
            self.last_scl.set(string.atoi(l))
         except:
            gui_message.error('Must pass a number.')
            self.first_scl.set( 0 )
            self.last_scl.set( 0 )
      else: # view coordinate values
         f = e1[0]
         try:
            l = e1[1]
         except:
            l = f
         try:
            self.stride = abs( string.atoi( e2[1] ) )
         except:
            self.stride = 1
         try:
            af = string.atof( f )
            al = string.atof( l )
            # find zone
            zmin = string.atof(self.darray[0])
            zmax = string.atof(self.darray[-1])
            fdarray = []
            z1 = string.atof( self.darray[0] )
            for i in range(1,len(self.darray)):
               if (zmax - zmin) > 0:           # accending order
                  z2 = string.atof( self.darray[i] )
                  fdarray.append( z1 + (z2 - z1) * 0.5 )
                  z1 = z2
               else:
                  z2 = string.atof( self.darray[i] )
                  fdarray.append( z2 + (z1 - z2) * 0.5 )
                  z1 = z2
            if (zmax - zmin) > 0:           # accending order
               iaf = 0
               for i in range( len(self.darray) ):
                   az1 = fdarray[i]
                   try:
                      az2 = fdarray[i+1]
                   except:
                      az2 = az1
                   if (af <= fdarray[0]):
                      iaf = i
                      break
                   elif (af < az1):
                      iaf = i
                      break
                   elif (af >= fdarray[-1]):
                      iaf = len(self.darray) - 1
                      break
               ial = 0
               for i in range( len(self.darray) ):
                   az1 = fdarray[i]
                   try:
                      az2 = fdarray[i+1]
                   except:
                      az2 = az1
                   if (al <= fdarray[0]):
                      ial = i
                      break
                   elif (al <= az1):
                      ial = i
                      break
                   elif (al >= fdarray[-1]):
                      ial = len(self.darray) - 1
                      break
            elif (zmax - zmin) == 0:           # Nothing to do 
              return
            #
            cf = string.atof( self.darray[iaf] )
            cl = string.atof( self.darray[ial] )
            #
            self.first_scl.set( iaf )
            self.last_scl.set( ial )
         except:
            gui_message.error("Try again in 'Index' mode.")
            self.first_scl.set( 0 )
            self.last_scl.set( 0 )

      # If the lock icon is set, then save the current state of the dimension
      if self.canvas_lockicon_toggle == 1:
         gui_control.locked_dim_information[self.dim_name] = [self.first_scl.get(), self.last_scl.get(), self.darray[ self.first_scl.get() ], self.darray[ self.last_scl.get() ], self.stride, self.opt.getvalue()]

         # Save the order of the dimension -- used for locking and unlocking
         temp_dic = {}
         found, l_key = self.find_locked_dim( parent )
         for i in range(parent.panelDM.ndim):
             temp_dic[gui_control.dim_axis[parent.panelDM.ndim-i-1]] = parent.panelDM.dim[i].dim_name
         if not found:
            use_key = len(gui_control.locked_dim_order)
         else:
            use_key = l_key
         gui_control.locked_dim_order[use_key] = temp_dic

   #
   # event to set the first dimension value, either in coordinates or index
   #
   def evt_first_scl( self, parent, event):
##       print 'In there !',parent.panelDM.ndim,event,type(event)
      for i in range( parent.panelDM.ndim ):
##          print i,parent.panelDM.dim[i].dim_name,parent.panelDM.grid,self.dim_name
         if self.dim_name == parent.panelDM.dim[i].dim_name: break
      if isinstance(parent.panelDM.grid,cdms2.gengrid.AbstractGenericGrid):
##          R=parent.panelDM.dim[i]
         if self.dim_name=='Row0': ## dealing wit longitude one
            R0=parent.panelDM.dim[i]
            R1=parent.panelDM.dim[i+1]
         else:
            R0=parent.panelDM.dim[i-1]
            R1=parent.panelDM.dim[i]

         if self.men1.index_flg == 1:
            ltxt=self.text_llb.get()
            ftxt=event
            sd = R0.stride
            estr=ftxt+' : '+ltxt+' by '+str(sd)
            R0.comb.setentry(estr)
            R0.text_flb.set(ftxt)
            R1.comb.setentry(estr)
            R1.text_flb.set(ftxt)
            if self.first_scl.do_second_row:
               if self.dim_name=='Row0': ## dealing wit longitude one
                  R0.first_scl.do_second_row=0
                  R1.first_scl.set(string.atoi(event))                  
                  R0.first_scl.do_second_row=1
               else:
                  R1.first_scl.do_second_row=0
                  R0.first_scl.set(string.atoi(event))                  
                  R1.first_scl.do_second_row=1
         else:
            ltxt=self.text_llb.get()
            f=string.atoi(event)
            ftxt=str(self.darray[f])[:13]
            self.text_flb.set(ftxt)
            estr=ftxt+' : '+ltxt
            self.comb.setentry(estr)

         af = string.atoi( event )
         al = self.last_scl.get()
         self.npts = []
         if (af <= al):
            for j in range( af, (al+1)  ):
               self.npts.append(j)
         else:
            for j in range( al, (af+1)  ):
               self.npts.insert( 0, j )
      elif isinstance(parent.panelDM.grid,cdms2.hgrid.AbstractCurveGrid):
         R0=parent.panelDM.dim[i]
         if self.men1.index_flg == 1:
            ltxt=self.text_llb.get()
            ftxt=event
            sd = R0.stride
            estr=ftxt+' : '+ltxt+' by '+str(sd)
            R0.comb.setentry(estr)
            R0.text_flb.set(ftxt)
            R0.first_scl.set(string.atoi(event))                  
         else:
            ltxt=self.text_llb.get()
            f=string.atoi(event)
            ftxt=str(self.darray[f])[:13]
            self.text_flb.set(ftxt)
            estr=ftxt+' : '+ltxt
            self.comb.setentry(estr)

         af = string.atoi( event )
         al = self.last_scl.get()
         self.npts = []
         if (af <= al):
            for j in range( af, (al+1)  ):
               self.npts.append(j)
         else:
            for j in range( al, (af+1)  ):
               self.npts.insert( 0, j )
      else:
         try:
            if self.men1.raw_flg == 1:
               l = self.rdarray[ parent.panelDM.dim[i].last_scl.get() ]
               f = self.rdarray[ string.atoi(event) ]
            elif self.men1.index_flg == 0: ## Lat/lon Selection
               if (string.lower( parent.panelDM.dim[i].dim_name) in gui_control.longitude_alias):
#               s = parent.panelDM.dim[i].start
                  l = self.darray[ parent.panelDM.dim[i].last_scl.get()]
                  f = self.darray[ string.atoi(event) ]
               else:
                  l = self.darray[ parent.panelDM.dim[i].last_scl.get() ]
                  f = self.darray[ string.atoi(event) ]
            else:
               l = parent.panelDM.dim[i].last_scl.get()
               f = string.atoi(event)
            sd = parent.panelDM.dim[i].stride
            estr = str(f) + ' : ' + str(l) + ' by ' + str(sd)
            self.text_flb.set( str(f)[0:13] )
            # create the new list of selected points (i.e., npts)
            if (string.lower( parent.panelDM.dim[i].dim_name) in gui_control.longitude_alias):
#            s = parent.panelDM.dim[i].start
               af = string.atoi( event )
               al = self.last_scl.get()
            else:
               af = string.atoi( event )
               al = self.last_scl.get()
            parent.panelDM.dim[i].npts = []
            if (af <= al):
               for j in range( af, (al+1)  ):
                  parent.panelDM.dim[i].npts.append(j)
            else:
               for j in range( al, (af+1)  ):
                  parent.panelDM.dim[i].npts.insert( 0, j )
            #
            parent.panelDM.dim[i].comb.setentry(estr)
         except Exception, err:
##             print 'Error in first select:',err
##          raise
            pass

      # If the lock icon is set, then save the current state of the dimension
      if self.canvas_lockicon_toggle == 1:
         gui_control.locked_dim_information[self.dim_name] = [self.first_scl.get(), self.last_scl.get(), self.darray[ self.first_scl.get() ], self.darray[ self.last_scl.get() ], self.stride, self.opt.getvalue()]

         # Save the order of the dimension -- used for locking and unlocking
         temp_dic = {}
         found, l_key = self.find_locked_dim( parent )
         for i in range(parent.panelDM.ndim):
             temp_dic[gui_control.dim_axis[parent.panelDM.ndim-i-1]] = parent.panelDM.dim[i].dim_name
         if not found:
            use_key = len(gui_control.locked_dim_order)
         else:
            use_key = l_key
         gui_control.locked_dim_order[use_key] = temp_dic

   #
   # event to set the last dimension value, either in coordinates or index
   #
   def evt_last_scl( self, parent, event ):
      for i in range( parent.panelDM.ndim ):
         if self.dim_name == parent.panelDM.dim[i].dim_name: break
      if isinstance(parent.panelDM.grid,cdms2.gengrid.AbstractGenericGrid):
##          R=parent.panelDM.dim[i]
         if self.dim_name=='Row0': ## dealing wit longitude one
            R0=parent.panelDM.dim[i]
            R1=parent.panelDM.dim[i+1]
         else:
            R0=parent.panelDM.dim[i-1]
            R1=parent.panelDM.dim[i]

         if self.men1.index_flg == 1:
            ftxt=self.text_flb.get()
            ltxt=event
            sd = R0.stride
            estr=ftxt+' : '+ltxt+' by '+str(sd)
            R0.comb.setentry(estr)
            R0.text_llb.set(ltxt)
            R1.comb.setentry(estr)
            R1.text_llb.set(ltxt)
            if self.last_scl.do_second_row:
               if self.dim_name=='Row0': ## dealing wit longitude one
                  R0.last_scl.do_second_row=0
                  R1.last_scl.set(string.atoi(event))                  
                  R0.last_scl.do_second_row=1
               else:
                  R1.last_scl.do_second_row=0
                  R0.last_scl.set(string.atoi(event))                  
                  R1.last_scl.do_second_row=1                  
         else:
            ftxt=self.text_flb.get()
            l=string.atoi(event)
            ltxt=str(self.darray[l])[:13]
            self.text_llb.set(ltxt)
            estr=ftxt+' : '+ltxt
            self.comb.setentry(estr)

         af=self.first_scl.get()
         al=string.atoi(event)
         self.npts = []
         if (af <= al):
            for j in range( af, (al+1)  ):
               self.npts.append(j)
         else:
            for j in range( al, (af+1)  ):
               self.npts.insert( 0, j )

      elif isinstance(parent.panelDM.grid,cdms2.hgrid.AbstractCurveGrid):
         R0=parent.panelDM.dim[i]
         if self.men1.index_flg == 1:
            ftxt=self.text_flb.get()
            ltxt=event
            sd = R0.stride
            estr=ftxt+' : '+ltxt+' by '+str(sd)
            R0.comb.setentry(estr)
            R0.text_llb.set(ltxt)
            R0.last_scl.set(string.atoi(event))                  
         else:
            ftxt=self.text_flb.get()
            l=string.atoi(event)
            ltxt=str(self.darray[l])[:13]
            self.text_llb.set(ltxt)
            estr=ftxt+' : '+ltxt
            self.comb.setentry(estr)

         af=self.first_scl.get()
         al=string.atoi(event)
         self.npts = []
         if (af <= al):
            for j in range( af, (al+1)  ):
               self.npts.append(j)
         else:
            for j in range( al, (af+1)  ):
               self.npts.insert( 0, j )
         pass
      else:
         try:
            if self.men1.raw_flg == 1:
               f = self.rdarray[ self.first_scl.get() ]
               l = self.rdarray[ string.atoi(event) ]
            elif self.men1.index_flg == 0:
               if isinstance(parent.panelDM.grid,(cdms2.hgrid.AbstractCurveGrid,cdms2.gengrid.AbstractGenericGrid)):
                  f = self.darray.index(string.atof(parent.panelDM.dim[i].first_scl.get()))
                  l = string.atoi(event)
               elif (string.lower( parent.panelDM.dim[i].dim_name) in gui_control.longitude_alias):
   #               s = parent.panelDM.dim[i].start
                  f = self.darray[ self.first_scl.get() ]
                  l = self.darray[ string.atoi(event) ]
               else:
                  f = self.darray[ self.first_scl.get() ]
                  l = self.darray[ string.atoi(event) ]
            else:
               f = self.first_scl.get()
               l = string.atoi(event)
            sd = parent.panelDM.dim[i].stride
            estr = str(f) + ' : ' + str(l) + ' by ' + str(sd)
            self.text_llb.set( str(l)[0:13] )
            # create the new list of selected points (i.e., npts)
            if (string.lower( parent.panelDM.dim[i].dim_name) in gui_control.longitude_alias):
   #            s = parent.panelDM.dim[i].start
               af=self.first_scl.get() 
               al=string.atoi(event)
            else:
               af=self.first_scl.get()
               al=string.atoi(event)
            parent.panelDM.dim[i].npts = []
            if (af <= al):
               for j in range( af, (al+1)  ):
                  parent.panelDM.dim[i].npts.append(j)
            else:
               for j in range( al, (af+1)  ):
                  parent.panelDM.dim[i].npts.insert( 0, j )
            #
            parent.panelDM.dim[i].comb.setentry(estr)
         except:
            pass

      # If the lock icon is set, then save the current state of the dimension
      if self.canvas_lockicon_toggle == 1:
         gui_control.locked_dim_information[self.dim_name] = [self.first_scl.get(), self.last_scl.get(), self.darray[ self.first_scl.get() ], self.darray[ self.last_scl.get() ], self.stride, self.opt.getvalue()]

         # Save the order of the dimension -- used for locking and unlocking
         temp_dic = {}
         found, l_key = self.find_locked_dim( parent )
         for i in range(parent.panelDM.ndim):
             temp_dic[gui_control.dim_axis[parent.panelDM.ndim-i-1]] = parent.panelDM.dim[i].dim_name
         if not found:
            use_key = len(gui_control.locked_dim_order)
         else:
            use_key = l_key
         gui_control.locked_dim_order[use_key] = temp_dic

   #
   # Select the range of the coordinate values
   #
   def evt_select_values( self, parent, event ):
      self.srl.select_clear( 0, 'end' )
      try:
         sd = self.stride
      except:
         self.stride = 1
         sd = self.stride

      for i in range( len(self.npts) ):
         if (i % sd == 0):
             self.srl.select_set( self.npts[i] )

      self.srl.see( self.npts[0] )
   #
   # Select the dimension operation             
   #
   def evt_dim_function( self, parent, event ):
      self.opt.configure( menubutton_text=event[0:3] )
      if event[0:3] == 'awt':
         dim_index = 0
         while (1):
            if self.opt == parent.panelDM.dim[dim_index].opt: break
            dim_index += 1
         parent.panelDM.dim[dim_index].new_weight = None
         gui_functions.replace_axis_values( parent, dim_index, 'awt' )

      # If the lock icon is set, then save the current state of the dimension
      if self.canvas_lockicon_toggle == 1:
         gui_control.locked_dim_information[self.dim_name] = [self.first_scl.get(), self.last_scl.get(), self.darray[ self.first_scl.get() ], self.darray[ self.last_scl.get() ], self.stride, self.opt.getvalue()]

         # Save the order of the dimension -- used for locking and unlocking
         temp_dic = {}
         found, l_key = self.find_locked_dim( parent )
         for i in range(parent.panelDM.ndim):
             temp_dic[gui_control.dim_axis[parent.panelDM.ndim-i-1]] = parent.panelDM.dim[i].dim_name
         if not found:
            use_key = len(gui_control.locked_dim_order)
         else:
            use_key = l_key
         gui_control.locked_dim_order[use_key] = temp_dic

   def evt_lock( self, parent, event ):
      self.canvas_lockicon.delete("all")
      if (self.canvas_lockicon_toggle == 0):
         self.canvas_lockicon_toggle = 1
         self.canvas_lockicon.create_image(0,0, anchor=Tkinter.NW, image=self.lock )
         gui_control.locked_dim_information[self.dim_name] = [self.first_scl.get(), self.last_scl.get(), self.darray[ self.first_scl.get() ], self.darray[ self.last_scl.get() ], self.stride, self.opt.getvalue()]
      else:
         self.canvas_lockicon_toggle = 0
         self.canvas_lockicon.create_image(0,0, anchor=Tkinter.NW, image=self.unlock )
         del gui_control.locked_dim_information[self.dim_name]

      # Save the order of the dimension -- used for locking and unlocking
      temp_dic = {}
      found, l_key = self.find_locked_dim( parent )
      for i in range(parent.panelDM.ndim):
          temp_dic[gui_control.dim_axis[parent.panelDM.ndim-i-1]] = parent.panelDM.dim[i].dim_name
      if not found:
         use_key = len(gui_control.locked_dim_order)
      else:
         use_key = l_key
      gui_control.locked_dim_order[use_key] = temp_dic

      # Clear the locking order that was previously saved
      if (gui_control.locked_dim_information == {}):
         for x in gui_control.locked_dim_order.keys(): del(gui_control.locked_dim_order[x])

   def find_locked_dim( self, parent ):
      if (gui_control.locked_dim_order == {}): return False, None
      for j in range(len(gui_control.locked_dim_order)):
         x = gui_control.locked_dim_order.keys()[j]
         y = gui_control.locked_dim_order.values()[j]
         found = True
         for i in range(parent.panelDM.ndim):
             if parent.panelDM.dim[i].dim_name not in y.values():
                found = False
         if found: return found, x
      return found, x

   #
   # Select the dimension coordinates via the combo box scrolled list
   #
   def evt_select_command( self, parent, event ):
     pts1, pts2 = self.return_selection( parent )
     self.first_scl.set( pts1 )
     self.last_scl.set( pts2 )
   #
   # Return the selected coordinate points
   #
   def return_selection( self, parent ):
      gpts = self.comb.component( 'scrolledlist' ).curselection()
      apts = self.comb.component( 'scrolledlist' ).index('active')
      npts = self.npts

      Spt1 = apts

      if (len(gpts) == 0):
         Spt1 = Spt2 = apts
      elif (len(gpts) == 1):
         Spt2 = int(gpts[0])
      elif (len(npts) > 1):
         Spt2 = Spt1
      elif (len(gpts) == 2):
         if npts[0] < apts: 
             Spt1 = npts[0]
             Spt2 = apts
         else:
             Spt1 = apts
             Spt2 = npts[0]

      return Spt1, Spt2

#---------------------------------------------------------------------
# End Dimensions Panel Layout
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
