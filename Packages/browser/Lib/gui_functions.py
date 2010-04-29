#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Miscellaneous Functions -  gui_functions module
#
###############################################################################
#                                                                             #
# Module:       gui_functions module                                          #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                              	      #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System Browser miscellaneous GUI functions.    #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import __main__, os, sys, string, cdms2, cdtime, vcs, numpy
import gui_control
import gui_reset
import gui_message
import gui_defined_variables
from genutil import getAxisWeightByName
from gui_support import gui_color
from vcs import pagegui
## commenting out next line, dangerous! will replace funtion names one at a time
## from numpy import *

#------------------------------------------------------------------------
# Return directory history -  its sub-directories that the user accessed
#------------------------------------------------------------------------
def _scn_dir_history(  parent, dir_name ):
   try: parent.dir_history.remove( dir_name )
   except: pass
   parent.dir_history.insert( 0, dir_name )

#------------------------------------------------------------------------
# Return directory information -  its files and sub-directories
#------------------------------------------------------------------------
def _scn_a_dir( self ):
   import string
   d = []
   f = []
   search_pattern = None
   if (self.file_search_pattern is not None):
      sct=string.count( self.file_search_pattern, "*" )
      l = string.find( self.file_search_pattern, "*" )
      r = string.rfind( self.file_search_pattern, "*" )
      if sct == 0:  # No wild cards
        search_pattern = self.file_search_pattern
        sct = 2
      elif sct >= 2:
        search_pattern = self.file_search_pattern[(l+1):r]
      elif l == 0:
        search_pattern = self.file_search_pattern[1:]
##         sct = 0
      elif l > 0:
        search_pattern = self.file_search_pattern[0:-1]
        sct = 1
   else:
       sct = -1
   for x in os.listdir( './' ):
      if self.file_search_pattern is None:
         if os.path.isfile( x ):
##             try:
##                a=cdms2.open(x)
##                a.close()
##                f.append(x)
##             except:
##                 pass
           if x.lower()[-5:] in [ ".cdms", "*.cdml"]:
              f.append( x )
           elif  x.lower()[-4:] in [".cdf",".dic",".hdf", ".ctl", ".xml"] :
              f.append( x )
           elif  x.lower()[-3:] in [".nc","*.pp"] :
              f.append( x )
         elif os.path.isdir(x):
            d.append( x )
      elif( sct == -1 ):
         if os.path.isfile( x ): 
            f.append( x )
         else:
            d.append( x )
      elif sct == 0:
         if os.path.isfile( x ):
            if  (string.find( x, search_pattern ) != 0) and (string.find( x, search_pattern ) != -1):
               f.append( x )
         else:
            d.append( x )
      elif sct == 1:
         if os.path.isfile( x ):
            if  (string.find( x, search_pattern ) >= 0):
               if self.file_search_pattern[0]=='*':
                  n=len(search_pattern)
                  if x[-n:]==search_pattern:
                     f.append( x )
               elif self.file_search_pattern[-1]=='*':
                  n=len(search_pattern)
                  if x[:n]==search_pattern:
                     f.append( x )
               else:
                  f.append(x)
         else:
            d.append( x )
      elif sct >= 2:           # looking for "*[string], e.g., "*.nc" or "*.cdms"
         if os.path.isfile( x ):
            if string.find( x, search_pattern ) != -1 or search_pattern=='All Files':
               f.append( x )
         else:
            d.append( x )
   d.sort()
   dir = os.getcwd()
   i = 0
   while dir != "" and dir !=  "/":
        d.insert(i, dir)
        s=string.split( dir, '/' )[-1]
        dir = dir[0:(len(dir)-len(s)-1)]
        i += 1
   d.insert(i, "/")
   d.insert(i+1, "============================================================================")
   f.sort()
   return d,f

#---------------------------------------------------------------------------------
# List file information in Variable Information scrollbar window
#---------------------------------------------------------------------------------
def _scn_a_file( parent, name, g ):
      text = "*** Description of global file attributes in %s ***\n" % g.id
      for x in g.attributes.keys():
         text=text + "%s: %s\n" %(x, g.attributes[x])
      text = text + "\n*** End of global file %s description ***\n" % g.id
      return text

#---------------------------------------------------------------------------------
# List variable information in Variable Information scrollbar window
#---------------------------------------------------------------------------------
def _scn_a_var( parent, name ):
   # This is one of two places where the "Retain User Defined Settings"
   # is called. THIS IS NO LONGER IN USE!!!
   #if parent.menu.retain_user_settings_flg == 0:
   #   gui_reset.to_initial_state(parent)

   g = parent.panelDM.fid2
   text = ''
   try:
      ndim = len(g.listdimension(name))
      l = g.listall( name )
      dtext={ 0:'', 1:'', 2:'', 3:'', 4:'' }
      dlen={ 0:'', 1:'', 2:'', 3:'', 4:'' }
      dim_ct=-1
      for x in l:
         if x != '':
            if string.find( x, " Dimension " ) != -1:
               if dim_ct == -1:
                  text = text + ('Number of dimensions: %d' % ndim)
               dim_ct=dim_ct+1
            if dim_ct > -1:
               if string.find( x, " End of description " ) != -1:
                  end = x
               else:
                  dtext[dim_ct] = dtext[dim_ct] + x + '\n'
                  if string.find( x, "Length:") != -1:
                     dlen[dim_ct] = dlen[dim_ct] + x[8:len(x)]
            else:
               text = text + x + '\n'
      text2 = 'Dimension shape(s): '
      for i in range(ndim):
         if parent.menu.fortran_order_flg == 1:
               text2 = text2 + dlen[ndim-i-1] + ' '
         else:
               text2 = text2 + dlen[i] + ' '
      text = text2 + '\n\n' + text
   
      text = text + '\n' + '\n'
      for i in range(ndim):
         if parent.menu.fortran_order_flg:
            text = text + dtext[ndim-i-1]
         else:
            text = text + dtext[i]
         text = text + '' + '\n'
   
      text = text + end + '\n' + '\n'
      text = text + "*** Description of global file attributes in %s ***\n" % g.id
      for x in g.attributes.keys():
         text=text + "%s: %s\n" %(x, g.attributes[x])
      text = text + "\n*** End of global file %s description ***\n" % g.id

      return text

   except:
      ndim = 1
      a = g[name]
      text = str(a)
      return text


#------------------------------------------------------------------------
# Set the file's scroll-bar with file's variables
#------------------------------------------------------------------------
def _srl_file_var( self ):
   if gui_control.directory_or_database == 'directory':
      g = self.panelDM.fid2
      # Get the variables that also listed as axes
      axis_names = []
      if self.menu.view_axes_flg == 1:
         for axis in g.axes.values():
             if axis.isVirtual() != 1:
                a_name = axis.id + " (" + str(len(axis)) + ") - [" + axis.units + ":  (" + str(axis[0]) + ", " + str(axis[-1]) + ")]"
                axis_names.append(a_name)
      axis_names.sort()
      if axis_names != []:
         axis_names.insert( 0, '*************** Axes List **************' )

      spatial = 0
      if self.menu.view_bounds_weights_flg == 0: spatial = 1
      get_vars = g.getVariables( spatial=spatial )
      var_list = []
      bnds_wgts_list = []
      for i in range( len(get_vars) ):
          if get_vars[i].id[:7] in ['bounds_', 'weights_']:
             bnds_wgts_list.append( get_vars[i].id )
          else:
             var_list.append( get_vars[i].id )
      var_list.sort()

      ##################################################################################
      #                                                                                #
      # Separate the variables out by number of dimensions and show the length of each #
      # dimension.                                                                     #
      #                                                                                #
      ##################################################################################
      dlen_var={ 0:[], 1:[], 2:[], 3:[], 4:[], 5:[], 6:[], 7:[], 8:[], 9:[], 10:[] }
      for x in var_list:
        ndim = len(g.listdimension(x))
        l = g.listall( x )
        name_shape = x + " ("
        long_name = ""
        units = ""
        ndim = len(g.listdimension(x))
        for y in l:
            if string.find( y, "Length:") != -1:
               name_shape += y[8:len(y)] + ", "
            if string.find( y, "long_name:") != -1:
               long_name += y[11:len(y)] + ", "
            if string.find( y, "units:") != -1:
               units += y[7:len(y)]
        if (ndim == 0):
           name_shape = name_shape + ")"
        else:
           name_shape = name_shape[:-2] + ")"
        if (long_name != ""):
           name_shape = name_shape + " - [" + long_name + "]"
           if (units != ""):
              name_shape = name_shape[:-1] + units + "]"
        else:
           if (units != ""):
              name_shape = name_shape + " - ["  + units + "]"
        dlen_var[ndim].append( name_shape )

      var_list2 = []
      for i in range(len(dlen_var)):
          if dlen_var[i] != []:
             var_list2.append('************* %id Variable List *************' % i )
             for k in dlen_var[i]:
                 var_list2.append( k )

      var_list = var_list2

      # Comment out the old way of listing the variables
      #if var_list != []:
      #   var_list.insert( 0, '************* Variable List *************' )

      bnds_wgts_list.sort()
      if bnds_wgts_list != []:
         bnds_wgts_list.insert( 0, '******** Bounds and Weights List ******' )

      self.panelDM.lst3 = var_list + bnds_wgts_list + axis_names

   elif gui_control.directory_or_database == 'database':
      self.panelDM.lst3 = self.panelDM.lst2.variables.keys()
      self.panelDM.lst3.sort()
   #
   self.panelDM.var3 = self.panelDM.remember_var3 = None
   if self.panelSV.tin4.first_time == 1:
      self.panelSV.tin4.delete(0, 'end')
   else:
      self.panelSV.tin4.clear( )
   for i in range(len(self.panelDM.lst3)): 
      self.panelSV.tin4.insert( i, self.panelDM.lst3[i] )
   #
   #self.panelSV.tin4.delete( 0, len(self.panelSV.tin4.get()) )

#----------------------------------------------------------------------------------
# Don't show dimensions 1 thru ndim and their scroll-bar, title, choice button, etc.
#----------------------------------------------------------------------------------
def _blank_dim1_to_ndim( parent ):
   parent.annotate_start_over = 1 # Reset the annotation to show new variable information

   for i in range(gui_control.ndim):
      # Unlock the dimension icons and reset
      parent.panelDM.dim[i].canvas_lockicon.delete("all")
      parent.panelDM.dim[i].canvas_lockicon_toggle = 0
      parent.panelDM.dim[i].canvas_lockicon.create_image(0,0, anchor=Tkinter.NW, image=parent.panelDM.dim[i].unlock )

      parent.panelDM.dim[i].top_frame.pack_forget()

def set_locked_information( parent, i ):
      # Reset the locked information in the dimension
      try: 
         if (parent.panelDM.dim[i].dim_name in gui_control.locked_dim_information.keys()):
            dim_name = parent.panelDM.dim[i].dim_name

            # Get the first and last index values from the index array
            first = parent.panelDM.dim[i].darray.index(gui_control.locked_dim_information[dim_name][2])
            last = parent.panelDM.dim[i].darray.index(gui_control.locked_dim_information[dim_name][3])

            # Set dimension to saved values
            parent.panelDM.dim[i].first_scl.set( first )
            parent.panelDM.dim[i].last_scl.set( last )
            #parent.panelDM.dim[i].first_scl.set( gui_control.locked_dim_information[dim_name][0] )
            #parent.panelDM.dim[i].last_scl.set( gui_control.locked_dim_information[dim_name][1] )

            parent.panelDM.dim[i].stride = gui_control.locked_dim_information[dim_name][4]
            e = parent.panelDM.dim[i].comb._entryfield.get()
            e = e[:-1]+str(parent.panelDM.dim[i].stride)
            parent.panelDM.dim[i].comb._entryfield.setentry(e)

            parent.panelDM.dim[i].opt.configure( menubutton_text=gui_control.locked_dim_information[dim_name][5] )

            # Show the locked icon
            parent.panelDM.dim[i].canvas_lockicon.delete("all")
            parent.panelDM.dim[i].canvas_lockicon_toggle = 1
            parent.panelDM.dim[i].canvas_lockicon.create_image(0,0, anchor=Tkinter.NW, image=parent.panelDM.dim[i].lock )
      except: 
         parent.panelDM.dim[i].canvas_lockicon.delete("all")
         parent.panelDM.dim[i].canvas_lockicon_toggle = 0
         parent.panelDM.dim[i].canvas_lockicon.create_image(0,0, anchor=Tkinter.NW, image=parent.panelDM.dim[i].tiltedlock )
         pass

def set_lat_lon(parent,R0,R1):
   m=parent.panelDM.grid.getMesh()
   ## generates the lon/lat values to be displayed


   if R0 is not None:
      mlat,Mlat=vcs.minmax(m[:,0])
      if Mlat-mlat>170:
         lats=numpy.arange(-90.,90.5,.5)
      else:
         lats=numpy.arange(int(mlat)-1,int(Mlat)+1,.5)
      lats=lats.tolist()
      Lats=[]
      for l in lats:
         Lats.append('%.17g' % l)
      R0.darray=Lats
      R0.first_scl.configure(from_=0,to=len(lats)-1)
      R0.last_scl.configure(from_=0,to=len(lats)-1)
      R0.text_flb.set(str(lats[0]))
      R0.text_llb.set(str(lats[-1]))
      R0.first_scl.set(0)
      R0.last_scl.set(R0.darray.index(Lats[-1]))
      R0.srl.setlist( R0.darray )
      R0.npts=range(len(lats))

   if R1 is not None:   
      mlon,Mlon=vcs.minmax(m[:,1])
      if Mlon-mlon>350:
         lons=numpy.arange(-360,360.5,.5).tolist()
         Lons=[]
         for l in lons:
            Lons.append('%.17g' % l)
         R1.darray=Lons
         R1.first_scl.configure(from_=0,to=len(lons)-1)
         R1.last_scl.configure(from_=0,to=len(lons)-1)
         R1.first_scl.set(R1.darray.index('-180'))
         R1.last_scl.set(R1.darray.index('180'))
         R1.text_flb.set('-180')
         R1.text_llb.set('180')
         can_wrap=1
      else:
         lons=numpy.arange(int(mlon)-1,int(Mlon)+1,.5).tolist()
         Lons=[]
         for l in lons:
            Lons.append('%.17g' % l)
         R1.darray=Lons
         R1.first_scl.configure(from_=0,to=len(lons)-1)
         R1.last_scl.configure(from_=0,to=len(lons)-1)
         R1.first_scl.set(R1.darray.index(Lons[0]))
         R1.last_scl.set(R1.darray.index(Lons[-1]))
         R1.text_flb.set(str(Lons[0]))
         R1.text_llb.set(str(Lons[-1]))
         can_wrap=0


      R1.srl.setlist( R1.darray )
      R1.npts=range(len(lons))
      ## Meridian stuff
      if parent.menu.meridian_flg==1 and can_wrap:
         try:
            R1.first_scl.set(R1.darray.index(-180))
            R1.last_scl.set(R1.darray.index(180))
            R1.text_flb.set('-180')
            R1.text_llb.set('180')
         except:
            pass
      elif parent.menu.meridian_flg==0 and can_wrap:
         try:
            R1.first_scl.set(R1.darray.index(0))
            R1.last_scl.set(R1.darray.index(360))
            R1.text_flb.set('0')
            R1.text_llb.set('360')
         except:
            pass
#---------------------------------------------------------------------------------
# Show dimensions 1 to ndim, including their scroll-bar, title, choice button, etc.
#---------------------------------------------------------------------------------
def _srl_var_dim1_to_ndim( parent, slab = None, dim_index = None, new_axis = None ):
   import string
   dim_order = {}
   if slab is None:
      try:
         g = parent.panelDM.fid2
         a = g[parent.panelDM.var3]
         try:
            grd=a.getGrid()
         except:
            grd=None
         gl = list( g.listdimension( parent.panelDM.var3 ) )
         parent.panelDM.ndim = len( gl )
         if dim_index != None:
            hold = gl[dim_index]
            for i in range(len(gl)):
               if gl[i] == parent.panelDM.dim[dim_index].dim_name:
                  hold_index = i
                  break
            gl[dim_index] =  parent.panelDM.dim[dim_index].dim_name
            gl[hold_index] = hold
         if isinstance(grd,cdms2.gengrid.AbstractGenericGrid):
            parent.panelDM.ndim += 1              

      except:  # Must be an axis
         a = g[parent.panelDM.var3]
         try:
            grd=a.getGrid()
         except:
            grd=None
         gl = []
         gl.append( a.id )
         parent.panelDM.ndim = 1
         if isinstance(a,cdms2.auxcoord.AbstractCoordinateAxis):
            parent.panelDM.ndim = 2
   else:
      gl = slab.listdimnames()
      parent.panelDM.ndim = len( gl )
      try:
         grd=slab.getGrid()
      except:
         grd=None
      if isinstance(grd,cdms2.gengrid.AbstractGenericGrid):
         parent.panelDM.ndim += 1              

   parent.panelDM.grid=grd
   if parent.menu.fortran_order_flg:
      gl.reverse()
   #
   multi_dim_found=0
   nn=parent.panelDM.ndim
   try:
      grd_ax=grd.getAxisList()
   except:
      grd_ax=['','']
   if isinstance(grd,cdms2.gengrid.AbstractGenericGrid):
      nn-=1

   for i in range( nn ):
      parent.panelDM.dim[i].new_axis = None
      parent.panelDM.dim[i].new_weights = None
      if slab is None:
         try:
            b = list( g.dimensionarray( gl[i] ) )
            varobj = parent.panelDM.fid2.variables[ parent.panelDM.var3 ]
            axis_looked = varobj.getAxis(i)
            can_wrap = axis_looked.isCircular()
         except Exception,err:		# Must be an axis
            a_data = a[:]
            b = list( a_data )
            v_data = cdms2.MV2.masked_array(a_data, attributes=a.attributes,
                                          axes=(a,), id = a.id)
            axis_looked=v_data.getAxis(i)
            can_wrap = axis_looked.isCircular()
      else:
         axis_looked=slab.getAxis(i)
         b = slab.getdimattribute( i, 'values' )
         can_wrap = axis_looked.isCircular()
      if ( (dim_index == None) or (dim_index == i) ):
         vv = []
         vv_index = []
         si = 0
         ei = -1
         #
         if slab is None:
            try:
               u = g.getdimensionunits( gl[i] )
               d = g.dimensionarray( gl[i] )
            except:
               u = a.units      # Must be an axis
               d = a[:]
         else:
            u = slab.getdimattribute( i, 'units' )
            d = slab.getdimattribute( i, 'values' )

         # Special search for atmospheric chemistry folks. If the axis has the 
         # attribute "coord_labels", then use it instead of the index values.
         try:
            axis = g.getAxis( gl[i] )
         except:
            axis = slab.getAxis( i )

         # Test for mesh for irregular grids, needed
         try:
            m=grd.getMesh()
            nomesh=0
         except:
            nomesh=1
         if isinstance(grd,cdms2.gengrid.AbstractGenericGrid) and axis in grd_ax and nomesh==0:
##             print 'Ok we are dealing with a genereic grid'

            ## Figures out row0 and 1
            R0=parent.panelDM.dim[i+multi_dim_found]
            R1=parent.panelDM.dim[i+multi_dim_found+1]

            ## Prepares the index
            vv_index=range(len(axis))

            ## Stores indices
            R0.idarray=vv_index
            R0.stride=1

            R1.idarray=None
            R1.stride=1

            ## Sets 
               
            m=grd.getMesh()
            set_lat_lon(parent,R0,R1)
            
            estr=R0.text_flb.get()+' : '+R0.text_llb.get()
##             print 'Should be setting to:',estr
            R0.comb.setentry(estr)
            estr=R1.text_flb.get()+' : '+R1.text_llb.get()
            R1.comb.setentry(estr)

            if parent.menu.fortran_order_flg:
               dname = gui_control.dim_axis[i+multi_dim_found] 
            else:
               dname = gui_control.dim_axis[len( gl ) - i-multi_dim_found - 1] 
            dname0='Virtual-Latitude ('+gl[i]+')'
            dname1='Virtual-Longitude ('+gl[i]+')'

            R0.menu.configure( text = dname0, background = 'lightgrey')
            R1.menu.configure( text = dname1, background = 'lightgrey')
            
            ## Set name of "real" axis
            R0.dim_lab.set( dname0 )
            R1.dim_lab.set( dname1)
            R0.contains_bnds = 0
            R1.contains_bnds = 0

            ## increase multi_dim_found
            multi_dim_found+=1
         elif isinstance(grd,cdms2.hgrid.AbstractCurveGrid) and axis in grd_ax and nomesh==0:
            R0=parent.panelDM.dim[i+multi_dim_found]
            ## Prepares the index
            vv_index=range(len(axis))
            ## Stores indices
            R0.idarray=vv_index
            R0.stride=1
            if axis==grd_ax[0]:
               set_lat_lon(parent,R0,None)
               dname0='Virtual-Latitude ('+gl[i]+')'
            else:
               set_lat_lon(parent,None,R0)
               dname0='Virtual-Longitude ('+gl[i]+')'
            estr=R0.text_flb.get()+' : '+R0.text_llb.get()
##             print 'Should be setting to:',estr
            R0.comb.setentry(estr)
            R0.dim_lab.set( dname0 )
            R0.contains_bnds = 0
            R0.menu.configure( text = dname0, background = 'lightgrey')
         else:
##             print 'Normal axis'
            found_const_labels = 0
            try:
               cl = g(axis.coord_labels)[0:len(axis)]
               found_const_labels = 1
            except:
               pass
            if (found_const_labels == 1):
               for y in range(len(cl)):
                  vv_index.append(y)
                  vs="".join(cl[y])
##                   vs = string.replace(string.replace(vs, ",",""), " ", "")[1:-1]
                  vv.append(string.join(vs,''))
            elif ((string.lower(gl[i]) in gui_control.time_alias) or (string.lower(gl[i][:4]) == 'time') or axis.isTime() ):
               try:
                  #cal = g.getglobal('calendar')
                  cal=axis.calendar
               except:
                  cal = 'no_calendar'
               if cal not in gui_control.calendar_list.keys():
                  cal = 'no_calendar'
               #
##                print '********   cal = ', cal, gui_control.calendar_list[ cal ]
##                print '********** u = ', u,axis.info()
               try:
                  tc=axis.asComponentTime()
               except:
                  tc=None
               if (u is not None) or (u != ' '):
                  cdtime.DefaultCalendar = gui_control.calendar_list[ cal ]
                  for j in range(len(d)):
                     vv_index.append(j)
                     try:
                        vv.append('%s' % tc[j] )
                     except:
                        try:
                           vv.append('%s' % cdtime.reltime(d[j],u).tocomp() )
                        except:
                           vv.append( "%.17g" % j )
               else:
                  for j in range(len(d)):
                     vv_index.append(j)
                     vv.append( "%.17g" % j )
            elif ((string.lower(gl[i]) in gui_control.longitude_alias or axis.isLongitude()) and (can_wrap == 1)):
               l = len( d )
               #
               #********      Generate values to -360 or 360
               p = 2
               mc = 1
               c = mc * ( d[ -1 ] - d[ 0 ] )
               t = d[ -p ] - c
               vv.insert( 0, "%.17g" % t )
               j = 0
               vv_index.append(j)

               if (d[ 0 ] < d[ -1 ]): # Generate Longitude values for ascending order
                  while t > -360.0:
                     p += 1
                     if p > (l-1):
                        p = 1
                        mc += 1
                        c = mc * ( d[ -1 ] - d[ 0 ] )
                     t = d[ -p ] - c
                     vv.insert( 0, "%.17g" % t )
                     j += 1
                     vv_index.append(j)
               else: # Generate Longitude values for descending order
                  while t < 360.0:
                     p += 1
                     if p > (l-1):
                        p = 1
                        mc += 1
                        c = mc * ( d[ -1 ] - d[ 0 ] )
                     t = d[ -p ] - c
                     vv.insert( 0, "%.17g" % t )
                     j += 1
                     vv_index.append(j)
               #
               #******** 	   Save original values
               si = j+1
               ei = si + l -1
               for t in d:
                  vv.append( "%.17g" % t)
                  j += 1
                  vv_index.append(j)
               #
               #********      Generate values to -360 or 360
               p = 1
               mc = 1 
               c = mc * ( d[ -1 ] - d[ 0 ] )
               t = c + d[ p ]
               vv.append( "%.17g" % t )
               j += 1
               vv_index.append(j)

               if (d[ 0 ] < d[ -1 ]): # Generate Longitude values for ascending order
                  while t < 360.0:
                     p += 1 
                     if p > (l-1):
                        p = 1 
                        mc += 1
                        c = mc * ( d[ -1 ] - d[ 0 ] )
                     t = c + d[ p ]
                     vv.append( "%.17g" % t )
                     j += 1
                     vv_index.append(j)
               else: # Generate Longitude values for descending order
                  while t > -360.0:
                     p += 1
                     if p > (l-1):
                        p = 1
                        mc += 1
                        c = mc * ( d[ -1 ] - d[ 0 ] )
                     t = c + d[ p ]
                     vv.append( "%.17g" % t )
                     j += 1
                     vv_index.append(j)
            else:
               k = 0
               for y in b:
                  vv_index.append(k)
                  k += 1
                  vv.append( "%.17g" % y )
            
         
            #
            
            f=str(vv[si])
            l=str(vv[ei])
   
            parent.panelDM.dim[i+multi_dim_found].dim_lab.set( gl[i] )
            parent.panelDM.dim[i+multi_dim_found].darray = vv
            parent.panelDM.dim[i+multi_dim_found].idarray = vv_index
            parent.panelDM.dim[i+multi_dim_found].rdarray = d
            parent.panelDM.dim[i+multi_dim_found].stride = 1

            if axis.getBounds() is None:
               parent.panelDM.dim[i+multi_dim_found].contains_bnds = 0
            else:
               parent.panelDM.dim[i+multi_dim_found].contains_bnds = 1

            if ((string.lower(gl[i]) in gui_control.longitude_alias or axis.isLongitude()) and (can_wrap == 1)):
               f = parent.panelDM.dim[i+multi_dim_found].darray[si]
               l = parent.panelDM.dim[i+multi_dim_found].darray[ei]
               parent.panelDM.dim[i+multi_dim_found].text_flb.set( f[0:11] )
               parent.panelDM.dim[i+multi_dim_found].text_llb.set( l[0:11] )
            else:
               parent.panelDM.dim[i+multi_dim_found].text_flb.set( f[0:11] )
               parent.panelDM.dim[i+multi_dim_found].text_llb.set( l[0:11] )
            parent.panelDM.dim[i+multi_dim_found].first_scl.configure(from_ = 0, to = (len(vv) - 1) )
            parent.panelDM.dim[i+multi_dim_found].last_scl.configure( from_ = 0, to = (len(vv) - 1) )
            estr = str(f) + ' : ' + str(l) + ' by ' + str( parent.panelDM.dim[i].stride )
            parent.panelDM.dim[i+multi_dim_found].comb.setentry(estr)
            parent.panelDM.dim[i+multi_dim_found].comb.component( 'listbox' ).configure( selectbackground = 'lightgrey' )

            if ((string.lower(gl[i]) in gui_control.longitude_alias or axis.isLongitude()) and (can_wrap == 1)) or (isinstance(grd,cdms2.hgrid.AbstractCurveGrid) and axis == grd_ax[1] and nomesh==0):
               # Meridian was set from the "Option" menu
               try:
                  if parent.menu.meridian_flg == 1:
                     si = parent.panelDM.dim[i+multi_dim_found].darray.index( '-180' )
                     ei = parent.panelDM.dim[i+multi_dim_found].darray.index( '180' )
                     parent.panelDM.dim[i+multi_dim_found].comb.setentry("-180 : 180 by 1")
                     parent.panelDM.dim[i+multi_dim_found].text_flb.set( -180 )
                     parent.panelDM.dim[i+multi_dim_found].text_llb.set( 180 )
                  elif parent.menu.meridian_flg == 2:
                     si = parent.panelDM.dim[i+multi_dim_found].darray.index( '0' )
                     ei = parent.panelDM.dim[i+multi_dim_found].darray.index( '360' )
                     parent.panelDM.dim[i+multi_dim_found].comb.setentry("0 : 360 by 1")
                     parent.panelDM.dim[i+multi_dim_found].text_flb.set( 0 )
                     parent.panelDM.dim[i+multi_dim_found].text_llb.set( 360 )
               except:
                  pass
               parent.panelDM.dim[i+multi_dim_found].first_scl.set( si )
               parent.panelDM.dim[i+multi_dim_found].last_scl.set( ei )
               af = si
               al = ei
            else:
               parent.panelDM.dim[i+multi_dim_found].first_scl.set( vv_index[si] )
               parent.panelDM.dim[i+multi_dim_found].last_scl.set( vv_index[ei] )
               af = vv_index[si] 
               al =  vv_index[ei]
            #
            # Set the number on points
            parent.panelDM.dim[i+multi_dim_found].npts = []
            if (af <= al):
               for j in range( af, (al+1)  ):
                  parent.panelDM.dim[i+multi_dim_found].npts.append(j)
            else:
               for j in range( al, (af+1)  ):
                  parent.panelDM.dim[i+multi_dim_found].npts.insert( 0, j )

            #
            # Add the menu toggle items
            #
            if parent.menu.fortran_order_flg:
               dname = gui_control.dim_axis[i+multi_dim_found] + ': ' + gl[i]
            else:
               dname = gui_control.dim_axis[len( gl ) - i-multi_dim_found - 1] + ': ' + gl[i]

            parent.panelDM.dim[i+multi_dim_found].menu.configure( text = dname, background = 'lightgrey')

      
      if isinstance(grd,cdms2.gengrid.AbstractGenericGrid) and axis in grd_ax and nomesh==0:
##          print "Gengrid crap here"
         R0.men1.index_flg = 0
         R0.men1.sub_index = Tkinter.IntVar()
         R0.men1.sub_index.set( R0.men1.index_flg )
         R0.men1.insert_checkbutton(1, label = (gl[i]+'_Index'),
                                    selectcolor = gui_color.eight,
                                    variable = R0.men1.sub_index,
                                    command = gui_control.Command(evt_menu_index, parent, i+multi_dim_found, 1 )
                                    )
         R0.men1.raw_flg = 0
         R0.men1.index_flg = 0

         
         R1.men1.raw_flg = 0
         R1.men1.sub_index = Tkinter.IntVar()
         R1.men1.sub_index.set( R1.men1.index_flg )

         ## Not sure about the plus 1 double check !!!!
         R1.men1.insert_checkbutton(1, label = (gl[i]+'_Index'),
                                    selectcolor = gui_color.eight,
                                    variable = R1.men1.sub_index,
                                    command = gui_control.Command(evt_menu_index, parent, i+multi_dim_found+1, 1 )
                                    )

         R0.dim_name='Row0'
         R1.dim_name='Row1'
##          set_region_index_values( parent, R0.dim_name,
##                                   parent.menu.save_region, i+multi_dim_found)
##          set_region_index_values( parent, R1.dim_name,
##                                   parent.menu.save_region, i+multi_dim_found+1)
         R0.top_frame.pack( side='top' )
         R1.top_frame.pack( side='top' )

         # not sure what this is for....
         parent.panelDM.dim[i].pts = []


         ## increment multi dims stuff
         multi_dim_found+=1

      else:
      
      #      parent.panelDM.dim[i].men1.index_flg = 0
         # delete all menu items for each dimension
         for x in gl:
            parent.panelDM.dim[i+multi_dim_found].men1.delete( 0, 'end' ) 
         #
##          print '****** I am here i = ', i
         parent.panelDM.dim[i+multi_dim_found].men1.index_flg = 0
         parent.panelDM.dim[i+multi_dim_found].men1.sub_index = Tkinter.IntVar()
         parent.panelDM.dim[i+multi_dim_found].men1.sub_index.set( parent.panelDM.dim[i+multi_dim_found].men1.index_flg )
         parent.panelDM.dim[i+multi_dim_found].men1.insert_checkbutton(1, label = (gl[i]+'_Index'),
                               selectcolor = gui_color.eight,
                               variable = parent.panelDM.dim[i+multi_dim_found].men1.sub_index,
                               command = gui_control.Command(evt_menu_index, parent, i+multi_dim_found, 1 )
                                )
         parent.panelDM.dim[i+multi_dim_found].men1.raw_flg = 0

         if ((string.lower(gl[i]) in gui_control.time_alias) or (string.lower(gl[i][:4]) == 'time')):
            parent.panelDM.dim[i+multi_dim_found].men1.sub_raw = Tkinter.IntVar()
            parent.panelDM.dim[i+multi_dim_found].men1.sub_raw.set( parent.panelDM.dim[i+multi_dim_found].men1.raw_flg )
            parent.panelDM.dim[i+multi_dim_found].men1.insert_checkbutton(2, label = (gl[i]+'_Raw'),
                              selectcolor = gui_color.eleven,
                              variable = parent.panelDM.dim[i+multi_dim_found].men1.sub_raw,
                              command = gui_control.Command(evt_menu_raw, parent, i )
                               )
         parent.panelDM.dim[i+multi_dim_found].dim_name = gl[i]


         #
         # Get Axis Values
         #
         parent.panelDM.dim[i+multi_dim_found].men1.insert_separator( 3 )
         parent.panelDM.dim[i+multi_dim_found].men1.insert_command( 4,
                            label = 'Get Axis Values',
                            command = gui_control.Command(get_axis_values, parent, i+multi_dim_found)
                           )
         #
         # Get Axis Weight Values
         #
         parent.panelDM.dim[i+multi_dim_found].men1.insert_command( 5,
                            label = 'Get Axis Weight Values',
                            command = gui_control.Command(get_axis_weight_values, parent, i+multi_dim_found)
                           )
         #
         # Replace Axis Values
         #
         parent.panelDM.dim[i+multi_dim_found].men1.insert_command( 6,
                            label = 'Replace Axis Values',
                            command = gui_control.Command(replace_axis_values, parent, i+multi_dim_found)
                           )
         #
         # Add the other menu names
         #
         parent.panelDM.dim[i+multi_dim_found].men1.insert_separator( 7 )
         #
         parent.panelDM.dim[i+multi_dim_found].men1.reordermenu = Tkinter.Menu( parent.panelDM.dim[i].men1, tearoff=0 )
         k = gui_control.ndim+1
         K = gui_control.ndim+1
         l = 0
         for x in gl:
            if parent.menu.fortran_order_flg:
               dname = gui_control.dim_axis[l] + ': ' + gl[l]
##                print dname,gui_control.dim_axis
            else:
               dname = gui_control.dim_axis[len( gl ) - l - 1] + ': ' + gl[l]
##                print dname,gui_control.dim_axis
            parent.panelDM.dim[i+multi_dim_found].men1.reordermenu.insert_command(
               k, label = dname, 
               command = gui_control.Command(evt_transpose_dimensions, i+multi_dim_found, gl[i], (k-K), gl[k-K], parent)
               )
            k += 1
            l += 1
         parent.panelDM.dim[i+multi_dim_found].men1.add_cascade( label = 'Re-Order Dimensions', menu=parent.panelDM.dim[i+multi_dim_found].men1.reordermenu)
         #
         parent.panelDM.dim[i+multi_dim_found].srl.setlist( parent.panelDM.dim[i+multi_dim_found].darray )
         #
         if parent.panelDM.dim[i+multi_dim_found].contains_bnds == 0:
            parent.panelDM.dim[i+multi_dim_found].opt.setitems(gui_control.dimchlst2)
         else:
            ## Need to do this in case it had been reset before!
            parent.panelDM.dim[i+multi_dim_found].opt.setitems(gui_control.dimchlst)
         parent.panelDM.dim[i+multi_dim_found].opt.configure( menubutton_text=gui_control.dimchlst[0][0:3] )
         #
         parent.panelDM.dim[i+multi_dim_found].pts = []
         #
         # Set the region selection
         set_region_index_values( parent, parent.panelDM.dim[i+multi_dim_found].dim_name,
                                  parent.menu.save_region, i+multi_dim_found)
         parent.panelDM.dim[i+multi_dim_found].top_frame.pack( side='top' )
      # Adds labels function
      parent.panelDM.dim[i+multi_dim_found].men1.insert_separator( 9 )
      parent.panelDM.dim[i+multi_dim_found].men1.insert_command( 10,
                                                                 label = 'Generates Labels (for use with tickmarks)',
                                                                 command = gui_control.Command(generate_labels_list, parent, i+multi_dim_found,axis_looked)
                                                                 )
      # Reset the locked information in the dimension
      set_locked_information( parent, i )
      dim_order[gui_control.dim_axis[parent.panelDM.ndim-i-1]] = parent.panelDM.dim[i].dim_name
         
   subset_locked_dim_info = []
   for x in gui_control.locked_dim_information.keys():
       if x in dim_order.values(): subset_locked_dim_info.append(x)
   do_transpose = True;
   for x in subset_locked_dim_info:
      if x not in dim_order.values():
         do_transpose = False
         break

   if subset_locked_dim_info == []: do_transpose = False

   true_locked_order = return_correct_locked_order(dim_order, parent)
   if ((true_locked_order != {}) and (do_transpose == True)):
      for i in range(parent.panelDM.ndim-1):
          dim_search = true_locked_order[gui_control.dim_axis[i]]
          search_ct, k = return_name_order_index( dim_search, dim_order, i, parent)
          order_ct, l = return_name_order_index( dim_order[gui_control.dim_axis[i]], dim_order, i, parent)
          if ((dim_search != dim_order[gui_control.dim_axis[i]]) and (i != k)):
             evt_transpose_dimensions(search_ct, dim_search, order_ct, dim_order[gui_control.dim_axis[i]], parent)
             dim_order[gui_control.dim_axis[k]] = dim_order[gui_control.dim_axis[i]]
             dim_order[gui_control.dim_axis[l]] = dim_search

   # New variable, so re-initialize things here. "
#   parent.boxfill_level1 = None; parent.boxfill_level2 = None; parent.boxfill_legend = None
#   parent.boxfill_color1 = None; parent.boxfill_color2 = None;

   return 

def return_name_order_index(dim_name, dim_order, i, parent):
    for j in range(parent.panelDM.ndim):
        if dim_name == dim_order[gui_control.dim_axis[j]]: break
    search_ct = parent.panelDM.ndim-j-1
    return search_ct, j

def return_correct_locked_order(dim_order, parent):
    for x in gui_control.locked_dim_order.values():
        found = True
        for y in dim_order.values():
            if y not in x.values():
               found = False
               break
        if found: return x
    return {}

def return_latitude_region_values( region ):
   if region == 0: return -90.0, 90.0
   elif region == 1: return -40.0, 40.0
   elif region == 2: return -90.0, -58.0
   elif region == 3: return 62.0, 90.0
   elif region == 4: return 2.0, 70.0
   elif region == 5: return -46.0, -2.0
   elif region == 6: return 30.0, 70.0
   elif region == 7: return -58.0, 26.0
   elif region == 8: return 10.0, 70.0
   elif region == 9: return 2.0, 58.0
   elif region == 10: return 2.0, 58.0
   elif region == 11: return -58.0, 14.0
   elif region == 12: return -58.0, -2.0
   elif region == 13: return -58.0, -2.0
   elif region == 14: return -26.0, 26.0
   elif region == 15: return -26.0, 26.0
   else: return -90.0, 90.0

def return_longitude_region_values( region ):
   if region == 0: return -180.0, 180.0
   elif region == 1: return -45.0, 70.0
   elif region == 2: return -180.0, 180.0
   elif region == 3: return -180.0, 180.0
   elif region == 4: return 40.0, 175.0
   elif region == 5: return 100.0, 175.0
   elif region == 6: return -25.0, 40.0
   elif region == 7: return 25.0, 115.0
   elif region == 8: return -180.0, -50.0
   elif region == 9: return -80.0, 20.0
   elif region == 10: return 120.0, 280.0
   elif region == 11: return -90.0, -30.0
   elif region == 12: return -80.0, 20.0
   elif region == 13: return 120.0, 280.0
   elif region == 14: return -80.0, 20.0
   elif region == 15: return 120.0, 280.0
   else: return -180.0, 180.0

def set_region_index_values( parent, dim_name, region, dim_index):
##    grd=parent.panelDM.grid
##    try:
##       grd_ax=grd.getAxisList()
##    except:
##       grd_ax=['','']
   if region == 0: return
   
##    if isinstance(grd,(cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid)) and axis in grd_ax:
##       if curve==0:
##          af,al = return_latitude_region_values( region )
##       else:
##          af,al = return_longitude_region_values( region )
   if string.lower(dim_name) in gui_control.latitude_alias:
      af,al = return_latitude_region_values( region )
   elif string.lower(dim_name) in gui_control.longitude_alias:
      af,al = return_longitude_region_values( region )
   else:
      return

   # find zone
   zmin = string.atof(parent.panelDM.dim[dim_index].darray[0])
   zmax = string.atof(parent.panelDM.dim[dim_index].darray[-1])
   fdarray = []
   z1 = string.atof( parent.panelDM.dim[dim_index].darray[0] )
   for i in range(1,len(parent.panelDM.dim[dim_index].darray)):
      if (zmax - zmin) > 0:           # accending order
         z2 = string.atof( parent.panelDM.dim[dim_index].darray[i] )
         fdarray.append( z1 + (z2 - z1) * 0.5 )
         z1 = z2
      else:                           # descending order
         z2 = string.atof( parent.panelDM.dim[dim_index].darray[i] )
         fdarray.append( z2 + (z1 - z2) * 0.5 )
         z1 = z2

   if (zmax - zmin) > 0:           # accending order
      iaf = 0
      for i in range( len(parent.panelDM.dim[dim_index].darray) ):
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
             iaf = len(parent.panelDM.dim[dim_index].darray) - 1
             break
      ial = 0
      for i in range( len(parent.panelDM.dim[dim_index].darray) ):
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
             ial = len(parent.panelDM.dim[dim_index].darray) - 1
             break
   elif (zmax - zmin) < 0:         # descending order
      iaf = 0
      for i in range( len(parent.panelDM.dim[dim_index].darray) ):
          az1 = fdarray[i]
          try:
             az2 = fdarray[i+1]
          except:
             az2 = az1
          if (af >= fdarray[0]):
             iaf = i
             break
          elif (af > az1):
             iaf = i
             break
          elif (af <= fdarray[-1]):
             iaf = len(parent.panelDM.dim[dim_index].darray) - 1
             break
      ial = 0
      for i in range( len(parent.panelDM.dim[dim_index].darray) ):
          az1 = fdarray[i]
          try:
             az2 = fdarray[i+1]
          except:
             az2 = az1
          if (al >= fdarray[0]):
             ial = i
             break
          elif (al >= az1):
             ial = i
             break
          elif (al <= fdarray[-1]):
             ial = len(parent.panelDM.dim[dim_index].darray) - 1
             break
   elif (zmax - zmin) == 0:           # Nothing to do
     return
   #
   cf = string.atof( parent.panelDM.dim[dim_index].darray[iaf] )
   cl = string.atof( parent.panelDM.dim[dim_index].darray[ial] )
   #
   parent.panelDM.dim[dim_index].first_scl.set( iaf )
   parent.panelDM.dim[dim_index].last_scl.set( ial )

def evt_menu_index( parent, dim_index, called_from ):
   #
##    print 'In index'
   ## Test for mesh
   try:
      m=parent.panelDM.grid.getMesh()
      nomesh=0
   except:
      nomesh=1
   if isinstance(parent.panelDM.grid,cdms2.gengrid.AbstractGenericGrid) and parent.panelDM.dim[dim_index].dim_name in ['Row0','Row1'] and nomesh==0:
      ## Figures out which one is lat and which one is lon
      if parent.panelDM.dim[dim_index].dim_name=='Row0': ## dealing wit longitude one
         R0=parent.panelDM.dim[dim_index]
         R1=parent.panelDM.dim[dim_index+1]
      else:
         R0=parent.panelDM.dim[dim_index-1]
         R1=parent.panelDM.dim[dim_index]

      if R0.men1.index_flg == 1: ## We're going back to lat/lon
         R0.men1.index_flg=0
         R1.men1.index_flg=0
         i0=R0.first_scl.get()
         i1=R0.last_scl.get()
         set_lat_lon(parent,R0,R1)
         dname0='Virtual-Latitude ('+R0.dim_lab.get()+')'
         dname1='Virtual-Longitude ('+R0.dim_lab.get()+')'
      else: ## We're going to index mode
         R0.men1.index_flg=1
         R1.men1.index_flg=1
         vv = R0.idarray
         R0.srl.setlist( vv )
         R1.srl.setlist( vv )

         R0.first_scl.configure(from_ = 0, to = (len(vv) - 1) )
         R0.last_scl.configure( from_ = 0, to = (len(vv) - 1) )
         R1.first_scl.configure(from_ = 0, to = (len(vv) - 1) )
         R1.last_scl.configure( from_ = 0, to = (len(vv) - 1) )
         R1.text_flb.set('0')
         R1.text_llb.set(str(len(vv)-1))
         R0.text_flb.set('0')
         R0.text_llb.set(str(len(vv)-1))
         R0.first_scl.set(0)
         R0.last_scl.set(len(vv)-1)
         R1.first_scl.set(0)
         R1.last_scl.set(len(vv)-1)
         npts=range(len(vv))
         R1.npts=npts
         R0.npts=npts
         dname0=string.split(R0.dim_lab.get(),'(')[1]
         dname0=string.split(dname0,')')[:-1]
         dname1=dname0=string.join(dname0)
         
      ## Set name of "real" axis
      R0.dim_lab.set( dname0 )
      R1.dim_lab.set( dname1)

   elif isinstance(parent.panelDM.grid,cdms2.hgrid.AbstractCurveGrid) and nomesh==0:
      try:
         axis = parent.panelDM.fid2.getAxis( parent.panelDM.dim[dim_index].dim_name )
      except Exception,err:
         axis = parent.panelDV.lst1[ parent.panelDV.selected ].getAxis( dim_index )
      if axis in parent.panelDM.grid.getAxisList():
         R0=parent.panelDM.dim[dim_index]
         try:
            m=parent.panelDM.grid.getMesh()
##             print m
            nomesh=0
         except:
            R0.men1.index_flg=0
            nomesh=1
##             print "Can't do lat/lon reset to index"
         if R0.men1.index_flg == 1: ## We're going back to lat/lon
            R0.men1.index_flg=0
            i0=R0.first_scl.get()
            i1=R0.last_scl.get()
            if axis==parent.panelDM.grid.getAxisList()[0]:
               set_lat_lon(parent,R0,None)
               dname0='Virtual-Latitude ('+R0.dim_lab.get()+')'
            else:
               set_lat_lon(parent,None,R0)
               dname0='Virtual-Longitude ('+R0.dim_lab.get()+')'
         else: ## We're going to index mode
            R0.men1.index_flg=1
            vv = R0.idarray
            R0.srl.setlist( vv )

            R0.first_scl.configure(from_ = 0, to = (len(vv) - 1) )
            R0.last_scl.configure( from_ = 0, to = (len(vv) - 1) )
            R0.text_flb.set('0')
            R0.text_llb.set(str(len(vv)-1))
            R0.first_scl.set(0)
            R0.last_scl.set(len(vv)-1)
            npts=range(len(vv))
            R0.npts=npts
            dname0=string.split(R0.dim_lab.get(),'(')[1]
            dname0=string.split(dname0,')')[:-1]
         R0.dim_lab.set( dname0)

   else:
      try:
         axis = parent.panelDM.fid2.getAxis( parent.panelDM.dim[dim_index].dim_name )
      except Exception,err:
         axis = parent.panelDV.lst1[ parent.panelDV.selected ].getAxis( dim_index )
      if parent.panelDM.dim[dim_index].men1.index_flg == 1:
         if called_from == 1:
            parent.panelDM.dim[dim_index].men1.index_flg = 0
         #
         if parent.panelDM.dim[dim_index].men1.raw_flg == 1:
            return
         #
         if (string.lower( parent.panelDM.dim[dim_index].dim_name) in gui_control.longitude_alias or axis.isLongitude()):
            dlen = len(parent.panelDM.dim[dim_index].darray)
            f = parent.panelDM.dim[dim_index].rdarray[ parent.panelDM.dim[dim_index].first_scl.get() ]
            l = parent.panelDM.dim[dim_index].rdarray[ parent.panelDM.dim[dim_index].last_scl.get() ]
            # Meridian was set from the "Option" menu
            try:
               if parent.menu.meridian_flg == 1:
                  f = -180
                  l = 180
               elif parent.menu.meridian_flg == 2:
                  f = 0
                  l = 360
            except: pass
            vf = "%.17g" % f
            vl = "%.17g" % l
            f = 0
            while vf != parent.panelDM.dim[dim_index].darray[ f ]: f += 1
            l = 0
            while vl != parent.panelDM.dim[dim_index].darray[ l ]: l += 1
            lf = ( "%.17g" % parent.panelDM.dim[dim_index].rdarray[ 0 ] )
            ll = ("%.17g" %  parent.panelDM.dim[dim_index].rdarray[ -1 ] )
            parent.panelDM.dim[dim_index].first_scl.configure(from_ = 0, to = (dlen - 1) )
            parent.panelDM.dim[dim_index].last_scl.configure( from_ = 0, to = (dlen - 1) )
         else:
            f = parent.panelDM.dim[dim_index].first_scl.get()
            l = parent.panelDM.dim[dim_index].last_scl.get()
            lf = parent.panelDM.dim[dim_index].darray[ f ]
            ll = parent.panelDM.dim[dim_index].darray[ l ]
   #         lf = parent.panelDM.dim[dim_index].darray[ 0 ]
   #         ll = parent.panelDM.dim[dim_index].darray[len(parent.panelDM.dim[dim_index].darray) - 1]
            parent.panelDM.dim[dim_index].first_scl.set( f )
            parent.panelDM.dim[dim_index].last_scl.set( f )
         parent.panelDM.dim[dim_index].srl.setlist( parent.panelDM.dim[dim_index].darray )
         parent.panelDM.dim[dim_index].menu.configure( background = 'lightgrey' )
         parent.panelDM.dim[dim_index].comb.component( 'listbox' ).configure( selectbackground = 'lightgrey' )
         parent.panelDM.dim[dim_index].dim_lab.set( parent.panelDM.dim[dim_index].dim_name )
      else:
         if called_from == 1:
            parent.panelDM.dim[dim_index].men1.index_flg = 1
         #
         if parent.panelDM.dim[dim_index].men1.raw_flg == 1:
            return
         #
         if (string.lower( parent.panelDM.dim[dim_index].dim_name) in gui_control.longitude_alias or axis.isLongitude()):
            f = parent.panelDM.dim[dim_index].first_scl.get()
            l = parent.panelDM.dim[dim_index].last_scl.get()
            vf =  string.atof( parent.panelDM.dim[dim_index].darray[f] )
            vl = string.atof( parent.panelDM.dim[dim_index].darray[l] )
            f=0
            if vf <= parent.panelDM.dim[dim_index].rdarray[f]:
               f = 0
            elif vf >= parent.panelDM.dim[dim_index].rdarray[-1]:
               f = len( parent.panelDM.dim[dim_index].rdarray ) - 1
            else:
               while vf !=  parent.panelDM.dim[dim_index].rdarray[f]: f += 1
            l=0
            if vl <= parent.panelDM.dim[dim_index].rdarray[l]:
               l = 0
            elif vl >= parent.panelDM.dim[dim_index].rdarray[-1]:
               l = len( parent.panelDM.dim[dim_index].rdarray ) - 1
            else:
               while vl !=  parent.panelDM.dim[dim_index].rdarray[l]: l += 1

            dlen = len(parent.panelDM.dim[dim_index].rdarray)
            lf = str( 0 )
            ll = str( dlen - 1 )
            idarray = []
            for p in range( dlen ): idarray.append( p )
            parent.panelDM.dim[dim_index].first_scl.configure(from_ = 0, to = (dlen - 1) )
            parent.panelDM.dim[dim_index].last_scl.configure( from_ = 0, to = (dlen - 1) )
            parent.panelDM.dim[dim_index].srl.setlist( idarray )
         else:
            f = parent.panelDM.dim[dim_index].first_scl.get()
            l = parent.panelDM.dim[dim_index].last_scl.get()
            lf = str( f )
            ll = str( l )
   #         lf = str( 0 )
   #         ll = str( len(parent.panelDM.dim[dim_index].darray) - 1 )
            parent.panelDM.dim[dim_index].first_scl.set( f )
            parent.panelDM.dim[dim_index].last_scl.set( f )
            parent.panelDM.dim[dim_index].srl.setlist( parent.panelDM.dim[dim_index].idarray )
         #
         parent.panelDM.dim[dim_index].menu.configure( background = gui_color.eight )
         parent.panelDM.dim[dim_index].comb.component( 'listbox' ).configure( selectbackground = gui_color.eight )
         parent.panelDM.dim[dim_index].dim_lab.set( "Index" )

      parent.panelDM.dim[dim_index].first_scl.set( f )
      parent.panelDM.dim[dim_index].last_scl.set( l )
      parent.panelDM.dim[dim_index].text_flb.set(lf[0:11])
      parent.panelDM.dim[dim_index].text_llb.set(ll[0:11])

def evt_menu_raw( parent, dim_index ):
   if parent.panelDM.dim[dim_index].men1.raw_flg == 1:
      parent.panelDM.dim[dim_index].men1.raw_flg = 0
      hold_dim_index_flg = parent.panelDM.dim[dim_index].men1.index_flg
      if hold_dim_index_flg == 0:
         parent.panelDM.dim[dim_index].men1.index_flg = 1
      else:
         parent.panelDM.dim[dim_index].men1.index_flg = 0
      evt_menu_index( parent, dim_index, 0 )
      parent.panelDM.dim[dim_index].men1.index_flg = hold_dim_index_flg
   else:
      parent.panelDM.dim[dim_index].men1.raw_flg = 1
      f = str( parent.panelDM.dim[dim_index].rdarray[ parent.panelDM.dim[dim_index].first_scl.get() ] )
      l = str( parent.panelDM.dim[dim_index].rdarray[ parent.panelDM.dim[dim_index].last_scl.get() ] )
      lf = str( parent.panelDM.dim[dim_index].rdarray[ 0 ] )
      ll = str( parent.panelDM.dim[dim_index].rdarray[ len(parent.panelDM.dim[dim_index].rdarray) - 1 ] )
      parent.panelDM.dim[dim_index].srl.setlist( parent.panelDM.dim[dim_index].rdarray )
      estr = f + ' : ' + l
      parent.panelDM.dim[dim_index].comb.setentry(estr)
      parent.panelDM.dim[dim_index].text_flb.set(f[0:11])
      parent.panelDM.dim[dim_index].text_llb.set(l[0:11])
      parent.panelDM.dim[dim_index].menu.configure( background = gui_color.eleven )
      parent.panelDM.dim[dim_index].comb.component( 'listbox' ).configure( selectbackground = gui_color.eleven )
      parent.panelDM.dim[dim_index].dim_lab.set( "Raw" )

def get_axis_weight_values(parent, dim_index):
   # Get information from a file
   name = parent.panelDM.dim[dim_index].dim_name
   if (parent.panelDM.var3 is not None):
      file = parent.panelDM.fid2
      var  = parent.panelDM.var3
      wgt_name = var + '_' + name + '_weight'
      try:
         swgt = getAxisWeightByName( file[var], name )
         gui_control.record_command(parent,'\n## Getting weights for axis %s' % name,1)
         gui_control.record_command(parent,'%s = genutil.getAxisWeightByName(fid2["%s"],"%s")' % (wgt_name, var, name),1)
         gui_control.record_command(parent,'%s.id = "%s"' % (wgt_name, wgt_name),1)
      except: 
          gui_message.error( 'Invalid dimension. Could not get weights.')
          return
   else:
      var = parent.panelDV.lst1[ parent.panelDV.selected ]
      wgt_name =  var.id + '_' + name + '_weight'
      try:
         swgt = getAxisWeightByName( var, name )
         gui_control.record_command(parent,'\n## Getting weights for axis %s' % name,1)
         gui_control.record_command(parent,'%s = genutil.getAxisWeightByName(%s,"%s")' % (wgt_name, var.id, name),1)
         gui_control.record_command(parent,'%s.id = "%s"' % (wgt_name, wgt_name),1)
      except: 
          gui_message.error( 'Invalid dimension. Could not get weights.')
          return

   # Save the variable
   swgt.id = wgt_name
   __main__.__dict__[wgt_name] = swgt

   # Update the defined window
   gui_defined_variables.update_defined()

def get_axis_values(parent, dim_index):
   # Get information from a file
   name = parent.panelDM.dim[dim_index].dim_name
   if (parent.panelDM.var3 is not None):
      file = parent.panelDM.fid2
      var  = parent.panelDM.var3
      axis_name = var + '_' + name + '_axis'
      ax = cdms2.MV2.array(file[var].getAxisList(axes = name)[0][:])
      ax.setAxis(0,file[var].getAxisList( axes = name )[0])
      gui_control.record_command(parent,'\n## Getting axis %s' % name,1)
      gui_control.record_command(parent,'%s = MV.array(fid2["%s"].getAxisList(axes = "%s")[0][:])' % (axis_name, var, name),1)
      gui_control.record_command(parent,'%s.setAxis(0,fid2["%s"].getAxisList(axes = "%s")[0])' % (axis_name, var, name),1)
      gui_control.record_command(parent,'%s.id = "%s"\n' % (axis_name, axis_name),1)
   else:
      var = parent.panelDV.lst1[ parent.panelDV.selected ]
      axis_name =  var.id + '_' + name + '_axis'
      ax = cdms2.MV2.array(var.getAxisList(axes = name)[0][:])
      ax.setAxis(0,var.getAxisList( axes = name )[0])
      gui_control.record_command(parent,'\n## Getting axis %s' % name,1)
      gui_control.record_command(parent,'%s = MV.array(%s.getAxisList(axes = "%s")[0][:])' % (axis_name, var.id, name),1)
      gui_control.record_command(parent,'%s.setAxis(0,%s.getAxisList(axes = "%s")[0])' % (axis_name, var.id, name),1)
      gui_control.record_command(parent,'%s.id = "%s"\n' % (axis_name, axis_name),1)

   # Save the variable
   ax.id = axis_name
   __main__.__dict__[axis_name] = ax

   # Update the defined window
   gui_defined_variables.update_defined()

def return_axis_size(parent, dim_index):
   # Get information from a file
   name = parent.panelDM.dim[dim_index].dim_name
   if (parent.panelDM.var3 is not None):
      file = parent.panelDM.fid2
      var  = parent.panelDM.var3
      axis_name = var + '_' + name + '_axis'
      ax = cdms2.MV2.array(file[var].getAxisList(axes = name)[0][:])
      ax.setAxis(0,file[var].getAxisList( axes = name )[0])
   else:
      var = parent.panelDV.lst1[ parent.panelDV.selected ]
      axis_name =  var.id + '_' + name + '_axis'
      ax = cdms2.MV2.array(var.getAxisList(axes = name)[0][:])
      ax.setAxis(0,var.getAxisList( axes = name )[0])
   return len(ax)

class generate_labels_list:
   def __init__(self, parent, dim_index,axis):
      self.parent=parent
##       print axis
##       print parent.panelDM.dim[dim_index].text_flb.get()
      fst=int(parent.panelDM.dim[dim_index].first_scl.get())
      lst=int(parent.panelDM.dim[dim_index].last_scl.get())
         
##       print dir(parent)
##       print dir(parent.panelDM)
##       print dir(parent.panelDM.dim[dim_index])
##       print parent.panelDM.dim[dim_index].men1.index_flg
##       print 
      name=parent.panelDM.dim[dim_index].dim_name
      title = 'Generate labels for dimension %s' % ( name,)
      self.dialog=Pmw.Dialog(parent,title=title,command=self.generatelist)
      if parent.menu.popup_window_settings_flg == 1:
         self.dialog.transient( parent ) # draw widget on top of its parent
      f=Tkinter.Frame(self.dialog.interior(),
                      borderwidth=4,
##                       bg="Red"
                      )
      mydict={}

      if (axis.isLongitude()  \
          or isinstance(parent.panelDM.grid,(cdms2.hgrid.AbstractCurveGrid,cdms2.gengrid.AbstractGenericGrid)))\
          and parent.panelDM.dim[dim_index].men1.index_flg!=1:
         fst=float(parent.panelDM.dim[dim_index].darray[fst])
         lst=float(parent.panelDM.dim[dim_index].darray[lst])
      else:
         fst=axis[fst]
         lst=axis[lst]

      levs=vcs.mkscale(fst,lst)
      mydict=vcs.mklabels(levs)
      if axis.isLongitude() or string.find(parent.panelDM.dim[dim_index].dim_lab.get(),'Virtual-Longitude')>-1:
         for k in mydict.keys():
            if k<0:
               mydict[k]=mydict[k][1:]+'W'
            elif k!=0.:
               mydict[k]=mydict[k]+'E'
      elif axis.isLatitude() or string.find(parent.panelDM.dim[dim_index].dim_lab.get(),'Virtual-Latitude')>-1:
         for k in mydict.keys():
            if k<0:
               if k<-90:
                  del(mydict[k])
                  mydict[-90.]='-90'
                  k=-90.
               mydict[k]=mydict[k][1:]+'S'
            elif k==0:
               mydict[k]='Eq'
            else:
               if k>90:
                  del(mydict[k])
                  mydict[90.]='90'
                  k=90.
               mydict[k]=mydict[k]+'N'
      elif axis.isTime():
         for k in mydict.keys():
            mydict[k]=str(cdtime.reltime(k,axis.units).tocomp()).split()[0]

      if name in parent.vcs[0].listelements('list'):
         i=1
         while name+'_'+str(i) in parent.vcs[0].listelements('list'):
            i+=1
         name=name+'_'+str(i)
      self.list_name=Pmw.EntryField(f,
                                    value=name,
                                    labelpos='w',
                                    entry_background = 'white', entry_foreground = 'black',
                                    label_text='List name (to retrieve in tickmarks pull down)',
##                                     label_bg='red'
                                    )
      self.text=Pmw.ScrolledText(f,
                                 labelpos='nw',
                                 borderframe = 1,
                                 label_text='Dictionary one association per line value : text',
                                 text_background = 'white', text_foreground = 'black',
                                 text_height = 2,
##                                  label_bg='red',
##                                  text_bg='red'
                                )
      keys=mydict.keys()
      keys.sort()
      for k in keys:
         self.text.insert('end',str(k)+' : '+mydict[k]+'\n')
         
      self.text.pack(expand = 1, fill='both')
      self.list_name.pack(expand = 1, fill='both', side='left' )
      f.pack(expand = 1, fill='both')

      #
      # Must complete dialog before moving back to main GUI and
      # position the dialog popup
      #
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2 = string.split(geom[0], 'x')
      d1 = int( string.atoi( geom2[0] ) * 1.0 )
      d2 = int( string.atoi( geom2[1] ) * 0.65 )
      d3 = string.atoi( geom[1] )
      d4 = string.atoi( geom[2] )
      self.dialog.geometry("%dx%d+%d+%d" % (d1, d2, d3, d4))
   def generatelist(self,button):
      if button=='OK':
         txt=self.text.get()
         dict={}
         ln = txt.split('\n')
         for l in ln:
            sp=l.split(':')
            if len(sp)>1:
               dict[float(sp[0])]=':'.join(sp[1:]).strip()
         name=self.list_name.get()
         if name in self.parent.vcs[0].listelements('list'):
            i=1
            while name+'_'+str(i) in self.parent.vcs[0].listelements('list'):
               i+=1
            name=name+'_'+str(i)
            dialog=Pmw.Dialog(self.parent,title='Name change Warning')
            if parent.menu.popup_window_settings_flg == 1:
               dialog.transient( self.dialog ) # draw widget on top of its parent
            lbl=Tkinter.Label(dialog.interior(),text='Warning, list already existed name has been changed to: '+str(name))
            lbl.pack()
         
         vcs.dictionarytovcslist(dict,name)
         self.dialog.destroy()
      else:
         self.dialog.destroy()
      
class replace_axis_values:
   def __init__(self, parent, dim_index, replace_type = None):
      if replace_type != 'awt':
        title = 'Replace Dimension %s' % parent.panelDM.dim[dim_index].dim_name,
      else:
        title = "Replace '%s' Dimension Weights" % parent.panelDM.dim[dim_index].dim_name,
      self.dialog = Pmw.SelectionDialog(parent,
          title = title,
          buttons = ('OK', 'Cancel'),
          defaultbutton = 'OK',
          scrolledlist_labelpos = 'n',
          label_text = 'Defined Variables:',
          listbox_background = 'white',
          listbox_foreground = 'black',
          scrolledlist_items = pagegui.return_defined_data_list(),
          command = gui_control.Command(self.evt_replace_axis_values, parent, dim_index, replace_type )
      )
   
      if parent.menu.popup_window_settings_flg == 1:
         self.dialog.transient( parent ) # draw widget on top of its parent

      #
      # Must complete dialog before moving back to main GUI and
      # position the dialog popup
      #
      parent_geom = parent.geometry()
      geom = string.split(parent_geom, '+')
      geom2 = string.split(geom[0], 'x')
      d1 = int( string.atoi( geom2[0] ) * 0.8 )
      d2 = int( string.atoi( geom2[1] ) * 0.4 )
      d3 = string.atoi( geom[1] )
      d4 = string.atoi( geom[2] )
      self.dialog.activate(geometry="%dx%d+%d+%d" % (d1, d2, d3, d4))


   def evt_replace_axis_values( self, parent, dim_index, replace_type, result ):
      if result == 'OK':
#         print "I am replacing the dimension = ", dim_index, dir(parent.panelDM.dim[dim_index])
#         print "Origial dim size = ", len (parent.panelDM.dim[dim_index].darray), len (parent.panelDM.dim[dim_index].idarray), parent.panelDM.dim[dim_index].npts, parent.panelDM.dim[dim_index].pts
#         print 'panelDV = ', dir(parent.panelDV)
#         print 'lst1 = ', parent.panelDV.lst1, dir(parent.panelDV.lst1)

         # Get the replacement slab
         try:
            r_name = string.split( self.dialog.getcurselection()[0], ' ')[0]
         except Exception,err:
            if replace_type == 'awt':
              parent.panelDM.dim[dim_index].opt.configure( menubutton_text=gui_control.dimchlst[0][0:3] )
            self.dialog.destroy()
            return
         aa = __main__.__dict__

##          print 'r_name = ', r_name
         r_slab = aa[ r_name ]

         # Check the sizes. If size doesn't match, then return error
         o_size = return_axis_size(parent, dim_index)
         if ( (len(r_slab.shape) != 1) or (r_slab.shape[0] != o_size) ):
            gui_message.error( 'Invalid shape or size. Axis requires a 1D array that matches in size.')
            if replace_type == 'awt':
              parent.panelDM.dim[dim_index].opt.configure( menubutton_text=gui_control.dimchlst[0][0:3] )
            self.dialog.destroy()
            return

         if replace_type != 'awt':
            if ( parent.panelDV.selected_list != {} ):
               s_slab = parent.panelDV.lst1[ parent.panelDV.selected ]
               old_dim = s_slab.getAxis( dim_index )
               old_dim[:]=r_slab.astype(old_dim[:].dtype.char).filled()
               old_dim.setBounds(None)
               gui_control.record_command(parent,"\n## Replacing values for dimension %i of %s with values of %s" % (dim_index,s_slab.id, r_slab.id),1)
               gui_control.record_command(parent,"old_dim = %s.getAxis( %i )" % (s_slab.id, dim_index),1)
               gui_control.record_command(parent,"old_dim[:]=%s.astype(old_dim[:].dtype.char).filled()" % (r_slab.id),1)
               gui_control.record_command(parent,"old_dim.setBounds(None)\n",1)
##                old_dim = r_slab.astype(old_dim.dtype.char)
               _srl_var_dim1_to_ndim( parent, slab=parent.panelDV.lst1[ parent.panelDV.selected ],dim_index=dim_index )
            else:
               _srl_var_dim1_to_ndim( parent, slab=None, dim_index=dim_index, new_axis=r_slab )
         else:
            parent.panelDM.dim[dim_index].new_weight = r_slab

         #print "the selection = ",  r_name
         #print 'r_slab = ', r_slab
         #parent.panelDM.dim[dim_index].menu.configure( background = gui_color.ten )
      else:
         if replace_type == 'awt':
           parent.panelDM.dim[dim_index].opt.configure( menubutton_text=gui_control.dimchlst[0][0:3] )

      self.dialog.destroy()
	


#------------------------------------------------------------------------
# Return the list of available printers. Printer names should be located
# in the user's $HOME/PCMDI_GRAPHICS/HARD_COPY file
#------------------------------------------------------------------------
def get_available_printers( ):
   plist = []
   try:
      fn = '%s/PCMDI_GRAPHICS/HARD_COPY' % os.environ['HOME']
   except:
      gui_message.error( 'Cannot find the file [ HARD_COPY ] file.')
      return plist
   try:
      f=open( fn )
   except:
##       print ('Cannot open the file [ %s ].' % fn)
      return plist
   ln = f.readline()
   while ln:
      if ln[0] != '#':
         if (ln[0:9] == 'landscape' or ln[0:8] == 'portrait' or
            ln[0] == ' ' or ln[0] == '\n'):
            pass
         else:
            plist.append(ln[0:-1])
      ln = f.readline()
   f.close( )
   return plist

#-----------------------------------------------------------------------------------
# Transpose two dimensions
#-----------------------------------------------------------------------------------
def evt_transpose_dimensions( index, dim_name, t_index, t_dim_name, parent ):
   #print '********* index = ', index
   #print '********* dim_name = ', dim_name
   #print '&&&&&&&&& t_index = ', t_index
   #print '&&&&&&&&& t_dim_name = ', t_dim_name
   #print 'ndim = ', parent.panelDM.ndim


   # remove from view the two transposing dimensions
   d = {}
   for i in range( parent.panelDM.ndim ):
      d[parent.panelDM.dim[i].dim_name] = i
#   print '**********d = ', d

   d[dim_name] = t_index
   d[t_dim_name] = index
#   print '**********d 2 = ', d
  
   gl = []
   for i in range( parent.panelDM.ndim ):
      for j in range( parent.panelDM.ndim ):
         if (d[parent.panelDM.dim[j].dim_name] == i):
            gl.append(parent.panelDM.dim[j].dim_name)
            break
#   print '**********gl = ', gl
     
   tp = []
   for i in range( parent.panelDM.ndim ):
      for j in range( parent.panelDM.ndim ):
         if (parent.panelDM.dim[j].dim_name == gl[i]):
            tp.append( parent.panelDM.dim.pop(j) )
            break
#   print '***********tp 0', tp[0].dim_name
#   print '***********tp 1', tp[1].dim_name
#   print '***********tp 2', tp[2].dim_name
#   print '***********dim ', parent.panelDM.dim

   for i in range( parent.panelDM.ndim ):
      parent.panelDM.dim.insert( i, tp.pop( 0 ) )
#   print '***********dim 0', parent.panelDM.dim[0].dim_name
#   print '***********dim 1', parent.panelDM.dim[1].dim_name
#   print '***********dim 2', parent.panelDM.dim[2].dim_name
#   print '***********dim ', parent.panelDM.dim

   # hide the dimensions for just a moment
   _blank_dim1_to_ndim( parent )

   # display the dimensions in the new order
   for i in range( parent.panelDM.ndim ):
      parent.panelDM.dim[i].top_frame.pack( side='top' )
      if parent.menu.fortran_order_flg:
         dname = gui_control.dim_axis[i] + ': ' + gl[i]
      else:
         dname = gui_control.dim_axis[len( gl ) - i - 1] + ': ' + gl[i]
      parent.panelDM.dim[i].menu.configure( text = dname)

   # delete menu items below the separator line for each dimension
   for i in range( parent.panelDM.ndim ):
      parent.panelDM.dim[i].men1.delete( 0, 'end' ) 

   # re-order the dimension list in the menu
   for i in range( parent.panelDM.ndim ):
      parent.panelDM.dim[i].men1.sub_index.set( parent.panelDM.dim[i].men1.index_flg )
      parent.panelDM.dim[i].men1.insert_checkbutton(1, label = (gl[i]+'_Index'),
                            selectcolor = gui_color.eight,
                            variable = parent.panelDM.dim[i].men1.sub_index,
                            command = gui_control.Command(evt_menu_index, parent, i, 1 )
                             )
      if ((string.lower(gl[i]) in gui_control.time_alias) or (string.lower(gl[i][:4]) == 'time')):
          parent.panelDM.dim[i].men1.sub_raw.set( parent.panelDM.dim[i].men1.raw_flg )
          parent.panelDM.dim[i].men1.insert_checkbutton(2, label = (gl[i]+'_Raw'),
                            selectcolor = gui_color.eleven,
                            variable = parent.panelDM.dim[i].men1.sub_raw,
                            command = gui_control.Command(evt_menu_raw, parent, i )
                             )
      parent.panelDM.dim[i].dim_name = gl[i]
      #
      # Get Axis Values
      #
      parent.panelDM.dim[i].men1.insert_separator( 3 )
      parent.panelDM.dim[i].men1.insert_command( 4,
                         label = 'Get Axis Values',
                         command = gui_control.Command(get_axis_values, parent, i)
                        )
      #
      # Get Axis Weight Values
      #
      parent.panelDM.dim[i].men1.insert_command( 5,
                         label = 'Get Axis Weight Values',
                         command = gui_control.Command(get_axis_weight_values, parent, i)
                        )
      #
      # Replace Axis Values
      #
      parent.panelDM.dim[i].men1.insert_command( 6,
                         label = 'Replace Axis Values',
                         command = gui_control.Command(replace_axis_values, parent, i)
                        )
      #
      parent.panelDM.dim[i].men1.insert_separator( 7 )
      #
      parent.panelDM.dim[i].men1.reordermenu = Tkinter.Menu( parent.panelDM.dim[i].men1, tearoff=0 )
      k = 7
      l = 0
      for x in gl:
         if parent.menu.fortran_order_flg:
            dname = gui_control.dim_axis[l] + ': ' + gl[l]
         else:
            dname = gui_control.dim_axis[len( gl ) - l - 1] + ': ' + gl[l]
         parent.panelDM.dim[i].men1.reordermenu.insert_command( k, label = dname, 
                        command = gui_control.Command(evt_transpose_dimensions, i, gl[i], (k-7), gl[k-7], parent)
         )
         k += 1
         l += 1
      parent.panelDM.dim[i].men1.add_cascade( label = 'Re-Order Dimensions', menu=parent.panelDM.dim[i].men1.reordermenu)

      # Reset the locked information in the dimension
      set_locked_information( parent, i )
      
      # Save the re-order information
      found = []
      for i in range(parent.panelDM.ndim):
         if parent.panelDM.dim[i].canvas_lockicon_toggle == 1:
            parent.panelDM.dim[i].evt_lock( parent, None)
            found.insert(i, True)
         else:
            found.insert(i, False)
      for i in range(len(found)):
         if found[i]:
            parent.panelDM.dim[i].canvas_lockicon_toggle = 0
            parent.panelDM.dim[i].evt_lock( parent, None)

#-----------------------------------------------------------------------------------
# Select the first and last dimension values -- Will also highlight the dimension's
# new range
#-----------------------------------------------------------------------------------
def get_dimension_selections(Csrl,Cpts):
  s_index = e_index = Cpts[0]
  if (len(Cpts) > 1):
     e_index = Cpts[-1]
  e_index += 1

  current_selection = Csrl.curselection()
  cur_start = cur_end = string.atoi(current_selection[0])
  if (len(current_selection) > 1):
     cur_end = string.atoi(current_selection[-1])

  Spt = -1
  for i in range(s_index, e_index):
      if Csrl.select_includes(i) == 0:
         Spt = i
         break

  if Spt == -1:
     if cur_start == s_index:
        Spt = e_index
     else:
        Spt = cur_start

  if (len(current_selection) == 0):
     Csrl.select_set(Cpts[0], Cpts[0])
     return Cpts
  elif (len(current_selection) == 2):
     if (len(Cpts) == 1):
        Npts = []
        for i in range(len(current_selection)):
            Npts.append(string.atoi(current_selection[i]))
        Csrl.select_set(Npts[0], Npts[1])
        Cpts = []
        for i in range(Npts[0], (Npts[1]+1)):
            Cpts.append(i)
     else:
        Csrl.select_clear(0, (Csrl.size()-1))
        Csrl.select_set(Spt)
        Cpts = []
        Cpts.append(Spt)
     return Cpts
  else:
     if (cur_start < s_index):
        Spt = cur_start
     elif (cur_end > e_index):
        Spt = cur_end
     Csrl.select_clear(0, (Csrl.size()-1))
     Csrl.select_set(Spt)
     Cpts = []
     Cpts.append(Spt)
     return Cpts

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
