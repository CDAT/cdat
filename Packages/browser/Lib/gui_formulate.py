#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The PCMDI Data Browser Formulate Data -  gui_formulate module
#
###############################################################################
#                                                                             #
# Module:       gui_formulate module                                          #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI Software System browser formulate data. This function   #
#               subslices the data and [ if need ] will take a sum, average   #
#               or area weighted average of a dimension. Singleton dimensions #
#               are squeezed out (or elminated) as a result.                  #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import os, sys, string, types, cdms2, cdutil, vcs
from genutil.statistics import geometricmean
from genutil.statistics import std
import gui_menu
import gui_functions
from gui_support import gui_color
import gui_control
import gui_message
import MV2

#---------------------------------------------------------------------------
# Modify the data to remove only the first single dimension
#---------------------------------------------------------------------------
def zapFirstDimension(a):
    l=[]
    for i in range(1,len(a.shape)): l.append(a.shape[i])
    a.shape=l
    return

#---------------------------------------------------------------------------
# Modify the data by scaling the incoming data by the incoming multiplier.
#---------------------------------------------------------------------------
def re_scale_data( a, multiplier ):
   b = cdms2.createVariable(a, copy=1)  # Copies tv
   b[:] = (a*multiplier).astype(b.dtype.char) # Multiplies
   return b

#---------------------------------------------------------------------------
# Modify the data just before plotting or storing in the "Selected" list
#---------------------------------------------------------------------------
def data(parent, d_name = None, var = None, new_var = None):
   from_file = 1
   record_okay = 1
   if (d_name is not None):    # Get information from a file
       file = parent.panelDM.fid2
       if (file is None):
          gui_message.error('Must enter or select a file from the "File" entry window.')
          return None
       var  = parent.panelDM.var3
       if (var == None):
          gui_message.error('Must enter or select a variable from the "Variable" entry window.')
          return None
       try:
          dim_names = list( file.listdimension( var ) )
       except: 			# Must be an axis
         dim_names = []
         dim_names.append( var )
   else:			# Get information from a transient variable object
       from_file = 0
       try:
          dim_names = var.listdimnames()
       except:
          gui_message.error('No dimension names in defined variable [ %s ].' % var)
          return None
   # 
   parent.panelDM.ndim = len(dim_names) # get the correct number of dimensions

   # Get the rank order and initialize settings
   f = []
   l = []
   t = []
   dl = []
   this_dim = []
   take_dim = []
   reverse_dim = []
   rank = []
   vl = dim_names
   ## ATTENTION MIGHT NOT WORK WITH DATA WITH MESH+TIME OR OTHER DIM
   ## MIGHT NEED multidim stuff
   for i in range(parent.panelDM.ndim):
##        print parent.panelDM.dim[i].dim_name,parent.panelDM.dim[i].dim_name
       if not (isinstance(parent.panelDM.grid,cdms2.gengrid.AbstractGenericGrid) and parent.panelDM.dim[i].dim_name in ['Row1']):
##            print 'in the if ?',parent.panelDM.grid
           if not parent.panelDM.dim[i].dim_name == 'Row0':
##                print 'appending:',parent.panelDM.dim[i].dim_name
               rank.append( vl.index( parent.panelDM.dim[i].dim_name ) )
           else:
               rank.append(i)
           f.append( -1 )
           l.append( -1 )
           t.append( -1 )
           dl.append( -1 )
           this_dim.append( -1 )
           take_dim.append( -1 )
           reverse_dim.append( -1 )

   # Get the necessary information from the GUI
   chk_type_ct = 0
   for i_index in range(parent.panelDM.ndim):
       if not (isinstance(parent.panelDM.grid,cdms2.gengrid.AbstractGenericGrid) and parent.panelDM.dim[i_index].dim_name in ['Row1']):
           i = rank.index( i_index )
           dim_array = parent.panelDM.dim[i].pts
##            print dim_array,parent.panelDM.dim[i].men1.index_flg
           indices = parent.panelDM.dim[i].npts
           f[i_index] = parent.panelDM.dim[i].npts[ 0 ]
           l[i_index] = parent.panelDM.dim[i].npts[ -1 ]
##            print f[i_index], l[i_index],parent.panelDM.ndim
           t[i_index] = parent.panelDM.dim[i].opt.getcurselection()
           dl[i_index] = parent.panelDM.dim[i].dim_name

           if t[i_index] != 'def':
               chk_type_ct = chk_type_ct + 1
               this_dim[ i ] =  i_index

   varobj = None
   if (d_name is not None):
      try:
         varobj = file.variables[ var ]
      except:		# Must be an axis
         a = file[var]
         # Makes better sense to show the index values for axis variables
         #varobj = cdms2.MV2.masked_array(a[:], attributes=a.attributes, axes=(a,), id = a.id)
         varobj = cdms2.MV2.masked_array(a[:], attributes=a.attributes, id = a.id)

      # Check to make sure the data is smaller than the swap size
      if (varobj.dtype.char == 'f') or (varobj.dtype.char == 'i'): data_size = 4
      else: data_size = 8
      for i in range(len(l)):
         data_size *= ((l[i]-f[i])+1)
      if (data_size > parent.swap_size):
         gui_message.error("Selected data is greater than %.17g Mbytes.\nPlease select a smaller subset\nor reset the GUI swap space under\nthe 'Options' main menu item.\nYou need at least %.17gMbytes" % (parent.swap_size/1000000.,(data_size/1000000+1)))
         return None

   # Reverse dimensions if necessary
   for p_index in range(parent.panelDM.ndim):
      p = rank.index( p_index )
      sign1 = 1
      if ( (parent.panelDM.dim[p].npts[0] - parent.panelDM.dim[p].npts[-1]) < 0 ): sign1 = -1
      sign2 = 1
      if ( (parent.panelDM.dim[p].idarray[0] - parent.panelDM.dim[p].idarray[-1]) < 0 ): sign2 = -1
      if (len(parent.panelDM.dim[p].npts) == 1): sign1 = sign2
      reverse = 0
      if (sign1 != sign2): reverse = 1
      if reverse:
        parent.panelDM.dim[p].dim_name
        reverse_dim[ p_index ] = -1*parent.panelDM.dim[p].stride
      else:
        reverse_dim[ p_index ] = parent.panelDM.dim[p].stride


   # Get the appropriate slab and squeeze out single dimensions
   dargs= []
   dargs2= []
   kpargs = {}
   for p_index in range(parent.panelDM.ndim):
      p = rank.index( p_index )
##       print 'pinedex,p,grid,dimname',p_index,p,parent.panelDM.grid,parent.panelDM.dim[p].dim_name
      if varobj is not None:
          axis = varobj.getAxis( p_index )
      else:
          axis = var.getAxis( p_index )
      ## Test for irregular grid and no mesh
      try:
          nomesh=0
          m=parent.panelDM.grid.getMesh()
      except:
          nomesh=1
      dolast = True
      if (parent.panelDM.dim[p].dim_name in gui_control.longitude_alias or axis.isLongitude()):
          dolast = False
          if (parent.panelDM.dim[p].men1.index_flg == 0):
             first = float( parent.panelDM.dim[p].darray[f[p_index]] )
             last  = float( parent.panelDM.dim[p].darray[l[p_index]] )
          else:
             first = float( parent.panelDM.dim[p].rdarray[f[p_index]] )
             last  = float( parent.panelDM.dim[p].rdarray[l[p_index]] )
          if varobj is not None:
             lon=varobj.getAxis( p_index ) 	# data from a file
          else:
             lon=var.getAxis( p_index )		# data from defined (i.e., memory)

          lon_i,lon_j,lon_k = lon.mapIntervalExt((first, last, 'cc'))

          # This is a temporary bug fix for CDMS. This is not reflected in the
          # the recording.
          try:
             con = parent.panelDM.fid2[varobj.id]
             lon = con.getLongitude()
             lon.attributes['units'] = 'degrees_east'
          except: pass

          # record wrapper and stride command for longitude
          # NOTE - Dean you need to record this piece of code later when you have time
#          gui_control.record_command( parent, "\n# Get the wrapped and strided longitude indices ", 1 )
#          gui_control.record_command( parent, "lon=varobj.getAxis( p_index )", 1)
#          gui_control.record_command( parent, "lon_i,lon_j,lon_k = lon.mapIntervalExt((%.17g, %.17g, 'cc'))" % (first, last), 1)

          dargs.append( slice(lon_i, lon_j, reverse_dim[ p_index ]) )
          dargs2.append( "longitude = (%g, %g)" % (first,last) )
      elif (isinstance(parent.panelDM.grid,cdms2.gengrid.AbstractGenericGrid) and parent.panelDM.dim[p].dim_name in ['Row0','Row1']) and nomesh==0:
          dolast = False
          # dictionary is: kpargs
          if (parent.panelDM.dim[p].men1.index_flg == 1):
             first = float( parent.panelDM.dim[p].darray[f[p_index]] )
             last  = float( parent.panelDM.dim[p].darray[l[p_index]] )
          else:
              ## need to figure out the long/lat selected
              R0=parent.panelDM.dim[p]
              R1=parent.panelDM.dim[p+1]
              lat_1 = R0.first_scl.get()
              lat_2 = R0.last_scl.get()
              lon_1 = R1.first_scl.get()
              lon_2 = R1.last_scl.get()
              lat_1=float(R0.darray[int(lat_1)])
              lat_2=float(R0.darray[int(lat_2)])
              lon_1=float(R1.darray[int(lon_1)])
              lon_2=float(R1.darray[int(lon_2)])
              kpargs['latitude']=(lat_1,lat_2)
              kpargs['longitude']=(lon_1,lon_2)
              dargs2.append("latitude = (%g, %g), longitude = (%g, %g)" % (lat_1,lat_2,lon_1,lon_2))
      elif (isinstance(parent.panelDM.grid,cdms2.hgrid.AbstractCurveGrid)) and nomesh==0:
          try:
              axis = parent.panelDM.fid2.getAxis( parent.panelDM.dim[p].dim_name )
          except Exception,err:
              axis = parent.panelDV.lst1[ parent.panelDV.selected ].getAxis( p )
          axes=parent.panelDM.grid.getAxisList()
          ## i have to use the id 'cause the axis itself seems to be copied somewhere beteween the getgrid used for panel dim and now....
          if axis.id in [axes[0].id,axes[1].id]:
              if (parent.panelDM.dim[p].men1.index_flg == 1):
                 first = float( parent.panelDM.dim[p].darray[f[p_index]] )
                 last  = float( parent.panelDM.dim[p].darray[l[p_index]] )
              else:
                  R0=parent.panelDM.dim[p]
                  v1 = float(R0.darray[int(R0.first_scl.get())])
                  v2 = float(R0.darray[int(R0.last_scl.get())])
                  if axis.id == axes[0].id:
                      kpargs['latitude']=(v1,v2)
                  else:
                      kpargs['longitude']=(v1,v2)
              dolast = False
      if dolast:
         if reverse_dim[ p_index ] != -1:
             dargs.append( slice(f[p_index],l[p_index]+1, reverse_dim[ p_index ]) )
         else:
            lh = l[p_index]
            if lh == 0:
               lh = None
            else:
               lh -= 1
            dargs.append( slice(f[p_index], lh, reverse_dim[ p_index ]) )

         if parent.panelDM.dim[p].dim_name in gui_control.latitude_alias:
            if (parent.panelDM.dim[p].men1.index_flg == 0):
               first = float( parent.panelDM.dim[p].darray[f[p_index]] )
               last  = float( parent.panelDM.dim[p].darray[l[p_index]] )
            else:
               first = float( parent.panelDM.dim[p].rdarray[f[p_index]] )
               last  = float( parent.panelDM.dim[p].rdarray[l[p_index]] )
            dargs2.append( "latitude = (%g, %g)" % (first,last) )
         else:
             try:
                beg=dargs[-1].start
                end=dargs[-1].stop-1
                if (parent.panelDM.dim[p].dim_name in gui_control.time_alias or axis.isTime()):
                    try:
                        tc=axis.asComponentTime()
                        dargs2.append( "%s = ('%s', '%s')" % (parent.panelDM.dim[p].dim_name, tc[beg], tc[end] ) )
                    except:
                        dargs2.append( '%s = (%g, %g)' % (parent.panelDM.dim[p].dim_name, axis[beg], axis[end] ) )
                else:
                    dargs2.append( '%s = (%g, %g)' % (parent.panelDM.dim[p].dim_name, axis[beg], axis[end] ) )
             except:
                dargs2.append( '%s' % (dargs[-1]) )

   kpargs['squeeze'] = 0
   if (d_name is not None):
##       print 'getting it',varobj,tuple(dargs), kpargs
##       slab = apply(varobj.subRegion, tuple(dargs), kpargs)
       ## Charles changed it from apply varobj.subRegion to varobj, as curvilinear and
       ## generic grid don't work with subRegion
      if ((data_size > 64000000) and (parent.panelSV.tin3.get()[0:7] == "http://")):
         gui_message.error('Could not retrieve the requested data. Try selecting a smaller sub-region.')
         return None
      else:
          slab = apply(varobj, tuple(dargs), kpargs)
      #########################################################################
      #                                                                       #
      # This code below is for replacing the axis with new axis values.       #
      # This section of code is only for variable objects from a file. That is#
      # after the slab has been obtained from the file, then check each axis to
      # determine if the axis values need changing.                           #
      # This works for strided and randomly selected axis as well.            #
      #                                                                       #
      #########################################################################
#
#     No longer needed since CDMS is now doing all the work. Counterpart to this code is found
#     in gui_functions.py. Look for "Counterpart".
#
#      for p_index in range(parent.panelDM.ndim):
#         p = rank.index( p_index )
#         if (parent.panelDM.dim[p].new_axis != None):
###             print 'ok we have a pbm Houston'
#            axis = slab.getAxis(p_index)
#            s = int(reverse_dim[ p_index ])
#            if s > 0:
#               fi =  int(f[p_index])
#               li =  int(l[p_index]) + 1
#            else:
#               fi =  int(f[p_index])
#               li =  int(l[p_index]) - 1
#            axis_len = len(axis[:])
#            kp = 0
#            for k in range(fi,li, s):
#             axis[kp] = parent.panelDM.dim[p].new_axis[k].astype(axis.dtype.char)
#             kp += 1
#             if kp == axis_len: break
   else:
       ## Charles changed it from apply varobj.subRegion to varobj, as curvilinear and
       ## generic grid don't work with subRegion
##       slab = apply(var.subRegion, tuple(dargs), kpargs)
##       print 'getting it',var,tuple(dargs), kpargs
      if var.shape!=():
        slab = apply(var, tuple(dargs), kpargs)
      else:
          slab=var

   #Make sure it's an MV, for 0D var
   if not MV2.isMaskedVariable(slab):
       slab=MV2.array(slab)
   # Make sure to change the Id
   if d_name != None: slab.id = slab.name = d_name

   #
   # record subRegion command
   r=''
   if from_file == 1:
      var_name = var
      if d_name is not None: var_name = d_name
      s_index = "%s=fid2( '%s', " % ( var_name, var )
      s_coord = "%s=fid2( '%s', " % ( var_name, var )
   else:
      var_name = var.id
      if new_var is not None: var_name = new_var
      s_index = "%s=%s( " % ( var_name, var.id )
      s_coord = "%s=%s( " % ( var_name, var.id )

   for x in dargs: s_index = s_index + str(x) + ', '
   for x in dargs2: s_coord = s_coord + x + ', '
   for x in rank: r+=str(x)
   s_index = s_index + "squeeze = 0, order = '%s' )" % r
   s_coord = s_coord + "squeeze = 0, order = '%s' )" % r
   gui_control.record_command( parent, "\n# Get new slab ", 1 )
   gui_control.record_command( parent, s_coord, 1 )
##    gui_control.record_command( parent, "# The equivalent slicing of the slab can be done with longitude\n# and latitude coordinate values, but the 'by' or stride functionality\n# is omitted when using coordinate values. For example: \n#", 1 )
##    gui_control.record_command( parent, '# ' + s_coord, 1 )

   slab=MV2.transpose( slab, rank )

   # Return the appropriate slab
   if chk_type_ct == 0: # return the slab
      if parent.menu.squeeze_dim_flg == 1:
##          gui_control.record_command( parent, "\n# Squeeze the slab ", 1 )
##          gui_control.record_command( parent, "%s = %s( squeeze=1 )" % (var_name, var_name), 1 )
         try:
             slab = slab( squeeze=1 )
         except: # ok 0d slabs can not be squeezed anymore...
             pass
         if vcs.VCS_validation_functions.isNumber(slab):
            gui_control.record_command( parent, "%s = cdms2.asVariable( %s )" % (var_name, var_name), 1 )
            slab = cdms2.asVariable( slab )
            slab.id=slab.name=var_name
      return slab
   else:  # return the either average or sum slab
      if from_file == 1:
         var_name = var
      else:
         var_name = var.id
         if new_var is not None: slab.id = new_var
      i_index = 0
      for i in range(parent.panelDM.ndim):
       if this_dim[i] != -1:
         if (t[ this_dim[i] ] == 'sum'):   # return the summed dimension
            tempslab = cdutil.averager(slab, axis="(" + slab.getAxis(i_index).id  + ")", weight='equal', action='sum')
            # record sum command
            s = "\n%s=cdutil.averager( %s, axis='(%s)', weight='equal', action='sum' )" % (slab.id, var_name, slab.getAxis(i_index).id)
            gui_control.record_command( parent, s, 1 )
         elif (t[ this_dim[i] ] == 'avg'): # return the averaged diminsion
            tempslab = cdutil.averager(slab, axis="(" + slab.getAxis(i_index).id  + ")", weight='equal')
            # record sum command
            s = "\n%s=cdutil.averager( %s, axis='(%s)', weight='equal' )" % (slab.id, var_name, slab.getAxis(i_index).id)
            gui_control.record_command( parent, s, 1 )
         elif (t[ this_dim[i] ] == 'wgt'): # return the weighted averaged dimension
            tempslab = cdutil.averager(slab, axis= "(" + slab.getAxis(i_index).id  + ")" )
            # record averager command
            s = "\n%s=cdutil.averager( %s, axis='(%s)' )" % (slab.id, var_name, slab.getAxis(i_index).id)
            gui_control.record_command( parent, s, 1 )
         elif (t[ this_dim[i] ] == 'awt'): # return the altered weighted averaged dimension
            weight_slab = parent.panelDM.dim[i_index].new_weight
            tempslab = cdutil.averager(slab, axis= "(" + slab.getAxis(i_index).id  + ")",
                         weight=weight_slab.filled() )
            # record sum command
            s = "\n# Currently, VCDAT cannot record the altered average weight command."
            gui_control.record_command( parent, s, 1 )
            record_okay = 0
         elif (t[ this_dim[i] ] == 'gtm'): # return the geometrical mean
            tempslab = geometricmean(slab, axis = i_index )
            # record geometric mean command
            s = "\n%s=genutil.statistics.geometricmean( %s, axis=%d )" % (slab.id, var_name, i_index)
            gui_control.record_command( parent, s, 1 )
         elif (t[ this_dim[i] ] == 'std'): # return the standard deviation
            tempslab = std(slab, axis= i_index)
            # record geometric mean command
            s = "\n%s=genutil.statistics.std( %s, axis=%d )" % (slab.id, var_name, i_index)
            gui_control.record_command( parent, s, 1 )
   
         # copy the original's slab's id back to the averaged slab
         if record_okay == 1:
            tempslab.id = slab.id
            gui_control.record_command( parent, ("%s.id = '%s'" % (slab.id, slab.id)), 1 )

         slab = tempslab
       else:
         i_index += 1

      if (parent.menu.squeeze_dim_flg == 1) and (len(tempslab.shape) > 0):
         if record_okay == 1:
            gui_control.record_command( parent, "\n# Squeeze the slab ", 1 )
            gui_control.record_command( parent, "%s = %s( squeeze=1 )" % (slab.id, slab.id), 1 )
         tempslab = tempslab(squeeze=1)
         if type( tempslab ) in [ types.IntType, types.FloatType ]:
            if record_okay == 1:
               gui_control.record_command( parent, "%s = cdms2.asVariable( %s ) % (var_name, var_name)", 1 )
            tempslab = cdms2.asVariable( tempslab )
      return tempslab

#---------------------------------------------------------------------
# End GUI formulate data.          
#---------------------------------------------------------------------

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
