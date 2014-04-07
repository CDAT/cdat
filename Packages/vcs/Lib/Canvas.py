#!/usr/bin/env python
# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The VCS Canvas API controls -  canvas module
#
###############################################################################
#                                                                             #
# Module:       canvas module                                                 #
#                                                                             #
# Copyright:    "See file Legal.htm for copyright information."               #
#                                                                             #
# Authors:      PCMDI Software Team                                           #
#               Lawrence Livermore National Laboratory:                       #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  PCMDI's VCS Canvas is used to display plots and to create and #
#               run animations.  It is always visible on the screen in a      #
#               landscape (width exceeding height), portrait (height exceeding#
#               width), or full-screen mode.                                  #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################

"""Canvas: the class representing a vcs drawing window
Normally, created by vcs.init()
Contains the method plot.
"""
import _vcs, string, types, signal, warnings
import tempfile
#import Tkinter
from pauser import pause
import thread
import numpy.ma, MV2
import numpy, cdutil
from queries import *
import boxfill, isofill, isoline, outfill, outline, taylor, meshfill, projection
import xyvsy, yxvsx, xvsy, vector, scatter, continents, line, marker, fillarea
import texttable, textorientation, textcombined, template, colormap
#import colormapgui as _colormapgui
#import canvasgui as _canvasgui
import displayplot
#import animationgui as _animationgui
#import graphicsmethodgui as _graphicsmethodgui
#import templateeditorgui as _templateeditorgui
#import gui_template_editor as _gui_template_editor
#import pagegui as _pagegui
#import projectiongui as _projectiongui
from error import vcsError
import cdms2
import copy
import cdtime,vcs
import os
import sys
import random
import genutil
from cdms2.grid import AbstractRectGrid
import shutil
from types import *
import VCS_validation_functions
import AutoAPI
from xmldocs import plot_keywords_doc,graphics_method_core,axesconvert,xaxisconvert,yaxisconvert, plot_1D_input, plot_2D_input, plot_output, plot_2_1D_input, create_GM_input, get_GM_input, boxfill_output, isofill_output, isoline_output, yxvsx_output, xyvsy_output, xvsy_output, scatter_output, outfill_output, outline_output, plot_2_1D_options
# Flag to set if the initial attributes file has aready been read in
called_initial_attributes_flg = 0
gui_canvas_closed = 0
canvas_closed = 0
#import Pmw
import vcsaddons

from PyQt4 import QtGui,QtCore
## class QAnimThread(QtCore.QThread):
##     def __init__(self,parent,func,*args):
##         QtCore.QThread.__init__(self,parent)
##         self.func=func
##         self.args=args
##     def run(self):
##         self.func(*self.args)
        
def showerror(msg):
    d=QtGui.QErrorMessage()
    d.showMessage(msg)
    d.exec_()

def dictionarytovcslist(dictionary,name):
    for k in dictionary.keys():
        if not isinstance(k,(float,int,long)):
            raise Exception,'Error, vcs list must have numbers only as keys'
    _vcs.dictionarytovcslist(dictionary,name)
    return None

def _determine_arg_list(g_name, actual_args):
    "Determine what is in the argument list for plotting graphics methods" 

##     print 'IN Determine arglist:',actual_args
    itemplate_name = 2
    igraphics_method = 3
    igraphics_option = 4


    # Note: Set default graphics method to 'default', which is invalid.
    # If it is not modified in this routine, it will be filled in later
    # in _reconstruct_tv after the grid type is established.
    #
    ## Xtrargs - {} - added by C.Doutriaux, needed for projection object passed
    ## Need to be passed as keyword later
    arglist = [None, None, 'default', 'default', 'default',{}]   
    arghold = []   
    argstring=[]
    args = actual_args
    found_slabs = 0
    for i in range(len(args)):
       if (type(args[i]) == types.StringType):
          argstring.append(args[i])
       else:
          try:
             possible_slab = cdms2.asVariable (args[i], 0)
             if not possible_slab.iscontiguous():
                 possible_slab = possible_slab.ascontiguousarray()
             arglist[found_slabs] = possible_slab
             if found_slabs == 2:
                 raise vcsError, "Too many slab arguments."
             found_slabs = found_slabs + 1
          except cdms2.CDMSError:
              arghold.append(args[i])

    #
    # Now find the template
    #
    args = arghold
    arghold = []
    found_template = 0
    for i in range(len(args)):
       if (istemplate(args[i])):
          if found_template:
             raise vcsError, 'You can only specify one template object.'
          arglist[itemplate_name] = args[i].name
          found_template = found_template + 1
       else:
          arghold.append(args[i])
    #
    # Now find the graphics method
    #
    args = arghold
    arghold = []
    found_graphics_method = 0
    for i in range(len(args)):
        if (isgraphicsmethod(args[i])):
            if found_graphics_method:
                raise vcsError,'You can only specify one graphics method.'
            arglist[igraphics_method] = graphicsmethodtype(args[i])
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (isline(args[i])):
            if found_graphics_method:
                raise vcsError,'You can only specify one graphics method.'
            arglist[igraphics_method] = 'line'
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (ismarker(args[i])):
            if found_graphics_method:
                raise vcsError,'You can only specify one graphics method.'
            arglist[igraphics_method] = 'marker'
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (isfillarea(args[i])):
            if found_graphics_method:
                raise vcsError,'You can only specify one graphics method.'
            arglist[igraphics_method] = 'fillarea'
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (istext(args[i])):
            if found_graphics_method:
                raise vcsError,'You can only specify one graphics method.'
            arglist[igraphics_method] = 'text'
            arglist[igraphics_option] = args[i].Tt_name + ':::' + args[i].To_name
            found_graphics_method = found_graphics_method + 1
        elif (isprojection(args[i])):
            arglist[5]['projection']=args[i].name
        elif isinstance(args[i],vcsaddons.core.VCSaddon):
            if found_graphics_method:
                raise vcsError,'You can only specify one graphics method.'
            arglist[igraphics_method] = graphicsmethodtype(args[i])
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1            
        else:
            raise vcsError, "Unknown type %s of argument to plotting command." %\
                                        type(args[i])
    if g_name is not None:
        arglist[igraphics_method] = g_name

# Now install the string arguments, left to right.
    if found_template == 0:
        if len(argstring) > 0:
           arglist[itemplate_name] = argstring[0]
           del argstring[0]
    if found_graphics_method == 0 and g_name is None:
        if len(argstring) > 0 :
           arglist[igraphics_method] = argstring[0]
           del argstring[0]

# Check for various errors
    if len(argstring) >= 1:
        arglist[igraphics_option] = argstring[0]
        del argstring[0]

    if len(argstring) > 0:
        if g_name is None:
            raise vcsError, "Error in argument list for vcs plot command."
        else:
            raise vcsError, "Error in argument list for vcs %s  command." % g_name
           
    if isinstance(arglist[igraphics_method],vcsaddons.core.VCSaddon):
        if found_slabs!=arglist[igraphics_method].g_nslabs:
            raise vcsError, "%s requires %i slab(s)" % (arglist[igraphics_method].g_name,arglist[igraphics_method].g_nslabs)
    else:
        if (string.lower(arglist[igraphics_method]) in ('scatter','vector','xvsy')):
            if found_slabs != 2:
                raise vcsError, "Graphics method requires 2 slabs."
        elif arglist[igraphics_method].lower() == 'meshfill':
            if found_slabs == 0:
                raise vcsError, "Graphics method requires at least 1 slab."
            elif found_slabs == 1:
                g=arglist[0].getGrid()
                if not isinstance(g, (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid,cdms2.grid.TransientRectGrid)):
                    raise vcsError, "Meshfill requires 2 slab if first slab doesn't have a Rectilinear, Curvilinear or Generic Grid type"                
        elif ((arglist[igraphics_method] == 'continents') or
              (arglist[igraphics_method] == 'line') or
              (arglist[igraphics_method] == 'marker') or
              (arglist[igraphics_method] == 'fillarea') or
              (arglist[igraphics_method] == 'text')):
            if found_slabs != 0:
                raise vcsError, "Continents or low-level primative methods requires 0 slabs."
        elif string.lower(arglist[igraphics_method])=='default':
            pass                            # Check later
        else:
            if found_slabs != 1:
                raise vcsError, "Graphics method requires 1 slab."
    if isinstance(arglist[3],str): arglist[3]=arglist[3].lower()
    return arglist

def _process_keyword(obj, target, source, keyargs, default=None):
    """ Set obj.target from:
    - keyargs[source]
    - default
    - obj.source
    in that order."""
    arg = keyargs.get(source)
    if arg is not None:
        setattr(obj, target, arg)
    elif default is not None:
        setattr(obj, target, default)
    elif hasattr(obj, source):
        setattr(obj, target, getattr(obj, source))
    return arg

def finish_queued_X_server_requests( self ):
    """ Wait for the X server to execute all pending events. 

        If working with C routines, then use BLOCK_X_SERVER
        found in the VCS module routine to stop the X server
        from continuing. Thus, eliminating the asynchronous
        errors.
    """
    x_num = self.canvas.xpending()
    count = 0
    while x_num != 0:
        x_num = self.canvas.xpending()
        count += 1
        # Move on already! The X sever must be completed by this point!
        # If count of 1000 is reached, then discard all events from
        # this point on in the queue.
        if count > 1000:
           self.canvas.xsync_discard()
           break 

class Canvas(object,AutoAPI.AutoAPI):
    """
 Function: Canvas                     # Construct a VCS Canvas class Object

 Description of Function:
    Construct the VCS Canas object. There can only be at most 8 VCS
    Canvases open at any given time.
    
 Example of Use:
    a=vcs.Canvas()                    # This examples constructs a VCS Canvas
"""
    #############################################################################
    #                                                                           #
    # Set attributes for VCS Canvas Class (i.e., set VCS Canvas Mode).          #
    #                                                                           #
    #############################################################################
    __slots__ = [
        '_mode',
        '_pause_time',
        '_viewport',
        '_worldcoordinate',
        '_winfo_id',
        '_varglist',
	'_canvas_gui',
        '_animate_info',
        '_canvas_template_editor',
        '_isplottinggridded',
        '_user_actions_names',
        '_user_actions',
        '_animate',
        '_canvas',
        'mode',
        'pause_time',
        'viewport',
        'worldcoordinate',
        'winfo_id',
        'varglist',
        'canvas_gui'
        'animate_info',
        'canvas_template_editor',
        'isplottinggridded',
        'ratio',
        'canvas',
        'animate',
        'user_actions_names',
        'user_actions',
        'size',
        'canvas_guianimate_info',
        ]
    def _set_user_actions_names(self,value):
        value=VCS_validation_functions.checkListElements(self,'user_actions_names',value,VCS_validation_functions.checkString)
        self._user_actions_names = value
        while len(self._user_actions)<len(self._user_actions_names):
            self._user_actions.append(self._user_actions[-1])
    def _get_user_actions_names(self):
        return self._user_actions_names
    user_actions_names = property(_get_user_actions_names,_set_user_actions_names)

    def _set_user_actions(self,value):
        value=VCS_validation_functions.checkListElements(self,'user_actions_names',value,VCS_validation_functions.checkCallable)
        self._user_actions = value
        while len(self._user_actions)<len(self._user_actions_names):
            self._user_actions.append(self._user_actions[-1])
    def _get_user_actions(self):
        return self._user_actions
    user_actions = property(_get_user_actions,_set_user_actions)

    def _setmode(self,value):
        value=VCS_validation_functions.checkInt(self,'mode',value,minvalue=0,maxvalue=1)
        self._mode=value
    def _getmode(self):
        return self._mode
    mode = property(_getmode,_setmode)

    def _setwinfo_id(self,value):
        value=VCS_validation_functions.checkInt(self,'winfo_id',value)
        self._winfo_id=value
    def _getwinfo_id(self):
        return self._winfo_id
    winfo_id = property(_getwinfo_id,_setwinfo_id)

    def _setvarglist(self,value):
        value=VCS_validation_functions.checkListElements(self,'varglist',value,VCS_validation_functions.checkCallable)
        self._varglist = value
    def _getvarglist(self):
        return self._varglist
    varglist = property(_getvarglist,_setvarglist)

    def _setcanvas_gui(self,value):
        self._canvas_gui = value
    def _getcanvas_gui(self):
        return self._canvas_gui
    canvas_gui = property(_getcanvas_gui,_setcanvas_gui)

    def _setcanvas(self,value):
        raise vcsError, "Error, canvas is not an attribute you can set"
    def _getcanvas(self):
        return self._canvas
    canvas = property(_getcanvas,_setcanvas)
    
    def _setanimate(self,value):
        raise vcsError, "Error, animate is not an attribute you can set"
    def _getanimate(self):
        return self._animate
    animate = property(_getanimate,_setanimate)

    def _setpausetime(self,value):
        value=VCS_validation_functions.checkInt(self,'pause_time',value)
        self._pause_time = value
    def _getpausetime(self):
        return self._pause_time
    pause_time = property(_getpausetime,_setpausetime)

    def _setviewport(self,value):
        if not isinstance(value,list) and not len(value)==4:
            raise vcsError,  "viewport must be of type list and have four values ranging between [0,1]."
        for v in range(4):
            if not 0.<=value[v]<=1.:
                raise vcsError,  "viewport must be of type list and have four values ranging between [0,1]."
        self._viewport=value
    def _getviewport(self):
        return self._viewport
    viewport = property(_getviewport,_setviewport)
    
    def _setworldcoordinate(self,value):
        if not isinstance(value,list) and not len(value)==4:
            raise vcsError,  "worldcoordinate must be of type list and have four values ranging between [0,1]."
        self._worldcoordinate=value
    def _getworldcoordinate(self):
        return self._worldcoordinate
    worldcoordinate = property(_getworldcoordinate,_setworldcoordinate)

    def _setcanvas_template_editor(self,value):
        self._canvas_template_editor=value # No check on this!
    def _getcanvas_template_editor(self):
        return self._canvas_template_editor
    canvas_template_editor =property(_getcanvas_template_editor,_setcanvas_template_editor)
    
    def _setisplottinggridded(self,value):
        if not isinstance(value,bool):
            raise vcsError,  "isplottinggridded must be boolean"
        self._isplottinggridded=value # No check on this!
    def _getisplottinggridded(self):
        return self._isplottinggridded
    isplottinggridded =property(_getisplottinggridded,_setisplottinggridded)
    
    def _setanimate_info(self,value):
        self._animate_info=value # No check on this!
    def _getanimate_info(self):
        return self._animate_info
    animate_info =property(_getanimate_info,_setanimate_info)
    
        
##     def __setattr__(self, name, value):
##         if (name == 'mode'):
##            try:
##               if (isinstance(value, types.IntType)) and (value in range(2)):
##                  self.__dict__['mode']=value
##               else:
##                  raise vcsError, "canvas setting mode failed, value = " + str(value)
##            except:
##               raise vcsError,  "canvas, " + name + ' must be 0 or 1.'
##               raise
##         elif (name == 'pause_time'):
##            if (not isinstance(value, types.IntType)):
##                raise vcsError, "Canvas' pause time must be integer."
##            self.__dict__['pause_time'] = value
##         elif (name == 'viewport'):
##            try:
##               if ((type(value) == types.ListType) and (len(value) == 4)):
##                  self.__dict__['viewport'] = value
##               else:
##                  raise vcsError,  "viewport must be of type list and have four values ranging between [0,1]."
##            except:
##               raise vcsError,  "viewport must be of type list and have four values ranging between [0,1]."
##               raise
##         elif (name == 'worldcoordinate'):
##            try:
##               if ((type(value) == types.ListType) and (len(value) == 4)):
##                  self.__dict__['worldcoordinate'] = value
##               else:
##                  raise vcsError,  "worldcoordinate must be of type list and have four ranging values."
##            except:
##               raise vcsError,  "worldcoordinate must be of type list and have four ranging values."
##               raise
##         elif (name == 'animate_info'):
##            self.__dict__['animate_info'] = value
##         elif (name == 'canvas_template_editor'):
##            self.__dict__['canvas_template_editor'] = value
##         elif (name == 'isplottinggridded'):
##            self.__dict__['isplottinggridded'] = value
##         elif (name == 'ratio'):
##            self.__dict__['ratio'] = value
##         else:
##            raise vcsError, 'Invalid member for setattr in VCS canvas.'
    def _datawc_tv(self, tv, arglist):
        """The graphics method's data world coordinates (i.e., datawc_x1, datawc_x2,
        datawc_y1, and datawc_y2) will override the incoming variable's coordinates.
        tv equals arglist[0] and assumed to be the first Variable. arglist[1] is 
        assumed to be the second variable."""

        # Determine the type of graphics method
        nvar = 1
        if arglist[3]   == 'boxfill':
           gm=self.getboxfill( arglist[4] )
        elif arglist[3] == 'isofill':
           gm=self.getisofill( arglist[4] )
        elif arglist[3] == 'isoline':
           gm=self.getisoline( arglist[4] )
        elif arglist[3] == 'outfill':
           gm=self.getoutfill( arglist[4] )
        elif arglist[3] == 'outline':
           gm=self.getoutline( arglist[4] )
        elif arglist[3] == 'continents':
           gm=self.getcontinents( arglist[4] )
        elif arglist[3] == 'scatter':
           nvar = 2
           gm=self.getscatter( arglist[4] )
        elif arglist[3] == 'vector':
           nvar = 2
           gm=self.getvector( arglist[4] )
        elif arglist[3] == 'xvsy':
           nvar = 2
           gm=self.getxvsy( arglist[4] )
        elif arglist[3] == 'xyvsy':
           gm=self.getxyvsy( arglist[4] )
        elif arglist[3] == 'yxvsx':
           gm=self.getyxvsx( arglist[4] )
        elif arglist[3] == 'taylor':
           gm=self.gettaylor( arglist[4] )
        elif arglist[3] == 'meshfill':
           gm=self.getmeshfill( arglist[4] )
        else:
	   return tv

        # Determine if the graphics method needs clipping
        f32 = numpy.array((1.e20),numpy.float32)
        set_new_x = 0
        set_new_y = 0
        if (gm.datawc_x1 != f32) and (gm.datawc_x2 != f32): set_new_x = 1
        if (gm.datawc_y1 != f32) and (gm.datawc_y2 != f32): set_new_y = 1

        try:
           if ((set_new_x == 1) and (set_new_y == 0)) or (arglist[3] == 'yxvsx'):
              tv = tv( longitude=(gm.datawc_x1, gm.datawc_x2) )
              if nvar == 2:
                 arglist[1] = arglist[1]( longitude=(gm.datawc_x1, gm.datawc_x2) )
           elif ((set_new_x == 0) and (set_new_y == 1)) or (arglist[3] == 'xyvsy'):
              tv = tv( latitude=(gm.datawc_y1, gm.datawc_y2) )
              if nvar == 2:
                 arglist[1] = arglist[1]( latitude=(gm.datawc_y1, gm.datawc_y2) )
           elif (set_new_x == 1) and (set_new_y == 1):
              tv = tv( latitude=(gm.datawc_y1, gm.datawc_y2), longitude=(gm.datawc_x1,gm.datawc_x2) )
              if nvar == 2:
                 arglist[1] = arglist[1]( latitude=(gm.datawc_y1, gm.datawc_y2), longitude=(gm.datawc_x1,gm.datawc_x2) )
        except:
           pass

        return tv

    def _reconstruct_tv(self, arglist, keyargs):
        """Reconstruct a transient variable from the keyword arguments.
        Also select the default graphics method, depending on the grid type
        of the reconstructed variable. For meshfill, ravel the last two
        dimensions if necessary.
        arglist[0] is assumed to be a Variable."""

        ARRAY_1 = 0
        ARRAY_2 = 1
        TEMPLATE = 2
        GRAPHICS_METHOD = 3
        GRAPHICS_OPTION = 4

        origv = arglist[ARRAY_1]

        # Create copies of domain and attributes
        variable = keyargs.get('variable')
        if variable is not None:
            origv=MV2.array(variable)
        tvdomain = origv.getDomain()
        attrs = copy.copy(origv.attributes)
        axislist = list(map(lambda x: x[0].clone(), tvdomain))

        # Map keywords to dimension indices
        rank = numpy.ma.rank(origv)
        dimmap = {}
        dimmap['x'] = xdim = rank-1
        dimmap['y'] = ydim = rank-2
        dimmap['z'] = zdim = rank-3
        dimmap['t'] = tdim = rank-4
        dimmap['w'] = wdim = rank-5

        # Process grid keyword
        grid = keyargs.get('grid')
        if grid is not None and xdim>=0 and ydim>=0:
            if grid.getOrder() is None or grid.getOrder()=='yx':
                axislist[xdim] = grid.getLongitude().clone()
                axislist[ydim] = grid.getLatitude().clone()
            else:
                axislist[xdim] = grid.getLatitude().clone()
                axislist[ydim] = grid.getLongitude().clone()

        # Process axis keywords
        for c in ['x','y','z','t','w']:
            if dimmap[c]<0:
                continue
            arg = keyargs.get(c+'axis')
            if arg is not None:
                axislist[dimmap[c]] = arg.clone()

        # Process array keywords
        for c in ['x','y','z','t','w']:
            if dimmap[c]<0:
                continue
            arg = keyargs.get(c+'array')
            if arg is not None:
                axis = axislist[dimmap[c]]
                axis = cdms2.createAxis(arg,id=axis.id)
                axis.setBounds(None)
                axislist[dimmap[c]]=axis
                
        # Process bounds keywords
        for c in ['x','y']:
            if dimmap[c]<0:
                continue
            arg = keyargs.get(c+'bounds')
            if arg is not None:
                axis = axislist[dimmap[c]]
                axis.setBounds(arg)

        # Process axis name keywords
        for c in ['x','y','z','t','w']:
            if dimmap[c]<0:
                continue
            arg = keyargs.get(c+'name')
            if arg is not None:
                axis = axislist[dimmap[c]]
                axis.id = axis.name = arg

        # Create the internal tv
        tv = cdms2.createVariable(origv, copy=0, axes=axislist, attributes=attrs)
        grid = tv.getGrid()

        isgridded = (grid is not None)

        # Set the default graphics method if not already set.
        if arglist[GRAPHICS_METHOD] in ['default','boxfill']:       # See _determine_arg_list
            try:
                nomesh=0
                m=grid.getMesh()
            except:
                nomesh=1
                
            if grid is None:
                if tv.rank()==1:
                    arglist[GRAPHICS_METHOD] = 'yxvsx1'
                else:
                    arglist[GRAPHICS_METHOD] = 'boxfill'
            elif isinstance(grid, AbstractRectGrid):
                arglist[GRAPHICS_METHOD] = 'boxfill'
            else:
                latbounds, lonbounds = grid.getBounds()
                if (latbounds is None) or (lonbounds is None):
                    if not isinstance(grid,cdms2.hgrid.AbstractCurveGrid):
                        # Plug in 'points' graphics method here, with:
                        #   arglist[GRAPHICS_METHOD] = 'points'
                        raise vcsError, "Cell boundary data is missing, cannot plot nonrectangular gridded data."
                    else:
                        arglist[GRAPHICS_METHOD] = 'boxfill'
                else:

                    # tv has a nonrectilinear grid with bounds defined,
                    # so use meshfill. Create another default meshobject to hang
                    # keywords on, since the true 'default' meshobject
                    # is immutable.
                    arglist[GRAPHICS_METHOD] = 'meshfill'

                    # Get the mesh from the grid.
                    try:
                        gridindices = tv.getGridIndices()
                    except:
                        gridindices = None
                    mesh = grid.getMesh(transpose=gridindices)

                    # mesh array needs to be mutable, so make it a tv.
                    # Normally this is done up front in _determine_arg_list.
                    arglist[ARRAY_2] = cdms2.asVariable(mesh, 0)
                    try:
                        meshobj = self.getmeshfill('__d_meshobj')
                    except:
                        meshobj = self.createmeshfill('__d_meshobj')
                        meshobj.wrap = [0.0, 360.0] # Wraparound
                    arglist[GRAPHICS_OPTION] = '__d_meshobj'

        # IF Meshfill method and no mesh passed then try to get the mesh from the object
        if arglist[GRAPHICS_METHOD]=='meshfill' and arglist[ARRAY_2] is None:
            # Get the mesh from the grid.
            try:
                gridindices = tv.getGridIndices()
                mesh = grid.getMesh(transpose=gridindices)
            except:
                gridindices = None
                mesh = grid.getMesh()

            # mesh array needs to be mutable, so make it a tv.
            # Normally this is done up front in _determine_arg_list.
            arglist[ARRAY_2] = cdms2.asVariable(mesh, 0)
            if arglist[GRAPHICS_OPTION] == 'default':
                try:
                    meshobj = self.getmeshfill('__d_meshobj')
                except:
                    meshobj = self.createmeshfill('__d_meshobj')
                    meshobj.wrap = [0.0, 360.0] # Wraparound
                arglist[GRAPHICS_OPTION] = '__d_meshobj'

        # Ravel the last two dimensions for meshfill if necessary
        ## value to know if we're plotting a grided meshfill
        self.isplottinggridded=False
        if (arglist[GRAPHICS_METHOD]=='meshfill') and (tv.shape[-1] != arglist[ARRAY_2].shape[-3]):
            tvshape = tv.shape
            if isgridded:
                ny, nx = grid.shape
                if nx*ny==arglist[ARRAY_2].shape[-3]:
                    ravelshape = tuple(list(tvshape)[:-2]+[ny*nx])
                    xdim=ydim
                    self.isplottinggridded=True
                else:
                    ny, nx = tvshape[-2:]
                    ravelshape = tuple(list(tvshape)[:-2]+[ny*nx])
            else:
                ny, nx = tvshape[-2:]
            ravelshape = tuple(list(tvshape)[:-2]+[ny*nx])
            tv = MV2.reshape(tv, ravelshape)
            xdim=ydim
            self.isplottinggridded=True
            if (tv.shape[-1] != arglist[ARRAY_2].shape[-3]):
                raise vcsError, "Mesh length = %d, does not match variable shape: %s"%(arglist[ARRAY_2].shape[-3], `tvshape`)
        else:
            if isgridded and (arglist[GRAPHICS_METHOD]=='meshfill'):
                if grid.shape[-1]==arglist[ARRAY_2].shape[-3]:
                    self.isplottinggridded=True 

        # Process variable attributes
        _process_keyword(tv, 'comment1', 'comment1', keyargs)
        _process_keyword(tv, 'comment2', 'comment2', keyargs)
        _process_keyword(tv, 'comment3', 'comment3', keyargs)
        _process_keyword(tv, 'comment4', 'comment4', keyargs)
        _process_keyword(tv, 'source', 'file_comment', keyargs)
        _process_keyword(tv, 'time', 'hms', keyargs)
        _process_keyword(tv, 'title', 'long_name', keyargs)
        _process_keyword(tv, 'name', 'name', keyargs, default=tv.id)
        time = keyargs.get('time')
        if time is not None:
            ctime = time.tocomp()
            ar.date = str(ctime)
        _process_keyword(tv, 'units', 'units', keyargs)
        _process_keyword(tv, 'date', 'ymd', keyargs)
        # If date has still not been set, try to get it from the first
        # time value if present
        if not hasattr(tv, 'date') and not hasattr(tv, 'time'):
            change_date_time(tv, 0)

        # Draw continental outlines if specified.
        contout = keyargs.get('continents',None)
        if contout is None:
            #            if xdim>=0 and ydim>=0 and isgridded:
            ## Charles put back the self.isplottinggridded in addition for meshfill, 
            if (xdim>=0 and ydim>=0 and tv.getAxis(xdim).isLongitude() and tv.getAxis(ydim).isLatitude()) or (self.isplottinggridded):
                contout = 1
            else:
                contout = 0
        if (isinstance(arglist[GRAPHICS_METHOD],str) and (arglist[GRAPHICS_METHOD]) == 'meshfill') or ((xdim>=0 and ydim>=0 and (contout>=1) and (contout<12))):
            self.canvas.setcontinentstype(contout)
            self.canvas.savecontinentstype(contout)
        else:
            self.canvas.setcontinentstype(0)
            self.canvas.savecontinentstype(0)

        # Reverse axis direction if necessary
        xrev = keyargs.get('xrev',0)
        if xrev==1 and xdim>=0:
            tv = tv[... , ::-1]
   
        # By default, latitudes on the y-axis are plotted S-N
        # levels on the y-axis are plotted with decreasing pressure
        if ydim>=0:
            yaxis = tv.getAxis(ydim)
            yrev = 0
## -- This code forces the latitude axis to alway be shown from -90 (South) to
##    90 (North). This causes a problem when wanting to view polar plots from
##    the North. So this feature has been removed.
##
##             if yaxis.isLatitude() and yaxis[0]>yaxis[-1]: yrev=1
##             if yaxis.isLevel() and yaxis[0]<yaxis[-1]: yrev=1

            yrev = keyargs.get('yrev',yrev)
            if yrev==1:
##                 yarray = copy.copy(yaxis[:])
##                 ybounds = yaxis.getBounds()
##                 yaxis[:] = yarray[::-1]
##                 yaxis.setBounds(ybounds[::-1,::-1])
                tv = tv[..., ::-1, :].clone()
                

#  -- This s no longer needed since we are making a copy of the data.
#     We now apply the axes changes below in __plot. Dean and Charles keep
#     an eye opened for the errors concerning datawc in the VCS module.
#        tv = self._datawc_tv( tv, arglist )
        return tv

    #############################################################################
    #                                                                           #
    # Print out the object's doc string.                                        #
    #                                                                           #
    #############################################################################
    def objecthelp(self, *arg):
        """
 Function: objecthelp               # Print out the object's doc string

 Description of Function:
    Print out information on the VCS object. See example below on its use.
    
 Example of Use:
    a=vcs.init()

    ln=a.getline('red')                 # Get a VCS line object
    a.objecthelp(ln)                    # This will print out information on how to use ln
    """
        for x in arg:
            print getattr(x, "__doc__", "")

    #############################################################################
    #                                                                           #
    # Initialize the VCS Canvas and set the Canvas mode to 0. Because the mode  #
    # is set to 0, the user will have to manually update the VCS Canvas by      #
    # using the "update" function.                                              #
    #                                                                           #
    #############################################################################
    def __init__(self, gui = 0, mode = 1, pause_time=0, call_from_gui=0, size=None):
        #############################################################################
        #                                                                           #
        # The two Tkinter calls were needed for earlier versions of CDAT using      #
        # tcl/tk 8.3 and Python 2.2. In these earlier version of CDAT, Tkinter must #
        # be called before "_vcs.init()", which uses threads. That is,              #
        # "_vcs.init()" calls "XInitThreads()" which causes Tkinter keyboard events #
        # to hang. By calling Tkinter.Tk() first solves the problem.                #
        #                                                                           #
        # The code must have "XInitThreads()". Without this function, Xlib produces #
        # asynchronous errors. This X thread function can be found in the           #
        # vcsmodule.c file located in the "initialize_X routine.                    #
        #                                                                           #
        # Graphics User Interface Mode:                                             #
        #        gui = 0|1    if ==1, create the canvas with GUI controls           #
        #                     (Default setting is *not* to display GUI controls)    #
        #                                                                           #
        # Note:                                                                     #
        #     For version 4.0, which uses tcl/tk 8.4 and Python 2.3, the below      #
        #     Tkinter.Tk() calls are not necessary.                                 #
        #                                                                           #
        #     The code will remain here, but commented out in case the bug in       #
        #     tcl/tk reappears.                                                     #
        #                                                                           #
#########if (call_from_gui == 0):                                                   #########
#########   try:                                                                    #########
#########      print ' NO local host'                                               #########
#########      rt = Tkinter.Tk() # Use the default  DISPLAY and screen              #########
#########       print ' I have a rt', rt                                            #########
#########    except:                                                                #########
#########      print ' :0.0 local host'                                             #########
#########      rt = Tkinter.Tk(":0.0") # Use the localhost:0.0 for the DISPLAY and screen ###
#########   rt.withdraw()                                                           #########
########### rt.destroy()                                                            #########
        #                                                                           #
        #############################################################################

        self.info = AutoAPI.Info(self)
        self.info.expose=["plot", "boxfill", "isofill", "isoline", "outfill", "outline", "scatter", "xvsy", "xyvsy", "yxvsx", "createboxfill", "getboxfill", "createisofill", "getisofill", "createisoline", "getisoline", "createyxvsx", "getyxvsx", "createxyvsy", "getxyvsy", "createxvsy", "getxvsy", "createscatter", "getscatter", "createoutfill", "getoutfill", "createoutline", "getoutline"]
        ospath = os.environ["PATH"]
        found = False
        for p in ospath.split(":"):
            if p==os.path.join(sys.prefix,"bin"):
                found = True
                break
        if found is False:
            os.environ["PATH"]=os.environ["PATH"]+":"+os.path.join(sys.prefix,"bin")
        global called_initial_attributes_flg
        global gui_canvas_closed
        global canvas_closed
##         import gui_support
        import time
##         from tkMessageBox import showerror

        is_canvas = len(_vcs.return_display_names()[0])

        if gui_canvas_closed == 1:
           showerror( "Error Message to User", "There can only be one VCS Canvas GUI opened at any given time and the VCS Canvas GUI cannot operate with other VCS Canvases.")
           return

        self.winfo_id = -99
        self.varglist = []
        self.canvas_gui= None
        self.isplottinggridded=False
        self.canvas_guianimate_info=None
# DEAN or CHARLES -- remove the one line below for VCS Canvas GUI to work
        #gui = 0
        if is_canvas == 0:
           if ( (gui == 1) and (gui_canvas_closed == 0) ):
               no_root = 0
               if (gui_support.root_exists()): no_root = 1
               if (no_root == 0):
                  parent = gui_support.root()
               else:
                  parent = gui_support._root
               self.canvas_gui = _canvasgui.CanvasGUI(canvas=self,top_parent=parent)
               # Must wait for the window ID to return before moving on...
               while self.winfo_id == -99: self.winfo_id = self.canvas_gui.frame.winfo_id()


        if size is None:
            psize = 1.2941176470588236
        elif isinstance(size,(int,float)):
            psize = size
        elif isinstance(size,str):
            if size.lower() in ['letter','usletter']:
                psize = size = 1.2941176470588236
            elif size.lower() in ['a4',]:
                psize = size = 1.4142857142857141
            else:
                raise Exception, 'Unknown size: %s' % size
        else:
            raise Exception, 'Unknown size: %s' % size

        self.size = size

        self.mode = mode
        self.pause_time = pause_time
        self._canvas =_vcs.init( self.winfo_id,psize ) # connect the canvas to the GUI
        self.viewport =[0,1,0,1]
        self.worldcoordinate = [0,1,0,1]
        self._animate = animate_obj( self )
        self._dotdir,self._dotdirenv = self._canvas.getdotdirectory()
        if ( (is_canvas == 0) and (gui == 1) and (gui_canvas_closed == 0) ): gui_canvas_closed = 1

## Initial.attributes is being called in main.c, so it is not needed here!
## Actually it is for taylordiagram graphic methods....
###########################################################################################
#  Okay, then this is redundant since it is done in main.c. When time perments, put the   #
#  taylordiagram graphic methods attributes in main.c Because this is here we must check  #
#  to make sure that the initial attributes file is called only once for normalization    #
#  purposes....                                                                           #
###########################################################################################
        if called_initial_attributes_flg == 0:
           pth = vcs.__path__[0].split(os.path.sep)
           pth=pth[:-4] # Maybe need to make sure on none framework config
           pth=['/']+pth+['bin', 'initial.attributes']
           try:
               self._scriptrun( os.path.join(*pth))
           except:
               pass
           self._scriptrun( os.path.join(os.environ['HOME'], self._dotdir, 'initial.attributes'))
	called_initial_attributes_flg = 1
        self.animate_info=[]
        self.canvas_template_editor=None
        self.ratio=0
        self._user_actions_names=['Clear Canvas','Close Canvas','Show arguments passsed to user action']
        self._user_actions = [self.clear, self.close, self.dummy_user_action]

    #############################################################################
    #                                                                           #
    # Update wrapper function for VCS.                                          #
    #                                                                           #
    #############################################################################
    def update(self, *args):
        """
 Function: update                   # Update the VCS Canvas.

 Description of Function:
    If a series of commands are given to VCS and the Canvas Mode is 
    set to manual, then use this function to update the plot(s) 
    manually.

 Example of Use:
    ...

    a=vcs.init()
    a.plot(s,'default','boxfill','quick')
    a.mode = 0                             # Go to manual mode
    box=x.getboxfill('quick')
    box.color_1=100
    box.xticlabels('lon30','lon30')
    box.xticlabels('','')
    box.datawc(1e20,1e20,1e20,1e20)
    box.datawc(-45.0, 45.0, -90.0, 90.0)

    a.update()                             # Update the changes manually
"""

        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        a = apply(self.canvas.updatecanvas, args)
        self.flush() # update the canvas by processing all the X events
        self.backing_store()
        pause (self.pause_time)

        self.canvas.UNBLOCK_X_SERVER()
        return a

    #############################################################################
    #                                                                           #
    # Update wrapper function for VCS with a check to update the continents.    #
    #                                                                           #
    #############################################################################
    def _update_continents_check(self, *args):
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        a = apply(self.canvas.updatecanvas_continents, args)
        self.flush() # update the canvas by processing all the X events
        self.backing_store()
        pause (self.pause_time)

        self.canvas.UNBLOCK_X_SERVER()
        return a

    #############################################################################
    #                                                                           #
    # Script VCS primary or secondary elements wrapper functions for VCS.       #
    #                                                                           #
    #############################################################################
    def scriptobject(self, obj, script_filename=None, mode=None):
        """
 Function: scriptobject       # Script a single primary or secondary class object

 Description of Function:
    Save individual attributes sets (i.e., individual primary class
    objects and/or secondary class objects). These attribute sets
    are saved in the user's current directory.

    Note: If the the filename has a ".py" at the end, it will produce a
          Python script. If the filename has a ".scr" at the end, it will
          produce a VCS script. If neither extensions are give, then by
          default a Python script will be produced.

    Note: Mode is either "w" for replace or "a" for append.

    Note: VCS does not allow the modification of `default' attribute sets,
          it will not allow them to be saved as individual script files.
          However, a `default' attribute set that has been copied under a
          different name can be saved as a script file.

 Example of Use:
    a=vcs.init()
    l=a.getline('red')         # To Modify an existing line object
    i=x.createisoline('dean')  # Create an instance of default isoline object
    ...
    x.scriptsingle(l,'line.scr','w') # Save line object as a VCS file 'line.scr'
    x.scriptsingle(i,'isoline.py')   # Save isoline object as a Python file 'isoline.py'
"""
        if istemplate(obj):
           template.P.script(obj, script_filename, mode)
        elif isgraphicsmethod(obj):
           if (obj.g_name == 'Gfb'):
              boxfill.Gfb.script(obj, script_filename, mode)
           elif (obj.g_name == 'Gfi'):
              isofill.Gfi.script(obj, script_filename, mode)
           elif (obj.g_name == 'Gi'):
              isoline.Gi.script(obj, script_filename, mode)
           elif (obj.g_name == 'Go'):
              outline.Go.script(obj, script_filename, mode)
           elif (obj.g_name == 'Gfo'):
              outfill.Gfo.script(obj, script_filename, mode)
           elif (obj.g_name == 'GXy'):
              xyvsy.GXy.script(obj, script_filename, mode)
           elif (obj.g_name == 'GYx'):
              yxvsx.GYx.script(obj, script_filename, mode)
           elif (obj.g_name == 'GXY'):
              xvsy.GXY.script(obj, script_filename, mode)
           elif (obj.g_name == 'Gv'):
              vector.Gv.script(obj, script_filename, mode)
           elif (obj.g_name == 'GSp'):
              scatter.GSp.script(obj, script_filename, mode)
           elif (obj.g_name == 'Gcon'):
              continents.Gcon.script(obj, script_filename, mode)
           elif (obj.g_name == 'Gtd'):
              obj.script( script_filename, mode)
           elif (obj.g_name == 'Gfm'):
              obj.script( script_filename, mode)
           else:
              print 'Could not find the correct graphics class object.'
        elif issecondaryobject(obj):
           if (obj.s_name == 'Tl'):
              line.Tl.script(obj, script_filename, mode)
           elif (obj.s_name == 'Tm'):
              marker.Tm.script(obj, script_filename, mode)
           elif (obj.s_name == 'Tf'):
              fillarea.Tf.script(obj, script_filename, mode)
           elif (obj.s_name == 'Tt'):
              texttable.Tt.script(obj, script_filename, mode)
           elif (obj.s_name == 'To'):
              textorientation.To.script(obj, script_filename, mode)
           elif (obj.s_name == 'Tc'):
              textcombined.Tc.script(obj, script_filename,mode)
           elif (obj.s_name == 'Proj'):
              obj.script( script_filename,mode)
           else:
              print 'Could not find the correct secondary class object.'
        else:
           print 'This is not a template, graphics method or secondary method object.'

    #############################################################################
    #                                                                           #
    # Remove VCS primary and secondary methods wrapper functions for VCS.       #
    #                                                                           #
    #############################################################################
    def removeobject(self, obj):
        """
 Function: remove

 Description of Function:
    The user has the ability to create primary and secondary class 
    objects. The function allows the user to remove these objects
    from the appropriate class list. 

    Note, To remove the object completely from Python, remember to
    use the "del" function.

    Also note, The user is not allowed to remove a "default" class
    object.

 Example of Use:
    a=vcs.init()
    line=a.getline('red')       # To Modify an existing line object
    iso=x.createisoline('dean') # Create an instance of an isoline object
    ...
    x.remove(line)      # Removes line object from VCS list
    del line            # Destroy instance "line", garbage collection
    x.remove(iso)       # Remove isoline object from VCS list
    del iso             # Destroy instance "iso", garbage collection
"""
        if istemplate(obj):
           msg =  _vcs.removeP(obj.name)
           obj.__dict__['name'] =  obj.__dict__['p_name'] = '__removed_from_VCS__'
        elif isgraphicsmethod(obj):
           if (obj.g_name == 'Gfb'):
              msg =  _vcs.removeGfb(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gfi'):
              msg =  _vcs.removeGfi(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gi'):
              msg =  _vcs.removeGi(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Go'):
              msg =  _vcs.removeGo(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gfo'):
              msg =  _vcs.removeGfo(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'GXy'):
              msg =  _vcs.removeGXy(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'GYx'):
              msg =  _vcs.removeGYx(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'GXY'):
              msg =  _vcs.removeGXY(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gv'):
              msg =  _vcs.removeGv(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'GSp'):
              msg =  _vcs.removeGSp(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gcon'):
              msg =  _vcs.removeGcon(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gfm'):
              msg =  _vcs.removeGfm(obj.name)
              obj.name =  obj.g_name = '__removed_from_VCS__'
           elif (obj.g_name == 'Gtd'):
               n=len(vcs.taylordiagrams)
               ndel=0
               for i in range(n):
                   t=vcs.taylordiagrams[i-ndel]
                   if t.name==obj.name:
                       msg =  'Removed Taylordiagram graphics method: '+t.name
                       a=vcs.taylordiagrams.pop(i-ndel)
                       ndel=ndel+1
                       del(a)
           else:
              msg = 'Could not find the correct graphics class object.'
        elif issecondaryobject(obj):
           if (obj.s_name == 'Tl'):
              msg =  _vcs.removeTl(obj.name)
              obj.name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'Tm'):
              msg =  _vcs.removeTm(obj.name)
              obj.name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'Tf'):
              msg =  _vcs.removeTf(obj.name)
              obj.name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'Tt'):
              msg =  _vcs.removeTt(obj.name)
              obj.name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'To'):
              msg =  _vcs.removeTo(obj.name)
              obj.name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'Tc'):
              msg =  _vcs.removeTt(obj.Tt_name)
              msg +=  _vcs.removeTo(obj.To_name)
              obj.Tt_name =  obj.s_name = '__removed_from_VCS__'
              obj.To_name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'Proj'):
              msg =  _vcs.removeProj(obj.name)
              obj.name =  obj.s_name = '__removed_from_VCS__'
           elif (obj.s_name == 'Cp'):
              msg =  _vcs.removeCp(obj.name)
              obj.s_name =  obj.__dict__['name'] = '__removed_from_VCS__'
           else:
              msg =  'Could not find the correct secondary class object.'
        else:
           msg = 'This is not a template, graphics method, or secondary method object.'
        return msg

## Removed by C. Doutriaux, too many prints. We need to think
## if we want to raise an exception here?
##         if msg[:7]!='Removed':
##             print msg
            
    def syncP(self, *args):
        return apply(self.canvas.syncP, args)

    def removeP(self, *args):
        return apply(self.canvas.removeP, args)


    def clean_auto_generated_objects(self,type=None):
        """ cleans all self/auto genrated objects in vcs, only if they're not in use
        Example:
        import vcs
        x=vcs.init()
        x.clean_auto_generated_objects() # cleans everything
        x.clean_auto_generated_objects('template') # cleans template objects
        """
        
        if type is None:
            type = self.listelements()
        elif isinstance(type,str):
            type=[type,]
        elif not isinstance(type,(list,tuple)):
            return
        for objtype in type:
            for obj in self.listelements(objtype):
                if obj[:2]=="__":
                    try:
                        exec("o = self.get%s(obj)" % objtype)
                        destroy = True
                        if objtype=='template':
##                             print o.name
                            dnames = self.return_display_names()
                            for d in dnames:
                                dpy = self.getplot(d)
                                if o.name in [dpy.template,dpy._template_origin]:
                                    destroy = False
                                    break
                        if destroy : self.removeobject(o)
##                         try:
##                             exec("o = self.get%s(obj)" % objtype)
##                             print 'stayed'
##                         except:
##                             print 'gone'
                    except Exception,err:
##                         print 'Error for:',o.name,err
##                         raise vcsError,err
                        pass
                        
        return
    
    def check_name_source(self,name,source,type):
        elts = self.listelements(type)
        if name is None:
            rnd = random.randint(0,100000)
            name = '__%s_%i' % (type[:4],rnd)
            while name in elts:
                rnd = random.randint(0,100000)
                name = '__%s_%i' % (type[:4],rnd)
        if not isinstance(name,str):
            raise vcsError, '%s object name must be a string or %s name' % (type,type)
        elif len(name)>16:
                raise vcsError,'%s object name must be at most 16 character long' % (type)

        if not isinstance(source,str):
            exec("ok = vcs.is%s(source)" % (type,))
        else:
            ok=0
        if (not isinstance(source,str)) and ok==0:
            raise vcsError,'Error %s object source must be a string or a %s object' % (type,type)
        elif ok:
            source=source.name

        if name in elts:
            raise vcsError, "Error %s object named %s already exists" % (type,name)
        if not source in elts:
            raise vcsError, "Error source %s object (%s) does not exist!" % (type,name)
        return name,source
    
    #############################################################################
    #                                                                           #
    # Template functions for VCS.                                               #
    #                                                                           #
    #############################################################################
    def createtemplate(self, name=None, source='default'):
        """
 Function: createtemplate                  # Construct a new template

 Description of Function:
    Create a new template given the the name and the existing template to copy
    the attributes from. If no existing template name is given, then the default
    template will be used as the template to which the attributes will be copied
    from.

    If the name provided already exists, then a error will be returned. Template
    names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('template')                       # Show all the existing templates
    con=a.createtemplate('example1') # create 'example1' template from 'default' template 
    a.show('template')                       # Show all the existing templates
    con=a.createtemplate('example2','quick') # create 'example2' from 'quick' template
    a.listelements('template')               # Show all the templates as a Python list
"""
        name,source = self.check_name_source(name,source,'template')

        return template.P(self, name, source, 0)

    def gettemplate(self, Pt_name_src='default'):
        """
 Function: gettemplate                       # Construct a new template

 Description of Function:
    VCS contains a list of predefined templates. This function will create a
    template class object from an existing VCS template. If no template name
    is given, then template 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createtemplate function.)

 Example of Use:
    a=vcs.init()
    a.show('template')                  # Show all the existing templates
    templt=a.gettemplate()              # templt instance of 'default' template
    templt2=a.gettemplate('quick')      # templt2 contains 'quick' template
"""
        # Check to make sure the argument passed in is a STRING
        if (type(Pt_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Pt_name = None
        return template.P(self, Pt_name, Pt_name_src, 1)

    #############################################################################
    #                                                                           #
    # Projection functions for VCS.                                             #
    #                                                                           #
    #############################################################################
    def createprojection(self,name=None, source='default'):
        """
 Function: createprojection                # Construct a new projection method

 Description of Function:
    Create a new projection method given the the name and the existing
    projection method to copy the attributes from. If no existing
    projection method name is given, then the default projection
    method will be used as the projection method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Projection
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('projection')
    p=a.createprojection('example1',)
    a.show('projection')
    box=a.createprojection('example2','quick')
    a.show('projection')
"""

        name,source = self.check_name_source(name,source,'projection')
        return projection.Proj(self, name, source, 0)

    def getprojection(self,Proj_name_src='default'):
        """
 Function: getprojection                    # Construct a new projection method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    projection class object from an existing VCS projection method. If
    no projection name is given, then projection 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a 
    different name can be modified. (See the createprojection function.)

 Example of Use:
    a=vcs.init()
    a.show('projection')                   # Show all the existing projection methods
    box=a.getprojection()                  # box instance of 'default' projection
                                        # method
    box2=a.getprojection('quick')          # box2 instance of existing 'quick' projection
                                        #         graphics method
"""

        # Check to make sure the argument passed in is a STRING
        if (type(Proj_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Proj_name = None
        p=projection.Proj(self, Proj_name, Proj_name_src, 1)
        if Proj_name_src!='default' : p.type=p.type
        return p

    #############################################################################
    #                                                                           #
    # Boxfill functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createboxfill(self,name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createboxfill                # Construct a new boxfill graphics method

 Description of Function:
    Create a new boxfill graphics method given the the name and the existing
    boxfill graphics method to copy the attributes from. If no existing
    boxfill graphics method name is given, then the default boxfill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('boxfill')
    box=a.createboxfill('example1',)
    a.show('boxfill')
    box=a.createboxfill('example2','quick')
    a.show('boxfill')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createboxfill Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

        name,source = self.check_name_source(name,source,'boxfill')
        return boxfill.Gfb(self, name, source, 0)
    createboxfill.__doc__ = createboxfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, boxfill_output) 

    def getboxfill(self,Gfb_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::
 Function: getboxfill                        # Construct a new boxfill graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    boxfill class object from an existing VCS boxfill graphics method. If
    no boxfill name is given, then boxfill 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a 
    different name can be modified. (See the createboxfill function.)

 Example of Use:
    a=vcs.init()
    a.show('boxfill')                   # Show all the existing boxfill graphics methods
    box=a.getboxfill()                  # box instance of 'default' boxfill graphics
                                        # method
    box2=a.getboxfill('quick')          # box2 instance of existing 'quick' boxfill
                                        #         graphics method
######################################################################################################################
###########################################                            ###############################################
########################################## End getboxfill Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gfb_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gfb_name = None
        return boxfill.Gfb(self, Gfb_name, Gfb_name_src, 1)
    getboxfill.__doc__ = getboxfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, boxfill_output) 
   

    def boxfill(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: boxfill                        # Generate a boxfill plot

 Description of Function:
    Generate a boxfill plot given the data, boxfill graphics method, and 
    template. If no boxfill class object is given, then the 'default' boxfill
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('boxfill')                        # Show all the existing boxfill graphics methods
    box=a.getboxfill('quick')                # Create instance of 'quick'
    a.boxfill(array,box)                # Plot array using specified box and default 
                                        #         template
    templt=a.gettemplate('AMIP')        # Create an instance of template 'AMIP'
    a.clear()                           # Clear VCS canvas
    a.boxfill(array,box,template)       # Plot array using specified box and template
    a.boxfill(box,array,template)       # Plot array using specified box and template
    a.boxfill(template,array,box)       # Plot array using specified box and template
    a.boxfill(template,array,box)       # Plot array using specified box and template
    a.boxfill(array,'AMIP','quick')     # Use 'AMIP' template and 'quick' boxfill
    a.boxfill('AMIP',array,'quick')     # Use 'AMIP' template and 'quick' boxfill
    a.boxfill('AMIP','quick',array)     # Use 'AMIP' template and 'quick' boxfill

###################################################################################################################
###########################################                         ###############################################
########################################## End boxfill Description ################################################
#########################################                         #################################################
###################################################################################################################

"""
        arglist=_determine_arg_list('boxfill',args)
        return self.__plot(arglist, parms)
    boxfill.__doc__ = boxfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2D_input, plot_output) 

    #############################################################################
    #                                                                           #
    # Taylordiagram functions for VCS.                                          #
    #                                                                           #
    #############################################################################
    def createtaylordiagram(self,name=None, source='default'):
        """
 Function: createtaylordiagram  # Construct a new taylordiagram graphics method

 Description of Function:
    Create a new taylordiagram graphics method given the the name and the existing
    taylordiagram graphics method to copy the attributes from. If no existing
    taylordiagram graphics method name is given, then the default taylordiagram graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('taylordiagram')
    td=a.createtaylordiagram('example1',)
    a.show('taylordiagram')
    td=a.createtaylordiagram('example2','quick')
    a.show('taylordiagram')
"""
        
        name,source = self.check_name_source(name,source,'taylordiagram')
        srcfound=0
        for m in vcs.taylordiagrams:
            if m.name==name :
                raise vcsError, 'Error creating taylordiagram graphic method: '+Gtd_name+' already exist'
            if m.name==source:
                srcfound=1
        if not srcfound:
            raise vcsError, 'Error creating taylordiagram graphic method '+Gtd_name_src+' does not exist'
        n=vcs.taylor.Gtd()
        n._name=name
        for m in vcs.taylordiagrams:
            if m.name==source :
                n.max=m.max
                n.quadrans=m.quadrans
                n.skillValues=m.skillValues
                n.skillColor=m.skillColor
                n.skillDrawLabels=m.skillDrawLabels
                n.skillCoefficient=m.skillCoefficient
                n.detail=m.detail
                n.referencevalue=m.referencevalue
                n.Marker=copy.deepcopy(m.Marker)
                n.arrowlength=m.arrowlength
                n.arrowangle=m.arrowangle
                n.arrowbase=m.arrowbase
                n.xticlabels1=m.xticlabels1
                n.xmtics1=m.xmtics1
                n.yticlabels1=m.yticlabels1
                n.ymtics1=m.xmtics1
                n.cticlabels1=m.cticlabels1
                n.cmtics1=m.xmtics1
                
                break
        vcs.taylordiagrams.append(n)
        n.Marker.equalize()
        return n

    def gettaylordiagram(self,Gtd_name_src='default'):
        """
 Function: gettaylordiagram                     # Construct a new taylordiagram graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    taylordiagram class object from an existing VCS taylordiagram graphics method. If
    no taylordiagram name is given, then taylordiagram 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a 
    different name can be modified. (See the createboxfill function.)

 Example of Use:
    a=vcs.init()
    a.show('taylordiagram')                    # Show all the existing taylordiagram graphics methods
    td=a.gettaylordiagram()                    # td instance of 'default' taylordiagram graphics
                                               # method
    td2=a.gettaylordiagram('default')          # td2 instance of existing 'default' taylordiagram
                                               #         graphics method
                                        """
        
        
        # Check to make sure the argument passed in is a STRING
        if (type(Gtd_name_src) != types.StringType):
            raise vcsError, 'The argument must be a string.'
        
        for m in vcs.taylordiagrams:
            if m.name==Gtd_name_src:
##                 n=copy.copy(m)
                n=m
                #vcs.taylordiagrams.append(n)
                n.Marker.equalize()
                return n
        raise vcsError,'Error, taylordiagram \"'+Gtd_name_src+'\" does not exist'
    
    def taylordiagram(self, *args, **parms):
        """
 Function: taylordiagram                        # Generate an taylordiagram plot

 Description of Function:
    Generate a taylordiagram plot given the data, taylordiagram graphics method, and
    template. If no taylordiagram class object is given, then the 'default' taylordiagram
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('taylordiagram')                   # Show all the existing taylordiagram graphics methods
    td=a.gettaylordiagram()                   # Create instance of 'default'
    a.taylordiagram(array,td)                 # Plot array using specified iso and default 
                                              #       template
    a.clear()                                 # Clear VCS canvas
    a.taylordiagram(array,td,template)        # Plot array using specified iso and template
"""
        arglist=_determine_arg_list('taylordiagram',args)
        return self.__plot(arglist, parms)

    
    #############################################################################
    #                                                                           #
    # Meshfill functions for VCS.                                               #
    #                                                                           #
    #############################################################################
    
    def createmeshfill(self,name=None, source='default'):
        """
 Function: createmeshfill                # Construct a new meshfill graphics method

 Description of Function:
    Create a new meshfill graphics method given the the name and the existing
    meshfill graphics method to copy the attributes from. If no existing
    meshfill graphics method name is given, then the default meshfill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('meshfill')
    mesh=a.createmeshfill('example1',)
    a.show('meshfill')
    mesh=a.createmeshfill('example2','quick')
    a.show('meshfill')
"""

        name,source = self.check_name_source(name,source,'meshfill')
        return meshfill.Gfm(self, name, source, 0)

    def getmeshfill(self,Gfm_name_src='default'):
        """
 Function: getmeshfill                        # Construct a new meshfill graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    meshfill class object from an existing VCS meshfill graphics method. If
    no meshfill name is given, then meshfill 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a 
    different name can be modified. (See the createmeshfill function.)

 Example of Use:
    a=vcs.init()
    a.show('meshfill')                   # Show all the existing meshfill graphics methods
    mesh=a.getmeshfill()                  # mesh instance of 'default' meshfill graphics
                                        # method
    mesh2=a.getmeshfill('quick')          # mesh2 instance of existing 'quick' meshfill
                                        #         graphics method
"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gfm_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gfm_name = None
        return meshfill.Gfm(self, Gfm_name, Gfm_name_src, 1)

   

    def prettifyAxisLabels(self,ticks,axis):
        for k in ticks.keys():
            if len(ticks[k])==0:
                continue
            if axis=="longitude":
                K = k % 360
                if K>180:
                    if int(K)==float(K):
                      ticks[k]="%iW" % (360-K)
                    else:
                      ticks[k]="%.2fW" % (360-K)
                elif K<180:
                    if numpy.allclose(K,0.):
                      ticks[k]="0"
                    elif int(K)==float(K):
                      ticks[k]="%iE" % (K)
                    else:
                      ticks[k]="%.2fE" % (K)
                else:
                  if k==-180.:
                    ticks[k]="180W"
                  else:
                    ticks[k]="180E"
            elif axis=="latitude":
                if k<0:
                    if len(ticks[k])>4:
                      ticks[k]="%.1f" % eval(ticks[k][1:])+"S"
                    else:
                      ticks[k]=ticks[k][1:]+"S"
                elif k>0:
                  if len(ticks[k])>4:
                    ticks[k]="%.1f" % eval(ticks[k])+"N"
                  else:
                    ticks[k]=ticks[k]+"N"
                else:
                    ticks[0]="Eq"
        return ticks
    
    def setTicksandLabels(self,gm,datawc_x1,datawc_x2,datawc_y1,datawc_y2,x=None,y=None):
        """ Sets the labels and ticks for a graphics method made in python
        Usage setTicksandLabels(gm,datawc_x1,datawc_x2,datawc_y1,datawc_y2,x=None,y=None)
        datawc are world coordinates
        
        """
        if isinstance(gm,vcs.taylor.Gtd):
            return
        # Now the template stuff
        # first create the dictionary to remember which ones are changed
        dic={}
        for i in ('xticlabels1','xmtics1','xticlabels2','xmtics2','yticlabels1','ymtics1','yticlabels2','ymtics2'):
            dic[i]=False
        #xticklabels1
        if gm.xticlabels1 is None or gm.xticlabels1=='*':
            if x=="longitude" and abs(datawc_x2-datawc_x1)>30:
              ticks="lon30"
            else:
              ticks=vcs.mkscale(datawc_x1,datawc_x2)
              ticks=self.prettifyAxisLabels(vcs.mklabels(ticks),x)
            ## for k in ticks.keys() : # make sure you're in the range
            ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
            ##         del(ticks[k])
            setattr(gm,'xticlabels1',ticks)
            dic['xticlabels1']=True
        #xmtics1
        if gm.xmtics1 is None or gm.xmtics1=='*':
            ticks=vcs.mkscale(datawc_x1,datawc_x2)
            tick2=[]
            for i in range(len(ticks)-1):
                tick2.append((ticks[i]+ticks[i+1])/2.)
            ticks=self.prettifyAxisLabels(vcs.mklabels(tick2),x)
            ## for k in ticks.keys() : # make sure you're in the range
            ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
            ##         del(ticks[k])
            setattr(gm,'xmtics1',ticks)
            dic['xmtics1']=True
        #xticklabels2
        if  hasattr(gm,"xticlabels2") and (gm.xticlabels2 is None or gm.xticlabels2=='*'):
            ticks=vcs.mkscale(datawc_x1,datawc_x2)
            ticks=self.prettifyAxisLabels(vcs.mklabels(ticks),x)
            ## for k in ticks.keys():
            ##     ticks[k]=''
            ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
            ##         del(ticks[k])
            setattr(gm,'xticlabels2',ticks)
            dic['xticlabels2']=True
        #xmtics2
        if hasattr(gm,"xmtics2") and (gm.xmtics2 is None or gm.xmtics2=='*'):
            ticks=vcs.mkscale(datawc_x1,datawc_x2)
            tick2=[]
            for i in range(len(ticks)-1):
                tick2.append((ticks[i]+ticks[i+1])/2.)
            ticks=self.prettifyAxisLabels(vcs.mklabels(tick2),x)
            ## for k in ticks.keys() : # make sure you're in the range
            ##     if k<numpy.minimum(datawc_x1,datawc_x2) or k>numpy.maximum(datawc_x2,datawc_x1):
            ##         del(ticks[k])
            setattr(gm,'xmtics2',ticks)
            dic['xmtics2']=True
        #yticklabels1
        if gm.yticlabels1 is None or gm.yticlabels1=='*':
            if y=="latitude" and abs(datawc_y2-datawc_y1)>20:
              ticks="lat20"
            else:
              ticks=vcs.mkscale(datawc_y1,datawc_y2)
              ticks=self.prettifyAxisLabels(vcs.mklabels(ticks),y)
            ## for k in ticks.keys() : # make sure you're in the range
            ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
            ##         del(ticks[k])
            setattr(gm,'yticlabels1',ticks)
            dic['yticlabels1']=True
        #ymtics1
        if gm.ymtics1 is None or gm.ymtics1=='*':
            ticks=vcs.mkscale(datawc_y1,datawc_y2)
            tick2=[]
            for i in range(len(ticks)-1):
                tick2.append((ticks[i]+ticks[i+1])/2.)
            ticks=self.prettifyAxisLabels(vcs.mklabels(tick2),y)
            ## for k in ticks.keys() : # make sure you're in the range
            ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
            ##         del(ticks[k])
            setattr(gm,'ymtics1',ticks)
            dic['ymtics1']=True
        #yticklabels2
        if hasattr(gm,"yticlabels2") and (gm.yticlabels2 is None or gm.yticlabels2=='*'):
            ticks=vcs.mkscale(datawc_y1,datawc_y2)
            ticks=self.prettifyAxisLabels(vcs.mklabels(ticks),y)
            ## for k in ticks.keys():
            ##     ticks[k]=''
            ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
            ##         del(ticks[k])
            setattr(gm,'yticlabels2',ticks)
            dic['yticlabels2']=True
        #ymtics2
        if hasattr(gm,"ymtics2") and (gm.ymtics2 is None or gm.ymtics2=='*'):
            ticks=vcs.mkscale(datawc_y1,datawc_y2)
            tick2=[]
            for i in range(len(ticks)-1):
                tick2.append((ticks[i]+ticks[i+1])/2.)
            ticks=self.prettifyAxisLabels(vcs.mklabels(tick2),y)
            ## for k in ticks.keys() : # make sure you're in the range
            ##     if k<numpy.minimum(datawc_y1,datawc_y2) or k>numpy.maximum(datawc_y2,datawc_y1):
            ##         del(ticks[k])
            setattr(gm,'ymtics2',ticks)
            dic['ymtics2']=True
        return dic
        

    def meshfill(self,*args, **parms):
        """
 Function: meshfill               # Generate an meshfill plot

 Description of Function:
    Generate a meshfill plot given the data, the mesh, a meshfill graphics method, and
    a template. If no meshfill class object is given, then the 'default' meshfill
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

    Format:
    This function expects 1D data (any extra dimension will be used for animation)
    In addition the mesh array must be of the same shape than data with 2 additional dimension representing the vertices coordinates for the Y (0) and X (1) dimension
    Let's say you want to plot a spatial assuming mesh containing 10,000 grid cell, then data must be shape (10000,) or (n1,n2,n3,...,10000) if additional dimensions exist (ex time,level), these dimension would be used only for animation and will be ignored in the rest of this example.
    The shape of the mesh, assuming 4 vertices per grid cell, must be (1000,2,4), where the array [:,0,:] represent the Y coordinates of the vertices (clockwise or counterclockwise) and the array [:,1:] represents the X coordinates of the vertices (the same clockwise/counterclockwise than the Y coordinates)
    In brief you'd have:
    data.shape=(10000,)
    mesh.shape=(10000,2,4)
    
 Example of Use:
    a=vcs.init()
    a.show('meshfill')                   # Show all the existing meshfill graphics methods
    mesh=a.getmeshfill()                 # Create instance of 'default'
    a.meshfill(array,mesh)               # Plot array using specified mesh and default 
                                         #       template
    a.clear()                            # Clear VCS canvas
    a.meshfill(array,mesh,mesh_graphic_method,template) # Plot array using specified mesh mesh graphic method and template
"""
        arglist=_determine_arg_list('meshfill',args)
        return self.__plot(arglist, parms)

    #############################################################################
    #                                                                           #
    # Isofill functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createisofill(self,name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createisofill  # Construct a new isofill graphics method

 Description of Function:
    Create a new isofill graphics method given the the name and the existing
    isofill graphics method to copy the attributes from. If no existing
    isofill graphics method name is given, then the default isofill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('isofill')
    iso=a.createisofill('example1',)
    a.show('isofill')
    iso=a.createisofill('example2','quick')
    a.show('isofill')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createisofill Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

        name,source = self.check_name_source(name,source,'isofill')

        return isofill.Gfi(self, name, source, 0)
    createisofill.__doc__ = createisofill.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, isofill_output)

    def getisofill(self,Gfi_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getisofill          Construct a new isofill graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    isofill class object from an existing VCS isofill graphics method. If
    no isofill name is given, then isofill 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createisofill function.)

 Example of Use:
    a=vcs.init()
    a.show('isofill')                   # Show all the existing isofill graphics methods
    iso=a.getisofill()                  # iso instance of 'default' isofill graphics
                                        #       method
    iso2=a.getisofill('quick')          # iso2 instance of existing 'quick' isofill
                                        #       graphics method
######################################################################################################################
###########################################                            ###############################################
########################################## End getisofill Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gfi_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gfi_name = None
        return isofill.Gfi(self, Gfi_name, Gfi_name_src, 1)
    getisofill.__doc__ = getisofill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, isofill_output)

    def isofill(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: isofill                        # Generate an isofill plot

 Description of Function:
    Generate a isofill plot given the data, isofill graphics method, and
    template. If no isofill class object is given, then the 'default' isofill
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('isofill')                   # Show all the existing isofill graphics methods
    iso=a.getisofill('quick')           # Create instance of 'quick'
    a.isofill(array,iso)                # Plot array using specified iso and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.isofill(array,iso,template)       # Plot array using specified iso and template

###################################################################################################################
###########################################                         ###############################################
########################################## End isofill Description ################################################
#########################################                         #################################################
###################################################################################################################

"""
        arglist=_determine_arg_list('isofill',args)            
        return self.__plot(arglist, parms)
    isofill.__doc__ = isofill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2D_input, plot_output)


    #############################################################################
    #                                                                           #
    # Isoline functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createisoline(self, name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createisoline                # Construct a new isoline graphics method

 Description of Function:
    Create a new isoline graphics method given the the name and the existing
    isoline graphics method to copy the attributes from. If no existing
    isoline graphics method name is given, then the default isoline graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:

    a=vcs.init()
    a.show('isoline')
    iso=a.createisoline('example1',)
    a.show('isoline')
    iso=a.createisoline('example2','quick')
    a.show('isoline')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createisoline Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

        name,source = self.check_name_source(name,source,'isoline')
        return isoline.Gi(self, name, source, 0)
    createisoline.__doc__ = createisoline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, isoline_output)

    def getisoline(self,Gi_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getisoline                        # Construct a new isoline graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    isoline class object from an existing VCS isoline graphics method. If
    no isoline name is given, then isoline 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createisoline function.)

 Example of Use:
    a=vcs.init()
    a.show('isoline')                   # Show all the existing isoline graphics methods
    iso=a.getisoline()                  # iso instance of 'default' isoline graphics
                                        #       method
    iso2=a.getisoline('quick')          # iso2 instance of existing 'quick' isoline
                                        #       graphics method
######################################################################################################################
###########################################                            ###############################################
########################################## End getisoline Description ################################################
#########################################                            #################################################
######################################################################################################################
"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gi_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gi_name = None
        return isoline.Gi(self, Gi_name, Gi_name_src, 1)
    getisoline.__doc__ = getisoline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, isoline_output)

    def isoline(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: isoline                        # Generate an isoline plot

 Description of Function:
    Generate a isoline plot given the data, isoline graphics method, and
    template. If no isoline class object is given, then the 'default' isoline
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('isoline')                   # Show all the existing isoline graphics methods
    iso=a.getisoline('quick')           # Create instance of 'quick'
    a.isoline(array,iso)                # Plot array using specified iso and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.isoline(array,iso,template)       # Plot array using specified iso and template

###################################################################################################################
###########################################                         ###############################################
########################################## End isoline Description ################################################
#########################################                         #################################################
###################################################################################################################

"""
        arglist=_determine_arg_list('isoline',args)
        return self.__plot(arglist, parms)
    isoline.__doc__ = isoline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2D_input, plot_output)

    #############################################################################
    #                                                                           #
    # Outline functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createoutline(self, name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createoutline                # Construct a new outline graphics method

 Description of Function:
    Create a new outline graphics method given the the name and the existing
    outline graphics method to copy the attributes from. If no existing
    outline graphics method name is given, then the default outline graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:

    a=vcs.init()
    a.show('outline')
    out=a.createoutline('example1',)
    a.show('outline')
    out=a.createoutline('example2','quick')
    a.show('outline')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createoutline Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

        name,source = self.check_name_source(name,source,'outline')

        return outline.Go(self, name, source, 0)
    createoutline.__doc__ = createoutline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, outline_output)

    def getoutline(self,Go_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getoutline                        # Construct a new outline graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    outline class object from an existing VCS outline graphics method. If
    no outline name is given, then outline 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createoutline function.)

 Example of Use:
    a=vcs.init()
    a.show('outline')                   # Show all the existing outline graphics methods
    out=a.getoutline()                  # out instance of 'default' outline graphics
                                        #       method
    out2=a.getoutline('quick')          # out2 instance of existing 'quick' outline
                                        #       graphics method

######################################################################################################################
###########################################                            ###############################################
########################################## End getoutline Description ################################################
#########################################                            #################################################
######################################################################################################################
"""

        # Check to make sure the argument passed in is a STRING
        if (type(Go_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Go_name = None
        return outline.Go(self, Go_name, Go_name_src, 1)
    getoutline.__doc__ = getoutline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, outline_output)

    def outline(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: outline                        # Generate an outline plot

 Description of Function:
    Generate a outline plot given the data, outline graphics method, and
    template. If no outline class object is given, then the 'default' outline
    graphics method is used. Simerly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('outline')                   # Show all the existing outline graphics methods
    out=a.getoutline('quick')           # Create instance of 'quick'
    a.outline(array,out)                # Plot array using specified out and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.outline(array,out,template)       # Plot array using specified out and template

###################################################################################################################
###########################################                         ###############################################
########################################## End outline Description ################################################
#########################################                         #################################################
###################################################################################################################

"""
        arglist=_determine_arg_list('outline',args)
        return self.__plot(arglist, parms)
    outline.__doc__ = outline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2D_input, plot_output)

    #############################################################################
    #                                                                           #
    # Outfill functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createoutfill(self, name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createoutfill                # Construct a new outfill graphics method

 Description of Function:
    Create a new outfill graphics method given the the name and the existing
    outfill graphics method to copy the attributes from. If no existing
    outfill graphics method name is given, then the default outfill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:

    a=vcs.init()
    a.show('outfill')
    out=a.createoutfill('example1',)
    a.show('outfill')
    out=a.createoutfill('example2','quick')
    a.show('outfill')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createoutfill Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

        name,source = self.check_name_source(name,source,'outfill')

        return outfill.Gfo(self, name, source, 0)
    createoutfill.__doc__ = createoutfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, outfill_output)

    def getoutfill(self,Gfo_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getoutfill                        # Construct a new outfill graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    outfill class object from an existing VCS outfill graphics method. If
    no outfill name is given, then outfill 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createoutfill function.)

 Example of Use:
    a=vcs.init()
    a.show('outfill')                   # Show all the existing outfill graphics methods
    out=a.getoutfill()                  # out instance of 'default' outfill graphics
                                        #       method
    out2=a.getoutfill('quick')          # out2 instance of existing 'quick' outfill
                                        #       graphics method

######################################################################################################################
###########################################                            ###############################################
########################################## End getoutfill Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gfo_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gfo_name = None
        return outfill.Gfo(self, Gfo_name, Gfo_name_src, 1)
    getoutfill.__doc__ = getoutfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, outfill_output)

    def outfill(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: outfill                        # Generate an outfill plot

 Description of Function:
    Generate a outfill plot given the data, outfill graphics method, and
    template. If no outfill class object is given, then the 'default' outfill
    graphics method is used. Simerly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('outfill')                   # Show all the existing outfill graphics methods
    out=a.getoutfill('quick')           # Create instance of 'quick'
    a.outfill(array,out)                # Plot array using specified out and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.outfill(array,out,template)       # Plot array using specified out and template

###################################################################################################################
###########################################                         ###############################################
########################################## End outfill Description ################################################
#########################################                         #################################################
###################################################################################################################

"""
        arglist=_determine_arg_list('outfill',args)
        return self.__plot(arglist, parms)
    outfill.__doc__ = outfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2D_input, plot_output)

    #############################################################################
    #                                                                           #
    # Xyvsy functions for VCS.                                                  #
    #                                                                           #
    #############################################################################
    def createxyvsy(self,name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createxyvsy                  # Construct a new Xyvsy graphics method

 Description of Function:
    Create a new Xyvsy graphics method given the the name and the existing
    Xyvsy graphics method to copy the attributes from. If no existing
    Xyvsy graphics method name is given, then the default Xyvsy graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:

    a=vcs.init()
    a.show('xyvsy')
    xyy=a.createxyvsy('example1',)
    a.show('xyvsy')
    xyy=a.createxyvsy('example2','quick')
    a.show('xyvsy')

#######################################################################################################################
###########################################                             ###############################################
########################################## End createxyvsy Description ################################################
#########################################                             #################################################
#######################################################################################################################

"""

        name,source = self.check_name_source(name,source,'xyvsy')

        return xyvsy.GXy(self, name, source, 0)
    createxyvsy.__doc__ = createxyvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, xyvsy_output) 

    def getxyvsy(self,GXy_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getxyvsy        # Construct a new Xyvsy graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    Xyvsy class object from an existing VCS Xyvsy graphics method. If
    no Xyvsy name is given, then Xyvsy 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createxyvsy function.)

 Example of Use:
    a=vcs.init()
    a.show('xyvsy')                     # Show all the existing Xyvsy graphics methods
    xyy=a.getxyvsy()                    # xyy instance of 'default' Xyvsy graphics
                                        #       method
    xyy2=a.getxyvsy('quick')            # xyy2 instance of existing 'quick' Xyvsy
                                        #       graphics method
####################################################################################################################
###########################################                          ###############################################
########################################## End getxyvsy Description ################################################
#########################################                          #################################################
####################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(GXy_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        GXy_name = None
        return xyvsy.GXy(self, GXy_name, GXy_name_src, 1)
    getxyvsy.__doc__ = getxyvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, get_GM_input, xyvsy_output) 

    def xyvsy(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: xyvsy                        # Generate a Xyvsy plot

 Description of Function:
    Generate a Xyvsy plot given the data, Xyvsy graphics method, and
    template. If no Xyvsy class object is given, then the 'default' Xyvsy
    graphics method is used. Simerly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('xyvsy')                   # Show all the existing Xyvsy graphics methods
    xyy=a.getxyvsy('quick')           # Create instance of 'quick'
    a.xyvsy(array,xyy)                # Plot array using specified xyy and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.xyvsy(array,xyy,template)       # Plot array using specified xyy and template

#################################################################################################################
###########################################                       ###############################################
########################################## End xyvsy Description ################################################
#########################################                       #################################################
#################################################################################################################

"""
        arglist=_determine_arg_list('xyvsy',args)
        return self.__plot(arglist, parms)
    xyvsy.__doc__ = xyvsy.__doc__ % (plot_keywords_doc,graphics_method_core,xaxisconvert,plot_1D_input, plot_output)

    #############################################################################
    #                                                                           #
    # Yxvsx functions for VCS.                                                  #
    #                                                                           #
    #############################################################################
    def createyxvsx(self, name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createyxvsx                  # Construct a new Yxvsx graphics method

 Description of Function:
    Create a new Yxvsx graphics method given the the name and the existing
    Yxvsx graphics method to copy the attributes from. If no existing
    Yxvsx graphics method name is given, then the default Yxvsx graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:

    a=vcs.init()
    a.show('yxvsx')
    yxx=a.createyxvsx('example1',)
    a.show('yxvsx')
    yxx=a.createyxvsx('example2','quick')
    a.show('yxvsx')

#######################################################################################################################
###########################################                             ###############################################
########################################## End createyxvsx Description ################################################
#########################################                             #################################################
#######################################################################################################################

"""

        name,source = self.check_name_source(name,source,'yxvsx')

        return yxvsx.GYx(self, name, source, 0)
    createyxvsx.__doc__ = createyxvsx.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, yxvsx_output) 

    def getyxvsx(self,GYx_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getyxvsx                     # Construct a new Yxvsx graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    Yxvsx class object from an existing VCS Yxvsx graphics method. If
    no Yxvsx name is given, then Yxvsx 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createyxvsx function.)

 Example of Use:
    a=vcs.init()
    a.show('yxvsx')                     # Show all the existing Yxvsx graphics methods
    yxx=a.getyxvsx()                    # yxx instance of 'default' Yxvsx graphics
                                        #       method
    yxx2=a.getyxvsx('quick')            # yxx2 instance of existing 'quick' Yxvsx
                                        #       graphics method
####################################################################################################################
###########################################                          ###############################################
########################################## End getyxvsx Description ################################################
#########################################                          #################################################
####################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(GYx_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        GYx_name = None
        return yxvsx.GYx(self, GYx_name, GYx_name_src, 1)
    getyxvsx.__doc__ = getyxvsx.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, get_GM_input, yxvsx_output) 

    def yxvsx(self, *args, **parms):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::
 
 Function: yxvsx                        # Generate a Yxvsx plot

 Description of Function:
    Generate a Yxvsx plot given the data, Yxvsx graphics method, and
    template. If no Yxvsx class object is given, then the 'default' Yxvsx
    graphics method is used. Simerly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('yxvsx')                   # Show all the existing Yxvsx graphics methods
    yxx=a.getyxvsx('quick')           # Create instance of 'quick'
    a.yxvsx(array,yxx)                # Plot array using specified yxx and default
                                      #       template
    a.clear()                         # Clear VCS canvas
    a.yxvsx(array,yxx,template)       # Plot array using specified yxx and template

#################################################################################################################
###########################################                       ###############################################
########################################## End yxvsx Description ################################################
#########################################                       #################################################
#################################################################################################################

"""
        arglist=_determine_arg_list('yxvsx',args)
        return self.__plot(arglist, parms)
    yxvsx.__doc__ = yxvsx.__doc__ % (plot_keywords_doc,graphics_method_core,xaxisconvert,plot_1D_input, plot_output) 

    #############################################################################
    #                                                                           #
    # XvsY functions for VCS.                                                   #
    #                                                                           #
    #############################################################################
    def createxvsy(self, name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createxvsy                      # Construct a new XvsY graphics method

 Description of Function:
    Create a new XvsY graphics method given the the name and the existing
    XvsY graphics method to copy the attributes from. If no existing
    XvsY graphics method name is given, then the default XvsY graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('xvsy')
    xy=a.createxvsy('example1',)
    a.show('xvsy')
    xy=a.createxvsy('example2','quick')
    a.show('xvsy')

######################################################################################################################
###########################################                            ###############################################
########################################## End createxvsy Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

        name,source = self.check_name_source(name,source,'xvsy')

        return xvsy.GXY(self, name, source, 0)
    createxvsy.__doc__ = createxvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, xvsy_output) 

    def getxvsy(self,GXY_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getxvsy                   # Construct a new XvsY graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    XvsY class object from an existing VCS XvsY graphics method. If
    no XvsY name is given, then XvsY 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createxvsy function.)

 Example of Use:
    a=vcs.init()
    a.show('xvsy')                      # Show all the existing XvsY graphics methods
    xy=a.getxvsy()                      # xy instance of 'default' XvsY graphics
                                        #       method
    xy2=a.getxvsy('quick')              # xy2 instance of existing 'quick' XvsY
                                        #       graphics method

###################################################################################################################
###########################################                         ###############################################
########################################## End getxvsy Description ################################################
#########################################                         #################################################
###################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(GXY_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        GXY_name = None
        return xvsy.GXY(self, GXY_name, GXY_name_src, 1)
    getxvsy.__doc__ = getxvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, get_GM_input, xvsy_output) 

    def xvsy(self, *args, **parms):
        """
Options:::
%s
%s
%s
%s
    :::
 Output:::
%s
    :::

 Function: xvsy                      # Generate a XvsY plot

 Description of Function:
    Generate a XvsY plot given the data, XvsY graphics method, and
    template. If no XvsY class object is given, then the 'default' XvsY
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('xvsy')                   # Show all the existing XvsY graphics methods
    xy=a.getxvsy('quick')            # Create instance of 'quick'
    a.xvsy(array,xy)                 # Plot array using specified xy and default 
                                     #       template
    a.clear()                        # Clear VCS canvas
    a.xvsy(array,xy,template)        # Plot array using specified xy and template

#################################################################################################################
###########################################                       ###############################################
########################################## End xvsy Description ################################################
#########################################                       #################################################
#################################################################################################################

"""
        arglist=_determine_arg_list('xvsy',args)
        return self.__plot(arglist, parms)
    xvsy.__doc__ = xvsy.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2_1D_input, plot_output)

    #############################################################################
    #                                                                           #
    # Vector functions for VCS.                                                 #
    #                                                                           #
    #############################################################################
    def createvector(self, name=None, source='default'):
        """
 Function: createvector                # Construct a new vector graphics method

 Description of Function:
    Create a new vector graphics method given the the name and the existing
    vector graphics method to copy the attributes from. If no existing
    vector graphics method name is given, then the default vector graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('vector')
    vec=a.createvector('example1',)
    a.show('vector')
    vec=a.createvector('example2','quick')
    a.show('vector')
"""

        name,source = self.check_name_source(name,source,'vector')

        return vector.Gv(self, name, source, 0)

    def getvector(self,Gv_name_src='default'):
        """
 Function: getvector                   # Construct a new vector graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    vector class object from an existing VCS vector graphics method. If
    no vector name is given, then vector 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createvector function.)

 Example of Use:
    a=vcs.init()
    a.show('vector')                   # Show all the existing vector graphics methods
    vec=a.getvector()                  # vec instance of 'default' vector graphics
                                        #       method
    vec2=a.getvector('quick')          # vec2 instance of existing 'quick' vector
                                        #       graphics method
"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gv_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gv_name = None
        return vector.Gv(self, Gv_name, Gv_name_src, 1)
 
    def vector(self, *args, **parms):
        """
 Function: vector                      # Generate a vector plot

 Description of Function:
    Generate a vector plot given the data, vector graphics method, and
    template. If no vector class object is given, then the 'default' vector
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('vector')                   # Show all the existing vector graphics methods
    vec=a.getvector('quick')           # Create instance of 'quick'
    a.vector(array,vec)                # Plot array using specified vec and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.vector(array,vec,template)       # Plot array using specified vec and template
"""
        arglist=_determine_arg_list('vector',args)
        return self.__plot(arglist, parms)

    #############################################################################
    #                                                                           #
    # Scatter functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createscatter(self, name=None, source='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: createscatter                # Construct a new scatter graphics method

 Description of Function:
    Create a new scatter graphics method given the the name and the existing
    scatter graphics method to copy the attributes from. If no existing
    scatter graphics method name is given, then the default scatter graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('scatter')
    sct=a.createscatter('example1',)
    a.show('scatter')
    sct=a.createscatter('example2','quick')
    a.show('scatter')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createscatter Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

        name,source = self.check_name_source(name,source,'scatter')

        return scatter.GSp(self, name, source, 0)
    createscatter.__doc__ = createscatter.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, scatter_output)

    def getscatter(self,GSp_name_src='default'):
        """
Options:::
%s
%s
%s
:::
 Input:::
%s
    :::
 Output:::
%s
    :::

 Function: getscatter                   # Construct a new scatter graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    scatter class object from an existing VCS scatter graphics method. If
    no scatter name is given, then scatter 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createscatter function.)

 Example of Use:
    a=vcs.init()
    a.show('scatter')                   # Show all the existing scatter graphics methods
    sct=a.getscatter()                  # sct instance of 'default' scatter graphics
                                        #       method
    sct2=a.getscatter('quick')          # sct2 instance of existing 'quick' scatter
                                        #       graphics method

######################################################################################################################
###########################################                            ###############################################
########################################## End getscatter Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

        # Check to make sure the argument passed in is a STRING
        if (type(GSp_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        GSp_name = None
        return scatter.GSp(self, GSp_name, GSp_name_src, 1)
    getscatter.__doc__ = getscatter.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, scatter_output)

    def scatter(self, *args, **parms):
        """
Options:::
%s
%s
%s
%s
    :::
 Output:::
%s
    :::

 Function: scatter                      # Generate a scatter plot

 Description of Function:
    Generate a scatter plot given the data, scatter graphics method, and
    template. If no scatter class object is given, then the 'default' scatter
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('scatter')                   # Show all the existing scatter graphics methods
    sct=a.getscatter('quick')           # Create instance of 'quick'
    a.scatter(array,sct)                # Plot array using specified sct and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.scatter(array,sct,template)       # Plot array using specified sct and template

###################################################################################################################
###########################################                         ###############################################
########################################## End scatter Description ################################################
#########################################                         #################################################
###################################################################################################################

"""

        arglist=_determine_arg_list('scatter',args)
        return self.__plot(arglist, parms)
    scatter.__doc__ = scatter.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert,plot_2_1D_input, plot_output)


    #############################################################################
    #                                                                           #
    # Continents functions for VCS.                                             #
    #                                                                           #
    #############################################################################
    def createcontinents(self, name=None, source='default'):
        """
 Function: createcontinents               # Construct a new continents graphics method

 Description of Function:
    Create a new continents graphics method given the the name and the existing
    continents graphics method to copy the attributes from. If no existing
    continents graphics method name is given, then the default continents graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then a error will be returned. Graphics
    method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('continents')
    con=a.createcontinents('example1',)
    a.show('continents')
    con=a.createcontinents('example2','quick')
    a.show('continents')
"""

        name,source = self.check_name_source(name,source,'continents')

        return continents.Gcon(self, name, source, 0)

    def getcontinents(self,Gcon_name_src='default'):
        """
 Function: getcontinents               # Construct a new continents graphics method

 Description of Function:
    VCS contains a list of graphics methods. This function will create a
    continents class object from an existing VCS continents graphics method. If
    no continents name is given, then continents 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute
    sets. However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createcontinents function.)

 Example of Use:
    a=vcs.init()
    a.show('continents')              # Show all the existing continents graphics
                                      # methods
    con=a.getcontinents()               # con instance of 'default' continents graphics
                                        #       method
    con2=a.getcontinents('quick')       # con2 instance of existing 'quick' continents
                                        #       graphics method
"""

        # Check to make sure the argument passed in is a STRING
        if (type(Gcon_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Gcon_name = None
        return continents.Gcon(self, Gcon_name, Gcon_name_src, 1)

    def continents(self, *args, **parms):
        """
 Function: continents                      # Generate a continents plot

 Description of Function:
    Generate a continents plot given the data, continents graphics method, and
    template. If no continents class object is given, then the 'default' continents
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

 Example of Use:
    a=vcs.init()
    a.show('continents')                # Show all the existing continents graphics
                                        # methods
    con=a.getcontinents('quick')        # Create instance of 'quick'
    a.continents(array,con)             # Plot array using specified con and default 
                                        #       template
    a.clear()                           # Clear VCS canvas
    a.continents(array,con,template)    # Plot array using specified con and template
"""
        arglist=_determine_arg_list('continents',args)
        return self.__plot(arglist, parms)

    #############################################################################
    #                                                                           #
    # Line  functions for VCS.                                                  #
    #                                                                           #
    #############################################################################
    def createline(self,name=None, source='default', ltype=None, 
                 width=None, color=None, priority=1,
                 viewport=None, worldcoordinate=None,
                 x=None, y=None, projection='default'):
        """
 Function: createline                       # Construct a new line secondary method

 Description of Function:
    Create a new line secondary method given the the name and the existing
    line secondary method to copy the attributes from. If no existing line
    secondary method name is given, then the default line secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    If the name provided already exists, then a error will be returned. 
    Secondary method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('line')
    ln=a.createline('example1',)
    a.show('line')
    ln=a.createline('example2','black')
    a.show('line')
    ln2=a.createline(name='new', name_src='red',ltype='dash', width=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of line object 'red'
    a.line(ln2)                      # Plot using specified line object
"""

        name,source = self.check_name_source(name,source,'line')

        ln = line.Tl(self, name, source, 0)
        if (ltype is not None):
            ln.type = ltype
        if (width is not None):
            ln.width = width
        if (color is not None):
            ln.color = color
        if (priority is not None):
            ln.priority = priority
        if (viewport is not None):
            ln.viewport = viewport
        elif source=='default':
            ln.viewport = self.viewport
        if (worldcoordinate is not None):
            ln.worldcoordinate = worldcoordinate
        elif source=='default':
            ln.worldcoordinate = self.worldcoordinate
        if (x is not None):
            ln.x = x
        if (y is not None):
            ln.y = y
        ln.projection=projection
        return ln

    def getline(self,name='default', ltype=None, width=None, color=None,
                 priority=None, viewport=None,
                 worldcoordinate=None,
                 x=None, y=None):
        """
 Function: getline        # Construct a new line secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create a
    line class object from an existing VCS line secondary method. If
    no line name is given, then line 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createline function.)

 Example of Use:
    a=vcs.init()
    a.show('line')                   # Show all the existing line secondary methods
    ln=a.getline()                   # ln instance of 'default' line secondary
                                     #       method
    ln2=a.getline('quick')           # ln2 instance of existing 'quick' line
                                     #       secondary method
    ln3=a.getline(name='red', ltype='dash', width=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of line object 'red'
    a.line(ln3)                      # Plot using specified line object
"""

        # Check to make sure the argument passed in is a STRING
        if (type(name) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Tl_name = None
        ln = line.Tl(self, Tl_name, name, 1)
        if ltype is not None and ln.name!='default':
            ln.type=ltype
        if width is not None and ln.name!='default':
            ln.width = width
        if color is not None and ln.name!='default':
            ln.color=color
        if priority is not None and ln.name!='default':
            ln.priority=priority
        if viewport is not None and ln.name!='default':
            ln.viewport=viewport
        if worldcoordinate is not None and ln.name!='default':
            ln.worldcooridnate = worldcoordinate
        if viewport is not None and ln.name!='default':
            ln.viewport = viewport
        if x is not None and ln.name!='default':
            ln.x = x
        if y is not None and ln.name!='default':
            ln.y = y
        return ln
    

## Following comented out by C.Doutriaux on 8/16/04 because getline('default') returns None
##         try:
##            ln = line.Tl(self, Tl_name, name, 1)
##            if (ltype is not None) and (ln.name != "default"):
##               ln.type = ltype
##            if (width is not None) and (ln.name != "default"):
##               ln.width = width
##            if (color is not None) and (ln.name != "default"):
##               ln.color = color
##            if (priority is not None) and (ln.name != "default"):
##               ln.priority = priority
##            if (self.viewport is None):
##               if (viewport is not None) and (ln.name != "default"):
##                  ln.viewport = viewport
##            else:
##               ln.viewport = self.viewport
##            if (self.worldcoordinate is None):
##               if (worldcoordinate is not None) and (ln.name != "default"):
##                  ln.worldcoordinate = worldcoordinate
##            else:
##               ln.worldcoordinate = self.worldcoordinate
##            if (x is not None) and (ln.name != "default"):
##               ln.x = x
##            if (y is not None) and (ln.name != "default"):
##               ln.y = y
##            return ln
##         except:
##            pass


    def line(self, *args, **parms):
        """
 Function: line                           # Generate a line plot

 Description of Function:
    Plot a line segment on the Vcs Canvas. If no line class 
    object is given, then an error will be returned.

 Example of Use:
    a=vcs.init()
    a.show('line')                      # Show all the existing line objects
    ln=a.getline('red')                 # Create instance of 'red'
    ln.width=4                          # Set the line width
    ln.color = 242                      # Set the line color
    ln.type = 4                         # Set the line type
    ln.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]] # Set the x value points
    ln.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]] # Set the y value points
    a.line(ln)                          # Plot using specified line object
"""
        arglist=_determine_arg_list('line',args)
        return self.__plot(arglist, parms)

    def drawline(self, name=None, ltype='solid', width=1, color=241,
                 priority=1, viewport=[0.0,1.0,0.0,1.0],
                 worldcoordinate=[0.0,1.0,0.0,1.0],
                 x=None, y=None,projection='default',bg=0):
        """
 Function: drawline                           # Generate and draw a line plot

 Description of Function:
    Generate and draw a line object on the VCS Canvas.

 Example of Use:
    a=vcs.init()
    a.show('line')                      # Show all the existing line objects
    ln=a.drawline(name='red', ltype='dash', width=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of line object 'red'
    a.line(ln)                          # Plot using specified line object
"""
        if (name is None) or (type(name) != types.StringType):
            raise vcsError, 'Must provide string name for the line.'
        else:
            lo = self.listelements('line')
            if name in lo:
               ln = self.getline( name )
            else:
               ln = self.createline( name )
        ln.type = ltype
        ln.width = width
        ln.color = color
        ln.priority = priority
        ln.viewport = viewport
        ln.worldcoordinate = worldcoordinate
        ln.x = x
        ln.y = y
        ln.projection=projection
        self.line( ln ,bg=bg)

        return ln


    #############################################################################
    #                                                                           #
    # Marker  functions for VCS.                                                #
    #                                                                           #
    #############################################################################
    def createmarker(self,name=None, source='default', mtype=None,
                 size=None, color=None,priority=1,
                 viewport=None, worldcoordinate=None,
                 x=None, y=None,projection=None):
        """
 Function: createmarker                   # Construct a new marker secondary method

 Description of Function:
    Create a new marker secondary method given the the name and the existing
    marker secondary method to copy the attributes from. If no existing marker
    secondary method name is given, then the default marker secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    If the name provided already exists, then a error will be returned.
    Secondary method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('marker')
    mrk=a.createmarker('example1',)
    a.show('marker')
    mrk=a.createmarker('example2','black')
    a.show('boxfill')
    mrk2=a.createmarker(name='new', name_src='red',mtype='dash', size=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of marker object 'red'
    a.marker(mrk2)                      # Plot using specified marker object
"""

        name,source = self.check_name_source(name,source,'marker')

        mrk = marker.Tm(self, name, source, 0)
        if (mtype is not None):
            mrk.type = mtype
        if (size is not None):
            mrk.size = size 
        if (color is not None):
            mrk.color = color
        if (priority is not None):
            mrk.priority = priority
        if (viewport is not None):
            mrk.viewport = viewport
        elif source=='default':
            mrk.viewport = self.viewport
        if (worldcoordinate is not None):
            mrk.worldcoordinate = worldcoordinate
        elif source == 'default':
            mrk.worldcoordinate = self.worldcoordinate
        if (x is not None):
            mrk.x = x
        if (y is not None):
            mrk.y = y
        if (projection is not None):
            mrk.projection=projection
        return mrk

    def getmarker(self,name='default', mtype=None, size=None, color=None,
                 priority=None, viewport=None,
                 worldcoordinate=None,
                 x=None, y=None):
        """
 Function: getmarker                      # Construct a new marker secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create a
    marker class object from an existing VCS marker secondary method. If
    no marker name is given, then marker 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createmarker function.)

 Example of Use:
    a=vcs.init()
    a.show('marker')                    # Show all the existing marker secondary methods
    mrk=a.getmarker()                   # mrk instance of 'default' marker secondary
                                        #       method
    mrk2=a.getmarker('quick')           # mrk2 instance of existing 'quick' marker
                                        #       secondary method
    mrk3=a.getmarker(name='red', mtype='dash', size=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of marker object 'red'
    a.marker(mrk3)                      # Plot using specified marker object
"""

        # Check to make sure the argument passed in is a STRING
        if (type(name) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Tm_name = None
        mrk = marker.Tm(self, Tm_name, name, 1)
        if (mtype is not None) and (mrk.name != "default"):
            mrk.type = mtype
        if (size is not None) and (mrk.name != "default"):
            mrk.size = size
        if (color is not None) and (mrk.name != "default"):
            mrk.color = color
        if (priority is not None) and (mrk.name != "default"):
            mrk.priority = priority
        if (viewport is not None) and (mrk.name != "default"):
            mrk.viewport = viewport
        if (worldcoordinate is not None) and (mrk.name != "default"):
            mrk.worldcoordinate = worldcoordinate
        if (x is not None) and (mrk.name != "default"):
            mrk.x = x
        if (y is not None) and (mrk.name != "default"):
            mrk.y = y
        return mrk

    def marker(self, *args, **parms):
        """
 Function: marker                           # Generate a marker plot

 Description of Function:
    Plot a marker segment on the Vcs Canvas. If no marker class
    object is given, then an error will be returned.

 Example of Use:
    a=vcs.init()
    a.show('marker')                     # Show all the existing marker objects
    mrk=a.getmarker('red')               # Create instance of 'red'
    mrk.size=4                           # Set the marker size
    mrk.color = 242                      # Set the marker color
    mrk.type = 4                         # Set the marker type
    mrk.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]] # Set the x value points
    mrk.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]] # Set the y value points
    a.marker(mrk)                          # Plot using specified marker object
"""
        arglist=_determine_arg_list('marker',args)
        return self.__plot(arglist, parms)

    def drawmarker(self, name=None, mtype='solid', size=1, color=241,
                 priority=1, viewport=[0.0,1.0,0.0,1.0],
                 worldcoordinate=[0.0,1.0,0.0,1.0],
                 x=None, y=None,bg=0):
        """
 Function: drawmarker                           # Generate and draw a marker plot

 Description of Function:
    Generate and draw a marker object on the VCS Canvas.

 Example of Use:
    a=vcs.init()
    a.show('marker')                      # Show all the existing marker objects
    mrk=a.drawmarker(name='red', mtype='dash', size=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of marker object 'red'
    a.marker(mrk)                          # Plot using specified marker object
"""
        if (name is None) or (type(name) != types.StringType):
            raise vcsError, 'Must provide string name for the marker.'
        else:
            lo = self.listelements('marker')
            if name in lo:
               mrk = self.getmarker( name )
            else:
               mrk = self.createmarker( name )
        mrk.type = mtype
        mrk.size = size
        mrk.color = color
        mrk.priority = priority
        mrk.viewport = viewport
        mrk.worldcoordinate = worldcoordinate
        mrk.x = x
        mrk.y = y
        self.marker( mrk ,bg=bg)

        return mrk


    #############################################################################
    #                                                                           #
    # Fillarea  functions for VCS.                                              #
    #                                                                           #
    #############################################################################
    def createfillarea(self,name=None, source='default', style=None,
                 index=None, color=None, priority=1,
                 viewport=None, worldcoordinate=None,
                 x=None, y=None):
        """
 Function: createfillarea     # Construct a new fillarea secondary method

 Description of Function:
    Create a new fillarea secondary method given the the name and the existing
    fillarea secondary method to copy the attributes from. If no existing fillarea
    secondary method name is given, then the default fillarea secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    If the name provided already exists, then a error will be returned.
    Secondary method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('fillarea')
    fa=a.createfillarea('example1',)
    a.show('fillarea')
    fa=a.createfillarea('example2','black')
    a.show('fillarea')
    fa2=a.createmarker(name='new', name_src='red',style=1, index=1,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of fill area object 'red'
    a.fillarea(fa2)                      # Plot using specified fill area object
"""

        name,source = self.check_name_source(name,source,'fillarea')

        fa = fillarea.Tf(self, name, source, 0)
        if (style is not None):
            fa.style = style
        if (index is not None):
            fa.index = index
        if (color is not None):
            fa.color = color
        if (priority is not None):
            fa.priority = priority
        if (viewport is not None):
            fa.viewport = viewport
        elif source == 'default':
            fa.viewport = self.viewport
        if (worldcoordinate is not None):
            fa.worldcoordinate = worldcoordinate
        elif source=='default':
            fa.worldcoordinate = self.worldcoordinate
        if (x is not None):
            fa.x = x
        if (y is not None):
            fa.y = y
        return fa


    def getfillarea(self,name='default', style=None,
                 index=None, color=None,
                 priority=None, viewport=None,
                 worldcoordinate=None,
                 x=None, y=None):
        """
 Function: getfillarea              # Construct a new fillarea secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create a
    fillarea class object from an existing VCS fillarea secondary method. If
    no fillarea name is given, then fillarea 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createfillarea function.)

 Example of Use:
    a=vcs.init()
    a.show('fillarea')                 # Show all the existing fillarea secondary methods
    fa=a.getfillarea()                 # fa instance of 'default' fillarea secondary
                                       #       method
    fa2=a.getfillarea('quick')         # fa2 instance of existing 'quick' fillarea
                                        #       secondary method
    fa3=a.createmarker(name='new', name='red',style=1, index=1,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of fill area object 'red'
    a.fillarea(fa3)                      # Plot using specified fill area object
"""

        # Check to make sure the argument passed in is a STRING
        if (type(name) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Tf_name = None
        fa = fillarea.Tf(self, Tf_name, name, 1)
        if (style is not None) and (fa.name != "default"):
            fa.style = style
        if (index is not None) and (fa.name != "default"):
            fa.index = index
        if (color is not None) and (fa.name != "default"):
            fa.color = color
        if (priority is not None) and (fa.name != "default"):
            fa.priority = priority
        if (viewport is not None) and (fa.name != "default"):
            fa.viewport = viewport
        if (worldcoordinate is not None) and (fa.name != "default"):
            fa.worldcoordinate = worldcoordinate
        if (x is not None) and (fa.name != "default"):
            fa.x = x
        if (y is not None) and (fa.name != "default"):
            fa.y = y
        return fa

    def fillarea(self, *args, **parms):
        """
 Function: fillarea                           # Generate a fillarea plot

 Description of Function:
    Plot a fillarea segment on the Vcs Canvas. If no fillarea class
    object is given, then an error will be returned.

 Example of Use:
    a=vcs.init()
    a.show('fillarea')                  # Show all the existing fillarea objects
    fa=a.getfillarea('red')             # Create instance of 'red'
    fa.style=1                          # Set the fillarea style
    fa.index=4                          # Set the fillarea index
    fa.color = 242                      # Set the fillarea color
    fa.type = 4                         # Set the fillarea type
    fa.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]] # Set the x value points
    fa.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]] # Set the y value points
    a.fillarea(fa)                          # Plot using specified fillarea object
"""
        arglist=_determine_arg_list('fillarea',args)
        return self.__plot(arglist, parms)

    def drawfillarea(self, name=None, style=1, index=1, color=241,
                 priority=1, viewport=[0.0,1.0,0.0,1.0],
                 worldcoordinate=[0.0,1.0,0.0,1.0],
                 x=None, y=None,bg=0):
        """
 Function: drawfillarea                           # Generate and draw a fillarea plot

 Description of Function:
    Generate and draw a fillarea object on the VCS Canvas.

 Example of Use:
    a=vcs.init()
    a.show('fillarea')                      # Show all the existing fillarea objects
    fa=a.drawfillarea(name='red', mtype='dash', size=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of fillarea object 'red'
    a.fillarea(fa)                          # Plot using specified fillarea object
"""
        if (name is None) or (type(name) != types.StringType):
            raise vcsError, 'Must provide string name for the fillarea.'
        else:
            lo = self.listelements('fillarea')
            if name in lo:
               fa = self.getfillarea( name )
            else:
               fa = self.createfillarea( name )
        fa.style = style
        fa.index = index
        fa.color = color
        fa.priority = priority
        fa.viewport = viewport
        fa.worldcoordinate = worldcoordinate
        fa.x = x
        fa.y = y
        self.fillarea( fa, bg=bg )

        return fa


    #############################################################################
    #                                                                           #
    # Text Table  functions for VCS.                                            #
    #                                                                           #
    #############################################################################
    def createtexttable(self,name=None, source='default', font=None,
                 spacing=None, expansion=None, color=None, priority=1,
                 viewport=None, worldcoordinate=None,
                 x=None, y=None):
        """
 Function: createtexttable            # Construct a new texttable secondary method

 Description of Function:
    Create a new texttable secondary method given the the name and the existing
    texttable secondary method to copy the attributes from. If no existing texttable
    secondary method name is given, then the default texttable secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    If the name provided already exists, then a error will be returned.
    Secondary method names must be unique.

 Example of Use:
    a=vcs.init()
    a.show('texttable')
    tt=a.createtexttable('example1',)
    a.show('texttable')
    tt=a.createtexttable('example2','black')
    a.show('texttable')
    tt=a.createtexttable(name='new',name_src='red',font=1,spacing=1,expansion=1,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of texttable object 'new'
    a.texttable(tt)                      # Plot using specified texttable object
"""

        name,source = self.check_name_source(name,source,'texttable')

        tt = texttable.Tt(self, name, source, 0)
        try:
           if (font is not None):
              tt.font = font
           if (spacing is not None):
              tt.spacing = spacing
           if (expansion is not None):
              tt.expansion = expansion
           if (color is not None):
              tt.color = color
           if (priority is not None):
              tt.priority = priority
           if (self.viewport is None):
              if (viewport is not None):
                 tt.viewport = viewport
           else:
              tt.viewport = self.viewport
           if (self.worldcoordinate is None):
              if (worldcoordinate is not None):
                 tt.worldcoordinate = worldcoordinate
           else:
              tt.worldcoordinate = self.worldcoordinate
           if (x is not None):
              tt.x = x
           if (y is not None):
              tt.y = y
           return tt
        except:
           pass

    def gettexttable(self,name='default', font=None,
                 spacing=None, expansion=None, color=None,
                 priority=None, viewport=None,
                 worldcoordinate=None,
                 x=None, y=None):
        """
 Function: gettexttable           # Construct a new texttable secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create a
    texttable class object from an existing VCS texttable secondary method. If
    no texttable name is given, then texttable 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createtexttable function.)

 Example of Use:
    a=vcs.init()
    a.show('texttable')              # Show all the existing texttable secondary methods
    tt=a.gettexttable()              # tt instance of 'default' texttable secondary
                                     #       method
    tt2=a.gettexttable('quick')      # tt2 instance of existing 'quick' texttable
                                     #       secondary method
    tt3=a.gettexttable(name='red', font=1, spacing=1,expansion=1,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of texttable object 'red'
    a.texttable(tt3)                      # Plot using specified texttable object
"""

        # Check to make sure the argument passed in is a STRING
        if (type(name) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        Tt_name = None
        return texttable.Tt(self, Tt_name, name, 1 )
##         try:
##            tt = texttable.Tt(self, Tt_name, name, 1)
##            if (font is not None) and (tt.name != "default"):
##               tt.font = font
##            if (spacing is not None) and (tt.name != "default"):
##               tt.spacing = spacing
##            if (expansion is not None) and (tt.name != "default"):
##               tt.expansion = expansion
##            if (color is not None) and (tt.name != "default"):
##               tt.color = color
##            if (priority is not None) and (tt.name != "default"):
##               tt.priority = priority
##            if (self.viewport is None):
##               if (viewport is not None) and (tt.name != "default"):
##                  tt.viewport = viewport
##            else:
##               tt.viewport = self.viewport
##            if (self.worldcoordinate is None):
##               if (worldcoordinate is not None) and (tt.name != "default"):
##                  tt.worldcoordinate = worldcoordinate
##            else:
##               tt.worldcoordinate = self.worldcoordinate
##            if (x is not None) and (tt.name != "default"):
##               tt.x = x
##            if (y is not None) and (tt.name != "default"):
##               tt.y = y
##            return tt
##         except:
##            pass

    #############################################################################
    #                                                                           #
    # Text Orientation  functions for VCS.                                      #
    #                                                                           #
    #############################################################################
    def createtextorientation(self,name=None, source='default'):
        """
 Function: createtextorientation   # Construct a new textorientation secondary method

 Description of Function:
    Create a new textorientation secondary method given the the name and 
    the existing textorientation secondary method to copy the attributes
    from. If no existing textorientation secondary method name is given,
    then the default textorientation secondary method will be used as the
    secondary method to which the attributes will be copied from.

    If the name provided already exists, then a error will be returned.
    Secondary method names must be unique.
    
 Example of Use:
    a=vcs.init()
    a.show('textorientation')
    to=a.createtextorientation('example1',)
    a.show('textorientation')
    to=a.createtextorientation('example2','black')
    a.show('textorientation')
"""

        name,source = self.check_name_source(name,source,'textorientation')

        return textorientation.To(self, name, source, 0)

    def gettextorientation(self,To_name_src='default'):
        """
 Function: gettextorientation       # Construct a new textorientation secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create
    a textorientation class object from an existing VCS textorientation
    secondary method. If no textorientation name is given, then 
    textorientation 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createtextorientation function.)

 Example of Use:
    a=vcs.init()
    a.show('textorientation')    # Show all the existing textorientation secondary methods
    to=a.gettextorientation()    # to instance of 'default' textorientation secondary
                                 #       method
    to2=a.gettextorientation('quick')  # to2 instance of existing 'quick' textorientation
                                       #       secondary method
"""

        # Check to make sure the argument passed in is a STRING
        if (type(To_name_src) != types.StringType):
           raise vcsError, 'The argument must be a string.'

        To_name = None
        return textorientation.To(self, To_name, To_name_src, 1)

    #############################################################################
    #                                                                           #
    # Text Combined  functions for VCS.                                         #
    #                                                                           #
    #############################################################################
    def createtextcombined(self,Tt_name=None, Tt_source='default', To_name=None, To_source='default', font=None, spacing=None, expansion=None, color=None, priority=1, viewport=None, worldcoordinate=None, x=None, y=None, height=None, angle=None, path=None, halign=None, valign=None, projection=None):
        """
 Function: createtext or createtextcombined  # Construct a new text combined secondary method

 Description of Function:
    Create a new textcombined secondary method given the the names and 
    the existing texttable and textorientation secondary methods to copy
    the attributes from. If no existing texttable and textorientation
    secondary method names are given, then the default texttable and
    textorientation secondary methods will be used as the secondary method
    to which the attributes will be copied from.

    If the name provided already exists, then a error will be returned.
    Secondary method names must be unique.
    
 Example of Use:
    a=vcs.init()
    a.show('texttable')
    a.show('textorientation')
    tc=a.createtextcombined('example1','std','example1','7left')
    a.show('texttable')
    a.show('textorientation')
"""

        ## Check if to is defined
        if To_name is None:
            To_name=Tt_name
        Tt_name,Tt_source = self.check_name_source(Tt_name,Tt_source,'texttable')
        To_name,To_source = self.check_name_source(To_name,To_source,'textorientation')

        tc = textcombined.Tc(self, Tt_name, Tt_source, To_name, To_source, 0)
        if (font is not None):
            tc.font = font
        if (spacing is not None):
            tc.spacing = spacing
        if (expansion is not None):
            tc.expansion = expansion
        if (color is not None):
            tc.color = color
        if (priority is not None):
            tc.priority = priority
        if (viewport is not None):
            tc.viewport = viewport
##         elif Tt_source=='default' and To_source=='default':
##             tc.viewport = self.viewport
        else:
            tc.viewport = self.viewport
        if (worldcoordinate is not None):
            tc.worldcoordinate = worldcoordinate
        elif Tt_source=='default' and To_source=='default':
            tc.worldcoordinate = self.worldcoordinate
        if (x is not None):
            tc.x = x
        if (y is not None):
            tc.y = y
        if (height is not None):
            tc.height = height
        if (angle is not None):
            tc.angle = angle
        if (path is not None):
            tc.path = path
        if (halign is not None):
            tc.halign = halign
        if (valign is not None):
            tc.valign = valign
        if (projection is not None):
            tc.projection = projection
        return tc

    #
    # Set alias for the secondary createtextcombined.
    createtext = createtextcombined

    def gettextcombined(self,Tt_name_src='default', To_name_src='default', string=None, font=None, spacing=None, expansion=None, color=None, priority=None, viewport=None, worldcoordinate=None , x=None, y=None, height=None, angle=None, path=None, halign=None, valign=None):
        """
 Function: gettext or gettextcombined   # Construct a new textcombined secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create
    a textcombined class object from an existing VCS texttable secondary
    method and an existing VCS textorientation secondary method. If no 
    texttable or textorientation names are given, then the 'default' names
    will be used in both cases.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createtextcombined function.)

Example of Use:
    a=vcs.init()
    a.show('texttable')                  # Show all the existing texttable secondary methods
    a.show('textorientation')            # Show all the existing textorientation secondary methods
    tc=a.gettextcombined()               # Use 'default' for texttable and textorientation
    tc2=a.gettextcombined('std','7left') # Use 'std' texttable and '7left' textorientation
    if istextcombined(tc):               # Check to see if tc is a textcombined
       tc.list()                         # Print out all its attriubtes
"""


        # Check to make sure the arguments passed in are a STRINGS
        if (type(Tt_name_src) != types.StringType):
            raise vcsError, 'The first argument must be a string.'
        if (type(To_name_src) != types.StringType):
            raise vcsError, 'The second argument must be a string.'
        
        Tt_name = None
        To_name = None
        tc = textcombined.Tc(self,Tt_name,Tt_name_src,To_name,To_name_src,1)
        if (string is not None) and (tc.Tt_name != "default"):
            tc.string = string
        if (font is not None) and (tc.Tt_name != "default"):
            tc.font = font
        if (spacing is not None) and (tc.Tt_name != "default"):
            tc.spacing = spacing
        if (expansion is not None) and (tc.Tt_name != "default"):
            tc.expansion = expansion
        if (color is not None) and (tc.Tt_name != "default"):
            tc.color = color
        if (priority is not None) and (tc.Tt_name != "default"):
            tc.priority = priority
        if (viewport is not None) and (tc.Tt_name != "default"):
            tc.viewport = viewport
        if (worldcoordinate is not None) and (tc.Tt_name != "default"):
            tc.worldcoordinate = worldcoordinate
        if (x is not None) and (tc.To_name != "default"):
            tc.x = x
        if (y is not None) and (tc.To_name != "default"):
            tc.y = y
        if (height is not None) and (tc.To_name != "default"):
            tc.height = height
        if (angle is not None) and (tc.To_name != "default"):
            tc.angle = angle
        if (path is not None) and (tc.To_name != "default"):
            tc.path = path
        if (halign is not None) and (tc.To_name != "default"):
            tc.halign = halign
        if (valign is not None) and (tc.To_name != "default"):
            tc.valign = valign
        return tc

    #
    # Set alias for the secondary gettextcombined.
    gettext = gettextcombined

    def textcombined(self, *args, **parms):
        """
        Function: text or textcombined         # Generate a textcombined plot
        
        Description of Function:
        Plot a textcombined segment on the Vcs Canvas. If no textcombined class
        object is given, then an error will be returned.
        
        Example of Use:
        a=vcs.init()
        a.show('texttable')                 # Show all the existing texttable objects
        a.show('textorientation')           # Show all the existing textorientation objects
        tt=a.gettext('std','7left')         # Create instance of 'std' and '7left'
        tt.string = 'Text1'                 # Show the string "Text1" on the VCS Canvas
        tt.font=2                           # Set the text size
        tt.color = 242                      # Set the text color
        tt.angle = 45                       # Set the text angle
        tt.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]] # Set the x value points
        tt.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]] # Set the y value points
        a.text(tt)                          # Plot using specified text object

        Optionally you can pass a string, the coordinates and any keyword
        Example:
        x.plot('Hi',.5,.5,color=241,angle=45)
        """
        ## First check if color is a string
        if 'color' in parms.keys():
            if type(parms['color'])==type(''):
                parms['color']=self.match_color(parms['color'])
                
        if not isinstance(args[0],vcs.textcombined.Tc):
            args=list(args)
            ## Ok we have a user passed text object let's first create a random text combined
##             icont=1
##             while icont:
##                 n=random.randint(0,100000)
##                 try:
##                     t=self.createtextcombined('__'+str(n),'default','__'+str(n),'default')
##                     icont=0
##                 except:
##                     pass
            t=self.createtextcombined()
            t.string=[args.pop(0)]
            t.x=[args.pop(0)]
            t.y=[args.pop(0)]
            #t.list()
            for k in parms.keys():
                setattr(t,k,parms[k])
                del(parms[k])
            args.insert(0,t)
        arglist=_determine_arg_list('text',args)
        return self.__plot(arglist, parms)
    #
    # Set alias for the secondary textcombined.
    text = textcombined

    def gettextextent(self,textobject):
        """Returns the coordinate of the box surrounding a text object once printed
        Example:
        x=vcs.init()
        t=x.createtext()
        t.x=[.5]
        t.y=[.5]
        t.string=['Hello World']
        extent = x.gettextextent(t)
        print extent
        """
        if not vcs.istext(textobject):
            raise vcsError,'You must pass a text object'
        To = textobject.To_name
        Tt = textobject.Tt_name
        return apply(self.canvas.gettextextent,(Tt,To))


    def match_color(self,color,colormap=None):
        """
Function: cmatch_color                          # Returns the color in the colormap that is closet from the required color
Description of Function:
           Given a color (defined as rgb values -0/100 range- or a string name) and optionally a colormap name,
           returns the color number that is closet from the requested color
           (using rms difference between rgb values)
           if colormap is not map use the currently used colormap
Example of use:
           a=vcs.init()
           print a.match_color('salmon')
           print a.match_color('red')
           print a.match_color([0,0,100],'defaullt') # closest color from blue

"""
        # First gets the rgb values 
        if type(color)==type(''):
            vals=genutil.colors.str2rgb(color)
            vals[0]/=2.55
            vals[1]/=2.55
            vals[2]/=2.55
        else:
            vals=color

        # Now gets the colormap to look in
        if colormap is None: colormap=self.getcolormapname()
        cmap=self.getcolormap(colormap)

        # Now tries determines the min rms diff
        rmsmin=2.E40
        match=None
        for i in cmap.index.keys():
            col=cmap.index[i]
            rms=numpy.sqrt((vals[0]-col[0])**2+\
                             (vals[1]-col[1])**2+\
                             (vals[2]-col[2])**2 \
                             )
            if rms<rmsmin:
                rmsmin=rms
                match=i
        return match
        
    def drawtextcombined(self, Tt_name=None, To_name=None, string=None,
                 font=1, spacing=2, expansion=100, color=241,
                 height = 14, angle=0, path='right', halign = 'left',
                 valign = 'half',
                 priority=1, viewport=[0.0,1.0,0.0,1.0],
                 worldcoordinate=[0.0,1.0,0.0,1.0],
                 x=None, y=None,bg=0):
        """
 Function: drawtexttable                           # Generate and draw a texttable plot

 Description of Function:
    Generate and draw a texttable object on the VCS Canvas.

 Example of Use:
    a=vcs.init()
    a.show('texttable')                      # Show all the existing texttable objects
    tt=a.drawtexttable(Tt_name = 'red', To_name='7left', mtype='dash', size=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )      # Create instance of texttable object 'red'
    a.texttable(tt)                          # Plot using specified texttable object
"""
        if (Tt_name is None) or (type(Tt_name) != types.StringType):
            raise vcsError, 'Must provide string name for the texttable.'
        else:
            lot = self.listelements('texttable')
            if Tt_name not in lot:
               tt = self.createtexttable( Tt_name )
            loo = self.listelements('textorientation')
            if To_name not in loo:
               to = self.createtextorientation( To_name )
            t=self.gettextcombined( Tt_name, To_name )

        # Set the Text Table (Tt) members
        t.string = string

        # Set the Text Table (Tt) members
        t.font = font
        t.spacing = spacing
        t.expansion = expansion
        t.color = color
        t.priority = priority
        t.viewport = viewport
        t.worldcoordinate = worldcoordinate
        t.x = x
        t.y = y

        # Set the Text Orientation (To) members
        t.height = height
        t.angle = angle
        t.path = path
        t.halign = halign
        t.valign = valign

        self.text( t ,bg=bg)

        return t
    #
    # Set alias for the secondary drawtextcombined.
    drawtext = drawtextcombined


    _plot_keywords_ = ['variable','grid','xaxis','yaxis','xrev','yrev','continents','xarray','yarray',
                       'name','time','units','ymd','file_comment',
                       'xbounds','ybounds','xname','yname','xunits','yunits','xweights','yweights',
                       'comment1','comment2','comment3','comment4','hms','long_name','zaxis',
                       'zarray','zname','zunits','taxis','tarray','tname','tunits','waxis','warray',
                       'wname','wunits','bg','ratio']



    def replot(self):
        """ Clears and plots with last used plot arguments
        """
        self.clear()
        self.plot(*self.__last_plot_actual_args, **self.__last_plot_keyargs)

    ###########################################################################
    #                                                                         #
    # Plot wrapper for VCS.                                                   #
    #                                                                         #
    ###########################################################################
    def plot(self, *actual_args, **keyargs):
        """
Options:::
%s
%s
%s
%s
%s
    :::
 Output:::
%s
    :::

 Function: plot

 Description of plot:
    Plot an array(s) of data given a template and graphics method. The VCS template is
    used to define where the data and variable attributes will be displayed on the VCS 
    Canvas. The VCS graphics method is used to define how the array(s) will be shown
    on the VCS Canvas.

 The form of the call is:
    plot(array1=None, array2=None, template_name=None, graphics_method=None,
            graphics_name=None, [key=value [, key=value [, ...]]])
    
            where array1 and array2 are NumPy arrays.

    Plot keywords:
      ratio [default is none]
            None: let the self.ratio attribute decide
            0,'off': overwritte self.ratio and do nothing about the ratio
            'auto': computes an automatic ratio
            '3',3: y dim will be 3 times bigger than x dim (restricted to original tempalte.data area
            Adding a 't' at the end of the ratio, makes the tickmarks and boxes move along.
            
    Variable attribute keys:
       comment1         = string   #Comment plotted above file_comment
       comment2         = string   #Comment plotted above comment1
       comment3         = string   #Comment plotted above comment2
       comment4         = string   #Comment plotted above comment4
       file_comment     = string   #Comment (defaults to file.comment)
       hms              = string (hh:mm:ss) #Hour, minute, second
       long_name        = string   #Descriptive variable name
       name             = string   #Variable name (defaults to var.id)
       time             = cdtime   #instance (relative or absolute),
                                    cdtime, reltime or abstime value
       units            = string   #Variable units
       ymd              = string (yy/mm/dd) #Year, month, day
    
    Dimension attribute keys (dimension length=n):
       [x|y|z|t|w]array = NumPy array of length n    # x or y Dimension values
       [x|y|z|t|w]array = NumPy array of length n    # x or y Dimension values
       [x|y]bounds       = NumPy array of shape (n,2) # x or y Dimension boundaries
       [x|y|z|t|w]name   = string                     # x or y Dimension name
       [x|y|z|t|w]units  = string                     # x or y Dimension units
       [x|y]weights      = NumPy array of length n    # x or y Dimension weights (used to
                                                        calculate area-weighted mean)
    
    CDMS object:
       [x|y|z|t|w]axis   = CDMS axis object           # x or y Axis
       grid              = CDMS grid object           # Grid object (e.g. grid=var.getGrid()
       variable          = CDMS variable object       # Variable object
    
    Other:
       [x|y]rev         = 0|1                         # if ==1, reverse the direction of the x
    							     or y axis
       continents	= 0,1,2,3,4,5,6,7,8,9,10,11   #	if >=1, plot continental outlines
    							     (default: plot if xaxis is
    							     longitude, yaxis is latitude -or-
    							     xname is 'longitude' and yname is
    							     'latitude'
                                                      # The continents-type values are integers
						      # ranging from 0 to 11, where: 
						      #    0 signifies "No Continents" 
						      #    1 signifies "Fine Continents" 
						      #    2 signifies "Coarse Continents" 
						      #    3 signifies "United States"
						      #    4 signifies "Political Borders"
						      #    5 signifies "Rivers"

						      # Values 6 through 11 signify the line type
                                                      # defined by the files data_continent_other7
                                                      # through data_continent_other12. 

    Graphics Output in Background Mode:
       bg                 = 0|1   # if ==1, create images in the background
                                                             (Don't display the VCS Canvas)
    
 Note:
    More specific attributes take precedence over general attributes. In particular,
    specifie attributes override variable object attributes, dimension attributes and
    arrays override axis objects, which override grid objects, which override variable
    objects.
    
    For example, if both 'file_comment' and 'variable' keywords are specified, the value of
    'file_comment' is used instead of the file comment in the parent of variable. Similarly,
    if both 'xaxis' and 'grid' keywords are specified, the value of 'xaxis' takes precedence
    over the x-axis of grid.

 Example of Use:
    x=vcs.init()        # x is an instance of the VCS class object (constructor)
    x.plot(array)       # this call will use default settings for template and boxfill
    x.plot(array, 'AMIP', 'isofill','AMIP_psl') # this is specifying the template and 
                                                  graphics method
    t=x.gettemplate('AMIP')        # get a predefined the template 'AMIP'
    vec=x.getvector('quick')       # get a predefined the vector graphics method 'quick'
    x.plot(array1, array2, t, vec) # plot the data as a vector using the 'AMIP' template
    x.clear()                      # clear the VCS Canvas of all plots
    box=x.createboxfill('new')     # create boxfill graphics method 'new'
    x.plot(box,t,array)            # plot array data using box 'new' and template 't'

###############################################################################################################
###########################################                      ##############################################
########################################## End plot Description ###############################################
#########################################                      ################################################
###############################################################################################################

"""
        self.__last_plot_actual_args = actual_args
        self.__last_plot_keyargs = keyargs

        finish_queued_X_server_requests( self )
        passed_var = keyargs.get("variable",None)
        arglist = _determine_arg_list ( None, actual_args )
        if passed_var is not None:
            arglist[0] = cdms2.asVariable(passed_var)
        self.canvas.BLOCK_X_SERVER()

        # Prevent the varglist from duplicating its contents if the GUI Canvas is in use  
        try:  
            sal = keyargs['sal']
            if (keyargs['sal'] == 0): del keyargs['sal']
        except:
            sal = 1

        try:
            if (self.canvas_gui.top_parent.menu.vcs_canvas_gui_settings_flg == 1): # Must be from VCDAT
               self.canvas_gui.dialog.dialog.configure( title = ("Visualization and Control System (VCS) GUI"))
        except: 
            # Connect the VCS Canvas to the GUI
            if (self.canvas_gui is not None) and (sal == 1):
               #####################################################################################################
               # Charles and Dean - This command will only allow one plot on a page for the VCS Canvas GUI.        #
               # It is committed out so that there can be two or more plots on a page. Must keep a watch to see    #
               # what other problems occur without this command. See vcsmodule.c: PyVCS_connect_gui_and_canvas.    #
               #                                                                                                   #
               # self._connect_gui_and_canvas( self.winfo_id )                                                     #
               #####################################################################################################
               self.canvas_gui.dialog.dialog.configure( title = ("%i. Visualization and Control System (VCS)" % self.canvasid()))

        # Plot the data
        a = self.__plot( arglist, keyargs )

        # Continuation to remove arglist from duplicating its contents
        if (sal == 0): arglist = []

        for x in arglist: self.varglist.append( x ) # save the plot argument list
        self.canvas.UNBLOCK_X_SERVER()

        if self.canvas_gui is not None:
            self.canvas_gui.dialog.dialog.deiconify()
            # This command makes sure that the VCS Canvas Gui is in front of the VCDAT window.
#            self.canvas_gui.dialog.dialog.transient( self.canvas_gui.top_parent )
            self.canvas_gui.show_data_plot_info( self.canvas_gui.parent, self )
        return a
    plot.__doc__ = plot.__doc__ % (plot_2_1D_options, plot_keywords_doc,graphics_method_core,axesconvert,plot_2_1D_input, plot_output)

    def _create_random_template(self,name):
##         icont=1
##         while icont:
##             try:
##                 n=random.randint(0,100000)                                                    
##                 tmpl=self.createtemplate('__'+str(n),name)
##                 icont=0
##             except:
##                 if not name in self.listelements('template'):
##                     print "'Template' is currently set to P_default."
##                     name='default'
        tmpl = self.createtemplate(None,name)
        return tmpl
    
    def plot_filledcontinents(self,slab,template_name,g_type,g_name,bg,ratio):
        cf=cdutil.continent_fill.Gcf()
        if g_type.lower()=='boxfill':
            g=self.getboxfill(g_name)
        lons=slab.getLongitude()
        lats=slab.getLatitude()

        if lons is None or lats is None:
            return
        if g.datawc_x1>9.9E19:
            cf.datawc_x1=lons[0]
        else:
            cf.datawc_x1=g.datawc_x1
        if g.datawc_x2>9.9E19:
            cf.datawc_x2=lons[-1]
        else:
            cf.datawc_x2=g.datawc_x2
        if g.datawc_y1>9.9E19:
            cf.datawc_y1=lats[0]
        else:
            cf.datawc_y1=g.datawc_y1
        if g.datawc_y2>9.9E19:
            cf.datawc_y2=lats[-1]
        else:
            cf.datawc_y2=g.datawc_y2
        try:
            t=self.gettemplate(template_name)
            cf.plot(x=self,template=template_name,ratio=ratio)
        except Exception,err:
            print err

    def get_gm(self,type,name):
        if type=='isoline':
            func=self.getisoline
        elif type=='isofill':
            func=self.getisofill
        elif type=='boxfill':
            func=self.getboxfill
        elif type=='meshfill':
            func=self.getmeshfill
        elif type=='scatter':
            func=self.getscatter
        elif type=='xvsy':
            func=self.getxvsy
        elif type=='xyvsy':
            func=self.getxyvsy
        elif type=='yxvsx':
            func=self.getyxvsx
        elif type=='vector':
            func=self.getvector
        elif type=='outfill':
            func=self.getoutfill
        elif type=='outline':
            func=self.getoutline
        elif type=='taylordiagram':
            func=self.gettaylordiagram
        elif isinstance(type,vcsaddons.core.VCSaddon):
            func = type.getgm
        else:
            return None
        gm=func(name)
        return gm
                
    def generate_gm(self,type,name):
        if type=='isoline':
            func=self.createisoline
        elif type=='isofill':
            func=self.createisofill
        elif type in ['boxfill','default']:
            func=self.createboxfill
        elif type=='meshfill':
            func=self.createmeshfill
        elif type=='scatter':
            func=self.createscatter
        elif type=='xvsy':
            func=self.createxvsy
        elif type=='xyvsy':
            func=self.createxyvsy
        elif type=='yxvsx':
            func=self.createyxvsx
        elif type=='vector':
            func=self.createvector
        elif type=='outfill':
            func=self.createoutfill
        elif type=='outline':
            func=self.createoutline
        elif type=='taylordiagram':
            func=self.createtaylordiagram
        elif isinstance(type,vcsaddons.core.VCSaddon):
            func = type.creategm
        else:
            return None
        copy_mthd=func(source = name)
        return copy_mthd

    def __plot (self, arglist, keyargs):
        # This routine has five arguments in arglist from _determine_arg_list
        # It adds one for bg and passes those on to Canvas.plot as its sixth arguments.

        ## First of all try some cleanup
        assert len(arglist) == 6
        xtrakw=arglist.pop(5)
        for k in xtrakw.keys():
            if k in keyargs.keys():
                raise vcsError,'Multiple Definition for '+str(k)
            else:
                keyargs[k]=xtrakw[k]
        assert arglist[0] is None or cdms2.isVariable (arglist[0])
        assert arglist[1] is None or cdms2.isVariable (arglist[1])
        assert type(arglist[2]) == types.StringType
        if not isinstance(arglist[3],vcsaddons.core.VCSaddon): assert type(arglist[3]) == types.StringType
        assert type(arglist[4]) == types.StringType

        ##reset animation
        self.animate.create_flg = 0

        # Store the origin template. The template used to plot may be changed below by the 
        # _create_random_template function, which copies templates for modifications.
        template_origin = arglist[2]
        tmptmpl = self.gettemplate(arglist[2])
        tmptmpl.data._ratio=-999
        
        copy_mthd=None
        copy_tmpl=None
        if arglist[2] in ['default','default_dud']:
            if arglist[3]=='taylordiagram':
                copy_tmpl=self.createtemplate(source='deftaylor')
            else:
                copy_tmpl=self.createtemplate(source=arglist[2])

        # By defalut do the ratio thing for lat/lon and linear projection
        # but it can be overwritten by keyword
        doratio=str(keyargs.get('ratio',self.ratio)).strip().lower()
        if doratio[-1]=='t' and doratio[0]=='0' :
            if float(doratio[:-1])==0.: doratio='0'
        
        ## Check for curvilinear grids, and wrap options !
        if arglist[0] is not None and arglist[1] is None and arglist[3]=="meshfill":
            g=arglist[0].getGrid()
            if isinstance(g, (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid)):
                g=self.getmeshfill(arglist[4])
                if not 'wrap' in keyargs.keys() and g.wrap==[0.,0.]:
                    keyargs['wrap']=[0.,360.]
            else:
                if arglist[0].rank<2:
                    arglist[3]='yxvsx'
                    arglist[4]='default'
                else:
                    xs=arglist[0].getAxis(-1)
                    ys=arglist[0].getAxis(-2)
                    if xs.isLongitude() and ys.isLatitude() and isinstance(g,cdms2.grid.TransientRectGrid):
                        arglist[1]=MV2.array(g.getMesh())
                        if not 'wrap' in keyargs.keys():
                            keyargs['wrap']=[0.,360.]
                    elif ys.isLongitude() and xs.isLatitude() and isinstance(g,cdms2.grid.TransientRectGrid):
                        arglist[1]=MV2.array(g.getMesh())
                        if not 'wrap' in keyargs.keys():
                            keyargs['wrap']=[360.,0.]
                    else:
                        arglist[3]='boxfill'
                        copy_mthd=self.generate_gm('boxfill','default')
                        m=self.getmeshfill(arglist[4])
                        md=self.getmeshfill()
                        if md.levels!=m.levels:
                            copy_mthd.boxfill_type='custom'
                            copy_mthd.levels=m.levels
                            copy_mthd.fillareacolors=m.fillareacolors
                        for att in ['projection',
                                    'xticlabels1',
                                    'xticlabels2',
                                    'xmtics1',
                                    'xmtics2',
                                    'yticlabels1',
                                    'yticlabels2',
                                    'ymtics1',
                                    'ymtics2',
                                    'datawc_x1',
                                    'datawc_x2',
                                    'datawc_y1',
                                    'datawc_y2',
                                    'xaxisconvert',
                                    'yaxisconvert',
                                    'legend',
                                    'ext_1',
                                    'ext_2',
                                    'missing']:
                            setattr(copy_mthd,att,getattr(m,att))
        elif arglist[0] is not None and arglist[0].rank()<2 and arglist[3] in ['boxfill','default'] and not isinstance(arglist[0].getGrid(),cdms2.gengrid.AbstractGenericGrid):
            arglist[3]='yxvsx'
            try:
                tmp=self.getyxvsx(arglist[4])
                #tmp.list()
            except Exception,err:
                arglist[4]='default'
            
##                         arglist[4]=copy_mthd.name
                        
        # preprocessing for extra keyword (at-plotting-time options)
        cmds={}
        # First of all a little preprocessing for legend !
        if 'legend' in keyargs.keys() and arglist[3]=='boxfill':
            # we now have a possible problem since it can be legend for the graphic method or the template!
            k=keyargs['legend']
            isboxfilllegend=0
            if type(k)==type({}):
#                print k.keys()
                # ok it's a dictionary if the key type is string then it's for template, else it's for boxfill
                if type(k.keys()[0])!=type(''):
                    # not a string, therefore it's boxfill !
                    isboxfilllegend=1
            elif type(k) in [type([]), type(())]:
                # ok it's a list therefore if the length is not 4 we have a boxfill legend
                if len(k)!=4 and len(k)!=5:
                    isboxfilllegend=1
                elif len(k)==5:
                    if not type(k[4]) in [type({}),type(0),type(0.)]:
                        raise vcsError, "Error, at-plotting-time argument 'legend' is ambiguous in this context\nCannot determine if it is template or boxfill keyword,\n tips to solve that:\n\tif you aim at boxfill keyword, pass legend as a dictionary, \n\tif your aim at template, add {'priority':1} at the end of the list\nCurrently legend is passed as:"+repr(k)
                    elif type(k[4])!=type({}):
                        isboxfilllegend=1
                else:
                    # ok it's length 4, now the only hope left is that not all values are between 0 and 1
                    for i in range(4):
                        if k[i]>1. or k[i]<0. : isboxfilllegend=1
                    if isboxfilllegend==0: raise vcsError, "Error, at-plotting-time argument 'legend' is ambiguous in this context\nCannot determine if it is template or boxfill keyword,\n tips to solve that:\n\tif you aim at boxfill keyword, pass legend as a dictionary, \n\tif your aim at template, add {'priority':1} at the end of the list\nCurrently legend is passed as:"+repr(k)

            # ok it is for the boxfill let's do it        
            if isboxfilllegend:
                if copy_mthd is None:
                    copy_mthd=self.generate_gm(arglist[3],arglist[4])
                copy_mthd.legend=k
                del(keyargs['legend'])
            

        # There is no way of knowing if the template has been called prior to this plot command. 
        # So it is done here to make sure that the template coordinates are normalized. If already
        # normalized, then no change will to the template.
        try: self.gettemplate( template_origin )
        except:
            pass

        ## Creates dictionary/list to remember what we changed
        slab_changed_attributes={}
        slab_created_attributes=[]
        axes_changed={}
        axes_changed2={}

        
        for p in keyargs.keys(): # loops through possible keywords for graphic method
            if p in [
                'projection',
                'xticlabels1',
                'xticlabels2',
                'xmtics1',
                'xmtics2',
                'yticlabels1',
                'yticlabels2',
                'ymtics1',
                'ymtics2',
                'datawc_x1',
                'datawc_y1',
                'datawc_x2',
                'datawc_y2',
                'xaxisconvert',
                'yaxisconvert',
                'label',
                'line',
                'linewidth',
                'linecolors',
                'text',
                'textcolors',
                'level',
                'level_1',
                'level_2',
                'ext_1',
                'ext_2',
                'missing',
                'color_1',
                'color_2',
                'fillareastyle',
                'fillareacolors',
                'fillareaindices',
                'levels',
                'mesh',
                'wrap',
                'marker',
                'markercolor',
                'markersize',
                'linecolor',
                'outline',
                'outfill',
                'detail',
                'max',
                'quadrans',
                'skillValues',
                'skillColor',
                'skillCoefficient',
                'referencevalue',
                'arrowlength',
                'arrowangle',
                'arrowbase',
                'scale',
                'alignement',
                'type',
                'reference',
                # Now the "special" keywords
                'worldcoordinate',
                ]:
                if copy_mthd is None: copy_mthd=self.generate_gm(arglist[3],arglist[4])
                if copy_mthd is None: raise vcsError, 'Error, at-plotting-time option: '+p+' is not available for graphic method type:'+arglist[3]
                if not p in ['worldcoordinate',]: # not a special keywords
                    setattr(copy_mthd,p,keyargs[p])
                elif p=='worldcoordinate':
                    setattr(copy_mthd,'datawc_x1',keyargs[p][0])
                    setattr(copy_mthd,'datawc_x2',keyargs[p][1])
                    setattr(copy_mthd,'datawc_y1',keyargs[p][2])
                    setattr(copy_mthd,'datawc_y2',keyargs[p][3])
                del(keyargs[p])
            # Now template settings keywords
            elif p in [
                'viewport',
                ]:
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])
                copy_tmpl.reset('x',keyargs[p][0],keyargs[p][1],copy_tmpl.data.x1,copy_tmpl.data.x2)
                copy_tmpl.reset('y',keyargs[p][2],keyargs[p][3],copy_tmpl.data.y1,copy_tmpl.data.y2)
                del(keyargs[p])
            # Now template and x/y related stuff (1 dir only)
            elif p[1:] in [
                'label1',
                'label2',
                ]:
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])
                k=keyargs[p]
                if type(k)!=type([]):# not a list means only priority set
                    if not type(k)==type({}):
                        setattr(getattr(copy_tmpl,p),'priority',k)
                    elif type(k)==type({}):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl,p),kk,k[kk])                        
                else:
                    if p[0]=='x':
                        setattr(getattr(copy_tmpl,p),'y',k[0])
                    else:
                        setattr(getattr(copy_tmpl,p),'x',k[0])
                    if type(k[-1])==type({}):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl,p),kk,k[-1][kk])
                        
                del(keyargs[p])
            # Now template and x1 and x2/y1 and y2 related stuff (1 dir only)
            elif p[1:] in [
                'mintic1',
                'mintic2',
                'tic1',
                'tic2',
                ]:
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])

                k=keyargs[p]
                if type(k)!=type([]):# not a list means only priority set
                    if not type(k)==type({}):
                        setattr(getattr(copy_tmpl,p),'priority',k)
                    elif type(k)==type({}):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl,p),kk,k[kk])                        
                else:
                    if p[0]=='x':
                        setattr(getattr(copy_tmpl,p),'y1',k[0])
                        setattr(getattr(copy_tmpl,p),'y2',k[1])
                    else:
                        setattr(getattr(copy_tmpl,p),'x1',k[0])
                        setattr(getattr(copy_tmpl,p),'x2',k[1])
                    if type(k[-1])==type({}):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl,p),kk,k[-1][kk])
                        
                del(keyargs[p])
            # Now template with x1, x2, x3, x4, x5
            elif p in [
                'box1','box2','box3','box4',
                'line1','line2','line3','line4',
                'data','legend',
                ]:
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])
                k=keyargs[p]
                if type(k)!=type([]):# not a list means only priority set
                    if not type(k)==type({}):
                        setattr(getattr(copy_tmpl,p),'priority',k)
                    elif type(k)==type({}):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl,p),kk,k[kk])                        
                else:
                    setattr(getattr(copy_tmpl,p),'x1',k[0])
                    setattr(getattr(copy_tmpl,p),'x2',k[1])
                    setattr(getattr(copy_tmpl,p),'y1',k[2])
                    setattr(getattr(copy_tmpl,p),'y2',k[3])
                    if type(k[-1])==type({}):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl,p),kk,k[-1][kk])
                        
                del(keyargs[p])
            # Now MV2 related keywords
            ## Charles note: It's here that we need to remember what changed so i can unset it later
            elif p in [
                'title',
                'comment1',
                'comment2',
                'comment3',
                'comment4',
                'source',
                'crdate',
                'crtime',
                'dataname',
                'file',
                'function',
                'transformation',
                'units',
                'id',
               ]:
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])
                k=keyargs[p]
                if getattr(getattr(copy_tmpl,p),'priority')==0:
                    setattr(getattr(copy_tmpl,p),'priority',1)
                if not isinstance(k,list):# not a list means only priority set
                    if isinstance(k,dict):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl,p),kk,k[kk])                        
                    elif isinstance(k,int): 
                        setattr(getattr(copy_tmpl,p),'priority',k)
                    elif isinstance(k,str):
                        slab_changed_attributes[p]=k
##                         if hasattr(arglist[0],p):
##                             slab_changed_attributes[p]=getattr(arglist[0],p)
##                         else:
##                             slab_created_attributes.append(p)
##                         setattr(arglist[0],p,k)
                else:
##                     if hasattr(arglist[0],p):
##                         slab_changed_attributes[p]=getattr(arglist[0],p)
##                     else:
##                         slab_created_attributes.append(p)
##                     setattr(arglist[0],p,k[0])
                    slab_changed_attributes[p]=k[0]
                    setattr(getattr(copy_tmpl,p),'x',k[1])
                    setattr(getattr(copy_tmpl,p),'y',k[2])
                    if type(k[-1])==type({}):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl,p),kk,k[-1][kk])
                        
                del(keyargs[p])
            # Now Axis related keywords
            elif p[1:] in [
                'name',
                'value',
                'units',
               ]:
                if p[0]=='x':
                    ax=arglist[0].getAxis(-1)
                    if ax is not None:
                        ax=ax.clone()
                    if keyargs.has_key('xaxis'):
                        ax=keyargs['xaxis'].clone()
                        keyargs['xaxis']=ax
                    g=arglist[0].getGrid()
                    if isinstance(g, (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid)) or arglist[3].lower()=='meshfill':
                        ax=None
                        del(g)
                elif p[0]=='y':
                    ax=arglist[0].getAxis(-2)
                    if ax is not None:
                        ax=ax.clone()
                    if keyargs.has_key('yaxis'):
                        ax=keyargs['yaxis'].clone()
                        keyargs['yaxis']=ax
                    g=arglist[0].getGrid()
                    if isinstance(g, (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid)) or arglist[3].lower()=='meshfill':
                        ax=None
                        del(g)
                elif p[0]=='z':
                    ax=arglist[0].getLevel()
                    if ax is not None:
                        ax=ax.clone()
                elif p[0]=='t':
                    ax=arglist[0].getTime()
                    if ax is not None:
                        ax=ax.clone()
                if not ax is None:
                    ids=arglist[0].getAxisIds()
                    for i in range(len(ids)):
                        if ax.id==ids[i]:
                            if not axes_changed.has_key(i):
                                axes_changed[i]=ax
                    if arglist[1] is not None:
                        ids2=arglist[1].getAxisIds()
                        for i in range(len(ids2)):
                            if ax.id==ids2[i]:
                                if not axes_changed2.has_key(i):
                                    axes_changed2[i]=ax
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])
                k=keyargs[p]
                if getattr(getattr(copy_tmpl,p),'priority')==0:
                    setattr(getattr(copy_tmpl,p),'priority',1)
                if type(k)!=type([]):# not a list means only priority set
                    if type(k)==type({}):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl,p),kk,k[kk])                        
                    elif type(k)==type(0):
                        setattr(getattr(copy_tmpl,p),'priority',k)
                    elif isinstance(k,str):
                        if p[1:]!='name':
                            setattr(ax,p[1:],k)
                        else:
                            try:
                                setattr(ax,'id',k)
                            except Exception,err:
##                                 print err
                                pass
                    elif k is None:
                        if p[1:]!='name':
                            setattr(ax,p[1:],'')
                        else:
                            setattr(ax,'id','')
                       
                else:
                    if p[1:]!='name':
                        setattr(ax,p[1:],k[0])
                    else:
                        setattr(ax,'id',k)
                    setattr(getattr(copy_tmpl,p),'x',k[1])
                    setattr(getattr(copy_tmpl,p),'y',k[2])
                    if type(k[-1])==type({}):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl,p),kk,k[-1][kk])
                        
                del(keyargs[p])
            # Finally take care of commands
            elif p in [
                'pdf','ps','postscript','gif','ras',
               ]:
                cmds[p]=keyargs[p]
                del(keyargs[p])


        ## Check if datawc has time setting in it
        wasnone=0
        if copy_mthd is None:
            if arglist[3]!='default':
                copy_mthd=self.generate_gm(arglist[3],arglist[4])
            else:
                copy_mthd=self.generate_gm('boxfill',arglist[4])
            wasnone=1
##                and (type(copy_mthd.datawc_x1) in [type(cdtime.comptime(1900)),type(cdtime.reltime(0,'days since 1900'))] or \
##                type(copy_mthd.datawc_x2) in [type(cdtime.comptime(1900)),type(cdtime.reltime(0,'days since 1900'))]) \


        if (hasattr(copy_mthd,'datawc_x1') and hasattr(copy_mthd,'datawc_x2')) \
               and arglist[0].getAxis(-1).isTime() \
               and copy_mthd.xticlabels1=='*' \
               and copy_mthd.xticlabels2=='*' \
               and copy_mthd.xmtics1 in ['*',''] \
               and copy_mthd.xmtics2 in ['*',''] \
               and not (copy_mthd.g_name in ['GXy','GXY'] and arglist[0].rank()==1) :
            ax=arglist[0].getAxis(-1).clone()
            ids=arglist[0].getAxisIds()
            for i in range(len(ids)):
                if ax.id==ids[i]:
                    if not axes_changed.has_key(i):
                        ax=ax.clone()
                        axes_changed[i]=ax
                    break
            if arglist[1] is not None:
                ids2=arglist[1].getAxisIds()
                for i in range(len(ids2)):
                    if ax.id==ids2[i]:
                        if not axes_changed2.has_key(i):
                            axes_changed2[i]=ax
            try:
                ax.toRelativeTime(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                convertedok = True
            except:
                convertedok = False
            if (copy_mthd.xticlabels1=='*' or copy_mthd.xticlabels2=='*') and convertedok and copy_mthd.g_name not in ["GSp",]:
                if wasnone:
                    wasnone=0
                    copy_mthd=self.generate_gm(arglist[3],arglist[4])
                convert_datawc = False
                for cax in axes_changed.keys():
                    if axes_changed[cax] == ax:
                        convert_datawc = True
                        break
                if convert_datawc:
                    oax = arglist[0].getAxis(cax).clone()
                    t=type(copy_mthd.datawc_x1)
                    if not t in [type(cdtime.reltime(0,'months since 1900')),type(cdtime.comptime(1900))]:
                        if copy_mthd.datawc_x1>9.E19:
                            copy_mthd.datawc_x1 = cdtime.reltime(oax[0],oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                        else:
                            copy_mthd.datawc_x1 = cdtime.reltime(copy_mthd.datawc_x1,oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                        if copy_mthd.datawc_x2>9.E19:
                            copy_mthd.datawc_x2 = cdtime.reltime(oax[-1],oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                        else:
                            copy_mthd.datawc_x2 = cdtime.reltime(copy_mthd.datawc_x2,oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                if copy_mthd.xticlabels1=='*' : copy_mthd.xticlabels1=vcs.generate_time_labels(copy_mthd.datawc_x1,copy_mthd.datawc_x2,copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                if copy_mthd.xticlabels2=='*' : copy_mthd.xticlabels2=vcs.generate_time_labels(copy_mthd.datawc_x1,copy_mthd.datawc_x2,copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
        elif not (getattr(copy_mthd,'g_name','')=='Gfm' and isinstance(arglist[0].getGrid(), (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid))):
            try:
                if arglist[0].getAxis(-1).isTime():
                    if (copy_mthd.xticlabels1=='*' and copy_mthd.xticlabels2=='*' and copy_mthd.g_name != 'GXy') \
                       and copy_mthd.g_name not in ['GSp']:
                        if wasnone:
                            wasnone=0
                            copy_mthd=self.generate_gm(arglist[3],arglist[4])
                        t=arglist[0].getAxis(-1).clone()
                        timeunits=t.units
                        calendar=t.getCalendar()
                        t0=cdtime.reltime(t[0],timeunits)
                        t1=cdtime.reltime(t[-1],timeunits)
                        copy_mthd.xticlabels1=vcs.generate_time_labels(t0,t1,timeunits,calendar)
            except:
                pass

        if (hasattr(copy_mthd,'datawc_y1') and hasattr(copy_mthd,'datawc_y2'))\
               and copy_mthd.yticlabels1=='*' \
               and copy_mthd.yticlabels2=='*' \
               and copy_mthd.ymtics1 in ['*',''] \
               and copy_mthd.ymtics2 in ['*',''] \
               and arglist[0].getAxis(-2).isTime() and (arglist[0].rank()>1 or copy_mthd.g_name in ['GXy',]) \
               and not (copy_mthd.g_name=='Gfm' and isinstance(arglist[0].getGrid(), (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid))):
            ax=arglist[0].getAxis(-2).clone()
            if copy_mthd.g_name in ["GSp",]:
                ax = arglist[1].getAxis(-2).clone()
                axes_changed2={}
            ids=arglist[0].getAxisIds()
            for i in range(len(ids)):
                if ax.id==ids[i]:
                    if not axes_changed.has_key(i):
                        ax=ax.clone()
                        axes_changed[i]=ax
##                     else:
##                         ax=axes_changed[i]
                    break
            if arglist[1] is not None:
                ids2=arglist[1].getAxisIds()
                for i in range(len(ids2)):
                    if ax.id==ids2[i]:
                        if not axes_changed2.has_key(i):
                            axes_changed2[i]=ax
##                         else:
##                             ax=axes_changed2[i]
                        break
            try:
                ax.toRelativeTime(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                convertedok = True
            except:
                convertedok = False
            if (copy_mthd.yticlabels1=='*' or copy_mthd.yticlabels2=='*') and convertedok:
                if wasnone:
                    wasnone=0
                    copy_mthd=self.generate_gm(arglist[3],arglist[4])
                convert_datawc = False
                A=axes_changed
                if copy_mthd.g_name in ["GSp",]:
                    A=axes_changed2
                for cax in A.keys():
                    if A[cax] is ax:
                        convert_datawc = True
                        break
                if convert_datawc:
                    oax = arglist[0].getAxis(cax).clone()
                    if copy_mthd.datawc_y1>9.E19:
                        copy_mthd.datawc_y1 = cdtime.reltime(oax[0],oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                    else:
                        copy_mthd.datawc_y1 = cdtime.reltime(copy_mthd.datawc_y1,oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                    if copy_mthd.datawc_y2>9.E19:
                        copy_mthd.datawc_y2 = cdtime.reltime(oax[-1],oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                    else:
                        copy_mthd.datawc_y2 = cdtime.reltime(copy_mthd.datawc_y2,oax.units).tocomp(oax.getCalendar()).torel(copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                if copy_mthd.yticlabels1=='*' :
                    copy_mthd.yticlabels1=vcs.generate_time_labels(copy_mthd.datawc_y1,copy_mthd.datawc_y2,copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
                if copy_mthd.yticlabels2=='*' : copy_mthd.yticlabels2=vcs.generate_time_labels(copy_mthd.datawc_y1,copy_mthd.datawc_y2,copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar)
        elif not (getattr(copy_mthd,'g_name','')=='Gfm' and isinstance(arglist[0].getGrid(), (cdms2.gengrid.AbstractGenericGrid,cdms2.hgrid.AbstractCurveGrid))):
            try:
                if arglist[0].getAxis(-2).isTime() and arglist[0].rank()>1 and copy_mthd.g_name not in ['GYx','GXy','GXY','GSp']:
                    if copy_mthd.yticlabels1=='*' and copy_mthd.yticlabels2=='*':
                        if wasnone:
                            wasnone=0
                            copy_mthd=self.generate_gm(arglist[3],arglist[4])
##                         print copy_mthd.datawc_y1,copy_mthd.datawc_y2,copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar
                        t=arglist[0].getAxis(-2).clone()
                        timeunits=t.units
                        calendar=t.getCalendar()
                        t0=cdtime.reltime(t[0],timeunits)
                        t1=cdtime.reltime(t[-1],timeunits)
                        copy_mthd.yticlabels1=vcs.generate_time_labels(t0,t1,timeunits,calendar)
            except:
                pass

        def clean_val(value):
            if numpy.allclose(value,0.):
                return 0.
            elif value<0:
                sign=-1
                value=-value
            else:
                sign=1
            i=int(numpy.log10(value))
            if i>0:
                j=i
                k=10.
            else:
                j=i-1
                k=10.
            v=int(value/numpy.power(k,j))*numpy.power(k,j)
            return v*sign
        
        def mkdic(method,values):
            if method=='area_wt':
                func=numpy.sin
                func2=numpy.arcsin
            elif method=='exp':
                func=numpy.exp
                func2=numpy.log
            elif method=='ln':
                func=numpy.log
                func2=numpy.exp
            elif method=='log10':
                func=numpy.log10
            vals=[]
            for v in values:
                if method=='area_wt':
                    vals.append(func(v*numpy.pi/180.))
                else:
                    vals.append(func(v))                    
            min,max=vcs.minmax(vals)
            levs=vcs.mkscale(min,max)
##             levs=vcs.mkevenlevels(min,max)
            vals=[]
            for l in levs:
                if method=='log10':
                    v=numpy.power(10,l)
                elif method=='area_wt':
                    v=func2(l)/numpy.pi*180.
                else:
                    v=func2(l)
                vals.append(clean_val(v))
            dic=vcs.mklabels(vals)
            dic2={}
            for k in dic.keys():
                try:
                    if method=='area_wt':
                        dic2[func(k*numpy.pi/180.)]=dic[k]
                    else:
                        dic2[func(k)]=dic[k]
                except:
                    pass
            return dic2
        
        def set_convert_labels(copy_mthd,test=0):
            did_something = False
            for axc in ['x','y']:
                try:
                    mthd=getattr(copy_mthd,axc+'axisconvert')
                    if mthd!='linear':
                        for num in ['1','2']:
                            if getattr(copy_mthd,axc+'ticlabels'+num)=='*':
                                if axc=='x':
                                    axn=-1
                                else:
                                    axn=-2
                                dic=mkdic(mthd,arglist[0].getAxis(axn)[:])
                                if test==0 : setattr(copy_mthd,axc+'ticlabels'+num,dic)
                                did_something = True
                except:
                    pass
            return did_something

        if copy_mthd is None:
            copy_mthd=self.get_gm(arglist[3],arglist[4])
            if set_convert_labels(copy_mthd,test=1):
                copy_mthd=self.generate_gm(arglist[3],arglist[4])
                set_convert_labels(copy_mthd)
            else:
                copy_mthd=None
                
        else:
            set_convert_labels(copy_mthd)

        if copy_mthd is None:
            copy_mthd=self.generate_gm(arglist[3],arglist[4])
        x=None
        y=None
        try:
            if arglist[0].getAxis(-1).isLongitude():
                x="longitude"
            elif arglist[0].getAxis(-1).isLatitude():
                x="latitude"
            if copy_mthd.g_name in ["GXy","GXY"]:
                datawc_x1=MV2.minimum(arglist[0])
                datawc_x2=MV2.maximum(arglist[0])
                x=None
            else:            
              try:
                if arglist[0].getAxis(-1).isCircularAxis():
                  datawc_x1=arglist[0].getAxis(-1)[0]
                else:
                  datawc_x1=arglist[0].getAxis(-1).getBounds()[0][0]
              except:
                datawc_x1=arglist[0].getAxis(-1)[0]
              try:
                if arglist[0].getAxis(-1).isCircularAxis():
                  datawc_x2=arglist[0].getAxis(-1)[-1]
                else:
                  datawc_x2=arglist[0].getAxis(-1).getBounds()[-1][1]
              except:
                datawc_x2=arglist[0].getAxis(-1)[-1]
            if arglist[0].getAxis(-2).isLongitude():
                y="longitude"
            elif arglist[0].getAxis(-2).isLatitude():
                y="latitude"
            
            if copy_mthd.g_name in ["GYx",]:
                datawc_y1=MV2.minimum(arglist[0])
                datawc_y2=MV2.maximum(arglist[0])
                y=None
            elif copy_mthd.g_name in ["GYX",]:
                datawc_y1=MV2.minimum(arglist[1])
                datawc_y2=MV2.maximum(arglist[1])
                y=None
            else:
                try:
                  datawc_y1=arglist[0].getAxis(-2).getBounds()[0][0]
                except:
                  datawc_y1=arglist[0].getAxis(-2)[0]
                try:
                  datawc_y2=arglist[0].getAxis(-2).getBounds()[-1][1]
                except:
                  datawc_y2=arglist[0].getAxis(-2)[-1]
        except:
            pass
        try:
            dic = self.setTicksandLabels(copy_mthd,datawc_x1,datawc_x2,datawc_y1,datawc_y2,x=x,y=y)
        except:
            pass

        if not copy_mthd is None: arglist[4]=copy_mthd.name
        if not copy_tmpl is None: arglist[2]=copy_tmpl.name

        ## End of preprocessing !
        
        # get the background value
        bg = keyargs.get('bg', 0)

        # line added by Charles Doutriaux to plugin the taylordiagram and bypass the C code for graphic methods
        hold_cont_type = self.canvas.getcontinentstype()
        if isinstance(arglist[3],str) and arglist[3].lower()=='taylordiagram':
            for p in slab_changed_attributes.keys():
                if hasattr(arglist[0],p):
                    tmp = getattr(arglist[0],p)
                else:
                    tmp = (None,None)
                setattr(arglist[0],p,slab_changed_attributes[p])
                slab_changed_attributes[p]=tmp
            # first look at the extra arguments and make sure there is no duplicate
            for k in keyargs.keys():
                if not k in ['template','skill','bg']:
                    del(keyargs[k])                    
                if k=='template':
                    arglist[2]=keyargs[k]
                    del(keyargs[k])
            # look through the available taylordiagram methods and use the plot function
            for t in vcs.taylordiagrams:
                if t.name==arglist[4]:
                    t.plot(arglist[0],canvas=self,template=arglist[2],**keyargs)
                    result = self.getplot(self.return_display_names()[-1], arglist[2])
                    result.g_type='taylordiagram'
                    result.g_name=arglist[4]
                    result.extradisplays=t.displays
##                     dn.array=arglist[0]
                    for p in slab_changed_attributes.keys():
                        tmp = slab_changed_attributes[p]
                        if tmp == (None,None):
                            delattr(arglist[0],p)
                        else:
                            setattr(arglist[0],p,tmp)
                            
                    return result
##                     return self.getplot(dn, template_origin)
            raise vcsError, 'Error taylordiagram method: '+arglist[4]+' not found'
        else:
            for keyarg in keyargs.keys():
                if not keyarg in self.__class__._plot_keywords_:
                     raise vcsError, 'Invalid keyword: %s'%keyarg

            if (arglist[0] is not None or keyargs.has_key('variable')):
                arglist[0] = self._reconstruct_tv(arglist, keyargs)
                # Check data's dimension size (VCS cannot take variables with
                # with dimensions larger than 4D, below makes sure the variable
                # has no more than 4 dimensions.
                while (len(arglist[0].shape) > 4):
                   # Scale the first dimension down to size 1, then squeeze it out.
                   arglist[0]= (MV2.take(arglist[0],(0,),0)).subRegion( squeeze=1)
                ## Now applies the attributes change
                for p in slab_changed_attributes.keys():
                    if hasattr(arglist[0],p):
                        tmp = getattr(arglist[0],p)
                    else:
                        tmp=(None,None)
                    setattr(arglist[0],p,slab_changed_attributes[p])
                    slab_changed_attributes[p]=tmp
                ## Now applies the axes changes
                for i in axes_changed.keys():
                    arglist[0].setAxis(i,axes_changed[i])
                for i in axes_changed2.keys():
                    arglist[1].setAxis(i,axes_changed2[i])
            # Check to make sure that you have at least 2 dimensions for the follow graphics methods
##             if (len(arglist[0].shape) < 2) and (arglist[3] in ['boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'vector', 'scatter']):
            ## Flipping the order to avoid the tv not exist problem
            if (arglist[3] in ['boxfill', 'isofill', 'isoline', 'outfill', 'outline', 'vector']) and (len(arglist[0].shape) < 2 ):
                raise vcsError, 'Invalid number of dimensions for %s' % arglist[3]


            ## Ok now does the linear projection for lat/lon ratio stuff
            if arglist[3] in ['marker','line','fillarea','text']:
                # fist create a dummy template
                t=self.createtemplate()
                # Now creates a copy of the primitives, in case it's used on other canvases with diferent ratios
                if arglist[3]=='text':
                    nms = arglist[4].split(":::")
                    p = self.createtext(Tt_source=nms[0],To_source=nms[1])
                elif arglist[3]=='marker':
                    p = self.createmarker(source=arglist[4])
                elif arglist[3]=='line':
                    p = self.createline(source=arglist[4])
                elif arglist[3]=='fillarea':
                    p = self.createfillarea(source=arglist[4])

                t.data.x1 = p.viewport[0]
                t.data.x2 = p.viewport[1]
                t.data.y1 = p.viewport[2]
                t.data.y2 = p.viewport[3]
                
                proj = self.getprojection(p.projection)

                if proj.type=='linear' and doratio[:4]=='auto':
                    lon1,lon2,lat2,lat2 = p.worldcoordinate
                    t.ratio_linear_projection(lon1,lon2,lat1,lat2,None,box_and_ticks=box_and_ticks)
                    p.viewport = [t.data.x1,t.data.x2,t.data.y1,t.data.y2]
                    arglist[4] = p.name
                elif not doratio in ['0','off','none','auto','autot']:
                    if doratio[-1]=='t':
                        doratio=doratio[:-1]
                    Ratio=float(doratio)
                    t.ratio(Ratio)
                    p.viewport = [t.data.x1,t.data.x2,t.data.y1,t.data.y2]
                    if arglist[3]=='text':
                        arglist[4] = p.Tt_name+':::'+p.To_name
                    else:
                        arglist[4]=p.name
            elif (arglist[3] in ['boxfill','isofill','isoline','outfill','outline','vector','meshfill'] or isinstance(arglist[3],vcsaddons.core.VCSaddon)) and doratio in ['auto','autot'] and not (doratio=='auto' and arglist[2]=='ASD'):
                box_and_ticks=0
                if doratio[-1]=='t' or template_origin=='default':
                    box_and_ticks=1

                if arglist[3]=='isoline':
                    func=self.getisoline
                elif arglist[3]=='isofill':
                    func=self.getisofill
                elif arglist[3]=='boxfill':
                    func=self.getboxfill
                elif arglist[3]=='meshfill':
                    func=self.getmeshfill
                elif arglist[3]=='vector':
                    func=self.getvector
                elif arglist[3]=='outfill':
                    func=self.getoutfill
                elif arglist[3]=='outline':
                    func=self.getoutline
                if isinstance(arglist[3],vcsaddons.core.VCSaddon):
                    gm= arglist[3]
                else:
                    gm=func(arglist[4])
                p=self.getprojection(gm.projection)
                if p.type == 'linear':
                    if gm.g_name =='Gfm':
                        if self.isplottinggridded:
                            lon1,lon2=vcs.minmax(arglist[1][...,:,1,:])
                            lat1,lat2=vcs.minmax(arglist[1][...,:,0,:])
                            if lon2-lon1>360:
                                lon1,lon2=0.,360.
                            if gm.datawc_x1<9.99E19:
                                lon1=gm.datawc_x1
                            if gm.datawc_x2<9.99E19:
                                lon2=gm.datawc_x2
                            if gm.datawc_y1<9.99E19:
                                lat1=gm.datawc_y1
                            if gm.datawc_y2<9.99E19:
                                lat2=gm.datawc_y2
                            if copy_tmpl is None:
                                copy_tmpl=self._create_random_template(arglist[2])
                                arglist[2]=copy_tmpl.name
                            copy_tmpl.ratio_linear_projection(lon1,lon2,lat1,lat2,None,box_and_ticks=box_and_ticks)
                    elif arglist[0].getAxis(-1).isLongitude() and arglist[0].getAxis(-2).isLatitude():
                        if copy_tmpl is None:
                            copy_tmpl=self._create_random_template(arglist[2])
                        if gm.datawc_x1<9.99E19:
                            lon1=gm.datawc_x1
                        else:
                            lon1=min(arglist[0].getAxis(-1))
                        if gm.datawc_x2<9.99E19:
                            lon2=gm.datawc_x2
                        else:
                            lon2=max(arglist[0].getAxis(-1))
                        if gm.datawc_y1<9.99E19:
                            lat1=gm.datawc_y1
                        else:
                           lat1=min(arglist[0].getAxis(-2))
                        if gm.datawc_y2<9.99E19:
                            lat2=gm.datawc_y2
                        else:
                            lat2=max(arglist[0].getAxis(-2))
                        copy_tmpl.ratio_linear_projection(lon1,lon2,lat1,lat2,None,box_and_ticks=box_and_ticks)
                        arglist[2]=copy_tmpl.name
            elif not (doratio in ['0','off','none','auto','autot']) or  (arglist[3] in ['boxfill','isofill','isoline','outfill','outline','vector','meshfill'] and str(doratio).lower() in ['auto','autot']) and arglist[2]!='ASD' :
                box_and_ticks=0
                if doratio[-1]=='t' or template_origin=='default':
                    box_and_ticks=1
                    if doratio[-1]=='t':
                        doratio=doratio[:-1]
                try:
                    Ratio=float(doratio)
                except:
                    Ratio=doratio
                if copy_tmpl is None:
                    copy_tmpl=self._create_random_template(arglist[2])
                    arglist[2]=copy_tmpl.name
                copy_tmpl.ratio(Ratio,box_and_ticks=box_and_ticks)
                            
                            
            if hasattr(self,'_isplottinggridded') : del(self._isplottinggridded)
            # Get the continents for animation generation
            self.animate.continents_value = self.canvas.getcontinentstype()

            # Get the option for doing graphics in the background.
            if bg:
                arglist.append('bg')
            else:
                arglist.append('fg')
            if arglist[3]=='scatter':
                if not (numpy.equal(arglist[0].getAxis(-1)[:],arglist[1].getAxis(-1)[:]).all()):
                    raise vcsError, 'Error - ScatterPlot requires X and Y defined in the same place'
            if arglist[3]=='vector':
                if not (numpy.equal(arglist[0].getAxis(-1)[:],arglist[1].getAxis(-1)[:]).all()) or not(numpy.equal(arglist[0].getAxis(-2)[:],arglist[1].getAxis(-2)[:]).all()):
                    raise vcsError, 'Error - VECTOR components must be on the same grid.'
            if isinstance(arglist[3],vcsaddons.core.VCSaddon):
                if arglist[1] is None:
                    dn = arglist[3].plot(arglist[0],template=arglist[2],bg=bg,x=self)
                else:
                    dn = arglist[3].plot(arglist[0],arglist[1],template=arglist[2],bg=bg,x=self)
            else:
                dn = apply(self.canvas.plot, tuple(arglist))
            if self.mode!=0 : self.update()
            #if not bg: pause(self.pause_time)

            # Restore the continents type
##             print 'HOLD CONTINENTS:',hold_cont_type,self.canvas.getcontinentstype()
##             self.plot_filledcontinents(arglist[0],arglist[2],arglist[3],arglist[4],bg,doratio)


        if isinstance(arglist[3],str):
            result = self.getplot(dn, template_origin)
            #self.canvas.setcontinentstype(hold_cont_type)
            # Pointer to the plotted slab of data and the VCS Canas display infomation. 
            # This is needed to find the animation min and max values and the number of 
            # displays on the VCS Canvas.
            self.animate_info.append( (result, arglist[:2]) )
            self.animate.update_animate_display_list( )
        else:
            result = dn
            

        # Make sure xmainloop is started. This is needed to check for X events
        # (such as, Canvas Exposer, button or key press and release, etc.)
        if ( (self.canvas.THREADED() == 0) and (bg == 0) ):
            thread.start_new_thread( self.canvas.startxmainloop, ( ) )

        # Now executes output commands
        for cc in cmds.keys():
            c=string.lower(cc)
            if type(cmds[cc])!=type(''):
                args=tuple(cmds[cc])
            else:
                args=(cmds[cc],)
            if c=='ps' or c=='postscript':
                apply(self.postscript,args)
            elif c=='pdf':
                apply(self.pdf,args)
            elif c=='gif':
                apply(self.gif,args)
            elif c=='eps':
                apply(self.eps,args)
            elif c=='cgm':
                apply(self.cgm,args)
            elif c=='ras':
                apply(self.ras,args)
        
        #self.clean_auto_generated_objects("template")
        for p in slab_changed_attributes.keys():
            tmp = slab_changed_attributes[p]
            if tmp == (None,None):
                delattr(arglist[0],p)
            else:
                setattr(arglist[0],p,tmp)
        return result

    #############################################################################
    #                                                                           #
    # VCS utility wrapper to return the number of displays that are "ON".       #
    #                                                                           #
    #############################################################################
    def return_display_ON_num(self, *args):
        return apply(self.canvas.return_display_ON_num, args)

    #############################################################################
    #                                                                           #
    # VCS utility wrapper to return the current display names.                  #
    #                                                                           #
    #############################################################################
    def return_display_names(self, *args):
        return apply(self.canvas.return_display_names, args)

    #############################################################################
    #                                                                           #
    # VCS utility wrapper to remove the display names.                          #
    #                                                                           #
    #############################################################################
    def remove_display_name(self, *args):
        return apply(self.canvas.remove_display_name, args)

    #############################################################################
    #                                                                           #
    # CGM  wrapper for VCS.                                                     #
    #                                                                           #
    #############################################################################
    def cgm(self, file,mode='r'):
        """
 Function: cgm

 Description of Function:
    To save a graphics plot in CDAT the user can call CGM along with the name of
    the output. This routine will save the displayed image on the VCS canvas as
    a binary vector graphics that can be imported into MSWord or Framemaker. CGM
    files are in ISO standards output format.

    The CGM command is used to create or append to a cgm file. There are two modes
    for saving a cgm file: `Append' mode (a) appends cgm output to an existing cgm
    file; `Replace' (r) mode overwrites an existing cgm file with new cgm output.
    The default mode is to overwrite an existing cgm file (i.e. mode (r)).
 
 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.cgm(o)
    a.cgm('example')           # by default a cgm file will overwrite an existing file
    a.cgm('example','a')  # 'a' will instruct cgm to append to an existing file
    a.cgm('example','r')  # 'r' will instruct cgm to overwrite an existing file
    a.cgm('example',mode='r')  # 'r' will instruct cgm to overwrite an existing file

"""
        return apply(self.canvas.cgm, (file,mode))

    #############################################################################
    #                                                                           #
    # Clear VCS Canvas wrapper for VCS.                                         #
    #                                                                           #
    #############################################################################
    def clear(self, *args, **kargs):
        """
 Function: clear

 Description of Function:
    In VCS it is necessary to clear all the plots from a page. This routine
    will clear all the VCS displays on a page (i.e., the VCS Canvas object).
 
 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.clear()

"""
        self.animate.close()
        self.animate_info=[]
        self.animate.update_animate_display_list( )

        return apply(self.canvas.clear, args)

    #############################################################################
    #                                                                           #
    # Close VCS Canvas wrapper for VCS.                                         #
    #                                                                           #
    #############################################################################
    def close(self, *args, **kargs):
        """
 Function: close

 Description of Function:
    Close the VCS Canvas. It will not deallocate the VCS Canvas object.
    To deallocate the VCS Canvas, use the destroy method.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.close()

"""
        global gui_canvas_closed

        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        #   Hide the GUI
        if (self.canvas_gui is not None):
           self.canvas_gui.dialog.dialog.withdraw() # just withdraw the GUI for later
           gui_canvas_closed = 0

        # Close the VCS Canvas
        a = apply( self.canvas.close, args )

        # Stop the (thread) execution of the X main loop (if it is running).
        self.canvas.stopxmainloop( )
        self.canvas.UNBLOCK_X_SERVER()

        return a

    #############################################################################
    #                                                                           #
    # Destroy VCS Canvas Object (i.e., call the Dealloc C code).      		#
    #                                                                           #
    #############################################################################
    def destroy(self):
        """
 Function: destroy

 Description of Function:
    Destroy the VCS Canvas. It will deallocate the VCS Canvas object.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.destory()

"""
        import gc

        # Now stop the X main loop and its thread. Also close the VCS Canvas if it
	# is visible.
#        self.canvas.stopxmainloop( )
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        del self
        gc.garbage
        gc.collect()
# This is in contruction...... I have not yet completed this. This function
# generates a segmentation fault.
#        try:
#           apply(self.canvas.destroy)
#        except:
#           pass

    

    #############################################################################
    #                                                                           #
    # Colormap Graphical User Interface wrapper for VCS.                        #
    #                                                                           #
    #############################################################################
    def colormapgui(self, gui_parent=None, transient=0, max_intensity=None):
        '''
 Function: colormapgui

 Description of Function:
    Run the VCS colormap interface.

    The colormapgui command is used to bring up the VCS colormap interface. The interface
    is used to select, create, change, or remove colormaps.

 Example of Use:
    a=vcs.init()
    a.colormapgui()
    a.colormapgui(max_intensity = 255)
'''
        
        import warnings
        warnings.warn("The colormap gui has been removed from CDAT, you can access it via the UV-CDAT GUI.", Warning)
        return
##         _colormapgui.create(self, gui_parent=gui_parent, transient=transient, max_intensity=max_intensity)

    #############################################################################
    #                                                                           #
    # Projection Editor, Graphic User Interface wrapper for VCS.                #
    #                                                                           #
    #############################################################################
    def projectiongui(self, gui_parent=None, projection='default'):
        '''
 Function: projectiongui

 Description of Function:
    Run the VCS projection editor interface.

    The projectiongui command is used to bring up the VCS projection interface. The interface
    is used to select, create, change, or remove projections.

 Example of Use:
    a=vcs.init()
    a.projectiongui()
'''
        import warnings
        warnings.warn("The projection gui has been removed from CDAT, you can access it via the UV-CDAT GUI.", Warning)
        return
        ## _projectiongui.create(gui_parent=gui_parent,canvas=self,projection=projection)

    #############################################################################
    #                                                                           #
    # Graphics Method Change display.                                           #
    #                                                                           #
    #############################################################################
    def change_display_graphic_method(self,display,type,name):
        '''
 Function: change_display_graphic_method

 Description of Function:
    Changes the type and graphic metohd of a display.

'''
        return apply(self.canvas.change_display_graphic_method,(display,type,name))
    #############################################################################
    #                                                                           #
    # Figures out which display is selected in graphic method editor mode       #
    #                                                                           #
    #############################################################################
    def get_selected_display(self):
        """
 Function: get_selected_display

 Description of Function:
    In GMEDITOR mode returns selected display.
    If nothing selected returns None
    """
        return apply(self.canvas.get_selected_display,())
    #############################################################################
    #                                                                           #
    # Graphics Method Graphical User Interface wrapper for VCS.                 #
    #                                                                           #
    #############################################################################
    def graphicsmethodgui(self, gm_type='boxfill', gm_name='default',
                          gui_parent=None):
        '''
 Function: graphicsmethodgui

 Description of Function:
    Run the VCS graphicsmethod interface.

    The graphicsmethodgui command is used to bring up the VCS graphics method interface.
    The interface is used to alter existing graphics method attributes.

 Example of Use:
    a=vcs.init()
    a.graphicsmethodgui('boxfill', 'quick')
'''
        import warnings
        warnings.warn("The graphics method gui has been removed from CDAT, you can access it via the UV-CDAT GUI.", Warning)
        return
    ## _graphicsmethodgui.create( self, gm_type=gm_type, gm_name=gm_name,
    ## gui_parent=gui_parent)

    #############################################################################
    #                                                                           #
    # Template Editor Graphical User Interface wrapper for VCS.                 #
    #                                                                           #
    #############################################################################
    def templateeditor(self, template_name='default', template_orig_name='default', plot=None, gui_parent=None, canvas = None, called_from = 0):
        ##from tkMessageBox import showerror
        '''
 Function: templateeditor

 Description of Function:
    Run the VCS templateeditor GUI.

    The templateeditor command is used to bring up the VCS template editor interface.
    The interface is used to alter templates.

 Example of Use:
    a=vcs.init()
    a.templateeditor('AMIP2')
'''
        send_canvas = canvas
        if (canvas == None): send_canvas = self

        if (template_name == 'default'):
           showerror( 'Error Message to User', 'Cannot edit the "default" template. Please copy the "default" template to new name and then edit the newly created template.')
        else:
           if (self.canvas.SCREEN_MODE() == "DATA"):
              self.canvas.SCREEN_TEMPLATE_FLAG()
              t=_gui_template_editor.create(gui_parent=gui_parent, canvas=send_canvas, plot=plot, template_name=template_name, template_orig_name=template_orig_name, called_from = called_from)
              return t
           else:
              showerror( 'Error Message to User', 'VCS will only allow one Template Editor at a time. Please the close previous template editor and try again.')

    #############################################################################
    #                                                                           #
    # Send a request to turn on a picture template object in the VCS Canvas.    #
    #                                                                           #
    #############################################################################
    def _select_one(self, template_name,attr_name,X1,X2,Y1,Y2):
        # flush and block the X main loop
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        self.canvas._select_one(template_name,attr_name,X1,X2,Y1,Y2)

        self.canvas.UNBLOCK_X_SERVER()

    #############################################################################
    #                                                                           #
    # Send a request to turn off a picture template object in the VCS Canvas.   #
    #                                                                           #
    #############################################################################
    def _unselect_one(self, template_name,attr_name,X1,X2,Y1,Y2):
        # flush and block the X main loop
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        self.canvas._unselect_one(template_name,attr_name,X1,X2,Y1,Y2)

        self.canvas.UNBLOCK_X_SERVER()

    #############################################################################
    #                                                                           #
    # Set the template editor event flag to select all template objects on the  #
    # VCS Canvas.                                                               #
    #                                                                           #
    #############################################################################
    def _select_all(self):
        # flush and block the X main loop
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        self.canvas._select_all()

        self.canvas.UNBLOCK_X_SERVER()

    #############################################################################
    #                                                                           #
    # Set the template editor event flag to unselect all the template objects   #
    # on the VCS Canvas.                                                        #
    #                                                                           #
    #############################################################################
    def _unselect_all(self):
        # flush and block the X main loop
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        self.canvas._unselect_all()

        self.canvas.UNBLOCK_X_SERVER()

    #############################################################################
    #                                                                           #
    # Set the template editor mode for the VCS Canvas screen.                   #
    #                                                                           #
    #############################################################################
    def _SCREEN_TEMPLATE_FLAG(self):
         self.canvas.SCREEN_TEMPLATE_FLAG()

    #############################################################################
    #                                                                           #
    # Set the graphic method editor mode for the VCS Canvas screen.                   #
    #                                                                           #
    #############################################################################
    def _SCREEN_GM_FLAG(self):
         self.canvas.SCREEN_GM_FLAG()

    #############################################################################
    #                                                                           #
    # Set the data mode for the VCS Canvas screen.                              #
    #                                                                           #
    #############################################################################
    def _SCREEN_DATA_FLAG(self):
         self.canvas.SCREEN_DATA_FLAG()

    #############################################################################
    #                                                                           #
    # Set the screen check mode to DATA for the VCS Canvas.                     #
    #                                                                           #
    #############################################################################
    def _SCREEN_CHECKMODE_DATA_FLAG(self):
         self.canvas.SCREEN_CHECKMODE_DATA_FLAG()

    #############################################################################
    #                                                                           #
    # Return the Screen mode, either data mode or template editor mode.         #
    #                                                                           #
    #############################################################################
    def SCREEN_MODE(self, *args):
        return apply(self.canvas.SCREEN_MODE, args)

    #############################################################################
    #                                                                           #
    # Return the Screen mode, either data mode or template editor mode.         #
    #                                                                           #
    #############################################################################
    def plot_annotation(self, *args):
        apply(self.canvas.plot_annotation, args)

    def pageeditor(self, gui_parent=None, continents=None):
        '''
 Function: pageeditor

 Description of Function:
    Run the VCS page editor GUI.

    The pageeditor command is used to bring up the VCS page editor interface.
    The interface is used to design canvases.

 Example of Use:
    a=vcs.init()
    a.pageieditor()
'''
        #_pagegui.create(canvas=self, gui_parent=gui_parent)
        return _pagegui.PageDescriptionEditor(canvas=self, gui_parent=gui_parent,
                                              continents=continents)

    #############################################################################
    #                                                                           #
    # Flush X event que wrapper for VCS.                                        #
    #                                                                           #
    #############################################################################
    def flush(self, *args):
        """
 Function: flush

 Description of Function:
    The flush command executes all buffered X events in the que.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.flush()

"""
        return apply(self.canvas.flush, args)

    #############################################################################
    #                                                                           #
    # Return how many events are in the que.                                    #
    #                                                                           #
    #############################################################################
    def xpending(self, *args):
        return apply(self.canvas.xpending, args)

    #############################################################################
    #                                                                           #
    # Block the X server. It may NOT process do X11 commands.                   #
    #                                                                           #
    #############################################################################
    def BLOCK_X_SERVER(self, *args):
        return apply(self.canvas.BLOCK_X_SERVER, args)

    #############################################################################
    #                                                                           #
    # Unblock the X server. It may now proceed to do X11 commands.              #
    #                                                                           #
    #############################################################################
    def UNBLOCK_X_SERVER(self, *args):
        return apply(self.canvas.UNBLOCK_X_SERVER, args)

    #############################################################################
    #                                                                           #
    # Return whether or not it is threaded.                                     #
    #                                                                           #
    #############################################################################
    def THREADED(self, *args):
        return apply(self.canvas.THREADED, args)

    #############################################################################
    #                                                                           #
    # Geometry wrapper for VCS.                                                 #
    #                                                                           #
    #############################################################################
    def geometry(self, *args):
        """
 Function: geometry

 Description of Function:
    The geometry command is used to set the size and position of the VCS canvas.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.geometry(450,337)

"""
        if (args[0] <= 0) or (args[1] <= 0):
           raise ValueError, 'Error -  The width and height values must be an integer greater than 0.'

        a=apply(self.canvas.geometry, args)
        self.flush() # update the canvas by processing all the X events

        return a

    #############################################################################
    #                                                                           #
    # VCS Canvas Information wrapper.                                           #
    #                                                                           #
    #############################################################################
    def canvasinfo(self, *args):
        """
 Function: canvasinfo

 Description of Function:
    Obtain the current attributes of the VCS Canvas window.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.canvasinfo()

"""
        return apply(self.canvas.canvasinfo, args)

    #############################################################################
    #                                                                           #
    # Get continents type wrapper for VCS.                                      #
    #                                                                           #
    #############################################################################
    def getcontinentstype(self, *args):
        """
 Function: getcontinentstype

 Description of Function:
    Retrieve continents type from VCS. Remember the value can only be between
    0 and 11.

 Example of Use:
     a=vcs.init()
     cont_type = a.getcontinentstype() # Get the continents type
"""
        return apply(self.canvas.getcontinentstype, args)


    ###########################################################################
    #                                                                         #
    # Postscript to GIF wrapper for VCS.                                      #
    #                                                                         #
    ###########################################################################
    def pstogif(self, filename, *opt):
        """
  Function: pstogif
 
  Description of Function:
     In some cases, the user may want to save the plot out as a gif image. This
     routine allows the user to convert a postscript file to a gif file.
 
  Example of Use:
     a=vcs.init()
     a.plot(array)
     a.pstogif('filename.ps')       # convert the postscript file to a gif file (l=landscape)
     a.pstogif('filename.ps','l')   # convert the postscript file to a gif file (l=landscape)
     a.pstogif('filename.ps','p')   # convert the postscript file to a gif file (p=portrait)
 """ 
        from os import popen
 
        # Generate the VCS postscript file
        if (filename[-3:] != '.ps'):
           filename = filename + '.ps'
 
        # Set the default orientation to landscape'
        if len(opt) == 0:
            orientation = 'l'
        else:
            orientation = opt[0]
        # end of if len(orientation) == 0:
     
        cmd1 = 'gs -r72x72 -q -sDEVICE=ppmraw -sOutputFile=- '
        cmd2flip = ' | pnmflip -cw '
        cmd3 = '| pnmcrop | ppmtogif > '
     
        if orientation == 'l':
            cmd = cmd1 + filename + cmd2flip + cmd3 + filename[:-2] + 'gif\n'
        elif orientation == 'p':
            cmd = cmd1 + filename + cmd3 + filename[:-2] + 'gif \n'
        else:
            cmd = '\n'
        # end if orientation == 'l':
        f=popen(cmd, 'w')
        f.close()
        return
 
#    def gif(self, gif_name):
#        """
# Function: gif
#
# Description of Function:
#    In some cases, the user may want to save the plot out as a gif image. This
#    routine allows the user to save the VCS canvas output as a SUN gif file.
#    This file can be converted to other gif formats with the aid of xv and other
#    such imaging tools found freely on the web.
#
#    If no path/file name is given and no previously created gif file has been
#    designated, then file
#
#        /$HOME/PCMDI_GRAPHICS/default.gif
#
#    will be used for storing gif images. However, if a previously created gif 
#    file is designated, that file will be used for gif output.
#
# Example of Use:
#    a=vcs.init()
#    a.plot(array)
#    a.gif('example')           # overwrite existing gif file
#    a.gif(s,a,t,'example')     # overwrite existing gif file
#""" 
#       import os,sys,tempfile
#
#       # get name filename
#       extension = gif_name[-4:]
#       if (extension == '.gif'):
#          name = gif_name[:-4]
#       else:
#          name = gif_name
#
#       # Generate the VCS postscript file
#        ps_name = name + '.ps'
#       self.postscript(ps_name, 'r')
#
#       # Use temporary files for: ppm_name, ppm_cropname, ppm_cropname, & ppm_rotatename
#       # Convert the VCS postscript file to a ppm file using GhostScript (gs)
#       ppm_name= tempfile.mktemp()
#        pid = os.fork()
#        if pid == 0:   # must be the child!
#            try:
#                # This whole thing is within a try block to make sure
#                # the child can't escape.
#                out_name = "-sOutputFile=" + ppm_name
#                arglist=[ "-q", "-dNOPAUSE", "-dNO_PAUSE", "-dSAFER", "-sDEVICE=ppmraw", "-dBATCH", out_name, ps_name]
#                os.execvp("gs", arglist) # execute the gs command
#            except:
#                sys.stderr.write('Unexpected exception in child!\n')
#                os._exit(2) # exit child but not parent
# 
#        # Stop the parent until the child is finished! Alway wait on children.
#        os.wait()
#
## Crop ppm file using pnmcrop
#        ppm_cropname= tempfile.mktemp()
#        os_command = 'pnmcrop %s > %s' % (ppm_name,ppm_cropname)
#        os.system(os_command)
#        os.remove(ppm_name)
#
#        # Rotate ppm file using pnmrotate
#        ppm_rotatename= tempfile.mktemp()
#        os_command = 'pnmrotate -noantialias -90 %s > %s' % (ppm_cropname,ppm_rotatename)
#        os.system(os_command)
#        os.remove(ppm_cropname)
#
#        # Fiinally, generate GIF file using ppmtogif
##        os_command = 'ppmtogif %s > %s.gif' % (ppm_rotatename,name)
#        os.system(os_command)
#        os.remove(ppm_rotatename)

    #############################################################################
    #                                                                           #
    # Grid wrapper for VCS.                                                     #
    #                                                                           #
    #############################################################################
    def grid(self, *args):
        """
 Function: grid
    
 Description of Function:
    Set the default plotting region for variables that have more dimension values
    than the graphics method. This will also be used for animating plots over the
    third and fourth dimensions.
 
 
 Example of Use:
    a=vcs.init()
    a.grid(12,12,0,71,0,45)
""" 
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        p = apply(self.canvas.grid, args)

        self.canvas.UNBLOCK_X_SERVER()

        return p

    #############################################################################
    #                                                                           #
    # Landscape VCS Canvas orientation wrapper for VCS.                         #
    #                                                                           #
    #############################################################################
    def landscape(self, width=-99, height=-99, x=-99, y=-99, clear=0):
        """
 Function: landscape

 Description of Function:
    Change the VCS Canvas orientation to Landscape.

     Note: the (width, height) and (x, y) arguments work in pairs. That is, you must
           set (width, height) or (x, y) together to see any change in the VCS Canvas.

           If the portrait method is called  with arguments before displaying a VCS Canvas,
           then the arguments (width, height, x, y, and clear) will have no effect on the
           canvas.

     Known Bug: If the visible plot on the VCS Canvas is not adjusted properly, then resize
                the screen with the point. Some X servers are not handling the threads properly
                to keep up with the demands of the X client.

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.landscape() # Change the VCS Canvas orientation and set object flag to landscape
    a.landscape(clear=1) # Change the VCS Canvas to landscape and clear the page
    a.landscape(width = 400, height = 337) # Change to landscape and set the window size
    a.landscape(x=100, y = 200) # Change to landscape and set the x and y screen position
    a.landscape(width = 400, height = 337, x=100, y = 200, clear=1) # Chagne to landscape and give specifications
"""
        if (self.orientation() == 'landscape'): return

        if ( ((not isinstance(width, IntType))) or ((not isinstance(height, IntType))) or
             ((not isinstance(x, IntType))) or ((not isinstance(y, IntType))) or
             ((width != -99) and (width < 0)) or ( (height != -99) and (height < 0)) or
              ((x != -99) and  (x < 0)) or ((y != -99) and (y <0)) ):
           raise ValueError, 'If specified, width, height, x, and y must be integer values greater than or equal to 0.'
        if ( ((not isinstance(clear, IntType))) and (clear not in [0,1])):
           raise ValueError, "clear must be: 0 - 'the default value for not clearing the canvas' or 1 - 'for clearing the canvas'."
           
        if ( (width==-99) and (height==-99) and (x==-99) and (y==-99) and (clear==0) ):
            cargs = ()
            try: dict = apply(self.canvas.canvasinfo, cargs)
            except: dict={}
            height=dict.get('width',-99)
            width=dict.get('height',-99)
            x=dict.get('x',-99)
            y=dict.get('y',-99)
        self.flush() # update the canvas by processing all the X events

        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        args = (width, height, x, y, clear)
        l = apply(self.canvas.landscape, args)

        self.canvas.UNBLOCK_X_SERVER()

        return l

    #############################################################################
    #                                                                           #
    # List Primary and Secondary elements wrapper for VCS.                      #
    #                                                                           #
    #############################################################################
    def listelements(self, *args):
        """
 Function: listelements

 Description of Function:
    Returns a Python list of all the VCS class objects.

   The list that will be returned:
   ['template', 'boxfill', 'continent', 'isofill', 'isoline', 'outfill', 'outline',
    'scatter', 'vector', 'xvsy', 'xyvsy', 'yxvsx', 'colormap', 'fillarea', 'format', 
    'line', 'list', 'marker', 'text']

 Example of Use:
    a=vcs.init()
    a.listelements()
"""
        if args != () and string.lower( args[0] ) =='taylordiagram':
            L = []
            for t in vcs.taylordiagrams:
                L.append(t.name)
        else:
            L = apply(self.canvas.listelements, args)

        L.sort()

        return L

    #############################################################################
    #                                                                           #
    # update VCS's Canvas orientation wrapper for VCS.                          #
    #                                                                           #
    #############################################################################
    def updateorientation(self, *args):
        """
 Example of Use:
    a=vcs.init()
    x.updateorientation()
"""
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        a = apply(self.canvas.updateorientation, args)

        self.canvas.UNBLOCK_X_SERVER()
        return a

    #############################################################################
    #                                                                           #
    # Open VCS Canvas wrapper for VCS.                                          #
    #                                                                           #
    #############################################################################
    def open(self, *args):
        """
 Function: open

 Description of Function:
    Open VCS Canvas object. This routine really just manages the VCS canvas. It will
    popup the VCS Canvas for viewing. It can be used to display the VCS Canvas.

 Example of Use:
    a=vcs.init()
    a.open()    
"""
        a = apply(self.canvas.open, args)

        # Make sure xmainloop is started. This is needed to check for X events
        # (such as, Canvas Exposer, button or key press and release, etc.)
        if ( self.canvas.THREADED() == 0 ):
          thread.start_new_thread( self.canvas.startxmainloop, ( ) )

        return a

    #############################################################################
    #                                                                           #
    # Return VCS Canvas ID.                                                     #
    #                                                                           #
    #############################################################################
    def canvasid(self, *args):
        '''
 Function: canvasid

 Description of Function:
    Return VCS Canvas object ID. This ID number is found at the top of the VCS Canvas
    as part of its title.

 Example of Use:
    a=vcs.init()
    a.open()
    id = a.canvasid()
'''
        return apply(self.canvas.canvasid, args)

    #############################################################################
    #                                                                           #
    # Connect the VCS Canvas to the GUI.                                        #
    #                                                                           #
    #############################################################################
    def _connect_gui_and_canvas(self, *args):
        return apply(self.canvas.connect_gui_and_canvas, args)

    #############################################################################
    #                                                                           #
    # Page VCS Canvas orientation ('portrait' or 'landscape') wrapper for VCS.  #
    #                                                                           #
    #############################################################################
    def page(self, *args):
        """
 Function: page

 Description of Function:
    Change the VCS Canvas orientation to either 'portrait' or 'landscape'.

    The orientation of the VCS Canvas and of cgm and raster images is controlled by
    the PAGE command. Only portrait (y > x) or landscape (x > y) orientations are
    permitted.

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.page()      # Change the VCS Canvas orientation and set object flag to portrait
"""
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        l = apply(self.canvas.page, args)

        self.canvas.UNBLOCK_X_SERVER()

        return l

    #############################################################################
    #                                                                           #
    # Portrait VCS Canvas orientation wrapper for VCS.                          #
    #                                                                           #
    #############################################################################
    def portrait(self, width=-99, height=-99, x=-99, y=-99, clear=0):
        """
 Function: portrait

 Description of Function:
    Change the VCS Canvas orientation to Portrait.

     Note: the (width, height) and (x, y) arguments work in pairs. That is, you must
           set (width, height) or (x, y) together to see any change in the VCS Canvas.

           If the portrait method is called  with arguments before displaying a VCS Canvas,
           then the arguments (width, height, x, y, and clear) will have no effect on the
           canvas.

     Known Bug: If the visible plot on the VCS Canvas is not adjusted properly, then resize
                the screen with the point. Some X servers are not handling the threads properly
                to keep up with the demands of the X client.

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.portrait()      # Change the VCS Canvas orientation and set object flag to portrait
    a.portrait(clear=1) # Change the VCS Canvas to portrait and clear the page
    a.portrait(width = 337, height = 400) # Change to portrait and set the window size
    a.portrait(x=100, y = 200) # Change to portrait and set the x and y screen position
    a.portrait(width = 337, height = 400, x=100, y = 200, clear=1) # Chagne to portrait and give specifications
""" 
        if (self.orientation() == 'portrait'): return

        if ( ((not isinstance(width, IntType))) or ((not isinstance(height, IntType))) or
             ((not isinstance(x, IntType))) or ((not isinstance(y, IntType)))  or 
             ((width != -99) and (width < 0)) or ( (height != -99) and (height < 0)) or
              ((x != -99) and  (x < 0)) or ((y != -99) and (y <0)) ):
           raise ValueError, 'If specified, width, height, x, and y must be integer values greater than or equal to 0.'
        if ( ((not isinstance(clear, IntType))) and (clear not in [0,1])):
           raise ValueError, "clear must be: 0 - 'the default value for not clearing the canvas' or 1 - 'for clearing the canvas'."

        if ( (width==-99) and (height==-99) and (x==-99) and (y==-99) and (clear==0) ):
            cargs = ()
            try: dict = apply(self.canvas.canvasinfo, cargs)
            except: dict={}
            height=dict.get('width',-99)
            width=dict.get('height',-99)
            x=dict.get('x',-99)
            y=dict.get('y',-99)
        self.flush() # update the canvas by processing all the X events

        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        args = (width, height, x, y, clear)
        p = apply(self.canvas.portrait, args)

        self.canvas.UNBLOCK_X_SERVER()
        return p

    ##########################################################################
    #                                                                        #
    # png wrapper for VCS.                                                   #
    #                                                                        #
    ##########################################################################
    def ffmpeg(self, movie, files, bitrate=1024, rate=None, options=''):
        """
 Function: ffmpeg

 Description of Function:
    MPEG output from a list of valid files.
    Note that ffmpeg is smart enough to output to more than just mpeg format

 Example of Use:
    a=vcs.init()
    #... code to generate png files ...
    # here is dummy example
    files =[]
    for i in range(10):
      x.png('my_png__%i' % i)
      files.append('my_png__%i.png' % i)
    x.ffmpeg('mymovie.mpeg','my_png_%d.png') # generates mpeg from pattern
    x.ffmpeg('mymovie.mpeg',files) # generates from list of files
    x.ffmpeg('mymovie.mpeg','my_png_%d.png',bitrate=512) # generates mpeg at 512kbit bitrate (bitrate is important to movie quality)
    x.ffmpeg('mymovie.mpeg','my_png_%d.png',rate=50) # generates movie with 50 frame per second
    x.ffmpeg('mymovie.mpeg','my_png_%d.png',options='-r 50 -b 1024k') # genrats movie at 50 frame per sec and 1024k bitrate
    NOTE : via the optins arg you can add audio file to your movie (see ffmpeg help)
    returns the output string generated by ffmpeg program
    ALWAYS overwrite output file
"""
        cmd = 'ffmpeg -y '

        if rate is not None:
            cmd+=' -r %s ' % rate
        if isinstance(files,(list,tuple)):
            rnd = "%s/.uvcdat/__uvcdat_%i" % (os.environ["HOME"],numpy.random.randint(60000))
            Files = []
            for i,f in enumerate(files):
                fnm = "%s_%i.png" % (rnd,i)
                shutil.copy(f,fnm)
                Files.append(fnm)
            cmd+='-i %s_%%d.png' % (rnd)
        elif isinstance(files,str):
            cmd+='-i '+files
        if rate is not None:
            cmd+=' -r %s ' % rate
        if bitrate is not None:
            cmd+=' -b:v %sk' % bitrate
        cmd+=' '+options
        cmd+=' '+movie
        print "COMMANF FFMPEG:",cmd
        o = os.popen(cmd).read()
        if isinstance(files,(list,tuple)):
            for f in Files:
                os.remove(f)
        return o
    ##########################################################################
    #                                                                        #
    # bg dims wrapper for VCS.                                               #
    #                                                                        #
    ##########################################################################
    def setbgoutputdimensions(self, width=None,height=None,units='inches'):
        """
 Function: setbgoutputdimensions

 Description of Function:
    Sets dimensions for output in bg mode.

 Example of Use:
    a=vcs.init()
    a.setbgoutputdimensions(width=11.5, height= 8.5)  # US Legal
    a.setbgoutputdimensions(width=21, height=29.7, units='cm')  # A4
"""
        if not units in ['inches','in','cm','mm','pixel','pixels','dot','dots']:
            raise Exception,"units must be on of inches, in, cm, mm, pixel(s) or dot(s)"

        dpi = 72. # dot per inches
        if units in ["in","inches"]:
            factor = 1.
        elif units == 'cm':
            factor = 0.393700787
        elif units=='mm':
            factor = 0.0393700787
        else:
            factor = 1./72
        width,height,sfactor = self._compute_width_height(width,height,factor)
        W = int(width*dpi*sfactor)
        H = int(height*dpi*sfactor)

        # if portrait then switch
        if self.isportrait() and W>H:
            tmp = W
            W= H
            H = tmp
            
        return apply(self.canvas.setbgoutputdimensions,(W,H))
    ##########################################################################
    #                                                                        #
    # png wrapper for VCS.                                                   #
    #                                                                        #
    ##########################################################################
    def png(self, file, width=None,height=None,units=None,draw_white_background = 0):
        """
 Function: png

 Description of Function:
    PNG output, dimensions set via setbgoutputdimensions

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.png('example')       # Overwrite a png file
"""
        if units is not None or width is not None or height is not None:
            if self.iscanvasdisplayed():
                warnings.warn("Dimensions cannot be set once canvas is opened, window dims will be used, use bg=1 when plotting to control output dimesnsions")
            else:
                warnings.warn("Dimensions must be set apriori (before plotting in bg mode) via setbgoutputdimensions function")
        if self.iscanvasdisplayed():
            info=self.canvasinfo()
            self.setbgoutputdimensions(info["width"],info["height"],"pixels")
            
        if not file.split('.')[-1].lower() in ['png']:
            file+='.png'
        return apply(self.canvas.png,(file,draw_white_background))
    #############################################################################
    #                                                                           #
    # pdf wrapper for VCS.                                               #
    #                                                                           #
    #############################################################################
    def pdf(self, file, width=None,height=None,units='inches'):
        """
 Function: postscript

 Description of Function:
    SVG output is another form of vector graphics.

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.png('example')       # Overwrite a postscript file
    a.png('example', width=11.5, height= 8.5)  # US Legal
    a.png('example', width=21, height=29.7, units='cm')  # A4
"""
        if not units in ['inches','in','cm','mm','pixel','pixels','dot','dots']:
            raise Exception,"units must be on of inches, in, cm, mm, pixel(s) or dot(s)"

        dpi = 72. # dot per inches
        if units in ["in","inches"]:
            factor = 1.
        elif units == 'cm':
            factor = 0.393700787
        elif units=='mm':
            factor = 0.0393700787
        else:
            factor = 1./72
        width,height,sfactor = self._compute_width_height(width,height,factor)
        W = int(width*dpi*sfactor)
        H = int(height*dpi*sfactor)

            
        if not file.split('.')[-1].lower() in ['pdf']:
            file+='.pdf'
        return apply(self.canvas.pdf,(file,W,H))
    #############################################################################
    #                                                                           #
    # SVG wrapper for VCS.                                               #
    #                                                                           #
    #############################################################################
    def svg(self, file, width=None,height=None,units='inches'):
        """
 Function: postscript

 Description of Function:
    SVG output is another form of vector graphics.

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.svg('example')       # Overwrite a postscript file
    a.svg('example', width=11.5, height= 8.5)  # US Legal
    a.svg('example', width=21, height=29.7, units='cm')  # A4
"""
        if not units in ['inches','in','cm','mm','pixel','pixels','dot','dots']:
            raise Exception,"units must be on of inches, in, cm, mm, pixel(s) or dot(s)"

        dpi = 72. # dot per inches
        if units in ["in","inches"]:
            factor = 1.
        elif units == 'cm':
            factor = 0.393700787
        elif units=='mm':
            factor = 0.0393700787
        else:
            factor = 1./72
        width,height,sfactor = self._compute_width_height(width,height,factor)
        W = int(width*dpi*sfactor)
        H = int(height*dpi*sfactor)

         # if portrait then switch
        if self.isportrait() and W>H:
            tmp = W
            W= H
            H = tmp
           
        if not file.split('.')[-1].lower() in ['svg']:
            file+='.svg'
        return apply(self.canvas.svg,(file,W,H))

    def _compute_margins(self,W,H,top_margin,bottom_margin,right_margin,left_margin,dpi):
        try:
            ci = self.canvasinfo()
            height = ci['height']
            width = ci['width']
            factor=1./72;
            size = float(width)/float(height)
        except Exception,err:
            factor=1.;
            if self.size is None:
                size = 1.2941176470588236
            else:
                size = self.size
        if bottom_margin is not None:
            bottom_margin  = bottom_margin*factor
        if left_margin is not None:
            left_margin = left_margin*factor
        if right_margin is not None:
            right_margin=right_margin*factor
        if top_margin is not None:
            top_margin  = top_margin*factor

        # now for sure factor is 1.
        factor =1.
        if left_margin is None and right_margin is None and top_margin is None and bottom_margin is None:
            # default margins
            left_margin = .25
            right_margin = .25
            top_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H - twidth/size)/dpi - top_margin
            bottom_margin = (top_margin+bottom_margin)/2.
            top_margin=bottom_margin
        elif left_margin is None and right_margin is None and top_margin is None:    # bottom_defined
            left_margin = .25
            right_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            top_margin = (H - twidth/size)/dpi - bottom_margin
        elif left_margin is None and right_margin is None and bottom_margin is None: # top_defined
            left_margin = .25
            right_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H)/dpi - top_margin
        elif top_margin is None and bottom_margin is None and left_margin is None:   # right defined
            left_margin = .25
            top_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H - twidth/size)/dpi - top_margin
        elif top_margin is None and bottom_margin is None and right_margin is None:  # left defined
            right_margin = .25
            top_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H - twidth/size)/dpi - top_margin
        elif top_margin is None and right_margin is None:                            # left defined and bottom
            right_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            top_margin = (H - twidth/size)/dpi - bottom_margin
        elif top_margin is None and left_margin is None:                             # right defined and bottom
            left_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            top_margin = (H - twidth/size)/dpi - bottom_margin
        elif bottom_margin is None and left_margin is None:                          # right defined and top
            left_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H - twidth/size)/dpi - top_margin
        elif bottom_margin is None and right_margin is None:                         # left defined and top
            right_margin = .25
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H - twidth/size)/dpi - top_margin
        elif bottom_margin is None:                                                  # all but bottom
            twidth = W - (left_margin+right_margin)*dpi
            bottom_margin = (H - twidth/size)/dpi - top_margin
        elif top_margin is None:                                                     # all but top
            twidth = W - (left_margin+right_margin)*dpi
            top_margin = (H - twidth/size)/dpi - bottom_margin
        elif right_margin is None:                                                   # all but right
            theight = H - (top_margin+bottom_margin)*dpi
            right_margin = (W - theight*size)/dpi + left_margin
        elif left_margin is None:                                                   # all but left
            theight = H - (top_margin+bottom_margin)*dpi
            left_margin = (W - theight*size)/dpi + right_margin

        return top_margin,bottom_margin,right_margin,left_margin


    def _compute_width_height(self,width,height,factor,ps=True):
        sfactor = factor
        if width is None and height is None:
            try:
                ci = self.canvasinfo()
                height = ci['height']
                width = ci['width']
                sfactor =  1./72.
                if ps is True:
                    ratio  = width/float(height)
                    if self.size == 1.4142857142857141:
                        # A4 output
                        width=29.7
                        sfactor=0.393700787
                        height= 21.
                    elif self.size == 1./1.4142857142857141:
                        width=21.
                        sfactor=0.393700787
                        height= 29.7
                    else:
                        sfactor = 1.
                        if ratio >1:
                            width=11.
                            height=width/ratio
                        else:
                            height=11.
                            width=height*ratio
            except: # canvas never opened
                if self.size is None:
                    sfactor = 1.
                    height=8.5
                    width= 11.
                elif self.size == 1.4142857142857141:
                    sfactor = 0.393700787
                    width = 29.7
                    height = 21.
                else:
                    sfactor = 1.
                    height=8.5
                    width= self.size*height
        elif width is None:
            if self.size is None:
                width = 1.2941176470588236*height
            else:
                width = self.size*height
        elif height is None:
            if self.size is None:
                height = width / 1.2941176470588236
            else:
                height = width / self.size
        ## Now forces correct aspect ratio for dumping in bg
        ## if self.iscanvasdisplayed():
        ##     info=self.canvasinfo()
        ##     ratio = float(info["height"])/float(info["width"])
        ##     if ratio < 1.:
        ##         ratio=1./ratio
        ## else:
        ##     ratio = 1.3127035830618892
        ## if height>width:
        ##     if height/width>ratio:
        ##         height=ratio*width
        ##     else:
        ##         width=height/ratio
        ## else:
        ##     if width/height>ratio:
        ##         width=ratio*height
        ##     else:
        ##         height=width/ratio
        return width,height,sfactor
    
    def postscript(self, file,mode='r',orientation=None,width=None,height=None,units='inches',left_margin=None,right_margin=None,top_margin=None,bottom_margin=None):
        """
 Function: postscript

 Description of Function:
    Postscript output is another form of vector graphics. It is larger than its CGM output
    counter part, because it is stored out in ASCII format.
    
    There are two modes for saving a postscript file: `Append' (a) mode appends postscript
    output to an existing postscript file; and `Replace' (r) mode overwrites an existing 
    postscript file with new postscript output. The default mode is to overwrite an existing
    postscript file (i.e. mode (r)).


 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.postscript('example')       # Overwrite a postscript file
    a.postscript('example', 'a')  # Append postscript to an existing file
    a.postscript('example', 'r')  # Overwrite an existing file
    a.postscript('example', mode='a')  # Append postscript to an existing file
    a.postscript('example', width=11.5, height= 8.5)  # US Legal (default)
    a.postscript('example', width=21, height=29.7, units='cm')  # A4
    a.postscript('example', right_margin=.2,left_margin=.2,top_margin=.2,bottom_margin=.2)  # US Legal output and control of margins (for printer friendly output), default units 'inches'
"""
        if not units in ['inches','in','cm','mm','pixel','pixels','dot','dots']:
            raise Exception,"units must be on of inches, in, cm, mm, pixel(s) or dot(s)"

        dpi = 72. # dot per inches
        if units in ["in","inches"]:
            factor = 1.
        elif units == 'cm':
            factor = 0.393700787
        elif units=='mm':
            factor = 0.0393700787
        else:
            factor = 1./72

        # figures out width/height
        width,height,sfactor = self._compute_width_height(width,height,factor)
        W = int(width*dpi*sfactor)
        H = int(height*dpi*sfactor)

##         print "will usE:",W,H,float(W)/H
        # figures out margins
            
        top_margin,bottom_margin,right_margin,left_margin = self._compute_margins(W,H,top_margin,bottom_margin,right_margin,left_margin,dpi)

        R = int(right_margin*dpi)
        L = int(left_margin*dpi)
        T = int(top_margin*dpi)
        B = int(bottom_margin*dpi)

        if W>H:
            tmp = H
            H = W
            W = tmp
        # orientation keyword is useless left for backward compatibility
        if not file.split('.')[-1].lower() in ['ps','eps']:
            file+='.ps'
        if mode=='r':
            return apply(self.canvas.postscript,(file,W,H,R,L,T,B))
        else:
            n=random.randint(0,100000)
            psnm='/tmp/'+'__VCS__tmp__'+str(n)+'.ps'
            apply(self.canvas.postscript,(psnm,W,H,R,L,T,B))
            if os.path.exists(file):
                f=open(file,'r+')
                f.seek(0,2) # goes to end of file
                f2=open(psnm)
                f.writelines(f2.readlines())
                f2.close()
                f.close()
                os.remove(psnm)
            else:
                shutil.move(psnm,file)

    #############################################################################
    #                                                                           #
    # Postscript wrapper for VCS.                                               #
    #                                                                           #
    #############################################################################
    def postscript_old(self, file,mode='r',orientation=None):
        """
 Function: postscript

 Description of Function:
    Postscript output is another form of vector graphics. It is larger than its CGM output
    counter part, because it is stored out in ASCII format. To save out a postscript file,
    VCS will first create a cgm file in the user's %s directory. Then it will
    use gplot to convert the cgm file to a postscript file in the location the user has
    chosen.

    There are two modes for saving a postscript file: `Append' (a) mode appends postscript
    output to an existing postscript file; and `Replace' (r) mode overwrites an existing 
    postscript file with new postscript output. The default mode is to overwrite an existing
    postscript file (i.e. mode (r)).

    The POSTSCRIPT command is used to create a postscript file. Orientation is 'l' = landscape,
    or 'p' = portrait. The default is the current orientation of your canvas.
 
 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.postscript('example')       # Overwrite a postscript file
    a.postscript('example', 'a')  # Append postscript to an existing file
    a.postscript('example', 'r')  # Overwrite an existing file
    a.postscript('example', 'r', 'p')  # Overwrite postscript file with a portrait postscript file
    a.postscript('example', mode='a')  # Append postscript to an existing file
    a.postscript('example', orientation='r')  # Overwrite an existing file
    a.postscript('example', mode='r', orientation='p')  # Overwrite postscript file with a portrait postscript file
""" % self._dotdir
        if orientation is None:
            orientation=self.orientation()[0]
        return apply(self.canvas.postscript_old,(file,mode,orientation))

    #############################################################################
    #                                                                           #
    # Old PDF wrapper for VCS.                                                  #
    #                                                                           #
    #############################################################################
    def pdf_old(self, file,orientation=None,options='',width=None,height=None,units='inches',left_margin=None,right_margin=None,top_margin=None,bottom_margin=None):
        """
 Function: pdf

 Description of Function:
    To save out a PDF file,
    VCS will first create a cgm file in the user's %s directory. Then it will
    use gplot to convert the cgm file to a postscript file in the location the user has
    chosen. And then convert it pdf using ps2pdf

    The pdf command is used to create a pdf file. Orientation is 'l' = landscape,
    or 'p' = portrait. The default is landscape.
 
 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.pdf('example')      # Creates a landscape pdf file
    a.pdf('example','p')  # Creates a portrait pdf file
    a.pdf(file='example',orientation='p')  # Creates a portrait pdf file
    a.pdf(file='example',options='-dCompressPages=false')  # Creates a pdf file w/o compressing page, can be any option understood by ps2pdf
""" % (self._dotdir)

        n=random.randint(0,100000)
        if file[-3:].lower()!='pdf':
            file+='.pdf'
        psnm='/tmp/'+'__VCS__tmp__'+str(n)+'.ps'
        a=self.postscript(psnm,orientation=orientation,width=width,height=height,units=units,left_margin=left_margin,right_margin=right_margin,top_margin=top_margin,bottom_margin=bottom_margin)
        os.popen('ps2pdf14 '+options+' '+psnm+' '+file).readlines()
        os.remove(psnm)
        return a
    

    #############################################################################
    #                                                                           #
    # Printer wrapper for VCS.                                                  #
    #                                                                           #
    #############################################################################
    def printer(self, printer= None, orientation=None,width=None,height=None,units='inches',left_margin=None,right_margin=None,top_margin=None,bottom_margin=None):
        """
 Function: printer

 Description of Function:
    This function creates a temporary cgm file and then sends it to the specified
    printer. Once the printer received the information, then the temporary cgm file
    is deleted. The temporary cgm file is created in the user's %s directory.

    The PRINTER command is used to send the VCS Canvas plot(s) directly to the printer.
    Orientation can be either: 'l' = landscape, or 'p' = portrait.

    Note: VCS graphical displays can be printed only if the user customizes a HARD_COPY
    file (included with the VCS software) for the home system. The path to the HARD_COPY
    file must be: 

              /$HOME/%s/HARD_COPY

    where /$HOME denotes the user's home directory.


    For more information on the HARD_COPY file, see URL:

    http://www-pcmdi.llnl.gov/software/vcs/vcs_guidetoc.html#1.Setup

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.printer('printer_name') # Send plot(s) to postscript printer
    a.printer('printer_name',top_margin=1,units='cm') # Send plot(s) to postscript printer with 1cm margin on top of plot
""" % (self._dotdir,self._dotdir)
        if printer is None:
            printer = (os.environ.get('PRINTER'),)
            
        if not units in ['inches','in','cm','mm','pixel','pixels','dot','dots']:
            raise Exception,"units must be on of inches, in, cm, mm, pixel(s) or dot(s)"

        dpi = 72. # dot per inches
        if units in ["in","inches"]:
            factor = 1.
        elif units == 'cm':
            factor = 0.393700787
        elif units=='mm':
            factor = 0.0393700787
        else:
            factor = 1./72
        # figures out width/height
        width,height,sfactor = self._compute_width_height(width,height,factor)
        W = int(width*dpi*sfactor)
        H = int(height*dpi*sfactor)
        top_margin,bottom_margin,right_margin,left_margin = self._compute_margins(W,H,top_margin,bottom_margin,right_margin,left_margin,dpi)
        
        R = int(right_margin*dpi)
        L = int(left_margin*dpi)
        T = int(top_margin*dpi)
        B = int(bottom_margin*dpi)

        if W>H:
            tmp = H
            H = W
            W = tmp
            
        return apply(self.canvas.printer, (printer,W,H,R,L,T,B))

    #############################################################################
    #                                                                           #
    # Showbg wrapper for VCS.                                                   #
    #                                                                           #
    #############################################################################
    def showbg(self, *args):
        """
 Function: showbg

 Description of Function:
    This function displays graphics segments, which are currently stored in the frame buffer,
    on the VCS Canvas. That is, if the plot function was called with the option bg = 1 (i.e.,
    background mode), then the plot is produced in the frame buffer and not visible to the
    user. In order to view  the graphics segments, this function will copy the contents of
    the frame buffer to the VCS Canvas, where the graphics can be viewed by the user.

 Example of Use:
    a=vcs.init()
    a.plot(array, bg=1)
    x.showbg()
"""
        a = apply(self.canvas.showbg, args)

        # Make sure xmainloop is started. This is needed to check for X events
        # (such as, Canvas Exposer, button or key press and release, etc.)
        if ( self.canvas.THREADED() == 0 ):
          thread.start_new_thread( self.canvas.startxmainloop, ( ) )

        return a

    #############################################################################
    #                                                                           #
    # Backing Store wrapper for VCS.                                            #
    #                                                                           #
    #############################################################################
    def backing_store(self, *args):
        """
 Function: backing_store

 Description of Function:
    This function creates a backing store pixmap for the VCS Canvas.

 Example of Use:
    a=vcs.init()
    a.backing_store()
"""
        return apply(self.canvas.backing_store, args)

    #############################################################################
    #                                                                           #
    # Update the animation slab. Used only for the VCS Canvas GUI.              #
    #                                                                           #
    #############################################################################
    def update_animation_data(self, *args):
        return apply(self.canvas.update_animation_data, args)

    #############################################################################
    #                                                                           #
    # Return the dimension information. Used only for the VCS Canvas GUI.       #
    #                                                                           #
    #############################################################################
    def return_dimension_info(self, *args):
        return apply(self.canvas.return_dimension_info, args)

    #############################################################################
    #                                                                           #
    # Raster wrapper for VCS.                                                   #
    #                                                                           #
    #############################################################################
    def raster(self, file, mode='a'):
        """
 Function: raster
 
 Description of Function:
    In some cases, the user may want to save the plot out as an raster image. This
    routine allows the user to save the VCS canvas output as a SUN raster file.
    This file can be converted to other raster formats with the aid of xv and other
    such imaging tools found freely on the web.

    If no path/file name is given and no previously created raster file has been
    designated, then file

    /$HOME/%s/default.ras

    will be used for storing raster images. However, if a previously created raster
    file is designated, that file will be used for raster output.
 
 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.raster('example','a')   # append raster image to existing file
    a.raster('example','r')   # overwrite existing raster file
    a.raster(file='example',mode='r')   # overwrite existing raster file
"""  % (self._dotdir)
        return apply(self.canvas.raster, (file,mode))

    #############################################################################
    #                                                                           #
    # Reset grid wrapper for VCS.                                               #
    #                                                                           #
    #############################################################################
    def resetgrid(self, *args):
        """
 Function: resetgrid

 Description of Function:
    Set the plotting region to default values.
    
 Example of Use:
    Not Working!
"""     
        return apply(self.canvas.resetgrid, args)

    #############################################################################
    #                                                                           #
    # Script wrapper for VCS.                                                   #
    #                                                                           #
    #############################################################################
    def _scriptrun(self, *args):
        """
 Function: _scriptrun

 Description of Function:
    Run VCS script file.
   
 Example of Use:
    x._scriptrun('script_filename.scr')
"""
        # First reads the C stuff
        apply(self.canvas.scriptrun, args)
        # Now does the python Graphic methods
        f=open(args[0],'r')
        ln=f.readlines()
        f.close()
        # browse through the file to look for taylordiagram/python graphics methods
        ifound=0 # found a taylor graphic method
        for l in ln:
            if ifound==0 : s=''
            if l[:4]=='Gtd_':
                ifound=1
##             if l[:4]=='Gmf_':
##                 ifound=2
            if ifound == 1:
                i=string.find(l,')')
                if i==-1:  # not found the end,i.e ')'
                    s=s+l[:-1] # we dont want the trail carriage return
                else:
                    ifound=0
                    s=s+l[:i+1]
                    # Now break the string
                    # now gets the name and prepare the graphics method
                    sp=string.split(s,'(')
                    name=string.join(string.split(sp[0],'_')[1:],'_')
                    if name!='default' : # we cannot change default
                        try:
                            td=self.createtaylordiagram(name)
                        except Exception,err:
                            td=self.gettaylordiagram(name)
                        sp=string.split(sp[1],';') # breaks the thing into different attributes
                        imark=0
                        for a in sp : # the last one is ')'
                            sp2=string.split(a,'=')
                            if string.strip(sp2[0])=='Marker' : imark=1
                            if len(sp2)==2:
                                if imark:
                                    setattr(td.Marker,string.strip(sp2[0]),eval(sp2[1]))
                                else:
                                    setattr(td,string.strip(sp2[0]),eval(sp2[1]))
##             elif ifound == 2:
##                 i=string.find(l,')')
##                 if i==-1:  # not found the end,i.e ')'
##                     s=s+l[:-1] # we dont want the trail carriage return
##                 else:
##                     ifound=0
##                     s=s+l[:i+1]
##                     # Now break the string
##                     # now gets the name and prepare the graphics method
##                     sp=string.split(s,'(')
##                     name=string.join(string.split(sp[0],'_')[1:],'_')
##                     if name!='default' : # we cannot change default
##                         try:
##                             mesh=self.createmeshfill(name)
##                         except:
##                             mesh=self.getmeshfill(name)
##                         sp=string.split(sp[1],';') # breaks the thing into different attributes
##                         for a in sp : # the last one is ')'
##                             sp2=string.split(a,'=')
##                             if len(sp2)==2:
##                                 setattr(mesh,string.strip(sp2[0]),eval(sp2[1]))
                                
    #############################################################################
    #                                                                           #
    # Import old VCS file script commands into CDAT.                            #
    #                                                                           #
    #############################################################################
    def scriptrun(self, *args):
        import __main__
        ## Following comented by C. Doutriaux seems to be useless
        ## from cdms2.selectors import Selector

        # Open VCS script file for reading and read all lines into a Python list
        fin = open(args[0], 'r')
        l=fin.readlines()
        line_ct = len(l)
        i = 0

        # Check to see if it is a VCS generated Python script file. If it is, then simply
        # call the execfile function to execute the script and close the file.
        if ( (l[0][0:37] == "#####################################") and
             (l[1][0:35] == "#                                 #") and
             (l[2][0:33] == "# Import and Initialize VCS     #") and
             (l[3][0:31] == "#                             #") and
             (l[4][0:29] == "#############################") ):
            fin.close()
            execfile( args[0], __main__.__dict__ )
            return

        while i < line_ct:                          
           # Loop through all lines and determine when a VCS command line
           # begins and ends. That is, get only one VCS command at a time
           scr_str = l[i]
           lt_paren_ct = string.count(l[i], '(')
           rt_paren_ct = string.count(l[i], ')')
           while lt_paren_ct > rt_paren_ct:
              i += 1
              scr_str += l[i]
              lt_paren_ct += string.count(l[i], '(')
              rt_paren_ct += string.count(l[i], ')')
           i += 1
           scr_str = string.strip( scr_str )
        
           # Get the VCS command
           vcs_cmd = string.split(string.split(scr_str, '(')[0], '_')[0]
        
           function = source = name = units = title = lon_name = lat_name = ''
           comment1 = comment2 = comment3 = comment4 = ''
           if vcs_cmd == 'A':
              # Get the data via CDMS. That is, retrieve that data as a
              # _TransientVariable. But first, get the source, name, title,
              # etc. of the file.
              slab_name = string.split(scr_str, '(')[0][2:]
              a=string.split(scr_str,'",')
              for j in range(len(a)):
                 b=string.split(a[j],'="')
                 if string.lower(b[0][-4:]) == 'file':
                    fcdms=cdms2.open(b[1])                       # Open CDMS file
                 elif string.lower(b[0][-8:]) == 'function':
                    function =b[1]                              # Get function
                 elif string.lower(b[0]) == 'source':
                    source = b[1]
                 elif ( (string.lower(b[0][-4:]) == 'name') and
                        (string.lower(b[0][-5:]) != 'xname') and
                        (string.lower(b[0][-5:]) != 'yname') ):
                    name = string.split( b[1], '")')[0]
                 elif string.lower(b[0]) == 'units':
                    units = string.split( b[1], '")')[0]
                 elif string.lower(b[0][-5:]) == 'title':
                    title = string.split( b[1], '")')[0]
                 elif string.lower(b[0][-5:]) == 'xname':
                    lon_name = string.strip(string.split( b[1], '")')[0])
                 elif string.lower(b[0][-5:]) == 'yname':
                    lat_name = string.strip(string.split( b[1], '")')[0])
                 elif string.lower(b[0][-9:]) == 'comment#1':
                    comment1 = b[1]
                 elif string.lower(b[0][-9:]) == 'comment#2':
                    comment2 = b[1]
                 elif string.lower(b[0][-9:]) == 'comment#3':
                    comment3 = b[1]
                 elif string.lower(b[0][-9:]) == 'comment#4':
                    comment4 = b[1]
## Comented out by C. Doutriaux, shouldn't print anything
##               print 'function = ', function
##               print 'source = ', source
##               print 'name = ', name
##               print 'units = ', units
##               print 'title = ', title
##               print 'lon_name = ', lon_name
##               print 'lat_name = ', lat_name
##               print 'comment1 = ', comment1
##               print 'comment2 = ', comment2
##               print 'comment3 = ', comment3
##               print 'comment4 = ', comment4

              if function != '':
                 b=string.split(function, '(')
                 ftype=b[0]
                 V=string.split(b[1],',')[0]
## Comented out by C. Doutriaux, shouldn't print anything
##                  print 'ftype = ', ftype
##                  print 'V = ', V
##                  print 'slab_name = ', slab_name
                 __main__.__dict__[ slab_name ] = __main__.__dict__[ V ] * 1000.
#                 __main__.__dict__[ slab_name ] = cdutil.averager(
#                    __main__.__dict__[ V ], axis='( %s )' % 'zeros_ones_dim_1',
#                       weight='equal')
                 continue


              a=string.split(scr_str,',')
              
              # Now get the coordinate values
              x1 = x2 = y1 = y2 = None
              for j in range(len(a)):
                 c=string.split(a[j], ',')[0]
                 b=string.split(c, '=')
                 if string.lower(b[0]) == 'xfirst':
                    x1 = string.atof( string.split(b[1], ')')[0] )
                 elif string.lower(b[0]) == 'xlast':
                    x2 = string.atof( string.split(b[1], ')')[0] )
                 elif string.lower(b[0][-6:]) == 'yfirst':
                    y1 = string.atof( string.split(b[1], ')')[0] )
                 elif string.lower(b[0]) == 'ylast':
                    y2 = string.atof( string.split(b[1], ')')[0] )

              # Get the variable from the CDMS opened file
              V=fcdms.variables[name]

              # Check for the order of the variable and re-order dimensions
              # if necessary
              Order = '(%s)(%s)' % (lat_name,lon_name)
              Order = string.strip( string.replace( Order, '()', '' ) )
              if Order == '': Order = None
              axis_ids = V.getAxisIds()
              re_order_dimension = 'no'
              try:                 # only re-order on two or more dimensions
                 if (axis_ids[-1] != lon_name) and (axis_ids[-2] != lat_name):
                    re_order_dimension = 'yes'
              except:
                 pass

              # Must have the remaining dimension names in the Order list
              if Order is not None:
                 O_ct = string.count(Order,'(')
                 V_ct = len( V.getAxisIds() )
                 for j in range(O_ct, V_ct):
                    Order = ('(%s)' % axis_ids[V_ct-j-1]) + Order

              # Set the data dictionary up to retrieve the dat from CDMS
              if re_order_dimension == 'no':
                 if ( (x1 is not None) and (x2 is not None) and
                    (y1 is not None) and (y2 is not None) ):
                    data_dict = {lon_name:(x1,x2) ,lat_name:(y1,y2), 'order':Order}
                 elif ( (x1 is not None) and (x2 is not None) and
                    (y1 is None) and (y2 is None) ):
                    data_dict = {lon_name:(x1,x2), 'order':Order}
                 elif ( (x1 is None) and (x2 is None) and
                    (y1 is not None) and (y2 is not None) ):
                    data_dict = {lat_name:(y1,y2), 'order':Order}
                 elif ( (x1 is None) and (x2 is None) and
                    (y1 is None) and (y2 is None) ):
                    data_dict = {}
              else:
                 if ( (x1 is not None) and (x2 is not None) and
                    (y1 is not None) and (y2 is not None) ):
                    data_dict = {lat_name:(x1,x2) ,lon_name:(y1,y2), 'order':Order}
                 elif ( (x1 is not None) and (x2 is not None) and
                    (y1 is None) and (y2 is None) ):
                    data_dict = {lon_name:(x1,x2), 'order':Order}
                 elif ( (x1 is None) and (x2 is None) and
                    (y1 is not None) and (y2 is not None) ):
                    data_dict = {lat_name:(y1,y2), 'order':Order}
                 elif ( (x1 is None) and (x2 is None) and
                    (y1 is None) and (y2 is None) ):
                    data_dict = {}

              # Now store the _TransientVariable in the main dictionary for use later
              __main__.__dict__[ slab_name ] = apply(V, (), data_dict)

              fcdms.close()                                     # Close CDMS file
           elif vcs_cmd == 'D':
              # plot the data with the appropriate graphics method and template
              a=string.split(scr_str,',')
              a_name = b_name = None
              for j in range(len(a)):
                 b=string.split(a[j],'=')
                 if string.lower(b[0][-3:]) == 'off':
                    off = string.atoi( b[1] )
                 elif string.lower(b[0]) == 'priority':
                    priority = string.atoi( b[1] )
                 elif string.lower(b[0]) == 'type':
                    graphics_type = b[1]
                 elif string.lower(b[0]) == 'template':
                    template = b[1]
                 elif string.lower(b[0]) == 'graph':
                    graphics_name = b[1]
                 elif string.lower(b[0]) == 'a':
                    a_name = string.split(b[1],')')[0]
                 elif string.lower(b[0]) == 'b':
                    b_name = string.split(b[1],')')[0]

              arglist=[]
            
              if a_name is not None:
                 arglist.append(__main__.__dict__[ a_name ])
              else:
                 arglist.append( None )
              if b_name is not None:
                 arglist.append(__main__.__dict__[ b_name ])
              else:
                 arglist.append( None )
              arglist.append(template)
              arglist.append(graphics_type)
              arglist.append(graphics_name)

              # flush and block the X main loop
              finish_queued_X_server_requests( self )
              self.canvas.BLOCK_X_SERVER()

              if (a_name is not None) and (graphics_type != 'continents'):
                 dn = self.__plot(arglist, {'bg':0})

              # Unblock the (thread) execution of the X main loop (if it is running).
              self.canvas.UNBLOCK_X_SERVER()

           elif string.lower( vcs_cmd ) == 'canvas':
              apply(self.canvas.open, args)
              if ( self.canvas.THREADED() == 0 ):
                 thread.start_new_thread( self.canvas.startxmainloop, ( ) )
           elif string.lower( vcs_cmd ) == 'page':
              orientation = string.lower( string.split(scr_str,'(')[1][:-1] )
              finish_queued_X_server_requests( self )
              self.canvas.BLOCK_X_SERVER()
              if orientation == 'portrait':
                 apply(self.canvas.portrait, args)
              else:
                 apply(self.canvas.landscape, args)
              self.canvas.UNBLOCK_X_SERVER()
           else: # Send command to VCS interpreter
              if (len(scr_str) > 1) and (scr_str[0] != '#'):
                 # Save command to a temporary file first, then read script command
                 # This is the best solution. Aviods rewriting C code that I know works!
                 temporary_file_name = tempfile.mktemp('.scr')
                 fout = open(temporary_file_name, 'w')
                 fout.writelines( scr_str )
                 fout.close()
                 self._scriptrun(temporary_file_name)
                 os.remove(temporary_file_name)
        fin.close()

    #############################################################################
    #                                                                           #
    # Set default graphics method and template wrapper for VCS.                 #
    #                                                                           #
    #############################################################################
    def set(self, *args):
        """
 Function: set

 Description of Function:
    Set the default VCS primary class objects: template and graphics methods.
    Keep in mind the template, determines the appearance of each graphics segment;
    the graphic method specifies the display technique; and the data defines what
    is to be displayed. Note, the data cannot be set with this function.

 Example of Use:
    a=vcs.init()
    a.set('isofill','quick') # Changes the default graphics method to Isofill: 'quick'
    a.plot(array)
"""
        return apply(self.canvas.set, args)

    #############################################################################
    #                                                                           #
    # Touch all segements displayed on the VCS Canvas.                          #
    #                                                                           #
    #############################################################################
    def updateVCSsegments(self, *args):
        finish_queued_X_server_requests( self )
        self.canvas.BLOCK_X_SERVER()

        a = apply(self.canvas.updateVCSsegments, args)

        self.canvas.UNBLOCK_X_SERVER()
        return a

    #############################################################################
    #                                                                           #
    # Set VCS color map wrapper for VCS.                                        #
    #                                                                           #
    #############################################################################
    def setcolormap(self, *args):
        """
 Function: setcolormap

 Description of Function:
    It is necessary to change the colormap. This routine will change the VCS
    color map. 

    If the the visul display is 16-bit, 24-bit, or 32-bit TrueColor, then a redrawing
    of the VCS Canvas is made evertime the colormap is changed.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.setcolormap("AMIP")
"""
        # Don't update the VCS segment if there is no Canvas. This condition
        # happens in the initalize function for VCDAT only. This will cause a
        # core dump is not checked.
        try:
           updateVCSsegments_flag = args[1]
        except:
           updateVCSsegments_flag = 1
        a=apply(self.canvas.setcolormap, args)
        if updateVCSsegments_flag == 1:
           self.canvas.updateVCSsegments(self.mode) # pass down self and mode to _vcs module
        self.flush() # update the canvas by processing all the X events
        return a

    #############################################################################
    #                                                                           #
    # Set VCS color map cell wrapper for VCS.                                   #
    #                                                                           #
    #############################################################################
    def setcolorcell(self, *args):
        """
 Function: setcolorcell

 Description of Function:
    Set a individual color cell in the active colormap. If default is
    the active colormap, then return an error string.

    If the the visul display is 16-bit, 24-bit, or 32-bit TrueColor, then a redrawing
    of the VCS Canvas is made evertime the color cell is changed.

    Note, the user can only change color cells 0 through 239 and R,G,B
    value must range from 0 to 100. Where 0 represents no color intensity
    and 100 is the greatest color intensity.

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.setcolormap("AMIP")
    a.setcolorcell(11,0,0,0)
    a.setcolorcell(21,100,0,0)
    a.setcolorcell(31,0,100,0)
    a.setcolorcell(41,0,0,100)
    a.setcolorcell(51,100,100,100)
    a.setcolorcell(61,70,70,70)

"""

        a=apply(self.canvas.setcolorcell, args)
        self.canvas.updateVCSsegments(self.mode) # pass down self and mode to _vcs module
        self.flush() # update the canvas by processing all the X events
        return a

    #############################################################################
    #                                                                           #
    # Set continents type wrapper for VCS.                           		#
    #                                                                           #
    #############################################################################
    def setcontinentstype(self, *args):
        """
 Function: setcontinentstype

 Description of Function:
    One has the option of using continental maps that are predefined or that
    are user-defined. Predefined continental maps are either internal to VCS
    or are specified by external files. User-defined continental maps are
    specified by additional external files that must be read as input. 

    The continents-type values are integers ranging from 0 to 11, where: 
        0 signifies "No Continents" 
        1 signifies "Fine Continents" 
        2 signifies "Coarse Continents" 
        3 signifies "United States" (with "Fine Continents") 
        4 signifies "Political Borders" (with "Fine Continents") 
        5 signifies "Rivers" (with "Fine Continents") 

    Values 6 through 11 signify the line type defined by the files
    data_continent_other7 through data_continent_other12. 

 Example of Use:
    a=vcs.init()
    a.setcontinentstype(3)
    a.plot(array,'default','isofill','quick')
"""
        return apply(self.canvas.setcontinentstype, args)

    #############################################################################
    #                                                                           #
    # Screen GIF wrapper for VCS.                                               #
    #                                                                           #
    #############################################################################
    def gif(self, filename='noname.gif', merge='r', orientation=None, geometry='1600x1200'):
        """
 Function: gif

 Description of Function:
    In some cases, the user may want to save the plot out as a gif image. This
    routine allows the user to save the VCS canvas output as a SUN gif file.
    This file can be converted to other gif formats with the aid of xv and other
    such imaging tools found freely on the web.

    If no path/file name is given and no previously created gif file has been
    designated, then file

        /$HOME/%s/default.gif

    will be used for storing gif images. However, if a previously created gif 
    file is designated, that file will be used for gif output.

    By default, the page orientation is in Landscape mode (l). To translate the page
    orientation to portrait mode (p), set the orientation = 'p'.

    The GIF command is used to create or append to a gif file. There are two modes
    for saving a gif file: `Append' mode (a) appends gif output to an existing gif
    file; `Replace' (r) mode overwrites an existing gif file with new gif output. 
    The default mode is to overwrite an existing gif file (i.e. mode (r)).

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.gif(filename='example.gif', merge='a', orientation='l', geometry='800x600')
    a.gif('example')         # overwrite existing gif file (default is merge='r')
    a.gif('example',merge='r')  # overwrite existing gif file
    a.gif('example',merge='a')     # merge gif image into existing gif file
    a.gif('example',orientation='l') # merge gif image into existing gif file with landscape orientation
    a.gif('example',orientation='p') # merge gif image into existing gif file with portrait orientation
    a.gif('example',geometry='600x500') # merge gif image into existing gif file and set the gif geometry
""" % (self._dotdir)
        if orientation is None:
            orientation=self.orientation()[0]
        g = string.split(geometry,'x')
        f1 = f1=string.atof(g[0]) / 1100.0 * 100.0
        f2 = f2=string.atof(g[1]) / 849.85 * 100.0
        geometry = "%4.1fx%4.1f" % (f2,f1)
        nargs = ('gif', filename, merge, orientation, geometry)
        return apply(self.canvas.gif_or_eps, nargs)

    #############################################################################
    #                                                                           #
    # Screen GhostScript (gs) wrapper for VCS.                                  #
    #                                                                           #
    #############################################################################
    def gs(self, filename='noname.gs', device='png256', orientation=None, resolution='792x612'):
        """
 Function: gs

 Description of Function:
    This routine allows the user to save the VCS canvas in one of the many
    GhostScript (gs) file types (also known as devices). To view other 
    GhostScript devices, issue the command "gs --help" at the terminal 
    prompt. Device names include: bmp256, epswrite, jpeg, jpeggray, 
    pdfwrite, png256, png16m, sgirgb, tiffpack, and tifflzw. By default 
    the device = 'png256'.

    If no path/file name is given and no previously created gs file has been
    designated, then file

        /$HOME/%s/default.gs

    will be used for storing gs images. However, if a previously created gs 
    file exist, then this output file will be used for storage.

    By default, the page orientation is the canvas' orientation.
    To translate the page orientation to portrait mode (p), set the parameter orientation = 'p'.
    To translate the page orientation to landscape mode (l), set the parameter orientation = 'l'.

    The gs command is used to create a single gs file at this point. The user
    can use other tools to append separate image files.

 Example of Use:
    a=vcs.init()
    a.plot(array)
    a.gs('example') #defaults: device='png256', orientation='l' and resolution='792x612'
    a.gs(filename='example.tif', device='tiffpack', orientation='l', resolution='800x600')
    a.gs(filename='example.pdf', device='pdfwrite', orientation='l', resolution='200x200')
    a.gs(filename='example.jpg', device='jpeg', orientation='p', resolution='1000x1000')
""" % (self._dotdir)
        if orientation is None:
            orientation=self.orientation()[0]
        r = string.split(resolution,'x')
        f1 = f1=string.atof(r[0]) / 1100.0 * 100.0
        f2 = f2=string.atof(r[1]) / 849.85 * 100.0
        resolution = "%4.1fx%4.1f" % (f2,f1)
        nargs = (filename, device, orientation, resolution)
        return apply(self.canvas.gs, nargs)

    #############################################################################
    #                                                                           #
    # Screen Encapsulated PostScript wrapper for VCS.                           #
    #                                                                           #
    #############################################################################
    def eps(self, file, mode='r',orientation=None,width=None,height=None,units='inches',left_margin=None,right_margin=None,top_margin=None,bottom_margin=None):
        """
        Function: Encapsulated PostScript
        
        Description of Function:
        In some cases, the user may want to save the plot out as an Encapsulated
        PostScript image. This routine allows the user to save the VCS canvas output
        as an Encapsulated PostScript file.
        This file can be converted to other image formats with the aid of xv and other
        such imaging tools found freely on the web.
        
        
        Example of Use:
        a=vcs.init()
        a.plot(array)
        a.postscript('example')       # Overwrite a postscript file
        a.postscript('example', 'a')  # Append postscript to an existing file
        a.postscript('example', 'r')  # Overwrite an existing file
        a.postscript('example', mode='a')  # Append postscript to an existing file
        a.postscript('example', width=11.5, height= 8.5)  # US Legal (default)
        a.postscript('example', width=21, height=29.7, units='cm')  # A4
        a.postscript('example', right_margin=.2,left_margin=.2,top_margin=.2,bottom_margin=.2)  # US Legal output and control of margins (for printer friendly output), default units 'inches'
        """
        ext = file.split(".")[-1]
        if ext.lower()!='eps':
            file=file+'.eps'
        num = numpy.random.randint(10000000)
        tmpfile = "/tmp/vcs_tmp_eps_file_%i.ps" % num
        if mode=='a' and os.path.exists(file):
            os.rename(file,tmpfile)
        self.postscript(tmpfile,mode,orientation,width,height,units,left_margin,right_margin,top_margin,bottom_margin)
        os.popen("ps2epsi %s %s" % ( tmpfile, file)).readlines()
        os.remove(tmpfile)
                 
    #############################################################################
    #                                                                           #
    # Show VCS primary and secondary elements wrapper for VCS.                  #
    #                                                                           #
    #############################################################################
    def show(self, *args):
        """
 Function: show

 Description of Function:
    Show the list of VCS primary and secondary class objects.

 Example of Use:
    a=vcs.init()
    a.show('boxfill')
    a.show('isofill')
    a.show('line')
    a.show('marker')
    a.show('text')
"""
        if args != () and string.lower(args[0]) == 'taylordiagram':
            ln=[]
            ln.append('*******************Taylor Diagrams Names List**********************')
            nms=[]
            i=0
            ln.append('')
            for t in vcs.taylordiagrams:
                if i%3==0 :
                   ln[-1]=ln[-1]+'(%4s):' % str(i+1)
                ln[-1]=ln[-1]+'%20s' % t.name
                i=i+1
                if i%3==0 : ln.append('')
            if ln[-1]=='' : ln.pop(-1)
            ln.append('*****************End Taylor Diagrams Names List********************')
            for l in ln:
                print l
            return None
        elif args == ():
           return self.listelements()
        else:
            return apply(self.canvas.show, args)

    #############################################################################
    #                                                                           #
    # Look if a graphic method is in a file           .                         #
    #                                                                           #
    #############################################################################
    def isinfile(self,GM,file=None):
        """ Checks if a graphic method is stored in a file
        if no file name is passed then looks into the initial.attribute file"""
        nm=GM.name
        gm=GM.g_name
        key=gm+'_'+nm+'('
        if file is None:
            file=os.path.join(os.environ['HOME'],self._dotdir,'initial.attributes') 
        f=open(file,'r')
        for ln in f.xreadlines():
            if string.find(ln,key)>-1:
                f.close()
                return 1
        return 0
    #############################################################################
    #                                                                           #
    # Save VCS initial.attribute file  wrapper for VCS.                         #
    #                                                                           #
    #############################################################################
    def saveinitialfile(self):
        """
 Function: saveinitialfile                      # Save initial.attribute file

 Description of Function:
    At start-up, VCS reads a script file named initial.attributes that
    defines the initial appearance of the VCS Interface. Although not
    required to run VCS, this initial.attributes file contains many
    predefined settings to aid the beginning user of VCS. The path to
    the file must be:

         /$HOME/%s/initial.attributes

    The contents of the initial.attributes file can be customized by
    the user.

 Example of Use:
    a=vcs.init()
    ...

    a.saveinitialfile()

 WARNING: This removes first ALL object genrated automatically (i.e. whose name starts with '__') in order to preserve this, rename objects first
 e.g:
    b=a.createboxfill()
    b.name='MyBoxfill'

 # graphic method is now preserved
""" % (self._dotdir)
        self.clean_auto_generated_objects()
        msg = _vcs.saveinitialfile()
        # Now adds the taylordiagram stuff
        fnm=os.path.join(os.environ['HOME'],self._dotdir,'initial.attributes')
        for td in vcs.taylordiagrams:
            if self.isinfile(td)==0 : td.script(fnm)
##         # Now adds the meshfill stuff
##         for mesh in vcs.meshfills:
##             if self.isinfile(mesh)==0 : mesh.script(fnm)
        return msg
        

    #############################################################################
    #                                                                           #
    # Script to a file the current state of VCS wrapper for VCS.                #
    #                                                                           #
    #############################################################################
    def scriptstate(self,script_name):
        """
 Function: scriptstate       # Save state of VCS

 Description of Function:
    The VCS scripting capability serves many purposes. It allows one to save the
    system state for replay in a later session; to save primary and secondary 
    element attributes for use in later visual presentations; to save a sequence 
    of interactive operations for replay; or to recover from a system failure.

 Example of Use:
    a=vcs.init()
    ...

    a.scriptstate(script_filename)
"""
        msg = _vcs.scriptstate(script_name)
        # Now adds the taylordiagram stuff
        for td in vcs.taylordiagrams:
            td.script(script_name)
        return msg

    #############################################################################
    #                                                                           #
    # Raise VCS Canvas to the top of all its siblings.                          #
    #                                                                           #
    #############################################################################
    def canvasraised(self, *args):
        """
 Function: canvasraised                         # Raise the VCS Canvas to the top

 Description of Function:
    This function marks a VCS Canvas as eligible to be displayed and  
    positions the window at the top of the stack of its siblings.

 Example of Use:
    a=vcs.init()
    ...

    a.canvasraised()
"""

        return  apply(self.canvas.canvasraised, args)

    #############################################################################
    #                                                                           #
    # Returns 1 if a VCS Canvas is displayed on the screen. Returns a 0 if no   #
    # VCS Canvas is displayed on the screen.                                    #
    #                                                                           #
    #############################################################################
    def iscanvasdisplayed(self, *args):
        """
 Function: iscanvasdisplayed          # Return 1 if a VCS Canvas is displayed

 Description of Function:
    This function returns a 1 if a VCS Canvas is displayed or a 0 if  
    no VCS Canvas is displayed on the screen.
    
 Example of Use:
    a=vcs.init()
    ...
    
    a.iscanvasdisplayed()
"""

        return  apply(self.canvas.iscanvasdisplayed, args)

    #############################################################################
    #                                                                           #
    # Is VCS's orientation landscape?                                           #
    #                                                                           #
    #############################################################################
    def islandscape(self):
        """
 Function: islandscape

 Description of Function:
    Indicates if VCS's orientation is landscape.

    Returns a 1 if orientation is landscape.
    Otherwise, it will return a 0, indicating false (not in landscape mode).

 Example of Use:
    a=vcs.init()
    ...

    if a.islandscape():
       a.portrait()               # Set VCS's orientation to portrait mode
"""
        if (self.orientation() == 'landscape'):
            return 1
        else:
            return 0

    #############################################################################
    #                                                                           #
    # Is VCS's orientation portrait?                                            #
    #                                                                           #
    #############################################################################
    def isportrait(self):
        """
 Function: isportrait

 Description of Function:
    Indicates if VCS's orientation is portrait.

    Returns a 1 if orientation is portrait.
    Otherwise, it will return a 0, indicating false (not in portrait mode).

 Example of Use:
    a=vcs.init()
    ...

    if a.isportrait():
       a.landscape()               # Set VCS's orientation to landscape mode
"""
        if (self.orientation() == 'portrait'):
            return 1
        else:
            return 0
    #############################################################################
    #                                                                           #
    # Dislplay plot functions for VCS.                                          #
    #                                                                           #
    #############################################################################
    def getplot(self, Dp_name_src='default', template=None):
        """
 Function: getplot                  # Get existing display plot

 Description of Function:
    This function will create a display plot object from an existing display
    plot object from an existing VCS plot. If no display plot name
    is given, then None is returned.

 Example of Use:
    a=vcs.init()
    a.show('template')                  # Show all the existing templates
    plot1=a.getplot('dpy_plot_1')       # plot1 instance of 'dpy_plot_1' display plot
"""
        if not isinstance(Dp_name_src,str):
           raise ValueError, 'Error -  The argument must be a string.'

        Dp_name = None
        display = displayplot.Dp(self, Dp_name, Dp_name_src, 1)
        if template is not None:
            display._template_origin = template
        return display

    #############################################################################
    #                                                                           #
    # Colormap functions for VCS.                                               #
    #                                                                           #
    #############################################################################
    def createcolormap(self,Cp_name=None, Cp_name_src='default'):
        """
 Function: createcolormap               # Construct a new colormap secondary method

 Description of Function:
    Create a new colormap secondary method given the the name and the existing
    colormap secondary method to copy the attributes from. If no existing colormap
    secondary method name is given, then the default colormap secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    If the name provided already exists, then a error will be returned.
    Secondary method names must be unique.

 Example of Use:
    a=vcs.init()
    cp=a.createcolormap('example1',)
    a.show('colormap')
    cp=a.createcolormap('example2','AMIP')
    a.show('colormap')

"""
        # Check to make sure the arguments passed in are STRINGS
        if (type(Cp_name) != StringType):
           raise ValueError, 'Error -  The first argument must be a string.'
        if (type(Cp_name_src) != StringType):
           raise ValueError, 'Error -  The second argument must be a string.'

        return colormap.Cp(self, Cp_name, Cp_name_src, 0)

    def getcolormap(self,Cp_name_src='default'):
        """
 Function: getcolormap                      # Construct a new colormap secondary method

 Description of Function:
    VCS contains a list of secondary methods. This function will create a
    colormap class object from an existing VCS colormap secondary method. If
    no colormap name is given, then colormap 'default' will be used.

    Note, VCS does not allow the modification of `default' attribute sets.
    However, a `default' attribute set that has been copied under a
    different name can be modified. (See the createcolormap function.)

 Example of Use:
    a=vcs.init()
    a.show('colormap')                      # Show all the existing colormap secondary methods
    cp=a.getcolormap()                      # cp instance of 'default' colormap secondary
                                            #       method
    cp2=a.getcolormap('quick')              # cp2 instance of existing 'quick' colormap
                                            #       secondary method
"""
        # Check to make sure the argument passed in is a STRING
        if (type(Cp_name_src) != StringType):
           raise ValueError, 'Error -  The argument must be a string.'

        Cp_name = None
        return colormap.Cp(self, Cp_name, Cp_name_src, 1)

    #############################################################################
    #                                                                           #
    # Font functions.                       #
    #                                                                           #
    #############################################################################
    def addfont(self, path,name=""):
        """
        Add a font to VCS, path then a name you'd like to associate it with
        """
        if not os.path.exists(path):
            raise ValueError, 'Error -  The font path does not exists'
        if os.path.isdir(path):
            dir_files=[]
            files=[]
            if name=="":
                subfiles = os.listdir(path)
                for file in subfiles:
                    dir_files.append(os.path.join(path,file))
            elif name=='r':
                for root,dirs,subfiles in os.walk(path):
                    for file in subfiles:
                        dir_files.append(os.path.join(root,file))
            for f in dir_files:
                if f.lower()[-3:]in ['ttf','pfa','pfb']:
                    files.append([f,""])
        else:
            files=[[path,name],]

        nms = []
        for f in files:
            fnm,name = f
            nms.append(apply(self.canvas.addfont,(fnm,name)))
        if len(nms)==0:
            raise vcsError,'No font Loaded'
        elif len(nms)>1:
            return nms
        else:
            return nms[0]
    

    def getfontnumber(self, name):
        """
        get the font number associated with a font name
        """
        nb = apply(self.canvas.getfontnumber,(name,))
        if nb==-1:
            raise vcsError,"Font name not existing! %s" % name
        return nb
    
    def getfontname(self, number):
        """
        get the font name associated with a font number
        """
        nm = apply(self.canvas.getfontname,(number,))
        if nm=="":
            raise vcsError,"Error font number not existing %i" % number
        return nm
    
    def getfont(self, font):
        """
        get the font name/number associated with a font number/name
        """
        if isinstance(font,int):
            return self.getfontname(font)
        elif isinstance(font,str):
            return self.getfontnumber(font)
        else:
            raise vcsError,"Error you must pass a string or int"
        
    def switchfonts(self,font1,font2):
        """ Switch 2 font indexes, you can pass either the font names or indexes """
        if isinstance(font1,str):
            index1 = self.getfont(font1)
        elif isinstance(font1,(int,float)):
            index1 = int(font1)
            nm = self.getfont(index1) # make sure font exists
        else:
            raise vcsError,"Error you must pass either a number or font name!, you passed for font 1: %s" % font1
        if isinstance(font2,str):
            index2 = self.getfont(font2)
        elif isinstance(font2,(int,float)):
            index2 = int(font2)
            nm = self.getfont(index2) # make sure font exists
        else:
            raise vcsError,"Error you must pass either a number or font name!, you passed for font 2: %s" % font2
        
        return apply(self.canvas.switchfontnumbers,(index1,index2))
    
    def copyfontto(self,font1,font2):
        """ copy name and path of font 1 into font 2, you can pass either the font names or indexes """
        if isinstance(font1,str):
            index1 = self.getfont(font1)
        elif isinstance(font1,(int,float)):
            index1 = int(font1)
            nm = self.getfont(index1) # make sure font exists
        else:
            raise vcsError,"Error you must pass either a number or font name!, you passed for font 1: %s" % font1
        if isinstance(font2,str):
            index2 = self.getfont(font2)
        elif isinstance(font2,(int,float)):
            index2 = int(font2)
            nm = self.getfont(index2) # make sure font exists
        else:
            raise vcsError,"Error you must pass either a number or font name!, you passed for font 2: %s" % font2
        return apply(self.canvas.copyfontto,(index1,index2))
    
    def setdefaultfont(self,font):
        """Sets the passed font as the default font for vcs"""
        if isinstance(font,str):
            font = self.getfont(font)
        return self.copyfontto(font,1)
 
    #############################################################################
    #                                                                           #
    # Orientation VCS Canvas orientation wrapper for VCS.                       #
    #                                                                           #
    #############################################################################
    def orientation(self, *args):
        """
 Function: orientation

 Description of Function:
    Return VCS's orientation. Will return either Portrait or Landscape.

 Example of Use:
    a=vcs.init()
    a.orientation()      # Return either "landscape" or "portrait"
""" 
        return apply(self.canvas.orientation, args)

    #############################################################################
    #                                                                           #
    # Get VCS color map cell wrapper for VCS.                                   #
    #                                                                           #
    #############################################################################
    def getcolorcell(self, *args):
        """
 Function: getcolorcell
 
 Description of Function:
    Get an individual color cell in the active colormap. If default is
    the active colormap, then return an error string.

    If the the visul display is 16-bit, 24-bit, or 32-bit TrueColor, then a redrawing
    of the VCS Canvas is made evertime the color cell is changed.

    Note, the user can only change color cells 0 through 239 and R,G,B
    value must range from 0 to 100. Where 0 represents no color intensity
    and 100 is the greatest color intensity.
 
 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.setcolormap("AMIP")
    a.getcolorcell(11,0,0,0)
    a.getcolorcell(21,100,0,0)
    a.getcolorcell(31,0,100,0)
    a.getcolorcell(41,0,0,100)
    a.getcolorcell(51,100,100,100)
    a.getcolorcell(61,70,70,70)

"""
        a=apply(self.canvas.getcolorcell, args)
        return a

    #############################################################################
    #                                                                           #
    # Get VCS color map name wrapper for VCS.                                   #
    #                                                                           #
    #############################################################################
    def getcolormapname(self, *args):
        """
 Function: getcolormapcell

 Description of Function:
    Get colormap name of the active colormap.


 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.getcolormapname()
"""
        a=apply(self.canvas.getcolormapname, args)
        return a
    
    def dummy_user_action(self,*args,**kargs):
##         print 'In dummy baby!'
        print 'Arguments:',args
        print 'Keywords:',kargs
        return None
    
#############################################################################
#                                                                           #
# Primarily used for reseting the animation date and time string.           #
#                                                                           #
#############################################################################
def change_date_time(tv, number):
    timeaxis = tv.getTime()
    if timeaxis is not None:
        try:
            tobj = cdtime.reltime(timeaxis[number], timeaxis.units)
            cobj = tobj.tocomp(timeaxis.getCalendar())
            tv.date = '%s/%s/%s\0'%(cobj.year, cobj.month, cobj.day)
            tv.time = '%s:%s:%s\0'%(cobj.hour, cobj.minute, cobj.second)
        except:
            pass

#############################################################################
#                                                                           #
# Animate wrapper for VCS.                                                  #
#                                                                           #
#############################################################################
class animate_obj_old:
   """
 Function: animate

 Description of Function:
    Animate the contents of the VCS Canvas. The animation can also be controlled from
    the animation GUI. (See VCDAT for more details.)
 
    See the animation GUI documenation located at URL:
        http://www-pcmdi.llnl.gov/software/vcs

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.animate()

"""
    
   ##############################################################################
   # Initialize the animation flags						#
   ##############################################################################
   def __init__(self, vcs_self):
      self.vcs_self = vcs_self
      self.gui_popup = 0
      self.create_flg = 0
      self.run_flg = 0
      self.continents_value = 0
      self.continents_hold_value = 1
      
   ##############################################################################
   # Create the animation images. If min or max is None, then			#
   # the animator will find the min and max values from the dataset.		#
   # If min and max are set to 1e20, then no min and max animation		#
   # value is used (i.e., each animation frame will have different		#
   # min and max values. If min and max are set by the user, then		#
   # these values are used for the animation min and max.			#
   #										#
   # If you are running animation from a program, set thread_it to 0.		#
   # This will cause the Python program to wait for the create function		#
   # to finish before moving onto the next command line.			#
   ##############################################################################
   def create( self, parent=None, min=None, max=None, save_file=None, thread_it = 1, rate=None, bitrate=None, ffmpegoptions='' ):
      from vcs import minmax
      from numpy.ma import maximum,minimum
      ##from tkMessageBox import showerror

      # Cannot "Run" or "Create" an animation while already creating an animation
      if self.run_flg == 1: return
      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.vcs_self.animate_info == []:
         str = "No data found!"
         showerror( "Error Message to User", str )
         return
      finish_queued_X_server_requests( self.vcs_self )
      self.vcs_self.canvas.BLOCK_X_SERVER()

      # Stop the (thread) execution of the X main loop (if it is running).
      self.vcs_self.canvas.stopxmainloop( )

      # Force VCS to update its orientation, needed when the user changes the
      # VCS Canvas size.
      self.vcs_self.canvas.updateorientation()

      # Make sure the animate information is up-to-date for creating images
      if ((self.gui_popup == 1) and (self.create_flg == 0)):
         self.update_animate_display_list( )

      # Save the min and max values for the graphics methods.
      # Will need to restore values back when animation is done.
      self.save_original_min_max()

      # Set up the animation min and max values by changing the graphics method
      # Note: cannot set the min and max values if the default graphics method is set.
      do_min_max = 'yes'
      try:
         if (parent is not None) and (parent.iso_spacing == 'Log'):
            do_min_max = 'no'
      except:
         pass

      # Draw specified continental outlines if needed.
      self.continents_hold_value = self.vcs_self.canvas.getcontinentstype( )
      self.vcs_self.canvas.setcontinentstype( self.continents_value )

      if ( do_min_max == 'yes' ):
         minv = []
         maxv=[]
         if (min is None) or (max is None):
            for i in range(len(self.vcs_self.animate_info)):
               minv.append( 1.0e77 )
               maxv.append( -1.0e77 )
            for i in range(len(self.vcs_self.animate_info)):
               dpy, slab = self.vcs_self.animate_info[i]
               mins, maxs = minmax(slab)
               minv[i] = float(minimum(float(minv[i]), float(mins)))
               maxv[i] = float(maximum(float(maxv[i]), float(maxs)))
         if ((type(min) == types.ListType) or (type(max) == types.ListType)):
            for i in range(len(self.vcs_self.animate_info)):
               try:
                  minv.append( min[i] )
               except:
                  minv.append( min[-1] )
               try:
                  maxv.append( max[i] )
               except:
                  maxv.append( max[-1] )
         else:
            for i in range(len(self.vcs_self.animate_info)):
                minv.append( min )
                maxv.append( max )

         # Set the min an max for each plot in the page. If the same graphics method is used
         # to display the plots, then the last min and max setting of the data set will be used.
         for i in range(len(self.vcs_self.animate_info)):
            try:
               self.set_animation_min_max( minv[i], maxv[i], i )
            except Exception,err:
               pass # if it is default, then you cannot set the min and max, so pass.

      if save_file is None or save_file.split('.')[-1].lower()=='ras':
          if thread_it == 1:
              thread.start_new_thread( self.vcs_self.canvas.animate_init, (save_file,) )
              ## from cdatguiwrap import VCSQtManager
              ## w = VCSQtManager.window(0)
              #self.mythread=QAnimThread(None,self.vcs_self.canvas.animate_init,save_file)
              #self.mythread.start()
          else:
              self.vcs_self.canvas.animate_init( save_file )
      else: # ffmpeg stuff
          save_info = self.vcs_self.animate_info
          animation_info = self.animate_info_from_python()
          slabs=[]
          templates=[]
          dpys=[]
          for i in range(len(self.vcs_self.animate_info)):
              dpy, slab = self.vcs_self.animate_info[i]
              slabs.append(slab)
              dpys.append(dpy)
              templates.append(dpy.template)
          sh =slabs[0].shape
          if dpy.g_type in ['boxfill', 'isofill', 'isoline', 'meshfill', 'outfill', 'outline', 'taylordiagram', 'vector', ]:
              r=len(sh)-2
          else:
              r=len(sh)-1
          # now create the list of all previous indices to plot
          indices=[]
          for i in range(r):
              this = list(range(sh[i]))
              tmp=[]
              if indices == []:
                  for k in this:
                      indices.append([k,])
              else:
                  for j in range(len(indices)):
                      for k in this:
                          tmp2=copy.copy(indices[j])
                          tmp2.append(k)
                          tmp.append(tmp2)
                  indices=tmp
          count=1
          white_square=self.vcs_self.createfillarea()
          white_square.color=240
          white_square.x=[0,1,1,0]
          white_square.y=[0,0,1,1]
          new_vcs=vcs.init()
          if self.vcs_self.orientation()=='portrait':
              new_vcs.portrait()
          #self.vcs_self.close()

          d = Pmw.Dialog(title="Creating Frames")
          d.geometry("200x150+0+0")
          S=genutil.Statusbar(d.interior(),ycounter=50)
          S.pack(expand=1,fill='both')
          n=float(len(indices))/100.
          for index in indices:
              S.show(count/n)
              new_vcs.clear()
              new_vcs.plot(white_square,bg=1)
              for i in range(len(save_info)):
                  slab=slabs[i]
                  template=templates[i]
                  gtype = string.lower(animation_info["gtype"][i])
                  gname = animation_info["gname"][i]
                  exec("gm = new_vcs.get%s('%s')" % (gtype,gname))
                  for j in index:
                      slab=slab[j]
                  new_vcs.plot(slab,gm,new_vcs.gettemplate(template),bg=1)
              new_vcs.png("tmp_anim_%i" % count)
              count+=1
          new_vcs.ffmpeg(save_file,"tmp_anim_%d.png",bitrate=bitrate,rate=rate,options=ffmpegoptions)
          for i in range(count-1):
              os.remove("tmp_anim_%i.png" % (i+1))
          d.destroy()
          del(new_vcs)
      self.create_flg = 1

      self.vcs_self.canvas.UNBLOCK_X_SERVER()

   def animate_info_from_python(self):
       gtype = []
       gname = []
       tmpl = []
       for i in self.vcs_self.animate_info:
            d=i[0]
            tmpl.append(d.template)
            gtype.append(d.g_type)
            gname.append(d.g_name)
       return {"template":tmpl,"gtype":gtype,"gname":gname}

   ##############################################################################
   # Save original min and max values    					#
   ##############################################################################
   def save_original_min_max( self ):
      animation_info = self.animate_info_from_python()
      self.save_min = {}
      self.save_max = {}
      self.save_legend = {}
      self.save_levels = {}
      self.save_mean_veloc = {}
      for i in range(len(self.vcs_self.animate_info)):
         gtype = string.lower(animation_info["gtype"][i])
         if gtype == "boxfill":
            gm=self.vcs_self.getboxfill(animation_info['gname'][i])
            self.save_min[i] = gm.level_1
            self.save_max[i] = gm.level_2
#            self.save_legend[i] = gm.legend
         elif ( gtype == "meshfill" ):
            gm=self.vcs_self.getmeshfill(animation_info['gname'][i])
            self.save_levels[i] = gm.levels
         elif ( gtype == "isofill" ):
            gm=self.vcs_self.getisofill(animation_info['gname'][i])
            self.save_levels[i] = gm.levels
         elif ( gtype == "isoline" ):
            gm=self.vcs_self.getisoline(animation_info['gname'][i])
            self.save_levels[i] = gm.levels
         elif ( gtype == "yxvsx" ):
            gm=self.vcs_self.getyxvsx(animation_info['gname'][i])
            self.save_min[i] = gm.datawc_y1
            self.save_max[i] = gm.datawc_y2
         elif ( gtype == "xyvsy" ):
            gm=self.vcs_self.getxyvsy(animation_info['gname'][i])
            self.save_min[i] = gm.datawc_x1
            self.save_max[i] = gm.datawc_x2
         elif ( gtype == "vector" ):
            gm=self.vcs_self.getvector(animation_info['gname'][i])
            self.save_mean_veloc[i] = gm.reference

   ##############################################################################
   # Restore min and max values                                                 #
   ##############################################################################
   def restore_min_max( self ):
      animation_info = self.animate_info_from_python()
      try:
       for i in range(len(self.vcs_self.animate_info)):
         gtype = string.lower(animation_info["gtype"][i])
         if gtype == "boxfill":
            gm=self.vcs_self.getboxfill(animation_info['gname'][i])
            gm.level_1 = self.save_min[i]
            gm.level_2 = self.save_max[i]
#            gm.legend = self.save_legend[i]
         elif ( gtype == "meshfill" ):
            gm=self.vcs_self.getmeshfill(animation_info['gname'][i])
            gm.levels = self.save_levels[i]
         elif ( gtype == "isofill" ):
            gm=self.vcs_self.getisofill(animation_info['gname'][i])
            gm.levels = self.save_levels[i]
         elif ( gtype == "isoline" ):
            gm=self.vcs_self.getisoline(animation_info['gname'][i])
            gm.levels = self.save_levels[i]
         elif ( gtype == "yxvsx" ):
            gm=self.vcs_self.getyxvsx(animation_info['gname'][i])
            gm.datawc_y1 = self.save_min[i]
            gm.datawc_y2 = self.save_max[i]
         elif ( gtype == "xyvsy" ):
            gm=self.vcs_self.getxyvsy(animation_info['gname'][i])
            gm.datawc_x1 = self.save_min[i]
            gm.datawc_x2 = self.save_max[i]
         elif ( gtype == "vector" ):
            gm=self.vcs_self.getvector(animation_info['gname'][i])
            gm.reference = self.save_mean_veloc[i]
      except:
          pass
   
   ##############################################################################
   # Set the animation min and max values    					#
   ##############################################################################
   def set_animation_min_max( self, min, max, i ):
      from vcs import mkscale, mklabels, getcolors
      animation_info = self.animate_info_from_python()
      gtype = string.lower(animation_info["gtype"][i])
      levs = mkscale(min,max)
      dic = mklabels(levs)
      cols = getcolors(levs)
      if gtype == "boxfill":
         gm=self.vcs_self.getboxfill(animation_info['gname'][i])
         if gm.boxfill_type == 'custom':
             gm.fillareacolors = cols
             gm.levels = levs
         else:
             gm.level_1=levs[0]
             gm.level_2=levs[-1]
             gm.legend=None
      elif ( gtype == "meshfill" ):
         gm=self.vcs_self.getmeshfill(animation_info['gname'][i])
         if (min == 1e20) and (max ==1e20):
            gm.levels=(1e20,1e20)
         else:
            gm.levels = levs
            gm.fillareacolors = cols
      elif ( gtype == "isofill" ):
         gm=self.vcs_self.getisofill(animation_info['gname'][i])
         if (min == 1e20) and (max ==1e20):
            gm.levels=(1e20,1e20)
         else:
            gm.levels = levs
            gm.fillareacolors = cols
      elif ( gtype == "isoline" ):
         gm=self.vcs_self.getisoline(animation_info['gname'][i])
         if (min == 1e20) and (max ==1e20):
            gm.levels=(1e20,1e20)
         else:
            gm.levels = levs
            gm.fillareacolors = cols
      elif ( gtype == "yxvsx" ):
         gm=self.vcs_self.getyxvsx(animation_info['gname'][i])
         if (min != 1e20) and (max !=1e20):
            gm.yticlabels1=dic
            gm.yticlabels2=dic
            min = levs[0]
            max = levs[-1]
         gm.datawc_y1 = min
         gm.datawc_y2 = max
      elif ( gtype == "xyvsy" ):
         gm=self.vcs_self.getxyvsy(animation_info['gname'][i])
         if (min != 1e20) and (max !=1e20):
            gm.xticlabels1=dic
            gm.xticlabels2=dic
            min = levs[0]
            max = levs[-1]
         gm.datawc_x1 = min
         gm.datawc_x2 = max
      elif ( gtype == "vector" ):
         gm=self.vcs_self.getvector(animation_info['gname'][i])
         mean_veloc = 1e20
         if (min != 1e20) and (max !=1e20):
            mean_veloc = float( int( numpy.sqrt( (min**2)+(max**2) ) ) )
         gm.reference = mean_veloc
      animation_info['gname'][i] = gm.name

   ##############################################################################
   # Return the animation min and max values                                    #
   ##############################################################################
   def return_animation_min_max( self ):
      dpy, slab = self.vcs_self.animate_info[0]
      return vcs.minmax(slab)

   ##############################################################################
   # Load animation from a stored Raster file.   				#
   ##############################################################################
   def load_from_file( self, parent=None, load_file=None, thread_it = 1 ):
      ##from tkMessageBox import showerror
      if os.access(load_file, os.R_OK) == 0:
         showerror( "Error Message to the User", "The specfied file does not have read permission or does not exist. Please check the availability of the file.")
         return

      finish_queued_X_server_requests( self.vcs_self )
      self.vcs_self.canvas.BLOCK_X_SERVER()

      # Stop the (thread) execution of the X main loop (if it is running).
      self.vcs_self.canvas.stopxmainloop( )

      if thread_it == 1:
          thread.start_new_thread( self.vcs_self.canvas.animate_load, (load_file,) )
      else:
          self.vcs_self.canvas.animate_init( load_file )
      self.create_flg = 1

      self.vcs_self.canvas.UNBLOCK_X_SERVER()

   ##############################################################################
   # Creating animation flag                 					#
   ##############################################################################
   def creating_animation_flg( self ):
      return self.vcs_self.canvas.creating_animation()

   ##############################################################################
   # Run animation flag                 					#
   ##############################################################################
   def run_animation_flg( self ):
      return self.run_flg

   ##############################################################################
   # Run or start the animation              					#
   ##############################################################################
   def run( self ):
      # Cannot "Create" an animation while running an animation.
      if self.vcs_self.canvas.creating_animation() == 1: return

      if ((self.create_flg == 1) and (self.run_flg == 0)):
         self.run_flg = 1
         #thread.start_new_thread( self.vcs_self.canvas.animate_run,( ) )
         self.vcs_self.canvas.animate_run()

   ##############################################################################
   # Stop the animation creation                                                #
   ##############################################################################
   def stop_create( self ):
      if (self.create_flg == 1):
         self.vcs_self.canvas.animate_stop_create()

   ##############################################################################
   # Stop the animation                                 			#
   ##############################################################################
   def stop( self ):
      if (self.create_flg == 1) and (self.run_flg == 1):
         self.run_flg = 0
         self.vcs_self.canvas.animate_stop()
      elif (self.create_flg == 1):
         self.vcs_self.canvas.animate_stop_create()
	
   ##############################################################################
   # View the specified animation frame                          		#
   ##############################################################################
   def frame( self, value=1 ):
      if (self.create_flg == 1) and (self.run_flg == 0):
         self.vcs_self.canvas.animate_frame( value )

   ##############################################################################
   # Return the number of animate frames                                    	#
   ##############################################################################
   def number_of_frames( self ):
      if self.create_flg == 1:
         return self.vcs_self.canvas.animate_number_of_frames( )

   ##############################################################################
   # Pause the animation loop                                               	#
   # Value ranges from 0 to 100                                                 #
   ##############################################################################
   def pause( self, value=1 ):
      if (((not isinstance(value, types.IntType))) or (value not in range(0, 101))):
         raise vcsError, "Pause value must be between an integer between 0 and 100."

      if (self.create_flg == 1) and (self.run_flg == 1):
         self.vcs_self.canvas.animate_pause( value )

   ##############################################################################
   # Zoom in on the animation                                               	#
   # Value ranges from 0 to 20                                                  #
   ##############################################################################
   def zoom( self, value=1 ):
      if (((not isinstance(value, types.IntType))) or (value not in range(1, 21))):
         raise vcsError, "Zoom value must be between an integer between 1 and 20."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_zoom( value )

   ##############################################################################
   # Pan the zoomed animation or frame in the x (or horizontal) direction   	#
   # Value ranges from -100 to 100						#
   ##############################################################################
   def horizontal( self, value=0 ):
      if (((not isinstance(value, types.IntType))) or (value not in range(-100, 101))):
         raise vcsError, "Horizontal pan value must be between an integer between -100 and 100."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_horizontal( value )

   ##############################################################################
   # Pan the zoomed animation or frame in the y (or vertical) direction   	#
   # Value ranges from -100 to 100						#
   ##############################################################################
   def vertical( self, value=0 ):
      if (((not isinstance(value, types.IntType))) or (value not in range(-100, 101))):
         raise vcsError, "Vertical pan value must be between an integer between -100 and 100."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_vertical( value )

   ##############################################################################
   # Set the direction of the animation:                                        #
   # Value 1 -> forward, 2 -> backward       	                                #
   ##############################################################################
   def direction( self, value=1 ):
      if (((not isinstance(value, types.IntType))) or (value not in range(1, 3))):
         raise vcsError, "Direction value must be between either 1='forward' or 2='backward'."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_direction( value )

   ##############################################################################
   # Mode sets the cycle, forth and back, or animate once                   	#
   # Value: 1 -> cycle, 2 -> animate once, and 3 -> forth and back              #
   ##############################################################################
   def mode( self, value=1 ):
      if (((not isinstance(value, types.IntType))) or (value not in [1, 3])):
         raise vcsError, "Mode value must be between either 1 or 3."

      if value == 2:
         self.run_flg = 0

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_mode( value )

   ##############################################################################
   # Update the animation display list                                      	#
   ##############################################################################
   def update_animate_display_list( self ):
        current_display_list = self.vcs_self.return_display_names()
         
        temp_list = []
        for i in range(len(self.vcs_self.animate_info)):
           if self.vcs_self.animate_info[i][0].name in current_display_list:
              temp_list.append( (self.vcs_self.animate_info[i][0],
                                self.vcs_self.animate_info[i][1]) )
        self.vcs_self.animate_info = temp_list

   ##############################################################################
   # Close the animate session                                              	#
   ##############################################################################
   def close( self ):
      if self.create_flg == 1:
         self.vcs_self.canvas.animate_close()
         self.gui_popup = 0
         self.create_flg = 0
         self.run_flg = 0
         self.vcs_self.canvas.getcontinentstype( self.continents_hold_value )
         self.continents_value = 0
         self.continents_hold_value = 1
      self.vcs_self.animate_info = []

      # Now that the animation is completed, restore the graphics methods min and max values.
      self.restore_min_max()

   ##############################################################################
   # Pop up the animation GUI                                              	#
   ##############################################################################
   def gui( self, gui_parent=None, transient=0):
      if self.gui_popup == 0:
         self.gui_popup = 1
         a = _animationgui.create(self, gui_parent, transient)
         return a

class animate_obj(animate_obj_old):

    class AnimationSignals(QtCore.QObject):
        """Inner class to hold signals since main object is not a QObject
        """
        drew = QtCore.pyqtSignal()
        created = QtCore.pyqtSignal()
        canceled = QtCore.pyqtSignal()
        paused = QtCore.pyqtSignal()

    def __init__(self, vcs_self):
        animate_obj_old.__init__(self,vcs_self)
        self.zoom_factor = 1.
        self.vertical_factor = 0
        self.horizontal_factor = 0
        self.allArgs = []
        self.canvas = None
        self.animation_seed = None
        self.animation_files = []
        self.runTimer = QtCore.QTimer() #used to run the animation
        self.runTimer.timeout.connect(self.next)
        self.fps(10) #sets runTimer interval
        self.current_frame = 0
        self.loop = True
        self.signals = self.AnimationSignals() #holds signals, since we are not a QObject

    def create( self, parent=None, min=None, max=None, save_file=None, thread_it = 1, rate=5., bitrate=None, ffmpegoptions='', axis=0):
        self.current_frame = 0
        if thread_it:
            class crp(QtCore.QObject): 

                def __init__(self, anim):
                    QtCore.QObject.__init__(self)
                    self.animationTimer = QtCore.QBasicTimer()
                    self.animationFrame = 0
                    self.anim = anim
                    self.dialog = QtGui.QProgressDialog("Creating animation...", "Cancel", 0, 0)
                    self.dialog.setModal(True)
                    self.dialog.canceled.connect(self.dailogCanceled)
                    
                def timerEvent(self, event):
                    self.dialog.setValue(self.animationFrame)
                    if self.animationFrame>=len(self.anim.allArgs):
                        self.animationTimer.stop()
                        self.anim.animationCreated()
                        return
                    self.anim.renderFrame(self.animationFrame)
                    self.animationFrame += 1

                def dailogCanceled(self):
                    self.animationTimer.stop()
                    self.anim.animationCanceled()

            global C
            C=crp(self)
            self._actualCreate(parent,min,max,save_file,rate,bitrate,ffmpegoptions,axis,C)
        else:
            C=None
            self._actualCreate(parent,min,max,save_file,rate,bitrate,ffmpegoptions,axis)
        return C

    def _actualCreate( self, parent=None, min=None, max=None, save_file=None, rate=5., bitrate=None, ffmpegoptions='', axis=0, sender=None):
        alen = None
        if self.canvas is None:  
            self.canvas=vcs.init()
        self.canvas.clear()
        dims = self.vcs_self.canvasinfo()
        if dims['height']<500:
            factor = 2
        else:
            factor=1
        if dims["width"]<dims["height"]:
            self.canvas.portrait(width=dims["width"],height=dims["height"])
        self.canvas.setbgoutputdimensions(width = dims['width']*factor,height=dims['height']*factor,units='pixel')
        truncated = False
        for I in self.vcs_self.animate_info:
            if alen is None:
                alen = I[1][0].shape[axis]
            else:
                l = I[1][0].shape[axis]
                if l!=alen:
                    alen = numpy.minimum(alen,l)
                    truncated = True
        if truncated:
            warnings.warn("Because of inconsistent shapes over axis: %i, the animation length will be truncated to: %i\n" % (axis,alen))
        if self.animation_seed is not None:
            if self.animation_files != []:
                for fnm in self.animation_files:
                    os.remove(fnm)
        self.animation_seed = None
        self.animation_files = []
        # Save the min and max values for the graphics methods.
        # Will need to restore values back when animation is done.
        self.save_original_min_max()
        # Note: cannot set the min and max values if the default graphics method is set.
        do_min_max = 'yes'
        try:
           if (parent is not None) and (parent.iso_spacing == 'Log'):
              do_min_max = 'no'
        except:
           pass
        if ( do_min_max == 'yes' ):
             minv = []
             maxv=[]
             if (min is None) or (max is None):
                for i in range(len(self.vcs_self.animate_info)):
                   minv.append( 1.0e77 )
                   maxv.append( -1.0e77 )
                for i in range(len(self.vcs_self.animate_info)):
                   dpy, slab = self.vcs_self.animate_info[i]
                   mins, maxs = vcs.minmax(slab)
                   minv[i] = float(numpy.minimum(float(minv[i]), float(mins)))
                   maxv[i] = float(numpy.maximum(float(maxv[i]), float(maxs)))
             elif ((type(min) == types.ListType) or (type(max) == types.ListType)):
                for i in range(len(self.vcs_self.animate_info)):
                   try:
                      minv.append( min[i] )
                   except:
                      minv.append( min[-1] )
                   try:
                      maxv.append( max[i] )
                   except:
                      maxv.append( max[-1] )
             else:
                for i in range(len(self.vcs_self.animate_info)):
                    minv.append( min )
                    maxv.append( max )
             # Set the min an max for each plot in the page. If the same graphics method is used
             # to display the plots, then the last min and max setting of the data set will be used.
             for i in range(len(self.vcs_self.animate_info)):
                try:
                   self.set_animation_min_max( minv[i], maxv[i], i )
                except Exception,err:
                   pass # if it is default, then you cannot set the min and max, so pass.

        self.allArgs = []
        for i in range(alen):
            #y.clear()
            frameArgs = []
            for I in self.vcs_self.animate_info:
                d=I[0]
                kw={}
                n = len(I[1][0].shape)
                for j,id in enumerate(I[1][0].getAxisIds()):
                    if j!=axis and j<n-2:
                        kw[id]=slice(0,1)
                    elif j==axis:
                        kw[id]=slice(i,i+1)
                    else:
                        break
                args = [I[1][0](**kw),]
                if I[1][1] is not None:
                    kw={}
                    n = len(I[1][1].shape)
                    for j,id in enumerate(I[1][1].getAxisIds()):
                        if j!=axis and j<n-2:
                            kw[id]=slice(0,1)
                        elif j==axis:
                            kw[id]=slice(i,i+1)
                        else:
                            break
                    args.append(I[1][1](**kw))
                args += [d.template,d.g_type,d.g_name]
                #b=y.getboxfill(d.g_name)
                #y.plot(*args,bg=1)
                frameArgs.append(args)
            self.allArgs.append(frameArgs)

        if sender is None:
            for i in xrange(len(self.allArgs)):
                self.renderFrame(i)
            self.restore_min_max()
            self.canvas = None
            self.animationCreated()
        else:
            sender.animationTimer.start(0, sender)
            sender.dialog.setRange(0, len(self.allArgs))
            sender.dialog.show()

    def animationCreated(self):
        self.create_flg = 1
        self.restore_min_max()
        self.canvas = None
        self.signals.created.emit()

    def animationCanceled(self):
        self.create_flg = 0
        self.restore_min_max()
        self.signals.canceled.emit()

    def renderFrame(self, i):
        if self.animation_seed is None:
            self.animation_seed = numpy.random.randint(10000000)
        frameArgs = self.allArgs[i]
        fn = os.path.join(os.environ["HOME"],".uvcdat","__uvcdat_%i_%i.png" % (self.animation_seed,i))
        self.animation_files.append(fn)

        #BB: this clearing and replotting somehow fixes vcs internal state
        # and prevents segfaults when running multiple animations
        self.vcs_self.replot()



        self.canvas.clear()
        self.vcs_self.plot(*frameArgs[0],bg=1)
        self.vcs_self.clear()
        for args in frameArgs:
            self.canvas.plot(*args, bg=1)
        self.canvas.png(fn,draw_white_background=1)
        #self.canvas.png("sample")
        
    # def runner(self):
    #     self.runit = True
    #     while self.runit:
    #         for fn in self.animation_files:
    #             if not self.runit:
    #                 self.run_flg = 0
    #                 break
    #             self.vcs_self.canvas.put_png_on_canvas(fn,self.zoom_factor,self.vertical_factor,self.horizontal_factor)
    #             import time
    #             time.sleep(self.pause_value)

    def next(self):
        """Draws next frame of animation
        """
        if self.create_flg == 1:
            if self.current_frame < len(self.animation_files)-1:
                self.current_frame += 1
            elif self.loop or self.first_run:
                self.current_frame = 0
            else:
                self.pause_run()
            self.draw(self.current_frame)
        else:
            self.pause_run()
        if self.first_run:
            self.first_run = False

    def run(self,*args):
        if self.create_flg == 1 and self.run_flg == 0:
            self.first_run = True
            self.run_flg = 1
            self.runTimer.start()

    def pause_run(self):
        self.run_flg = 0
        self.runTimer.stop()
        self.signals.paused.emit()

    def draw(self, frame):
        #print "Clearing!!!!!!"
        if self.create_flg == 1:
            self.current_frame = frame
            self.vcs_self.canvas.put_png_on_canvas(self.animation_files[frame],
                    self.zoom_factor, self.vertical_factor, self.horizontal_factor)
            self.signals.drew.emit()
        
    def frame(self, frame):
        self.draw(frame)

    def save(self,movie,bitrate=1024, rate=None, options=''):
        if self.create_flg == 1:
            fnms = os.path.join(os.environ["HOME"],".uvcdat","__uvcdat_%i_%%d.png" %      (self.animation_seed))
            if rate is None:
                rate = self.fps()
            self.vcs_self.ffmpeg(movie, fnms, bitrate, rate, options)

    def number_of_frames(self):
        return len(self.animation_files)

    def stop(self):
        self.pause_run()
        self.current_frame = 0

    def pause(self, value):
        value = max(value, 0.0001)
        self.fps(1/value)

    def zoom(self,value):
        self.zoom_factor = value

    def horizontal(self,value):
        self.horizontal_factor = value

    def vertical(self,value):
        self.vertical_factor = value

    def fps(self, value=None):
        if value is not None:
            value = max(value, 0.0001)
            self.frames_per_second = value
            self.runTimer.setInterval(1000/value)
            return self
        return self.frames_per_second

    
############################################################################
#        END OF FILE                                                       #
############################################################################
