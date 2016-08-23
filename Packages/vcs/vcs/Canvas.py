"""
Canvas
    The object onto which all plots are drawn.

    Usually created using :py:func:`vcs.init`, this object provides easy access
    to the functionality of the entire VCS module.
"""
import warnings
import numpy.ma
import MV2
import numpy
import cdutil
from queries import *  # noqa
import boxfill
import isofill
import isoline
import vector
import line
import marker
import fillarea
import texttable
import textorientation
import textcombined
import template
import displayplot
import vtk
from VTKPlots import VTKVCSBackend
from weakref import WeakSet, WeakKeyDictionary

from error import vcsError
import cdms2
import copy
import cdtime
import vcs
import os
import re
import sys
import random
from cdms2.grid import AbstractRectGrid
import shutil
import subprocess
import inspect
import VCS_validation_functions
from xmldocs import plot_keywords_doc, graphics_method_core, axesconvert, xaxisconvert, \
    plot_1D_input, plot_2D_input, plot_output, plot_2_1D_input, \
    plot_2_1D_options
gui_canvas_closed = 0
canvas_closed = 0
import vcsaddons  # noqa
import vcs.manageElements  # noqa
import configurator  # noqa
from projection import no_deformation_projections  # noqa

# Python < 3 DeprecationWarning ignored by default
warnings.simplefilter('default')


class SIGNAL(object):

    def __init__(self, name=None):
        self._functions = WeakSet()
        self._methods = WeakKeyDictionary()
        self._name = name

    def __call__(self, *args, **kargs):
        # Call handler functions
        for func in self._functions:
            func(*args, **kargs)

        # Call handler methods
        for obj, funcs in self._methods.items():
            for func in funcs:
                func(obj, *args, **kargs)

    def connect(self, slot):
        if inspect.ismethod(slot):
            if slot.__self__ not in self._methods:
                self._methods[slot.__self__] = set()

            self._methods[slot.__self__].add(slot.__func__)

        else:
            self._functions.add(slot)

    def disconnect(self, slot):
        if inspect.ismethod(slot):
            if slot.__self__ in self._methods:
                self._methods[slot.__self__].remove(slot.__func__)
        else:
            if slot in self._functions:
                self._functions.remove(slot)

    def clear(self):
        self._functions.clear()
        self._methods.clear()


def dictionarytovcslist(dictionary, name):
    for k in dictionary.keys():
        if not isinstance(k, (float, int, long)):
            raise Exception('Error, vcs list must have numbers only as keys')
    dictionarytovcslist(dictionary, name)
    return None


def _determine_arg_list(g_name, actual_args):
    "Determine what is in the argument list for plotting graphics methods"

    itemplate_name = 2
    igraphics_method = 3
    igraphics_option = 4

    # Note: Set default graphics method to 'default', which is invalid.
    # If it is not modified in this routine, it will be filled in later
    # in _reconstruct_tv after the grid type is established.
    #
    # Xtrargs - {} - added by C.Doutriaux, needed for projection object passed
    # Need to be passed as keyword later
    arglist = [None, None, 'default', 'default', 'default', {}]
    arghold = []
    argstring = []
    args = actual_args
    found_slabs = 0
    for i in range(len(args)):
        if isinstance(args[i], str):
            argstring.append(args[i])
        else:
            try:
                possible_slab = cdms2.asVariable(args[i], 0)
                if hasattr(possible_slab, 'iscontiguous'):
                    if not possible_slab.iscontiguous():
                        # this seems to loose the id...
                        saved_id = possible_slab.id
                        possible_slab = possible_slab.ascontiguousarray()
                        possible_slab.id = saved_id
                arglist[found_slabs] = possible_slab
                if found_slabs == 2:
                    raise vcsError("Too many slab arguments.")
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
                raise vcsError('You can only specify one template object.')
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
                raise vcsError('You can only specify one graphics method.')
            arglist[igraphics_method] = graphicsmethodtype(args[i])
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (isline(args[i])):
            if found_graphics_method:
                raise vcsError('You can only specify one graphics method.')
            arglist[igraphics_method] = 'line'
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (ismarker(args[i])):
            if found_graphics_method:
                raise vcsError('You can only specify one graphics method.')
            arglist[igraphics_method] = 'marker'
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (isfillarea(args[i])):
            if found_graphics_method:
                raise vcsError('You can only specify one graphics method.')
            arglist[igraphics_method] = 'fillarea'
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        elif (istext(args[i])):
            if found_graphics_method:
                raise vcsError('You can only specify one graphics method.')
            arglist[igraphics_method] = 'text'
            arglist[igraphics_option] = args[
                i].Tt_name + ':::' + args[i].To_name
            found_graphics_method = found_graphics_method + 1
        elif (isprojection(args[i])):
            arglist[5]['projection'] = args[i].name
        elif isinstance(args[i], vcsaddons.core.VCSaddon):
            if found_graphics_method:
                raise vcsError('You can only specify one graphics method.')
            arglist[igraphics_method] = graphicsmethodtype(args[i])
            arglist[igraphics_option] = args[i].name
            found_graphics_method = found_graphics_method + 1
        else:
            raise vcsError("Unknown type %s of argument to plotting command." %
                           type(args[i]))
    if g_name is not None:
        arglist[igraphics_method] = g_name

# Now install the string arguments, left to right.
    if found_template == 0:
        if len(argstring) > 0:
            arglist[itemplate_name] = argstring[0]
            del argstring[0]
    if found_graphics_method == 0 and g_name is None:
        if len(argstring) > 0:
            arglist[igraphics_method] = argstring[0]
            del argstring[0]

# Check for various errors
    if len(argstring) >= 1:
        arglist[igraphics_option] = argstring[0]
        del argstring[0]

    if len(argstring) > 0:
        if g_name is None:
            raise vcsError("Error in argument list for vcs plot command.")
        else:
            raise vcsError(
                "Error in argument list for vcs %s  command." %
                g_name)

    if isinstance(arglist[igraphics_method], vcsaddons.core.VCSaddon):
        if found_slabs != arglist[igraphics_method].g_nslabs:
            raise vcsError(
                "%s requires %i slab(s)" %
                (arglist[igraphics_method].g_name,
                 arglist[igraphics_method].g_nslabs))
    else:
        if arglist[igraphics_method].lower() in (
                'scatter', 'vector', 'xvsy', 'stream', 'glyph', '3d_vector', '3d_dual_scalar'):
            if found_slabs != 2:
                raise vcsError(
                    "Graphics method %s requires 2 slabs." %
                    arglist[igraphics_method])
        elif arglist[igraphics_method].lower() == 'meshfill':
            if found_slabs == 0:
                raise vcsError("Graphics method requires at least 1 slab.")
            elif found_slabs == 1:
                g = arglist[0].getGrid()
                if not isinstance(g, (cdms2.gengrid.AbstractGenericGrid,
                                      cdms2.hgrid.AbstractCurveGrid, cdms2.grid.TransientRectGrid)):
                    raise vcsError("Meshfill requires 2 slab if first slab doesn't have a "
                                   "Rectilinear, Curvilinear or Generic Grid type")
        elif ((arglist[igraphics_method] == 'line') or
              (arglist[igraphics_method] == 'marker') or
              (arglist[igraphics_method] == 'fillarea') or
              (arglist[igraphics_method] == 'text')):
            if found_slabs != 0:
                raise vcsError(
                    "Continents or low-level primative methods requires 0 slabs.")
        elif arglist[igraphics_method].lower() == 'default':
            pass                            # Check later
        else:
            if found_slabs != 1 and not(
                    found_slabs == 2 and arglist[igraphics_method].lower() == "1d"):
                raise vcsError(
                    "Graphics method %s requires 1 slab." %
                    arglist[igraphics_method])
    if isinstance(arglist[3], str):
        arglist[3] = arglist[3].lower()
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


class Canvas(object):
    """
    The object onto which all plots are drawn.

    Usually created using :py:func:`vcs.init`, this object provides easy access
    to the functionality of the entire VCS module.
    """
    __slots__ = [
        '_mode',
        '_pause_time',
        '_viewport',
        '_worldcoordinate',
        '_winfo_id',
        '_varglist',
        '_animate_info',
        '_isplottinggridded',
        '_user_actions_names',
        '_user_actions',
        '_animate',
        '_canvas',
        '_canvas_id',
        'mode',
        'pause_time',
        'viewport',
        'worldcoordinate',
        'winfo_id',
        'varglist',
        'animate_info',
        'canvas_template_editor',
        'isplottinggridded',
        'ratio',
        'canvas',
        'animate',
        'user_actions_names',
        'user_actions',
        'size',
        'ParameterChanged',
        'colormap',
        'backgroundcolor',
        'bgX',
        'bgY',
        'display_names',
        '_dotdir',
        '_dotdirenv',
        'drawLogo',
        'enableLogo',
        'backend',
        'configurator',
        '__last_plot_actual_args',
        '__last_plot_keyargs',
        '_continents',
        '_continents_line',
        '_savedcontinentstype',
        '__weakref__',
    ]

#     def applicationFocusChanged(self, old, current ):
#         self.backend.applicationFocusChanged()

    def _set_user_actions_names(self, value):
        value = VCS_validation_functions.checkListElements(
            self,
            'user_actions_names',
            value,
            VCS_validation_functions.checkString)
        self._user_actions_names = value
        while len(self._user_actions) < len(self._user_actions_names):
            self._user_actions.append(self._user_actions[-1])

    def _get_user_actions_names(self):
        return self._user_actions_names
    user_actions_names = property(
        _get_user_actions_names,
        _set_user_actions_names)

    def _set_user_actions(self, value):
        value = VCS_validation_functions.checkListElements(
            self,
            'user_actions_names',
            value,
            VCS_validation_functions.checkCallable)
        self._user_actions = value
        while len(self._user_actions) < len(self._user_actions_names):
            self._user_actions.append(self._user_actions[-1])

    def _get_user_actions(self):
        return self._user_actions
    user_actions = property(_get_user_actions, _set_user_actions)

    def _setmode(self, value):
        value = VCS_validation_functions.checkInt(
            self,
            'mode',
            value,
            minvalue=0,
            maxvalue=1)
        self._mode = value

    def _getmode(self):
        return self._mode
    mode = property(_getmode, _setmode)

    def _setwinfo_id(self, value):
        value = VCS_validation_functions.checkInt(self, 'winfo_id', value)
        self._winfo_id = value

    def _getwinfo_id(self):
        return self._winfo_id
    winfo_id = property(_getwinfo_id, _setwinfo_id)

    def _setvarglist(self, value):
        value = VCS_validation_functions.checkListElements(
            self,
            'varglist',
            value,
            VCS_validation_functions.checkCallable)
        self._varglist = value

    def _getvarglist(self):
        return self._varglist
    varglist = property(_getvarglist, _setvarglist)

    def _setcanvas(self, value):
        raise vcsError("Error, canvas is not an attribute you can set")

    def _getcanvas(self):
        return self._canvas
    canvas = property(_getcanvas, _setcanvas)

    def _setanimate(self, value):
        raise vcsError("Error, animate is not an attribute you can set")

    def _getanimate(self):
        return self._animate
    animate = property(_getanimate, _setanimate)

    def _setpausetime(self, value):
        value = VCS_validation_functions.checkInt(self, 'pause_time', value)
        self._pause_time = value

    def _getpausetime(self):
        return self._pause_time
    pause_time = property(_getpausetime, _setpausetime)

    def _setviewport(self, value):
        if not isinstance(value, list) and not len(value) == 4:
            raise vcsError(
                "viewport must be of type list and have four values ranging between [0,1].")
        for v in range(4):
            if not 0. <= value[v] <= 1.:
                raise vcsError(
                    "viewport must be of type list and have four values ranging between [0,1].")
        self._viewport = value

    def _getviewport(self):
        return self._viewport
    viewport = property(_getviewport, _setviewport)

    def _setworldcoordinate(self, value):
        if not isinstance(value, list) and not len(value) == 4:
            raise vcsError(
                "worldcoordinate must be of type list and have four values ranging between [0,1].")
        self._worldcoordinate = value

    def _getworldcoordinate(self):
        return self._worldcoordinate
    worldcoordinate = property(_getworldcoordinate, _setworldcoordinate)

    def _setisplottinggridded(self, value):
        if not isinstance(value, bool):
            raise vcsError("isplottinggridded must be boolean")
        self._isplottinggridded = value  # No check on this!

    def _getisplottinggridded(self):
        return self._isplottinggridded
    isplottinggridded = property(_getisplottinggridded, _setisplottinggridded)

    def _setanimate_info(self, value):
        self._animate_info = value  # No check on this!

    def _getanimate_info(self):
        return self._animate_info
    animate_info = property(_getanimate_info, _setanimate_info)

    def start(self, *args, **kargs):
        self.interact(*args, **kargs)

    def interact(self, *args, **kargs):
        self.configure()
        self.backend.interact(*args, **kargs)

    def _datawc_tv(self, tv, arglist):
        """The graphics method's data world coordinates (i.e., datawc_x1, datawc_x2,
        datawc_y1, and datawc_y2) will override the incoming variable's coordinates.
        tv equals arglist[0] and assumed to be the first Variable. arglist[1] is
        assumed to be the second variable."""

        # Determine the type of graphics method
        nvar = 1
        if arglist[3] == 'boxfill':
            gm = self.getboxfill(arglist[4])
        elif arglist[3] == 'isofill':
            gm = self.getisofill(arglist[4])
        elif arglist[3] == 'isoline':
            gm = self.getisoline(arglist[4])
        elif arglist[3] == 'scatter':
            nvar = 2
            gm = self.getscatter(arglist[4])
        elif arglist[3] == 'vector':
            nvar = 2
            gm = self.getvector(arglist[4])
        elif arglist[3] == 'xvsy':
            nvar = 2
            gm = self.getxvsy(arglist[4])
        elif arglist[3] == 'xyvsy':
            gm = self.getxyvsy(arglist[4])
        elif arglist[3] == 'yxvsx':
            gm = self.getyxvsx(arglist[4])
        elif arglist[3] == 'taylor':
            gm = self.gettaylor(arglist[4])
        elif arglist[3] == 'meshfill':
            gm = self.getmeshfill(arglist[4])
        else:
            return tv

        # Determine if the graphics method needs clipping
        f32 = numpy.array((1.e20), numpy.float32)
        set_new_x = 0
        set_new_y = 0
        if (gm.datawc_x1 != f32) and (gm.datawc_x2 != f32):
            set_new_x = 1
        if (gm.datawc_y1 != f32) and (gm.datawc_y2 != f32):
            set_new_y = 1

        try:
            if ((set_new_x == 1) and (set_new_y == 0)) or (
                    arglist[3] == 'yxvsx'):
                tv = tv(longitude=(gm.datawc_x1, gm.datawc_x2))
                if nvar == 2:
                    arglist[1] = arglist[1](
                        longitude=(
                            gm.datawc_x1,
                            gm.datawc_x2))
            elif ((set_new_x == 0) and (set_new_y == 1)) or (arglist[3] == 'xyvsy'):
                tv = tv(latitude=(gm.datawc_y1, gm.datawc_y2))
                if nvar == 2:
                    arglist[1] = arglist[1](
                        latitude=(
                            gm.datawc_y1,
                            gm.datawc_y2))
            elif (set_new_x == 1) and (set_new_y == 1):
                tv = tv(
                    latitude=(
                        gm.datawc_y1, gm.datawc_y2), longitude=(
                        gm.datawc_x1, gm.datawc_x2))
                if nvar == 2:
                    arglist[1] = arglist[1](
                        latitude=(
                            gm.datawc_y1, gm.datawc_y2), longitude=(
                            gm.datawc_x1, gm.datawc_x2))
        except:
            pass

        return tv

    def savecontinentstype(self, value):
        self._savedcontinentstype = value

    def onClosing(self, cell):
        if self.configurator:
            self.endconfigure()
        self.backend.onClosing(cell)

    def _reconstruct_tv(self, arglist, keyargs):
        """Reconstruct a transient variable from the keyword arguments.
        Also select the default graphics method, depending on the grid type
        of the reconstructed variable. For meshfill, ravel the last two
        dimensions if necessary.
        arglist[0] is assumed to be a Variable."""

        ARRAY_1 = 0
        ARRAY_2 = 1
        # TEMPLATE = 2
        GRAPHICS_METHOD = 3
        GRAPHICS_OPTION = 4

        origv = arglist[ARRAY_1]

        # Create copies of domain and attributes
        variable = keyargs.get('variable')
        if variable is not None:
            origv = MV2.array(variable)
        tvdomain = origv.getDomain()
        attrs = copy.copy(origv.attributes)
        axislist = list(map(lambda x: x[0].clone(), tvdomain))

        # Map keywords to dimension indices
        try:
            rank = origv.ndim
        except:
            rank = len(origv.shape)

        dimmap = {}
        dimmap['x'] = xdim = rank - 1
        dimmap['y'] = ydim = rank - 2
        dimmap['z'] = rank - 3
        dimmap['t'] = rank - 4
        dimmap['w'] = rank - 5

        # Process grid keyword
        grid = keyargs.get('grid')
        if grid is not None and xdim >= 0 and ydim >= 0:
            if grid.getOrder() is None or grid.getOrder() == 'yx':
                axislist[xdim] = grid.getLongitude().clone()
                axislist[ydim] = grid.getLatitude().clone()
            else:
                axislist[xdim] = grid.getLatitude().clone()
                axislist[ydim] = grid.getLongitude().clone()

        # Process axis keywords
        for c in ['x', 'y', 'z', 't', 'w']:
            if dimmap[c] < 0:
                continue
            arg = keyargs.get(c + 'axis')
            if arg is not None:
                axislist[dimmap[c]] = arg.clone()

        # Process array keywords
        for c in ['x', 'y', 'z', 't', 'w']:
            if dimmap[c] < 0:
                continue
            arg = keyargs.get(c + 'array')
            if arg is not None:
                axis = axislist[dimmap[c]]
                axis = cdms2.createAxis(arg, id=axis.id)
                axis.setBounds(None)
                axislist[dimmap[c]] = axis

        # Process bounds keywords
        for c in ['x', 'y']:
            if dimmap[c] < 0:
                continue
            arg = keyargs.get(c + 'bounds')
            if arg is not None:
                axis = axislist[dimmap[c]]
                axis.setBounds(arg)

        # Process axis name keywords
        for c in ['x', 'y', 'z', 't', 'w']:
            if dimmap[c] < 0:
                continue
            arg = keyargs.get(c + 'name')
            if arg is not None:
                axis = axislist[dimmap[c]]
                axis.id = axis.name = arg

        # Create the internal tv
        tv = cdms2.createVariable(
            origv,
            copy=0,
            axes=axislist,
            attributes=attrs)
        grid = tv.getGrid()

        isgridded = (grid is not None)

        # Set the default graphics method if not already set.
        if arglist[GRAPHICS_METHOD] == "default" or\
                (arglist[GRAPHICS_METHOD] == 'boxfill' and arglist[GRAPHICS_METHOD + 1] == "default"):
                        # See _determine_arg_list

            if grid is None:
                if tv.ndim == 1:
                    arglist[GRAPHICS_METHOD] = 'yxvsx'
                else:
                    arglist[GRAPHICS_METHOD] = 'boxfill'
            elif isinstance(grid, AbstractRectGrid):
                arglist[GRAPHICS_METHOD] = 'boxfill'
            else:
                latbounds, lonbounds = grid.getBounds()
                if (latbounds is None) or (lonbounds is None):
                    if not isinstance(grid, cdms2.hgrid.AbstractCurveGrid):
                        # Plug in 'points' graphics method here, with:
                        #   arglist[GRAPHICS_METHOD] = 'points'
                        raise vcsError(
                            "Cell boundary data is missing, cannot plot nonrectangular gridded data.")
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
                    meshobj = self.createmeshfill()
                    meshobj.wrap = [0.0, 360.0]  # Wraparound
                    arglist[GRAPHICS_OPTION] = '__d_meshobj'

        # IF Meshfill method and no mesh passed then try to get the mesh from
        # the object
        if arglist[GRAPHICS_METHOD] == 'meshfill' and arglist[ARRAY_2] is None:
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
                meshobj = self.createmeshfill()
                meshobj.wrap = [0.0, 360.0]  # Wraparound
                arglist[GRAPHICS_OPTION] = meshobj.name

        # Ravel the last two dimensions for meshfill if necessary
        # value to know if we're plotting a grided meshfill
        self.isplottinggridded = False

        if isgridded and (arglist[GRAPHICS_METHOD] == 'meshfill'):
            self.isplottinggridded = True

        # Process variable attributes
        _process_keyword(tv, 'comment1', 'comment1', keyargs)
        _process_keyword(tv, 'comment2', 'comment2', keyargs)
        _process_keyword(tv, 'comment3', 'comment3', keyargs)
        _process_keyword(tv, 'comment4', 'comment4', keyargs)
        _process_keyword(tv, 'source', 'file_comment', keyargs)
        _process_keyword(tv, 'time', 'hms', keyargs)
        _process_keyword(tv, 'title', 'long_name', keyargs)
        _process_keyword(tv, 'name', 'name', keyargs, default=tv.id)
        tim = keyargs.get('time')
        if tim is not None:
            if isinstance(tim, (str, unicode)):
                ctime = cdtime.s2c(str(tim))
            else:
                ctime = tim.tocomp()
            tv.user_date = str(ctime)
        _process_keyword(tv, 'units', 'units', keyargs)
        _process_keyword(tv, 'date', 'ymd', keyargs)
        # If date has still not been set, try to get it from the first
        # time value if present
        if not hasattr(tv, 'user_date') and not hasattr(
                tv, 'date') and not hasattr(tv, 'time'):
            change_date_time(tv, 0)

        # Draw continental outlines if specified.
        contout = keyargs.get('continents', None)
        if contout is None:
            if (xdim >= 0 and ydim >= 0 and tv.getAxis(xdim).isLongitude() and tv.getAxis(ydim).isLatitude()) or\
                    (self.isplottinggridded):
                contout = self.getcontinentstype()
            else:
                contout = 0

        if (isinstance(arglist[GRAPHICS_METHOD], str) and (arglist[GRAPHICS_METHOD]) == 'meshfill') or (
                (xdim >= 0 and ydim >= 0 and (contout >= 1))):
            self.setcontinentstype(contout)
            self.savecontinentstype(contout)
        else:
            self.setcontinentstype(0)
            self.savecontinentstype(0)

        # Reverse axis direction if necessary
        xrev = keyargs.get('xrev', 0)
        if xrev == 1 and xdim >= 0:
            tv = tv[..., ::-1]

        # By default, latitudes on the y-axis are plotted S-N
        # levels on the y-axis are plotted with decreasing pressure
        if ydim >= 0:
            yrev = keyargs.get('yrev', 0)
            if yrev == 1:
                tv = tv[..., ::-1, :].clone()

#  -- This s no longer needed since we are making a copy of the data.
#     We now apply the axes changes below in __plot. Dean and Charles keep
#     an eye opened for the errors concerning datawc in the VCS module.
#        tv = self._datawc_tv( tv, arglist )
        return tv

    def objecthelp(self, *arg):
        """
    Print out information on the VCS object. See example below on its use.

    :Example:

    ::

        a=vcs.init()
        # Get a VCS line object
        ln=a.getline('red')
        # This will print out information on how to use ln
        a.objecthelp(ln)
    """
        for x in arg:
            print getattr(x, "__doc__", "")

    def __init__(self, mode=1, pause_time=0,
                 call_from_gui=0, size=None, backend="vtk", geometry=None, bg=None):
        self._canvas_id = vcs.next_canvas_id
        self.ParameterChanged = SIGNAL('ParameterChanged')
        vcs.next_canvas_id += 1
        self.colormap = None
        self.backgroundcolor = 255, 255, 255

        # displays plotted
        self.display_names = []
        ospath = os.environ["PATH"]
        found = False
        for p in ospath.split(":"):
            if p == os.path.join(sys.prefix, "bin"):
                found = True
                break
        if found is False:
            os.environ["PATH"] = os.environ["PATH"] + \
                ":" + os.path.join(sys.prefix, "bin")
        global gui_canvas_closed
        global canvas_closed

        self.winfo_id = -99
        self.varglist = []
        self.isplottinggridded = False

        if size is None:
            psize = 1.2941176470588236
        elif isinstance(size, (int, float)):
            psize = size
        elif isinstance(size, str):
            if size.lower() in ['letter', 'usletter']:
                psize = size = 1.2941176470588236
            elif size.lower() in ['a4', ]:
                psize = size = 1.4142857142857141
            else:
                raise Exception('Unknown size: %s' % size)
        else:
            raise Exception('Unknown size: %s' % size)

        self.size = psize

        self.mode = mode
        self._animate_info = []
        self.pause_time = pause_time
        self._canvas = vcs
        self.viewport = [0, 1, 0, 1]
        self.worldcoordinate = [0, 1, 0, 1]
        self._dotdir, self._dotdirenv = vcs.getdotdirectory()
        self.drawLogo = False
        self.enableLogo = True

        if geometry is not None:
            # Extract width and height, create dict
            if type(geometry) == dict:
                for key in geometry:
                    if key not in ("width", "height"):
                        raise ValueError("Unexpected key %s in geometry" % key)

                width = geometry.get("width", None)
                height = geometry.get("height", None)

                check_vals = [v for v in (width, height) if v is not None]
                VCS_validation_functions.checkListOfNumbers(self, 'geometry', check_vals,
                                                            minvalue=1, minelements=1, maxelements=2, ints=True)
            elif type(geometry) in (list, tuple):
                width, height = VCS_validation_functions.checkListOfNumbers(self, 'geometry', geometry,
                                                                            minvalue=1, minelements=2,
                                                                            maxelements=2, ints=True)
            else:
                raise ValueError("geometry should be list, tuple, or dict")
            geometry = {"width": width, "height": height}

        if geometry is not None and bg:
            self.bgX = geometry["width"]
            self.bgY = geometry["height"]
        else:
            # default size for bg
            self.bgX = 814
            self.bgY = 606

        if backend == "vtk":
            self.backend = VTKVCSBackend(self, geometry=geometry, bg=bg)
        elif isinstance(backend, vtk.vtkRenderWindow):
            self.backend = VTKVCSBackend(self, renWin=backend, bg=bg)
        else:
            warnings.warn(
                "Unknown backend type: '%s'\nAssiging 'as is' to "
                "backend, no warranty about anything working from this point on" %
                backend)
            self.backend = backend

        self._animate = self.backend.Animate(self)

        self.configurator = None
        self.setcontinentsline("default")
        self.setcontinentstype(1)

# Initial.attributes is being called in main.c, so it is not needed here!
# Actually it is for taylordiagram graphic methods....
#  Okay, then this is redundant since it is done in main.c. When time perments, put the
#  taylordiagram graphic methods attributes in main.c Because this is here we must check
#  to make sure that the initial attributes file is called only once for normalization
#  purposes....

        self.canvas_template_editor = None
        self.ratio = '0'
        self._user_actions_names = [
            'Clear Canvas',
            'Close Canvas',
            'Show arguments passsed to user action']
        self._user_actions = [self.clear, self.close, self.dummy_user_action]

    def configure(self):
        for display in self.display_names:
            d = vcs.elements["display"][display]
            if "3d" in d.g_type.lower():
                return
        if self.configurator is None:
            self.configurator = configurator.Configurator(self)
            self.configurator.update()
            self.configurator.show()

    def endconfigure(self):
        if self.configurator is not None:
            self.configurator.detach()
            self.configurator = None

    def processParameterChange(self, args):
        self.ParameterChanged(args)

    # Functions to set/querie drawing of UV-CDAT logo
    def drawlogoon(self):
        """Show UV-CDAT logo on the canvas"""
        self.enableLogo = True

    def drawlogooff(self):
        """Hide UV-CDAT logo on the canvas"""
        self.enableLogo = False

    def getdrawlogo(self):
        """
        Returns value of draw logo

        :returns: Boolean value of system variable which indicates whether log will be drawn
        :rtype: bool
        """
        return self.enableLogo

    def initLogoDrawing(self):
        self.drawLogo = self.enableLogo

    def update(self, *args, **kargs):
        """
    If a series of commands are given to VCS and the Canvas Mode is
    set to manual, then use this function to update the plot(s)
    manually.

    :Example:

    ::

        a=vcs.init()
        a.plot(s,'default','boxfill','quick')
        # Go to manual mode
        a.mode = 0
        box=x.getboxfill('quick')
        box.color_1=100
        box.xticlabels('lon30','lon30')
        box.xticlabels('','')
        box.datawc(1e20,1e20,1e20,1e20)
        box.datawc(-45.0, 45.0, -90.0, 90.0)
        # Update the changes manually
        a.update()
"""

        return self.backend.update(*args, **kargs)

    def scriptobject(self, obj, script_filename=None, mode=None):
        """
    Save individual attributes sets (i.e., individual primary class
    objects and/or secondary class objects). These attribute sets
    are saved in the user's current directory in one of two formats:
    Python script, or a Javascript Object.

    .. note::
        If the the filename has a ".py" at the end, it will produce a
        Python script. If the filename has a ".scr" at the end, it will
        produce a VCS script. If neither extensions are given, then by
        default a Javascript Object will be produced.

    .. attention::
        VCS does not allow the modification of 'default' attribute sets,
        it will not allow them to be saved as individual script files.
        However, a 'default' attribute set that has been copied under a
        different name can be saved as a script file.

    .. admonition:: VCS Scripts Deprecated

        SCR scripts are no longer generated by this function




    :Example:

    ::

        a=vcs.init()
        # To Modify an existing line object
        l=a.getline('red')
        # Create an instance of default isoline object
        i=a.createisoline('dean')
        #...
        # Save isoline object as a Python file 'isoline.py'
        a.scriptobject(i,'ex_isoline.py')
        # Save isoline object as a JSON object 'isoline2.json'
        a.scriptobject(i,'ex_isoline2')

:param script_filename: Name of the output script file.
:type script_filename: str

:param mode: Mode is either "w" for replace or "a" for append.
:type mode: str

:param obj: Any VCS primary class or secondary class object.
:type obj: VCS object
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
            elif (obj.g_name == 'Gtd'):
                obj.script(script_filename, mode)
            elif (obj.g_name == 'Gfm'):
                obj.script(script_filename, mode)
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
                textcombined.Tc.script(obj, script_filename, mode)
            elif (obj.s_name == 'Proj'):
                obj.script(script_filename, mode)
            else:
                print 'Could not find the correct secondary class object.'
        else:
            print 'This is not a template, graphics method or secondary method object.'

    def removeobject(self, obj):
        """
        Removes a VCS object from the cache of available objects that can be referred to by name.

        :param obj: The VCS object to be removed.
        :type obj: a VCS object

        :returns: A string stating the object was removed, its type, and its name
        :rtype: str
        """
        __doc__ = vcs.removeobject.__doc__  # noqa
        return vcs.removeobject(obj)

    def removeP(self, *args):
        return vcs.removeP(*args)

    def clean_auto_generated_objects(self, type=None):
        """
        Cleans up all automaticaly generated VCS objects.

        This function will delete all references to objects that
        VCS created automatically in response to user actions but are
        no longer in use. This shouldn't be necessary most of the time,
        but if you're running into performance/memory issues, calling it
        periodically may help.

        :param type: Type of objects to remove. By default, will remove everything.
        :type type: None, str, list/tuple (of str)
        """

        if type is None:
            type = self.listelements()
            type.remove("fontNumber")
        elif isinstance(type, (str, unicode)):
            type = [type, ]
        elif not isinstance(type, (list, tuple)):
            return
        for objtype in type:
            for obj in self.listelements(objtype):
                if obj[:2] == "__":
                    try:
                        exec("o = self.get%s(obj)" % objtype)
                        destroy = True
                        if objtype == 'template':
                            # print o.name
                            dnames = self.return_display_names()
                            for d in dnames:
                                dpy = self.getplot(d)
                                if o.name in [
                                        dpy.template, dpy._template_origin]:
                                    destroy = False
                                    break
                        if destroy:
                            self.removeobject(o)
                    except:
                        pass

        return

    def check_name_source(self, name, source, typ):
        return vcs.check_name_source(name, source, typ)
    check_name_source.__doc__ = vcs.manageElements.check_name_source.__doc__

    def createtemplate(self, name=None, source='default'):
        return vcs.createtemplate(name, source)
    createtemplate.__doc__ = vcs.manageElements.createtemplate.__doc__

    def gettemplate(self, Pt_name_src='default'):
        return vcs.gettemplate(Pt_name_src)
    gettemplate.__doc__ = vcs.manageElements.gettemplate.__doc__

    def createprojection(self, name=None, source='default'):
        return vcs.createprojection(name, source)
    createprojection.__doc__ = vcs.manageElements.createprojection.__doc__

    def getprojection(self, Proj_name_src='default'):
        return vcs.getprojection(Proj_name_src)
    getprojection.__doc__ = vcs.manageElements.getprojection.__doc__

    def createboxfill(self, name=None, source='default'):
        """
        Create a Boxfill Graphics Method

        :param name: The name of the created object
        :type name: str

        :param source: The object to inherit from
        :type source: a boxfill, or a string name of a boxfill

        :return: A boxfill graphics method
        """
        return vcs.createboxfill(name, source)
    createboxfill.__doc__ = vcs.manageElements.createboxfill.__doc__

    def getboxfill(self, Gfb_name_src='default'):
        """

        :param Gfb_name_src:
        :return:
        """
        return vcs.getboxfill(Gfb_name_src)
    getboxfill.__doc__ = vcs.manageElements.getboxfill

    def boxfill(self, *args, **parms):
        """
        Plot a boxfill.

        Generate a boxfill plot given the data, boxfill graphics method, and
        template. If no boxfill class object is given, then the 'default' boxfill
        graphics method is used. Similarly, if no template class object is given,
        then the 'default' template is used.

        :Example:

        ::

            a=vcs.init()
            # Show all the existing boxfill graphics methods
            a.show('boxfill')
            # Create instance of 'quick'
            box=a.getboxfill('quick')
            # Plot array using specified box and default template
            a.boxfill(array,box)
            # Create an instance of template 'AMIP'
            template=a.gettemplate('AMIP')
            # Clear VCS canvas
            a.clear()
            # Plot array using specified box and template
            a.boxfill(array,box,template)
            # Plot array using specified box and template
            a.boxfill(box,array,template)
            # Plot array using specified box and template
            a.boxfill(template,array,box)
            # Plot array using specified box and template
            a.boxfill(template,array,box)
            # Use 'AMIP' template and 'quick' boxfill
            a.boxfill(array,'AMIP','quick')
            # Use 'AMIP' template and 'quick' boxfill
            a.boxfill('AMIP',array,'quick')
            # Use 'AMIP' template and 'quick' boxfill
            a.boxfill('AMIP','quick',array)

%s
%s
%s
%s
%s
"""
        arglist = _determine_arg_list('boxfill', args)
        return self.__plot(arglist, parms)
    boxfill.__doc__ = boxfill.__doc__ % (
        plot_keywords_doc, graphics_method_core, axesconvert, plot_2D_input, plot_output)

    def createtaylordiagram(self, name=None, source='default'):
        return vcs.createtaylordiagram(name, source)
    createtaylordiagram.__doc__ = vcs.manageElements.createtaylordiagram.__doc__

    def gettaylordiagram(self, Gtd_name_src='default'):
        return vcs.gettaylordiagram(Gtd_name_src)
    gettaylordiagram.__doc__ = vcs.manageElements.gettaylordiagram.__doc__

    def taylordiagram(self, *args, **parms):
        """
        Generate a taylordiagram plot given the data, taylordiagram graphics method, and
        template. If no taylordiagram class object is given, then the 'default' taylordiagram
        graphics method is used. Similarly, if no template class object is given,
        then the 'default' template is used.

        :Example:

        ::

    a=vcs.init()
    # Show all the existing taylordiagram graphics methods
    a.show('taylordiagram')
    # Create instance of 'default'
    td=a.gettaylordiagram()
    # Plot array using specified iso and default template
    a.taylordiagram(array,td)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified iso and template
    a.taylordiagram(array,td,template)
"""
        arglist = _determine_arg_list('taylordiagram', args)
        return self.__plot(arglist, parms)

    def createmeshfill(self, name=None, source='default'):
        return vcs.createmeshfill(name, source)
    createmeshfill.__doc__ = vcs.manageElements.createmeshfill.__doc__

    def getmeshfill(self, Gfm_name_src='default'):
        return vcs.getmeshfill(Gfm_name_src)
    getmeshfill.__doc__ = vcs.manageElements.getmeshfill.__doc__

    def meshfill(self, *args, **parms):  # noqa
        """
    Generate a meshfill plot given the data, the mesh, a meshfill graphics method, and
    a template. If no meshfill class object is given, then the 'default' meshfill
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

    Format:
    This function expects 1D data (any extra dimension will be used for animation)
    In addition the mesh array must be of the same shape than data with 2 additional dimension
    representing the vertices coordinates for the Y (0) and X (1) dimension
    Let's say you want to plot a spatial assuming mesh containing 10,000 grid cell, then data must be shape (10000,)
    or (n1,n2,n3,...,10000) if additional dimensions exist (ex time,level), these dimension would be used only
    for animation and will be ignored in the rest of this example.
    The shape of the mesh, assuming 4 vertices per grid cell, must be (1000,2,4), where the array [:,0,:]
    represent the Y coordinates of the vertices (clockwise or counterclockwise) and the array [:,1:]
    represents the X coordinates of the vertices (the same clockwise/counterclockwise than the Y coordinates)
    In brief you'd have:
    data.shape=(10000,)
    mesh.shape=(10000,2,4)

    :Example:

        ::

    a=vcs.init()
    # Show all the existing meshfill graphics methods
    a.show('meshfill')
    # Create instance of 'default'
    mesh=a.getmeshfill()
    # Plot array using specified mesh and default template
    a.meshfill(array,mesh)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified mesh mesh graphic method and template
    a.meshfill(array,mesh,mesh_graphic_method,template)

:returns: ???
:rtype: ???
"""
        arglist = _determine_arg_list('meshfill', args)
        return self.__plot(arglist, parms)

    def create3d_scalar(self, name=None, source='default'):
        return vcs.create3d_scalar(name, source)

    create3d_scalar.__doc__ = vcs.manageElements.create3d_scalar.__doc__

    def get3d_scalar(self, Gfdv3d_name_src='default'):
        return vcs.get3d_scalar(Gfdv3d_name_src)
    get3d_scalar.__doc__ = vcs.manageElements.get3d_scalar.__doc__

    def scalar3d(self, *args, **parms):
        arglist = _determine_arg_list('3d_scalar', args)
        return self.__plot(arglist, parms)

    def create3d_vector(self, name=None, source='default'):
        return vcs.create3d_vector(name, source)

    create3d_vector.__doc__ = vcs.manageElements.create3d_vector.__doc__

    def get3d_vector(self, Gfdv3d_name_src='default'):
        return vcs.get3d_vector(Gfdv3d_name_src)
    get3d_vector.__doc__ = vcs.manageElements.get3d_vector.__doc__

    def vector3d(self, *args, **parms):
        arglist = _determine_arg_list('3d_vector', args)
        return self.__plot(arglist, parms)

    def create3d_dual_scalar(self, name=None, source='default'):
        return vcs.create3d_dual_scalar(name, source)

    create3d_dual_scalar.__doc__ = vcs.manageElements.create3d_dual_scalar.__doc__

    def get3d_dual_scalar(self, Gfdv3d_name_src='default'):
        return vcs.get3d_dual_scalar(Gfdv3d_name_src)
    get3d_dual_scalar.__doc__ = vcs.manageElements.get3d_dual_scalar.__doc__

    def dual_scalar3d(self, *args, **parms):
        arglist = _determine_arg_list('3d_dual_scalar', args)
        return self.__plot(arglist, parms)

    def createisofill(self, name=None, source='default'):
        """
        """
        return vcs.createisofill(name, source)
    createisofill.__doc__ = vcs.manageElements.createisofill.__doc__

    def getisofill(self, Gfi_name_src='default'):
        return vcs.getisofill(Gfi_name_src)
    getisofill.__doc__ = vcs.manageElements.getisofill.__doc__

    def isofill(self, *args, **parms):
        """
    Generate a isofill plot given the data, isofill graphics method, and
    template. If no isofill class object is given, then the 'default' isofill
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

    :Example:

    ::

        a=vcs.init()
         # Show all the existing isofill graphics methods
        a.show('isofill')
        # Create instance of 'quick'
        iso=a.getisofill('quick')
        # Plot array using specified iso and default template
        a.isofill(array,iso)
        # Clear VCS canvas
        a.clear()
        # Plot array using specified iso and template
        a.isofill(array,iso,template)

    %s
    %s
    %s
    %s
    %s
"""
        arglist = _determine_arg_list('isofill', args)
        return self.__plot(arglist, parms)
    isofill.__doc__ = isofill.__doc__ % (
        plot_keywords_doc, graphics_method_core, axesconvert, plot_2D_input, plot_output)

    def createisoline(self, name=None, source='default'):
        return vcs.createisoline(name, source)
    createisoline.__doc__ = vcs.manageElements.createisoline.__doc__

    def getisoline(self, Gi_name_src='default'):
        return vcs.getisoline(Gi_name_src)
    getisoline.__doc__ = vcs.manageElements.getisoline.__doc__

    def isoline(self, *args, **parms):
        """
    Generate a isoline plot given the data, isoline graphics method, and
    template. If no isoline class object is given, then the 'default' isoline
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

    :Example:

        ::

    a=vcs.init()
    # Show all the existing isoline graphics methods
    a.show('isoline')
    # Create instance of 'quick'
    iso=a.getisoline('quick')
    # Plot array using specified iso and default template
    a.isoline(array,iso)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified iso and template
    a.isoline(array,iso,template)

%s
%s
%s
%s
%s
"""
        arglist = _determine_arg_list('isoline', args)
        return self.__plot(arglist, parms)
    isoline.__doc__ = isoline.__doc__ % (
        plot_keywords_doc, graphics_method_core, axesconvert, plot_2D_input, plot_output)

    def create1d(self, name=None, source='default'):
        return vcs.create1d(name, source)
    create1d.__doc__ = vcs.manageElements.create1d.__doc__

    def get1d(self, name):
        return vcs.get1d(name)
    create1d.__doc__ = vcs.manageElements.create1d.__doc__

    def createxyvsy(self, name=None, source='default'):
        return vcs.createxyvsy(name, source)
    createxyvsy.__doc__ = vcs.manageElements.createxyvsy.__doc__

    def getxyvsy(self, GXy_name_src='default'):
        return vcs.getxyvsy(GXy_name_src)
    getxyvsy.__doc__ = vcs.manageElements.getxyvsy.__doc__

    def xyvsy(self, *args, **parms):
        """
    Generate a Xyvsy plot given the data, Xyvsy graphics method, and
    template. If no Xyvsy class object is given, then the 'default' Xyvsy
    graphics method is used. Simerly, if no template class object is given,
    then the 'default' template is used.

    :Example:

        ::

    a=vcs.init()
    # Show all the existing Xyvsy graphics methods
    a.show('xyvsy')
    # Create instance of 'quick'
    xyy=a.getxyvsy('quick')
    # Plot array using specified xyy and default template
    a.xyvsy(array,xyy)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified xyy and template
    a.xyvsy(array,xyy,template)

%s
%s
%s
%s
%s
"""
        arglist = _determine_arg_list('xyvsy', args)
        return self.__plot(arglist, parms)
    xyvsy.__doc__ = xyvsy.__doc__ % (
        plot_keywords_doc, graphics_method_core, xaxisconvert, plot_1D_input, plot_output)

    def createyxvsx(self, name=None, source='default'):
        return vcs.createyxvsx(name, source)
    createyxvsx.__doc__ = vcs.manageElements.createyxvsx.__doc__

    def getyxvsx(self, GYx_name_src='default'):
        return vcs.getyxvsx(GYx_name_src)
    getyxvsx.__doc__ = vcs.manageElements.getyxvsx.__doc__

    def yxvsx(self, *args, **parms):
        """
    Generate a Yxvsx plot given the data, Yxvsx graphics method, and
    template. If no Yxvsx class object is given, then the 'default' Yxvsx
    graphics method is used. Simerly, if no template class object is given,
    then the 'default' template is used.

     :Example:

        ::

    a=vcs.init()
    # Show all the existing Yxvsx graphics methods
    a.show('yxvsx')
    # Create instance of 'quick'
    yxx=a.getyxvsx('quick')
    # Plot array using specified yxx and default template
    a.yxvsx(array,yxx)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified yxx and template
    a.yxvsx(array,yxx,template)

%s
%s
%s
%s
%s
"""
        arglist = _determine_arg_list('yxvsx', args)
        return self.__plot(arglist, parms)
    yxvsx.__doc__ = yxvsx.__doc__ % (
        plot_keywords_doc, graphics_method_core, xaxisconvert, plot_1D_input, plot_output)

    def createxvsy(self, name=None, source='default'):
        return vcs.createxvsy(name, source)
    createxvsy.__doc__ = vcs.manageElements.createxvsy.__doc__

    def getxvsy(self, GXY_name_src='default'):
        return vcs.getxvsy(GXY_name_src)
    getxvsy.__doc__ = vcs.manageElements.getxvsy.__doc__

    def xvsy(self, *args, **parms):
        """
    Generate a XvsY plot given the data, XvsY graphics method, and
    template. If no XvsY class object is given, then the 'default' XvsY
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

     :Example:

        ::

    a=vcs.init()
    # Show all the existing XvsY graphics methods
    a.show('xvsy')
    # Create instance of 'quick'
    xy=a.getxvsy('quick')
    # Plot array using specified xy and default template
    a.xvsy(array,xy)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified xy and template
    a.xvsy(array,xy,template)

%s
%s
%s
%s
%s
"""
        arglist = _determine_arg_list('xvsy', args)
        return self.__plot(arglist, parms)
    xvsy.__doc__ = xvsy.__doc__ % (plot_keywords_doc,
                                   graphics_method_core,
                                   axesconvert,
                                   plot_2_1D_input,
                                   plot_output)

    def createvector(self, name=None, source='default'):
        return vcs.createvector(name, source)
    createvector.__doc__ = vcs.manageElements.createvector.__doc__

    def getvector(self, Gv_name_src='default'):
        return vcs.getvector(Gv_name_src)
    getvector.__doc__ = vcs.manageElements.getvector.__doc__

    def vector(self, *args, **parms):
        """
    Generate a vector plot given the data, vector graphics method, and
    template. If no vector class object is given, then the 'default' vector
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

    :Example:

        ::

    a=vcs.init()
    # Show all the existing vector graphics methods
    a.show('vector')
    # Create instance of 'quick'
    vec=a.getvector('quick')
    # Plot array using specified vec and default template
    a.vector(array,vec)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified vec and template
    a.vector(array,vec,template)
"""
        arglist = _determine_arg_list('vector', args)
        return self.__plot(arglist, parms)

    def createscatter(self, name=None, source='default'):
        return vcs.createscatter(name, source)
    createscatter.__doc__ = vcs.manageElements.createscatter.__doc__

    def getscatter(self, GSp_name_src='default'):
        return vcs.getscatter(GSp_name_src)
    getscatter.__doc__ = vcs.manageElements.getscatter.__doc__

    def scatter(self, *args, **parms):
        """
    Generate a scatter plot given the data, scatter graphics method, and
    template. If no scatter class object is given, then the 'default' scatter
    graphics method is used. Similarly, if no template class object is given,
    then the 'default' template is used.

    :Example:

        ::

    a=vcs.init()
    # Show all the existing scatter graphics methods
    a.show('scatter')
    # Create instance of 'quick'
    sct=a.getscatter('quick')
    # Plot array using specified sct and default template
    a.scatter(array,sct)
    # Clear VCS canvas
    a.clear()
    # Plot array using specified sct and template
    a.scatter(array,sct,template)

%s
%s
%s
%s
%s
"""

        arglist = _determine_arg_list('scatter', args)
        return self.__plot(arglist, parms)
    scatter.__doc__ = scatter.__doc__ % (
        plot_keywords_doc, graphics_method_core, axesconvert, plot_2_1D_input, plot_output)

    def createline(self, name=None, source='default', ltype=None,  # noqa
                   width=None, color=None, priority=None,
                   viewport=None, worldcoordinate=None,
                   x=None, y=None, projection=None):
        return vcs.createline(name, source, ltype, width, color,
                              priority, viewport, worldcoordinate, x, y, projection)
    createline.__doc__ = vcs.manageElements.createline.__doc__

    def getline(self, name='default', ltype=None, width=None, color=None,
                priority=None, viewport=None,
                worldcoordinate=None,
                x=None, y=None):
        return vcs.getline(
            name, ltype, width, color, priority, viewport, worldcoordinate, x, y)
    getline.__doc__ = vcs.manageElements.getline.__doc__

    def line(self, *args, **parms):
        """
    Plot a line segment on the Vcs Canvas. If no line class
    object is given, then an error will be returned.

    :Example:

        ::

    a=vcs.init()
    # Show all the existing line objects
    a.show('line')
    # Create instance of 'red'
    ln=a.getline('red')
    # Set the line width
    ln.width=4
    # Set the line color
    ln.color = 242
    # Set the line type
    ln.type = 4
    # Set the x value points
    ln.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]]
    # Set the y value points
    ln.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]]
    # Plot using specified line object
    a.line(ln)

"""
        arglist = _determine_arg_list('line', args)
        return self.__plot(arglist, parms)

    def drawline(self, name=None, ltype='solid', width=1, color=241,  # noqa
                 priority=1, viewport=[0.0, 1.0, 0.0, 1.0],
                 worldcoordinate=[0.0, 1.0, 0.0, 1.0],
                 x=None, y=None, projection='default', bg=0):
        """
    Generate and draw a line object on the VCS Canvas.

     :Example:

 ::

    a=vcs.init()
    # Show all the existing line objects
    a.show('line')
    # Create instance of line object 'red'
    ln=a.drawline(name='red', ltype='dash', width=2,
                  color=242, priority=1, viewport=[0, 1.0, 0, 1.0],
                  worldcoordinate=[0,100, 0,50],
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )
    # Plot using specified line object
    a.line(ln)


:param name: Name of created object
:type name: str

:param ltype: One of "dash", "dash-dot", "solid", "dot", or "long-dash".
:type ltype: str

:param width: Thickness of the line to be drawn
:type width: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the line will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:param projection: Specify a geographic projection used to convert x/y from spherical coordinates into 2D coordinates.
:type projection: str or projection object

:returns: A VCS line object
:rtype: vcs.line.Tl
"""
        if (name is None) or (not isinstance(name, str)):
            raise vcsError('Must provide string name for the line.')
        else:
            lo = self.listelements('line')
            if name in lo:
                ln = self.getline(name)
            else:
                ln = self.createline(name)
        ln.type = ltype
        ln.width = width
        ln.color = color
        ln.priority = priority
        ln.viewport = viewport
        ln.worldcoordinate = worldcoordinate
        ln.x = x
        ln.y = y
        ln.projection = projection
        self.line(ln, bg=bg)

        return ln

    def createmarker(self, name=None, source='default', mtype=None,  # noqa
                     size=None, color=None, priority=1,
                     viewport=None, worldcoordinate=None,
                     x=None, y=None, projection=None):
        return vcs.createmarker(name, source, mtype, size, color, priority,
                                viewport, worldcoordinate, x, y, projection)
    createmarker.__doc__ = vcs.manageElements.createmarker.__doc__

    def getmarker(self, name='default', mtype=None, size=None, color=None,
                  priority=None, viewport=None,
                  worldcoordinate=None,
                  x=None, y=None):
        return vcs.getmarker(
            name, mtype, size, color, priority, viewport, worldcoordinate, x, y)
    getmarker.__doc__ = vcs.manageElements.getmarker.__doc__

    def marker(self, *args, **parms):
        """
        Plot a marker segment on the Vcs Canvas. If no marker class
        object is given, then an error will be returned.

         :Example:

    ::

            a=vcs.init()
            # Show all the existing marker objects
            a.show('marker')
            # Create instance of 'red'
            mrk=a.getmarker('red')
            # Set the marker size
            mrk.size=4
            # Set the marker color
            mrk.color = 242
            # Set the marker type
            mrk.type = 4
            # Set the x value points
            mrk.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]]
            # Set the y value points
            mrk.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]]
            # Plot using specified marker object
            a.marker(mrk)

:returns: a VCS displayplot object
:rtype: vcs.displayplot.Dp
        """
        arglist = _determine_arg_list('marker', args)
        return self.__plot(arglist, parms)

    def drawmarker(self, name=None, mtype='solid', size=1, color=241,
                   priority=1, viewport=[0.0, 1.0, 0.0, 1.0],
                   worldcoordinate=[0.0, 1.0, 0.0, 1.0],
                   x=None, y=None, bg=0):
        """
        Generate and draw a marker object on the VCS Canvas.

        :Example:

        ::

    a=vcs.init()
    # Show all the existing marker objects
    a.show('marker')
    # Create instance of marker object 'red'
    mrk=a.drawmarker(name='red', mtype='dot', size=2,
                  color=242, priority=1, viewport=[0, 1.0, 0, 1.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )
    # Plot using specified marker object
    a.marker(mrk)


:param name: Name of created object
:type name: str

:param mtype: Marker type, i.e. 'dot', 'plus', 'star, etc.
:type mtype: str

:param size: Size of the marker to draw
:type size: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the marker will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:returns: A drawmarker object
:rtype: vcs.marker.Tm
"""
        if (name is None) or (not isinstance(name, str)):
            raise vcsError('Must provide string name for the marker.')
        else:
            lo = self.listelements('marker')
            if name in lo:
                mrk = self.getmarker(name)
            else:
                mrk = self.createmarker(name)
        mrk.type = mtype
        mrk.size = size
        mrk.color = color
        mrk.priority = priority
        mrk.viewport = viewport
        mrk.worldcoordinate = worldcoordinate
        mrk.x = x
        mrk.y = y
        self.marker(mrk, bg=bg)

        return mrk

    def createfillarea(self, name=None, source='default', style=None,
                       index=None, color=None, priority=1,
                       viewport=None, worldcoordinate=None,
                       x=None, y=None):
        return vcs.createfillarea(
            name, source, style, index, color, priority, viewport, worldcoordinate, x, y)
    createfillarea.__doc__ = vcs.manageElements.createfillarea.__doc__

    def getfillarea(self, name='default', style=None,
                    index=None, color=None,
                    priority=None, viewport=None,
                    worldcoordinate=None,
                    x=None, y=None):
        return vcs.getfillarea(
            name, style, index, color, priority, viewport, worldcoordinate, x, y)
    getfillarea.__doc__ = vcs.manageElements.getfillarea.__doc__

    def fillarea(self, *args, **parms):
        """
    Generate a fillarea plot

    Plot a fillarea segment on the Vcs Canvas. If no fillarea class
    object is given, then an error will be returned.

    :Example:

        ::

    a=vcs.init()
    # Show all the existing fillarea objects
    a.show('fillarea')
    # Create instance of 'red'
    fa=a.createfillarea('red')
    # Set the fillarea style
    fa.style=1
    # Set the fillarea index
    fa.index=4
    # Set the fillarea color
    fa.color = 242
    # Set the fillarea type
    fa.type = 4
    # Set the x value points
    fa.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]]
    # Set the y value points
    fa.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]]
    # Plot using specified fillarea object
    a.fillarea(fa)

:returns: A fillarea object
:rtype: vcs.displayplot.Dp
"""
        arglist = _determine_arg_list('fillarea', args)
        return self.__plot(arglist, parms)

    def drawfillarea(self, name=None, style=1, index=1, color=241,
                     priority=1, viewport=[0.0, 1.0, 0.0, 1.0],
                     worldcoordinate=[0.0, 1.0, 0.0, 1.0],
                     x=None, y=None, bg=0):
        """
        Generate and draw a fillarea object on the VCS Canvas.

        :Example:
::

    a=vcs.init()
    # Show all the existing fillarea objects
    a.show('fillarea')
    # Create instance of fillarea object 'red'
    fa=a.drawfillarea(name='red', style=1, color=242,
                  priority=1, viewport=[0, 1.0, 0, 1.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50], bg=0 )
    # Plot using specified fillarea object
    a.fillarea(fa)


:param name: Name of created object
:type name: str

:param style: One of "hatch", "solid", or "pattern".
:type style: str

:param index: Specifies which `pattern <http://uvcdat.llnl.gov/gallery/fullsize/pattern_chart.png>`_
to fill the fillarea with. Accepts ints from 1-20.

:type index: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))

:type color: str or int

:param priority: The layer on which the fillarea will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:param bg: Boolean value. True => object drawn in background (not shown on canvas). False => object shown on canvas.
:type bg: bool

:returns: A fillarea object
:rtype: vcs.fillarea.Tf
"""
        if (name is None) or (not isinstance(name, str)):
            raise vcsError('Must provide string name for the fillarea.')
        else:
            lo = self.listelements('fillarea')
            if name in lo:
                fa = self.getfillarea(name)
            else:
                fa = self.createfillarea(name)
        fa.style = style
        fa.index = index
        fa.color = color
        fa.priority = priority
        fa.viewport = viewport
        fa.worldcoordinate = worldcoordinate
        fa.x = x
        fa.y = y
        self.fillarea(fa, bg=bg)

        return fa

    def createtexttable(self, name=None, source='default', font=None,
                        spacing=None, expansion=None, color=None, priority=None,
                        viewport=None, worldcoordinate=None,
                        x=None, y=None):
        return vcs.createtexttable(name, source, font, spacing, expansion, color, priority,
                                   viewport, worldcoordinate, x, y)
    createtexttable.__doc__ = vcs.manageElements.createtexttable.__doc__

    def gettexttable(self, name='default', font=None,
                     spacing=None, expansion=None, color=None,
                     priority=None, viewport=None,
                     worldcoordinate=None,
                     x=None, y=None):
        return vcs.gettexttable(name, font, spacing, expansion, color, priority,
                                viewport, worldcoordinate, x, y)
    gettexttable.__doc__ = vcs.manageElements.gettexttable.__doc__

    def createtextorientation(self, name=None, source='default'):
        return vcs.createtextorientation(name, source)
    createtextorientation.__doc__ = vcs.manageElements.createtextorientation.__doc__

    def gettextorientation(self, To_name_src='default'):
        return vcs.gettextorientation(To_name_src)
    gettextorientation.__doc__ = vcs.manageElements.gettextorientation.__doc__

    def createtextcombined(self, Tt_name=None, Tt_source='default', To_name=None, To_source='default',  # noqa
                           font=None, spacing=None, expansion=None, color=None,
                           priority=None, viewport=None, worldcoordinate=None, x=None, y=None,
                           height=None, angle=None, path=None, halign=None, valign=None, projection=None):
        return vcs.createtextcombined(Tt_name, Tt_source, To_name, To_source,
                                      font, spacing, expansion, color, priority, viewport, worldcoordinate,
                                      x, y, height, angle, path, halign, valign, projection)
    createtextcombined.__doc__ = vcs.manageElements.createtextcombined.__doc__
    #
    # Set alias for the secondary createtextcombined.
    createtext = createtextcombined

    def gettextcombined(self, Tt_name_src='default', To_name_src=None, string=None,
                        font=None, spacing=None, expansion=None, color=None,
                        priority=None, viewport=None, worldcoordinate=None, x=None, y=None,
                        height=None, angle=None, path=None, halign=None, valign=None):
        return vcs.gettextcombined(Tt_name_src, To_name_src, string,
                                   font, spacing, expansion, color,
                                   priority, viewport, worldcoordinate,
                                   x, y, height, angle, path, halign, valign)
    gettextcombined.__doc__ = vcs.manageElements.gettextcombined.__doc__
    #
    # Set alias for the secondary gettextcombined.
    gettext = gettextcombined

    def textcombined(self, *args, **parms):
        """
        Plot a textcombined segment on the Vcs Canvas. If no textcombined class
        object is given, then an error will be returned.
        *This function can also be called using the format **text(self, *args, **parms)** *

        :Example:

        ::

        a=vcs.init()
        # Show all the existing texttable objects
        a.show('texttable')
        # Show all the existing textorientation objects
        a.show('textorientation')
        # Create instance of 'std' and '7left'
        tt=a.gettext('std','7left')
        # Show the string "Text1" on the VCS Canvas
        tt.string = 'Text1'
        # Set the text size
        tt.font=2
        # Set the text color
        tt.color = 242
        # Set the text angle
        tt.angle = 45
        # Set the x value points
        tt.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]]
        # Set the y value points
        tt.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]]
        # Plot using specified text object
        a.text(tt)
        #Optionally you can pass a string, the coordinates, and any keyword
        a.plot('Hi',.5,.5,color=241,angle=45)

:returns: ???
:rtype: ???
        """
        # First check if color is a string
        if 'color' in parms.keys():
            if isinstance(parms['color'], type('')):
                parms['color'] = self.match_color(parms['color'])

        if not isinstance(args[0], vcs.textcombined.Tc):
            args = list(args)
            # Ok we have a user passed text object let's first create a random text combined
# icont=1
# while icont:
# n=random.randint(0,100000)
# try:
# t=self.createtextcombined('__'+str(n),'default','__'+str(n),'default')
# icont=0
# except:
# pass
            t = self.createtextcombined()
            t.string = [args.pop(0)]
            t.x = [args.pop(0)]
            t.y = [args.pop(0)]
            # t.list()
            for k in parms.keys():
                setattr(t, k, parms[k])
                del(parms[k])
            args.insert(0, t)
        arglist = _determine_arg_list('text', args)
        return self.__plot(arglist, parms)
    #
    # Set alias for the secondary textcombined.
    text = textcombined

    def gettextextent(self, textobject):
        """Returns the coordinate of the box surrounding a text object once printed

        :Example:

        ::

        a=vcs.init()
        t=a.createtext()
        t.x=[.5]
        t.y=[.5]
        t.string=['Hello World']
        extent = a.gettextextent(t)
        print extent

:param textobject: A VCS text object
:type textobject: textcombined

:returns: ???
:rtype: ???

        """
        if not vcs.istext(textobject):
            raise vcsError('You must pass a text object')
        To = textobject.To_name
        Tt = textobject.Tt_name
        return self.backend.gettextextent(To, Tt)

    def match_color(self, color, colormap=None):  # noqa
        return vcs.match_color(color, colormap)

    def drawtextcombined(self, Tt_name=None, To_name=None, string=None,
                         font=1, spacing=2, expansion=100, color=241,
                         height=14, angle=0, path='right', halign='left',
                         valign='half',
                         priority=1, viewport=[0.0, 1.0, 0.0, 1.0],
                         worldcoordinate=[0.0, 1.0, 0.0, 1.0],
                         x=None, y=None, bg=0):
        """
    Generate and draw a textcombined object on the VCS Canvas.

     :Example:

        ::

    a=vcs.init()
    # Show all the existing texttable objects
    a.show('texttable')
    # Create instance of texttable object 'red'
    tt=a.drawtexttable(Tt_name = 'red', To_name='7left', mtype='dash', size=2,
                       color=242, priority=1, viewport=[0, 1.0, 0, 1.0],
                       worldcoordinate=[0,100, 0,50]
                       x=[0,20,40,60,80,100],
                       y=[0,10,20,30,40,50] )
    # Plot using specified texttable object
    a.texttable(tt)

:param name: Name of created object
:type name: str

:param style: One of "hatch", "solid", or "pattern".
:type style: str

:param index: Specifies which `pattern <http://uvcdat.llnl.gov/gallery/fullsize/pattern_chart.png>`_
              to fill the fillarea with. Accepts ints from 1-20.
:type index: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the fillarea will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:param bg: Boolean value. True => object drawn in background (not shown on canvas). False => object shown on canvas.
:type bg: bool

:returns: A texttable object
:rtype:
"""
        if (Tt_name is None) or (not isinstance(Tt_name, str)):
            raise vcsError('Must provide string name for the texttable.')
        else:
            lot = self.listelements('texttable')
            if Tt_name not in lot:
                self.createtexttable(Tt_name)
            loo = self.listelements('textorientation')
            if To_name not in loo:
                self.createtextorientation(To_name)
            t = self.gettextcombined(Tt_name, To_name)

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

        self.text(t, bg=bg)

        return t
    #
    # Set alias for the secondary drawtextcombined.
    drawtext = drawtextcombined

    _plot_keywords_ = ['variable', 'grid', 'xaxis', 'xarray',  'xrev', 'yaxis', 'yarray', 'yrev', 'continents',
                       'xbounds', 'ybounds', 'zaxis', 'zarray', 'taxis', 'tarray', 'waxis', 'warray', 'bg', 'ratio',
                       'donotstoredisplay', 'render', 'continents_line', "display_name"]

    _deprecated_plot_keywords_ = ["time", "units", "file_comment", "xname", "yname", "zname", "tname", "wname",
                                  "xunits", "yunits", "zunits", "tunits", "wunits", "comment1", "comment2", "comment3",
                                  "comment4", "long_name"]
    # def replot(self):
    #    """ Clears and plots with last used plot arguments
    #    """
    #    self.clear()
    #    self.plot(*self.__last_plot_actual_args, **self.__last_plot_keyargs)

    def plot(self, *actual_args, **keyargs):
        """
    Plot an array(s) of data given a template and graphics method. The VCS template is
    used to define where the data and variable attributes will be displayed on the VCS
    Canvas. The VCS graphics method is used to define how the array(s) will be shown
    on the VCS Canvas.

    .. describe:: Plot Usage:

        .. code-block:: python

            plot(array1=None, array2=None, template_name=None,
                graphics_method=None, graphics_name=None,
                [key=value [, key=value [, ...]]])

        .. note::
             array1 and array2 are NumPy arrays.

    .. describe:: Plot attribute keywords:

        .. note::
            More specific attributes take precedence over general attributes. In particular,
            specific attributes override variable object attributes, dimension attributes and
            arrays override axis objects, which override grid objects, which override variable
            objects.

            For example, if both 'file_comment' and 'variable' keywords are specified, the value of
            'file_comment' is used instead of the file comment in the parent of variable. Similarly,
            if both 'xaxis' and 'grid' keywords are specified, the value of 'xaxis' takes precedence
            over the x-axis of grid.

        *  ratio [default is none]

            * None: let the self.ratio attribute decide
            *  0,'off': overwrite self.ratio and do nothing about the ratio
            * 'auto': computes an automatic ratio
            * '3',3: y dim will be 3 times bigger than x dim (restricted to original tempalte.data area
            * Adding a 't' at the end of the ratio, makes the tickmarks and boxes move along.

        * Dimension attribute keys (dimension length=n):

            * x or y Dimension values

                .. code-block:: python

                    [x|y|z|t|w]array = NumPy array of length n
                    [x|y|z|t|w]array = NumPy array of length n

            * x or y Dimension boundaries

                .. code-block:: python

                    [x|y]bounds = NumPy array of shape (n,2)

        * CDMS object:

            * x or y Axis

                .. code-block:: python

                    [x|y|z|t|w]axis = CDMS axis object

            * Grid object (e.g. grid=var.getGrid())

                .. code-block:: python

                    grid = CDMS grid object

            * Variable object

                .. code-block:: python

                    variable = CDMS variable object

        * Other:

            * Reverse the direction of the x or y axis:

                .. code-block:: python

                    [x|y]rev = 0|1

                .. note::
                    For example, xrev = 1 would reverse the
                    direction of the x-axis

            * Continental outlines:

                .. code-block:: python

                    continents = 0,1,2,3,4,5,6,7,8,9,10,11
                    # VCS line object to define continent appearance
                    continents_line = vcs.getline("default")

                .. note::
                    If continents >=1, plot continental outlines.
                    By default: plot of xaxis is longitude, yaxis is latitude
                    -OR- xname is 'longitude' and yname is 'latitude'

                * List of continents-type values (integers from 0-11)

                    * 0 signifies "No Continents"
                    * 1 signifies "Fine Continents"
                    * 2 signifies "Coarse Continents"
                    * 3 signifies "United States"
                    * 4 signifies "Political Borders"
                    * 5 signifies "Rivers"

                .. note::
                    Values 6 through 11 signify the line type defined by
                    the files data_continent_other7 through data_continent_other12.

            * To set whether the displayplot object generated by this plot is stored

                .. code-block:: python

                    donotstoredisplay = True|False

            * Whether to actually render the plot or not (useful for doing a bunch of plots in a row)

                .. code-block:: python

                    render = True|False

            * VCS Display plot name (used to prevent duplicate display plots)

                .. code-block:: python

                    display_name = "__display_123"

            * Ratio of height/width for the plot; autot and auto will choose a "good" ratio for you.

                .. code-block:: python

                    ratio = 1.5|"autot"|"auto"

            * Plot the actual grid or the dual grid

                .. code-block:: python

                    plot_based_dual_grid = True | False

                .. note::
                    This is based on what is needed by the plot: isofill, isoline, vector need
                    point attributes, boxfill and meshfill need cell attributes
                    the default is True (if the parameter is not specified).

            * Graphics Output in Background Mode:

                .. code-block:: python

                    # if ==1, create images in the background
                    bg = 0|1


    :Example:

        ::

    # x is an instance of the VCS class object (constructor)
    x=vcs.init()
    # this call will use default settings for template and boxfill
    x.plot(array)
    # this is specifying the template and graphics method
    x.plot(array, 'AMIP', 'isofill','AMIP_psl')
    # get a predefined the template 'AMIP'
    t=x.gettemplate('AMIP')
    # get a predefined the vector graphics method 'quick'
    vec=x.getvector('quick')
    # plot the data as a vector using the 'AMIP' template
    x.plot(array1, array2, t, vec)
    # clear the VCS Canvas of all plots
    x.clear()
    # create boxfill graphics method 'new'
    box=x.createboxfill('new')
    # plot array data using box 'new' and template 't'
    x.plot(box,t,array)

%s
%s
%s
%s
%s
%s
"""
        self.__last_plot_actual_args = actual_args
        self.__last_plot_keyargs = keyargs
        passed_var = keyargs.get("variable", None)
        arglist = _determine_arg_list(None, actual_args)
        if passed_var is not None:
            arglist[0] = cdms2.asVariable(passed_var)

        try:
            pfile = actual_args[0].parent
            keyargs['cdmsfile'] = pfile.uri if hasattr(
                pfile,
                'uri') else pfile.id
        except:
            pass

        if "continents_line" in keyargs:
            # Stash the current line type
            old_line = self.getcontinentsline()
            self.setcontinentsline(keyargs["continents_line"])

        # Plot the data
        a = self.__plot(arglist, keyargs)

        if "continents_line" in keyargs:
            # Restore the canvas line type
            self.setcontinentsline(old_line)
        return a
    plot.__doc__ = plot.__doc__ % (plot_2_1D_options,
                                   plot_keywords_doc,
                                   graphics_method_core,
                                   axesconvert,
                                   plot_2_1D_input,
                                   plot_output)

    def plot_filledcontinents(
            self, slab, template_name, g_type, g_name, bg, ratio):
        cf = cdutil.continent_fill.Gcf()
        if g_type.lower() == 'boxfill':
            g = self.getboxfill(g_name)
        lons = slab.getLongitude()
        lats = slab.getLatitude()

        if lons is None or lats is None:
            return
        if g.datawc_x1 > 9.9E19:
            cf.datawc_x1 = lons[0]
        else:
            cf.datawc_x1 = g.datawc_x1
        if g.datawc_x2 > 9.9E19:
            cf.datawc_x2 = lons[-1]
        else:
            cf.datawc_x2 = g.datawc_x2
        if g.datawc_y1 > 9.9E19:
            cf.datawc_y1 = lats[0]
        else:
            cf.datawc_y1 = g.datawc_y1
        if g.datawc_y2 > 9.9E19:
            cf.datawc_y2 = lats[-1]
        else:
            cf.datawc_y2 = g.datawc_y2
        try:
            self.gettemplate(template_name)
            cf.plot(x=self, template=template_name, ratio=ratio)
        except Exception as err:
            print err

    def __new_elts(self, original, new):
        for e in vcs.elements.keys():
            for k in vcs.elements[e].keys():
                if k not in original[e]:
                    new[e].append(k)
        return new

    def __plot(self, arglist, keyargs):

            # This routine has five arguments in arglist from _determine_arg_list
            # It adds one for bg and passes those on to Canvas.plot as its sixth
            # arguments.

            # First of all let's remember which elets we have before comin in here
            # so that anything added (temp objects) can be removed at clear
            # time
        original_elts = {}
        new_elts = {}
        for k in vcs.elements.keys():
            original_elts[k] = vcs.elements[k].keys()
            new_elts[k] = []
        # First of all try some cleanup
        assert len(arglist) == 6
        xtrakw = arglist.pop(5)
        for k in xtrakw.keys():
            if k in keyargs.keys():
                raise vcsError('Multiple Definition for ' + str(k))
            else:
                keyargs[k] = xtrakw[k]
        assert arglist[0] is None or cdms2.isVariable(arglist[0])
        assert arglist[1] is None or cdms2.isVariable(arglist[1])
        assert isinstance(arglist[2], str)
        if not isinstance(arglist[3], vcsaddons.core.VCSaddon):
            assert isinstance(arglist[3], str)
        assert isinstance(arglist[4], str)

        if self.animate.is_playing():
            self.animate.stop()
            while self.animate.is_playing():
                pass
        # reset animation
        self.animate.create_flg = 0

        # Store the origin template. The template used to plot may be changed below by the
        # _create_random_template function, which copies templates for
        # modifications.
        template_origin = arglist[2]
        tmptmpl = self.gettemplate(arglist[2])
        tmptmpl.data._ratio = -999

        copy_mthd = None
        copy_tmpl = None
        if arglist[2] in ['default', 'default_dud']:
            if arglist[3] == 'taylordiagram':
                arglist[2] = "deftaylor"
                # copy_tmpl=self.createtemplate(source='deftaylor')
            # else:
            #    copy_tmpl=self.createtemplate(source=arglist[2])
        check_mthd = vcs.getgraphicsmethod(arglist[3], arglist[4])
        check_tmpl = vcs.gettemplate(arglist[2])
        # By defalut do the ratio thing for lat/lon and linear projection
        # but it can be overwritten by keyword
        doratio = str(keyargs.get('ratio', self.ratio)).strip().lower()
        if doratio[-1] == 't' and doratio[0] == '0':
            if float(doratio[:-1]) == 0.:
                doratio = '0'

        # Check for curvilinear grids, and wrap options !
        if arglist[0] is not None:
            inGrid = arglist[0].getGrid()
        else:
            inGrid = None
        if arglist[0] is not None and arglist[
                1] is None and arglist[3] == "meshfill":
            if isinstance(
                    inGrid, (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid)):
                g = self.getmeshfill(arglist[4])
                if 'wrap' not in keyargs and g.wrap == [0., 0.]:
                    keyargs['wrap'] = [0., 360.]
            else:
                if arglist[0].rank < 2:
                    arglist[3] = 'yxvsx'
                    arglist[4] = 'default'
                else:
                    xs = arglist[0].getAxis(-1)
                    ys = arglist[0].getAxis(-2)
                    if xs.isLongitude() and ys.isLatitude() and isinstance(
                            inGrid, cdms2.grid.TransientRectGrid):
                        arglist[1] = MV2.array(inGrid.getMesh())
                        if 'wrap' not in keyargs:
                            keyargs['wrap'] = [0., 360.]
                    elif ys.isLongitude() and xs.isLatitude() and isinstance(inGrid, cdms2.grid.TransientRectGrid):
                        arglist[1] = MV2.array(inGrid.getMesh())
                        if "wrap" not in keyargs:
                            keyargs['wrap'] = [360., 0.]
                    else:
                        arglist[3] = 'boxfill'
                        copy_mthd = vcs.creategraphicsmethod(
                            'boxfill',
                            'default')
                        check_mthd = copy_mthd
                        m = self.getmeshfill(arglist[4])
                        md = self.getmeshfill()
                        if md.levels != m.levels:
                            copy_mthd.boxfill_type = 'custom'
                            copy_mthd.levels = m.levels
                            copy_mthd.fillareacolors = m.fillareacolors
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
                            setattr(copy_mthd, att, getattr(m, att))
        elif arglist[0] is not None \
                and arglist[0].rank() < 2 \
                and arglist[3] in ['boxfill', 'default'] \
                and not isinstance(inGrid, cdms2.gengrid.AbstractGenericGrid):
            arglist[3] = '1d'
            try:
                tmp = self.getyxvsx(arglist[4])
                # tmp.list()
            except:
                arglist[4] = 'default'
        elif inGrid is not None and arglist[0] is not None and\
                isinstance(arglist[0], cdms2.avariable.AbstractVariable) and\
                not isinstance(inGrid, cdms2.grid.AbstractRectGrid) and\
                arglist[3] in ["boxfill", "default"] and arglist[4] == "default":
            arglist[3] = "meshfill"

# arglist[4]=copy_mthd.name
        # Ok let's check for meshfill needed
        if inGrid is not None and arglist[0] is not None and\
                isinstance(arglist[0], cdms2.avariable.AbstractVariable) and\
                not isinstance(arglist[0].getGrid(), cdms2.grid.AbstractRectGrid) and\
                arglist[3] not in ["meshfill", ]:
            raise RuntimeError("You are attempting to plot unstructured grid" +
                               "with a method that is not meshfill")
        # preprocessing for extra keyword (at-plotting-time options)
        cmds = {}
        # First of all a little preprocessing for legend !
        if 'legend' in keyargs.keys() and arglist[3] == 'boxfill':
            # we now have a possible problem since it can be legend for the
            # graphic method or the template!
            k = keyargs['legend']
            isboxfilllegend = 0
            if isinstance(k, type({})):
                #                print k.keys()
                # ok it's a dictionary if the key type is string then it's for
                # template, else it's for boxfill
                if not isinstance(k.keys()[0], type('')):
                    # not a string, therefore it's boxfill !
                    isboxfilllegend = 1
            elif type(k) in [type([]), type(())]:
                # ok it's a list therefore if the length is not 4 we have a
                # boxfill legend
                if len(k) != 4 and len(k) != 5:
                    isboxfilllegend = 1
                elif len(k) == 5:
                    if not type(k[4]) in [type({}), type(0), type(0.)]:
                        raise vcsError(
                            "Error, at-plotting-time argument 'legend' is ambiguous in this context\n"
                            "Cannot determine if it is template or boxfill keyword,\n tips to solve that:\n"
                            "\tif you aim at boxfill keyword, pass legend as a dictionary, \n"
                            "\tif your aim at template, add {'priority':1} at the end of the list\n"
                            "Currently legend is passed as:" +
                            repr(k))
                    elif not isinstance(k[4], type({})):
                        isboxfilllegend = 1
                else:
                    # ok it's length 4, now the only hope left is that not all
                    # values are between 0 and 1
                    for i in range(4):
                        if k[i] > 1. or k[i] < 0.:
                            isboxfilllegend = 1
                    if isboxfilllegend == 0:
                        raise vcsError(
                            "Error, at-plotting-time argument 'legend' is ambiguous"
                            "in this context\nCannot determine if it is template or boxfill keyword,\n "
                            "tips to solve that:\n\tif you aim at boxfill keyword, pass legend as a dictionary, \n"
                            "\tif your aim at template, add {'priority':1} at the end of the list\n"
                            "Currently legend is passed as:" +
                            repr(k))

            # ok it is for the boxfill let's do it
            if isboxfilllegend:
                if copy_mthd is None:
                    copy_mthd = vcs.creategraphicsmethod(
                        arglist[3],
                        arglist[4])
                copy_mthd.legend = k
                del(keyargs['legend'])
                check_mthd = copy_mthd

        # There is no way of knowing if the template has been called prior to this plot command.
        # So it is done here to make sure that the template coordinates are normalized. If already
        # normalized, then no change will to the template.
        try:
            self.gettemplate(template_origin)
        except:
            pass

        # Creates dictionary/list to remember what we changed
        slab_changed_attributes = {}
        axes_changed = {}
        axes_changed2 = {}

        # loops through possible keywords for graphic method
        for p in keyargs.keys():
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
                'labelskipdistance',
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
                if p not in ['worldcoordinate', ]:  # not a special keywords
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    setattr(copy_mthd, p, keyargs[p])
                elif p == 'worldcoordinate':
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    setattr(copy_mthd, 'datawc_x1', keyargs[p][0])
                    setattr(copy_mthd, 'datawc_x2', keyargs[p][1])
                    setattr(copy_mthd, 'datawc_y1', keyargs[p][2])
                    setattr(copy_mthd, 'datawc_y2', keyargs[p][3])
                del(keyargs[p])
            # Now template settings keywords
            elif p in [
                'viewport',
            ]:
                if copy_tmpl is None:
                    copy_tmpl = vcs.createtemplate(source=arglist[2])
                    check_tmpl = copy_tmpl
                copy_tmpl.reset(
                    'x',
                    keyargs[p][0],
                    keyargs[p][1],
                    copy_tmpl.data.x1,
                    copy_tmpl.data.x2)
                copy_tmpl.reset(
                    'y',
                    keyargs[p][2],
                    keyargs[p][3],
                    copy_tmpl.data.y1,
                    copy_tmpl.data.y2)
                del(keyargs[p])
            # Now template and x/y related stuff (1 dir only)
            elif p[1:] in [
                'label1',
                'label2',
            ]:
                if copy_tmpl is None:
                    copy_tmpl = vcs.createtemplate(source=arglist[2])
                    check_tmpl = copy_tmpl
                k = keyargs[p]
                # not a list means only priority set
                if not isinstance(k, type([])):
                    if not isinstance(k, type({})):
                        setattr(getattr(copy_tmpl, p), 'priority', k)
                    elif isinstance(k, type({})):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl, p), kk, k[kk])
                else:
                    if p[0] == 'x':
                        setattr(getattr(copy_tmpl, p), 'y', k[0])
                    else:
                        setattr(getattr(copy_tmpl, p), 'x', k[0])
                    if isinstance(k[-1], type({})):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl, p), kk, k[-1][kk])

                del(keyargs[p])
            # Now template and x1 and x2/y1 and y2 related stuff (1 dir only)
            elif p[1:] in [
                'mintic1',
                'mintic2',
                'tic1',
                'tic2',
            ]:
                if copy_tmpl is None:
                    copy_tmpl = vcs.createtemplate(source=arglist[2])
                    check_tmpl = copy_tmpl

                k = keyargs[p]
                # not a list means only priority set
                if not isinstance(k, type([])):
                    if not isinstance(k, type({})):
                        setattr(getattr(copy_tmpl, p), 'priority', k)
                    elif isinstance(k, type({})):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl, p), kk, k[kk])
                else:
                    if p[0] == 'x':
                        setattr(getattr(copy_tmpl, p), 'y1', k[0])
                        setattr(getattr(copy_tmpl, p), 'y2', k[1])
                    else:
                        setattr(getattr(copy_tmpl, p), 'x1', k[0])
                        setattr(getattr(copy_tmpl, p), 'x2', k[1])
                    if isinstance(k[-1], type({})):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl, p), kk, k[-1][kk])

                del(keyargs[p])
            # Now template with x1, x2, x3, x4, x5
            elif p in [
                'box1', 'box2', 'box3', 'box4',
                'line1', 'line2', 'line3', 'line4',
                'data', 'legend',
            ]:
                if copy_tmpl is None:
                    copy_tmpl = vcs.createtemplate(source=arglist[2])
                    check_tmpl = copy_tmpl
                k = keyargs[p]
                # not a list means only priority set
                if not isinstance(k, type([])):
                    if not isinstance(k, type({})):
                        setattr(getattr(copy_tmpl, p), 'priority', k)
                    elif isinstance(k, type({})):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl, p), kk, k[kk])
                else:
                    setattr(getattr(copy_tmpl, p), 'x1', k[0])
                    setattr(getattr(copy_tmpl, p), 'x2', k[1])
                    setattr(getattr(copy_tmpl, p), 'y1', k[2])
                    setattr(getattr(copy_tmpl, p), 'y2', k[3])
                    if isinstance(k[-1], type({})):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl, p), kk, k[-1][kk])

                del(keyargs[p])
            # Now MV2 related keywords
            # Charles note: It's here that we need to remember what changed so
            # i can unset it later
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
                k = keyargs[p]
                if copy_tmpl is None:
                    copy_tmpl = vcs.createtemplate(source=arglist[2])
                    check_tmpl = copy_tmpl
                if getattr(getattr(check_tmpl, p), 'priority') == 0:
                    setattr(getattr(copy_tmpl, p), 'priority', 1)
                if not isinstance(
                        k, list):  # not a list means only priority set
                    if isinstance(k, dict):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl, p), kk, k[kk])
                    elif isinstance(k, int):
                        setattr(getattr(copy_tmpl, p), 'priority', k)
                    elif isinstance(k, str):
                        slab_changed_attributes[p] = k
# if hasattr(arglist[0],p):
# slab_changed_attributes[p]=getattr(arglist[0],p)
# else:
# slab_created_attributes.append(p)
# setattr(arglist[0],p,k)
                else:
                    # if hasattr(arglist[0],p):
                    # slab_changed_attributes[p]=getattr(arglist[0],p)
                    # else:
                    # slab_created_attributes.append(p)
                    # setattr(arglist[0],p,k[0])
                    slab_changed_attributes[p] = k[0]
                    setattr(getattr(copy_tmpl, p), 'x', k[1])
                    setattr(getattr(copy_tmpl, p), 'y', k[2])
                    if isinstance(k[-1], type({})):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl, p), kk, k[-1][kk])

                del(keyargs[p])
            # Now Axis related keywords
            elif p[1:] in [
                'name',
                'value',
                'units',
            ]:
                if p[0] == 'x':
                    ax = arglist[0].getAxis(-1)
                    if ax is not None:
                        ax = ax.clone()
                    if 'xaxis' in keyargs:
                        ax = keyargs['xaxis'].clone()
                        keyargs['xaxis'] = ax
                    g = arglist[0].getGrid()
                    if isinstance(g, (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid)) or arglist[
                            3].lower() == 'meshfill':
                        ax = None
                        del(g)
                elif p[0] == 'y':
                    ax = arglist[0].getAxis(-2)
                    if ax is not None:
                        ax = ax.clone()
                    if 'yaxis' in keyargs:
                        ax = keyargs['yaxis'].clone()
                        keyargs['yaxis'] = ax
                    g = arglist[0].getGrid()
                    if isinstance(g, (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid)) or arglist[
                            3].lower() == 'meshfill':
                        ax = None
                        del(g)
                elif p[0] == 'z':
                    ax = arglist[0].getLevel()
                    if ax is not None:
                        ax = ax.clone()
                elif p[0] == 't':
                    ax = arglist[0].getTime()
                    if ax is not None:
                        ax = ax.clone()
                if ax is not None:
                    ids = arglist[0].getAxisIds()
                    for i in range(len(ids)):
                        if ax.id == ids[i]:
                            if i not in axes_changed:
                                axes_changed[i] = ax
                    if arglist[1] is not None:
                        ids2 = arglist[1].getAxisIds()
                        for i in range(len(ids2)):
                            if ax.id == ids2[i]:
                                if i not in axes_changed2:
                                    axes_changed2[i] = ax
                if copy_tmpl is None:
                    check_tmpl = copy_tmpl = vcs.createtemplate(
                        source=arglist[2])
                k = keyargs[p]
                if getattr(getattr(copy_tmpl, p), 'priority') == 0:
                    setattr(getattr(copy_tmpl, p), 'priority', 1)
                # not a list means only priority set
                if not isinstance(k, type([])):
                    if isinstance(k, type({})):
                        for kk in k.keys():
                            setattr(getattr(copy_tmpl, p), kk, k[kk])
                    elif isinstance(k, type(0)):
                        setattr(getattr(copy_tmpl, p), 'priority', k)
                    elif isinstance(k, str):
                        if p[1:] != 'name':
                            setattr(ax, p[1:], k)
                        else:
                            try:
                                setattr(ax, 'id', k)
                            except:
                                # print err
                                pass
                    elif k is None:
                        if p[1:] != 'name':
                            setattr(ax, p[1:], '')
                        else:
                            setattr(ax, 'id', '')

                else:
                    if p[1:] != 'name':
                        setattr(ax, p[1:], k[0])
                    else:
                        setattr(ax, 'id', k)
                    setattr(getattr(copy_tmpl, p), 'x', k[1])
                    setattr(getattr(copy_tmpl, p), 'y', k[2])
                    if isinstance(k[-1], type({})):
                        for kk in k[-1].keys():
                            setattr(getattr(copy_tmpl, p), kk, k[-1][kk])

                del(keyargs[p])
            # Finally take care of commands
            elif p in [
                'pdf', 'ps', 'postscript', 'gif', 'ras',
            ]:
                cmds[p] = keyargs[p]
                del(keyargs[p])

        # Check if datawc has time setting in it
        # if copy_mthd is None:
        #     if arglist[3]!='default':
        #         copy_mthd=vcs.creategraphicsmethod(arglist[3],arglist[4])
        #         print "5555555"
        #     else:
        #         copy_mthd=vcs.creategraphicsmethod('boxfill',arglist[4])
        #         print "5555555bbbbbbbb"
        #     wasnone=1
# and (type(copy_mthd.datawc_x1) in [type(cdtime.comptime(1900)),type(cdtime.reltime(0,'days since 1900'))] or \
# type(copy_mthd.datawc_x2) in
# [type(cdtime.comptime(1900)),type(cdtime.reltime(0,'days since 1900'))])
# \

        if (hasattr(check_mthd, 'datawc_x1') and hasattr(check_mthd, 'datawc_x2')) \
                and arglist[0].getAxis(-1).isTime() \
                and check_mthd.xticlabels1 == '*' \
                and check_mthd.xticlabels2 == '*' \
                and check_mthd.xmtics1 in ['*', ''] \
                and check_mthd.xmtics2 in ['*', ''] \
                and not (check_mthd.g_name in ['G1d'] and
                         (check_mthd.flip is True or arglist[1] is not None) and
                         arglist[0].ndim == 1):  # used to be GXy GX
            ax = arglist[0].getAxis(-1).clone()
            ids = arglist[0].getAxisIds()
            for i in range(len(ids)):
                if ax.id == ids[i]:
                    if i not in axes_changed:
                        ax = ax.clone()
                        axes_changed[i] = ax
                    break
            if arglist[1] is not None:
                ids2 = arglist[1].getAxisIds()
                for i in range(len(ids2)):
                    if ax.id == ids2[i]:
                        if i not in axes_changed2:
                            axes_changed2[i] = ax
            try:
                ax.toRelativeTime(
                    check_mthd.datawc_timeunits,
                    check_mthd.datawc_calendar)
                convertedok = True
            except:
                convertedok = False
            # and check_mthd.g_name not in ["G1d",]: #used to be Gsp
            if (check_mthd.xticlabels1 ==
                    '*' or check_mthd.xticlabels2 == '*') and convertedok:
                convert_datawc = False
                for cax in axes_changed.keys():
                    if axes_changed[cax] == ax:
                        convert_datawc = True
                        break
                if convert_datawc:
                    oax = arglist[0].getAxis(cax).clone()
                    t = type(check_mthd.datawc_x1)
                    if t not in [type(cdtime.reltime(0, 'months since 1900')), type(
                            cdtime.comptime(1900))]:
                        if copy_mthd is None:
                            copy_mthd = vcs.creategraphicsmethod(
                                arglist[3],
                                arglist[4])
                            check_mthd = copy_mthd
                        if check_mthd.datawc_x1 > 9.E19:
                            copy_mthd.datawc_x1 = cdtime.reltime(
                                oax[0],
                                oax.units).tocomp(
                                oax.getCalendar()).torel(
                                copy_mthd.datawc_timeunits,
                                copy_mthd.datawc_calendar)
                        else:
                            copy_mthd.datawc_x1 = cdtime.reltime(
                                copy_mthd.datawc_x1,
                                oax.units).tocomp(
                                oax.getCalendar()).torel(
                                copy_mthd.datawc_timeunits,
                                copy_mthd.datawc_calendar)
                        if copy_mthd.datawc_x2 > 9.E19:
                            copy_mthd.datawc_x2 = cdtime.reltime(oax[-1], oax.units).tocomp(
                                oax.getCalendar()).torel(copy_mthd.datawc_timeunits, copy_mthd.datawc_calendar)
                        else:
                            copy_mthd.datawc_x2 = cdtime.reltime(
                                copy_mthd.datawc_x2,
                                oax.units).tocomp(
                                oax.getCalendar()).torel(
                                copy_mthd.datawc_timeunits,
                                copy_mthd.datawc_calendar)
                if copy_mthd.xticlabels1 == '*':
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    copy_mthd.xticlabels1 = vcs.generate_time_labels(
                        copy_mthd.datawc_x1,
                        copy_mthd.datawc_x2,
                        copy_mthd.datawc_timeunits,
                        copy_mthd.datawc_calendar)
                if copy_mthd.xticlabels2 == '*':
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    copy_mthd.xticlabels2 = vcs.generate_time_labels(
                        copy_mthd.datawc_x1,
                        copy_mthd.datawc_x2,
                        copy_mthd.datawc_timeunits,
                        copy_mthd.datawc_calendar)
        elif not (getattr(check_mthd, 'g_name', '') == 'Gfm' and
                  isinstance(arglist[0].getGrid(), (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid))):
            try:
                if arglist[0].getAxis(-1).isTime():  # used to GXy
                    if check_mthd.xticlabels1 == '*' and check_mthd.xticlabels2 == '*' and\
                            not (check_mthd.g_name == 'G1d' and check_mthd.flip) and\
                            check_mthd.g_name not in ['G1d']:  # used to be GSp
                        if copy_mthd is None:
                            copy_mthd = vcs.creategraphicsmethod(
                                arglist[3],
                                arglist[4])
                            check_mthd = copy_mthd
                        t = arglist[0].getAxis(-1).clone()
                        timeunits = t.units
                        calendar = t.getCalendar()
                        t0 = cdtime.reltime(t[0], timeunits)
                        t1 = cdtime.reltime(t[-1], timeunits)
                        copy_mthd.xticlabels1 = vcs.generate_time_labels(
                            t0,
                            t1,
                            timeunits,
                            calendar)
            except:
                pass

        if (hasattr(check_mthd, 'datawc_y1') and hasattr(check_mthd, 'datawc_y2'))\
                and check_mthd.yticlabels1 == '*' \
                and check_mthd.yticlabels2 == '*' \
                and check_mthd.ymtics1 in ['*', ''] \
                and check_mthd.ymtics2 in ['*', ''] \
                and arglist[0].getAxis(-2).isTime() \
                and (arglist[0].ndim > 1 or (check_mthd.g_name == 'G1d' and check_mthd.flip)) \
                and not (check_mthd.g_name == 'Gfm' and
                         isinstance(arglist[0].getGrid(),
                                    (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid))):  # GXy
            ax = arglist[0].getAxis(-2).clone()
            # used to be  Sp
            if check_mthd.g_name == "G1d" and check_mthd.linewidth == 0:
                ax = arglist[1].getAxis(-2).clone()
                axes_changed2 = {}
            ids = arglist[0].getAxisIds()
            for i in range(len(ids)):
                if ax.id == ids[i]:
                    if i not in axes_changed:
                        ax = ax.clone()
                        axes_changed[i] = ax
# else:
# ax=axes_changed[i]
                    break
            if arglist[1] is not None:
                ids2 = arglist[1].getAxisIds()
                for i in range(len(ids2)):
                    if ax.id == ids2[i]:
                        if i not in axes_changed2:
                            axes_changed2[i] = ax
# else:
# ax=axes_changed2[i]
                        break
            try:
                ax.toRelativeTime(
                    check_mthd.datawc_timeunits,
                    check_mthd.datawc_calendar)
                convertedok = True
            except:
                convertedok = False
            if (check_mthd.yticlabels1 ==
                    '*' or check_mthd.yticlabels2 == '*') and convertedok:
                convert_datawc = False
                A = axes_changed
                # GSp
                if check_mthd.g_name == "G1d" and check_mthd.linewidth == 0:
                    A = axes_changed2
                for cax in A.keys():
                    if A[cax] is ax:
                        convert_datawc = True
                        break
                if convert_datawc:
                    oax = arglist[0].getAxis(cax).clone()
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    if copy_mthd.datawc_y1 > 9.E19:
                        copy_mthd.datawc_y1 = cdtime.reltime(
                            oax[0],
                            oax.units).tocomp(
                            oax.getCalendar()).torel(
                            copy_mthd.datawc_timeunits,
                            copy_mthd.datawc_calendar)
                    else:
                        copy_mthd.datawc_y1 = cdtime.reltime(
                            copy_mthd.datawc_y1,
                            oax.units).tocomp(
                            oax.getCalendar()).torel(
                            copy_mthd.datawc_timeunits,
                            copy_mthd.datawc_calendar)
                    if copy_mthd.datawc_y2 > 9.E19:
                        copy_mthd.datawc_y2 = cdtime.reltime(oax[-1], oax.units).tocomp(
                            oax.getCalendar()).torel(copy_mthd.datawc_timeunits, copy_mthd.datawc_calendar)
                    else:
                        copy_mthd.datawc_y2 = cdtime.reltime(
                            copy_mthd.datawc_y2,
                            oax.units).tocomp(
                            oax.getCalendar()).torel(
                            copy_mthd.datawc_timeunits,
                            copy_mthd.datawc_calendar)
                if check_mthd.yticlabels1 == '*':
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    copy_mthd.yticlabels1 = vcs.generate_time_labels(
                        copy_mthd.datawc_y1,
                        copy_mthd.datawc_y2,
                        copy_mthd.datawc_timeunits,
                        copy_mthd.datawc_calendar)
                if check_mthd.yticlabels2 == '*':
                    if copy_mthd is None:
                        copy_mthd = vcs.creategraphicsmethod(
                            arglist[3],
                            arglist[4])
                        check_mthd = copy_mthd
                    copy_mthd.yticlabels2 = vcs.generate_time_labels(
                        copy_mthd.datawc_y1,
                        copy_mthd.datawc_y2,
                        copy_mthd.datawc_timeunits,
                        copy_mthd.datawc_calendar)
        elif not (getattr(check_mthd, 'g_name', '') == 'Gfm' and
                  isinstance(arglist[0].getGrid(),
                             (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid))):
            try:
                # ['GYx','GXy','GXY','GSp']:
                if arglist[
                        0].getAxis(-2).isTime() and arglist[0].ndim > 1 and copy_mthd.g_name not in ["G1d", ]:
                    if check_mthd.yticlabels1 == '*' and check_mthd.yticlabels2 == '*':
                        if copy_mthd is None:
                            copy_mthd = vcs.creategraphicsmethod(
                                arglist[3],
                                arglist[4])
                            check_mthd = copy_mthd
# print
# copy_mthd.datawc_y1,copy_mthd.datawc_y2,copy_mthd.datawc_timeunits,copy_mthd.datawc_calendar
                        t = arglist[0].getAxis(-2).clone()
                        timeunits = t.units
                        calendar = t.getCalendar()
                        t0 = cdtime.reltime(t[0], timeunits)
                        t1 = cdtime.reltime(t[-1], timeunits)
                        copy_mthd.yticlabels1 = vcs.generate_time_labels(
                            t0,
                            t1,
                            timeunits,
                            calendar)
            except:
                pass

        def clean_val(value):
            if numpy.allclose(value, 0.):
                return 0.
            elif value < 0:
                sign = -1
                value = -value
            else:
                sign = 1
            i = int(numpy.log10(value))
            if i > 0:
                j = i
                k = 10.
            else:
                j = i - 1
                k = 10.
            v = int(value / numpy.power(k, j)) * numpy.power(k, j)
            return v * sign

        def mkdic(method, values):
            if method == 'area_wt':
                func = numpy.sin
                func2 = numpy.arcsin
            elif method == 'exp':
                func = numpy.exp
                func2 = numpy.log
            elif method == 'ln':
                func = numpy.log
                func2 = numpy.exp
            elif method == 'log10':
                func = numpy.log10
            vals = []
            for v in values:
                if method == 'area_wt':
                    vals.append(func(v * numpy.pi / 180.))
                else:
                    vals.append(func(v))
            min, max = vcs.minmax(vals)
            levs = vcs.mkscale(min, max)
# levs=vcs.mkevenlevels(min,max)
            vals = []
            for l in levs:
                if method == 'log10':
                    v = numpy.power(10, l)
                elif method == 'area_wt':
                    v = func2(l) / numpy.pi * 180.
                else:
                    v = func2(l)
                vals.append(clean_val(v))
            dic = vcs.mklabels(vals)
            dic2 = {}
            for k in dic.keys():
                try:
                    if method == 'area_wt':
                        dic2[func(k * numpy.pi / 180.)] = dic[k]
                    else:
                        dic2[func(k)] = dic[k]
                except:
                    pass
            return dic2

        def set_convert_labels(copy_mthd, test=0):
            did_something = False
            for axc in ['x', 'y']:
                try:
                    mthd = getattr(copy_mthd, axc + 'axisconvert')
                    if mthd != 'linear':
                        for num in ['1', '2']:
                            if getattr(
                                    copy_mthd, axc + 'ticlabels' + num) == '*':
                                if axc == 'x':
                                    axn = -1
                                else:
                                    axn = -2
                                dic = mkdic(mthd, arglist[0].getAxis(axn)[:])
                                if test == 0:
                                    setattr(
                                        copy_mthd,
                                        axc +
                                        'ticlabels' +
                                        num,
                                        dic)
                                did_something = True
                except:
                    pass
            return did_something

        if set_convert_labels(check_mthd, test=1):
            if copy_mthd is None:
                copy_mthd = vcs.creategraphicsmethod(arglist[3], arglist[4])
                check_mthd = copy_mthd
                set_convert_labels(copy_mthd)
        if copy_mthd is None:
            copy_mthd = vcs.creategraphicsmethod(arglist[3], arglist[4])
            check_mthd = copy_mthd

        x = None
        y = None
        try:
            if arglist[0].getAxis(-1).isLongitude():
                x = "longitude"
            elif arglist[0].getAxis(-1).isLatitude():
                x = "latitude"
            # in ["GXy","GXY"]:
            if check_mthd.g_name == "G1d" and (
                    check_mthd.flip or arglist[1] is not None):
                datawc_x1 = MV2.minimum(arglist[0])
                datawc_x2 = MV2.maximum(arglist[0])
                x = None
            else:
                try:
                    if arglist[0].getAxis(-1).isCircularAxis():
                        datawc_x1 = arglist[0].getAxis(-1)[0]
                    else:
                        datawc_x1 = arglist[0].getAxis(-1).getBounds()[0][0]
                except:
                    datawc_x1 = arglist[0].getAxis(-1)[0]
                try:
                    if arglist[0].getAxis(-1).isCircularAxis():
                        datawc_x2 = arglist[0].getAxis(-1)[-1]
                    else:
                        datawc_x2 = arglist[0].getAxis(-1).getBounds()[-1][1]
                except:
                    datawc_x2 = arglist[0].getAxis(-1)[-1]
            if arglist[0].getAxis(-2).isLongitude():
                y = "longitude"
            elif arglist[0].getAxis(-2).isLatitude():
                y = "latitude"

            if check_mthd.g_name == "G1d" and not check_mthd.flip and arglist[
                    1] is None:  # in ["GYx",]:
                datawc_y1 = MV2.minimum(arglist[0])
                datawc_y2 = MV2.maximum(arglist[0])
                y = None
            # in ["GYX",]:
            elif check_mthd.g_name == "G1d" and arglist[1] is not None:
                datawc_y1 = MV2.minimum(arglist[1])
                datawc_y2 = MV2.maximum(arglist[1])
                y = None
            else:
                try:
                    datawc_y1 = arglist[0].getAxis(-2).getBounds()[0][0]
                except:
                    datawc_y1 = arglist[0].getAxis(-2)[0]
                try:
                    datawc_y2 = arglist[0].getAxis(-2).getBounds()[-1][1]
                except:
                    datawc_y2 = arglist[0].getAxis(-2)[-1]
            if isinstance(arglist[0].getGrid(
            ), (cdms2.gengrid.AbstractGenericGrid, cdms2.hgrid.AbstractCurveGrid)):
                x = "longitude"
                y = "latitude"
        except:
            pass
        try:
            copy_mthd = vcs.setTicksandLabels(
                check_mthd,
                copy_mthd,
                datawc_x1,
                datawc_x2,
                datawc_y1,
                datawc_y2,
                x=x,
                y=y)
        except:
            pass

        if copy_mthd is not None:
            arglist[4] = copy_mthd.name
        if copy_tmpl is not None:
            arglist[2] = copy_tmpl.name

        # End of preprocessing !

        # get the background value
        bg = keyargs.get('bg', 0)

        if isinstance(arglist[3], str) and arglist[
                3].lower() == 'taylordiagram':
            for p in slab_changed_attributes.keys():
                if hasattr(arglist[0], p):
                    tmp = getattr(arglist[0], p)
                else:
                    tmp = (None, None)
                setattr(arglist[0], p, slab_changed_attributes[p])
                slab_changed_attributes[p] = tmp
            # first look at the extra arguments and make sure there is no
            # duplicate
            for k in keyargs.keys():
                if k not in ['template', 'skill', 'bg']:
                    del(keyargs[k])
                if k == 'template':
                    arglist[2] = keyargs[k]
                    del(keyargs[k])
            # look through the available taylordiagram methods and use the plot
            # function
            t = vcs.elements["taylordiagram"].get(arglist[4], None)
            if t is None:
                raise ValueError(
                    "unknown taylordiagram graphic method: %s" %
                    arglist[4])
            t.plot(arglist[0], canvas=self, template=arglist[2], **keyargs)

            dname = keyargs.get("display_name")
            if dname is not None:
                dn = vcs.elements["display"][dname]
            else:
                nm, src = self.check_name_source(None, "default", "display")
                dn = displayplot.Dp(nm)
            dn.continents = self.getcontinentstype()
            dn.continents_line = self.getcontinentsline()
            dn.template = arglist[2]
            dn.g_type = arglist[3]
            dn.g_name = arglist[4]
            dn.array = arglist[:2]
            dn.extradisplays = t.displays
            for p in slab_changed_attributes.keys():
                tmp = slab_changed_attributes[p]
                if tmp == (None, None):
                    delattr(arglist[0], p)
                else:
                    setattr(arglist[0], p, tmp)
            dn.newelements = self.__new_elts(original_elts, new_elts)

            return dn
        else:  # not taylor diagram
            if isinstance(arglist[3], vcsaddons.core.VCSaddon):
                gm = arglist[3]
            else:
                tp = arglist[3]
                if tp == "text":
                    tp = "textcombined"
                elif tp == "default":
                    tp = "boxfill"
                elif tp in ("xvsy", "xyvsy", "yxvsx", "scatter"):
                    tp = "1d"
                gm = vcs.elements[tp][arglist[4]]
                if hasattr(gm, "priority") and gm.priority == 0:
                    return
            p = self.getprojection(gm.projection)
            if p.type in no_deformation_projections and (
                    doratio == "0" or doratio[:4] == "auto"):
                doratio = "1t"
            for keyarg in keyargs.keys():
                if keyarg not in self.__class__._plot_keywords_ + self.backend._plot_keywords:
                    if keyarg in self.__class__._deprecated_plot_keywords_:
                        warnings.warn("Deprecation Warning: Keyword '%s' will be removed in the next version"
                                      "of UV-CDAT." % keyarg)
                    else:
                        warnings.warn(
                            'Unrecognized vcs plot keyword: %s, assuming backend (%s) keyword' %
                            (keyarg, self.backend.type))

            if arglist[0] is not None or 'variable' in keyargs:
                arglist[0] = self._reconstruct_tv(arglist, keyargs)
                # Now applies the attributes change
                for p in slab_changed_attributes.keys():
                    if hasattr(arglist[0], p):
                        tmp = getattr(arglist[0], p)
                    else:
                        tmp = (None, None)
                    setattr(arglist[0], p, slab_changed_attributes[p])
                    slab_changed_attributes[p] = tmp
                # Now applies the axes changes
                for i in axes_changed.keys():
                    arglist[0].setAxis(i, axes_changed[i])
                for i in axes_changed2.keys():
                    arglist[1].setAxis(i, axes_changed2[i])
            # Check to make sure that you have at least 2 dimensions for the follow graphics methods
            # Flipping the order to avoid the tv not exist problem
            if (arglist[3] in ['boxfill', 'isofill', 'isoline', 'vector']) and (
                    len(arglist[0].shape) < 2):
                raise vcsError(
                    'Invalid number of dimensions for %s' %
                    arglist[3])

            # Ok now does the linear projection for lat/lon ratio stuff
            if arglist[3] in ['marker', 'line', 'fillarea', 'text']:
                # fist create a dummy template
                t = self.createtemplate()
                # Now creates a copy of the primitives, in case it's used on
                # other canvases with diferent ratios
                if arglist[3] == 'text':
                    nms = arglist[4].split(":::")
                    p = self.createtext(Tt_source=nms[0], To_source=nms[1])
                elif arglist[3] == 'marker':
                    p = self.createmarker(source=arglist[4])
                elif arglist[3] == 'line':
                    p = self.createline(source=arglist[4])
                elif arglist[3] == 'fillarea':
                    p = self.createfillarea(source=arglist[4])
                t.data.x1 = p.viewport[0]
                t.data.x2 = p.viewport[1]
                t.data.y1 = p.viewport[2]
                t.data.y2 = p.viewport[3]

                proj = self.getprojection(p.projection)
                if proj.type in no_deformation_projections and (
                        doratio == "0" or doratio[:4] == "auto"):
                    doratio = "1t"

                if proj.type == 'linear' and doratio[:4] == 'auto':
                    lon1, lon2, lat1, lat2 = p.worldcoordinate
                    t.ratio_linear_projection(
                        lon1,
                        lon2,
                        lat1,
                        lat2,
                        None,
                        box_and_ticks=box_and_ticks)
                    p.viewport = [t.data.x1, t.data.x2, t.data.y1, t.data.y2]
                    arglist[4] = p.name
                elif doratio not in ['0', 'off', 'none', 'auto', 'autot']:
                    if doratio[-1] == 't':
                        doratio = doratio[:-1]
                    Ratio = float(doratio)
                    t.ratio(Ratio)
                    p.viewport = [t.data.x1, t.data.x2, t.data.y1, t.data.y2]
                    if arglist[3] == 'text':
                        arglist[4] = p.Tt_name + ':::' + p.To_name
                    else:
                        arglist[4] = p.name
                else:
                    if arglist[3] == 'text' and keyargs.get(
                            "donotstoredisplay", False) is True:
                        sp = p.name.split(":::")
                        del(vcs.elements["texttable"][sp[0]])
                        del(vcs.elements["textorientation"][sp[1]])
                        del(vcs.elements["textcombined"][p.name])
                    elif arglist[3] == 'marker':
                        del(vcs.elements["marker"][p.name])
                    elif arglist[3] == 'line':
                        del(vcs.elements["line"][p.name])
                    elif arglist[3] == 'fillarea':
                        del(vcs.elements["fillarea"][p.name])
                # cleanup temp template
                del(vcs.elements["template"][t.name])
            elif (arglist[3] in ['boxfill', 'isofill', 'isoline',
                                 'vector', 'meshfill'] or
                  isinstance(arglist[3], vcsaddons.core.VCSaddon)) and \
                    doratio in ['auto', 'autot'] and not (doratio == 'auto' and arglist[2] == 'ASD'):
                box_and_ticks = 0
                if doratio[-1] == 't' or template_origin == 'default':
                    box_and_ticks = 1

                if isinstance(arglist[3], vcsaddons.core.VCSaddon):
                    gm = arglist[3]
                else:
                    tp = arglist[3]
                    if tp == "text":
                        tp = "textcombined"
                    gm = vcs.elements[tp][arglist[4]]
                p = self.getprojection(gm.projection)
                if p.type in no_deformation_projections:
                    doratio = "1t"
                if p.type == 'linear':
                    if gm.g_name == 'Gfm':
                        if self.isplottinggridded:
                            # TODO: This computation is wrong as a meshfill can be wrapped.
                            # this means that we have to create the VTK dataset before
                            # we know the actual lon1, lon2.
                            lon1, lon2 = vcs.minmax(arglist[1][..., :, 1, :])
                            lat1, lat2 = vcs.minmax(arglist[1][..., :, 0, :])
                            if lon2 - lon1 > 360:
                                lon1, lon2 = 0., 360.
                            if gm.datawc_x1 < 9.99E19:
                                lon1 = gm.datawc_x1
                            if gm.datawc_x2 < 9.99E19:
                                lon2 = gm.datawc_x2
                            if gm.datawc_y1 < 9.99E19:
                                lat1 = gm.datawc_y1
                            if gm.datawc_y2 < 9.99E19:
                                lat2 = gm.datawc_y2
                            if copy_tmpl is None:
                                copy_tmpl = vcs.createtemplate(
                                    source=arglist[2])
                                arglist[2] = copy_tmpl.name
                            copy_tmpl.ratio_linear_projection(
                                lon1,
                                lon2,
                                lat1,
                                lat2,
                                None,
                                box_and_ticks=box_and_ticks)
                    elif arglist[0].getAxis(-1).isLongitude() and arglist[0].getAxis(-2).isLatitude():
                        if copy_tmpl is None:
                            copy_tmpl = vcs.createtemplate(source=arglist[2])
                        if gm.datawc_x1 < 9.99E19:
                            lon1 = gm.datawc_x1
                        else:
                            lon1 = min(arglist[0].getAxis(-1))
                        if gm.datawc_x2 < 9.99E19:
                            lon2 = gm.datawc_x2
                        else:
                            lon2 = max(arglist[0].getAxis(-1))
                        if gm.datawc_y1 < 9.99E19:
                            lat1 = gm.datawc_y1
                        else:
                            lat1 = min(arglist[0].getAxis(-2))
                        if gm.datawc_y2 < 9.99E19:
                            lat2 = gm.datawc_y2
                        else:
                            lat2 = max(arglist[0].getAxis(-2))
                        copy_tmpl.ratio_linear_projection(
                            lon1,
                            lon2,
                            lat1,
                            lat2,
                            None,
                            box_and_ticks=box_and_ticks,
                            x=self)
                        arglist[2] = copy_tmpl.name
            elif not (doratio in ['0', 'off', 'none', 'auto', 'autot']) or\
                (arglist[3] in ['boxfill', 'isofill', 'isoline', 'vector', 'meshfill'] and
                 str(doratio).lower() in ['auto', 'autot']) and arglist[2] != 'ASD':
                box_and_ticks = 0
                if doratio[-1] == 't' or template_origin == 'default':
                    box_and_ticks = 1
                    if doratio[-1] == 't':
                        doratio = doratio[:-1]
                try:
                    Ratio = float(doratio)
                except:
                    Ratio = doratio
                if copy_tmpl is None:
                    copy_tmpl = vcs.createtemplate(source=arglist[2])
                    arglist[2] = copy_tmpl.name
                copy_tmpl.ratio(Ratio, box_and_ticks=box_and_ticks, x=self)

            if hasattr(self, '_isplottinggridded'):
                del(self._isplottinggridded)
            # Get the continents for animation generation
            self.animate.continents_value = self._continentspath()

            # Get the option for doing graphics in the background.
            if bg:
                arglist.append(True)
            else:
                arglist.append(False)
            if arglist[3] == 'scatter':
                if not (
                        numpy.equal(arglist[0].getAxis(-1)[:], arglist[1].getAxis(-1)[:]).all()):
                    raise vcsError(
                        'Error - ScatterPlot requires X and Y defined in the same place')
            if arglist[3] == 'vector':
                if not (numpy.equal(arglist[0].getAxis(-1)[:], arglist[1].getAxis(-1)[:]).all()) or not(
                        numpy.equal(arglist[0].getAxis(-2)[:], arglist[1].getAxis(-2)[:]).all()):
                    raise vcsError(
                        'Error - VECTOR components must be on the same grid.')
            if "bg" in keyargs:
                del(keyargs["bg"])
            if isinstance(arglist[3], vcsaddons.core.VCSaddon):
                if arglist[1] is None:
                    dn = arglist[3].plot(
                        arglist[0],
                        template=arglist[2],
                        bg=bg,
                        x=self,
                        **keyargs)
                else:
                    dn = arglist[3].plot(
                        arglist[0],
                        arglist[1],
                        template=arglist[2],
                        bg=bg,
                        x=self,
                        **keyargs)
            else:
                returned_kargs = self.backend.plot(*arglist, **keyargs)
                if not keyargs.get("donotstoredisplay", False):
                    dname = keyargs.get("display_name")
                    if dname is not None:
                        dn = vcs.elements['display'][dname]
                    else:
                        nm, src = self.check_name_source(
                            None, "default", "display")
                        dn = displayplot.Dp(nm, parent=self)
                    dn.template = arglist[2]
                    dn.g_type = arglist[3]
                    dn.g_name = arglist[4]
                    dn.array = arglist[:2]
                    dn.backend = returned_kargs
                else:
                    dn = None

            if dn is not None:
                dn._template_origin = template_origin
                dn.ratio = keyargs.get("ratio", None)
                dn.continents = self.getcontinentstype()
                dn.continents_line = self.getcontinentsline()
                dn.newelements = self.__new_elts(original_elts, new_elts)

            if self.mode != 0:
                # self.update()
                pass

        result = dn
        if isinstance(arglist[3], str):
            # Pointer to the plotted slab of data and the VCS Canas display infomation.
            # This is needed to find the animation min and max values and the number of
            # displays on the VCS Canvas.
            if dn is not None:
                self.animate_info.append((result, arglist[:2]))

        # Now executes output commands
        for cc in cmds.keys():
            c = cc.lower()
            if not isinstance(cmds[cc], type('')):
                args = tuple(cmds[cc])
            else:
                args = (cmds[cc],)
            if c == 'ps' or c == 'postscript':
                self.postscript(*args)
            elif c == 'pdf':
                self.pdf(*args)
            elif c == 'gif':
                self.gif(*args)
            elif c == 'eps':
                self.eps(*args)
            elif c == 'cgm':
                self.cgm(*args)
            elif c == 'ras':
                self.ras(*args)

        # self.clean_auto_generated_objects("template")
        for p in slab_changed_attributes.keys():
            tmp = slab_changed_attributes[p]
            if tmp == (None, None):
                delattr(arglist[0], p)
            else:
                setattr(arglist[0], p, tmp)
        if dn is not None:
            self.display_names.append(result.name)
            if result.g_type in (
                    "3d_scalar", "3d_vector") and self.configurator is not None:
                self.endconfigure()
            if self.backend.bg is False and self.configurator is not None:
                self.configurator.update()

        return result

    def setAnimationStepper(self, stepper):
        self.backend.setAnimationStepper(stepper)

    def return_display_names(self, *args):
        return self.display_names

    def remove_display_name(self, *args):
        """
        Removes a plotted item from the canvas.

        :param args: Any number of display names to remove.
        :type args: str list
        """
        for a in args:
            if a in self.display_names:
                self.display_names.remove(a)
        self.update()

    def cgm(self, file, mode='w'):
        """
        Export an image in CGM format.

:param file: Filename to save
:param mode: Ignored.
        """

        if mode != 'w':
            warnings.warn(
                "cgm only supports 'w' mode ignoring your mode ('%s')" %
                mode)
        return self.backend.cgm(file)

    def clear(self, *args, **kargs):
        """
        Clears all the VCS displays on a page (i.e., the VCS Canvas object).

        :Example:

        ::

    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    #clear VCS displays from the page
    a.clear()

"""
        if self.animate.created():
            self.animate.close()
        if self.configurator is not None:
            self.configurator.stop_animating()
        self.animate_info = []
        self.animate.update_animate_display_list()

        preserve_display = kargs.get("preserve_display", False)
        if "preserve_display" in kargs:
            del kargs["preserve_display"]
        self.backend.clear(*args, **kargs)
        for nm in self.display_names:
            # Lets look at elements created by dispaly production
            # Apparently when updating we shouldn't be clearing these elemnts
            # yet
            if kargs.get("render", True):
                dn = vcs.elements["display"][nm]
                new_elts = getattr(dn, "newelements", {})
                for e in new_elts.keys():
                    if e == "display":
                        continue
                    for k in new_elts[e]:
                        if k in vcs.elements[e].keys():
                            del(vcs.elements[e][k])
            if not preserve_display:
                del(vcs.elements["display"][nm])
        self.display_names = []
        return

    def close(self, *args, **kargs):
        """
        Close the VCS Canvas. It will not deallocate the VCS Canvas object.
        To deallocate the VCS Canvas, use the destroy method.

        :Example:

        ::

    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    #close the vcs canvas
    a.close()

"""
        if self.configurator:
            self.endconfigure()
        a = self.backend.close(*args, **kargs)
        self.animate_info = []

        return a

    def destroy(self):
        """
    Destroy the VCS Canvas. It will deallocate the VCS Canvas object.

    :Example:

        ::

    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.destory()

"""
        import gc

        del self
        gc.garbage
        gc.collect()

    def change_display_graphic_method(self, display, type, name):
        '''
        Changes the type and graphic method of a plot.

        :param display: Display to change.
        :param type: New graphics method type.
        :param name: Name of new graphics method.
        :type display: str or vcs.displayplot.Dp
        :type name: str
        :type type: str
        '''

        if isinstance(display, (str, unicode)):
            display = vcs.elements["display"][display]
        display.g_type = type
        display.g_name = name
        self.update()

    def get_selected_display(self):
        """
        .. deprecated:: ?.?


    """
        return self.canvas.get_selected_display(*())

    def plot_annotation(self, *args):
        self.canvas.plot_annotation(*args)

    def flush(self, *args):
        """
    The flush command executes all buffered X events in the queue.

    :Example:

        ::

    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.flush()

"""
        return self.backend.flush(*args)

    def geometry(self, *args):
        """
    The geometry command is used to set the size and position of the VCS canvas.

     :Example:

        ::

    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.geometry(450,337)

"""
        if len(args) == 0:
            return self.backend.geometry()

        if (args[0] <= 0) or (args[1] <= 0):
            raise ValueError(
                'Error -  The width and height values must be an integer greater than 0.')

        a = self.backend.geometry(*args)
        self.flush()  # update the canvas by processing all the X events

        return a

    def canvasinfo(self, *args, **kargs):
        """
        Obtain the current attributes of the VCS Canvas window.

        :returns: Dictionary with keys: "mapstate" (whether the canvas is opened), "height", "width", "depth", "x", "y"
        """
        return self.backend.canvasinfo(*args, **kargs)

    def getcontinentstype(self, *args):
        """
    Retrieve continents type from VCS; either an integer between 0 and 11 or the
    path to a custom continentstype.

     :Example:

        ::

     a=vcs.init()
     # Get the continents type
     cont_type = a.getcontinentstype()

:returns: An int between 1 and 0, or the path to a custom continentstype
:rtype: int or system filepath
"""
        try:
            return self._continents
        except:
            return None

    def pstogif(self, filename, *opt):
        """
     In some cases, the user may want to save the plot out as a gif image. This
     routine allows the user to convert a postscript file to a gif file.

    :Example:

        ::

     a=vcs.init()
     a.plot(array)
     # convert the postscript file to a gif file (l=landscape)
     a.pstogif('filename.ps')
     # convert the postscript file to a gif file (l=landscape)
     a.pstogif('filename.ps','l')
     # convert the postscript file to a gif file (p=portrait)
     a.pstogif('filename.ps','p')

:param filename: String name of the desired output file
:type filename: str

:param opt: One of 'l' or 'p', indicating landscape or portrait mode, respectively.
:type opt: str

:returns: ???
:rtype: ???
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
        f = popen(cmd, 'w')
        f.close()
        return

    def grid(self, *args):
        """
    Set the default plotting region for variables that have more dimension values
    than the graphics method. This will also be used for animating plots over the
    third and fourth dimensions.


    :Example:

        ::

    a=vcs.init()
    a.grid(12,12,0,71,0,45)
"""

        p = self.canvas.grid(*args)

        return p

    def landscape(self, width=-99, height=-99, x=-99, y=-99, clear=0):
        """
    Change the VCS Canvas orientation to Landscape.

     .. note::
        The (width, height) and (x, y) arguments work in pairs. That is, you must
        set (width, height) or (x, y) together to see any change in the VCS Canvas.

        If the portrait method is called  with arguments before displaying a VCS Canvas,
        then the arguments (width, height, x, y, and clear) will have no effect on the
        canvas.

     .. warning::
        If the visible plot on the VCS Canvas is not adjusted properly, then resize
        the screen with the point. Some X servers are not handling the threads properly
        to keep up with the demands of the X client.

     :Example:

        ::

    a=vcs.init()
    a.plot(array)
    # Change the VCS Canvas orientation and set object flag to landscape
    a.landscape()
    # Change the VCS Canvas to landscape and clear the page
    a.landscape(clear=1)
    # Change to landscape and set the window size
    a.landscape(width = 400, height = 337)
    # Change to landscape and set the x and y screen position
    a.landscape(x=100, y = 200)
    # Change to landscape and give specifications
    a.landscape(width = 400, height = 337, x=100, y = 200, clear=1)

:param width: Width of the canvas, in pixels
:type width: int

:param height: Height of the canvas, in pixels
:type height: int

:param x: Unused
:type x: int

:param y: Unused
:type y: int

:param clear: Indicates the canvas should be cleared (1), or should not be cleared (0), when orientation is changed.
:type clear: int

"""
        if (self.orientation() == 'landscape'):
            return

        if (((not isinstance(width, int))) or ((not isinstance(height, int))) or
                ((not isinstance(x, int))) or ((not isinstance(y, int))) or
                ((width != -99) and (width < 0)) or ((height != -99) and (height < 0)) or
                ((x != -99) and (x < 0)) or ((y != -99) and (y < 0))):
            raise ValueError(
                'If specified, width, height, x, and y must be integer values greater than or equal to 0.')
        if (((not isinstance(clear, int))) and (clear not in [0, 1])):
            raise ValueError(
                "clear must be: 0 - 'the default value for not clearing the canvas' or 1 - 'for clearing the canvas'.")

        if ((width == -99) and (height == -99) and (x == -99) and (y == -99) and (clear == 0)):
            cargs = ()
            try:
                dict = self.canvasinfo(*cargs)
            except:
                dict = {}
            height = dict.get('width', -99)
            width = dict.get('height', -99)
            x = dict.get('x', -99)
            y = dict.get('y', -99)
        self.flush()  # update the canvas by processing all the X events

        args = (width, height, x, y, clear)
        l = self.backend.landscape(*args)

        return l

    def listelements(self, *args):
        """
        Returns a Python list of all the VCS class objects.

        The list that will be returned:
        ['1d', '3d_dual_scalar', '3d_scalar', '3d_vector', 'boxfill', 'colormap', 'display', 'fillarea',
         'font', 'fontNumber', 'isofill', 'isoline', 'line', 'list', 'marker', 'meshfill', 'projection',
         'scatter', 'taylordiagram', 'template', 'textcombined', 'textorientation', 'texttable',
         'vector', 'xvsy', 'xyvsy', 'yxvsx']

         :Example:

        ::

        a=vcs.init()
        a.listelements()

:returns: A list of string names of all VCS class objects
:rtype: list

"""
        f = vcs.listelements
        L = sorted(f(*args))

        return L

    def updateorientation(self, *args):
        """
     .. deprecated:: ?.?
        Use :func:`landscape` or :func:`portrait` instead.
"""

        a = self.canvas.updateorientation(*args)

        return a

    def open(self, width=None, height=None, **kargs):
        """
    Open VCS Canvas object. This routine really just manages the VCS canvas. It will
    popup the VCS Canvas for viewing. It can be used to display the VCS Canvas.

    :Example:

        ::

    a=vcs.init()
    a.open()
    a.open(800,600)

:param width: Integer representing the desire width of the opened window in pixels
:type width: int

:param height: Integer representing the desire height of the opened window in pixels
:type height: int

"""

        a = self.backend.open(width, height, **kargs)

        return a

    def canvasid(self, *args):
        '''
        Get the ID of this canvas.

        This ID number is found at the top of the VCS Canvas, as part of its title.
        '''
        return self._canvas_id

    def portrait(self, width=-99, height=-99, x=-99, y=-99, clear=0):
        """
        Change the VCS Canvas orientation to Portrait.

        .. note::
            If the current orientation of the canvas is already portrait, nothing happens.

        :Example:

        ::

    a=vcs.init()
    a.plot(array)
    # Change the VCS Canvas orientation and set object flag to portrait
    a.portrait()
    # Change the VCS Canvas to portrait and clear the page
    a.portrait(clear=1)
    # Change to portrait and set the window size
    a.portrait(width = 337, height = 400)
    # Change to portrait and set the x and y screen position
    a.portrait(x=100, y = 200)
    # Change to portrait and give specifications
    a.portrait(width = 337, height = 400, x=100, y = 200, clear=1)

:param width: Width to set the canvas to (in pixels)
:type width: int

:param height: Height to set the canvas to (in pixels)
:type height: int

:param x: Unused.
:type x: None

:param y: Unused.
:type y: None

:param clear: 0: Do not clear the canvas when orientation is changed. 1: clear the canvas when orientation is changed.
:type clear: int

"""
        if (self.orientation() == 'portrait'):
            return

        if (((not isinstance(width, int))) or ((not isinstance(height, int))) or
                ((not isinstance(x, int))) or ((not isinstance(y, int))) or
                ((width != -99) and (width < 0)) or ((height != -99) and (height < 0)) or
                ((x != -99) and (x < 0)) or ((y != -99) and (y < 0))):
            raise ValueError(
                'If specified, width, height, x, and y must be integer values greater than or equal to 0.')
        if (((not isinstance(clear, int))) and (clear not in [0, 1])):
            raise ValueError(
                "clear must be: 0 - 'the default value for not clearing the canvas' or 1 - 'for clearing the canvas'.")

        if ((width == -99) and (height == -99) and (x == -99) and (y == -99) and (clear == 0)):
            cargs = ()
            try:
                dict = self.canvasinfo(*cargs)
            except:
                dict = {}
            height = dict.get('width', -99)
            width = dict.get('height', -99)
            x = dict.get('x', -99)
            y = dict.get('y', -99)
        self.flush()  # update the canvas by processing all the X events

        args = (width, height, x, y, clear)
        p = self.backend.portrait(*args)

        return p

    def ffmpeg(self, movie, files, bitrate=1024, rate=None, options=None):
        """
    MPEG output from a list of valid files.
    Can output to more than just mpeg format.

    .. note::
        ffmpeg ALWAYS overwrites the output file

    :Example:

        ::

    a=vcs.init()
    #... code to generate png files ...
    # here is a dummy example
    files =[]
    for i in range(10):
      a.png('my_png__%i' % i)
      files.append('my_png__%i.png' % i)
    # generates mpeg from pattern
    a.ffmpeg('mymovie.mpeg','my_png_%d.png')
    # generates from list of files
    a.ffmpeg('mymovie.mpeg',files)
    # generates mpeg at 512kbit
    a.ffmpeg('mymovie.mpeg','my_png_%d.png',bitrate=512)
    # generates movie with 50 frame per second
    a.ffmpeg('mymovie.mpeg','my_png_%d.png',rate=50)
    # generates movie at 50 frame per sec and 1024k bitrate
    a.ffmpeg('mymovie.mpeg','my_png_%d.png',options='-r 50 -b 1024k')
    #   NOTE: via the options arg you can add audio file to your movie (see ffmpeg help)

:param movie: Output video file name
:type movie: str

:param files: String file name
:type files: str, list, or tuple

:param rate: Desired output framerate
:type rate: str

:param options: Additional FFMPEG arguments
:type options: str

:returns: The output string generated by ffmpeg program
:rtype: str

"""
        args = ["ffmpeg", "-y"]

        if rate is not None:
            args.extend(("-framerate", str(rate)))

        if isinstance(files, (list, tuple)):
            test_file = files[0]

            rnd = "%s/.uvcdat/__uvcdat_%i" % (
                os.path.expanduser("~"), numpy.random.randint(600000000))
            Files = []
            for i, f in enumerate(files):
                fnm = "%s_%i.png" % (rnd, i)
                shutil.copy(f, fnm)
                Files.append(fnm)
            args.extend(("-i", "%s_%%d.png" % rnd))
        elif isinstance(files, str):
            # Extract formatter
            percent_re = re.compile(r"(%0*d)")
            str_format = percent_re.search(files)
            if str_format is not None:
                prefix, group, suffix = percent_re.split(files, maxsplit=1)
                numeric_length = len(str_format.group(0).split("0")) - 1
                path, pre = os.path.split(prefix)
                if path == '':
                    path = "."
                file_names = os.listdir(path)
                for f in file_names:
                    # Make sure it starts with the "before number" part
                    if not f.startswith(pre):
                        continue

                    # Make sure the length is correct
                    if len(f) != len(pre) + numeric_length + len(suffix):
                        continue

                    # Iterate the numeric section
                    for i in range(len(pre), len(pre) + numeric_length):
                        try:
                            int(f[i])
                        except ValueError:
                            # Invalid character found, exit the loop
                            break
                    else:
                        # Make sure the ending is correct
                        if f.endswith(suffix):
                            test_file = os.path.join(path, f)
                            # We found a test file, we can stop now
                            break
                else:
                    test_file = False

            args.extend(('-i', files))

        args.extend(("-pix_fmt", "yuv420p"))

        if test_file is not False:
            # H264 requires even numbered heights and widths
            width, height = self.backend.png_dimensions(test_file)
            if width % 2 == 1:
                width = width + 1
            if height % 2 == 1:
                height = height + 1
            args.extend(("-vf", "scale=%d:%d" % (width, height)))

        if options is not None:
            args.append(options)

        args.append(movie)

        result = subprocess.call(args)

        if isinstance(files, (list, tuple)):
            for f in Files:
                os.remove(f)

        return result == 0

    def getantialiasing(self):
        return self.backend.getantialiasing()

    def setantialiasing(self, antialiasing):
        """ Set antialiasing rate.

:param antialiasing: Integer from 0-64, representing the antialising rate (0 means no antialiasing).
:type antialiasing: int
        """
        self.backend.setantialiasing(antialiasing)

    def setbgoutputdimensions(self, width=None, height=None, units='inches'):
        """
        Sets dimensions for output in bg mode.

         :Example:

        ::

            a=vcs.init()
            # US Legal
            a.setbgoutputdimensions(width=11.5, height= 8.5)
            # A4
            a.setbgoutputdimensions(width=21, height=29.7, units='cm')

:param width: Float representing the desired width of the output, using the specified unit of measurement
:type width: float

:param height: Float representing the desired height of the output, using the specified unit of measurement.
:type height: float

:param units: One of ['inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']. Defaults to 'inches'.
:type units: str

        """
        if units not in [
                'inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']:
            raise Exception(
                "units must be on of inches, in, cm, mm, pixel(s) or dot(s)")

        W, H = self._compute_width_height(
            width, height, units)

        # in pixels?
        self.bgX = W
        self.bgY = H
        return
    # display ping

    def put_png_on_canvas(
            self, filename, zoom=1, xOffset=0, yOffset=0, *args, **kargs):
        self.backend.put_png_on_canvas(
            filename,
            zoom,
            xOffset,
            yOffset,
            *args,
            **kargs)

    def png(self, file, width=None, height=None,
            units=None, draw_white_background=True, **args):
        """
    PNG output, dimensions set via setbgoutputdimensions

    :Example:

        ::

    a=vcs.init()
    a.plot(array)
    # Overwrite a png file
    a.png('example')

:param file: Output image filename
:type file: str

:param width: Float representing the desired width of the output png, using the specified unit of measurement
:type width: float

:param height: Float representing the desired height of the output png, using the specified unit of measurement.
:type height: float

:param units: One of ['inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']. Defaults to 'inches'.
:type units: str

:param draw_white_background: Boolean value indicating whether or not the background should be white. Defaults to True.
:type draw_white_background: bool
"""
        base = os.path.dirname(file)
        if base != "" and not os.path.exists(base):
            raise vcsError("Output path: %s does not exist" % base)
        if units not in [
                'inches', 'in', 'cm', 'mm',
                None, 'pixel', 'pixels', 'dot', 'dots']:
            raise Exception(
                "units must be on of inches, in, cm, mm, pixel(s) or dot(s)")

        W, H = self._compute_width_height(
            width, height, units)
        return self.backend.png(
            file, W, H, units, draw_white_background, **args)

    def pdf(self, file, width=None, height=None, units='inches',
            textAsPaths=True):
        """
    PDF output is another form of vector graphics.

    .. note::
        The textAsPaths parameter preserves custom fonts, but text can no longer be edited in the file

    :Example:

        ::

    a=vcs.init()
    a.plot(array)
    # Overwrite a postscript file
    a.pdf('example')
    # US Legal
    a.pdf('example', width=11.5, height= 8.5)
    # A4
    a.pdf('example', width=21, height=29.7, units='cm')

:param file: Desired string name of the output file
:type file: str

:param width: Integer specifying the desired width of the output, measured in the chosen units
:type width: int

:param height: Integer specifying the desired height of the output, measured in the chosen units
:type height: int

:param units: Must be one of ['inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']. Default is 'inches'.
:type units: str

:param textAsPaths: Specifies whether to render text objects as paths.
:type textAsPaths: bool

:returns: ???
:rtype: ???
"""
        if units not in [
                'inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']:
            raise Exception(
                "units must be on of inches, in, cm, mm, pixel(s) or dot(s)")

        W, H = self._compute_width_height(
            width, height, units)

        if not file.split('.')[-1].lower() in ['pdf']:
            file += '.pdf'
        return self.backend.pdf(file, W, H, textAsPaths)

    def svg(self, file, width=None, height=None, units='inches',
            textAsPaths=True):
        """
    SVG output is another form of vector graphics.

    .. note::
        The textAsPaths parameter preserves custom fonts, but text can no longer be edited in the file

    :Example:

        ::

    a=vcs.init()
    a.plot(array)
    # Overwrite a postscript file
    a.svg('example')
    # US Legal
    a.svg('example', width=11.5, height= 8.5)
    # A4
    a.svg('example', width=21, height=29.7, units='cm')

:param file:
:type file:

:param width: Float to set width of output SVG, in specified unit of measurement
:type width: float

:param height: Float to set height of output SVG, in specified unit of measurement
:type height: float

:param units: One of ['inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']. Deafults to 'inches'.
:type units: str

:param textAsPaths: Specifies whether to render text objects as paths.
:type textAsPaths: bool

:returns: ???
:rtype: ???
"""
        if units not in [
                'inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']:
            raise Exception(
                "units must be on of inches, in, cm, mm, pixel(s) or dot(s)")

        W, H = self._compute_width_height(
            width, height, units)

        if not file.split('.')[-1].lower() in ['svg']:
            file += '.svg'
        return self.backend.svg(file, W, H, textAsPaths)

    def _compute_margins(
            self, W, H, top_margin, bottom_margin, right_margin, left_margin, dpi):
        try:
            ci = self.canvasinfo()
            height = ci['height']
            width = ci['width']
            factor = 1. / 72
            size = float(width) / float(height)
        except:
            factor = 1.
            if self.size is None:
                size = 1.2941176470588236
            else:
                size = self.size
        if bottom_margin is not None:
            bottom_margin = bottom_margin * factor
        if left_margin is not None:
            left_margin = left_margin * factor
        if right_margin is not None:
            right_margin = right_margin * factor
        if top_margin is not None:
            top_margin = top_margin * factor

        # now for sure factor is 1.
        factor = 1.
        if left_margin is None and right_margin is None and top_margin is None and bottom_margin is None:
            # default margins
            left_margin = .25
            right_margin = .25
            top_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H - twidth / size) / dpi - top_margin
            bottom_margin = (top_margin + bottom_margin) / 2.
            top_margin = bottom_margin
        # bottom_defined
        elif left_margin is None and right_margin is None and top_margin is None:
            left_margin = .25
            right_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            top_margin = (H - twidth / size) / dpi - bottom_margin
        # top_defined
        elif left_margin is None and right_margin is None and bottom_margin is None:
            left_margin = .25
            right_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H) / dpi - top_margin
        # right defined
        elif top_margin is None and bottom_margin is None and left_margin is None:
            left_margin = .25
            top_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H - twidth / size) / dpi - top_margin
        # left defined
        elif top_margin is None and bottom_margin is None and right_margin is None:
            right_margin = .25
            top_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H - twidth / size) / dpi - top_margin
        # left defined and bottom
        elif top_margin is None and right_margin is None:
            right_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            top_margin = (H - twidth / size) / dpi - bottom_margin
        # right defined and bottom
        elif top_margin is None and left_margin is None:
            left_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            top_margin = (H - twidth / size) / dpi - bottom_margin
        # right defined and top
        elif bottom_margin is None and left_margin is None:
            left_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H - twidth / size) / dpi - top_margin
        # left defined and top
        elif bottom_margin is None and right_margin is None:
            right_margin = .25
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H - twidth / size) / dpi - top_margin
        # all but bottom
        elif bottom_margin is None:
            twidth = W - (left_margin + right_margin) * dpi
            bottom_margin = (H - twidth / size) / dpi - top_margin
        # all but top
        elif top_margin is None:
            twidth = W - (left_margin + right_margin) * dpi
            top_margin = (H - twidth / size) / dpi - bottom_margin
        # all but right
        elif right_margin is None:
            theight = H - (top_margin + bottom_margin) * dpi
            right_margin = (W - theight * size) / dpi + left_margin
        # all but left
        elif left_margin is None:
            theight = H - (top_margin + bottom_margin) * dpi
            left_margin = (W - theight * size) / dpi + right_margin

        return top_margin, bottom_margin, right_margin, left_margin

    def isopened(self):
        """
        Returns a boolean value indicating whether the canvas is opened or not.

:returns: A boolean value indicating whether the Canvas is opened (1), or closed (0)
:rtype: bool
        """

        return self.backend.isopened()

    def _compute_width_height(self, width, height, units, ps=False):
        dpi = 72.  # dot per inches
        if units in ["in", "inches"]:
            factor = 1.
        elif units == 'cm':
            factor = 0.393700787
        elif units == 'mm':
            factor = 0.0393700787
        else:
            factor = 1. / 72
        sfactor = factor
        if width is None and height is None:
            if self.isopened():
                try:
                    ci = self.canvasinfo()
                    height = ci['height']
                    width = ci['width']
                    sfactor = 1. / 72.
                    if ps is True:
                        ratio = width / float(height)
                        if self.size == 1.4142857142857141:
                            # A4 output
                            width = 29.7
                            sfactor = 0.393700787
                            height = 21.
                        elif self.size == 1. / 1.4142857142857141:
                            width = 21.
                            sfactor = 0.393700787
                            height = 29.7
                        else:
                            sfactor = 1.
                            if ratio > 1:
                                width = 11.
                                height = width / ratio
                            else:
                                height = 11.
                                width = height * ratio
                except:  # canvas never opened
                    if self.size is None:
                        sfactor = 1.
                        height = 8.5
                        width = 11.
                    elif self.size == 1.4142857142857141:
                        sfactor = 0.393700787
                        width = 29.7
                        height = 21.
                    else:
                        sfactor = 1.
                        height = 8.5
                        width = self.size * height
            else:
                width = self.bgX
                height = self.bgY
        elif width is None:
            if self.size is None:
                width = 1.2941176470588236 * height
            else:
                width = self.size * height
        elif height is None:
            if self.size is None:
                height = width / 1.2941176470588236
            else:
                height = width / self.size
        W = int(width * dpi * sfactor)
        H = int(height * dpi * sfactor)
        if (self.isportrait() and W > H) \
                or (self.islandscape() and H > W):
            tmp = W
            W = H
            H = tmp
        return W, H

    def postscript(self, file, mode='r', orientation=None, width=None, height=None, units='inches', textAsPaths=True):
        """
    Postscript output is another form of vector graphics. It is larger than its CGM output
    counter part, because it is stored out in ASCII format.

    There are two modes for saving a postscript file: `Append' (a) mode appends postscript
    output to an existing postscript file; and `Replace' (r) mode overwrites an existing
    postscript file with new postscript output. The default mode is to overwrite an existing
    postscript file (i.e. mode (r)).

    .. note::
        The textAsPaths parameter preserves custom fonts, but text can no longer be edited in the file

    :Example:

        ::

    a=vcs.init()
    a.plot(array)
    # Overwrite a postscript file
    a.postscript('example')
    # Append postscript to an existing file
    a.postscript('example', 'a')
    # Overwrite an existing file
    a.postscript('example', 'r')
    # Append postscript to an existing file
    a.postscript('example', mode='a')
    # US Legal (default)
    a.postscript('example', width=11.5, height= 8.5)
    # A4
    a.postscript('example', width=21, height=29.7, units='cm')
    # US Legal output and control of margins (for printer friendly output), default units 'inches'
    a.postscript('example', right_margin=.2,left_margin=.2,top_margin=.2,bottom_margin=.2)

:param file: String name of the desired output file
:type file: str

:param mode: The mode in which to open the file. One of 'r' or 'a'.
:type mode: str

:param orientation: Deprecated.
:type orientation: None

:param width: Desired width of the postscript output, in the specified unit of measurement
:type width: int

:param height: Desired height of the postscript output, in the specified unit of measurement
:type height: int
:type units: str

:param textAsPaths: Specifies whether to render text objects as paths.
:type textAsPaths: bool

:returns: ???
:rtype: ???
"""
        if units not in [
                'inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']:
            raise Exception(
                "units must be on of inches, in, cm, mm, pixel(s) or dot(s)")

        # figures out width/height
        W, H = self._compute_width_height(
            width, height, units, ps=True)

        # orientation keyword is useless left for backward compatibility
        if not file.split('.')[-1].lower() in ['ps', 'eps']:
            file += '.ps'
        if mode == 'r':
            return self.backend.postscript(file, W, H, units="pixels", textAsPaths=textAsPaths)
        else:
            n = random.randint(0, 10000000000000)
            psnm = '/tmp/' + '__VCS__tmp__' + str(n) + '.ps'
            self.backend.postscript(psnm, W, H, units="pixels")
            if os.path.exists(file):
                f = open(file, 'r+')
                f.seek(0, 2)  # goes to end of file
                f2 = open(psnm)
                f.writelines(f2.readlines())
                f2.close()
                f.close()
                os.remove(psnm)
            else:
                shutil.move(psnm, file)

    def _scriptrun(self, *args):
        return vcs._scriptrun(*args)

    def scriptrun(self, aFile, *args, **kargs):
        vcs.scriptrun(aFile, *args, **kargs)

    def setcolormap(self, name):
        """

        It is necessary to change the colormap. This routine will change the VCS
        color map.

        If the the visul display is 16-bit, 24-bit, or 32-bit TrueColor, then a redrawing
        of the VCS Canvas is made evertime the colormap is changed.

        :Example:

        ::

            a=vcs.init()
            a.plot(array,'default','isofill','quick')
            a.setcolormap("AMIP")

        :param name: Name of the coormap to use
        :type name: str

        :returns: ???
        :rtype: ???
        """
        # Don't update the VCS segment if there is no Canvas. This condition
        # happens in the initalize function for VCDAT only. This will cause a
        # core dump is not checked.
        # try:
        #   updateVCSsegments_flag = args[1]
        # except:
        #   updateVCSsegments_flag = 1

        self.colormap = name
        self.update()
        return

    def setcolorcell(self, *args):
        """
    Set a individual color cell in the active colormap. If default is
    the active colormap, then return an error string.

    If the the visul display is 16-bit, 24-bit, or 32-bit TrueColor, then a redrawing
    of the VCS Canvas is made evertime the color cell is changed.

    Note, the user can only change color cells 0 through 239 and R,G,B
    value must range from 0 to 100. Where 0 represents no color intensity
    and 100 is the greatest color intensity.

    :Example:

        ::

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

        a = vcs.setcolorcell(self.colormap, *args)
        return a

    def setcontinentsline(self, line="default"):
        """
        One has the option of configuring the appearance of the lines used to
        draw continents by providing a VCS Line object.

        :Example:

        ::

        a = vcs.init()
        line = vcs.createline()
        line.width = 5
        # Use custom continents line
        a.setcontinentsline(line)
        # Use default line
        a.setcontinentsline("default")

:param line: Line to use for drawing continents. Can be a string name of a line, or a VCS line object
:type line: str or vcs.line.Tl

:returns: ???
:rtype: ???
        """
        linename = VCS_validation_functions.checkLine(self, "continentsline", line)
        line = vcs.getline(linename)
        self._continents_line = line

    def getcontinentsline(self):
        if self._continents_line is None:
            return vcs.getline("default")
        else:
            return self._continents_line

    def setcontinentstype(self, value):
        """
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

          6 uses a custom continent set

      You can also pass a file by path.

      :Example:

        ::

      a=vcs.init()
      a.setcontinentstype(3)
      a.plot(array,'default','isofill','quick')

:param value: Integer representing continent type, as specified in function description
:type value: int

:returns: ???
:rtype: ???
  """
        continent_path = VCS_validation_functions.checkContinents(self, value)
        self._continents = value
        if continent_path is not None and not os.path.exists(
                continent_path):
            warnings.warn(
                "Continents file not found: %s, substituing with fine continents" %
                continent_path)
            self._continents = 1
            return

    def _continentspath(self):
        try:
            path = VCS_validation_functions.checkContinents(self, self._continents)
            if path is None and self._continents != 0:
                return VCS_validation_functions.checkContinents(self, 1)
            else:
                return path
        except:
            return VCS_validation_functions.checkContinents(self, 1)

    def gif(self, filename='noname.gif', merge='r', orientation=None,
            geometry='1600x1200'):
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

 :Example:
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
            orientation = self.orientation()[0]
        g = geometry.split('x')
        f1 = f1 = float(g[0]) / 1100.0 * 100.0
        f2 = f2 = float(g[1]) / 849.85 * 100.0
        geometry = "%4.1fx%4.1f" % (f2, f1)
        nargs = ('gif', filename, merge, orientation, geometry)
        return self.backend.gif(nargs)

    def gs(self, filename='noname.gs', device='png256',
           orientation=None, resolution='792x612'):

        warnings.warn("Export to GhostScript is no longer supported", DeprecationWarning)

    def eps(self, file, mode='r', orientation=None, width=None, height=None, units='inches', textAsPaths=True):
        """
        In some cases, the user may want to save the plot out as an Encapsulated
        PostScript image. This routine allows the user to save the VCS canvas output
        as an Encapsulated PostScript file.
        This file can be converted to other image formats with the aid of xv and other
        such imaging tools found freely on the web.

        :Example:

        ::

        a=vcs.init()
        a.plot(array)
        # Overwrite a postscript file
        a.postscript('example')
        # Append postscript to an existing file
        a.postscript('example', 'a')
        # Overwrite an existing file
        a.postscript('example', 'r')
        # Append postscript to an existing file
        a.postscript('example', mode='a')
         # US Legal (default)
        a.postscript('example', width=11.5, height= 8.5)
        # A4
        a.postscript('example', width=21, height=29.7, units='cm')
        a.postscript('example', right_margin=.2,left_margin=.2,top_margin=.2,bottom_margin=.2)
        # US Legal output and control of margins (for printer friendly output), default units 'inches'


:param file: String name of the desired output file
:type file: str

:param mode: The mode in which to open the file. One of 'r' or 'a'.
:type mode: str

:param orientation: Deprecated.
:type orientation: None

:param width: Width of the output image, in the unit of measurement specified
:type width: float

:param height: Height of the output image, in the unit of measurement specified
:type height: float

:param units: One of ['inches', 'in', 'cm', 'mm', 'pixel', 'pixels', 'dot', 'dots']. Defaults to 'inches'.
:type units: str

:returns: ???
:rtype: ???
"""
        ext = file.split(".")[-1]
        if ext.lower() != 'eps':
            file = file + '.eps'
        num = numpy.random.randint(100000000000)
        tmpfile = "/tmp/vcs_tmp_eps_file_%i.ps" % num
        if mode == 'a' and os.path.exists(file):
            os.rename(file, tmpfile)
        self.postscript(
            tmpfile,
            mode,
            orientation,
            width,
            height,
            units,
            textAsPaths)

        os.popen("ps2epsi %s %s" % (tmpfile, file)).readlines()
        os.remove(tmpfile)

    def show(self, *args):
        return vcs.show(*args)
    show.__doc__ = vcs.__doc__

    def isinfile(self, GM, file=None):
        """
        Checks if a graphic method is stored in a file
        if no file name is passed then looks into the initial.attributes file

:param GM: The graphics method to search for
:type GM: str

:param file: String name of the file to search
:type file: str

:returns: ???
:rtype: ???

        """
        nm = GM.name
        gm = GM.g_name
        key = gm + '_' + nm + '('
        if file is None:
            file = os.path.join(
                os.path.expanduser("~"),
                self._dotdir,
                'initial.attributes')
        f = open(file, 'r')
        for ln in f:
            if ln.find(key) > -1:
                f.close()
                return 1
        return

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

 :Example:
    a=vcs.init()
    ...

    a.saveinitialfile()

 WARNING: This removes first ALL object generated automatically
 (i.e. whose name starts with '__') in order to preserve this, rename objects first
 e.g:
    b=a.createboxfill()
    b.name='MyBoxfill'

 # graphic method is now preserved
""" % (self._dotdir)
        self.clean_auto_generated_objects()
        return vcs.saveinitialfile()

    def raisecanvas(self, *args):
        """
        Raise the VCS Canvas to the top of all open windows.
        """
        return self.backend.raisecanvas(*args)

    def islandscape(self):
        """
    Indicates if VCS's orientation is landscape.

    Returns a 1 if orientation is landscape.
    Otherwise, it will return a 0, indicating false (not in landscape mode).

    :Example:

        ::

    a=vcs.init()
    # ...
    if a.islandscape():
        # Set VCS's orientation to portrait mode
       a.portrait()

:returns: Integer indicating VCS is in landscape mode (1), or not (0)
:rtype: int
"""
        if (self.orientation() == 'landscape'):
            return 1
        else:
            return 0

    def isportrait(self):
        """
    Indicates if VCS's orientation is portrait.


    :Example:

        ::

    a=vcs.init()
    #...
    if a.isportrait():
        # Set VCS's orientation to landscape mode
        a.landscape()

:returns: Returns a 1 if orientation is portrait, or 0 if not in portrait mode
:rtype: bool

"""
        if (self.orientation() == 'portrait'):
            return 1
        else:
            return 0

    def getplot(self, Dp_name_src='default', template=None):
        """
        .. deprecated:: 1.0
            The geplot function is dperecated

    This function will create a display plot object from an existing display
    plot object from an existing VCS plot. If no display plot name
    is given, then None is returned.

     :Example:

        ::

    a=vcs.init()
    # Show all the existing templates
    a.show('template')
    # plot1 instance of 'dpy_plot_1' display plot
    plot1=a.getplot('dpy_plot_1')

:param Dp_name_src: String name of an existing display plot object
:type Dp_name_src: str

:param template: ???
:type template: ???

:returns: ???
:rtype: ???
"""
        if not isinstance(Dp_name_src, str):
            raise ValueError('Error -  The argument must be a string.')

        Dp_name = None
        display = displayplot.Dp(self, Dp_name, Dp_name_src, 1)
        if template is not None:
            display._template_origin = template
        return display

    def createcolormap(self, Cp_name=None, Cp_name_src='default'):
        """
        """
        return vcs.createcolormap(Cp_name, Cp_name_src)
    createcolormap.__doc__ = vcs.manageElements.createcolormap.__doc__

    def getcolormap(self, Cp_name_src='default'):
        return vcs.getcolormap(Cp_name_src)
    getcolormap.__doc__ = vcs.manageElements.getcolormap.__doc__

    def addfont(self, path, name=""):
        """
        Add a font to VCS.

    :param path: Path to the font file you wish to add (must be .ttf)
    :type path: str

    :param name: Name to use to represent the font.
    :type name: str
"""
        if not os.path.exists(path):
            raise ValueError('Error -  The font path does not exists')
        if os.path.isdir(path):
            dir_files = []
            files = []
            if name == "":
                subfiles = os.listdir(path)
                for file in subfiles:
                    dir_files.append(os.path.join(path, file))
            elif name == 'r':
                for root, dirs, subfiles in os.walk(path):
                    for file in subfiles:
                        dir_files.append(os.path.join(root, file))
            for f in dir_files:
                if f.lower()[-3:]in ['ttf', 'pfa', 'pfb']:
                    files.append([f, ""])
        else:
            files = [[path, name], ]

        nms = []
        for f in files:
            fnm, name = f
            i = max(vcs.elements["fontNumber"].keys()) + 1
            vcs.elements["font"][name] = fnm
            vcs.elements["fontNumber"][i] = name
        if len(nms) == 0:
            raise vcsError('No font Loaded')
        elif len(nms) > 1:
            return nms
        else:
            return nms[0]

    def getfontnumber(self, name):
        return vcs.getfontnumber(name)
    getfontnumber.__doc__ = vcs.utils.getfontnumber.__doc__

    def getfontname(self, number):
        return vcs.getfontname(number)
    getfontname.__doc__ = vcs.utils.getfontname.__doc__

    def getfont(self, font):
        """
        Get the font name/number associated with a font number/name

:param font: The font name/number
:type font: int or str

:returns: If font parameter was a string, will return the integer associated with that string.
          If font parameter was an integer, will return the string associated with that integer.
:rtype: int or str
        """
        if isinstance(font, int):
            return self.getfontname(font)
        elif isinstance(font, str):
            return self.getfontnumber(font)
        else:
            raise vcsError("Error you must pass a string or int")

    def switchfonts(self, font1, font2):
        """
        Switch the font numbers of two fonts.

:param font1: The first font
:type font1: int or str

:param font2: The second font
:type font2: int or str
"""
        if isinstance(font1, str):
            index1 = self.getfont(font1)
        elif isinstance(font1, (int, float)):
            index1 = int(font1)
            self.getfont(index1)  # make sure font exists
        else:
            raise vcsError(
                "Error you must pass either a number or font name!, you passed for font 1: %s" %
                font1)
        if isinstance(font2, str):
            index2 = self.getfont(font2)
        elif isinstance(font2, (int, float)):
            index2 = int(font2)
            self.getfont(index2)  # make sure font exists
        else:
            raise vcsError(
                "Error you must pass either a number or font name!, you passed for font 2: %s" %
                font2)
        return self.canvas.switchfontnumbers(*(index1, index2))

    def copyfontto(self, font1, font2):
        """
        Copy `font1` into `font2`.

:param font1: Name/number of font to copy
:type font1: str or int

:param font2: Name/number of destination
:type font2: str or int
"""
        if isinstance(font1, str):
            index1 = self.getfont(font1)
        elif isinstance(font1, (int, float)):
            index1 = int(font1)
            self.getfont(index1)  # make sure font exists
        else:
            raise vcsError(
                "Error you must pass either a number or font name!, you passed for font 1: %s" %
                font1)
        if isinstance(font2, str):
            index2 = self.getfont(font2)
        elif isinstance(font2, (int, float)):
            index2 = int(font2)
            self.getfont(index2)  # make sure font exists
        else:
            raise vcsError(
                "Error you must pass either a number or font name!, you passed for font 2: %s" %
                font2)
        return self.canvas.copyfontto(*(index1, index2))

    def setdefaultfont(self, font):
        """
        Sets the passed/def show font as the default font for vcs

:param font: Font name or index to use as default
:type font: str or int
        """
        if isinstance(font, str):
            font = self.getfont(font)
        return self.copyfontto(font, 1)

    def orientation(self, *args, **kargs):
        """
        Return canvas orientation.

        The current implementation does not use any args or kargs.

        :Example:

        ::

        a = vcs.init()
        # Show current orientation of the canvas
        a.orientation()

:returns: A string indicating the orientation of the canvas, i.e. 'landscape' or 'portrait'
:rtype: str
        """
        return self.backend.orientation(*args, **kargs)

    def getcolorcell(self, *args):
        """%s""" % vcs.getcolorcell.__doc__
        return vcs.getcolorcell(args[0], self)

    def getcolormapname(self):
        """
        Returns the name of the colormap this canvas is set to use by default.

        To set that colormap, use :ref:`setcolormap`_.
        """
        if self.colormap is None:
            return vcs._colorMap
        return self.colormap

    def dummy_user_action(self, *args, **kargs):
        print 'Arguments:', args
        print 'Keywords:', kargs
        return None


def change_date_time(tv, number):
    timeaxis = tv.getTime()
    if timeaxis is not None:
        try:
            tobj = cdtime.reltime(timeaxis[number], timeaxis.units)
            cobj = tobj.tocomp(timeaxis.getCalendar())
            tv.date = '%s/%s/%s\0' % (cobj.year, cobj.month, cobj.day)
            tv.time = '%s:%s:%s\0' % (cobj.hour, cobj.minute, cobj.second)
        except:
            pass
