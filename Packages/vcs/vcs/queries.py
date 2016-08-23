#!/usr/bin/env python
#
# The VCS query controls -  query module
#
##########################################################################
#                                                                               #
# Module:       query module                                                    #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Functions which get information about vcs graphics objects      #
#               such as graphics methods and templates.                         #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
##########################################################################

"""
###########################################################################################
#                                                                                         #
# Functions which get information about vcs graphics objects such as graphics methods.    #
#                                                                                         #
###########################################################################################
"""
import boxfill
import isofill
import isoline
import taylor
import meshfill
import unified1D
import vector
import line
import marker
import fillarea
import texttable
import textorientation
import textcombined
import template
import dv3d
import displayplot
import projection
import vcs
import xmldocs

from error import vcsError


def isgraphicsmethod(gobj):
    """
    Indicates if the entered argument is one of the following graphics
    methods: boxfill, isofill, isoline,
    scatter, vector, xvsy, xyvsy, yxvsx.

    :Example:

::

    a=vcs.init()
    # To Modify an existing boxfill use:
    box=a.getboxfill('quick')
    #...
    if vcs.isgraphicsmethod(box):
       box.list()

:param gobj: A graphics object
:type gobj: A VCS graphics object

:returns: Integer reperesenting whether gobj is one of the above graphics methods. 1 indicates true, 0 indicates false.
:rtype:
"""
    import vcsaddons
    if (isinstance(gobj, boxfill.Gfb)):
        return 1
    elif (isinstance(gobj, isofill.Gfi)):
        return 1
    elif (isinstance(gobj, dv3d.Gf3Dscalar)):
        return 1
    elif (isinstance(gobj, dv3d.Gf3DDualScalar)):
        return 1
    elif (isinstance(gobj, dv3d.Gf3Dvector)):
        return 1
    elif (isinstance(gobj, isoline.Gi)):
        return 1
    elif (isinstance(gobj, vector.Gv)):
        return 1
    elif (isinstance(gobj, unified1D.G1d)):
        return 1
    elif (isinstance(gobj, taylor.Gtd)):
        return 1
    elif (isinstance(gobj, meshfill.Gfm)):
        return 1
    elif isinstance(gobj, vcsaddons.core.VCSaddon):
        return 1
    else:
        return 0


def graphicsmethodlist():
    """
    List available graphics methods.

    :Example:

::

    a=vcs.init()
    # Return graphics method list
    gm_list=a.graphicsmethodlist()

:returns: A list of available grapics methods (i.e., boxfill, isofill, isoline, outfill,
          scatter, vector, xvsy, xyvsy, yxvsx, taylordiagram ).
:rtype: list
"""
    return ['boxfill', 'isofill', 'isoline', 'meshfill', 'scatter',
            'vector', 'xvsy', 'xyvsy', 'yxvsx', 'taylordiagram', '1d', '3d_scalar', '3d_dual_scalar', '3d_vector']


def graphicsmethodtype(gobj):
    """
    Check the type of a graphics object.

    Returns a None if the object is not a graphics method.

    :Example:

::

    a=vcs.init()
    # Get an existing boxfill graphics method in VCS
    box=a.getboxfill('quick')
    # Get an existing isofill graphics method in VCS
    iso=a.getisofill('quick')
    # Get an existing line element in VCS
    ln=a.getline('quick')
    #...
    # Will print 'boxfill'
    print vcs.graphicsmethodtype(box)
    # Will print 'isofill'
    print vcs.graphicsmethodtype(iso)
    # Will print None, because ln is not a graphics method
    print vcs.graphicsmethodtype(ln)

:returns: If gobj is a graphics method object, returns its type: 'boxfill', 'isofill', 'isoline',
          'scatter', 'vector', 'xvsy', 'xyvsy', or 'yxvsx', 'taylordiagram'.
          If gobj is not a graphics method object, returns None.
:rtype: str or None
    """
    import vcsaddons
    if (isinstance(gobj, boxfill.Gfb)):
        return 'boxfill'
    elif (isinstance(gobj, isofill.Gfi)):
        return 'isofill'
    elif (isinstance(gobj, dv3d.Gf3Dscalar)):
        return '3d_scalar'
    elif (isinstance(gobj, dv3d.Gf3DDualScalar)):
        return '3d_dual_scalar'
    elif (isinstance(gobj, dv3d.Gf3Dvector)):
        return '3d_vector'
    elif (isinstance(gobj, isoline.Gi)):
        return 'isoline'
    elif (isinstance(gobj, vector.Gv)):
        return 'vector'
    elif (isinstance(gobj, unified1D.G1d)):
        return "1d"
    elif (isinstance(gobj, taylor.Gtd)):
        return 'taylordiagram'
    elif (isinstance(gobj, meshfill.Gfm)):
        return 'meshfill'
    elif isinstance(gobj, vcsaddons.core.VCSaddon):
        return gobj
    else:
        raise vcsError('The object passed is not a graphics method object.')


def isplot(pobj):
    """
    Check to see if this object is a VCS secondary display plot.

     :Example:

::

    a=vcs.init()
    # Show all available display
    a.show('display')
    # To test an existing display object
    test_obj = a.getdisplay('default')
    # ...
    if queries.isdisplay(test_obj):
       test_obj.list()

:param obj: A VCS object
:type obj: VCS Object

:returns: An integer indicating whether the object is a display plot (1), or not (0).
:rtype: int
"""
    if (isinstance(pobj, displayplot.Dp)):
        return 1
    else:
        return 0


def iscolormap(obj):
    """
    Check to see if this object is a VCS secondary colormap.

     :Example:

::

    a=vcs.init()
    # Show all available colormap
    a.show('colormap')
    # To test an existing colormap object
    test_obj = a.getcolormap('default')
    # ...
    if queries.iscolormap(test_obj):
       test_obj.list()

:param obj: A VCS object
:type obj: VCS Object

:returns: An integer indicating whether the object is a colormap (1), or not (0).
:rtype: int
"""
    if (isinstance(obj, vcs.colormap.Cp)):
        return 1
    else:
        return 0


def istemplate(gobj):
    """
    Check to see if this object is a template.

     :Example:

::

    a=vcs.init()
    # Show all available template
    a.show('template')
    # To test an existing template object
    test_obj = a.gettemplate('default')
    # ...
    if queries.istemplate(test_obj):
       test_obj.list()

:param obj: A VCS object
:type obj: VCS Object

:returns: An integer indicating whether the object is a template (1), or not (0)
:rtype: int
"""
    if (isinstance(gobj, template.P)):
        return 1
    else:
        return 0


def issecondaryobject(sobj):
    """
        Check to see if this object is a VCS secondary object

        .. note::
            Secondary objects will be one of the following:
            1.) colormap: specification of combinations of 256 available
                       colors
            2.) fill area: style, style index, and color index
            3.) format: specifications for converting numbers to display
                       strings
            4.) line: line type, width, and color index
            5.) list: a sequence of pairs of numerical and character values
            6.) marker: marker type, size, and color index
            7.) text table: text font type, character spacing, expansion, and
                       color index
            8.) text orientation: character height, angle, path, and
                       horizontal/vertical alignment
            9.) projections

     :Example:

::

    a=vcs.init()
    # Show all available lines
    a.show('line')
    # To test an existing line object
    test_obj = a.getprojection('default')
    # ...
    if queries.issecondaryobject(test_obj):
       test_obj.list()

:param obj: A VCS object
:type obj: VCS Object

:returns: An integer indicating whether the object is a projection graphics object (1), or not (0).
:rtype: int
"""
    if (isinstance(sobj, line.Tl)):
        return 1
    elif (isinstance(sobj, marker.Tm)):
        return 1
    elif (isinstance(sobj, fillarea.Tf)):
        return 1
    elif (isinstance(sobj, texttable.Tt)):
        return 1
    elif (isinstance(sobj, textorientation.To)):
        return 1
    elif (isinstance(sobj, textcombined.Tc)):
        return 1
    elif (isinstance(sobj, marker.Tm)):
        return 1
    elif (isinstance(sobj, projection.Proj)):
        return 1
    elif (isinstance(sobj, vcs.colormap.Cp)):
        return 1
    else:
        return 0


def isprojection(obj):
    """
    Check to see if this object is a VCS secondary projection graphics object.

     :Example:

::

    a=vcs.init()
    # Show all available projection
    a.show('projection')
    # To test an existing projection object
    test_obj = a.getprojection('default')
    # ...
    if queries.isprojection(test_obj):
       test_obj.list()

:param obj: A VCS object
:type obj: VCS Object

:returns: An integer indicating whether the object is a projection graphics object (1), or not (0).
:rtype: int
"""
    if (isinstance(obj, projection.Proj)):
        return 1
    else:
        return 0


def istaylordiagram(obj):
    if (isinstance(obj, taylor.Gtd)):
        return 1
    else:
        return 0
istaylordiagram.__doc__ = xmldocs.istaylordiagram_doc


def ismeshfill(obj):
    if (isinstance(obj, meshfill.Gfm)):
        return 1
    else:
        return 0
ismeshfill.__doc__ = xmldocs.ismeshfill_doc


def isboxfill(obj):
    if (isinstance(obj, boxfill.Gfb)):
        return 1
    else:
        return 0
isboxfill.__doc__ = xmldocs.isboxfill_doc


def is3d_scalar(obj):
    if (isinstance(obj, dv3d.Gf3Dscalar) or isinstance(obj, dv3d.Gf3DDualScalar)):
        return 1
    else:
        return 0
is3d_scalar.__doc__ = xmldocs.is3d_scalar_doc


def is3d_dual_scalar(obj):
    if isinstance(obj, dv3d.Gf3DDualScalar):
        return 1
    else:
        return 0
is3d_dual_scalar.__doc__ = xmldocs.is3d_dual_scalar_doc


def is3d_vector(obj):
    if (isinstance(obj, dv3d.Gf3Dvector)):
        return 1
    else:
        return 0
is3d_vector.__doc__ = xmldocs.is3d_vector_doc


def isisofill(obj):
    if (isinstance(obj, isofill.Gfi)):
        return 1
    else:
        return 0
isisofill.__doc__ = xmldocs.isisofill_doc


def isisoline(obj):
    if (isinstance(obj, isoline.Gi)):
        return 1
    else:
        return 0
isisoline.__doc__ = xmldocs.isisoline_doc


def isscatter(obj):
    if (isinstance(obj, unified1D.G1d)) and obj.g_type == "scatter":
        return 1
    else:
        return 0
isscatter.__doc__ = xmldocs.isscatter_doc


def isxyvsy(obj):
    if (isinstance(obj, unified1D.G1d)) and obj.g_type == "xyvsy":
        return 1
    else:
        return 0
isxyvsy.__doc__ = xmldocs.isxyvsy_doc


def isyxvsx(obj):
    if (isinstance(obj, unified1D.G1d)) and obj.g_type == "yxvsx":
        return 1
    else:
        return 0
isyxvsx.__doc__ = xmldocs.isyxvsx_doc


def isxvsy(obj):
    if (isinstance(obj, unified1D.G1d)) and obj.g_type == "yxvsx":
        return 1
    else:
        return 0
isxvsy.__doc__ = xmldocs.isxvsy_doc


def is1d(obj):
    if (isinstance(obj, unified1D.G1d)):
        return 1
    else:
        return 0
is1d.__doc__ = xmldocs.is1d_doc


def isvector(obj):
    if (isinstance(obj, vector.Gv)):
        return 1
    else:
        return 0
isvector.__doc__ = xmldocs.isvector_doc


def isline(obj):
    if (isinstance(obj, line.Tl)):
        return 1
    else:
        return 0
isline.__doc__ = xmldocs.isline_doc


def ismarker(obj):
    if (isinstance(obj, marker.Tm)):
        return 1
    else:
        return 0
ismarker.__doc__ = xmldocs.ismarker_doc


def isfillarea(obj):
    if (isinstance(obj, fillarea.Tf)):
        return 1
    else:
        return 0
isfillarea.__doc__ = xmldocs.isfillarea_doc


def istexttable(obj):
    if (isinstance(obj, texttable.Tt)):
        return 1
    else:
        return 0
istexttable.__doc__ = xmldocs.istexttable_doc


def istextorientation(obj):
    if (isinstance(obj, textorientation.To)):
        return 1
    else:
        return 0
istextorientation.__doc__ = xmldocs.istextorientation_doc


def istextcombined(obj):
    if (isinstance(obj, textcombined.Tc)):
        return 1
    else:
        return 0
istextcombined.__doc__ = xmldocs.istextcombined_doc

# Set an alias for the secondary text combined method in VCS.               #
# This is much easier to type than 'textcombined'.                          #
istext = istextcombined
