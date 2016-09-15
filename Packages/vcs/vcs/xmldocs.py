plot_keywords_doc = """
:param xaxis: Axis object to replace the slab -1 dim axis
:param yaxis: Axis object to replace the slab -2 dim axis, only if slab has more than 1D
:param zaxis: Axis object to replace the slab -3 dim axis, only if slab has more than 2D
:param taxis: Axis object to replace the slab -4 dim axis, only if slab has more than 3D
:param waxis: Axis object to replace the slab -5 dim axis, only if slab has more than 4D
:param xrev: reverse x axis
:param yrev: reverse y axis, only if slab has more than 1D
:param xarray: Values to use instead of x axis
:param yarray: Values to use instead of y axis, only if var has more than 1D
:param zarray: Values to use instead of z axis, only if var has more than 2D
:param tarray: Values to use instead of t axis, only if var has more than 3D
:param warray: Values to use instead of w axis, only if var has more than 4D
:param continents: continents type number
:param name: replaces variable name on plot
:param time: replaces time name on plot
:param units: replaces units value on plot
:param ymd: replaces year/month/day on plot
:param hms: replaces hh/mm/ss on plot
:param file_comment: replaces file_comment on plot
:param xbounds: Values to use instead of x axis bounds values
:param ybounds: Values to use instead of y axis bounds values (if exist)
:param xname: replace xaxis name on plot
:param yname: replace yaxis name on plot (if exists)
:param zname: replace zaxis name on plot (if exists)
:param tname: replace taxis name on plot (if exists)
:param wname: replace waxis name on plot (if exists)
:param xunits: replace xaxis units on plot
:param yunits: replace yaxis units on plot (if exists)
:param zunits: replace zaxis units on plot (if exists)
:param tunits: replace taxis units on plot (if exists)
:param wunits: replace waxis units on plot (if exists)
:param xweights: replace xaxis weights used for computing mean
:param yweights: replace xaxis weights used for computing mean
:param comment1: replaces comment1 on plot
:param comment2: replaces comment2 on plot
:param comment3: replaces comment3 on plot
:param comment4: replaces comment4 on plot
:param long_name: replaces long_name on plot
:param grid: replaces array grid (if exists)
:param bg: plots in background mode
:param ratio: sets the y/x ratio ,if passed as a string with 't' at the end, will aslo moves the ticks
:type xaxis: cdms2.axis.TransientAxis
:type yaxis: cdms2.axis.TransientAxis
:type zaxis: cdms2.axis.TransientAxis
:type taxis: cdms2.axis.TransientAxis
:type waxis: cdms2.axis.TransientAxis
:type xrev: bool
:type yrev: bool
:type xarray: array
:type yarray: array
:type zarray: array
:type tarray: array
:type warray: array
:type continents: int
:type name: str
:type time: A cdtime object
:type units: str
:type ymd: str
:type hms: str
:type file_comment: str
:type xbounds: array
:type ybounds: array
:type xname: str
:type yname: str
:type zname: str
:type tname: str
:type wname: str
:type xunits: str
:type yunits: str
:type zunits: str
:type tunits: str
:type wunits: str
:type xweights: array
:type yweights: array
:type comment1: str
:type comment2: str
:type comment3: str
:type comment4: str
:type long_name: str
:type grid: cdms2.grid.TransientRectGrid
:type bg: bool/int
:type ratio: int/str
"""  # noqa

data_time = """
            .. py:attribute:: datawc_timeunits (str)

                (Ex: 'days since 2000') units to use when displaying time dimension auto tick

            .. py:attribute:: datawc_calendar (int)

                (Ex: 135441) calendar to use when displaying time dimension auto tick, default is proleptic gregorian calendar

"""  # noqa
graphics_method_core_notime = """
            .. py:attribute:: xmtics1 (str/{float:str})

                (Ex: '') dictionary with location of intermediate tics as keys for 1st side of y axis

            .. py:attribute:: xmtics2 (str/{float:str})

                (Ex: '') dictionary with location of intermediate tics as keys for 2nd side of y axis

            .. py:attribute:: ymtics1 (str/{float:str})

                (Ex: '') dictionary with location of intermediate tics as keys for 1st side of y axis

            .. py:attribute:: ymtics2 (str/{float:str})

                (Ex: '') dictionary with location of intermediate tics as keys for 2nd side of y axis

            .. py:attribute:: xticlabels1 (str/{float:str})

                (Ex: '*') values for labels on 1st side of x axis

            .. py:attribute:: xticlabels2 (str/{float:str})

                (Ex: '*') values for labels on 2nd side of x axis

            .. py:attribute:: yticlabels1 (str/{float:str})

                (Ex: '*') values for labels on 1st side of y axis

            .. py:attribute:: yticlabels2 (str/{float:str})

                (Ex: '*') values for labels on 2nd side of y axis

            .. py:attribute:: projection (str/vcs.projection.Proj)

                (Ex: 'default') projection to use, name or object

            .. py:attribute:: datawc_x1 (float)

                (Ex: 1.E20) first value of xaxis on plot

            .. py:attribute:: datawc_x2 (float)

                (Ex: 1.E20) second value of xaxis on plot

            .. py:attribute:: datawc_y1 (float)

                (Ex: 1.E20) first value of yaxis on plot

            .. py:attribute:: datawc_y2 (float)

                (Ex: 1.E20) second value of yaxis on plot
            """  # noqa
graphics_method_core = """%s
%s""" % (graphics_method_core_notime, data_time)
axisconvert = """
    :param {axis}axisconvert: (Ex: 'linear') converting {axis}axis linear/log/log10/ln/exp/area_wt
    :type {axis}axisconvert: str\n"""
xaxisconvert = axisconvert.format(axis="x")
yaxisconvert = axisconvert.format(axis="y")
axesconvert = xaxisconvert + yaxisconvert
colorsdoc = """
    Sets the color_1 and color_2 properties of the object.

    :param color1: Sets the :py:attr:`color_1` value on the object
    :type color1: int

    :param color2: Sets the :py:attr:`color_2` value on the object
    :type color2: int
    """

extsdoc = """
        Sets the ext_1 and ext_2 values on the object.

:param ext1: Sets the :py:attr:`ext_1` value on the object. 'y' sets it to True, 'n' sets it to False.
:type ext1: str

:param ext2: Sets the :py:attr:`ext_2` value on the object. 'y' sets it to True, 'n' sets it to False.
:type ext2: str
           """
ticlabeldoc = """
        Sets the %sticlabels1 and %sticlabels2 values on the object

:param %stl1: Sets the object's value for :py:attr:`%sticlabels1`.
              Must be  a str, or a dictionary object with float:str mappings.
:type %stl1: {float:str} or str

:param %stl2: Sets the object's value for :py:attr:`%sticlabels2`.
              Must be a str, or a dictionary object with float:str mappings.
:type %stl2: {float:str} or str
           """
xticlabelsdoc = ticlabeldoc % (('x',) * 8)
yticlabelsdoc = ticlabeldoc % (('y',) * 8)

mticsdoc = """
        Sets the %smtics1 and %smtics2 values on the object

:param %smt1: Value for :py:attr:`%smtics1`. Must be a str, or a dictionary object with float:str mappings.
:type %smt1: {float:str} or str

:param %smt2: Value for :py:attr:`%smtics2`. Must be a str, or a dictionary object with float:str mappings.
:type %smt2: {float:str} or str
"""
xmticsdoc = mticsdoc % (('x',) * 8)
ymticsdoc = mticsdoc % (('y',) * 8)

datawcdoc = """
    Sets the data world coordinates for object

    :param dsp1: Sets the :py:attr:`datawc_y1` property of the object.
    :type dsp1: float

    :param dsp2: Sets the :py:attr:`datawc_y2` property of the object.
    :type dsp2: float

    :param dsp3: Sets the :py:attr:`datawc_x1` property of the object.
    :type dsp3: float

    :param dsp4: Sets the :py:attr:`datawc_x2` property of the object.
    :type dsp4: float

    """
xyscaledoc = """
        Sets xaxisconvert and yaxisconvert values for the object.

        :Example:

::

        a=vcs.init()
        #create a boxfill to work with
        box=a.createboxfill('temp')
        # set xaxisconvert and yaxisconvert to 'linear'
        box.xyscale(xat='linear', yat='linear')

    :param xat: Set value for x axis conversion.
    :type xat: str

    :param yat: Set value for y axis conversion.
    :type yat: str
           """
listdoc = """ Lists the current values of object attributes"""

# Scriptdocs section

# Use this dictionary for string replacements
#   dict keys are 'type', 'name', and 'call'
#       'type' : The type of VCS object it is (i.e. Graphics method, secondary method, etc.)
#       'name' : The name of the VCS object (i.e. boxfill, isofill, etc.)
#       'call' : The function call for the object. Mostly, this is == name.
#                   Some rare cases, like textcombined, require adjustment of this value.
dict = {}
dict['name'] = dict['type'] = dict['call'] = 'REPLACE_ME'


scriptdoc = """
    Saves out a copy of the %(name)s %(type)s in JSON, or Python format to a designated file.

        .. note::
            If the the filename has a '.py' at the end, it will produce a
            Python script. If no extension is given, then by default a
            .json file containing a JSON serialization of the object's
            data will be produced.

        .. warning::
            VCS Scripts Deprecated.
            SCR scripts are no longer generated by this function.

    :Example:

        .. doctest:: script_examples

            # Make a Canvas object to work with:
            >>> a=vcs.init()
            ...

            # Create %(call)s 'temp' that inherits from 'default'
            >>> ex=a.create%(call)s('temp')
            ...

            # Append to a Python script named 'filename.py'
            >>> ex.script('filename.py')
            ...

            # Create or overwrite a JSON file 'filename.json'.
            >>> ex.script('filename','w')
            ...

    :param script_filename: Output name of the script file. If no extension is specified, a .json object is created.
    :type script_filename: str

    :param mode: Either 'w' for replace, or 'a' for append. Defaults to 'a', if not specified.
    :type mode: str
"""


# Graphics Method scriptdocs
dict['type'] = 'graphics method'
dict['name'] = dict['call'] = 'colormap'
colormap_script = scriptdoc % dict

dict['name'] = dict['call'] = 'boxfill'
boxfill_script = scriptdoc % dict

dict['name'] = dict['call'] = 'isoline'
isoline_script = scriptdoc % dict

dict['name'] = dict['call'] = 'isofill'
isofill_script = scriptdoc % dict

dict['name'] = dict['call'] = 'yxvsx'
yxvsx_script = scriptdoc % dict

dict['name'] = dict['call'] = 'meshfill'
meshfill_script = scriptdoc % dict

dict['name'] = dict['call'] = 'fillarea'
fillarea_script = scriptdoc % dict

dict['name'] = dict['call'] = 'marker'
marker_script = scriptdoc % dict

dict['name'] = dict['call'] = 'line'
line_script = scriptdoc % dict

dict['name'] = 'text table and text orientation'
dict['call'] = 'textcombined'
textcombined_script = scriptdoc % dict

dict['name'] = dict['call'] = 'textorientation'
textorientation_script = scriptdoc % dict

dict['name'] = dict['call'] = 'texttable'
texttable_script = scriptdoc % dict

dict['name'] = dict['call'] = 'vector'
vector_script = scriptdoc % dict

# Object scriptdocs
dict['type'] = 'object'
dict['name'] = dict['call'] = 'template'
template_script = scriptdoc % dict

# Secondary Method scriptdocs
dict['type'] = 'secondary method'
dict['name'] = dict['call'] = 'projection'
projection_script = scriptdoc % dict

# dict['parent'] is for rare cases where there is no 'default' object to inherit from.
dict['parent'] = 'REPLACE_ME'
queries_is_doc = """
    Check to see if this object is a VCS %(type)s %(name)s graphics method.

    :Example:

    .. doctest:: queries_is

        # Make a VCS Canvas object to work with:
        >>> a=vcs.init()
        ...

        # Show all available %(name)s
        >>> a.show('%(name)s')
        *...

        # To  test an existing %(name)s object
        >>> ex = a.get%(name)s(%(parent)s)
        ...

        >>> print(vcs.queries.is%(name)s(ex))
        1

:param obj: A VCS object
:type obj: VCS Object

:returns: An integer indicating whether the object is a %(name)s graphics method (1), or not (0).
:rtype: int
"""
# queries.is[PRIMARY_OBJECT]
dict['type'] = 'primary'
dict['parent'] = "'default'"

dict['name'] = 'vector'
isvector_doc = queries_is_doc % dict
dict['name'] = 'taylordiagram'
istaylordiagram_doc = queries_is_doc % dict
dict['name'] = 'meshfill'
ismeshfill_doc = queries_is_doc % dict
dict['name'] = 'boxfill'
isboxfill_doc = queries_is_doc % dict
dict['name'] = 'isofill'
isisofill_doc = queries_is_doc % dict
dict['name'] = 'isoline'
isisoline_doc = queries_is_doc % dict
dict['name'] = '3d_scalar'
is3d_scalar_doc = queries_is_doc % dict
dict['name'] = '3d_dual_scalar'
is3d_dual_scalar_doc = queries_is_doc % dict
dict['name'] = '3d_vector'
is3d_vector_doc = queries_is_doc % dict
dict['name'] = 'xvsy'
isxvsy_doc = queries_is_doc % dict
dict['name'] = 'yxvsx'
isyxvsx_doc = queries_is_doc % dict
dict['name'] = '1d'
is1d_doc = queries_is_doc % dict

# special inheritance cases
dict['name'] = 'scatter'
dict['parent'] = "'default_scatter_'"
isscatter_doc = queries_is_doc % dict
dict['name'] = 'xyvsy'
dict['parent'] = "'default_xyvsy_'"
isxyvsy_doc = queries_is_doc % dict

# queries.is[SECONDARY_OBJECT]
dict['type'] = 'secondary'
dict['parent'] = "'default'"

dict['name'] = 'line'
isline_doc = queries_is_doc % dict
dict['name'] = 'marker'
ismarker_doc = queries_is_doc % dict
dict['name'] = 'fillarea'
isfillarea_doc = queries_is_doc % dict
dict['name'] = 'texttable'
istexttable_doc = queries_is_doc % dict
dict['name'] = 'textorientation'
istextorientation_doc = queries_is_doc % dict
dict['name'] = 'textcombined'
istextcombined_doc = queries_is_doc % dict
dict['name'] = 'marker'
ismarker_doc = queries_is_doc % dict

# TODO: revise this to use dict key references
get_methods_doc = """
    VCS contains a list of secondary methods. This function will create a
    %(name)s class object from an existing VCS %(name)s %(type)s. If
    no %(name)s name is given, then %(name)s '%(parent)s' will be used.

    .. note::

        VCS does not allow the modification of 'default' attribute sets.
        However, a 'default' attribute set that has been copied under a
        different name can be modified. (See the :ref:`vcs.manageElements.createfillarea` function.)

    :Example:

    ::

        # Show all the existing %(name)s %(type)s
        vcs.show('%(name)s')
        # instance of '%(parent)s' %(name)s %(type)s
        ex=vcs.get%(call)s()
        # instance of existing 'quick' %(name)s %(type)s
        ex2=vcs.get%(call)s('quick')
        # Create instance of %(name)s object 'red'
        ex3=vcs.create%(call)s(name='new', name='red',style=1, index=1,
                      color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                      worldcoordinate=[0,100, 0,50]
                      x=[0,20,40,60,80,100],
                      y=[0,10,20,30,40,50] )
        # Plot using specified %(call)s object
        vcs.%(name)s(ex3)
    """
# Get for secondary methods with a 'default' available
dict['parent'] = 'default'
dict['type'] = 'secondary method'

dict['name'] = dict['call'] = 'fillarea'
get_fillarea_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_texttable_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_template_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_projection_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_boxfill_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_taylor_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_meshfill_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_isofill_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_isoline_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_1d_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_xyvsy_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_yxvsx_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_xvsy_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_vector_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_scatter_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_line_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_marker_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_textorientation_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_textcombined_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_3d_scalar_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_3d_dual_scalar_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_3d_vector_doc = get_methods_doc % dict
dict['name'] = dict['call'] = 'texttable'
get_colormap_doc = get_methods_doc % dict

exts_attrs = """
            .. py:attribute:: ext_1 (str)

                Draws an extension arrow on right side (values less than first range value)

            .. py:attribute:: ext_2 (str)

                Draws an extension arrow on left side (values greater than last range value)
    """

fillarea_colors_attr = """
            .. py:attribute:: fillareacolors ([int,...])

                Colors to use for each level
"""

fillarea_attrs = """
            .. py:attribute:: fillareastyle (str)

                Style to use for levels filling: solid/pattern/hatch

            .. py:attribute:: fillareaindices ([int,...])

                List of patterns to use when filling a level and using pattern/hatch
"""

legend_attr = """
            .. py:attribute:: legend (None/{float:str})

                Replaces the legend values in the dictionary keys with their associated string
"""

level_attrs = """
            .. py:attribute:: level_1 (float)

                Sets the value of the legend's first level

            .. py:attribute:: level_2 (float)

                Sets the value of the legend's end level
"""

levels_attr = """
            .. py:attribute:: levels ([float,...]/[[float,float],...])

                Sets the levels range to use, can be either a list of contiguous levels, or list of tuples
                indicating first and last value of the range.
"""

missing_attr = """
            .. py:attribute:: missing (int)

                Color to use for missing value or values not in defined ranges
"""

meshfill_doc = """
    %s
    %s
    %s
    %s
    %s
    %s
""" % (levels_attr, fillarea_colors_attr, fillarea_attrs, legend_attr, exts_attrs, missing_attr)


isofill_doc = meshfill_doc

fillareadoc = """
    fillareacolor :: (int) (None) color to use for outfilling
    fillareastyle :: (str) ('solid') style to use for levels filling: solid/pattenr/hatch
    fillareaindex :: (int) (None) pattern to use when filling a level and using pattern/hatch
"""  # noqa

linesdoc = """    line :: ([str,...]/[vcs.line.Tl,...]/[int,...]) (['solid',]) line type to use for each isoline, can also pass a line object or line object name
    linecolors :: ([int,...]) ([241]) colors to use for each isoline
    linewidths :: ([float,...]) ([1.0]) list of width for each isoline
    """  # noqa
linedoc = """    line :: ([str,...]/[vcs.line.Tl,...]/[int,...]) (['solid',]) line type to use for each isoline, can also pass a line object or line object name
    linecolor :: (int) (241) colors to use for each isoline
    linewidth :: (float) (1.0) list of width for each isoline
    """  # noqa

textsdoc = """
    text :: (None/[vcs.textcombined.Tc,...]) (None) text objects or text objects names to use for each countour labels
    textcolors :: (None/[int,...]) (None) colors to use for each countour labels
"""  # noqa

markerdoc = """
    marker :: (None/int/str/vcs.marker.Tm) (None) markers type to use
    markercolor :: (None/int) (None) color to use for markers
    markersize :: (None/int) (None) size of markers
"""

#############################################################################
#                                                                           #
# Graphics Method input section.                                            #
#                                                                           #
#############################################################################

create_GM_input = """
:param new_GM_name: (Ex: 'my_awesome_gm') name of the new graphics method object. If no name is given, then one will be created for use.
:type new_GM_name: str
:param source_GM_name: (Ex: 'default') copy the contents of the source object to the newly created one. If no name is given, then the 'default' graphics methond contents is copied over to the new object.
:type source_GM_name: str
"""  # noqa

get_GM_input = """
:param GM_name: (Ex: 'default') retrieve the graphics method object of the given name. If no name is given, then retrieve the 'default' graphics method.
:type GM_name: str
"""  # noqa

plot_1D_input = """
:param slab: (Ex: [1, 2]) Data at least 1D, last dimension will be plotted
:type slab: array
"""  # noqa

plot_2D_input = """
:param slab: (Ex: [[0, 1]]) Data at least 2D, last 2 dimensions will be plotted
:type slab: array
"""  # noqa

plot_2_1D_input = """
:param slab_or_primary_object: Data at least 1D, last dimension(s) will be plotted, or secondary vcs object
:type slab_or_primary_object: array
"""  # noqa
plot_2_1D_options = """
:param slab2: Data at least 1D, last dimension(s) will be plotted
:param template: ('default') vcs template to use
:param gm: (Ex: 'default') graphic method to use
:type slab2: array
:type template: str/vcs.template.P
:type gm: VCS graphics method object
"""  # noqa
#############################################################################
#                                                                           #
# Graphics Method output section.                                           #
#                                                                           #
#############################################################################
plot_output = """
:return: Display Plot object representing the plot.
:rtype: vcs.displayplot.Dp
"""

boxfill_output = """
       boxfill :: (Ex: 0) no default
"""

isofill_output = """
       isofill :: (Ex: 0) no default
"""

isoline_output = """
       isoline :: (Ex: 0) no default
"""

yxvsx_output = """
       yxvsx :: (Ex: 0) no default
"""

xyvsy_output = """
       xyvsy :: (Ex: 0) no default
"""

xvsy_output = """
       xvsy :: (Ex: 0) no default
"""

scatter_output = """
       scatter :: (Ex: 0) no default
"""

outfill_output = """
       outfill :: (Ex: 0) no default
"""

outline_output = """
       outline :: (Ex: 0) no default
"""
