plot_keywords_doc = """
xaxis :: (cdms2.axis.TransientAxis) () Axis object to replace the slab -1 dim axis
yaxis :: (cdms2.axis.TransientAxis) () Axis object to replace the slab -2 dim axis, only if slab has more than 1D
zaxis :: (cdms2.axis.TransientAxis) () Axis object to replace the slab -3 dim axis, only if slab has more than 2D
taxis :: (cdms2.axis.TransientAxis) () Axis object to replace the slab -4 dim axis, only if slab has more than 3D
waxis :: (cdms2.axis.TransientAxis) () Axis object to replace the slab -5 dim axis, only if slab has more than 4D
xrev :: (bool) () reverse x axis
yrev :: (bool) () reverse y axis, only if slab has more than 1D
xarray :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of x axis
yarray :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of y axis, only if var has more than 1D
zarray :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of z axis, only if var has more than 2D
tarray :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of t axis, only if var has more than 3D
warray :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of w axis, only if var has more than 4D
continents :: (int) () continents type number
name :: (str) () replaces variable name on plot
time "" (cdtime.comptime/cdtime.reltime/cdtime.abstime) () replaces time name on plot
units :: (str) () replaces units value on plot
ymd :: (str) () replaces year/month/day on plot
hms :: (str) () replaces hh/mm/ss on plot
file_comment :: (str) () replaces file_comment on plot
xbounds :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of x axis bounds values
ybounds :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () Values to use instead of y axis bounds values (if exist)
xname :: (str) () replace xaxis name on plot
yname :: (str) () replace yaxis name on plot (if exists)
zname :: (str) () replace zaxis name on plot (if exists)
tname :: (str) () replace taxis name on plot (if exists)
wname :: (str) () replace waxis name on plot (if exists)
xunits :: (str) () replace xaxis units on plot
yunits :: (str) () replace yaxis units on plot (if exists)
zunits :: (str) () replace zaxis units on plot (if exists)
tunits :: (str) () replace taxis units on plot (if exists)
wunits :: (str) () replace waxis units on plot (if exists)
xweights :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () replace xaxis weights used for computing mean
yweights :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) () replace xaxis weights used for computing mean
comment1 :: (str) () replaces comment1 on plot
comment2 :: (str) () replaces comment2 on plot
comment3 :: (str) () replaces comment3 on plot
comment4 :: (str) () replaces comment4 on plot
long_name :: (str) () replaces long_name on plot
grid :: (cdms2.grid.TransientRectGrid) () replaces array grid (if exists)
bg :: (bool/int) () plots in background mode
ratio :: (int/str) () sets the y/x ratio ,if passed as a string with 't' at the end, will aslo moves the ticks
"""

data_time = """
    datawc_timeunits :: (str) ('days since 2000') units to use when disaplaying time dimension auto tick
    datawc_calendar:: (int) (135441) calendar to use when displaying time dimension auto tick, default is proleptic gregorian calendar
"""
graphics_method_core_notime = """
    xmtics1 :: (str/{float:str}) ('') dictionary with location of intermediate tics as keys for 1st side of y axis
    xmtics2 :: (str/{float:str}) ('') dictionary with location of intermediate tics as keys for 2nd side of y axis
    ymtics1 :: (str/{float:str}) ('') dictionary with location of intermediate tics as keys for 1st side of y axis
    ymtics2 :: (str/{float:str}) ('') dictionary with location of intermediate tics as keys for 2nd side of y axis
    xticlabels1 :: (str/{float:str}) ('*') values for labels on 1st side of x axis
    xticlabels2 :: (str/{float:str}) ('*') values for labels on 2nd side of x axis
    yticlabels1 :: (str/{float:str}) ('*') values for labels on 1st side of y axis
    yticlabels2 :: (str/{float:str}) ('*') values for labels on 2nd side of y axis
    projection :: (str/vcs_legacy.projection.Proj) ('default') projection to use, name or object
    datawc_x1 :: (float) (1.E20) first value of xaxis on plot
    datawc_x2 :: (float) (1.E20) second value of xaxis on plot
    datawc_y1 :: (float) (1.E20) first value of yaxis on plot
    datawc_y2 :: (float) (1.E20) second value of yaxis on plot
"""
graphics_method_core = """%s
%s""" % (graphics_method_core_notime , data_time)
axisconvert = """    %saxisconvert :: (str) ('linear') converting %saxis linear/log/log10/ln/exp/area_wt\n """
xaxisconvert = axisconvert % ("x","x")
yaxisconvert = axisconvert % ("y","y")
axesconvert = xaxisconvert + yaxisconvert
colorsdoc = """Options:::
           color1 :: (int) (16) value for color_1
           color2 :: (int) (239) value for color_2
           :::
           Sets the color_1 and color_2 values on the object
           """
    
extsdoc = """Options:::
           ext1 :: (str) ('n') value for ext_1
           ext2 :: (str) ('n') value for ext_2
           :::
           Sets the ext_1 and ext_2 values on the object
           """
ticlabeldoc = """Options:::
           %stl1 :: ({float:str}) ('') value for %sticlabels1
           %stl2 :: ({float:str}) ('') value for %sticlabels2
           :::
           Sets the %sticlabels1 and %sticlabels2 values on the object
           """
xticlabelsdoc = ticlabeldoc % (('x',)*6)
yticlabelsdoc = ticlabeldoc % (('y',)*6)

mticsdoc = """Options:::
           %smt1 :: ({float:str}) ('') value for %smtics1
           %smt2 :: ({float:str}) ('') value for %smtics2
           :::
           Sets the %smtics1 and %smtics2 values on the object
           """
xmticsdoc = mticsdoc % (('x',)*6)
ymticsdoc = mticsdoc % (('y',)*6)

datawcdoc = """Options:::
           dsp1 :: (float) (1e20) datawc_y1
           dsp2 :: (float) (1e20) datawc_y2
           dsp3 :: (float) (1e20) datawc_x1
           dsp4 :: (float) (1e20) datawc_x2
           :::
           Sets the data world coordinates for object
           """
xyscaledoc = """Options:::
           xat :: (str) ('linear') value for x axis convertion
           yat :: (str) ('linear') value for x axis convertion
           :::
           Sets the xaxisconvert and yaxisconvert values for object
           """
listdoc = """ Lists object values """

scriptdoc = """ Input:::
 script_filename :: (str) (0) name of the file to which the script will be saved
 :::
 Options:::
 mode :: (str) ('a') mode to open the file 'a' or 'w' (overwrite)
 :::"""

isofill_doc = """levels :: ([float,...]/[[float,float],...]) (([1.E20,1.E20],)) sets the levels range to use, can be either a list of contiguous levels, or list of tuples indicating, first and last value of the range
    fillareacolors :: ([int,...]) ([241]) colors to use for each level
    fillareastyle :: (str) ('solid') style to use for levels filling: solid/pattenr/hatch 
    fillareaindices :: ([int,...]) (None) list of patterns to use when filling a level and using pattern/hatch
    legend :: ({float:str}) (None) replaces the legend values in the dictionary keys with their associated string
    ext_1 :: (str) ('n') draws an extension arrow on right side (values less than first range value)
    ext_2 :: (str) ('n') draws an extension arrow on left side (values greater than last range value)
    missing :: (int) (241) color to use for missing value or values not in defined ranges
    legend :: (None/{float:str}) (None) replaces the legend values in the dictionary keys with their associated string
"""

outfilldoc = """outfill ([int,...]) ([1]) values to outfill """
outlinerdoc = """outline ([int,...]) ([1]) values to outline """

fillareadoc ="""
    fillareacolor :: (int) (None) color to use for outfilling
    fillareastyle :: (str) ('solid') style to use for levels filling: solid/pattenr/hatch 
    fillareaindex :: (int) (None) pattern to use when filling a level and using pattern/hatch
"""

linesdoc ="""    line :: ([str,...]/[vcs_legacy.line.Tl,...]/[int,...]) (['solid',]) line type to use for each isoline, can also pass a line object or line object name
    linecolors :: ([int,...]) ([241]) colors to use for each isoline
    linewidths :: ([float,...]) ([1.0]) list of width for each isoline
    """
linedoc ="""    line :: ([str,...]/[vcs_legacy.line.Tl,...]/[int,...]) (['solid',]) line type to use for each isoline, can also pass a line object or line object name
    linecolor :: (int) (241) colors to use for each isoline
    linewidth :: (float) (1.0) list of width for each isoline
    """

textsdoc = """
    text :: (None/[vcs_legacy.textcombined.Tc,...]) (None) text objects or text objects names to use for each countour labels
    textcolors :: (None/[int,...]) (None) colors to use for each countour labels
"""

markerdoc = """
    marker :: (None/int/str/vcs_legacy.marker.Tm) (None) markers type to use
    markercolor :: (None/int) (None) color to use for markers
    markersize :: (None/int) (None) size of markers
"""

#############################################################################
#                                                                           #
# Graphics Method input section.                                            #
#                                                                           #
#############################################################################

create_GM_input = """
    new_GM_name :: (str) (0) name of the new graphics method object. If no name is given, then one will be created for use.
    source_GM_name :: (str) (1) copy the contents of the source object to the newly created one. If no name is given, then the 'default' graphics methond contents is copied over to the new object.
"""

get_GM_input = """
    GM_name :: (str) (0) retrieve the graphics method object of the given name. If no name is given, then retrieve the 'default' graphics method.
"""

plot_1D_input = """
       slab :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) (0) Data at least 1D, last dimension will be plotted
"""

plot_2D_input = """
       slab :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) (0) Data at least 2D, last 2 dimensions will be plotted
"""

plot_2_1D_input = """
       slab_or_primary_object :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list/vcs_legacy.fillarea.Tf/vcs_legacy.line.Tl/vcs_legacy.marker.Tm/vcs_legacy.textcombined.Tc) (None) Data at least 1D, last dimension(s) will be plotted, or primary vcs_legacy object
"""
plot_2_1D_options = """
       slab2 :: (cdms2.tvariable.TransientVariable/numpy.core.ma.MaskedArray/numpy.ndarray/list) (None) Data at least 1D, last dimension(s) will be plotted
       template :: (str/vcs_legacy.template.P) ('default') vcs_legacy template
       gm :: (str/vcs_legacy.boxfill.Gfb/vcs_legacy.isofill.Gfi/vcs_legacy.isoline.Gi/vcs_legacy.meshfill.Gfm/vcs_legacy.vector.Gv/vcs_legacy.scatter.GSp/vcs_legacy.outline.Go/vcs_legacy.outline.Gfo/vcs_legacy.taylor.Gtd/vcs_legacy.xvsy.GXY/vcs_legacy.xyvsy.GXy/vcs_legacy.yxvsx.GYx/vcs_legacyaddons.core.VCSaddon/vcs_legacy.continents.Gcon) ('default') graphic method to use
"""
#############################################################################
#                                                                           #
# Graphics Methon output section.                                           #
#                                                                           #
#############################################################################
plot_output = """
       display ::  (vcs_legacy.displayplot.Dp) (0) no default
"""

boxfill_output = """
       boxfill ::  (vcs_legacy.boxfill.Gfb) (0) no default
"""

isofill_output = """
       isofill ::  (vcs_legacy.isofill.Gfi) (0) no default
"""

isoline_output = """
       isoline ::  (vcs_legacy.isoline.Gi) (0) no default
"""

yxvsx_output = """
       yxvsx ::  (vcs_legacy.yxvsx.GYx) (0) no default
"""

xyvsy_output = """
       xyvsy ::  (vcs_legacy.xyvsy.GXy) (0) no default
"""

xvsy_output = """
       xvsy ::  (vcs_legacy.xvsy.GXY) (0) no default
"""

scatter_output = """
       scatter ::  (vcs_legacy.scatter.GSp) (0) no default
"""

outfill_output = """
       outfill ::  (vcs_legacy.outfill.Gfo) (0) no default
"""

outline_output = """
       outline ::  (vcs_legacy.outline.Go) (0) no default
"""
