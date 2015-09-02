# Adapted for numpy/ma/cdms2 by convertcdms.py
# Tutorial for xmgrace module, using Numeric only

# it is recomended to run it usibng parser.py or just read it

print 'it is recomended to run this file using parser.py or just read it'

# ******************************************************************************
#
# Some preliminary stuff you can ignore.......
# These few steps are just to ensure that the tutorial finds the
# required data files.
import os
import sys
TESTDIR = '../cdat_tutorial_data/'
if len(sys.argv) > 2:
    TESTDIR = sys.argv[2] + '/'
elif len(sys.argv) > 1:
    TESTDIR = sys.argv[1] + '/'
if not os.path.isdir(TESTDIR):
    print 'Cannot find the tutorials data directory, please pass path as first argument.'
    print 'e.g.: python getting_started.py ../cdat_tutorial_data'
    sys.exit()
if not os.path.exists(TESTDIR + 'model.ctl'):
    print 'Cannot find the tutorials data, please pass path as first argument.'
    print 'e.g.: python getting_started.py ../cdat_tutorial_data'
    sys.exit()

TEMPDIR = './'

# quick example for using xmgrace
import cdms2 as cdms
from cdms2 import MV2 as MV

# preliminary work, retrieve data and make a zonal mean, 2 different year
f = cdms.open(TESTDIR + 'tas.rnl_ncep.nc')
tim = f.getAxis('time')
s1 = f('tas', time=(tim[0], tim[11]))
# Compute time mean
s1 = MV.average(s1)
# compute zonal mean
z1 = MV.average(s1, 1)
# Now the last year
s2 = f('tas', time=(tim[-12], tim[-1]))
# Compute time mean
s2 = MV.average(s2)
# compute zonal mean
z2 = MV.average(s2, 1)
# Now computes the difference
z3 = z2 - z1

# Now the real grace thing, plot the 2 on one graph and the diff on
# another graph

from genutil import xmgrace  # first we need to import xmgrace module

x = xmgrace.init()  # create our xmgrace object

# First let's set our graphs area
# graph 0, exist by default, but we need to add 1 graph, therefore:
x.add_graph()  # adds one graph to our graph list (x.Graph)

# Let's change the orientation of the page to portrait
x.portrait()

# Now let's set the area of graph 0
x.Graph[0].vymin = .55  # starts at 55% of the page
x.Graph[0].vymax = .9   # stops at 90% of the page
x.Graph[0].vxmin = .1  # starts at 10% of the page
x.Graph[0].vxmax = .75   # stops at 75% of the page
# and the area of graph 1
x.Graph[1].vymin = .1  # starts at 10% of the page
x.Graph[1].vymax = .45  # 45 % of page
# Let's offset the 2 graphs just for fun
x.Graph[1].vxmin = .25  # starts at 55% of the page
x.Graph[1].vxmax = .9   # stops at 90% of the page

# let's set the titles and subtitles
x.Graph[0].title = 'NCEP Reanalysis, Surface air temperature'
x.Graph[0].stitle = 'First and Last year'
x.Graph[1].stitle = 'Difference'


# ok now let's set the min ans max for the Yaxis
x.Graph[0].yaxis.min = 230.  # ymin for graph 0
x.Graph[0].yaxis.max = 305.  # ymax for graph 0
x.Graph[1].yaxis.min = -1.5  # ymin for graph 1
x.Graph[1].yaxis.max = 1.5  # ymax for graph 1

# Now let's set the tick marks for Graph 0
x.Graph[0].yaxis.tick.inc = 10  # Main tick every 5K
x.Graph[0].yaxis.tick.minor_ticks = 4  # 4 sub in between , 1 every 2K
# Now let's set the tick marks for Graph 1
x.Graph[1].yaxis.tick.inc = 1  # Main tick every K
x.Graph[1].yaxis.tick.minor_ticks = 4  # 4 sub in between , 1 every .2K
# Ok now let's plot the Xaxis at area weighted zonal mean
# First we need to get the values
lats = z1.getLatitude()
# Converts it to area weighted
lats = cdms.createAxis(MV.sin(lats[:] / 180. * MV.pi))
lats.designateLatitude()
# Puts these values back into the slab
z1.setAxis(0, lats)
# Same thing for second and third slab
z2.setAxis(0, lats)
z3.setAxis(0, lats)

# x values are now between -1 and 1
# therefore let's set the axis  from -1,1
x.Graph[0].xaxis.min = -1.
x.Graph[0].xaxis.max = 1.
x.Graph[1].xaxis.min = -1.
x.Graph[1].xaxis.max = 1.

# Set the xaxis ticks marks from 2 ways
# way one the "Normal/more flexible way"
dic = {-1.: '90S', -0.866: '60S', -0.5: '30S',
       0.: 'Eq', 0.5: '30N', 0.866: '60N', 1.: '90N'}
x.Graph[0].xaxis.tick.spec.loc = dic
# way 2 the "less convential" way
x.Graph[1].xaxis.tick.type = 'zmean'
# Ok now we want the North pole at the left, therefore reverse the axis
x.Graph[0].xaxis.invert = 'on'
x.Graph[1].xaxis.invert = 'on'
# Finally let's reduce the size a bit
x.Graph[0].xaxis.tick.label.char_size = .8
x.Graph[1].xaxis.tick.label.char_size = .8

# Ok now the legend
x.Graph[0].legend.char_size = .8
x.Graph[0].legend.x = .8  # Legend at 80% in x
x.Graph[0].legend.x = .8  # Legend at 80% in y
# or
x.Graph[1].legend.char_size = .8
x.Graph[1].legend.x = .05
x.Graph[1].legend.y = .35

# Ok now let's play with the appearence of the sets themself
# Ok by default we only have 2 sets (1 per graph)
# So we need to add 2 sets
x.add_set(0)  # adding to graph 0 (since it's 0 it is optional)

# First let's change the line width
x.Graph[0].Set[0].line.linewidth = 2
x.Graph[0].Set[1].line.linewidth = 2
x.Graph[1].Set[0].line.linewidth = 2
# Let's set the colors
x.Graph[0].Set[0].line.color = 'red'
x.Graph[0].Set[1].line.color = 4  # color 4 is blue
# Let's set the second's set style to dash
x.Graph[1].Set[0].line.linestyle = 'dot'
x.Graph[0].Set[1].line.linestyle = 'dash'
x.Graph[0].Set[0].line.linestyle = 2  # 2 is dash

# Now let's set the name for each set, to be used in the legend
x.Graph[0].Set[0].legend = 'Year 1'
x.Graph[0].Set[1].legend = 'Year 2'
x.Graph[1].Set[0].legend = 'Year 2-1'
# ok one last trick, in order to have the 0 line plotted
# let's create a dummy array with zeros in it:
zero = MV.zeros((2), MV.Float)
zero = MV.asVariable(zero)
zero.setdimattribute(0, 'values', [-1., 1.])
# add a dataset and assign it graph 1 black color
x.add_set(1, 'black')

# Finally just for fun let's place a big red "Sample" accross it
x.add_string(0.5, 0.5, 'Sample', color='red', char_size=9, rot=55, just=14)

# And plot these babies:
# You MUST pass a list of slab, even if only one slab
x.plot([z1, z2, z3, zero])
# or you can plot then 1 by 1
# first let's clear
x('kill G0.S0')
x('kill G0.S1')
x('kill G1.S0')
x('kill G1.S1')
x.plot(z1, G=0, S=0)
x.plot(z2, G=0, S=1)
x.plot(z3, G=1, S=0)
x.plot(zero, G=1, S=1)
# Finally let's save the result
x.ps(TEMPDIR + 'xmgrace_demo_cdms')  # postscript
x.ps(TEMPDIR + 'xmgrace_demo_cdms_gray',
     color='grayscale')  # grayscale postscript
# jpeg 300dpi, 80% quality compression
x.jpeg(TEMPDIR + 'xmgrace_demo_cdms', dpi=300, quality=80)
x.pdf(TEMPDIR + 'xmgrace_demo_cdms')
x.eps(TEMPDIR + 'xmgrace_demo_cdms')
x.svg(TEMPDIR + 'xmgrace_demo_cdms')
x.mif(TEMPDIR + 'xmgrace_demo_cdms')
x.pnm(TEMPDIR + 'xmgrace_demo_cdms')
x.png(TEMPDIR + 'xmgrace_demo_cdms')
x.metafile(TEMPDIR + 'xmgrace_demo_cdms')

# Finish
x.close()
