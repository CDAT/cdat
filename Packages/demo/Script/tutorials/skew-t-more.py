# Adapted for numpy/ma/cdms2 by convertcdms.py
# First import necessary modules
import sys,os,thermo,vcs,cdms2 as cdms

# initialize the VCS Canvas and creates the "Thermodynamic Diagram" graphic method
x=vcs.init()
x.portrait()
th=thermo.Gth(x=x,name='test')

## List setable items
th.list()

## Setting type of thermodynamic diagram, you can choose from: 'emagram', 'tephigram', 'stuve' or 'skewT'
th.type='skewT'

## Skewness of the plot
## th.skewness=-35.

## Graphic finess, higher number are better quality but slower
th.detail=75

## World Coordinates
## Temperatures at the bottom of the graph (in C)
th.datawc_x1=-50.
th.datawc_x2=50.
## Pressure at bottom and top of graph (in hPa)
## WARNING: worldcoordinate here are set in hPA but data level axis must be in Pa, not consistent
th.datawc_y1=1050.
th.datawc_y2=100.

## Drawing of paper, decide what to draw or not (1:yes , 0: no)
th.drawisothermsfilled=1
th.drawisotherms=1
th.drawisobars=1
th.drawdryadiabats=1
th.drawpseudoadiabats=1
th.drawmixingratio=1

## Create a template for T(P) i.e skewT paper
template=x.createtemplate('new')
template.data.x1=.1
template.data.x2=.85
template.data.y1=.1
template.data.y2=.9
template.box1.x1=template.data.x1
template.box1.x2=template.data.x2
template.box1.y1=template.data.y1
template.box1.y2=template.data.y2
template.xlabel1.y=template.data.y1*.9
template.ylabel1.y=template.data.x1*.9


## Now open the sample dataset and reads in the data for temperature as a function of level
## Open the file, read the T
f=cdms.open(os.path.join(sys.prefix,'sample_data','thermo.nc'))
t=f('t')

# In this example we need to redefine the the "level" axis on "ta" because it needs to be in Pa
## WARNING: in Pa, worldcoordiante are set in hPa, not consistent!
p=t.getLevel()
p=cdms.createAxis(p[:]*100)
p.id='level'
t.setAxis(1,p) ## Reset the axis on T

# Now we are good to go and plot t
th.plot_TP(t,template=template)

