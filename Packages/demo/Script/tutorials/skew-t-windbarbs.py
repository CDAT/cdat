# Adapted for numpy/ma/cdms2 by convertcdms.py
# First import necessary modules
import os,sys,thermo,vcs,cdms2 as cdms

# initialize the VCS Canvas and creates the "Thermodynamic Diagram" graphic method
x=vcs.init()
x.portrait()
th=thermo.Gth(x=x,name='test')

## Setting type of thermodynamic diagram, you can choose from: 'emagram', 'tephigram', 'stuve' or 'skewT'
th.type='skewT'

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

# Now we are going to create a second template on the right side of the first one, where we will plot the windbarbs
## Create a template for the windbarbs
template=x.createtemplate('new2')
template.data.x1=.86
template.data.x2=.96
template.data.y1=.1
template.data.y2=.9
template.box1.x1=template.data.x1
template.box1.x2=template.data.x2
template.box1.y1=template.data.y1
template.box1.y2=template.data.y2
template.xlabel1.y=template.data.y1*.9
template.ylabel1.y=template.data.x1*.9

## Read winds from a file
f=cdms.open(os.path.join(sys.prefix,'sample_data','thermo.nc'))
u=f('ua',time=slice(0,1),longitude=0,latitude=0,squeeze=1)
v=f('va',time=slice(0,1),longitude=0,latitude=0,squeeze=1)

## The following sets the scale for the windbarbs (in u/v units)
## Windbarbs scale (triangle, long bar, small bar)
th.windbarbsscales=[5,2,1]

## Plot the windbarbs, passing the P values (in Pa)
## Note that we are passing the P level this time (still in Pa)
th.plot_windbarb(u,v,P=u.getLevel()[:]*100.,template=template)
