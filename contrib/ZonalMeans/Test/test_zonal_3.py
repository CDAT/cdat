# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2,sys,ZonalMeans
cdms2.setAutoBounds("on")
cdms2.axis.level_aliases.append('depth')
import vcs
import vcs.test.support
bg= vcs.test.support.bg
x=vcs.init()
# Open data file
f=cdms2.open('test_data.nc')
O2=f("O2",level=slice(0,5))
print(O2.shape,O2.mask)
print('Level:',O2.getLevel())
f.close()

# Open grid file
f=cdms2.open('test_grid.nc')

# 1st with just bounds
blon = f('bounds_lon')
blat = f('bounds_lat')
mask = f('mask')
g=mask.getGrid()
O2.setAxis(-1,mask.getAxis(-1))
O2.setAxis(-2,mask.getAxis(-2))
O2.setGrid(g)

#sys.exit()
area = f('area')

res=ZonalMeans.compute(O2,area=area,delta_band=5)

print(res[1].shape)
x.plot(res[1][:,0],bg=bg)

vcs.test.support.check_plot(x)
