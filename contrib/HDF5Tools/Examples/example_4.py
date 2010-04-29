import HDF5Tools, vcs
path = './'

fnm = 'OMI-Aura_L2-OMAERUV_2007m0205t1530-o13622_v888-2007m0205t185330.he5'

print ' Open an HDF5 file, but this time using the OMI class, this is a particular type of HDF5/EOS files'
HDF = HDF5Tools.HDF5_OMI(path+fnm)
print 'We can now list the actual variables and their shape:'
vars = HDF.listvariables()
for v in vars:
    print 'Variable:',v,HDF.variables[v].shape,HDF.variables[v]._group
print 'And the dimensions ones that have been separated with "dimension_kw" keyword'
print 'display a var'
uva = HDF('UVAerosolIndex')
x=vcs.init()
m = x.createmeshfill('omi')
m.datawc_x1=-65
m.datawc_x2=-40
m.datawc_y1=-20
m.datawc_y2=10
sc = vcs.mkscale(-2,1)
sc.insert(0,-1.E20) # Extension on the left
sc.append(1.E20) # Extension on the right
colors = vcs.getcolors(sc)
m.levels = sc
m.fillareacolors = colors
x.plot(uva,m,ratio='autot')
raw_input('press enter')

print 'Ok now will read another var, w/o reading lat/lon'
print 'We will simply pass the grid to the read call'
salb = HDF('SurfaceAlbedo',grid=uva.getGrid())
print salb.shape
salb = salb[...,0]
print salb.shape
salb=salb(latitude=(-25,15),longitude=(-70,-30))
print salb.shape
x.clear()
sc = vcs.mkscale(0,.13)
colors = vcs.getcolors(sc)
m.levels = sc
m.fillareacolors = colors
x.plot(salb,m,ratio='autot')
raw_input('done')
