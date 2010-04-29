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
dims = HDF.listdimension()
for d in dims:
    print 'Dimension Var',d,HDF.variables[d].shape,HDF.variables[d]._group

print 'display a var'
V = HDF('UVAerosolIndex')

x=vcs.init()
x.plot(V)
raw_input('Press enter')
print 'now show some controling with meshfill'
m=x.createmeshfill('omi')
m.datawc_x1=0
m.datawc_x2=360
x.clear()
x.plot(V,m)
raw_input('Press enter')
m.datawc_x1=-70
m.datawc_x2=-35
x.clear()
x.plot(V,m)
raw_input('Press enter')
x.clear()
x.plot(V,m,ratio='autot')
raw_input('Press enter')
m.mesh='y'
m.missing=240 #White color for missing values
m.datawc_y1=5
m.datawc_y2=10
m.datawc_x1=-62
m.datawc_x2=-52
x.clear()
x.plot(V,m,ratio='autot')
print 'Note the overlap of cells, resolution could be imroved!'
raw_input('Press enter')
print 'And finaly for fast plotting/output'
m.mesh='n'
x.clear()
x.plot(V,m,ratio='autot',bg=1)
x.postscript('example_3')
x.showbg() # to redisplay the data if you want
raw_input('Press enter to end tutorial')
