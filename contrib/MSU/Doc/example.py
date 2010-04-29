import MSU,cdms

weights_file='../Data/weights.nc'

f=cdms.open(weights_file)
w=f('weights') # There's 3 channel on these weights => MSU computed for each one at once
f.close()

critw=50. # Criteria to set to missing if too many missing values
file='../Data/ta.nc'
fout='equivalent_msu.nc'

f=cdms.open(file)
ta=f('ta')
tc=ta.getTime().asComponentTime()
print 'Input data:',ta.shape
print 'Time span:',tc[0],tc[-1]
print 'Computing Equivalent MSU temperatures'
msu=MSU.msu(ta,w,critw)
print 'MSU computed, it has 3 channels as did the weights input'
msu.info()

fout=cdms.open(fout,'w')
fout.write(msu[...,0],typecode='f',id='tam2')
fout.write(msu[...,1],typecode='f',id='tam4')
fout.write(msu[...,2],typecode='f',id='tam6')
fout.close()

print 'Finished'
