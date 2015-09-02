import cdms2
import os
import cdat_info


incat = cdms2.open(os.path.join(cdat_info.get_sampledata_path(),"tas_ccsr-95a.xml"))
invar  = incat['tas'] 
intype = invar.dtype  # dtype(float32)

outfile = cdms2.open('newfile.nc', 'w')
outfile.createVariableCopy(invar)

outvar = outfile['tas']
outtype= outvar.dtype # dtype(float64)

outfile.close()

os.remove("newfile.nc")

assert(outtype==intype)
