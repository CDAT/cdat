import cdms2,cdat_info
import MV2
import sys
f = cdms2.open(cdat_info.get_prefix() + "/sample_data/clt.nc")
s = f('clt')
S2 = MV2.masked_greater(s, 87)
G = cdms2.createGaussianGrid(22)
S3 = S2.regrid(G)
assert(S3.max() < 87.1)
