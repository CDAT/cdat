import cdutil,MV2
import cdms2,numpy

cdms2.setAutoBounds("on")


a = MV2.masked_greater(MV2.array([1,4,5,6,7,8,9.]),.5)

assert numpy.ma.is_masked(cdutil.averager(a))
