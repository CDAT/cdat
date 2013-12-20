import cdms2
from cdms2.coord import TransientAxis2D, TransientVirtualAxis
from cdms2.hgrid import TransientCurveGrid
from cdms2.gengrid import TransientGenericGrid
import MV2
# =============================================================
def CurveGrid(v,lat,lon) :

 """ 
 """
# =============================================================
 ni, nj = lat.shape
 idi = "i"
 idj = "j"
 lat_units = 'degrees_north'
 lon_units = 'degrees_east'
 iaxis = TransientVirtualAxis(idi,ni)
 jaxis = TransientVirtualAxis(idj,nj)
 lataxis = TransientAxis2D(lat, axes=(iaxis, jaxis), 
                        attributes={'units':lat_units}, id="latitude")
 lonaxis = TransientAxis2D(lon, axes=(iaxis, jaxis), 
                        attributes={'units':lon_units}, id="longitude")
 curvegrid = TransientGenericGrid(lataxis, lonaxis, tempmask=None)
 attributs = None ; vid = None
 if hasattr(v,'attributes') : attributs= v.attributes
 if hasattr(v,'id') : vid= v.id
 axis0 = v.getAxis(0)
 return cdms2.createVariable(v, axes=[axis0,iaxis,jaxis], grid=curvegrid, \
                            attributes=attributs, id=v.id)


lat = MV2.array([[-20,-10,0,-15,-5]],'f')
lon = MV2.array([[0,10,20,50,60]],'f')

data1 = MV2.array([[[2,3,1,6,2]]],'f')
data2 = MV2.array([[[2,3,1,6,2]]],'f')


data1 = CurveGrid(data1, lat, lon)
data2 = CurveGrid(data2, lat, lon)

result = MV2.concatenate([data1,data2],axis=0)
