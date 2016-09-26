# Adapted for numpy/ma/cdms2 by convertcdms.py
import MV2 as MV
data = MV.array([[1.2,.9],[0.7,.6],[1.1,.8]])
print data
import vcs
x=vcs.init()
td=x.createtaylordiagram('new')
x.plot(data,td)
td.list()
# Note that here you can use either the color number
# or a "name"
td.Marker.color = ['red',244,'green']
x.clear()
x.plot(data,td)
td.Marker.id_color = ['red',244,'green']
td.Marker.id = ['Point 1','Point 2','Point 3']
td.Marker.symbol=['dot','cross','circle']
x.clear()
x.plot(data,td)
td.referencevalue = 2.
td.max = 2.5
x.clear()
x.plot(data,td)
