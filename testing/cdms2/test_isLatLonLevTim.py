import cdms2

val = [1,2,3]

a = cdms2.createAxis(val)

#First let's make sure it does not detect anything
assert(not a.isLatitude())
assert(not a.isLongitude())
assert(not a.isLevel())
assert(not a.isTime())

#Now quick tests for making it latitude
for u in ["DEGREESN","  deGREEn  ","degrees_north","degree_north","degree_n","degrees_n","degreen","degreesn"]:
  a.units = u
  assert(a.isLatitude())
  a.units=""
  assert(not a.isLatitude())
for i in ["lat","LAT","latitude","latituDE"]:
  a.id = i
  assert(a.isLatitude())
  a.id="axis"
  assert(not a.isLatitude())
a.axis="Y"
assert(a.isLatitude())
del(a.axis)
assert(not a.isLatitude())
#Now quick tests for making it longitude
for u in ["DEGREESe","  deGREEe  ","degrees_east","degree_east","degree_e","degrees_e","degreee","degreese"]:
  a.units = u
  assert(a.isLongitude())
  a.units=""
  assert(not a.isLongitude())
for i in ["lon","LON","longitude","lOngituDE"]:
  a.id = i
  assert(a.isLongitude())
  a.id="axis"
  assert(not a.isLongitude())
a.axis="X"
assert(a.isLongitude())
del(a.axis)
assert(not a.isLongitude())
#Now quick tests for making it level
for u in ["Pa","hPa","psi","N/m2","N*m-2","kg*m-1*s-2","atm","bar","torr"]:
  a.units = u
  assert(a.isLevel())
  a.units=""
  assert(not a.isLevel())
for i in ["lev","LEV","level","lEvEL","depth","   depth"]:
  a.id = i
  assert(a.isLevel())
  a.id="axis"
  assert(not a.isLevel())
a.axis="Z"
assert(a.isLevel())
del(a.axis)
assert(not a.isLevel())
a.positive="up"
assert(a.isLevel())

