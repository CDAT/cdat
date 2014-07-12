import vcs
import numpy
import cdtime

from vcs_test_common import *

x=vcs.init()

p=x.createprojection()
assert(p.type == "linear")
assert(vcs.queries.isprojection(p))

test_values_setting(p, "type", [-1,-2,-3,'linear', 'albers equal area', 'lambert', 'mercator', 'polar', 'polyconic', 'equid conic a', 'transverse mercator', 'stereographic', 'lambert azimuthal', 'azimuthal', 'gnomonic', 'orthographic', 'gen. vert. near per', 'sinusoidal', 'equirectangular', 'miller', 'van der grinten', 'hotin', 'robinson', 'space oblique', 'alaska', 'interrupted goode', 'mollweide', 'interrupted mollweide', 'hammer', 'wagner iv', 'wagner vii', 'oblated', 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,p,"POLAR","  POlyConic  "],["utm","state plane","foo",-4,31,256,[],{},])
b = x.createprojection("test_b_ok",p.name)
assert(b.name == "test_b_ok")
print b.type
assert(b.type == "polyconic")
## From vcs validation
for t in range(31):
  good = []
  bad =[]
  pos = []
  for param,val in vcs.VCS_validation_functions.proj_ok_parameters.iteritems():
    if t in val[0]:
      good.append(param)
      pos.append(val[1])
    else:
      bad.append(param)
  b.type=t
  b._parameters = [1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20, 1e+20]
  for i,att in enumerate(good):
    if (att in ['azimuthalangle','azimuthallongitude','satellite','path',] and (b.parameters[12]==0. or b.parameters[12]==1.e20)) \
        or \
        ( att=='standardparallel' and b.parameters[8]==1) \
        or \
        ( att in ['standardparallel1','standardparallel2'] and (b.parameters[8]==0 or b.parameters[8]==1.e20) and t==8)\
        :
      continue
    test_values_setting(b,att,[0.,])
    if b.type == "equid conic" and att=="subtype":
      ipos = 8
    else:
      ipos = pos[i]
    assert(b.parameters[ipos]==0.)
  for att in bad:
    try:
      setattr(b,att,[],[0.,])
      success = True
    except:
      success = False
    else:
      if success:
        raise ValueError, "Shouldn't have been able to set '%s' on projection of type %s" % (att,b.type)


