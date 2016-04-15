#!/usr/bin/env python
import cdms2, cdutil, genutil
import vcs,os
import sys

# This tests if extending the longitude to more than 360 decrees is handled correctly by
# proj4. See https://github.com/UV-CDAT/uvcdat/issues/1728 for more information.
pth = os.path.join(os.path.dirname(__file__), "..")
sys.path.append(pth)
import checkimage


cdmsfile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt2 = cdmsfile('clt')
clt3 = clt2(latitude=(-90.0, 90.0),squeeze=1,longitude=(-180, 200.0),time=('1979-01', '1988-12'),)
canvas = vcs.init()
gmBoxfill = vcs.getboxfill('a_robinson_boxfill')
kwargs = {}
kwargs[ 'cdmsfile' ] = cdmsfile.id
kwargs['bg'] = 1
canvas.plot(clt3, gmBoxfill, **kwargs)
fnm = "test_robinson_wrap.png"
canvas.png(fnm)
ret = checkimage.check_result_image(fnm, sys.argv[1], checkimage.defaultThreshold)
sys.exit(ret)
