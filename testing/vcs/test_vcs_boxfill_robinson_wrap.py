import os, sys, cdms2, cdutil, genutil, vcs, vcs.testing.regression as regression

# This tests if extending the longitude to more than 360 decrees is handled correctly by
# proj4. See https://github.com/UV-CDAT/uvcdat/issues/1728 for more information.
cdmsfile = cdms2.open(os.path.join(vcs.sample_data, "clt.nc"))
clt2 = cdmsfile('clt')
clt3 = clt2(latitude=(-90.0, 90.0),squeeze=1,longitude=(-180, 200.0),time=('1979-01', '1988-12'),)
canvas = regression.init()
gmBoxfill = vcs.getboxfill('a_robinson_boxfill')
kwargs = {}
kwargs[ 'cdmsfile' ] = cdmsfile.id
kwargs['bg'] = 1
canvas.plot(clt3, gmBoxfill, **kwargs)
regression.run(canvas, "test_vcs_boxfill_robinson_wrap.png")
