import sys, cdms2, vcs, vcs.testing.regression as regression

data = cdms2.open(vcs.sample_data+"/clt.nc")
v = data['v'][...,::10,::10]
u = data['u'][...,::10,::10]

canvas = regression.init()
gv = vcs.createvector()

gv.scaletype = 'off'
canvas.plot(u, v, gv)
outFilename = 'test_vcs_vectors_scale_options_off.png'
canvas.png(outFilename)
ret = regression.check_result_image(outFilename, sys.argv[1])
canvas.clear()

v = data['v'][...,::4,::4]
u = data['u'][...,::4,::4]
gv.scaletype = 'constant'
gv.scale = 0.1
canvas.plot(u, v, gv)
outFilename = 'test_vcs_vectors_scale_options_constant.png'
canvas.png(outFilename)
ret += regression.check_result_image(outFilename, sys.argv[2])
canvas.clear()

v = data['v']
u = data['u']
gv.scale = 1.0

gv.scaletype = 'linear'
canvas.plot(u, v, gv)
outFilename = 'test_vcs_vectors_scale_options_linear.png'
canvas.png(outFilename)
ret += regression.check_result_image(outFilename, sys.argv[3])
canvas.clear()

gv.scaletype = 'normalize'
canvas.plot(u, v, gv)
outFilename = 'test_vcs_vectors_scale_options_normalize.png'
canvas.png(outFilename)
ret += regression.check_result_image(outFilename, sys.argv[4])
canvas.clear()

gv.scaletype = 'constantNLinear'
canvas.plot(u, v, gv)
outFilename = 'test_vcs_vectors_scale_options_constantNLinear.png'
canvas.png(outFilename)
ret += regression.check_result_image(outFilename, sys.argv[5])
canvas.clear()

gv.scaletype = 'constantNNormalize'
canvas.plot(u, v, gv)
outFilename = 'test_vcs_vectors_scale_options_constantNNormalize.png'
canvas.png(outFilename)
ret += regression.check_result_image(outFilename, sys.argv[6])
canvas.clear()

sys.exit(ret)
