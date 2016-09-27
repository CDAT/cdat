import vcs, vtk

x = vcs.init()
x.open()
x.configure()

fnm = "test_vcs_configurator_resize.png"

win = x.backend.renWin
win.SetSize(814, 303)

out_filter = vtk.vtkWindowToImageFilter()
out_filter.SetInput(win)

win.Render()

png_writer = vtk.vtkPNGWriter()
png_writer.SetFileName(fnm)
png_writer.SetInputConnection(out_filter.GetOutputPort())
png_writer.Write()

import sys, os
if len(sys.argv) > 1:
    import vcs.testing.regression as regression
    src = sys.argv[1]
    ret = regression.check_result_image(fnm, src)
    sys.exit(ret)