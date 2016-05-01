import vcs, vtk

x = vcs.init()
x.open()
x.configure()

x.backend.renWin.SetSize(814, 303)

fnm = "test_vcs_configurator_resize.png"

win = x.backend.renWin
win.Render()
out_filter = vtk.vtkWindowToImageFilter()
out_filter.SetInput(win)

png_writer = vtk.vtkPNGWriter()
png_writer.SetFileName(fnm)
png_writer.SetInputConnection(out_filter.GetOutputPort())
png_writer.Write()

import sys, os
if len(sys.argv) > 1:
    import testing.regression as regression
    src = sys.argv[1]
    ret = regression.check_result_image(fnm, src)
    sys.exit(ret)