import vcs, vtk

x = vcs.init()
x.open()
x.configure()

x.backend.renWin.SetSize(814, 303)
x.backend.renWin.Modified()

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
    pth = os.path.join(os.path.dirname(__file__), "..")
    sys.path.append(pth)
    import checkimage
    src = sys.argv[1]
    ret = checkimage.check_result_image(fnm, src, checkimage.defaultThreshold)
    sys.exit(ret)
