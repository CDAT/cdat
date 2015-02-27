"""
Test button placement
"""
import os, sys
import vtk, vcs.vtk_ui
src=sys.argv[1]
pth = os.path.join(os.path.dirname(__file__),"../..")
sys.path.append(pth)
import checkimage

win = vtk.vtkRenderWindow()
win.SetSize(100, 250)
inter = vtk.vtkRenderWindowInteractor()
inter.SetRenderWindow(win)
manager = vcs.vtk_ui.manager.get_manager(inter)
win.SetOffScreenRendering(1)
win.AddRenderer(manager.renderer)

for i in range(5):
    button = vcs.vtk_ui.Button(inter, corner_radius=5, font="Arial", left=10 * i, top=30 * i, label="Test %d" % i, bgcolor=(.1, .1, .1), fgcolor=(1, 1, 1), size=14, halign=vcs.vtk_ui.button.LEFT_ALIGN, valign=vcs.vtk_ui.button.CENTER_ALIGN)
    button.place()
    button.show()

win.Render()

fnm = "test_vtk_ui_button_draw.png"

out_filter = vtk.vtkWindowToImageFilter()
out_filter.SetInput(win)
out_filter.Update()

png_writer = vtk.vtkPNGWriter()
png_writer.SetFileName(fnm)
png_writer.SetInputConnection(out_filter.GetOutputPort())
png_writer.Write()

print "fnm:",fnm
print "src:",src
ret = checkimage.check_result_image(fnm,src,checkimage.defaultThreshold)
sys.exit(ret)
