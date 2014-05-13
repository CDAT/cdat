import vcs
import vtk

interact = False
if interact:
  renWin = vtk.vtkRenderWindow()
  p = vtk.vtkPointPicker()
  i = vtk.vtkRenderWindowInteractor()
  i.SetRenderWindow(renWin)
  i.SetPicker(p)
  i.Initialize()
else:
  renWin = "vtk"

x=vcs.init(backend=renWin)

l = x.createline()
l.viewport = [0.,1.,0.,1.]
l.x=[0.0,1.0]
l.y=[0.0,1.0]
l.color = 242
l.width = 5
#l.type='dot'

x.plot(l)
x.png("test_lines")
if interact:
  i.Start()
else:
  raw_input("Press enter")


