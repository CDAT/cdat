import vcs
import sys
import cdms2
import vtk

renWin = vtk.vtkRenderWindow()
i = vtk.vtkRenderWindowInteractor()
i.SetRenderWindow(renWin)
i.Initialize()
f=cdms2.open(sys.prefix+'/sample_data/clt.nc')
s=f("clt")
x=vcs.init()
x.backend.renWin = renWin
gm=x.createisofill()
gm=x.createisoline()
gm=x.createboxfill()
x.plot(s,gm)

i.Start()

