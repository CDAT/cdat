import vcs
import sys
import cdms2
import vtk
import os

renWin = vtk.vtkRenderWindow()
i = vtk.vtkRenderWindowInteractor()
i.SetRenderWindow(renWin)
i.Initialize()
f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
s=f("clt")
f=cdms2.open(os.path.join(sys.prefix,"sample_data","sampleCurveGrid4.nc"))
s=f("sample")
x=vcs.init()
x.backend.renWin = renWin
gm=x.createisofill()
gm.datawc_x1=-180
gm.datawc_x2=180
#gm=x.createisoline()
#gm=x.createboxfill()
x.plot(s,gm)

i.Start()

