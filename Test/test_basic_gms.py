import vcs
import sys
import cdms2
import vtk
import os

interact = False
if interact:
  renWin = vtk.vtkRenderWindow()
  i = vtk.vtkRenderWindowInteractor()
  i.SetRenderWindow(renWin)
  i.Initialize()
f=cdms2.open(os.path.join(sys.prefix,'sample_data','clt.nc'))
s=f("clt")
#f=cdms2.open(os.path.join(sys.prefix,"sample_data","sampleCurveGrid4.nc"))
#s=f("sample")
x=vcs.init()
if interact:
  x.backend.renWin = renWin
x.setcolormap("rainbow")
gm=x.createisofill()
gm.datawc_x1=-130
gm.datawc_x2=140
gm.datawc_y1=-85
gm.datawc_y2=85
#gm=x.createisoline()
#gm=x.createboxfill()
bg = False
#bg = True
gm.levels = range(-20,135,10)
x.plot(s,gm,bg=bg)
#x.cgm("cgm_test")
x.png("test")#,width=2000,height=1000)
if interact:
  i.Start()
else:
  raw_input("Press enter")
