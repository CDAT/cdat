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
x.open()
tmpl = x.createtemplate()
#tmpl.data.x1=.001
#tmpl.data.x2=.5
#tmpl.data.y1=.0001
#tmpl.data.y2=.25
if interact:
  x.backend.renWin = renWin
x.setcolormap("rainbow")
gm=x.createisoline()
gm=x.createboxfill()
#gm=x.createisofill()
gm.datawc_x1=0
gm.datawc_x2=40
gm.datawc_y1=-40
gm.datawc_y2=40
bg = False
#bg = True
gm.levels = range(-20,135,10)
x.plot(s,tmpl,gm,bg=bg)
x.png("test")#,width=2000,height=1000)
if interact:
  i.Start()
else:
  raw_input("Press enter")
