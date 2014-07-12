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
s2 = f("u")
#f=cdms2.open(os.path.join(sys.prefix,"sample_data","sampleCurveGrid4.nc"))
#s=f("sample")
x=vcs.init()
x.backgroundcolor = 65*2.55,65*2.55,65*2.55
tmpl = x.createtemplate()
#tmpl.data.x1=.001
#tmpl.data.x2=.5
#tmpl.data.y1=.0001
#tmpl.data.y2=.25
if interact:
  x.backend.renWin = renWin
x.setcolormap("rainbow")
isol = x.createisoline()
isof = x.createisofill()
wc = [-180,180,-90,90]
isof.datawc_x1=wc[0]
isof.datawc_x2=wc[1]
isof.datawc_y1=wc[2]
isof.datawc_y2=wc[3]
isol.datawc_x1=wc[0]
isol.datawc_x2=wc[1]
isol.datawc_y1=wc[2]
isol.datawc_y2=wc[3]
bg = False
#bg = True
isof.levels = range(-20,135,10)
isol.levels = range(-20,135,10)
x.plot(s,tmpl,isof,bg=bg)
x.plot(s2,tmpl,isol,bg=bg)
x.png("test")#,width=2000,height=1000)
if interact:
  i.Start()
else:
  raw_input("Press enter")
