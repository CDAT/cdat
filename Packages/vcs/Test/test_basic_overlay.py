import vcs
import cdms2
import vtk
import os

interact = False
if interact:
    renWin = vtk.vtkRenderWindow()
    i = vtk.vtkRenderWindowInteractor()
    i.SetRenderWindow(renWin)
    i.Initialize()
f = cdms2.open(os.path.join(vcs.sample_data, 'clt.nc'))
s = f("clt")
s2 = f("u")
x = vcs.init()
x.backgroundcolor = 65 * 2.55, 65 * 2.55, 65 * 2.55
tmpl = x.createtemplate()
if interact:
    x.backend.renWin = renWin
x.setcolormap("rainbow")
isol = x.createisoline()
isof = x.createisofill()
wc = [-180, 180, -90, 90]
isof.datawc_x1 = wc[0]
isof.datawc_x2 = wc[1]
isof.datawc_y1 = wc[2]
isof.datawc_y2 = wc[3]
isol.datawc_x1 = wc[0]
isol.datawc_x2 = wc[1]
isol.datawc_y1 = wc[2]
isol.datawc_y2 = wc[3]
bg = False
isof.levels = range(-20, 135, 10)
isol.levels = range(-20, 135, 10)
x.plot(s, tmpl, isof, bg=bg)
x.plot(s2, tmpl, isol, bg=bg)
x.png("test")
if interact:
    i.Start()
else:
    raw_input("Press enter")
