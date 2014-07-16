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
x=vcs.init()
txt=x.createtext()
if interact:
  x.backend.renWin = renWin
txt.x = [.0000005,.00000005,.99999,.999999]
txt.y=[0.05,.9,.9,0.05]
txt.string = ["SAMPLE TEXT A","SAMPLE TEXT B","SAMPLE TEXT C","SAMPLE TEXT D"]
txt.halign = "center"
txt.valign="base"
txt.angle=45
x.plot(txt)
if interact:
  i.Start()
else:
  raw_input("Press enter")
