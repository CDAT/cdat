#!/usr/bin/env python

# This example demonstrates the use of vtkVectorText and vtkFollower.
# vtkVectorText is used to create 3D annotation.  vtkFollower is used to
# position the 3D text and to ensure that the text always faces the
# renderer's active camera (i.e., the text is always readable).

import vtk

def computeBounds( renderer, normalized_display_position, size ):
    upperRight = vtk.vtkCoordinate()
    upperRight.SetCoordinateSystemToNormalizedDisplay()
    upperRight.SetValue( normalized_display_position[0], normalized_display_position[1] )
    bds = [0.0]*6
    bds[0] = upperRight.GetComputedDisplayValue(renderer)[0] - size[0]
    bds[1] = bds[0] + size[0]
    bds[2] = upperRight.GetComputedDisplayValue(renderer)[1] - size[1]
    bds[3] = bds[2] + size[1]
    return bds

# Create the axes and the associated mapper and actor.
axes = vtk.vtkAxes()
axes.SetOrigin(0, 0, 0)
axesMapper = vtk.vtkPolyDataMapper()
axesMapper.SetInputConnection(axes.GetOutputPort())
axesActor = vtk.vtkActor()
axesActor.SetMapper(axesMapper)

# Create the 3D text and the associated mapper and follower (a type of
# actor).  Position the text so it is displayed over the origin of the
# axes.
atext = vtk.vtkVectorText()
atext.SetText("Origin")
textMapper = vtk.vtkPolyDataMapper()
textMapper.SetInputConnection(atext.GetOutputPort())
textActor = vtk.vtkFollower()
textActor.SetMapper(textMapper)
textActor.SetScale(0.1, 0.1, 0.1)

buttonRepresentation = vtk.vtkProp3DButtonRepresentation() 
#buttonRepresentation.FollowCameraOn()
buttonRepresentation.SetNumberOfStates(1)
buttonRepresentation.SetButtonProp( 0, textActor ) 


# Create the Renderer, RenderWindow, and RenderWindowInteractor.
ren = vtk.vtkRenderer()
renWin = vtk.vtkRenderWindow()
renWin.AddRenderer(ren)
iren = vtk.vtkRenderWindowInteractor()
iren.SetRenderWindow(renWin)
iren.SetInteractorStyle( vtk.vtkInteractorStyleTrackballCamera() )
ren.SetBackground(0.1, 0.2, 0.4)

buttonWidget = vtk.vtkButtonWidget()
buttonWidget.SetInteractor( iren )
position = [ 0.5, 0.5 ] 
size = [ 1.0, 1.0 ] 
# ComputeDisplayToWorld(double x, double y, double z, double worldPt[4])
bounds = computeBounds(ren, position,size)
print " Bounds = ", bounds 
buttonRepresentation.PlaceWidget( bounds )
buttonWidget.SetRepresentation(buttonRepresentation)

# Add the actors to the renderer.
ren.AddActor(axesActor)
# ren.AddActor(textActor)

# Zoom in closer.
ren.ResetCamera()
#ren.GetActiveCamera().Zoom(1.6)

# Reset the clipping range of the camera; set the camera of the
# follower; render.
ren.ResetCameraClippingRange()
textActor.SetCamera(ren.GetActiveCamera())

iren.Initialize()
buttonWidget.EnabledOn()
buttonWidget.ProcessEventsOn()
buttonWidget.On()
buttonWidget.Render()
buttonWidget.SetKeyPressActivationValue('e')
renWin.Render()
iren.Start()