# - FindSciVtk: Module to find include directories and
#   libraries for Vtk.
#
# Module usage:
#   find_package(SciVtk ...)
#
# This module will define the following variables:
#  HAVE_VTK, VTK_FOUND = Whether libraries and includes are found
#  Vtk_INCLUDE_DIRS       = Location of Vtk includes
#  Vtk_LIBRARY_DIRS       = Location of Vtk libraries
#  Vtk_LIBRARIES          = Required libraries

##################################################################
#
# Find module for VTK
#
# $Id: FindSciVtk500i.cmake 1239 2012-01-30 19:21:06Z mdurant $
#
##################################################################

set(Vtk_LIBRARY_LIST
        vtkCommon
        vtkCommonPythonD
        vtkDICOMParser
        vtkFiltering
        vtkFilteringPythonD
        vtkGenericFiltering
        vtkGenericFilteringPythonD
        vtkGraphics
        vtkGraphicsPythonD
        vtkHybrid
        vtkHybridPythonD
        vtkIO
        vtkIOPythonD
        vtkImaging
        vtkImagingPythonD
        vtkPythonCore
        vtkRendering
        vtkRenderingPythonD
        vtkVolumeRendering
        vtkVolumeRenderingPythonD
        vtkexpat
        vtkfreetype
        vtkftgl
        vtkjpeg
        vtkpng
        vtksqlite
        vtksys
        vtktiff
        vtkverdict
        vtkzlib
)

SciFindPackage(
  PACKAGE Vtk
  INSTALL_DIR vtk
  HEADERS vtkObject.h
  LIBRARIES "${Vtk_LIBRARY_LIST}"
  INCLUDE_SUBDIRS include/vtk-5.0 include/vtk/include/vtk-5.0 # Last for Visit
)

if (VTK_FOUND)
  message(STATUS "[FindSciVtk500i.cmake] - Found VTK")
  message(STATUS "[FindSciVtk500i.cmake] - Vtk_INCLUDE_DIRS = ${Vtk_INCLUDE_DIRS}")
  message(STATUS "[FindSciVtk500i.cmake] - Vtk_LIBRARIES = ${Vtk_LIBRARIES}")
  set(HAVE_VTK 1 CACHE BOOL "Whether have Vtk.")
else ()
  message(STATUS "[FindSciVtk500i.cmake] - Did not find VTK 5.0.0, use -DVTK_DIR to supply the VTK installation directory.")
  if (SciVtk_FIND_REQUIRED)
    message(FATAL_ERROR "[FindSciVtk500i.cmake] - Failing.")
  endif ()
endif ()

