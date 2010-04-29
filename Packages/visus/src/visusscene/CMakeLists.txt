########################################################################
#
# Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
# Produced at the Lawrence Livermore National Laboratory  
# Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
# LLNL-CODE-406031.  
# All rights reserved.  
#   
# This file is part of "Simple and Flexible Scene Graph Version 2.0."
# Please also read BSD_ADDITIONAL.txt.
#   
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#   
# @ Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the disclaimer below.
# @ Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the disclaimer (as noted below) in
#   the documentation and/or other materials provided with the
#   distribution.
# @ Neither the name of the LLNS/LLNL nor the names of its contributors
#   may be used to endorse or promote products derived from this software
#   without specific prior written permission.
#   
#  
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
# LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING
#
########################################################################



SET(VISUSSCENE_SRC

 VisusAssert.cpp
 
 VisusSharedValue.cpp

 VisusTime.cpp
 VisusDoubleTime.cpp
 VisusIntTime.cpp
 VisusGlobalTime.cpp
 VisusSharedGlobalTime.cpp

 VisusSharedIsoValue.cpp
 VisusBoundingBox.cpp
 VisusSharedBoundingBox.cpp
 VisusFont.cpp
 VisusSharedFont.cpp
 VisusTransformation2D.cpp
 VisusSharedTransformation2D.cpp
 VisusTransformation3D.cpp
 VisusSharedTransformation3D.cpp
 VisusDataRequest.cpp
 VisusSharedDataRequest.cpp
 VisusDataDescription.cpp
 VisusSharedDataDescription.cpp
 VisusSharedFieldIndex.cpp
 VisusBorderAxis.cpp
 VisusBorderBox2D.cpp
 VisusColorBar.cpp
 VisusColorMap.cpp
 VisusSharedColorMap.cpp
 VisusColor.cpp
 VisusSharedColor.cpp
 VisusOpenGLState.cpp
 VisusSharedOpenGLState.cpp
 VisusTickMarks.cpp
 VisusGraph.cpp
 VisusHistogram.cpp
 VisusLineGraph.cpp
 VisusSharedEarthRadius.cpp
 VisusCamera.cpp
 VisusSharedCamera.cpp
 VisusScreenShot.cpp
 VisusQuaternion.cpp

 VisusGroup.cpp
 VisusProducer.cpp
 VisusConsumer.cpp
 VisusSampleExtractor.cpp
 VisusAxisAlignedExtractor.cpp
 VisusOrthogonalSlice.cpp
 VisusTextureViewer.cpp
 VisusIsoSurface.cpp
 VisusColoredIsoSurface.cpp
 VisusMeshDisplay.cpp
 VisusIndexedDisplay.cpp
 VisusHeightField.cpp
 VisusParameter.cpp
 VisusParameterList.cpp
 VisusLabelNode.cpp
 VisusSphereSlice.cpp
 VisusDataTransform.cpp
 VisusProjector.cpp

 VisusProjection.cpp
 VisusSphereProjection.cpp

 VisusText.cpp
 VisusTextNode.cpp
 
 VisusSphere.cpp
 VisusSphereNode.cpp
 VisusEarthNode.cpp
 VisusHalo.cpp

 VisusShapes.cpp
 VisusShapesNode.cpp

 VisusSceneNode.cpp
 VisusDataProbe.cpp
 
 VisusSmartPointer.cpp
 VisusAtomicCounter.cpp
 VisusManagedObject.cpp
 VisusFactory.cpp

 VisusDataSource.cpp
 VisusDataSourceFactory.cpp
 VisusFastLoopDataSource.cpp
 VisusIncoreEncoder.cpp
 VisusIncoreDataSource.cpp
 VisusManagedIncoreSource.cpp
 VisusTimeLoader.cpp

 VisusData.cpp
 VisusMetricData.cpp
 VisusBlockData.cpp
 VisusIndexedData.cpp
 VisusMeshData.cpp
 VisusMutex.cpp
 VisusReadWriteLock.cpp
 VisusThread.cpp
 VisusTexture.cpp
 VisusDefaultColorMaps.cpp

 VisusXMLInterface.cpp

 PvTimeState.cpp
 VisusTrackball.cpp

 VisusRenderer.cpp

 VisusGuiBase.cpp
 VisusGuiInterface.cpp
)

IF (VISUS_OFFSCREEN_RENDERING STREQUAL "GLX")
  # ------------ GLX ---------------
  SET (VISUS_RENDER_SRC VisusRendererGLX.cpp)
ELSE (VISUS_OFFSCREEN_RENDERING STREQUAL "GLX")
  IF (VISUS_OFFSCREEN_RENDERING STREQUAL "GLX_PBUFFER")
    # ---------- GLX PBuffer  ---------------
    SET (VISUS_RENDER_SRC VisusRendererGLXPBuffer.cpp)    
  ELSE (VISUS_OFFSCREEN_RENDERING STREQUAL "GLX_PBUFFER")
    IF (VISUS_OFFSCREEN_RENDERING STREQUAL "WGL")
      # -------- WGL  -----------------------
      SET (VISUS_RENDER_SRC VisusRendererWGL.cpp)    
    ELSE (VISUS_OFFSCREEN_RENDERING STREQUAL "WGL")
      IF (VISUS_OFFSCREEN_RENDERING STREQUAL "OSMESA")
        # -------- OSMESA  -----------------------
        SET (VISUS_RENDER_SRC VisusRendererOSMESA.cpp)
        SET (VISUS_RENDERER_INCLUDE_DIR ${OSMESA_INCLUDE_DIR})    
      ELSE (VISUS_OFFSCREEN_RENDERING STREQUAL "OSMESA")
        # -------- NONE  ----------------------
        SET (VISUS_RENDER_SRC VisusRendererNONE.cpp)  
      ENDIF (VISUS_OFFSCREEN_RENDERING STREQUAL "OSMESA")

    ENDIF (VISUS_OFFSCREEN_RENDERING STREQUAL "WGL")

  ENDIF (VISUS_OFFSCREEN_RENDERING STREQUAL "GLX_PBUFFER")
  
ENDIF (VISUS_OFFSCREEN_RENDERING STREQUAL "GLX")

SET (VISUSSCENE_SRC
  ${VISUSSCENE_SRC}
  ${VISUS_RENDER_SRC}
)   

SET(VISUSSCENE_HEADERS

 VisusDefinitions.h
 VisusAssert.h
 VisusTypeInfo.h
 VisusGenericValue.h
 VisusSharedValue.h

 VisusDoubleTime.h
 VisusIntTime.h
 VisusTime.h
 VisusGenericTime.h
 VisusGlobalTime.h
 VisusSharedGlobalTime.h

 VisusIsoValue.h
 VisusSharedIsoValue.h
 VisusBoundingBox.h
 VisusSharedBoundingBox.h
 VisusFont.h
 VisusSharedFont.h
 VisusTransformation.h
 VisusTransformation2D.h
 VisusSharedTransformation2D.h
 VisusTransformation3D.h
 VisusSharedTransformation3D.h
 VisusDataRequest.h
 VisusSharedDataRequest.h
 VisusDataDescription.h
 VisusSharedDataDescription.h
 VisusFieldIndex.h
 VisusSharedFieldIndex.h
 VisusBorderAxis.h
 VisusBorderBox2D.h
 VisusColorBar.h
 VisusColorMap.h
 VisusSharedColorMap.h
 VisusColor.h
 VisusSharedColor.h
 VisusOpenGLState.h
 VisusSharedOpenGLState.h
 VisusTickMarks.h
 VisusGraph.h
 VisusHistogram.h
 VisusLineGraph.h
 VisusEarthRadius.h
 VisusSharedEarthRadius.h
 VisusCamera.h
 VisusSharedCamera.h
 VisusScreenShot.h
 VisusQuaternion.h

 VisusGroup.h
 VisusProducer.h
 VisusConsumer.h
 VisusSampleExtractor.h
 VisusAxisAlignedExtractor.h
 VisusOrthogonalSlice.h
 VisusTextureViewer.h
 VisusIsoSurface.h
 VisusColoredIsoSurface.h
 VisusMeshDisplay.h
 VisusIndexedDisplay.h
 VisusParameter.h
 VisusHeightField.h
 VisusParameterList.h
 VisusSphereSlice.h
 VisusDataTransform.h
 VisusProjector.h

 VisusProjection.h
 VisusSphereProjection.h

 VisusText.h
 VisusTextNode.h

 VisusEarthNode.h
 VisusHalo.h

 VisusSceneNode.h
 VisusDataProbe.h
 
 VisusAtomicCounter.h
 VisusSmartPointer.h
 VisusManagedObject.h
 VisusFactory.h

 VisusDataSource.h
 VisusDataSourceFactory.h
 VisusNullDataSource.h
 VisusFastLoopDataSource.h
 VisusIncoreDataSource.h
 VisusManagedIncoreSource.h
 VisusTimeLoader.h

 VisusData.h
 VisusMetricData.h
 VisusBlockData.h
 VisusIndexedData.h
 VisusMeshData.h
 VisusMutex.h
 VisusReadWriteLock.h
 VisusThread.h
 VisusDataRequest.h
 VisusTexture.h
 VisusDefaultColorMaps.h

 VisusXMLInterface.h

 PvTimeState.h
 VisusTrackball.h

 VisusRenderer.h

 VisusGuiBase.h
 VisusFLTKWindow.h
 VisusFLTKGui.h
 VisusGuiInterface.h
 VisusFLTKMaster.h
 VisusGuiThread.h
)

IF (VISUS_ENABLE_FLTK)

   SET (VISUSSCENE_HEADERS
       ${VISUSSCENE_HEADERS}
       VisusFLTKWindow.h
       VisusFLTKGui.h
       vfltk/FLTKNodeMenu.h
       vfltk/LabelNodeMenu.h
       vfltk/OrthoSliceMenu.h
       vfltk/SharedValueMenu.h
#       vfltk/FontMenu.h
       vfltk/FontMenuWidget.h
#       vfltk/ColorMapMenu.h
       vfltk/ColorMapMenuWidget.h
       vfltk/TickMarksMenu.h
       vfltk/BorderAxisMenuWidget.h
#       vfltk/MenuWrapper.h
   )

   SET (VISUSSCENE_SRC
       ${VISUSSCENE_SRC}
       VisusFLTKWindow.cpp
       VisusFLTKGui.cpp
       VisusFLTKMaster.cpp
       vfltk/LabelNodeMenu.cpp
       vfltk/OrthoSliceMenu.cpp
#       vfltk/SharedValueMenu.cpp
#       vfltk/FontMenu.cpp
       vfltk/FontMenuWidget.cpp
#       vfltk/ColorMapMenu.cpp
       vfltk/ColorMapMenuWidget.cpp
       vfltk/TickMarksMenu.cpp
       vfltk/BorderAxisMenuWidget.cpp
#       vfltk/MenuWrapper.cpp
   )

ENDIF (VISUS_ENABLE_FLTK)

SET(CMP0003 NEW)

IF (WIN32)
#SET(VISUSSCENE_SRC ${VISUSSCENE_SRC} ${VISUSSCENE_HEADERS} )
ENDIF (WIN32)

LoadVisusVariables()
INCLUDE_DIRECTORIES(${VISUS_INCLUDE_DIRS})

IF (VISUS_ENABLE_STATIC)

  SET (VISUS_TARGETS
      visusscene-static
  )

  ADD_LIBRARY(visusscene-static STATIC ${VISUSSCENE_SRC})

  SET_TARGET_PROPERTIES(visusscene-static
           PROPERTIES
           OUTPUT_NAME visusscene
           CLEAN_DIRECT_OUTPUT 1
           LINK_FLAGS ${VISUS_EXTRA_LINK_FLAGS}
  )
ELSE (VISUS_ENABLE_STATIC)
     
  SET (VISUS_TARGETS
  )
ENDIF (VISUS_ENABLE_STATIC)


IF (VISUS_ENABLE_DYNAMIC)
   SET(VISUS_TARGETS
       ${VISUS_TARGETS}
       visusscene-shared 
   )
   ADD_LIBRARY(visusscene-shared SHARED ${VISUSSCENE_SRC})

   TARGET_LINK_LIBRARIES(visusscene-shared ${VISUS_SHARED_LIBS})

   SET_TARGET_PROPERTIES(visusscene-shared
         PROPERTIES
         OUTPUT_NAME visusscene${SO_EXT}
         CLEAN_DIRECT_OUTPUT 1
         LINK_FLAGS ${VISUS_EXTRA_LINK_FLAGS}
   )
ENDIF (VISUS_ENABLE_DYNAMIC)

# ================================================================
# Test Case Macro
# ================================================================
MACRO(VISUS_TEST_CASE TEST_NAME)
  IF (VISUS_ENABLE_TESTS)

    SET(VISUS_TARGETS
       ${VISUS_TARGETS}
       ${TEST_NAME}
    )
    ADD_EXECUTABLE(${TEST_NAME} ${TEST_NAME}.cpp)
    TARGET_LINK_LIBRARIES(${TEST_NAME} ${VISUS_STATIC_LIBS})  
  ENDIF (VISUS_ENABLE_TESTS)
ENDMACRO(VISUS_TEST_CASE TEST_NAME)

# ================================================================
# Test Cases
# ================================================================
VISUS_TEST_CASE(visus_test_sphere)
VISUS_TEST_CASE(visus_test_ssa)
VISUS_TEST_CASE(visus_test_color_bar)
VISUS_TEST_CASE(visus_test_tick_marks)
VISUS_TEST_CASE(visus_test_isosurface)
VISUS_TEST_CASE(visus_test_camera)
VISUS_TEST_CASE(visus_test_texture_viewer)
VISUS_TEST_CASE(visus_test_slice_viewer)
VISUS_TEST_CASE(visus_test_earth_node)
VISUS_TEST_CASE(visus_test_sphere_slice)
VISUS_TEST_CASE(visus_test_renderer)
VISUS_TEST_CASE(visus_test_line_graph)
VISUS_TEST_CASE(visus_test_indexed_display)
VISUS_TEST_CASE(visus_test_histogram)
VISUS_TEST_CASE(visus_test_height_field) 
VISUS_TEST_CASE(visus_test_colored_isosurface) 
VISUS_TEST_CASE(visus_test_load_xml) 
VISUS_TEST_CASE(visus_test_earth_height) 
VISUS_TEST_CASE(visus_test_time) 
VISUS_TEST_CASE(visus_test_projector) 
VISUS_TEST_CASE(visus_test_font) 

IF (VISUS_ENABLE_FLTK)
   VISUS_TEST_CASE(visus_test_fltk)
   VISUS_TEST_CASE(visus_test_fltk_thread)
ENDIF (VISUS_ENABLE_FLTK)
   

SET_TARGET_PROPERTIES( ${VISUS_TARGETS}
      PROPERTIES
      COMPILE_FLAGS ${ADDITIONAL_COMPILE_FLAGS}
      CLEAN_DIRECT_OUTPUT 1
)

INSTALL(TARGETS ${VISUS_TARGETS} 
        ARCHIVE DESTINATION ${VISUS_LIBRARIES}
        LIBRARY DESTINATION ${VISUS_LIBRARIES}
        RUNTIME DESTINATION ${VISUS_BINARIES}
)

#IF (VISUS_ENABLE_TESTS)
#   SUBDIRS(test)
#ENDIF (VISUS_ENABLE_TESTS)

