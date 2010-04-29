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


#=========================================================
#  Format:
#     SWIGGenerate(ModuleName  IncludeFiles  DependModuleNames)
#=========================================================

INCLUDE(${VISUS_SOURCE_DIR}/config/SWIGConfig.cmake)

SET(PYVISUS_SRC
  __init__.py
  setup.py
)

#--------------------------------
# Set Smart Pointer Base Classes
#--------------------------------
SET(VISUS_SP_BASES
VisusGroup
VisusConsumer
VisusProducer
VisusSharedValue
)
SWIGSPBases("${VISUS_SP_BASES}")

#--------------------------------
# Set Interface Files
#--------------------------------
SET(VISUS_PY_CORE_SRC
VisusArrays.i
VisusBoundingBox.i
VisusCamera.i
VisusColor.i
VisusColorMap.i
VisusConsumer.i
VisusDataDescription.i
VisusDefaultColorMaps.i
VisusGenericValue.i
VisusEarthRadius.i
VisusFont.i
VisusIsoValue.i
VisusOpenGLState.i
VisusGroup.i
VisusProducer.i
VisusSmartPointer.i
VisusSTLInterface.i
VisusText.i
#VisusDataTransform.i
VisusTransformation2D.i
VisusTransformation3D.i
VisusXMLInterface.i
#VisusTime.i
VisusGenericTime.i
VisusIntTime.i
VisusDoubleTime.i
VisusGlobalTime.i
VisusProjection.i
VisusSphereProjection.i
VisusTypeInfo.i
)
SWIGGenerate("core" "${VISUS_PY_CORE_SRC}" "")

SET(VISUS_PY_CORE_SRC
VisusSharedValue.i
VisusSharedBoundingBox.i
VisusSharedDataRequest.i
VisusSharedEarthRadius.i
#VisusSharedGlobalTime.i
)
SWIGGenerate("shared" "${VISUS_PY_CORE_SRC}" "core")

SET(VISUS_PY_DATA_SRC
PvDataType.i
VisusUnits.i
VisusData.i
VisusMetricData.i
VisusIndexedData.i
VisusBlockData.i
VisusMeshData.i
VisusIndexedData.i
VisusTexture.i
VisusDataRequest.i
VisusDataSource.i
VisusDataSourceFactory.i
VisusFastLoopDataSource.i
VisusIncoreDataSource.i
)
SWIGGenerate("data" "${VISUS_PY_DATA_SRC}" "core")


SET(VISUS_PY_COMPONENT_SRC
VisusBorderAxis.i
VisusBorderBox2D.i
VisusColorBar.i
VisusLabelNode.i
VisusSceneNode.i
VisusShapesNode.i
#VisusSphere.i
VisusSphereNode.i
VisusText.i
VisusTextNode.i
VisusTickMarks.i
)
SWIGGenerate("component" "${VISUS_PY_COMPONENT_SRC}" "core")


SET(VISUS_PY_DISPLAY_SRC
VisusEarthNode.i
VisusMeshDisplay.i
VisusIndexedDisplay.i
VisusOrthogonalSlice.i
VisusRenderer.i
VisusTextureViewer.i
VisusGraph.i
VisusHistogram.i
VisusLineGraph.i
VisusHeightField.i
)
SWIGGenerate("display" "${VISUS_PY_DISPLAY_SRC}" "core")


SET(VISUS_PY_EXTRACT_SRC
VisusSampleExtractor.i
VisusAxisAlignedExtractor.i
VisusIsoSurface.i
VisusColoredIsoSurface.i
VisusProjector.i
)
SWIGGenerate("extract" "${VISUS_PY_EXTRACT_SRC}" "core")


IF (VISUS_ENABLE_FLTK) 
   SET(VISUS_PY_GUI_SRC
      VisusGuiBase.i
      VisusFLTKWindow.i
      VisusFLTKGui.i
      VisusFLTKMaster.i
      VisusGUI.i
   )
SWIGGenerate("gui" "${VISUS_PY_GUI_SRC}" "core")
ENDIF (VISUS_ENABLE_FLTK) 

#--------------------------------
# Set Include Directories
#--------------------------------
SET(INCLUDES
  ${VISUS_SRC}/glew
  ${VISUS_SRC}/xmlParser
  ${VISUS_SRC}/visusscene
  ${GLUT_INCLUDE_DIR}
  ${FTGL_INCLUDE_DIR}
  ${FREETYPE2_INCLUDE_DIR}
  ${FREETYPE2_FT2BUILD}
  ${FOX_INCLUDE_DIR}
  ${FREETYPE2_INCLUDE_DIR}
  ${PTHREAD_INCLUDE_DIR}
  ${PYTHON_INCLUDE_PATH}
)

#--------------------------------
#  Setup Include Path
#--------------------------------
INCLUDE_DIRECTORIES( ${INCLUDES} )

#--------------------------------
# Find Python
#--------------------------------
#FIND_PACKAGE(PythonLibs)


INSTALL(FILES ${PYVISUS_SRC} DESTINATION ${VISUS_PYTHON})

