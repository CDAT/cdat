# - FindSciVisIt: Module to find include directories and
#   libraries for VisIt.
#
# Module usage:
#   find_package(SciVisIt ...)
#
# This module will define the following variables:
#  HAVE_VISIT, VISIT_FOUND = Whether libraries and includes are found
#  VisIt_INCLUDE_DIRS       = Location of VisIt includes
#  VisIt_LIBRARY_DIRS       = Location of VisIt libraries
#  VisIt_LIBRARIES          = Required libraries

#################################################
#
# Find module for VisIt includes and libs from VisIt
#
# $Id: FindSciVisIt.cmake 1252 2012-02-07 19:45:49Z mdurant $
#
#################################################

# Make a list of library names
if (WIN32)
  set(VisIt_DIFF_LIBS enginelib_ser guilib viewerlib)
else ()
  set(VisIt_DIFF_LIBS engine_ser gui viewer avtpythonfilters_ser)
endif ()

# Libraries that are only required to support the volume plot
set(VisIt_VOLUME_LIBS
  air
  alan
  biff
  dye
  ell
  gage
  hest
  moss
  nrrd
  slivrG
  slivrV
  unrrdu
)

set(VisIt_LIBRARY_NAMES_SEARCHED
  ${VisIt_DIFF_LIBS}
  ${VisIt_VOLUME_LIBS}
  GLEW
  avtdatabase_ser
  avtdbatts
  avtdbin_ser
  avtexpressions_ser
  avtfilters_ser
  avtivp
  avtmath
  avtmir_ser
  avtpipeline_ser
  avtplotter_ser
  avtqtviswindow
  avtquery_ser
  avtshapelets
  avtview
  avtviswindow_ser
  avtwriter_ser
  bow
  engineproxy
  enginerpc
  lightweight_visit_vtk
  mdserverproxy
  mdserverrpc
  vclproxy
  vclrpc
  viewerproxy
  viewerrpc
  visit_verdict
  visit_vtk
  visitcommon
  visitpy
  vtkqt
  winutil
)

set(VisIt_SUBDIRS
  allhosts databases help movietemplates operators
  plots translations ultrawrapper
)

if (WIN32)
  set(VisIt_EXES
    engine_ser.exe vcl.exe mdserver.exe text2polys.exe
    visitconvert_ser.exe time_annotation.exe visitprotocol.exe
    mpeg2encode.exe visit_composite.exe qtssh.exe
    visit_transition.exe
  )
endif ()

# Installing VisIt from the package is not working on Windows, so
# ... So what?  What's the rest of this comment?
if (WIN32)
  set(VisIt_INC_SUBDIR include/visit/vtkqt)
  set(VisIt_LIB_SUBDIR lib)
else ()
  # VisIt_ARCH is required on Mac and Linux
  # if it isn't set, try to set it
  if (NOT VisIt_ARCH)
    if (EXISTS "${CMAKE_SOURCE_DIR}/CMake/DetermineVisItArchitecture.cmake")
      include("${CMAKE_SOURCE_DIR}/CMake/DetermineVisItArchitecture.cmake")
      DETERMINE_VisIt_ARCHITECTURE(VisIt_ARCH)
    endif ()
    if (VisIt_ARCH)
      message(STATUS "VisIt_ARCH is ${VisIt_ARCH}")
    else ()
      message(STATUS "Unable to determine VisIt_ARCH.")
      message(STATUS "Please specify on the command line with -DVisIt_ARCH=")
      if (SciVisIt_FIND_REQUIRED)
        message(FATAL_ERROR "FindVisIt script will not operate correctly.  Failing.")
      endif ()
    endif ()
  endif ()
  set(VisIt_INC_SUBDIR current/${VisIt_ARCH}/include/visit/vtkqt)
  set(VisIt_LIB_SUBDIR current/${VisIt_ARCH}/lib)
endif ()

# Call SciFindPackage
# With some tricks
# Note that although we want to find the base VisIt include dir,
# there are no headers in that directory.  So we search for the
# vtkqt include directory, and then derive the base include dir.
set(VisIt_INST_SUBDIR visit)
SciFindPackage(PACKAGE VisIt
  INSTALL_DIR "${VisIt_INST_SUBDIR}"
  HEADERS vtkQtRenderWindow.h
  LIBRARIES "${VisIt_LIBRARY_NAMES_SEARCHED}"
  INCLUDE_SUBDIRS "${VisIt_INC_SUBDIR}"
  LIBRARY_SUBDIRS "${VisIt_LIB_SUBDIR}"
)
if (VISIT_FOUND)
  message("First search located VisIt.")
else ()
  message("First search did not locate VisIt.  Trying second.")
  set(VisIt_INST_SUBDIR visit2)
  SciFindPackage(PACKAGE VisIt
    INSTALL_DIR "${VisIt_INST_SUBDIR}"
    HEADERS vtkQtRenderWindow.h
    LIBRARIES "${VisIt_LIBRARY_NAMES_SEARCHED}"
    INCLUDE_SUBDIRS "${VisIt_INC_SUBDIR}"
    LIBRARY_SUBDIRS "${VisIt_LIB_SUBDIR}"
  )
  if (VISIT_FOUND)
    message("Second search located VisIt.")
  endif ()
endif ()

# Look for the root of the VisIt install and the architecture dir
if (VISIT_FOUND)
# VisIt_ROOT_DIR is the ROOT of the VisIt installation, NOT the bin dir
  if (WIN32)
    get_filename_component(VisIt_ROOT_DIR
      "${VisIt_vtkQtRenderWindow_h_INCLUDE_DIR}/../../../" REALPATH)
  else ()
    get_filename_component(VisIt_ROOT_DIR
      "${VisIt_vtkQtRenderWindow_h_INCLUDE_DIR}/../../../../../" REALPATH)
  endif ()
  message(STATUS "VisIt_ROOT_DIR is ${VisIt_ROOT_DIR}")
endif ()
if (WIN32)
  set(VisIt_ARCH_DIR ${VisIt_ROOT_DIR})
  set(VisIt_ARCH_SUBDIR)
else ()
  get_filename_component(VisIt_ARCH_DIR
    ${VisIt_ROOT_DIR}/current/${VisIt_ARCH} REALPATH)
  get_filename_component(VisIt_VERSION_DIR ${VisIt_ARCH_DIR}/.. REALPATH)
  get_filename_component(VisIt_VERSION ${VisIt_VERSION_DIR} NAME)
  set(VisIt_ARCH_SUBDIR ${VisIt_VERSION}/${VisIt_ARCH})
endif ()
message(STATUS "VisIt_ARCH_DIR = ${VisIt_ARCH_DIR}")
message(STATUS "VisIt_ARCH_SUBDIR = ${VisIt_ARCH_SUBDIR}.")
message(STATUS "VisIt_VERSION = ${VisIt_VERSION}.")
message(STATUS "VisIt_VERSION = ${VisIt_VERSION}.")

# Find the remaining directories
if (VISIT_FOUND)

  message(STATUS "Found VisIt")
  get_filename_component(VisIt_LIB_DIR "${VisIt_avtdatabase_ser_LIBRARY_DIR}"
    REALPATH)
  if (WIN32)
    message(STATUS "Looking for VisIt_BIN_DIR in ${VisIt_ROOT_DIR}")
    get_filename_component(VisIt_BIN_DIR "${VisIt_ROOT_DIR}" REALPATH)
  else ()
    message(STATUS "Looking for VisIt_BIN_DIR in ${VisIt_ROOT_DIR}/bin")
    get_filename_component(VisIt_BIN_DIR "${VisIt_ROOT_DIR}/bin" REALPATH)
  endif ()
  message(STATUS "VisIt_LIB_DIR is ${VisIt_LIB_DIR}")
  message(STATUS "VisIt_BIN_DIR is ${VisIt_BIN_DIR}")
  get_filename_component(VisIt_INCLUDE_DIR "${VisIt_vtkQtRenderWindow_h_INCLUDE_DIR}/.." REALPATH)
  set(VisIt_INCLUDE_DIRS
    ${VisIt_INCLUDE_DIR}/
    ${VisIt_INCLUDE_DIR}/avt/Database/Database
    ${VisIt_INCLUDE_DIR}/avt/Database/Formats
    ${VisIt_INCLUDE_DIR}/avt/Database/Ghost
    ${VisIt_INCLUDE_DIR}/avt/DataBinning
    ${VisIt_INCLUDE_DIR}/avt/DBAtts/MetaData
    ${VisIt_INCLUDE_DIR}/avt/DBAtts/SIL
    ${VisIt_INCLUDE_DIR}/avt/Expressions/Abstract
    ${VisIt_INCLUDE_DIR}/avt/Expressions/CMFE
    ${VisIt_INCLUDE_DIR}/avt/Expressions/Conditional
    ${VisIt_INCLUDE_DIR}/avt/Expressions/Derivations
    ${VisIt_INCLUDE_DIR}/avt/Expressions/General
    ${VisIt_INCLUDE_DIR}/avt/Expressions/ImageProcessing
    ${VisIt_INCLUDE_DIR}/avt/Expressions/Management
    ${VisIt_INCLUDE_DIR}/avt/Expressions/Math
    ${VisIt_INCLUDE_DIR}/avt/Expressions/MeshQuality
    ${VisIt_INCLUDE_DIR}/avt/Expressions/TimeIterators
    ${VisIt_INCLUDE_DIR}/avt/FileWriter
    ${VisIt_INCLUDE_DIR}/avt/Filters
    ${VisIt_INCLUDE_DIR}/avt/IVP
    ${VisIt_INCLUDE_DIR}/avt/Math
    ${VisIt_INCLUDE_DIR}/avt/MIR/Base
    ${VisIt_INCLUDE_DIR}/avt/MIR/Discrete
    ${VisIt_INCLUDE_DIR}/avt/MIR/Tet
    ${VisIt_INCLUDE_DIR}/avt/MIR/Youngs
    ${VisIt_INCLUDE_DIR}/avt/MIR/Zoo
    ${VisIt_INCLUDE_DIR}/avt/Pipeline/AbstractFilters
    ${VisIt_INCLUDE_DIR}/avt/Pipeline/Data
    ${VisIt_INCLUDE_DIR}/avt/Pipeline/Pipeline
    ${VisIt_INCLUDE_DIR}/avt/Pipeline/Sinks
    ${VisIt_INCLUDE_DIR}/avt/Pipeline/Sources
    ${VisIt_INCLUDE_DIR}/avt/Plotter
    ${VisIt_INCLUDE_DIR}/avt/Plotter/vtk
    ${VisIt_INCLUDE_DIR}/avt/Preprocessor
    ${VisIt_INCLUDE_DIR}/avt/PythonFilters
    ${VisIt_INCLUDE_DIR}/avt/QtVisWindow
    ${VisIt_INCLUDE_DIR}/avt/Queries/Abstract
    ${VisIt_INCLUDE_DIR}/avt/Queries/Misc
    ${VisIt_INCLUDE_DIR}/avt/Queries/Pick
    ${VisIt_INCLUDE_DIR}/avt/Queries/Queries
    ${VisIt_INCLUDE_DIR}/avt/Shapelets
    ${VisIt_INCLUDE_DIR}/avt/View
    ${VisIt_INCLUDE_DIR}/avt/VisWindow/Colleagues
    ${VisIt_INCLUDE_DIR}/avt/VisWindow/Exceptions
    ${VisIt_INCLUDE_DIR}/avt/VisWindow/Interactors
    ${VisIt_INCLUDE_DIR}/avt/VisWindow/Proxies
    ${VisIt_INCLUDE_DIR}/avt/VisWindow/Tools
    ${VisIt_INCLUDE_DIR}/avt/VisWindow/VisWindow
    ${VisIt_INCLUDE_DIR}/common/comm
    ${VisIt_INCLUDE_DIR}/common/Exceptions/Database
    ${VisIt_INCLUDE_DIR}/common/Exceptions/Pipeline
    ${VisIt_INCLUDE_DIR}/common/Exceptions/Plotter
    ${VisIt_INCLUDE_DIR}/common/expr
    ${VisIt_INCLUDE_DIR}/common/misc
    ${VisIt_INCLUDE_DIR}/common/parser
    ${VisIt_INCLUDE_DIR}/common/plugin
    ${VisIt_INCLUDE_DIR}/common/proxybase
    ${VisIt_INCLUDE_DIR}/common/state
    ${VisIt_INCLUDE_DIR}/common/utility
    ${VisIt_INCLUDE_DIR}/engine/main
    ${VisIt_INCLUDE_DIR}/engine/rpc
    ${VisIt_INCLUDE_DIR}/gui
    ${VisIt_INCLUDE_DIR}/gui/designer/QvisColorButton
    ${VisIt_INCLUDE_DIR}/gui/designer/QvisDialogLineEditPlugin
    ${VisIt_INCLUDE_DIR}/gui/designer/QvisSpectrumBarPlugin
    ${VisIt_INCLUDE_DIR}/gui/designer/QvisVariableButtonPlugin
    ${VisIt_INCLUDE_DIR}/include
    ${VisIt_INCLUDE_DIR}/launcher/main
    ${VisIt_INCLUDE_DIR}/launcher/proxy
    ${VisIt_INCLUDE_DIR}/launcher/rpc
    ${VisIt_INCLUDE_DIR}/mdserver/main
    ${VisIt_INCLUDE_DIR}/mdserver/proxy
    ${VisIt_INCLUDE_DIR}/mdserver/rpc
    ${VisIt_INCLUDE_DIR}/viewer/main
    ${VisIt_INCLUDE_DIR}/viewer/proxy
    ${VisIt_INCLUDE_DIR}/viewer/rpc
    ${VisIt_INCLUDE_DIR}/visitpy/common
    ${VisIt_INCLUDE_DIR}/visitpy/mpicom/src
    ${VisIt_INCLUDE_DIR}/visitpy/visitpy
    ${VisIt_INCLUDE_DIR}/visit_vtk/full
    ${VisIt_INCLUDE_DIR}/visit_vtk/lightweight
    ${VisIt_INCLUDE_DIR}/vtkqt
    ${VisIt_INCLUDE_DIR}/winutil
  )
  set(VisIt_THIRDPARTY_INCLUDE_DIRS
    ${VisIt_INCLUDE_DIR}/third_party_builtin/bow
    ${VisIt_INCLUDE_DIR}/third_party_builtin/cognomen/src/cog
    ${VisIt_INCLUDE_DIR}/third_party_builtin/cognomen/src
    ${VisIt_INCLUDE_DIR}/third_party_builtin/glew/glew/include/GL
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/slivr
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/include
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/air
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/alan
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/bane
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/biff
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/coil
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/dye
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/echo
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/ell
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/gage
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/hest
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/hoover
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/limn
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/mite
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/moss
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/nrrd
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/push
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/teem
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/teem
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/ten
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/src/unrrdu
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/win32/include
    ${VisIt_INCLUDE_DIR}/third_party_builtin/slivr/teem-1.9.0-src/win32/src/pv
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/3rdParty/bzip2
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/3rdParty/jpeglib
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/3rdParty/tiff
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/3rdParty/zlib
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Basics
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Controller
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/DebugOut
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/3rdParty/boost
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/3rdParty/bzip2
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/3rdParty/tiff
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/3rdParty/zlib
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/DICOM
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/Images
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/IO/UVF
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Renderer
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Renderer/DX
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Renderer/GL
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Renderer/GPUMemMan
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok/Scripting
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok/tuvok
    ${VisIt_INCLUDE_DIR}/third_party_builtin/tuvok
    ${VisIt_INCLUDE_DIR}/third_party_builtin/verdict
    ${VisIt_INCLUDE_DIR}/third_party_builtin/zlib
  )

else ()

  message(STATUS "VisIt not found, set VisIt_ROOT_DIR to the location of the VISIT installation.")
  if (SciVisIt_FIND_REQUIRED)
    message(FATAL_ERROR "Failing.")
  endif ()

endif ()

