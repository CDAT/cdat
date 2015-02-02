
# regridding tests added by Alex Pletzer, Tech-X, pletzer@txcorp.com

add_test("test177_regrid_mask"
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/test177_regrid_mask.py)

if (EXISTS "${CMAKE_INSTALL_PREFIX}/bin/mpiexec") 
         
add_test("testMPIGrid"
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testMPIGrid.py)
        add_test("testAccuracy"
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testAccuracy.py)
add_test("testMarvel" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testMarvel.py)
add_test("testUnits" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testUnits.py)
add_test("testRaynaud" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testRaynaud.py)
add_test("testDateline" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDateline.py)
add_test("testEsmfInit4Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "4"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfInit.py)
add_test("testImports" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testImports.py)
add_test("testDiag" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testDiag.py)
add_test("testSo2TasACCESSFails" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testSo2TasACCESSFails.py)
add_test("testSo2Tas" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testSo2Tas.py)
if(CDAT_DOWNLOAD_SAMPLE_DATA)
  add_test("testWrite"
    "${PYTHON_EXECUTABLE}"
    ${cdat_SOURCE_DIR}/testing/regrid/testWrite.py
    ${CMAKE_INSTALL_DIR}/sample_data)
endif()
add_test("testMvCdmsRegrid" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testMvCdmsRegrid.py)
add_test("testLibCFLevelAndTime" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testLibCFLevelAndTime.py)
add_test("testMpi10Pes"  
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "10"
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testMpi.py)
add_test("testRemoteGet8Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "8"
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testRemoteGet.py)
add_test("testUnstructGrid" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testUnstructGrid.py)
add_test("testConserv" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testConserv.py)
add_test("testEsmfVsLibcf" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfVsLibcf.py)
add_test("testEsmfInterface1" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "1" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfInterface.py)
add_test("testEsmfInterface2Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfInterface.py)
add_test("testDistArray6Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "6"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDistArray.py)
add_test("testGhostedDistArray6Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "6"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testGhostedDistArray.py)
add_test("testDistTransientVar6Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "6"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDistTransientVar.py)
add_test("testEsmfGridToMeshRegridCsrv" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfGridToMeshRegridCsrv.py)
if (CDAT_BUILD_PARALLEL)
  add_test("testEsmf3DSmallNative" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf3DSmallNative.py)
  add_test("testEsmf3DSmallNative2Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf3DSmallNative.py)
endif (CDAT_BUILD_PARALLEL)
add_test("testEsmf3DSmallesmf" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf3DSmallesmf.py)
add_test("testEsmf3DSmallesmf2Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf3DSmallesmf.py)
add_test("testGridTypes" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testGridTypes.py)
add_test("testEsmfSmall" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSmall.py)
add_test("testEsmfSmall2Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSmall.py)
add_test("testEsmfSmall4Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "4"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSmall.py)
add_test("testEsmfSmall8Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "8"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSmall.py)
add_test("testTasRegrid" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testTasRegrid.py)
add_test("testTasRegrid2Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testTasRegrid.py)
add_test("testCltRegrid" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testCltRegrid.py)
#temporarily disabled
#add_test("testCltRegrid2Pes"
#         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2"
#          "${PYTHON_EXECUTABLE}"
#          ${cdat_SOURCE_DIR}/testing/regrid/testCltRegrid.py)
add_test("testRegrid2" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testRegrid2.py)
add_test("testRegrid2Tool" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testRegrid2Tool.py)
add_test("testEsmf_3x4_6x8_Conserve" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Conserve.py)
add_test("testEsmf_3x4_6x8_Bilinear" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Bilinear.py)
add_test("testEsmf_3x4_6x8_Conserve_Masked"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Conserve_Masked.py)
add_test("testEsmf_3x4_6x8_Bilinear_Masked" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Bilinear_Masked.py)
add_test("testMvGenericRegrid" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testMvGenericRegrid.py)
add_test("testSalinity" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testSalinity.py)
add_test("testEsmfSalinity" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSalinity.py)
add_test("testEsmfSalinity2Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "2"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSalinity.py)
add_test("testEsmfSalinity4Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "4"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSalinity.py)
add_test("testEsmfSalinity8Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "8"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSalinity.py)
#add_test("testRegridEsmfMpi8Pes" 
#         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "8"
#          "${PYTHON_EXECUTABLE}"
#          ${cdat_SOURCE_DIR}/testing/regrid/testRegridEsmfMpi.py)
add_test("testEsmf" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf.py)
add_test("testBergman" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testBergman.py)
add_test("testDistSrc" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDistSrc.py)
add_test("testDistSrc4Pes" 
         "${CMAKE_INSTALL_PREFIX}/bin/mpiexec" "-n" "4" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDistSrc.py)
add_test("testReverseOrderInterp"  
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testReverseOrderInterp.py)

else()

# if mpiexec is not found, run only the serial tests

add_test("testMPIGrid"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testMPIGrid.py)
        add_test("testAccuracy"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testAccuracy.py)
add_test("testMarvel" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testMarvel.py)
add_test("testUnits" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testUnits.py)
add_test("testRaynaud" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testRaynaud.py)
add_test("testDateline" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDateline.py)
add_test("testImports" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testImports.py)
add_test("testDiag" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testDiag.py)
add_test("testSo2TasACCESSFails" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testSo2TasACCESSFails.py)
add_test("testSo2Tas" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testSo2Tas.py)
add_test("testWrite" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testWrite.py
         ${CMAKE_INSTALL_DIR}/sample_data)
add_test("testMvCdmsRegrid" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testMvCdmsRegrid.py)
add_test("testLibCFLevelAndTime" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testLibCFLevelAndTime.py)
add_test("testUnstructGrid" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testUnstructGrid.py)
add_test("testConserv" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testConserv.py)
add_test("testEsmfVsLibcf" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfVsLibcf.py)
add_test("testEsmfInterface1" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfInterface.py)
add_test("testEsmfGridToMeshRegridCsrv" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfGridToMeshRegridCsrv.py)
add_test("testEsmf3DSmallesmf" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf3DSmallesmf.py)
add_test("testEsmf3DSmallesmf" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf3DSmallesmf.py)
add_test("testGridTypes" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testGridTypes.py)
add_test("testEsmfSmall" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSmall.py)
add_test("testTasRegrid" 
         "${PYTHON_EXECUTABLE}"
         ${cdat_SOURCE_DIR}/testing/regrid/testTasRegrid.py)
add_test("testCltRegrid" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testCltRegrid.py)
add_test("testRegrid2" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testRegrid2.py)
add_test("testRegrid2Tool" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testRegrid2Tool.py)
add_test("testEsmf_3x4_6x8_Conserve" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Conserve.py)
add_test("testEsmf_3x4_6x8_Bilinear" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Bilinear.py)
add_test("testEsmf_3x4_6x8_Conserve_Masked"
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Conserve_Masked.py)
add_test("testEsmf_3x4_6x8_Bilinear_Masked" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf_3x4_6x8_Bilinear_Masked.py)
add_test("testMvGenericRegrid" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testMvGenericRegrid.py)
add_test("testSalinity" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testSalinity.py)
add_test("testEsmfSalinity" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmfSalinity.py)
add_test("testEsmf" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testEsmf.py)
add_test("testBergman" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testBergman.py)
add_test("testDistSrc" 
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testDistSrc.py)
add_test("testReverseOrderInterp"  
          "${PYTHON_EXECUTABLE}"
          ${cdat_SOURCE_DIR}/testing/regrid/testReverseOrderInterp.py)

endif()

# Tests which should fail
set_tests_properties(testSo2TasACCESSFails PROPERTIES PASS_REGULAR_EXPRESSION "FAILED")


