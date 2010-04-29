

SET(LIBCONTOUR_SRC
contour.C 
edgeHash.C 
cellQueue.C 
range.C 
seedCells.C         
data.C 
dataslc.C 
datavol.C 
segTree.C 
contour2d.C 
contour3d.C
conplot_p.C 
conplot.C 
conplot2d.C
queue.C 
squeue.C 
rangeProp.C 
conplot3d.C 
rangeSweep.C 
datareg2.C 
conplotreg2.C 
dirSeeds.C
intTree.C 
bucketSearch.C 
datareg3.C 
conplotreg3.C 
regProp.C 
dirSeedsReg2.C 
regProp2.C 
seedChkr2.C 
seedChkr3.C 
seedAll.C 
respProp2.C
seedDirReg3.C
)

SET(LIBCONTOUR_HEADERS
        contour.h
)


IF (VISUS_ENABLE_STATIC)

  ADD_LIBRARY(contour-static STATIC ${LIBCONTOUR_SRC})


  SET_TARGET_PROPERTIES(contour-static
           PROPERTIES
           OUTPUT_NAME contour
           CLEAN_DIRECT_OUTPUT 1
  )

  INSTALL(TARGETS contour-static
	  LIBRARY DESTINATION ${VISUS_LIBRARIES}
	  ARCHIVE DESTINATION ${VISUS_LIBRARIES}     
  )
ENDIF (VISUS_ENABLE_STATIC)

IF (VISUS_ENABLE_DYNAMIC)
   ADD_LIBRARY(contour-shared SHARED ${LIBCONTOUR_SRC})

   SET_TARGET_PROPERTIES(contour-shared 
         PROPERTIES
         OUTPUT_NAME contour${SO_EXT}
         CLEAN_DIRECT_OUTPUT 1
   )

   INSTALL(TARGETS contour-shared
	LIBRARY DESTINATION ${VISUS_LIBRARIES}
	ARCHIVE DESTINATION ${VISUS_LIBRARIES}     
   )

ENDIF (VISUS_ENABLE_DYNAMIC)


INSTALL(FILES ${LIBCONTOUR_HEADERS} DESTINATION ${VISUS_INCLUDE})

