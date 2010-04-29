# 
# Makefile - Makefile for SGI IRIX version of Contour library
#
# $Id: Makefile,v 1.21.2.1 2007/09/04 17:37:02 tbremer Exp $
#

#===================================================
# these must be defined before mf-include is included
export VISUS_SRC_ROOT?=$(shell pwd)/../..
SUBDIR_NAME = libcontour
# get top-level definitions...
#===================================================
# MODIFY THESE THREE "_TARGETS" VARIABLES TO GET THE TOP-LEVEL MAKEFILE TO AUTOMATICALLY BUILD AND INSTALL STUFF FOR YOU:
LIB_TARGETS = $(VISUSTARGET)/lib/libcontour.a
EXE_TARGETS = 
HEADER_TARGETS = $(VISUSTARGET)/include/contour.h

#===================================================
include ../../config/common
include $(MF-INCLUDE)
include $(VISUS_SRC_ROOT)/config/common-rules
#===================================================
#
# To debug with purify set the following variable to: purify
#
PURIFY = 
#PURIFY = purify -purify-home=/lusr/pure/purify-4.1-irix6/
#

C++FILES =  contour.C edgeHash.C cellQueue.C range.C seedCells.C \
	    data.C dataslc.C datavol.C segTree.C contour2d.C contour3d.C \
	    conplot_p.C conplot.C conplot2d.C queue.C squeue.C rangeProp.C \
	    conplot3d.C rangeSweep.C datareg2.C conplotreg2.C dirSeeds.C \
	    intTree.C bucketSearch.C datareg3.C conplotreg3.C regProp.C \
	    dirSeedsReg2.C regProp2.C seedChkr2.C seedChkr3.C seedAll.C \
	    respProp2.C seedDirReg3.C
#	    respProp2.C seedDirReg3.C topologydata.C

#CCFLAGS = -g -Wall
CXXINCLUDES += -I/usr/include 


O++FILES =  $(LOCAL-OUTDIR)/contour.o $(LOCAL-OUTDIR)/contour2d.o $(LOCAL-OUTDIR)/contour3d.o $(LOCAL-OUTDIR)/range.o $(LOCAL-OUTDIR)/data.o $(LOCAL-OUTDIR)/dataslc.o $(LOCAL-OUTDIR)/datavol.o $(LOCAL-OUTDIR)/edgeHash.o $(LOCAL-OUTDIR)/cellQueue.o $(LOCAL-OUTDIR)/queue.o $(LOCAL-OUTDIR)/squeue.o $(LOCAL-OUTDIR)/seedCells.o $(LOCAL-OUTDIR)/segTree.o $(LOCAL-OUTDIR)/conplot_p.o $(LOCAL-OUTDIR)/conplot.o $(LOCAL-OUTDIR)/conplot2d.o $(LOCAL-OUTDIR)/rangeProp.o $(LOCAL-OUTDIR)/conplot3d.o $(LOCAL-OUTDIR)/rangeSweep.o $(LOCAL-OUTDIR)/datareg2.o $(LOCAL-OUTDIR)/conplotreg2.o $(LOCAL-OUTDIR)/dirSeeds.o $(LOCAL-OUTDIR)/intTree.o $(LOCAL-OUTDIR)/bucketSearch.o $(LOCAL-OUTDIR)/datareg3.o $(LOCAL-OUTDIR)/conplotreg3.o $(LOCAL-OUTDIR)/regProp.o $(LOCAL-OUTDIR)/dirSeedsReg2.o $(LOCAL-OUTDIR)/regProp2.o $(LOCAL-OUTDIR)/seedChkr2.o $(LOCAL-OUTDIR)/seedChkr3.o $(LOCAL-OUTDIR)/seedAll.o $(LOCAL-OUTDIR)/respProp2.o $(LOCAL-OUTDIR)/seedDirReg3.o


all: lib

$(LOCAL-OUTDIR)/libcontour.a: $(O++FILES)
	@echo "making $@  "
	ar rv $@  $^
	ranlib $@
	@echo "$@ created "


clean: subdir-clean

depend: MAKEDEPEND 

undepend: MAKEUNDEPEND

# DO NOT DELETE
