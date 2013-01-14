#!/bin/bash

#script does the dance we need to checkout the devel branch
#aka the master branch from the uvcdat-devel repo
#and makes sure it is named devel-master so that subprojects
#namely vistrails and paraview can key off that to check out
#their own uvcdat-devel branches

CTEST_UPDATE_COMMAND=$1
CTEST_SOURCE_DIRECTORY=$2
MASTER_REPO=$3
DEVEL_REPO=$4

${CTEST_UPDATE_COMMAND} clone --recursive ${MASTER_REPO} ${CTEST_SOURCE_DIRECTORY}
cd ${CTEST_SOURCE_DIRECTORY}
${CTEST_UPDATE_COMMAND} remote add devel ${DEVEL_REPO}
${CTEST_UPDATE_COMMAND} fetch devel
${CTEST_UPDATE_COMMAND} checkout -b devel-master devel/master

#below should do effectively the same thing
#${CTEST_UPDATE_COMMAND} clone --recursive ${DEVEL_REPO} ${CTEST_SOURCE_DIRECTORY}
#cd ${CTEST_SOURCE_DIRECTORY}
#git branch -m master devel-master
