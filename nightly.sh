#!/usr/bin/env bash

## script to build automatically CDAT

INSTALL_PATH=$1
GIT_BRANCH=$2
GIT_PATH=$3
QMAKE_EXE=$4

: ${INSTALL_PATH:="/lgm/uvcdat/nightly"}
: ${GIT_BRANCH:="next"}
: ${GIT_PATH:="/git/cdat"}
: ${QMAKE_EXE:="/usr/bin/qmake"}

cd ${GIT_PATH} ; \
git checkout ${GIT_BRANCH} ; \
git pull ; \
./clean_script all ; \
rm -rf ${INSTALL_PATH} ; \
rm -rf build_nightly ;\ 
mkdir build_nightly ;\
cd build_nightly ;\
cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} -DCDAT_BUILD_ESMF_ESMP=ON -DCDAT_BUILD_ESMF_PARALLEL=ON -DCDAT_BUILD_PARAVIEW=ON  -DCDAT_BUILD_VISIT=ON -DQT_QMAKE_EXECUTABLE=${QMAKE_EXE} .. ; \
cmake -DCMAKE_INSTALL_PREFIX=${INSTALL_PATH} -DCDAT_BUILD_ESMF_ESMP=ON -DCDAT_BUILD_ESMF_PARALLEL=ON -DCDAT_BUILD_PARAVIEW=ON  -DCDAT_BUILD_VISIT=ON -DQT_QMAKE_EXECUTABLE=${QMAKE_EXE} .. ; \
pwd ; \
make -j16 ; \
source ${INSTALL_PATH}/bin/setup_cdat.sh ; \
easy_install lep ; \
easy_install MyProxyClient ; \
cd ${INSTALL_PATH} ; \
git clone --depth=1 git://vistrails.org/vistrails.git ; \
cd vistrails ; \
git checkout uvcdat ; \
git pull ;


