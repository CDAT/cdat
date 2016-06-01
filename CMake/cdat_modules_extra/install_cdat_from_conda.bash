#!/usr/bin/env bash
SRCS=`dirname $0`
conda create -n TEST_UVCDAT -c uvcdat uvcdat
source activate TEST_UVCDAT
cd ${SRCS}/../..
echo "PATH:"`pwd`
for pkg in cdtime regrid2 cdms2 esg DV3D vcs vcsaddons cdutil unidata xmgrace genutil Thermo WK distarray; do
    cd Packages/${pkg}
    rm -rf build
    python setup.py install
    cd ../..
done




