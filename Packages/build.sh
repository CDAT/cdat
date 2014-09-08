#!/bin/bash

cd vcs
python setup.py install
cd ../DV3D
python setup.py install
cd ../vcsaddons
python setup.py install
