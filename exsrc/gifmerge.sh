#!/bin/sh
PACKAGE="gifmerge"
. ./prolog.sh
(cd gifmerge* ; make ; mv gifmerge ${prefix}/Externals/bin )

