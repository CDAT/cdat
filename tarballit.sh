#!/usr/bin/env sh
svn export http://www-pcmdi.llnl.gov/svn/repository/cdat/trunk $1
tar czvf $1-everything.tar.gz  $1
tar czvf $1-cdat.tar.gz --exclude $1/pysrc* --exclude $1/exsrc*  $1
tar czvf $1-pysrc.tar.gz $1/pysrc
tar czvf $1-exsrc.tar.gz $1/exsrc
