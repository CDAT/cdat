#!/bin/sh
if (test "$1" = "--debug") then
    D="--debug";
    OPT=-g; 
    shift
else
    D="";
    OPT=${OPT:=-O}
fi
export OPT

OS=`uname`

if (test -z "$1") then
    echo "Usage: $0 prefix";
    exit 1
fi
version=`more ../version`

if (test ! -d $1) then
    echo -n "$1/${version} is not a directory; create it? (y/[n])";
    y='n'
    read y;
    if (test ${y} = 'y') then
        mkdir -p $1/${version}/bin; mkdir $1/${version}/lib; mkdir $1/${version}/include ; mkdir -p $1/Externals/bin ; mkdir $1/Externals/lib ; mkdir $1/Externals/share ; mkdir $1/Externals/include
        if (test ! -d $1) then
            echo "Could not create $1, installation aborted.";
            exit 1
        fi
    else
        echo 'Installation aborted.';
        exit 1
    fi
fi
echo "Created  $1/${version} and $1/Externals directories"
echo "Python/CDAT built in $1/${version} and external dependencies binaries and libs are built to $1/Externals"

prefix=`(cd $1;pwd)`

if (test ! -d build) then
    # Unpack everything into build
    mkdir build
    /bin/cp src/*gz build
    cd build
    OS=`uname`
    if (test "${OS}" = "Linux" ) then
         TAR=`which tar`
    elif (test "${OS}" = "Darwin" ) then
    	 TAR=`which tar`
    elif (test "${OS}" = "CYGWIN_NT-5.1" ) then
         TAR=`which tar`
    elif (test "${OS}" = "CYGWIN_NT-6.0" ) then
         TAR=`which tar`
    elif (test "${OS}" = "AIX" ) then
         TAR=`which tar`
    else
         echo "Building tar for non GNU OS to unpack Python, some error messages may be generated but can be ignored"
	 chmod +w tar*gz
         for x in tar*gz;
         do 
           gunzip -f $x;
           tar xf `basename $x .gz`;
	   (cd tar-* ; ./configure --prefix=$1/Externals ; make ; make install; cd .. )> LOG.prolog;
           TAR=$1/Externals/bin/tar
         done
    fi
    #rm tar*gz
    chmod +w *.gz 
    for x in *.gz; 
    do 
        echo "$x"; 
	gunzip -f $x;
        ${TAR} xf `basename $x .gz`;
        /bin/rm -f `basename $x .gz`;
    done
#    for x in *.tgz; 
#    do 
#        echo "$x"; 
#        ${TAR} xzf $x;
#        /bin/rm -f $x
#    done
    cd ..
fi
cd build
echo "Installation to ${prefix}"
