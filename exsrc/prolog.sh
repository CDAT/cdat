#!/bin/sh
# superstition
## Undoing superstition, let's be rational here.
#unset PYTHONPATH
#unset PYTHONSTARTUP
#unset PYTHONHOME

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
    echo -n "$1 is not a directory; create it? (y/[n])";
    y='n'
    read y;
    if (test ${y} = 'y') then
        mkdir -p $1/${version}/bin; mkdir $1/${version}/lib; mkdir $1/${version}/include ; mkdir -p $1/Externals/bin ; mkdir $!/Externals/lib ; mkdir $1/Externals/share ; mkdir $1/Externals/include
        if (test ! -d $1) then
            echo "Could not create $1, installation aborted.";
            exit 1
        fi
    else
        echo 'Installation aborted.';
        exit 1
    fi
fi
prefix=`(cd $1;pwd)`

cp -f detect_fortran.py ${prefix}/${version}/bin/detect_fortran.py;
chmod +x  ${prefix}/${version}/bin/detect_fortran.py;

#mkdir -p build
/bin/cp src/${PACKAGE}*gz build
cd build
chmod +w ${PACKAGE}*gz
echo "untarring "${PACKAGE}

for x in ${PACKAGE}*gz;
    do
        echo $x;
        echo `basename $x .gz`;
	gunzip -f $x;
	tar xf `basename $x .gz`;
	/bin/rm -f `basename $x .gz`;
    done


#if (test ! -d build) then
#    # Unpack everything into build
#    mkdir -p build
#    /bin/cp src/*.gz build
#    cd build
#    chmod +w *.gz 
#    for x in *.gz; 
#    do 
#        echo "$x"; 
#        gunzip -f $x;
#        tar xf `basename $x .gz`;
#        /bin/rm -f `basename $x .gz`
#    done
#    cd ..
#fi
echo "Installation of ${PACKAGE} to ${prefix}"
