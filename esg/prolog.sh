#!/bin/sh
# superstition
unset PYTHONPATH
unset PYTHONSTARTUP
unset PYTHONHOME

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
if (test "${OS}" = "SunOS") then
   CC=cc; export CC 
fi
if (test "${OS}" = "OSF1") then
   CC=cc; export CC
fi

if (test -z "$1") then
    echo "Usage: $0 prefix";
    exit 1
fi
if (test ! -d $1) then
    echo -n "$1 is not a directory; create it? (y/[n])";
    y='n'
    read y;
    if (test ${y} = 'y') then
        mkdir $1; mkdir $1/bin; mkdir $1/lib; mkdir $1/include
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

if (test ! -d build) then
    # Unpack everything into build
    mkdir build
    /bin/cp *.gz build
    cd build
    chmod +w *.gz 
    for x in *.gz; 
    do 
        echo "$x"; 
        gunzip -f $x;
        tar xf `basename $x .gz`;
        /bin/rm -f `basename $x .gz`
    done
    cd ..
fi
cd build
echo "Installation to ${prefix}"
