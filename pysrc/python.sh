#!/bin/sh
. ./prolog.sh
# Python, idle
# This needs to be set or Python's installer will conclude _tkinter cannot
# be imported.
CCTEMP=${CC-gcc}
# Get command name WITHOUT the parameters
CCTEMP=`echo $CCTEMP | awk '{print $1}'` 
if (test "${CCTEMP}" = "gcc") then
config_opt="--with-gcc --without-cxx"
else
   config_opt="--without-gcc --without-cxx"
fi
OS=`uname`
if (test "${OS}" = "Darwin") then  # MacIntosh OSX
   CPPFLAGS="-I${prefix}/Externals/include"; export CPPFLAGS
   LDFLAGS="-L${prefix}/Externals/lib"; export LDFLAGS
   config_opt=""
   OPT=""; export OPT
fi
getaddrbug=""
if (test "${OS}" = "OSF1") then
    getaddrbug="--disable-ipv6"
fi
if (test "${OS}" = "AIX") then
    getaddrbug="--disable-ipv6"
fi
cd Python*
/bin/rm -f setup.py
/bin/cp ../../src/setup.py setup.py
CDAT_PREFIX=${prefix}/Externals; export CDAT_PREFIX
if (test "${OS}" = "Linux") then  # Linux -- needed for readline
   export LDFLAGS="-L${prefix}/Externals/lib -Wl,-R${prefix}/Externals/lib"
   if (test "${CCTEMP}" = "icc") then  # zeus x86_64 with Intel compiler
      if (test "${IC}" = "") then
	  echo "Run 'use <compiler>' to set environment variable IC to the location of libimf.a, libirc.a"
	  exit 1
      fi
      export LDFLAGS="${LDFLAGS} -L${IC}/lib -limf -lirc"
   fi
fi
./configure ${config_opt} --prefix=${prefix}/${version} ${getaddrbug}
if (test $? -ne 0) then
    echo "Python configure failed."; exit 1;
fi

make 
if (test $? -ne 0) then
    echo "Python make failed."; exit 1;
fi

make install 
if (test $? -ne 0) then
    echo "Python install failed."; exit 1;
fi

#cd Tools/idle
#${prefix}/bin/python setup.py install
#if (test $? -ne 0) then
#    echo "Python idle install failed."; exit 1;
#fi
mkdir -p ${prefix}/Externals/share
if (test "${OS}" = "CYGWIN_NT-5.1" ) then
    ln -s /usr/share/tcl* ${prefix}/Externals/share ;
    ln -s /usr/share/tk* ${prefix}/Externals/share ;
fi
if (test "${OS}" = "CYGWIN_NT-6.0" ) then
    ln -s /usr/share/tcl* ${prefix}/Externals/share ;
    ln -s /usr/share/tk* ${prefix}/Externals/share ;
fi

${prefix}/${version}/bin/python -c "import Tkinter"
if (test $? -ne 0) then
    echo "Python Tkinter import failed."; exit 1;
fi
echo "Python built with Tkinter correctly." 
