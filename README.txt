Climate Data Analysis Tools (CDAT)

VERSION: 6.0.alpha

Don't forget to check http://cdat.sf.net for the latest news and tips

UCRL-CODE-2002-0021

Documentation is at http://cdat.sf.net
Support: cdat-discussion@lists.sf.net

Please see file Legal.htm for our open-source release license.

Release specific Notes are in file: ReleaseNotes.txt


ON ALL PLATFORM YOU MUST UNSET THE FOLLOWING ENVIRONEMENT VARIABLES 
unset PYTHONPATH
unset PYTHONHOME

ON MOST PLATFORMS YOU MUST SET ENVIRONMENT VARIABLE CC BEFORE BUILDING

--------------------------------------------------------------------------
-------------------   Platform-specific instructions  --------------------
--------------------------------------------------------------------------

            Required value of                  
Platform    environment variable CC            Notes
========================================================================
AIX          cc_r
Irix 6.5     cc         Use configuration option -c irix to get n32 X11
Linux 2.4.2             Builds as is with gcc. 
HP-UX 11     cc +z
MacOS                   Builds as is with gcc. You need to install ghostscript
			and g77 (via fink for example)
OSF1         cc
Solaris      cc -mt     Be sure you are set up for Sun compiler).
                        Also on solaris (SunOS 5.8 at least)
                        you'll need to make sure the following variable 
			are set correctly when building. You don't need to have 
			them set later on.
			LD_LIBRARY_PATH_BASE and LD_LIBRARY_PATH must point to 
			your cc C compiler and Fortran77 libraries (for contrib)

			On our test build platform we have:
			CC = 'cc -mt'
			LD_LIBRARY_PATH=/usr/ccs/lib:/usr/lib:/opt/SUNWspro/lib
			LD_LIBRARY_PATH_BASE=/usr/ccs/lib:/usr/lib

x86_64 (Linux)
             gcc -fPIC  On our test platform it is necessary to compile netCDF
	                with -fPIC. We also used:

			CXX = 'g++ -fPIC'

x86_64 (Linux,
  Intel compiler)
             icc -fPIC  Also: run 'use <compiler>'

	                CXX = 'icpc -fPIC'
			FC = 'ifort -fPIC'
			F90 = 'ifort -fPIC'
			

The way you would set the environment variable CC depends on the shell you
use. Example:

For csh: setenv CC value
For sh: set CC=value; export CC

If the value is more than one word you have to quote it, e.g., "cc -mt".

--------------------------------------------------------------------------
-------------------             DOWNLOADS              -------------------
--------------------------------------------------------------------------

------ Where should I install cdat? 
In what follows we use the notation [CDAT_INSTALL_DIRECTORY] to indicate the full path name
of the directory into which cdat is to be installed.

We require that [CDAT_INSTALL_DIRECTORY]'s parent directory must already exist
for safety.  You must have write permission in it. For distributions that 
will be used by others, /usr/local/cdat or the like would be appropriate.

/usr/local is a poor choice, as chances of collisions with other installations
and unintended consequences are too high and uninstalling becomes difficult.

Since 5.0 there will be 2 directories created:
[CDAT_INSTALL_DIRECTORY]
and a directory where CDAT's external dependencies will be created
[EXTERNALS] (default is: [CDAT_DIRECTORY]/Externals but can be controlled via configure argument: --with-externals

[CDAT_INSTALL_DIRECTORY] and [EXTERNALS] will be created if necessary. 
--------------------------------------------------------------------------
-----------------------      INSTALLING CDAT       -----------------------
--------------------------------------------------------------------------

to install CDAT by default simply do:
./configure --prefix=[CDAT_DIRECTORY] 
make

the ./configure line can have extra arguments:
--help : gives you detail on all options
principal optional arguments are:
--with-externals=[EXTERNALS]   : to point to another place where to build/search for external dependencies
--with-python=[PATH_TO_YOUR_PYTHON] : to use your python
                                      NOTE: CDAT's packages will still be built in prefix
                                            allowing you to use a python where you don't have write permission
------- To test:
./test_script [CDAT_INSTALL_DIRECTORY]

------- To use the Graphical User Interface:
Make sure environment variable HOME set.
[CDAT_INSTALL_DIRECTORY]/bin/vcdat   # starts GUI


CDAT will look for and install if necessary (minimum version required/version distributed) :

	  python 2.7.0 / 2.7.0
	  setuptools 0.6 / 0.6c11
	  numpy 1.2.1 / 1.4.1
	  tcl/tk : 8.5.4 / 8.5.8
	  QT : 4.6.2 / 4.6.3
	  sip : 4.10.2 / 4.10.5
	  PyQt : 4.7.3 / 4.7.4
          readline : 5.2 / 6.1
	  zlib: any / 1.2.5
	  termcap: any / 1.3.1
          freetype 9.7.3 / 2.3.12
          fontconfig 2.4.2 / 2.8.0
          pkg-co nfig 0.9.0 / 0.23.0
          cairo 1.8.8 / 1.8.10
          ffmpeg
          jpeg any/6b
          libpng 1.4.1 / 1.4.2
          libxml 2.6.27 / 2.7.6
          libxslt 1.1.22 / 1.1.26
          Ghostscript
	  libuuid /1.6.2
	  Udunits 2 /2.1.14
          HDF5 any / 1.8.4-patch1
          NetCDF4 4.1.0/ 4.1.1
          XGKS
          Pmw 1.3 / 1.3
          gifsicle any any/ 1.58
          pbmplus 
          cmake 2.4 / 2.8.0


----------------------------------------------------------------------------------------
-------------------------------- Installing CDAT itself --------------------------------
----------------------------------------------------------------------------------------

This asumed you KNOW you have all the external dependencies

~~~~~~~~  Step 0 (if desired): Create a Configuration File

-- SETUP: configuration file --
CDAT installation can be controlled with a configuration file. 
The installation begins by reading the file installation/standard.py.
Details about the options you can set are detailed in that file.

You can use an alternate configuration file (say myconfig.py) by using in step 1:
--configuration=myconfig 
. 
Example of configuration file:
The file installation/pcmdi.py is an example of a configuration file, 
in this case the configuration we use at PCMDI. 
Once a configuration has been made, it will not be redone in 
subsequent installs unless you use the --force option. 

-- RESULT: cdat_info.py --
The result of the configuration process is a module cdat_info.py. This module is
installed into Python and also left for viewing in installation/cdat_info.py.

-- NOTES --
Note that in creating your own configuration file, you can use the full power of 
Python. 

The install process is also governed by a "control file". The standard control
file is installation/control.py; installation/debug.py is used for the debug 
builds. You shouldn't have to change anything in these files. 

~~~~~~~~  Step 1: Run "install" 
[CDAT_INSTALL_DIRECTORY]/[CDAT_VERSION]/bin/python install.py [options] [control-files]

In the following step you must execute the Python into which you intend to
install CDAT. Either use a full path name or make sure that the desired 
python is the one being chosen by your path. To emphasize this we'll show
the command as a full pathname below. Please read Notes below before building.

A full usage description is given in file HELP.txt. It can also be viewed with
[CDAT_INSTALL_DIRECTORY]/[CDAT_VERSION]/bin/python install.py --help. 


Standard install (now includes contrib Packages):
[CDAT_INSTALL_DIRECTORY]/[CDAT_VERSION]/bin/python install.py 

Standard without contributed packages
[CDAT_INSTALL_DIRECTORY]/[CDAT_VERSION]/bin/python install.py --disable-contrib

To install after changing the configuration, downloading a new version,
or changing platforms, use the --force option. For example:
[CDAT_INSTALL_DIRECTORY]/[CDAT_VERSION]/bin/python install.py --force

To install using your own configuration file:
[CDAT_INSTALL_DIRECTORY]/[CDAT_VERSION]/bin/python install.py --configuration=myconfig
where myconfig is your configuration file.

A list of all Packages installed will appear
If a Package fail an error message will be generated
At the end of the install, all failed Packages will be listed.

If the build has failed for some Packages, you should then examine the log 
for the piece it was working on when it failed.
For comparison the subdirectory "logs/samples" contains the log files for 
a build on Linux.  

A certain class of errors, such as a failure to locate the X11 libraries, 
can result in the build halting in an inconsistent state. Typically you will 
fix such a problem by adding a configuration or control file and rebuilding.
Be sure to use the --force option when you rebuild to insure a complete rebuild.

The set of packages to build is determined by the variable "packages."
To modify the list of packages, make a control file that modifies the variable
"packages"; use installation/pcmdi.py as a model. 

Many of the contributed packages in subdirectory contrib require a Fortran 
compiler. If the default compiler chosen by Pyfort is not what you want, you
may need to edit file configuration.py in the Pyfort source and reinstall it.
See http://pyfortran.sf.net.

------------------------------------------------------------------------------------
	Contrib Packages
------------------------------------------------------------------------------------
Bellow is the list of distributed contrib packages, documentation can be found
in the source (or online help)

asciidata
binaryio
ComparisonStatistics		1.2
cssgrid
dsgrid
eof
f2py				2.45.241_1926
grads
IaGraph				0.3.0.1
lmoments
natgrid
ort
pyclimate			1.2.1
pyIoapi				2005/09/21
ioapiTools                      2005/07/29
egenix-mx-base			2.0.6
pygmt				2004/05/03
pyncl				1.4.1
regridpack
Rpy 				0.4.6
shgrid
Scientific Python		2.4.9
SciPy				core-0.3.2
spherepack
trends

------------------------------------------------------------------------------------
----------------------------  Packages Specific Notes   ----------------------------
------------------------------------------------------------------------------------

...............................................
............... OpenDAP / DODS  ...............
...............................................
OpenDAP is now part of NetCDF4 and is turned on when building CDAT
...............................................
.................... GPLOT ....................
................. (OBSOLETE) ..................
...............................................
The gplot utility was required in the past to produce 
postscript. It is still distributed for users that wished to use it for
other purposes but it is not built and will not be distributed in the future. 
If you wish to install it follow the instructions bellow
Since it virtually never changes you may wish to install it somewhere 
in your path that is not the same as where you put cdat.

Choose an appropriate Makefile in subdirectory gplot.
Modification of the Makefile may be required for finding X11.

cd exsrc/src
gunzip gplot.tar.gz ; tar xvf gplot.tar
cd gplot
make -f Makefile.your_platform gplot

You can then install the executable "gplot" anywhere in your execution path.
For example, cp gplot [CDAT_INSTALL_DIRECTORY]/bin/gplot

...............................................
................. VTK Package  ................
............... (discontinued) ................
...............................................

VTK is not distributed anymore

...............................................
.............. SCRIP Interpolation ............
...............................................

Since CDAT Version 4 we include support for the SCRIP interpolation package
developed at Los Alamos National Laboratory. SCRIP interpolates gridded
data, and can be used with the nonrectangular grids introduced in CDAT
V4. Because the package is standalone and is written in Fortran 90, it is
not built by default. Note that CDAT has a built-in regridder for
rectangular grids. If you need the richer functionality of SCRIP, the
package is included in exsrc/src. See the SCRIP user guide for installation
instructions.

------------------------------------------------------------------------------------
-------------------------------------- Testing -------------------------------------
------------------------------------------------------------------------------------
Testing is NOT working at the moment

    [CDAT_INSTALL_DIRECTORY]/bin/python test_cdat.py -v3 -P 

more help with:
    [CDAT_INSTALL_DIRECTORY]/bin/python test_cdat.py --help

will run a basic set of tests.

------------------------------------------------------------------------------------
------------------------------------- Cleaning -------------------------------------
------------------------------------------------------------------------------------

For building again on a different system, or after downloading a new version,
or just to save disk space, run 
make clean


------------------------------------------------------------------------------------
--------------------------------- NOTES and REMARKS --------------------------------
------------------------------------------------------------------------------------


~~~~~~~~~~~~  HOME  ~~~~~~~~~~~
You must have your HOME environment variable set to a directory where you 
have write privleges in order for parts of CDAT to function correctly.
A subdirectory PCMDI_GRAPHICS is created in $HOME to hold some state 
between executions, and to hold certain graphical outputs.

~~~~~~~~~ Generalities ~~~~~~~~~
1. Warning messages differ from machine to machine but there will be some in 
   many of the packages, especially the ones we are including here but which 
   are written and maintained elsewhere. Do not panic. If you get to the 
   message 'Installation complete' at the end, you probably made it ok.

2. If you get missing externals that look like part of the Fortran runtime
   it may be that the the configuration for the Fortran compiler is wrong. 
   Be sure to set a Pyfort compiler id that matches the Fortran compiler 
   used for building DRS or other Fortran libraries. A basic install does not
   use Fortran so be sure you can do that first before trying to solve 
   this problem. See http://pyfortran.sf.net for details on Fortran compiler 
   ids in Pyfort.

3. If test_script fails importing cdtime, this usually indicates that the 
   libcdms build did not succeed. While the build should have exited 
   when that happens, it might not have. Check the log logs/LOG.libcdms.

4. If the CDMS libray build fails in compiling file cdunifql.c, 
   configuration variable CDMS_INCLUDE_QL must be "no". 
   This is the default except for the --PCMDI option.

~~~~~~~~~ Notes for Advanced Users ~~~~~~~~~
After your install, a file rebuild.py will appear in each package. You 
can execute python rebuild.py in a package to rebuild using the same 
installation target (that is, the same python, even if you accidentally
used the 'wrong' one to execute rebuild.py) and the same configuration. 

If you built originally using --debug, using
rebuild.py will ensure that you do so again. Rebuilds are logged into files
LOG.rebuild for stdout; stderr comes to your terminal. 

In the packages, you can 'clean' simply by removing the subdirectory
build with /bin/rm -fr build.  If present, such as in cdms, use ./clean_script.

Each package name placed in the list 'packages' can be one of the following:
   a. The name of a directory, either absolute or relative. In that directory,
      we look for a file setup.py, install_script, makefile, 
      Makefile, or MAKEFILE. The first one we find is used.
   b. The name of the file itself (absolute or relative, must be one of the 
      names listed above).

The action taken depends on the configuration variable, action. This is 
a dictionary mapping installation file names to execution lines.
If you wish to override these we suggest using your own configuration
file to do so; installation/debug.py is an example of this.

Congratulations, you read this document all the way to the end! You're
probably the first person EVER to do so! You should feel special! Have fun
using CDAT.

