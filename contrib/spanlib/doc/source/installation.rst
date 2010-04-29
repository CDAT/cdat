.. _install:

############
Installation
############

************
Requirements
************

Fortran library
===============

You need a F90 compiler to compile it, and `BLAS/LAPACK/LAPACK95 <http://www.netlib.org/lapack>`_
libraries compilated using this F90 compiler to be able to link with library.
If you are using the Intel fortran compiler, you may be able to link the library against
the `Math Kernel Library <http://www.intel.com/cd/software/products/asmo-na/eng/perflib/307757.htm>`_.

To run the F90 examples, you also need the F90 `netcdf library <http://www.unidata.ucar.edu/software/netcdf>`_.

Python module
=============

You need the fortran library, and the `CDAT <http://www-pcmdi.llnl.gov/software-portal/cdat>`_ package.

.. note::

	CDAT provides numerous useful libraries et utilities. One of them is the netcdf
	library, which can be used to run the F90 exemples.

********
Download
********

You can download the sources (tarball) of the package from the 
`Sourceforge repositories <http://sourceforge.net/project/showfiles.php?group_id=168272>`_ of the project.
Then, just type for example::

	tar xzf spanlib-2.0.tgz
	cd spanlib-2.0


****************************
Compilation and installation
****************************

Detailed instructions can be found in the :file:`INSTALL` file of the package.

First, compile it with::

	./configure
	make

Second, install it as root::

	sudo make install

Here is an example of :command:`./configure`::

	./configure --with-blas-lib=/usr/local/install/lapack-3.0/lib \
	--with-netcdf-lib=/usr/local/install/netcdf-3.6.1/lib \
	--with-netcdf-inc=/usr/local/install/netcdf-3.6.1/include \
	--prefix=$HOME --with-pythondir=$HOME/python FC=ifort

You can list all :command:`configure` options using::

	./configure --help

You can also list usefull :command:`make` targets with::

	.make  help


