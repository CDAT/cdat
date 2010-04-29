######################################################################
## SpanLib, Raynaud 2006-2007
######################################################################

lib_LIBRARIES = libspanlib.a
if UPPER_CASE_MOD
nodist_include_HEADERS=SPANLIB_LAPACK95.mod SPANLIB.mod
else
nodist_include_HEADERS=spanlib_lapack95.mod spanlib.mod
endif
nodist_libspanlib_a_SOURCES=spanlib_lapack95.f90
dist_libspanlib_a_SOURCES=spanlib.f90
CLEANFILES=*.pyf setup.cfg *.mod $(nodist_libspanlib_a_SOURCES)
AM_FCFLAGS=$(LAPACK95_INC) -fPIC
AM_F90FLAGS=$(AM_FCFLAGS) -fPIC

WRAPPER=spanlib_pywrap.f90
if WITH_PYTHON
PYTHON_LIBRARY_DIR = $(srcdir)/build/$(F2PY_BUILD_DIR)/spanlib
# PYTHON_LIBRARY = $(PYTHON_LIBRARY_DIR)/spanlib_fort.so
PYTHON_LIBRARY = $(PYTHON_LIBRARY_DIR)/spanlib_fort.so
endif

EXTRA_DIST=$(WRAPPER)

######################################################################
######################################################################

all-local: $(LIBRARIES) $(HEADERS) $(PYTHON_LIBRARY)

if UPPER_CASE_MOD
spanlib.o: SPANLIB_LAPACK95.mod
SPANLIB_LAPACK95.mod: spanlib_lapack95.o
else
spanlib.o: spanlib_lapack95.mod
spanlib_lapack95.mod: spanlib_lapack95.o
endif

# spanlib_lapack95.f90: check-lapack95


# check-lapack95: template.spanlib_lapack95.f90
spanlib_lapack95.f90: template.spanlib_lapack95.f90
	@echo "Updating lapack95 config..."
	@test -f spanlib_lapack95.f90  || cp $^ spanlib_lapack95.f90
	@if test "$(USE_MKL)" = "yes" ; then \
      rm -f spanlib_lapack95.f90 ; \
      sed -e "s/use f95_lapack/use mkl95_lapack/" \
          $^ > spanlib_lapack95.f90 ; \
      echo "Switched to MKL (intel) lapack95 config" ; \
   else  \
      rm -f spanlib_lapack95.f90 ; \
      sed "s/ => [_a-z]*//g" $^ > spanlib_lapack95.f90 ; \
      echo "Switched back to default lapack95 config" ; \
   fi
 	

if WITH_PYTHON
install-exec-hook:
if LOCAL_PYTHON_INSTALL
	$(PYTHON) setup.py install --install-lib $(PYTHONDIR)
else
	$(PYTHON) setup.py install
endif

uninstall-hook:
	@echo "Uninstalling $(PACKAGE) python module"
	-rm -rf $(PYTHONDIR)/spanlib*

clean-local:
	-test ! -f setup.py || $(PYTHON) setup.py clean
	-rm -rf build $(PACKAGE).pyf setup.py 

$(PYTHON_LIBRARY): $(HEADERS) setup.py setup.cfg $(PACKAGE).pyf $(WRAPPER) \
   $(LIBRARIES) $(top_srcdir)/lib/__init__.py $(top_srcdir)/lib/spanlib_python.py
	$(PYTHON) setup.py build --build-lib build/lib

$(PACKAGE).pyf: $(top_srcdir)/scripts/genpyf.py $(WRAPPER)
	@echo Creating $@
	$(top_srcdir)/scripts/genpyf.py $(WRAPPER) $@ 

setup.py: FORCE
	@test -f $@ || ( cp template.$@ $@ && echo "$@ created" )
	@version=`grep "AM_INIT_AUTOMAKE" $(top_srcdir)/configure.in | cut -d"," -f2 | cut -d")" -f1` ; \
	test "x$$version" = "x`grep version=\'.*\' $@ | cut -d\' -f2`" -a \
	"x$(F2PY_LIBS)" = "x`grep libs= $@ | cut -f1 | cut -d= -f2`" -a \
	"x$(F2PY_LIBDIRS)" = "x`grep libDirs= $@ | cut -f1 | cut -d= -f2`" -a \
	"x$(F2PY_INCDIRS)" = "x`grep incDirs= $@ | cut -f1 | cut -d= -f2`"|| \
	( rm -f $@ && sed \
	-e "s/_VERSION_/$$version/" \
	-e "s/_LIBS_/$(F2PY_LIBS)/" \
	-e "s=_LIBDIRS_=$(F2PY_LIBDIRS)=" \
	-e "s=_INCDIRS_=$(F2PY_INCDIRS)=" \
	template.$@ > $@  && \
	echo "$@ updated" )

setup.cfg: FORCE
	@test -f $@ || ( cp template.$@ $@ && echo "$@ created" )
	@test "x$(FC)" = "x`grep "^f90exec=" $@ | cut -f1 | cut -d= -f2`" -a \
	"x$(FC)" = "x`grep "^f77exec=" $@ | cut -f1 | cut -d= -f2`" || \
	( rm -f $@ && \
	sed -e "s/_FC_/$(FC)/" -e "s/_FCVENDOR_/$(F2PY_FC_VENDOR)/" template.$@ > $@  && \
	echo "$@ updated" )

##	sed "s/_FCVENDOR_/$(F2PY_FC_VENDOR)/" template.$@ > $@ 


FORCE:

endif

