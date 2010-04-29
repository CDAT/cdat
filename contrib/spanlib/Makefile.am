######################################################################
## SpanLib, Raynaud 2006
######################################################################

EXTRA_DIST = COPYING INSTALL AUTHORS README ChangeLog
#SUBDIRS = src lib scripts doc
SUBDIRS = src lib scripts
DIST_SUBDIRS = $(SUBDIRS) example

.PHONY: example doc

doc: documentation

documentation:
	cd $(top_srcdir)/doc && make

example: fortran1

fortran1 fortran2 fortran3 python1 python2:
	cd $(top_srcdir)/example && make $@

clean-local:
	cd example && make clean

help:
	@echo "Typical usages of make"
	@echo "======================"
	@echo "make"
	@echo "make install"
	@echo "-"
	@echo "make example OR make fortran1"
	@echo "make fortran2"
	@echo "make fortran3"
	@echo "make python1"
	@echo "make python2"
	@echo "-"
	@echo "make clean"
	@echo "make uninstall"




