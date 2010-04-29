######################################################################
## SpanLib, Raynaud 2006
######################################################################

if WITH_EXAMPLE
noinst_PROGRAMS = example1 example2 example3
example1_SOURCES = example1.f90
example2_SOURCES = example2.f90
example3_SOURCES = example3.f90
endif

LOCAL_LIB_F90=$(top_srcdir)/src/libspanlib.a
LOCAL_LIB_PY=$(top_srcdir)/src/build/tmp_lib/spanlib/spanlib_fort.so
SRC_F90=$(top_srcdir)/src/spanlib.f90
SRC_PY=$(SRC_F90) $(top_srcdir)/lib/spanlib_python.py $(top_srcdir)/src/spanlib_pywrap.f90
AM_FCFLAGS = -I$(top_srcdir)/src $(NETCDF_INC) $(LAPACK95_INC)
AM_LDFLAGS = -L$(top_srcdir)/src $(NETCDF_LIB) $(LAPACK95_LIB) $(LAPACK_LIB) $(BLAS_LIB)
LDADD = -lnetcdf $(LOCAL_LIB_F90) $(LAPACK95) $(LAPACK) $(BLAS)
URL = http://stefdeperou.free.fr/pub
NCINPUT = data2.cdf

######################################################################
define variables_fortran1
echo "# - orig:  original field (before its analysis)"; \
echo "# - reco1: reconstruction of the first pair of mssa modes"; \
echo "# - pair1: phases composites of the first pair"
endef
define variables_fortran2
echo "# - sst_box1/2:    left/right original fields"; \
echo "# - eof_box1/2:    left/right SVD EOFs"; \
echo "# - pc_box1/2:     left/right SVD PCs"; \
echo "# - sstrec_box1/2: left/right reconstructed fields"; \
echo "# - ev:            eigen values"
endef
define variables_fortran3
echo "# - sst_box1/2:      left/right original fields"; \
echo "# - sst_pca_box1/2:  left/right SST from pre-PCA"; \
echo "# - sst_model_box2:  right SST from SVD model"
endef

######################################################################
if WITH_PYTHON
VARIABLE_python1 = pair1
VARIABLE_python2 = eof_box1
VARIABLE_python3 = sst_model_box2
endif

######################################################################
EXTRA_DIST = example1.py example2.py
CLEANFILES = output_*.nc

FORTRANS=$(subst example,fortran,$(noinst_PROGRAMS) )
PYTHONS=$(patsubst example%.py,python%,$(EXTRA_DIST) )

######################################################################
######################################################################
if HAS_NCVIEWER
define fortran_view
@test -f $(word 3,$^) &&  ( \
echo "###########################################################" ; \
echo "# Visualisation of output file $(word 3,$^)"; \
echo "# This file contains the following variables of interest:" ;\
$(variables_$@) ;\
echo "###########################################################" )
test -f $(word 3,$^) && $(NCVIEWER) $(NCVIEWER_ARGS_$@)
endef
else
define fortran_view
@echo "###########################################################"
@echo "# Results are stored in the following netcdf file:"
@echo "# $(word 3,$^)"
@echo "###########################################################"
endef
endif
## gmake > 3.80
##define fortran_view_rule
##$(1) : $(NCINPUT) $(LOCAL_LIB) output_$(1).nc ; $$(call fortran_view,$(1),output_$(1).nc)
##endef

##################
## common config ##
###################

$(NCINPUT):
if HAS_DOWNLOADER
	@echo "###########################################################"
	@echo "# Download of input netcdf file:"
	@echo "# $(URL)/$@"
	@echo "###########################################################"
if USE_LINKS
	$(DOWNLOADER) -source 1 $(URL)/$@ > $<
else
	$(DOWNLOADER) $(URL)/$@
endif
else
	@echo "###########################################################"
	@echo "# You must first download the netcdf data file at url:"
	@echo "# $(URL)/$(NCINPUT)"
	@echo "###########################################################"
endif

#################
## F90 example ##
#################
if WITH_EXAMPLE
all-local: fortran1
else
all-local:
	@echo "###########################################################"
	@echo "# Your configuration has no f90 netcdf support so you can't"
	@echo "# run the example. If you wish to do it, use ./configure"
	@echo "# options --with-netcdf-inc and --with-netcdf-lib or"
	@echo "# or associated variables."
	@echo "###########################################################"
endif

## gmake > 3.80
##$(foreach fid,$(FORTRANS),$(eval $(call fortran_view_rule,$(fid))))
fortran1: $(NCINPUT) $(LOCAL_LIB_F90) output_fortran1.nc ; $(call fortran_view)
fortran2: $(NCINPUT) $(LOCAL_LIB_F90) output_fortran2.nc ; $(call fortran_view)
fortran3: $(NCINPUT) $(LOCAL_LIB_F90) output_fortran3.nc ; $(call fortran_view)

output_fortran%.nc: example%
	@rm -f $@
	@echo "###########################################################"
	@echo "# Running the F90 $^ program..."
	@echo "###########################################################"
	-./$^
	-@test -f $@ || echo "No netcdf file created"

$(LOCAL_LIB_F90): $(SRC_F90)
	cd $(top_srcdir)/src && make


####################
## Python example ##
####################

if WITH_PYTHON
$(PYTHONS): $(NCINPUT) $(LOCAL_LIB_F90) $(LOCAL_LIB_PY)
	@echo "###########################################################"
	@echo "# Running the python example `echo $@ |cut -c7` program..."
	@echo "###########################################################"
	$(PYTHON) example`echo $@ |cut -c7`.py
$(LOCAL_LIB_PY): $(SRC_PY)
	cd $(top_srcdir)/src && make
else
$(PYTHONS):
	@echo "###########################################################"
	@echo "# You can't run python examples with your configuration"
	@echo "###########################################################"
endif



