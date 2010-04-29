TOPDIR=..
SRCDIR=$(TOPDIR)/src
PYDIR=../lib
INSTALLDIR=$(HOME)
INSTALLLIBDIR=$(INSTALLDIR)/lib
INSTALLINCDIR=$(INSTALLDIR)/include
FC=ifort
PYFORT=pyfort
AR=ar
GENPYF=../scripts/genpyf.pl
FCARGS=-132 -O -check bounds
LAPACKDIR=/usr/local/install/lapack-3.0/lib/
LIBS=-L$LAPACKDIR -llapack -lblas -llapack -lblas
LIB=libspanlib.a
OBJ=spanlib.o
MOD=spanlib.mod
WRAPPER=spanlib_pywrap.f90
PYF=spanlib.pyf
PFP=spanlib.pfp

######################################################################

.SUFFIXES: .f90 .a .o .mod 

##%.a: %.o
$(LIB): $(OBJ)
	$(AR) -r $@ $<
##	ld -o $@ $<

%.o: %.mod

%.o: %.f90
	$(FC) $(FCARGS) $(INC) $(LIBS) -c $<

all: install

lib: $(LIB) $(MOD)

install: $(LIB) $(MOD)
	install --mode=755 -d $(INSTALLLIBDIR) $(INSTALLINCDIR)
	install --mode=644 $(LIB) $(INSTALLLIBDIR)
	install --mode=644 $(MOD) $(INSTALLINCDIR)

archive: $(LIB)

pfp:
	@echo Creating $(PFP)
	@echo "pyf('"$(PYF)"',\
	sources=['"$(WRAPPER)"'],\
	libraries='spanlib blas lapack blas lapack',\
	library_directories='. "$(LAPACKDIR)"',\
	compiler_options='-I.',\
	python_directory='"$(PYDIR)"',\
	package_name='spanlib',\
	freeform=1)" > $(PFP)

$(PYF): $(WRAPPER)
	$(GENPYF) $(WRAPPER) $(PYF)

python: pfp $(WRAPPER) $(PYF) $(LIB) $(MOD)
	$(PYFORT) -i $(PFP)
	

clean:
	rm -f *.o *.a *.mod









