PROG=toto
PROG=example
FC=ifort
FCARGS=-132 -O -check bounds
INC=-I/home/users/raynaud/lib/lapack95_modules -I$(HOME)/include -I/usr/local/include
LIB=-L/usr/local/install/lapack-3.0/lib/ -L$(HOME)/lib -llapack -lblas -lpcamssa -llapack -lblas -lpcamssa -L/usr/local/lib -lnetcdf
OBJS=$(PROG).o
##URL=http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CMB/.GLOBAL/.Reyn_SmithOIv2/.monthly/.ssta/T/%28Jan%201982%29%28Dec%202005%29RANGEEDGES/Y/%2829.5S%29%2829.5N%29RANGEEDGES/X/%28130E%29%28285E%29RANGEEDGES/data.cdf
##URL=http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.EMC/.CMB/.GLOBAL/.Reyn_SmithOIv2/.monthly/.ssta/X/%28130.5E%29%2875.5W%29RANGEEDGES/T/%28Jan%201982%29%28Dec%202005%29RANGEEDGES/Y/%2829.5S%29%2829.5N%29RANGEEDGES/data.cdf
URL=http://stefdeperou.free.fr/pub/data.cdf
DATA=data.cdf

######################################################################

.SUFFIXES: .f90 .a .o .mod 

%.o: %.f90
	$(FC) $(FCARGS) $(INC) $(LIB) -c $< 

all: analysis

analysis: $(PROG) $(DATA)
	./$(PROG)

prog: $(PROG)

force-prog:
	touch $(PROG).f90
	make $(PROG)

$(PROG): $(OBJS)
	$(FC) $(FCARGS) $(INC) -o $@ $< $(LIB)

$(OBJS): $(MODS)

data: $(DATA)

$(DATA):
	wget -O $(DATA) '$(URL)'

clean:
	rm -f *.o *.mod

clear: clean
	rm -f $(PROG)








