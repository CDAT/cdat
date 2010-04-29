OBJECTS = alg136.o \
cdfexp.o  cluagg.o	lmrglo.o  pelglo.o	quaglo.o  samlmr.o \
cdfgam.o  cluinf.o	lmrgno.o  pelgno.o	quagno.o  samlmu.o \
cdfgev.o  clukm.o	lmrgpa.o  pelgpa.o	quagpa.o  sampwm.o \
cdfglo.o  derf.o	lmrgum.o  pelgum.o	quagum.o  sort.o \
cdfgno.o  digamd.o	lmrkap.o  pelkap.o	quakap.o \
cdfgpa.o  dlgama.o	lmrnor.o  pelnor.o	quanor.o \
cdfgum.o  durand.o	lmrpe3.o  pelpe3.o	quape3.o \
cdfkap.o  gamind.o	lmrwak.o  pelwak.o	quastn.o \
cdfnor.o  lmrexp.o	pelexp.o  quaexp.o	quawak.o \
cdfpe3.o  lmrgam.o	pelgam.o  quagam.o	reglmr.o \
cdfwak.o  lmrgev.o	pelgev.o  quagev.o	regtst.o 

SOURCES = alg136.for \
cdfexp.for  cluagg.for	lmrglo.for  pelglo.for	quaglo.for  samlmr.for \
cdfgam.for  cluinf.for	lmrgno.for  pelgno.for	quagno.for  samlmu.for \
cdfgev.for  clukm.for	lmrgpa.for  pelgpa.for	quagpa.for  sampwm.for \
cdfglo.for  derf.for	lmrgum.for  pelgum.for	quagum.for  sort.for \
cdfgno.for  digamd.for	lmrkap.for  pelkap.for	quakap.for \
cdfgpa.for  dlgama.for	lmrnor.for  pelnor.for	quanor.for \
cdfgum.for  durand.for	lmrpe3.for  pelpe3.for	quape3.for \
cdfkap.for  gamind.for	lmrwak.for  pelwak.for	quastn.for \
cdfnor.for  lmrexp.for	pelexp.for  quaexp.for	quawak.for \
cdfpe3.for  lmrgam.for	pelgam.for  quagam.for	reglmr.for \
cdfwak.for  lmrgev.for	pelgev.for  quagev.for	regtst.for 


liblmoments.a: $(SOURCES)
	$(FORTRAN) -c $(SOURCES)
	ld -r -o liblmoments.a $(OBJECTS)
	/bin/rm -fr $(OBJECTS)


clean:
	/bin/rm -fr $(OBJECTS) liblmoments.a
