#!/usr/bin/env python
import sys

id2fc =  (	('gnu95',	('gfortran')),
		('intel',	('ifort','ifc')),
		('intelv',	('ifl')),
		('intele',	('efort','efc','ifort')),
		('intelev',	('efl')),
		('pg',		('pgf90')),
		('lahey',	('lf95')),
		('ibm',	('xlf90')),
		('compaqv',	('DF')),
		('nag',	('f95')),
		('compaq',	('fort','f90')),
		('absoft',	('f90')),
		('mips',	('f90')),
		('sun',	('f90')),
		('vast',	('f90')),
		('hpux',	('f90'))
 )

myId = ""
for id,fcs in id2fc:
	if sys.argv[1] in fcs:
		myId = id
		break

print myId	

#import scipy_distutils.fcompiler
#import sys
#if sys.argv[1].lower in scipy_distutils.fcompiler.fcompiler_class.keys():
#	sys.exit(0)
#else:
#	sys.exit(1)

