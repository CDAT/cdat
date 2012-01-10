from out_files import out
from in_files import input_tables
import sys,os

import cmor

test = sys.argv[1]

test = os.path.split(test)[1]
if test[-4:].lower()=='.f90':
    test=test[:-4]

print 'Checking results for:',test

outfiles = out.get(test,[])
intables = input_tables.get(test,['IPCC_test_table_A',])


class CMORResultCheckError(Exception):
    def __init__(self,args=None):
        self.args=args

nfiles = 0

print 'files:',outfiles
gotfiles=[]
missing=[]
for f in outfiles:
    if f is None:
        print 'No checking'
        sys.exit()
    tables=[]
    for t in intables:
        tables.append(os.path.join("Test",t))
    tbl = tables.pop(0)
    if len(tables)==0:
        tables=[None,]
    fnm = os.path.join("Test",f)
    if os.path.exists(fnm):
        nfiles+=1
        gotfiles.append(fnm)
        print 'Checking output file:',f
        cmor.checkCMOR(sys.stdout,fnm,tbl,other_tables=tables)
        print '----------------------- Success ------------------------------'
        os.remove(fnm)
    else:
        missing.append(fnm)
if nfiles == 0 and outfiles!=[]:
    raise CMORResultCheckError,["Error could not find any output file for test: Test/%s.f90" % (test),]
elif nfiles!=len(outfiles):
    raise CMORResultCheckError,["Error checking output files for test: Test/%s.f90 we could only find %i files when %i were expected.\n\n Expected files: \n\t%s\n\nPresent files: \n\t%s\n\nMissing files: \n\t%s\n" % (test,nfiles,len(outfiles),repr(outfiles),repr(gotfiles),repr(missing)),]
