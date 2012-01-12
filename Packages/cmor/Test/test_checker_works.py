####
# This is trying to test all that cmor_checker catches all the possible failures
###

import sys
import cmor
import shutil
import os
import cdms2
cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

fo = sys.stdout

if fo != sys.stdout:
    fo=open(fo,"w")
    
# First of all run the Test script that generates the "good" file
#execfile("Test/test_python_joerg_3.py")

file = 'Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc'


cmor.checkCMOR(fo,file,"Tables/CMIP5_6hrLev")


## #Ok at this point we are going to test failures to make sure it actually checks for each things
## # Ok testing the DRS

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/huhs/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmfos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/16hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolfdc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10b/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTEd_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIP5/outputt/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="Test/CMIpP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)


## try:
##     F="Test/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     try:
##         os.makedirs(os.path.split(F)[0])
##     except:
##         pass
##     shutil.copy(file,F)
##     print 'Testing DRS'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## #The following should pass, DRS test turned off
## F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
## try:
##     os.makedirs(os.path.split(F)[0])
## except:
##     pass
## shutil.copy(file,F)
## print 'No Testing DRS'
## cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
## os.remove(F)

## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_20f10010100-2010010218.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_20100g10100-2010010218.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010g100-2010010218.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_201001010g0-2010010218.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2f01001041 8.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010h010218.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010h418.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     F="Test/CMIP5/output/INSTITUTE_ID/pcmdi-10a/noVolc2000/6hr/atmos/hus/r1i1p1/hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-201001041g8.nc"
##     shutil.copy(file,F)
##     print 'Testing DRS wrong date'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev")
##     raise "DRS Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     f=cdms2.open(F,"a")
##     h=f("hus")
##     f.write(h,id='hur')
##     f.close()
##     print 'Testing many vars'
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise "Too man vars Check failed!"
## except cmor.check_CMOR_compliant.CMORError,err:
##     print 'Failed with CMOR error as expected:',err
##     pass
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## gbl = ['branch_time','contact','Conventions','creation_date','experiment','experiment_id','forcing','frequency','initialization_method','institute_id','institution','model_id','modeling_realm','parent_experiment_id','physics_version','product','project_id','realization','source','table_id','tracking_id']
## for gatt in gbl:
##     try:
##         print 'Testing no %s global att' % (gatt)
##         F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##         shutil.copy(file,F)
##         os.popen("ncdump %s > crap.txt"%F).readlines()
##         fi=open("crap.txt")
##         f=open("crapo.txt","w")
##         fglb=False
##         for l in fi.xreadlines():
##             if l.find("// global attributes")>-1:
##                 fglb = True
##             if fglb is False:
##                 f.write(l)
##             elif l.find(":%s" % gatt)==-1:
##                 f.write(l)
##         f.close()
##         fi.close()
##         os.remove(F)
##         os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##         os.remove("crap.txt")
##         os.remove("crapo.txt")
##         cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##         raise Exception,"No global attribute %s test failed" % (gatt)
##     except cmor.check_CMOR_compliant.CMORError,err:
##         if str(err).find("File must have global attribute: %s" % (gatt))>-1:
##             print 'Failed with CMOR error as expected:',err
##             pass
##         else:
##             raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
##     except Exception,err:
##         raise Exception,err
##     os.remove(F)

# test validity of Creation Time
## try:
##     gatt = "creation_date"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-3]+'blabla" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     print 'Did we get here?'
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("Creation Date must be in format: %Y-%m-%dT%H:%M:%SZ yours is")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "branch_time"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-4]+'"blabla" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("branch_time must be convertible to float, you have")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "branch_time"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-4]+'1. ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("if global attribute parent_experiment_id is N/A then branch_time must be 0., you have")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "experiment_id"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-3]+'x" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("experiment_id file attribute must be one of")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "experiment"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-3]+'x" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("experiment file attribute must be one of")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "forcing"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-3]+'x" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("file attribute forcing must be a comma separated list with values in")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "frequency"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-6]+'moyn" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("frequency must be one")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "frequency"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-6]+'mon" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("your file name indicates a frequency of ")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "parent_experiment_id"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-3]+'bla" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("parent_experiment_id file attribute must be one of")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "parent_experiment_id"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-6]+'noVolc2000" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("parent_experiment_id and experiment_id cannot be the same")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     gatt = "project_id"
##     print 'Testing Validity of %s' % (gatt)
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     fglb=False
##     for l in fi.xreadlines():
##         if l.find("// global attributes")>-1:
##             fglb = True
##         if fglb is False:
##             f.write(l)
##         elif l.find(":%s" % gatt)==-1:
##             f.write(l)
##         else:
##             f.write(l.strip()[:-3]+'0" ;\n')
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong global attribute %s test failed" % (gatt)
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("project_id must be ")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of global attribute %s failed for the wrong reason! %s" % (gatt,err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## try:
##     print 'Testing Validity of variable type'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for l in fi.xreadlines():
##         if l.find("float hus(time")>-1:
##             f.write(l.replace("float","double"))
##             fglb = True
##         else:
##             f.write(l)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong variable type test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("variable typecode must be ")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing Validity of coordinate type'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for l in fi.xreadlines():
##         if l.find("double lon(")>-1:
##             f.write(l.replace("double","float"))
##             fglb = True
##         else:
##             f.write(l)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong coord type test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("required typecode")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing Validity of coordinate type'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for l in fi.xreadlines():
##         if l.find("double ap(")>-1:
##             f.write(l.replace("double","float"))
##             fglb = True
##         else:
##             f.write(l)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong coord type test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("variable typecode")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing Validity of coordinate name'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for l in fi.xreadlines():
##         if l.find("lat")>-1:
##             if l.find('"latitude"')>-1:
##                 f.write(l.replace("lat","latitude",1))
##             else:
##                 f.write(l.replace("lat","latitude"))                
##         else:
##             f.write(l)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong coord name test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("Axis name latitude is not valid")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing Validity of coordinate units'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for l in fi.xreadlines():
##         if l.find("lev:units")>-1:
##             f.write(l.replace("1","%"))                
##         else:
##             f.write(l)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong coord units test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find("are not the required units")>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     import cdms2
##     print 'Testing Validity of time units'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     f=cdms2.open(F,"r+")
##     t=f['hus'].getTime()
##     t.toRelativeTime("seconds since 2010")
##     f.close()
##     f=open("Tables/CMIP5_6hrLev")
##     fot=open("tmp_table.txt","w")
##     for ln in f.xreadlines():
##         if ln.find("approx_in")>-1:
##             fot.write(ln.replace("0.25","21600."))
##         else:
##             fot.write(ln)
##     fot.close()
##     cmor.checkCMOR(fo,F,"tmp_table.txt",dodrs=False)
##     os.remove("tmp_table.txt")
##     raise Exception,"wrong time units test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('Time units must be in "days since')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)


## data = """time_bnds =
##   -0.125, 0.125,
##   0.125, 0.375,
##   0.375, 0.625,
##   0.625, 0.875,
##   0.875, 1.125,
##   1.125, 1.375,
##   1.375, 1.625,
##   1.625, 1.875 ;"""
## data2 = """time_bnds =
##   -0.1251, 0.1251,
##   0.1251, 0.3751,
##   0.3751, 0.6251,
##   0.6251, 0.8751,
##   0.8751, 1.1251,
##   1.1251, 1.3751,
##   1.3751, 1.6251,
##   1.6251, 1.8751 ;"""
## try:
##     import cdms2
##     print 'Testing time is mean of bounds'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     f.write(fi.read().replace(data,data2))
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"time is avg of bounds test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('Time units must be in "days since')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## data = """ time_bnds =
##   -0.125, 0.125,
##   0.125, 0.375,
##   0.375, 0.625,
##   0.625, 0.875,
##   0.875, 1.125,
##   1.125, 1.375,
##   1.375, 1.625,
##   1.625, 1.875 ;"""
## try:
##     import cdms2
##     print 'Testing lack of bounds'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     s=fi.read()
##     fi.close()
##     fi=open("crap.txt",'w')
##     fi.write(s.replace(data,""))
##     fi.close()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("time:bounds")>-1:
##             continue
##         if ln.find("double time_bnds")>-1:
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"lack of bounds test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('attribute bounds is required for axis')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## data = """ time_bnds =
##   -0.125, 0.125,
##   0.125, 0.375,
##   0.375, 0.625,
##   0.625, 0.875,
##   0.875, 1.125,
##   1.125, 1.375,
##   1.375, 1.625,
##   1.625, 1.875 ;"""
## try:
##     import cdms2
##     print 'Testing lack of bounds'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     s=fi.read()
##     fi.close()
##     fi=open("crap.txt",'w')
##     fi.write(s.replace(data,""))
##     fi.close()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("time:bounds")>-1:
##             continue
##         if ln.find("double time_bnds")>-1:
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"lack of bounds test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('attribute bounds is required for axis')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## data="""double ap(lev) ;"""
## n=567
## data=data[:15]
## data2=""" ap = 0.1, 0.2, 0.3, 0.22, 0.1 ;

##  b = 0, 0.1, 0.2, 0.5, 0.8 ;

##  ps ="""

## data3=""" ap_bnds =
##   0, 0.15,
##   0.15, 0.25,
##   0.25, 0.25,
##   0.25, 0.16,
##   0.16, 0 ;

##  b_bnds =
##   0, 0.05,
##   0.05, 0.15,
##   0.15, 0.35,
##   0.35, 0.65,
##   0.65, 1 ;
## """
## try:
##     import cdms2
##     print 'Testing lack of formulaterms'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     s=fi.read()
##     fi.close()
##     print 'orig:',len(s)
##     i=s.find(data)
##     print 'i,n:',i,n
##     s=s[:i]+s[i+n:]
##     s=s.replace(data2,"")
##     s=s.replace(data3,"")
##     print 'end:',len(s)
##     fi=open("crap.txt",'w')
##     fi.write(s)
##     fi.close()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("formula")>-1:
##             continue
##         if ln.find("1013")>-1:
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"lack of formula_terms test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('attribute formula_terms is required for axis')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong names on zfactors axis for hybrid'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("ap")>-1:
##             tmp=ln.replace("ap","ap2")
##             if tmp.find("formula_term")>-1: tmp=tmp.replace("ap2:","ap:")
##             tmp=tmp.replace("ap2_bnds","ap_bnds")
##             f.write(tmp)
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"lack of formula_terms test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('formula should be')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong names on stabdard_name axis for hybrid'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         ln=ln.replace("atmosphere_hybrid_sigma_pressure_coordinate","atmosphere_ln_pressure_coordinate")
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"lack of formula_terms test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('axis attribute formula should be')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## for anm in ['lon','lat','lev','time']:
    ## try:
    ##     print 'Testing axis attribute'
    ##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
    ##     shutil.copy(file,F)
    ##     os.popen("ncdump %s > crap.txt"%F).readlines()
    ##     fi=open("crap.txt")
    ##     f=open("crapo.txt","w")
    ##     for ln in fi.xreadlines():
    ##         if ln.find("%s:axis"%anm)>-1:
    ##             continue
    ##         f.write(ln)
    ##     f.close()
    ##     fi.close()
    ##     os.remove(F)
    ##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
    ##     os.remove("crap.txt")
    ##     os.remove("crapo.txt")
    ##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
    ##     raise Exception,"no axis attribute test failed"
    ## except cmor.check_CMOR_compliant.CMORError,err:
    ##     if str(err).find('axis must have associated axis attribute')>-1:
    ##         print 'Failed with CMOR error as expected:',err
    ##         pass
    ##     else:
    ##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
    ## except Exception,err:
    ##     raise Exception,err
    ## os.remove(F)
## try:
##     print 'Testing calendar attribute'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("time:calendar")>-1:
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"no calendar attribute test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('calendar attribute must be defined')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong calendar attribute'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("time:calendar")>-1:
##             f.write('time:calendar = "mayan" ;')
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"no calendar attribute test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('calendar must be one of')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong variable datatype'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("float hus")>-1:
##             f.write(ln.replace("float hus","double hus"))
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var typecode test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('variable typecode must be')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong variable name'
##     F="HUS_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus")>-1:
##             f.write(ln.replace("hus","HUS"))
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var name test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('does not start with standard CMIP5 variable name ')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong variable name'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus")>-1:
##             f.write(ln.replace("hus","tas"))
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var name test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('Your file name says it contains variable')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong variable units'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus:units")>-1:
##             f.write('                hus:units = "%" ;\n')
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var name test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('do not match IPCC units')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong dim ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("float hus(time, lev, lat, lon)")>-1:
##             f.write(ln.replace("lat, lon","lon, lat"))
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('in ordering for dimension')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)

## data = """ lon = 2, 6, 10, 14, 18, 22, 26, 30, 34, 38, 42, 46, 50, 54, 58, 62, 66, 70, 
##     74, 78, 82, 86, 90, 94, 98, 102, 106, 110, 114, 118, 122, 126, 130, 134, 
##     138, 142, 146, 150, 154, 158, 162, 166, 170, 174, 178, 182, 186, 190, 
##     194, 198, 202, 206, 210, 214, 218, 222, 226, 230, 234, 238, 242, 246, 
##     250, 254, 258, 262, 266, 270, 274, 278, 282, 286, 290, 294, 298, 302, 
##     306, 310, 314, 318, 322, 326, 330, 334, 338, 342, 346, 350, 354, 358 ;"""
## datanew = """ lon = """+"""358, 354, 350, 346, 342, 338, 334, 330, 326, 322, 318, 314, 310, 306, 302, 298, 294, 290, 286, 282, 278, 274, 270, 266, 262, 258, 254, 250, 246, 242, 238, 234, 230, 226, 222, 218, 214, 210, 206, 202, 198, 194, 190, 186, 182, 178, 174, 170, 166, 162, 158, 154, 150, 146, 142, 138, 134, 130, 126, 122, 118, 114, 110, 106, 102, 98, 94, 90, 86, 82, 78, 74, 70, 66, 62, 58, 54, 50, 46, 42, 38, 34, 30, 26, 22, 18, 14, 10, 6, 2 ;"""
## try:
##     print 'Testing wrong dim ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     s=fi.read()
##     fi.close()
##     fi=open("crap.txt","w")
##     fi.write(s.replace(data,datanew))
##     fi.close()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('axis values for lon must be stored:increasingly')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong dim ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("lon = 2, 6, 10"):
##             f.write(ln.replace("lon = 2, 6, 10","lon = -2, 6, 10"))
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('first longitude must be >= 0 degrees_east')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## data = """ lat = -88, -84, -80, -76, -72, -68, -64, -60, -56, -52, -48, -44, -40, -36, 
##     -32, -28, -24, -20, -16, -12, -8, -4, 0, 4, 8, 12, 16, 20, 24, 28, 32, 
##     36, 40, 44, 48, 52, 56, 60, 64, 68, 72, 76, 80, 84, 88 ;"""
## datanew = """ lat = """+"""88, 84, 80, 76, 72, 68, 64, 60, 56, 52, 48, 44, 40, 36, 
##     32, 28, 24, 20, 16, 12, 8, 4, 0, -4, -8, -12, -16, -20, -24, -28, -32, 
##     -36, -40, -44, -48, -52, -56, -60, -64, -68, -72, -76, -80, -84, -88 ;"""
## try:
##     print 'Testing wrong dim ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     s=fi.read()
##     fi.close()
##     fi=open("crap.txt","w")
##     fi.write(s.replace(data,datanew))
##     fi.close()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('axis values for lat must be stored:increasingly')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## data = """ time = 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75 ;"""
## datanew = """ time = 0, -0.25, -0.5, -0.75, -1, -1.25, -1.5, -1.75 ;"""

## try:
##     print 'Testing wrong dim ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2009123018.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     s=fi.read()
##     fi.close()
##     fi=open("crap.txt","w")
##     fi.write(s.replace(data,datanew))
##     fi.close()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('axis values for time must be stored:increasingly')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing fill and miss values different ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus:mis")>-1:
##             f.write("                hus:missing_value = 1.e+30f ;\n")
##             continue
##         elif ln.find("hus:_F")>-1:
##             f.write("                hus:_FillValue = 1.e+20f ;\n")
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('missing_value and _FillValue attributes are different')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing fill and miss values different ordering'
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus:mis")>-1:
##             f.write("                hus:missing_value = 1.e+30f ;\n")
##             continue
##         elif ln.find("hus:_F")>-1:
##             f.write("                hus:_FillValue = 1.e+30f ;\n")
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"wrong var ordering test failed"
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('missing_value and _FillValue must be set to 1.e20f')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## for att in ['associated_files','cell_measures','long_name','standard_name','units']:
##     try:
##         print 'Testing required var att %s' % att
##         F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##         shutil.copy(file,F)
##         os.popen("ncdump %s > crap.txt"%F).readlines()
##         fi=open("crap.txt")
##         f=open("crapo.txt","w")
##         for ln in fi.xreadlines():
##             if ln.find("hus:%s"%att)>-1:
##                 continue
##             f.write(ln)
##         f.close()
##         fi.close()
##         os.remove(F)
##         os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##         os.remove("crap.txt")
##         os.remove("crapo.txt")
##         cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##         raise Exception,"missing var att %s test failed" % att
##     except cmor.check_CMOR_compliant.CMORError,err:
##         if str(err).find('Attribute %s is required but not set for var hus' % att)>-1:
##             print 'Failed with CMOR error as expected:',err
##             pass
##         else:
##             raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
##     except Exception,err:
##         raise Exception,err
##     os.remove(F)
## att = 'associated_files'
## try:
##     print 'Testing wrong var att %s' % att
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus:%s"%att)>-1:
##             f.write('   hus:%s = "blabla" ; \n' % att)
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"missing var att %s test failed" % att
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('associated_files attributes must contain a baseURL')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
## try:
##     print 'Testing wrong var att %s' % att
##     F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
##     shutil.copy(file,F)
##     os.popen("ncdump %s > crap.txt"%F).readlines()
##     fi=open("crap.txt")
##     f=open("crapo.txt","w")
##     for ln in fi.xreadlines():
##         if ln.find("hus:%s"%att)>-1:
##             f.write(' hus:associated_files = "baseURL: http://cmip-pcmdi.llnl.gov/CMIP5/dataLocation gridspecFile: gridspec_fx_pcmdi-10a_noVolc2000_r0i0p0.nc cellAreaFile: areacellla_fx_pcmdi-10a_noVolc2000_r0i0p0.nc" ;')
##             continue
##         f.write(ln)
##     f.close()
##     fi.close()
##     os.remove(F)
##     os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
##     os.remove("crap.txt")
##     os.remove("crapo.txt")
##     cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
##     raise Exception,"missing var att %s test failed" % att
## except cmor.check_CMOR_compliant.CMORError,err:
##     if str(err).find('associated files should point to file containing: areacella')>-1:
##         print 'Failed with CMOR error as expected:',err
##         pass
##     else:
##         raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
## except Exception,err:
##     raise Exception,err
## os.remove(F)
att="cell_measures"
try:
    print 'Testing wrong var att %s' % att
    F="hus_6hrLev_pcmdi-10a_noVolc2000_r1i1p1_2010010100-2010010218.nc"
    shutil.copy(file,F)
    os.popen("ncdump %s > crap.txt"%F).readlines()
    fi=open("crap.txt")
    f=open("crapo.txt","w")
    for ln in fi.xreadlines():
        if ln.find("hus:%s"%att)>-1:
            f.write(' hus:%s = "area: cl" ;' % att)
            continue
        f.write(ln)
    f.close()
    fi.close()
    os.remove(F)
    os.popen("ncgen -b -o %s crapo.txt" % (F)).readlines()
    os.remove("crap.txt")
    os.remove("crapo.txt")
    cmor.checkCMOR(fo,F,"Tables/CMIP5_6hrLev",dodrs=False)
    raise Exception,"missing var att %s test failed" % att
except cmor.check_CMOR_compliant.CMORError,err:
    if str(err).find('variable attribute cell_measures should be')>-1:
        print 'Failed with CMOR error as expected:',err
        pass
    else:
        raise Exception,"Checker of variable type failed for the wrong reason! %s" % (err)
except Exception,err:
    raise Exception,err
os.remove(F)





