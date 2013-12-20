import cdms2,os,sys,cdat_info
pth = os.path.dirname(os.path.abspath(__file__))

if cdat_info.CDMS_INCLUDE_DAP=='yes':
    print 'Testing Dap'
    f=cdms2.open('http://esgcet.llnl.gov/dap/ipcc4/20c3m/ncar_ccsm3_0/pcmdi.ipcc4.ncar_ccsm3_0.20c3m.run6.atm.mo.xml')
    print f.listvariables()
else:
    print 'CDMS not built with DAP support, skipped DAP test'

if cdat_info.CDMS_INCLUDE_PP=='yes':
    print 'Testing PP'
    f=cdms2.open(os.path.join(pth,'testpp.pp'))
    print f.listvariables()
else:
    print 'CDMS not built with PP support, skipped PP test'

if cdat_info.CDMS_INCLUDE_HDF=='yes':
    print 'Testing HDF'
    f=cdms2.open(os.path.join(pth,'tdata.hdf'))
    print f.listvariables()
else:
    print 'CDMS not built with HDF4 support, skipped HDF4 test'

#if cdat_info.CDMS_INCLUDE_DRS=='yes':
#    print 'Testing DRS'
#    f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','ta_300_850_PCM_O_mm_xy_wa_r0000_0000.dic'))
#    print f.listvariables()
#else:
#    print 'CDMS not built with DRS support, skipped DRS test'

print "Testing GRIB2"
f=cdms2.open(os.path.join(pth,'testgrib2.ctl'))
print f.listvariables()

## if DAP:
##     try:
##         print 'DAP and Scientific ?'
##         import Scientific.IO.NetCDF
##         f=Scientific.IO.NetCDF.NetCDFFile('http://ferret.wrc.noaa.gov/cgi-bin/nph-nc/data/COADS_climatology.nc')
##         print f.variables
##     except:
##         print 'Failed'

    
#if cdat_info.CDMS_INCLUDE_DRS=='yes':
#    print 'Testing cdms2/ql ?'
#    f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','test.cdms2'))
#    print f.listvariables()
#else:
#    print 'CDMS not built with PSQL support, skipped PSQL test'

