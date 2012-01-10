import cmor,numpy,sys,os
try:
    import cdms2
except:
    print "This test code needs cdms2 interface for i/0"
    sys.exit()

cdms2.setNetcdfShuffleFlag(0)
cdms2.setNetcdfDeflateFlag(0)
cdms2.setNetcdfDeflateLevelFlag(0)

if len(sys.argv)>1:
    level = int(sys.argv[1])
else:
    level=int(os.environ.get("DEFLATE_LEVEL",0))

if len(sys.argv)>2:
    shuffle= int(sys.argv[2])
else:
    shuffle=int(os.environ.get("SHUFFLE",0))

if level==0:
    deflate = 0
else:
    deflate = 1

f=open("Test/speed_test_table_A")
s=f.read()
f.close()
s=s.replace("${DEFLATE_LEVEL}",str(level))
s=s.replace("${DEFLATE}",str(deflate))
s=s.replace("${SHUFFLE}",str(shuffle))
f=open("mytable","w")
f.write(s)
f.close()

cmor.setup(inpath="Test",set_verbosity=cmor.CMOR_NORMAL, netcdf_file_action = cmor.CMOR_REPLACE, exit_control = cmor.CMOR_EXIT_ON_MAJOR);
cmor.dataset(
    outpath = "Test",
    experiment_id = "2xCO2 equilibrium experiment",
    institution = "GICC (Generic International Climate Center, Geneva, Switzerland)",
    source = "GICCM1 (2002): atmosphere:  GICAM3 (gicam_0_brnchT_itea_2, T63L32); ocean: MOM (mom3_ver_3.5.2, 2x3L15); sea ice: GISIM4; land: GILSM2.5",
    calendar = "standard",
    realization = 1,
    contact = "Charles Doutriaux (doutriaux1@llnl.gov)",
    history = "Test for speed and compression.",
    comment = "NetCDF4 vs NetCDF3 testing",
    references = "http://cdat.sf.net",
    leap_year=0,
    leap_month=0,
    month_lengths=None,model_id="pcmdi-09a",forcing="co2")

tables=[]
tables.append(cmor.load_table("mytable"))
print 'Tables ids:',tables


## read in data, just one slice
var='tos'
#f=cdms2.open('/export/ipcc/20c3m/atm/3h/tas/miroc3_2_medres/run1/tas_A3.nc')
#f=cdms2.open('/export/ipcc/20c3m/atm/3h/pr/miroc3_2_medres/run1/pr_A3.nc')
f=cdms2.open('/export/ipcc/20c3m/ocn/mo/tos/miroc3_2_medres/run1/tos_O1_1934_2000.nc')
s=f[var]

ntimes = s.shape[0]
ntimes=100
Tim = s.getTime().clone()
myaxes=numpy.arange(10)
myvars=numpy.arange(10)
try:
    missing_value = s.missing_value[0]
except:
    try:
        missing_value=s.missing_value
    except:
        missing_value=None

print 'Missing:',type(missing_value),missing_value
#missing_value = -999.
myaxes[0] = cmor.axis(table_entry = 'latitude', 
                      units = 'degrees_north', 
                      coord_vals = s.getLatitude()[:])
myaxes[1] = cmor.axis(table_entry = 'longitude', 
                      units = 'degrees_north', 
                      coord_vals = s.getLongitude()[:])



myaxes[2] = cmor.axis(table_entry = 'time',
                      units = Tim.units,
                      coord_vals = Tim[:ntimes])

pass_axes = [myaxes[2],myaxes[0],myaxes[1]]

myvars[0] = cmor.variable( table_entry = var,
                           units = s.units,
                           axis_ids = pass_axes,
                           original_name = var,
                           history = 'no history',
                           comment = 'testing speed',
                           missing_value=missing_value,
                           tolerance = 1.e-2
                           )


import time,MV2
st = time.time()
totcmor=0
totcdms=0
maxcmor=0
mincmor=1000
maxcdms=0
mincdms=1000
c0=st
f=cdms2.open("Test/crap.nc","w")
step=10
for i in range(0,ntimes,step):
    print 'Time:',i
    j1=i
    j2=i+step
    cmor.write(myvars[0],s[j1:j2].filled(),step,file_suffix="speed-comp-02")
    c=time.time()
    #print 'cmor write time:',c-c0
    totcmor+=c-c0
    if maxcmor<c-c0:
        maxcmor=c-c0
    if mincmor>c-c0:
        mincmor=c-c0
    c0=c
    s2=s[j1:j2]
    sh=list(s2.shape)
    if len(sh)==2:
        sh.insert(0,1)
        s2=MV2.reshape(s2,sh)
        s2.setAxis(-1,s.getLongitude())
        s2.setAxis(-2,s.getLatitude())
        t=cdms2.createAxis([Tim[i],])
        t.id='time'
        t.units=Tim.units
        t.designateTime()
        s2.setAxis(0,t)
    f.write(s2,id=var)
    c=time.time()
    #print 'cdms time:',c-c0
    totcdms+=c-c0
    if maxcdms<c-c0:
        maxcdms=c-c0
    if mincdms>c-c0:
        mincdms=c-c0
    c0=c
f.close()
cmor.close()

import cdtime,os
ltime = cdtime.reltime(Tim[ntimes-1],Tim.units).tocomp()
ftime = cdtime.reltime(Tim[0],Tim.units).tocomp()
print ftime,ltime
print (var,var,ftime.year,ftime.month,ltime.year,ltime.month)
lcmor = os.stat("Test/IPCC_Fourth_Assessment/output/INSTITUTE_ID/pcmdi-09a/mon/REALM/%s/r1/%s_A1_pcmdi-09a_r1_%i%.2i-%i%.2i_speed-comp-02.nc" % (var,var,ftime.year,ftime.month,ltime.year,ltime.month))[6]
print 'level:',level,"shuffle:",shuffle
print 'total cmor:',totcmor,mincmor,totcmor/ntimes,maxcmor,lcmor
lcdms = os.stat("Test/crap.nc")[6]
print 'total cdms:',totcdms,mincdms,totcdms/ntimes,maxcdms,lcdms
print 'Size diff:',float(lcmor)/float(lcdms)
print 'speed diff:', totcmor/totcdms

if os.path.exists("summary.txt"):
    f = open("summary.txt")
    s=f.read()
    f.close()
    dic = eval(s)
else:
    dic = {}

dic[(level,shuffle)]=(float(lcmor)/float(lcdms),totcmor/totcdms)

for i in range(10):
    a = dic.get((i,0),"N/A")
    b = dic.get((i,1),"N/A")
    print 'Level: ',i,"no suffle:",a,"shuffle",b
f=open("summary.txt","w")
f.write(repr(dic))
f.close()

