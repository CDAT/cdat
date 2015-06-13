# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs,os,sys,support

if support.dogui is True:
    f=cdms.open(os.path.join(vcs.sample_data,'sampleCurveGrid4.nc'))
    
    s=f('sample')
    s=cdms.MV2.masked_greater(s,280.)
    
    x=vcs.init()
    
    x.plot(s)
    raw_input("pick a color on plot")
else:
    print 'You need to run this one by hand (turn support.dogui to 1 first)'
