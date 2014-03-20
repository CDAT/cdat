# Adapted for numpy/ma/cdms2 by convertcdms.py
import cdms2 as cdms,vcs_legacy,sys,os,support

if support.dogui:
    x=vcs_legacy.init()

    f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','test_anim.nc'))
    s=f('variable_6')
    print s.shape
    x.plot(s)
    raw_input()
    x.animate.create()
    raw_input()
    print 'Created'
    x.animate.run()

    raw_input()
else:
    print 'You need to run this one by hand (turn support.dogui to 1 first)'
    
