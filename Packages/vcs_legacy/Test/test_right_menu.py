# Adapted for numpy/ma/cdms2 by convertcdms.py
import vcs_legacy, cdms2 as cdms, EzTemplate, sys, genutil,support

def clear(*args,**kargs):
    x = kargs['canvas']
    x.clear()
    
def plot_ts(*args,**kargs):
    x = kargs['canvas']
    i=kargs['index_x']
    j=kargs['index_y']
    x.clear()
    x.plot(s,t1)
    ts = s[:,j,i]
    x.plot(ts,t2)
    
def plot_lat_time(*args,**kargs):
    x = kargs['canvas']
    i=kargs['index_x']
    j=kargs['index_y']
    x.clear()
    x.plot(s,t1)
    ts = s[:,:,i]
    x.plot(ts,t2)
    
def plot_lon_time(*args,**kargs):
    x = kargs['canvas']
    i=kargs['index_x']
    j=kargs['index_y']
    x.clear()
    x.plot(s,t1)
    ts = s[:,j]
    x.plot(ts,t2)
if support.dogui:

    x=vcs_legacy.init()
    x.portrait()
    y=vcs_legacy.init()
    y.open()
    y.portrait()


    T=EzTemplate.Multi(rows=2,columns=1)

    f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))

    global s,t2,t1

    s=f('clt')

    t1=T.get()
    t2=T.get()

    x.user_actions_names=['Clear','Plot time serie']
    x.user_actions=[clear,plot_ts]
    x.plot(s,t1)
    y.user_actions_names=['Clear','Plot lat/time cross section','Plot lon/time cross section']
    y.user_actions=[clear,plot_lat_time,plot_lon_time]
    y.plot(s,t1)
    raw_input("Press enter to end")
else:
    print 'You need to run this one by hand (turn support.dogui to 1 first)'
