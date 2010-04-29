# Adapted for numpy/ma/cdms2 by convertcdms.py
import sys, httplib,cdms2 as cdms,traceback,time

def get_html(url):
    sp=url.split('/')
    server=sp[2]
    path = '/'+'/'.join(sp[3:])

    http = httplib.HTTPConnection(server)

    http.connect()


    http.putrequest('GET',path)

    http.putheader('Accept','text/html')
    http.putheader('Accept','text/plain')
    http.endheaders()

    r = http.getresponse()
    page = r.read()
    r.close()
    http.close()
    return page.split()


def scan(url):
    if url[-1]!='/':
        url+='/'
    scanned=[]
    files = []
    others=[]
    root=url.replace('HTTP','http')
    page = get_html(url)
    for l in page:
        l2=l.replace('HTTP','http')
        l2=l2.replace('HREF','href')
        if l2.find("href=htt")>-1 or l2.find("href='htt")>-1 or l2.find("href=\"htt")>-1:
            try:
                sp=l2.split('http://')[1]
                sp=sp.split('>')[0]
                if sp[-1] in ['"',"'"]:
                    sp=sp[:-1]
                nurl="http://"+sp
                if nurl not in scanned and nurl.find(root)>-1:
##                     print '**********************',nurl
                    scanned.append(nurl)
                    try:
                        if nurl.split('.')[-1].lower() in ['das','dds','info','dods','html']:
                            nm='.'.join(nurl.split('.')[:-1])
                            if nm.split('.')[-1].lower() in ['z','gz']:
                                pass
                            else:
##                                 print 'Opening 1:',nm
                                f=cdms.open(nm)
                                scanned.append(nm+'.das')
                                scanned.append(nm+'.dds')
                                scanned.append(nm+'.dods')
                                scanned.append(nm+'.info')
                                scanned.append(nm+'.html')
##                                 vars=f.variables.keys()
                                f.close()
                                files.append(nm.replace(url,''))
                                #print 'Added:',nm,vars
                        elif nurl.split('.')[-1].lower() in ['z','gif','jpg','asc','txt','gz','doc','png','ps']:
                            pass
                        else:
##                             print 'Opening 2:','.'.join(nurl.split('.')[:-1])
                            f=cdms.open('.'.join(nurl.split('.')[:-1]))
##                             vars=f.variables.keys()
                            f.close()
                            files.append(nurl.replace(url,''))
##                         print 'Went fine'
                    except:
                        others.append(nurl.replace(url,''))
##                         print '1:',
##                         traceback.print_exc()
##                         print nurl,'trying',url.lower(),parent.lower()
##                         if nurl not in [url.lower(),parent.lower()]:
##                             print 'Scanning:                    ',nurl
##                             pfiles=scan(nurl,parent=url,break_sub=True)
##                             if isinstance(pfiles,str):
##                                 files.append(pfiles)
##                             scanned+=pscanned
##                             print 'Returned:',pfiles,len(files)
                        
            except :
                print '2:',
                traceback.print_exc()
                pass
    return files,others

    
    

#print time.localtime()

#files = scan('http://ferret.wrc.noaa.gov/cgi-bin/nph-nc/data/ABIOTIC2_SEAS.diag_ad-51.nc')
#files,others = scan('http://ferret.wrc.noaa.gov/cgi-bin/nph-nc/data')
#files = scan('http://ferret.wrc.noaa.gov/cgi-bin/nph-nc/data/GLODAP')
files, others = scan('http://nomads.ncdc.noaa.gov:9090/dods/NCDC_NOAAPort_ETA/200311/20031127')
#files, others = scan('http://nomads.ncdc.noaa.gov:9090/dods/NCDC_NOAAPort_ETA/200311')
#print time.localtime()

f=open('files.txt','w')
for fnm in files:
    print >>f,fnm
f.close()
f=open('others.txt','w')
for fnm in others:
    print >>f,fnm
f.close()
