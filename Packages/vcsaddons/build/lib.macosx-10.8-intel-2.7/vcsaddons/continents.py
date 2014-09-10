import httplib, urllib
import vcs,os
from core import VCSaddon
import vcsaddons
import numpy

class Guc(VCSaddon):
    ngdc_names = [
        "NOAA/NOS Medium Resolution Coastline (designed for 1:70,000)",
        "World Vector Shoreline (designed for 1:250,000)",
        "World Data Bank II (designed for 1:2,000,000)",
        "WCL (World Coast Line) (designed for 1:5,000,000)",
        "Rivers from WDBII",
        "All Political Boundaries from WDBII" ,
        "International Boundaries Only from WDBII",
        "Internal Boundaries Only from WDBII",
        "WVS and All Political Boundaries",
        ]
    
    def __init__(self,name=None,source='default',x=None,template = None):
        self.g_name='Guc'
        self.g_type='userconts'
        VCSaddon.__init__(self,name,source,x,template)
        if source == 'default':
            self.datadir=os.path.join(os.environ['HOME'],"PCMDI_GRAPHICS")
            self.types = [1,]
            self.colors = [242,]
            self.lines=['solid',]
            self.widths=[1,]
            self.sources = [0,]
        else:
            gm = vcsaddons.gms[self.g_name][source]
            self.datadir= gm.datadir
            self.types = gm.types
            self.colors = gm.colors
            self.lines = gm.line
            self.widths = gm.widths
            self.sources = gm.sources
            
        try:
            import bz2
            self.HAVE_BZ2=True
        except:
            self.HAVE_BZ2=False

            
    def gen_cont_file(self,lon1,lon2,lat1,lat2,type,patch=False):
        if isinstance(type,str):
            try:
                itype = self.ngdc_names.index(type)
            except Exception,err:
                print err,type
                itype=-1
        if not (0<=itype<len(self.ngdc_names)):
            oktype=""
            for i in range(len(self.ngdc_names)):
                oktype+="\n%i or '%s'" % (i,self.ngdc_names[i])
            raise Exception,"Error currently only ngdc source supported are: %s" % oktype

        files = []
        found = False
        for f in os.listdir(self.datadir):
            if f.find("mycont_C%i" % (itype+1))>-1:
                sp = f.split("_")
                flon1 = int(sp[2][1:])
                flon2=int(sp[3][1:])
                flat1=int(sp[4][1:])
                flat2=int(sp[5][1:].split(".dat")[0])
                if patch is False:
                    if flon1<=lon1 and flon2>=lon2 and flat1<=lat1 and flat2>=lat2:
                        found = True
                        break
                else:
                    pass # no implemented yet
        if found:
            fnm = f
        else:
            fnm = "mycont_C%i_L%i_L%i_l%i_l%i.dat" % (itype+1,int(lon1)-1,int(lon2)+1,int(lat1)-1,int(lat2)+1)
        fnm = os.path.join(self.datadir,fnm)

        if not os.path.exists(fnm):
            params = urllib.urlencode(
                {
                'lon_min':str(lon1),
                'lon_max':str(lon2),
                'lat_min':str(lat1),
                'lat_max':str(lat2),
                'coastline':type,
                'compression':'None',
                'convert':"0",
                                'plot_coast':"0",
                }
                )
            headers = {}#"Content-type": "application/x-www-form-urlencoded",
                #"Accept": "text/plain"}
            conn = httplib.HTTPConnection("rimmer.ngdc.noaa.gov:80")
            conn.request("POST", "/cgi-bin/mgg/coast/get_coast.pl", params, headers)
            response = conn.getresponse()
            if response.status!=httplib.OK:
                raise Exception,"HTTP Request failed (%s), try again later" % (response.reason)
            data = response.read()

            i0 = data.rfind("/coast/tmp")
            data = data[i0:]
            data=data[:data.find('"')]
            conn.request("GET",data)
            response = conn.getresponse()
            if response.status!=httplib.OK:
                raise Exception,"HTTP Request failed (%s), try again later" % (response.reason)
            data = response.read()
            conn.close()

            sp = data.split("# -b\n")[1:]
##             if self.HAVE_BZ2:
##                 import bz2
##                 fnm=fnm[:-4]
            f=open(fnm,"w")

            nmax = 1000
            N=0
            for s in sp:
                tmpsp=numpy.array(s.split()).astype('f')
                nprocessed = 0
                while nprocessed < len(tmpsp):
                    sp2=tmpsp[nprocessed:nprocessed+nmax*2]
                    nprocessed+=nmax*2
                    n = len(sp2)
                    if n>N: N=n
                    print >> f, n,1,sp2[1::2].min(),sp2[1::2].max(),sp2[::2].min(),sp2[::2].max()
                    for i in range(0,n,8):
                        vals = sp2[i:i+8].tolist()
                        for i in range(len(vals)/2):
                            v1,v2= vals[i*2],vals[i*2+1]
                            print >> f, "%f %f" % (v2,v1)
            print >> f, "-99 -99"
            f.close()
            if self.HAVE_BZ2:
                import bz2
                f=open(fnm)
                d=f.read()
                f.close()
                os.remove(fnm)
                d=bz2.compress(d)
                fnm = fnm + '.bz2'
                f=open(fnm,"w")
                f.write(d)
                f.close()
        return fnm

    def list(self):
        print '---------- UserContinents (Guc) member (attribute) listings ----------'
        print 'Canvas Mode = ',self.x.mode
        VCSaddon.list(self)
        print 'types =',self.types
        print 'colors =',self.colors
        print 'widths =',self.widths
        print 'lines =',self.lines
        print 'datadir =',self.datadir
        print 'sources =',self.sources

    def load_shapefile(self,source):
        import vcsaddons._gis
        if source[-4:]!='.shp':
            source+='.shp'
        shp = vcsaddons._gis.readshapefile(source)
        nelements = shp[1]
        data = ""
        maxpts = 100
        for nelement in range(nelements):
            shp_object = shp[nelement+3]
            nparts = shp_object[1]
            for p in range(nparts):
                P = shp_object[p+3]
##                 P.append(P[0])
##                 P.append(P[1])
##                 P.append(P[2])
##                 P.append(P[3])
                nvert = len(P)/2
                offset = 0
                npts = nvert/2
                while npts>0:
                    npts -= maxpts-1
                    if npts <=0:
                        npt = npts+maxpts-1
                    else:
                        npt = maxpts
                    l=P[4*(offset)+1:4*(offset+npt)+1:4]
                    L=P[4*(offset):4*(offset+npt):4]
                    data += "%i 1 %f %f %f %f \n" % ( npt*2, shp_object[2][1],shp_object[2][5],shp_object[2][0],shp_object[2][4])
                    for i in range(npt):
                        data+= "%f %f\n" % (l[i], L[i])
                    offset+=maxpts-1
        data += "-99 -99\n"
        return data
       
    def plot(self,data=None,template = None, bg=0, x=None):
        if x is None:
            x = self.x
        if template is None:
            template = self.template
        elif isinstance(template,str):
            template = x.gettemplate(template)
        elif not vcs.istemplate(template):
            raise "Error did not know what to do with template: %s" % template

        if data.getLongitude() is None or data.getLatitude() is None:
            raise Exception,["Error data do not have lat/lon"]
        if data is None:
            xmn=-180.
            xmx=180
            ymn=-90
            ymx=90
        else:
            xmn,xmx = vcs.minmax(data.getLongitude()[:])
            ymn,ymx = vcs.minmax(data.getLatitude()[:])
            
        xmn,xmx,ymn,ymx = self.prep_plot(xmn,xmx,ymn,ymx)

        n = len(self.sources)
        for i in range(n):
            type = self.types[i]
            source = self.sources[i]
            if type == 'ngdc' or type == 1:
                fnm = self.gen_cont_file(xmn,xmx,ymn,ymx,source)
                f=open(fnm)
                data=f.read()
                f.close()
                if self.HAVE_BZ2:
                    import bz2
                    data = bz2.decompress(data)
            elif type == "file" or type == 0:
                source = self.sources[i]
                f=open(source)
                data=f.read()
                f.close()
                if fnm[-3:]=='bz2':
                    if self.HAVE_BZ2 :
                        import bz2
                        data = bz2.decompress(data)
                    else:
                        raise Excetpion, ["Cannot uncompress bzipped data"]
            elif type == "shapefile" or type==2:
                source = self.sources[i]
                data = self.load_shapefile(source)
            f = open(os.path.join(os.environ.get("HOME","."),"PCMDI_GRAPHICS","data_continent_other10"),"w")
            f.write(data)
            f.close()
            c = x.createcontinents()
            j=i
            if i>=len(self.colors):
                j=-1
            c.linecolor=self.colors[j]
            j=i
            if i>=len(self.widths):
                j=-1
            c.linewidth=self.widths[j]
            j=i
            if i>=len(self.lines):
                j=-1
            c.line=self.lines[j]
            c.type=10

            c.datawc_x1 = xmn
            c.datawc_x2 = xmx
            c.datawc_y1 = ymn
            c.datawc_y2 = ymx
            c.xmtics1=self.xmtics1
            c.xmtics2=self.xmtics2
            c.ymtics1=self.ymtics1
            c.ymtics2=self.ymtics2
            c.xticlabels1=self.xticlabels1
            c.xticlabels2=self.xticlabels2
            c.yticlabels1=self.yticlabels1
            c.yticlabels2=self.yticlabels2
            c.xaxisconvert=self.xaxisconvert
            c.yaxisconvert= self.yaxisconvert
            c.legend = self.legend
            c.projection=self.projection

            self.x.plot(c,template,bg=bg)
            
