
#  Usage:
#
#    python setup.py install 
#
#
from numpy.distutils.core import setup, Extension
import os, sys, string, time, shutil
if not os.environ.has_key("BUILD_DIR"):
    ## If we build by hand and forgot to set BUILD_DIR
    print "Seeting BUILD_DIR to local dir since we didn't set it"
    os.environ['BUILD_DIR']="."
sys.path.append(os.environ['BUILD_DIR'])
import cdat_info

WM = "QT" # "X11" or "QT" # Window Manager
EM = "QT" # "X11" or "QT" # Event Manager
DRAW = "CAIRO" # or "X11"
#DRAW = "X11"
WM="X11"
EM="X11"

QT_PATH_LIB = '/usr/local/Trolltech/Qt-4.6.2/lib'
QT_PATH_INC = '/usr/local/Trolltech/Qt-4.6.2/include'
QT_PATH_BIN = '/usr/local/Trolltech/Qt-4.6.2/bin'
USE_FRAMEWORK = False

target_prefix = sys.prefix
removes = []

QT_LIBS_SFX=""

for i in range(len(sys.argv)):
    a = sys.argv[i]
    if a=='--prefix':
        target_prefix=sys.argv[i+1]
    sp = a.split("--prefix=")
    if len(sp)==2:
        target_prefix=sp[1]
    if a == '--enable-qt':
        WM='QT'
        EM='QT'
        removes.append(a)
        QT_PATH_LIB = '/usr/local/Trolltech/Qt-4.5.2/lib'
        QT_PATH_INC = '/usr/local/Trolltech/Qt-4.5.2/include'
        QT_PATH_BIN = '/usr/local/Trolltech/Qt-4.5.2/bin'
        print "Set bin to:",QT_PATH_BIN
        
    if a == '--qt-debug':
        QT_LIBS_SFX="_debug"
        removes.append(a)

        
    if a[:10] == '--with-qt=':
        WM='QT'
        EM='QT'
        removes.append(a)

        QT_PATH_LIB = os.path.join(a[10:],'lib')
        QT_PATH_INC = os.path.join(a[10:],'include')
        QT_PATH_BIN = os.path.join(a[10:],'bin')
        print "Set bin to:",QT_PATH_BIN
        
    if a[:14] == '--with-qt-inc=':
        WM='QT'
        EM='QT'
        QT_PATH_INC = a[14:]
        removes.append(a)
        
    if a[:14] == '--with-qt-lib=':
        WM='QT'
        EM='QT'
        QT_PATH_LIB = a[14:]
        removes.append(a)
        
    if a[:14] == '--with-qt-bin=':
        WM='QT'
        EM='QT'
        QT_PATH_BIN = a[14:]
        print "Set bin to:",QT_PATH_BIN
        removes.append(a)

    if a == '--enable-qt-framework':
        WM='QT'
        EM='QT'
        removes.append(a)

        USE_FRAMEWORK = True
        
        #QT_PATH_INC = "/Library/Frameworks/QtCore.framework/Headers"
        #QT_PATH_LIB = "/Library/Frameworks/QtCore.framework"

for a in removes:
   sys.argv.remove(a)

#QT_PATH_INC = '/usr/include/qt4'
#QT_PATH_LIB = '/usr'

try:
    externals = cdat_info.externals
except:
    #externals = os.path.join(sys.prefix,"Externals")
    externals = os.environ.get("EXTERNALS",externals)
    
os.environ['PATH']=os.environ['PATH']+':'+os.path.join(target_prefix,'bin')+':'+os.path.join(externals,'bin')
sys.path.insert(0,os.path.join(target_prefix,'lib','python%i.%i' % sys.version_info[:2],'site-packages'))

os.environ['PKG_CONFIG_PATH']=os.path.join(externals,'lib','pkgconfig')+':'+os.environ.get("PKG_CONFIG_PATH","")

here = os.getcwd().replace(" ","\ ")
vcsbase = os.path.join(here, 'Src','library')
vcsbase_proj = os.path.join(here,'Src','gctpc')
# cdatbase = os.path.join(sys.prefix,'include')
cdatbase = os.path.join(target_prefix,'include')
xgksroot = os.path.join(here,'Src', 'xgks')
# py = sys.prefix
py = target_prefix
major, minor = sys.version_info[0:2]
t = os.popen('uname')
uname = t.read()[:-1]
t.close()

pkgconfig = os.path.join(externals,'bin','pkg-config')
if not os.path.exists(pkgconfig):
    pkgconfig="pkg-config"
print "OK WE ARE USING:",pkgconfig
print "ENV FOR PKG IS:",os.environ.get("PKG_CONFIG_PATH","NO PKGPATH")
 
freetypelibdir = [ os.path.join(externals,'lib'), ]
freetypeincdir = [ os.path.join(externals,'include')]
freetypelibdir = os.popen("%s --libs-only-L freetype2" % pkgconfig).read().strip().split("-L")[1:]
c=[]
for e in freetypelibdir:
    c.append(e.strip())
freetypelibdir=c
c=[]
freetypeincdir = os.popen("%s --cflags-only-I freetype2" % pkgconfig).read().strip().split("-I")[1:]
for e in freetypeincdir:
    c.append(e.strip())
freetypeincdir=c
# Platform-specific modifications
# added freetype for output of fonts
c=[]
freetype_libs = os.popen("%s --libs-only-l freetype2" % pkgconfig).read().strip().split("-l")[1:]
for e in freetype_libs:
    c.append(e.strip())
freetype_libs=c

c=[]
cairolibdir = os.popen("%s --libs-only-L cairo" % pkgconfig).read().strip().split("-L")[1:]
c=[]
for e in cairolibdir:
    c.append(e.strip())
cairolibdir=c
c=[]
cairoincdir = os.popen("%s --cflags-only-I cairo" % pkgconfig).read().strip().split("-I")[1:]
for e in cairoincdir:
    c.append(e.strip())
cairoincdir=c
# Platform-specific modifications
# added freetype for output of fonts
c=[]
cairo_libs = os.popen("%s --libs-only-l cairo" % pkgconfig).read().strip().split("-l")[1:]
for e in cairo_libs:
    c.append(e.strip())
cairo_libs=c

c=[]
xml2libdir = os.popen("%s --libs-only-L xml2" % pkgconfig).read().strip().split("-L")[1:]
for e in xml2libdir:
    c.append(e.strip())
xml2libdir=c
c=[]
xml2incdir = os.popen("%s --cflags-only-I xml2" % pkgconfig).read().strip().split("-I")[1:]
for e in xml2incdir:
    c.append(e.strip())
xml2incdir=c
c=[]
# Platform-specific modifications
# added freetype for output of fonts
xml2_libs = os.popen("%s --libs-only-l xml2" % pkgconfig).read().strip().split("-l")[1:]
for e in xml2_libs:
    c.append(e.strip())
xml2_libs=c

vcs_extra_compile_args = []
# ??? Add code to detect Qt and locaton here
if WM=="QT" or EM=="QT":
    moc_mainwindow_path = os.path.abspath(os.path.join(os.environ['BUILD_DIR'], "moc_mainwindow.cpp"))
    print "Moc:",moc_mainwindow_path
    print "QT PATH:",QT_PATH_INC,QT_PATH_LIB,QT_PATH_BIN
    QT_SOURCES=""" main.cpp mainwindow.cpp qti.cpp %s""" % moc_mainwindow_path
    qtsourcelist=QT_SOURCES.split()
    vcsbase_qt = os.path.join(here, 'Src','Qt')
    s10 = map(lambda x: os.path.join(vcsbase_qt,x), qtsourcelist)

    qt_include_dirs=[os.path.join(here, 'Include','Qt'),]
## Generic non framework thing
    MOC = os.path.join(QT_PATH_BIN,"moc-qt4")
    print "looking for MOC in:",MOC
    if not os.path.exists(MOC):
        MOC = os.path.join(QT_PATH_BIN,"moc")
    if not os.path.exists(MOC):
        raise Exception,"Error could not find moc executable"
##    qt_vcs_extra_compile_args = ' -pipe -g -gdwarf-2 -Wall -W -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I. -I%s -I%s/QtCore -I%s/QtGui '%(QT_PATH_INC,QT_PATH_INC,QT_PATH_INC)
    qt_vcs_extra_compile_args = ' -pipe -g -gdwarf-2 -Wall -W -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I.'
#     MOC = os.path.join(QT_PATH_BIN,"moc")
#     qt_include_dirs = [ '%s' % QT_PATH_INC,
#                             '%s/Qt' % QT_PATH_INC,
#                             '%s/QtCore' % QT_PATH_INC,
#                             '%s/QtGui' % QT_PATH_INC,
#                             os.path.join(here, 'Include','Qt'),
#                             ]
    if USE_FRAMEWORK:
        #MOC = "/usr/bin/moc"
### Framework stuff
        qt_vcs_extra_compile_args += ' -pipe -g -gdwarf-2 -Wall -W -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I%s -F%s -I%s/QtCore.framework/Headers -I%s/QtGui.framework/Headers' % (QT_PATH_INC,QT_PATH_LIB,QT_PATH_LIB,QT_PATH_LIB)
#        vcs_extra_compile_args = ' -c -pipe -g -gdwarf-2 -Wall -W -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -F/Library/Frameworks  -I. -I%s -I%s/QtCore -I%s/QtGui '%(QT_PATH_INC,QT_PATH_INC,QT_PATH_INC)
        qt_vcs_extra_link_args = ' -F%s -framework QtCore -framework QtGui -lz -lm ' % (QT_PATH_LIB)
    else:
        qt_vcs_extra_compile_args += ' -I%s -I%s/QtCore -I%s/QtGui '%(QT_PATH_INC,QT_PATH_INC,QT_PATH_INC)
        qt_vcs_extra_link_args = ' -L%s -lQtCore%s -lQtGui%s'% (QT_PATH_LIB,QT_LIBS_SFX,QT_LIBS_SFX)

    ## Ok here we generate the moc file
    if os.path.exists(moc_mainwindow_path):
       os.remove(moc_mainwindow_path)
    moccmd = "%s Include/Qt/mainwindow.h > %s" % (MOC,moc_mainwindow_path)
    print "MOC:",moccmd
    ln = os.popen(moccmd).readlines()
    if not os.path.exists(moc_mainwindow_path):
        print "".join(ln)
        raise "Error could not generate the moc file"
    vcs_extra_compile_args = ['-c',]+qt_vcs_extra_compile_args.split()
    vcs_extra_link_args = [qt_vcs_extra_link_args,]
else:
    s10=[]
    vcs_extra_link_args = []
    qt_include_dirs=[]

if WM=="X11" or EM=="X11" or DRAW=="X11":
    x_libraries = freetype_libs + cairo_libs + xml2_libs + ['X11',] + cdat_info.mathlibs
    x11include = cdat_info.x11include
    x11libdir = cdat_info.x11libdir
else:
    x_libraries = freetype_libs + cairo_libs + xml2_libs + cdat_info.mathlibs
    x11include = []
    x11libdir = []

s11 = ['Src/events/main_event_loop.cpp','Src/events/vcs_editor.cpp']
if EM == "QT":
    vcs_extra_compile_args+=["-DQTEM","-DUSEQT"]
else:
    vcs_extra_compile_args+=["-DX11EM","-DUSEX11"]
    s11+=['Src/events/X11/draw_popups.c',]
    
if WM == "QT":
    vcs_extra_compile_args+=["-DQTWM","-DUSEQT"]
else:
    vcs_extra_compile_args+=["-DX11WM","-DUSEX11",]

s5 = [
      ]

if DRAW=="CAIRO":
    vcs_extra_compile_args+=["-DCAIRODRAW",]
    CAIRO_SOURCELIST="""
    vcs2cairo.c meta2cairo.c cairoXemulator.c
    """
    s5 = [   os.path.join(xgksroot,'ps','ps.c'),
             os.path.join(xgksroot,'svg','svg.c'),
             os.path.join(xgksroot,'pdf','pdf.c'),
             os.path.join(xgksroot,'png','png.c'),
             ]
else:
    vcs_extra_compile_args+=["-DX11DRAW","-DUSEX11"]
    CAIRO_SOURCELIST = ""
    s5 = [  ]
    

    # Define X11 location for Opteron platform x86_64
    if (os.uname()[4] in ['x86_64']): x11libdir.insert(0,'/usr/X11R6/lib64')

    #print >>sys.stderr, "vcs using these X11 directories: %s %s" %(x11include, x11libdir) 

## print 'QT_INCLUDE_DIRS', qt_include_dirs 
vcs_include_dirs = ['Include'] + \
			[os.path.join(externals,'include'),] + \
                     cairoincdir +\
		     freetypeincdir +\
                     xml2incdir +\
                   [ vcsbase_proj ] + \
                   [ cdatbase ] + \
                   cdat_info.cdunif_include_directories +\
                   x11include + \
                     [py+'/include', 
                     ] +\
		['/usr/include','/usr/local/include',   ] + \
                qt_include_dirs

vcs_library_dirs = [os.path.join(externals,'lib'),] + cairolibdir + freetypelibdir + xml2libdir + cdat_info.cdunif_library_directories + x11libdir + ['/usr/lib','/usr/local/lib']
vcs_libraries= x_libraries + cdat_info.cdunif_libraries

SOURCES_SCRIPTC ="""
main.c rscript.c misc.c getp.c procA.c procL.c procC.c procTt.c procTo.c procTl.c procTf.c procTm.c procP.c procGi.c procCOMM.c procDisp.c 
acquire.c fintwt.c isolines.c pict_elem.c format.c procTh.c procCGM.c procPage.c procCanvas.c procClear.c procGfi.c procGfo.c err_warn.c check_canvas_defer.c procInd.c procDump.c 
outlines.c procGo.c procGcon.c select_A.c removeA.c vcs_update.c isofills.c fillup.c filldown.c proj_gks.c procColor.c removeC.c removeP.c reset_A.c procRem.c procCop.c 
procRen.c procSleep.c removeGi.c removeGo.c removeGfi.c removeGfo.c removeGcon.c removeL.c removeTt.c removeTo.c removeTl.c removeTf.c removeTm.c removeTh.c getGi.c 
getGfi.c getGo.c getGfo.c getGcon.c getList.c getTt.c getTo.c getTl.c getTf.c getTm.c getTh.c getp_.c getA.c continents.c outfills.c compile_vcs.c save_image.c save_image_vcs.c save_gif_image_vcs.c 
write_ras.c computer.c procRun.c procPat.c procRas.c procGif.c procJpeg.c procPng.c procDRS.c
procnetCDF.c procHDF.c chkis.c set_text_attr.c boxfills.c procGfb.c procOvly.c logicomp.c removeGfb.c getGfb.c compu_log.c 
vectors.c getGv.c removeGv.c procGv.c procGXyvy.c procGYxvx.c procGXY.c removeGXy.c removeGYx.c removegxy_.c getGXyvy.c getGYxvx.c getGXY.c Xyvy.c Yxvx.c XvY.c 
procHints.c procControl.c procGSp.c getGSp.c removeGSp.c scatter_plot.c transform_axis.c markers.c python_misc.c vcs_canvas.c animation.c image_routines.c 
procLoop.c procLoop_cdat.c gd.c latitude.c
meshfill.c procGfm.c removeGfm.c getGfm.c procProj.c removeProj.c  getProj.c
procMETA.c
"""
sourcelist=string.split(SOURCES_SCRIPTC)
s1 = map(lambda x: os.path.join(vcsbase,x), sourcelist)

XGKSSOURCES ="""
act_ws.c aspect_flags.c cellarray.c choice.c colours.c 
deferral_ws.c escape.c event.c externinit.c fillarea.c 
gdp.c ggdp.c gks_error.c input.c inqWDT.c 
inqfillareas.c inqpixel.c inqpmarker.c inqpolylines.c 
inqtext.c inqtransform.c inquiries.c locator.c 
message.c metafile.c open_gks.c open_ws.c pick.c 
polylines.c polymarkers.c prmgr.c segments.c string.c 
stroke.c text.c transforms.c umalloc.c update.c 
valuator.c
"""
xgkssourcelist=string.split(XGKSSOURCES)
s2 = map(lambda x: os.path.join(xgksroot,x), xgkssourcelist)

s3 = [os.path.join(xgksroot,'cgm','cgmi.c'),
      os.path.join(xgksroot,'cgm','cgmo.c')
     ]
s4 = [os.path.join(xgksroot,'gksm','gksm.c')]

XSOURCELIST="""
xSet.c xcellarray.c xcolours.c xevent.c xfillarea.c 
xinqpixel.c xopws.c xpline.c xpmarker.c xport.c xtext.c 
xupdate.c
"""
s6list=string.split(XSOURCELIST)
s6=map(lambda x: os.path.join(xgksroot,'x', x), s6list)

PROJECTIONS_SOURCES="""
alberfor.c  cproj.c     gnomfor.c   haminv.c    lamccinv.c  obleqfor.c  polyfor.c  sinfor.c
stplnfor.c  vandgfor.c  alberinv.c  eqconfor.c  gnominv.c   imolwfor.c  merfor.c   obleqinv.c
polyinv.c   sininv.c    stplninv.c  vandginv.c  alconfor.c  eqconinv.c  goodfor.c  imolwinv.c
merinv.c    omerfor.c   psfor.c    somfor.c     tmfor.c     wivfor.c    alconinv.c equifor.c
goodinv.c   inv_init.c  millfor.c   omerinv.c   psinv.c     sominv.c    tminv.c    wivinv.c
azimfor.c   equiinv.c   gvnspfor.c  lamazfor.c  millinv.c   orthfor.c   report.c   sphdz.c
untfz.c     wviifor.c   aziminv.c   for_init.c  gvnspinv.c  lamazinv.c  molwfor.c  orthinv.c
robfor.c    sterfor.c   utmfor.c    wviiinv.c   br_gctp.c   gctp.c      hamfor.c    lamccfor.c
molwinv.c   paksz.c     robinv.c    sterinv.c   utminv.c
"""
s7list=string.split(PROJECTIONS_SOURCES)
s7=map(lambda x: os.path.join(vcsbase_proj, x), s7list)

TTF_SOURCELIST="""
  ttf2vcs.c
"""
s8list=string.split(TTF_SOURCELIST)
s8=map(lambda x: os.path.join(xgksroot,'ttf', x), s8list)

s9list=string.split(CAIRO_SOURCELIST)
s9=map(lambda x: os.path.join(xgksroot,'cairo', x), s9list)

vcs_library_include_dirs=[
             ]
os_name = os.uname()[0]
if os_name in ['CYGWIN_NT-5.1']: os_name = os_name[:-4]
vcs_macros = [ (os_name, None),
               ('CDCOMPAT', None),
               ('PYTHON', None),
               ('incxws', None),
               ('inctty', None),
               ('incps', None),
               ('inccgm', None),
	#       ('DOUBLE',None),
             ]
if cdat_info.CDMS_INCLUDE_DRS == "yes":
    vcs_macros.append(('DRS', None))
if cdat_info.CDMS_INCLUDE_HDF == "yes":
    vcs_macros.append(('HDF', None))
#
###############################################################################
#                                                                             #
# Mac OS X 10.x printed a large number of warnings. Removed the warnings from #
# the VCS build. This should speed things up....                              #
#                                                                             #
# If this effects other platforms put in sys.plotform for Darwin only.        #
#                                                                             #
###############################################################################
dovcs = True # turn this to off to do simply the sip part
try:
   from distutils import sysconfig
   if dovcs:
    vcs_so = '%s/vcs/_vcs.so' % sysconfig.get_python_lib()
    os.remove(vcs_so)
   sysconfig.get_config_vars('OPT')
   cflg= sysconfig._config_vars['OPT'].split()
   cflg2=[]
   for x in cflg:
       if x not in ['-Wall', '-Wstrict-prototypes']:
          cflg2.append(x)
   sysconfig._config_vars['OPT']=' '.join(cflg2)   
except:
   pass
#
## print "macros:",vcs_macros,"EM:",EM,"WM:",WM



## Testing extra args for Vistrails

if dovcs:
 setup (name = "vcs",
       version=cdat_info.Version,
       description = "Visualization and Control System",
       url = "http://www-pcmdi.llnl.gov/software",
       packages = ['vcs', 'vcs.test',],
       package_dir = {'vcs': 'Lib',
                      'vcs.test': 'Test',
                     },
       ext_modules = [
                      Extension('vcs.slabapi',
                               ['Src/slabapimodule.c',
                                'Src/slabapi.c',
                               ],
                               include_dirs = ['Include']
                      ),
    
                      ]
       )

#raw_input("ok first one done")
print 'slabapi done'
try:
    for dflt,rpl in [["-bundle","-dynamiclib -install_name %s/vcs/_vcs.so" % sysconfig.get_python_lib()],]:
## rpl = "-dynamiclib -Wl,-rpath,/lgm/cdat/VT/Python.framework/Versions/2.6/lib/python2.6/site-packages/vcs "
## rpl = "-bundle"
        sysconfig._config_vars['LDSHARED'] = sysconfig._config_vars['LDSHARED'].replace(dflt,rpl)
        sysconfig._config_vars['BLDSHARED'] = sysconfig._config_vars['BLDSHARED'].replace(dflt,rpl)
except:
   pass
#
## print "macros:",vcs_macros,"EM:",EM,"WM:",WM
info_dir = os.path.join(os.environ['BUILD_DIR'], "Info")
try:
 os.makedirs(info_dir)
except:
 pass
f=open(os.path.join(info_dir, "__init__.py"),"w")
print >> f, "macros = \"",vcs_macros,"EM:",EM,"WM:",WM,"\""
print >> f, "EM = \"",EM,"\""
print >> f, "WM = \"",WM,"\""
print >> f, "QT_PATH_LIB = \"",QT_PATH_LIB,"\""
print >> f, "QT_PATH_INC = \"",QT_PATH_INC,"\""
print >> f, "QT_PATH_BIN = \"",QT_PATH_BIN,"\""
print >> f, "USE_FRAMEWORK = ",USE_FRAMEWORK
f.close()

## Testing extra args for Vistrails

if dovcs:
 setup (name = "vcs",
       version=cdat_info.Version,
       description = "Visualization and Control System",
       url = "http://www-pcmdi.llnl.gov/software",
       packages = ['vcs', 'vcs.test','vcs.info'],
       package_dir = {'vcs': 'Lib',
                      'vcs.test': 'Test',
                      'vcs.info' : info_dir,
                     },
       ext_modules = [
    
                      Extension('vcs._vcs',
                                ['Src/vcsmodule.c',
                                 'Src/slabapi.c',
                                 'Src/f2c_lite.c',
                                 ] + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8 + s9 + s10 + s11,
                                define_macros = vcs_macros,
                                include_dirs = vcs_include_dirs,
                                library_dirs = vcs_library_dirs,
                                libraries = vcs_libraries,
                                extra_compile_args = vcs_extra_compile_args,
                                extra_link_args = vcs_extra_link_args,
                                ),
                      ]
       )

# Vistrails will need these includes later, let's copy them.
import shutil
ptho = sysconfig.get_python_lib()
try:
 shutil.rmtree("%s/vcs/Include" % ptho,ignore_errors=True)
 shutil.copytree("Include", "%s/vcs/Include" % ptho)
except Exception,err:
 ptho=target_prefix+"/lib/python%i.%i/site-packages/" % sys.version_info[:2]
 shutil.rmtree("%s/vcs/Include" % ptho,ignore_errors=False)
 try:
   shutil.copytree("Include", "%s/vcs/Include" % ptho)
 except:
   pass

print 'Copied the include files to: %s/vcs/Include' % sysconfig.get_python_lib()

if (WM=="QT" or EM=="QT") and sys.platform in ['darwin']:
    pref = sys.prefix
    ver = ".".join(sys.version.split(' ')[0].split(".")[:2])
    ccCmd = 'g++ -O3 -c %s -IInclude/Qt -IInclude -I/%s/include -I%s -o build/qpython.o Src/Qt/qpython.cpp' % (qt_vcs_extra_compile_args,pref,sysconfig.get_python_inc())
    print 'Running: ', ccCmd
    os.system(ccCmd)
    qt_vcs_extra_link_args = '%s/lib/python%s/config/libpython%s.a ' % (pref, ver, ver) + qt_vcs_extra_link_args
    ldCmd = 'g++ -o build/qpython build/qpython.o %s -lutil' % (qt_vcs_extra_link_args)
    print 'Running: ', ldCmd
    os.system(ldCmd)
    if 'install' in sys.argv:
        src = "%s/bin/cdat" % (target_prefix)
        print 'renaming to :',target_prefix
        try:
         shutil.move("build/qpython", src)
        except:
         pass
        if target_prefix.find("Versions")>-1:
            if target_prefix.find("Library/Frameworks")>-1:
                pth=os.path.sep+os.path.sep.join(target_prefix.split(os.path.sep)[:-5]+['bin','cdat'])
            else:
                pth=os.path.sep+os.path.sep.join(target_prefix.split(os.path.sep)[:-3]+['bin','cdat'])

else:
    pth = "%s/bin/cdat" % (target_prefix)
    src = os.path.sep.join([sys.prefix,"bin","python"])
    print 'symlinking from ',pth,sys.prefix
try:
   os.remove(pth)
except:
   pass
#print "At that point target is:",target_prefix
print "Symlink:",pth
#sys.exit()
os.symlink(src,pth)
#filedds = os.popen("find build/temp* -name '*.o'").readlines()
#ofiles=' '.join(files).replace('\n',' ')
#print ofiles
#ldCmd = 'g++ -o libvcs.dylib %s %s ' % (vcs_extra_link_args,ofiles)
#os.system(ldCmd)

if (WM=='QT' or EM=='QT'):
    import sipconfig
    from PyQt4 import pyqtconfig


    from distutils import sysconfig

    vcs_so = '%s/vcs/_vcs.so' % sysconfig.get_python_lib()
    vcs_inc = '%s/vcs/Include' % sysconfig.get_python_lib()

    ## vcs_so = '/Users/hvo/src/uvcdat/cdatBuild/lib/python2.7/site-packages/vcs/_vcs.so'
    ## vcs_inc = '/Users/hvo/src/uvcdat/cdat/Packages/vcs/Include'

    print "so is at:",vcs_so,vcs_inc

    # Get the PyQt configuration information.
    config = pyqtconfig.Configuration()

    # Get the extra SIP flags needed by the imported qt module.  Note that
    # this normally only includes those flags (-x and -t) that relate to SIP's
    # versioning system.
    qt_sip_flags = config.pyqt_sip_flags

    cdatwrap_dir_path = os.path.join(os.environ['BUILD_DIR'], 'cdatwrap')

    # The name of the SIP build file generated by SIP and used by the build
    # system.
    build_file = "pyqtscripting.sbf"
    build_file_path = os.path.join(cdatwrap_dir_path, build_file)

    os.system("rm -rf %s" % cdatwrap_dir_path)
    os.mkdir(cdatwrap_dir_path)
    os.system("touch %s/__init__.py" % cdatwrap_dir_path)

    # Run SIP to generate the code.  Note that we tell SIP where to find the qt
    # module's specification files using the -I flag.
    os.system(" ".join([ \
        config.sip_bin, \
        "-c", cdatwrap_dir_path, \
        "-b", build_file_path, \
        "-I", config.pyqt_sip_dir, \
        qt_sip_flags, \
        "cdat.sip" \
    ]))

    # Create the Makefile.  The QtModuleMakefile class provided by the
    # pyqtconfig module takes care of all the extra preprocessor, compiler and
    # linker flags needed by the Qt library.
    makefile = pyqtconfig.QtGuiModuleMakefile(
        dir=cdatwrap_dir_path,
        configuration=config,
        build_file=build_file
    )

    # Add the library we are wrapping.  The name doesn't include any platform
    # specific prefixes or extensions (e.g. the "lib" prefix on UNIX, or the
    # ".dll" extension on Windows).
    #makefile.extra_libs = ["vcs"]
    import cdat_info
    makefile.CFLAGS.append("-I%s/include" % cdat_info.externals)
    makefile.CFLAGS.append("-I%s" % vcs_inc)
    makefile.CFLAGS.append("-I%s/.." % sysconfig.get_python_inc())

    makefile.CXXFLAGS.append("-I%s/include" % cdat_info.externals)
    makefile.CXXFLAGS.append("-I%s" % vcs_inc)
    makefile.CXXFLAGS.append("-I%s/.." % sysconfig.get_python_inc())

    cwd = os.getcwd()
    makefile.LFLAGS.append("-Wl,-rpath,%s/cdatwrap" % cwd)
    makefile.LIBS.append(vcs_so);

    # Generate the Makefile itself.
    makefile.generate()
    os.chdir(cdatwrap_dir_path)
    os.system("make clean")
    if sys.platform in ['darwin']:
        os.system("MACOSX_DEPLOYMENT_TARGET=10.6 make")
    else:
        os.system("make")
    os.system("make install")
    
