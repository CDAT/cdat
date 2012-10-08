""" Test external packages dependencies for CDAT
Prints out Packages that need to be installed and why
"""
import sys,os

## Test 1: Pyfort
min_ver=8.5
a=os.popen4(sys.prefix+'/bin/pyfort -V')[1].readlines()
sp=a[0].split()
if sp[0]!='Pyfort':
    print 'Pyfort : Not Present in your python distribution'
elif float(sp[1])<min_ver:
    print 'Pyfort : Version '+str(min_ver)+' minimum is required, you have: '+sp[1]

## Test 2: Numeric
min_ver=23.1
try:
    import Numeric
    if float(Numeric.__version__)<min_ver:
        print 'Numeric : Version '+str(min_ver)+' minimum is required, you have: '+Numeric.__version__
except:
    print 'Numeric : Not Present in your python distribution'

## Test 3: Pmw
min_ver=1.2
try:
    import Pmw
    if float(Pmw.version())<min_ver:
        print 'Pmw : Version '+str(min_ver)+' minimum is required, you have: '+Pmw.version()
except:
    print 'Pmw : Not Present in your python distribution'

## Test 4: gplot
a=os.popen4('which gplot')[1].readlines()[0]
if a.find('not found')>-1:
    print 'gplot : Not present on your system'

## Test 5: xgks
if not os.path.exists(sys.prefix+'/lib/xgksfonts'):
    print 'xgks : xgksfonts directory not present in your python distribution'

## Test 6: gifsicle
a=os.popen4('which gifsicle')[1].readlines()[0]
if a.find('not found')>-1:
    print 'gifsicle : Not present on your system'

## Test 7: ghostscript and  fonts
a=os.popen4('which gs')[1].readlines()[0]
if a.find('not found')>-1:
    print 'ghostscript : Not present on your system'
else:
    jpeg=0
    png=0
    a=os.popen4('gs -h')[1].readlines()
    while a.pop(0).find('Available devices:')<0:
        continue
    for l in a:
        s=l.strip().split()
        if 'jpeg' in s:
            jpeg=1
        if 'png16' in s:
            png=1
            
    font=0
    a=os.popen4('gs -h')[1].readlines()
    while a.pop(0).find('Search path:')<0:
        continue
    for l in a:
        if l[0]==' ': # path lines starts with blank
            s=l.strip().split(':')
            for p in s:
                #print os.listdir(p.strip())
                if os.path.exists(p.strip()+'/n022003l.afm'):
                    font=1
        else:
            break
    if jpeg==0 and png==0 and font==0:
        print 'ghostscript : no jpeg nor png support built, missing extra fonts'
    elif jpeg==0 and png==0:
        print 'ghostscript : no jpeg nor png support built'
    elif jpeg==0:
        print 'ghostscript : no jpeg support built'
    elif png==0:
        print 'ghostscript : no png support built'
    elif font==0:
        print 'ghostscript : extra fonts not installed'
            
## Test 8: Netpbm/pbmplus
a=os.popen4('which ppmtogif')[1].readlines()[0]
if a.find('not found')>-1:
    if sys.platform in ['linux2','darwin','cygwin']:
        print 'netpbm : Not present on your system'
    else:
        print 'pbmplus : Not present on your system'


## Test 9: R libraries (not python module)
a=os.popen4('which R')[1].readlines()[0]
if a.find('not found')>-1:
    print 'R : Not present on your system'

## Test 10: VTK
try:
    import vtk
except:
       print 'VTK : Not present on your Python'
 
