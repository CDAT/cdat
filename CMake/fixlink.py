import os,sys,numpy
lib = '/usr/local/uvcdat/1.0.alpha/Library/Frameworks/Python.framework/Versions/2.7/lib/python2.7/site-packages/paraview/vtk/vtkCommonPython.so'
bad = 'ParaView-build'
#bad="System"
#bad="paraview3.11"
def change(lib,bad,paraviewPath,sameDir=False):
    cmd = 'otool -L %s' % lib
    print "LIB:",lib
    ln=os.popen(cmd).readlines()
    for l in ln[1:]:
        link = l.strip().split()[0]
        if link.find(bad)>-1:
            print link,"\t",
            nm=os.path.split(link)[1]
            print nm
            cmd = "install_name_tool -change %s %s/%s %s" % (link,paraviewPath,nm,lib)
            print "\t",cmd
            lns = os.popen(cmd)
            print "\t"+"".join(lns)
        if sameDir:
            if link[:6] in ["libvtk","libXdm","libKWC","libQVT","libVPI","libCos","libpro"]:
                cmd = "install_name_tool -change %s %s/%s %s" % (link,paraviewPath,link,lib)
                print "\t",cmd
                lns = os.popen(cmd)
                print "\t"+"".join(lns)
            

inpath =  "/".join(numpy.__path__[0].split("/")[:-1]+["paraview",])
inpath2 =  "/".join(numpy.__path__[0].split("/")[:-1]+["paraview","vtk"])
inpath3 =  "/".join(numpy.__path__[0].split("/")[:-1]+["vtk"])
inpath4 =  "/".join(numpy.__path__[0].split("/")[:-1]+["ParaView-3.11.1-py2.7.egg","paraview",])
inpath5 =  "/".join(numpy.__path__[0].split("/")[:-1]+["ParaView-3.11.1-py2.7.egg","paraview","vtk"])
inpath6 =  "/".join(numpy.__path__[0].split("/")[:-1]+["VTK-5.9.0-py2.7.egg","vtk"])
paraviewPath = "/".join(sys.prefix.split("/")[:-5]+["Externals","lib","paraview-3.11"]) #= '/usr/local/uvcdat/1.0.alpha/Externals/lib/paraview-3.11/'
def doPath(inpath,paraviewPath,sameDir=False):
    files = os.popen("ls %s" % inpath).readlines()
    for f in files:
        lib = inpath+"/"+f.strip()
        print lib
        change(lib,bad,paraviewPath,sameDir)
doPath(inpath,paraviewPath)
doPath(inpath2,paraviewPath)
doPath(inpath3,paraviewPath)
doPath(inpath4,paraviewPath)
doPath(inpath5,paraviewPath)
doPath(inpath6,paraviewPath)
doPath(paraviewPath,paraviewPath,True)


