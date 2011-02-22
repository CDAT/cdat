import sys,os

cnm = sys.argv[1]
pt1="Lib/*.py"
pt2="/Users/doutriaux1/tmp/cdat/Packages/qtbrowser/Lib/*.py"

def lookup(files,cnm):
    p1 = None
    for fnm in files:
        f=open(fnm.strip()).read()
        i1 = f.find("\nclass %s(" % cnm)
        if i1 >-1:
            print 'Found in file: ',fnm.strip()
            i2=f[i1+1:].find("\nclass ")
            if i2==-1:
                p1=f[i1:]
            else:
                p1=f[i1:i1+i2]
            print 'i1,i2:',i1,i2
            break
    return p1

f1=os.popen("ls %s" % pt1).readlines()
p1 = lookup(f1,cnm)
if p1 is None:
    print 'Could not find class %s in %s' % (cnm, pt1)
    sys.exit()

f2=os.popen("ls %s" % pt2).readlines()
p2 = lookup(f2,cnm)
if p2 is None:
    print 'Could not find class %s in %s' % (cnm,pt2)
    sys.exit()

f1=open("crap_diff_1.txt","w")
print >> f1,p1
f1.close()

f2=open("crap_diff_2.txt","w")
print >> f2,p2
f2.close()

print 'Diff:'
d=os.popen("diff crap_diff_1.txt crap_diff_2.txt").readlines()
print "".join(d)
