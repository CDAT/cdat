import os,sys

svn=sys.argv[1]

git = os.popen("git status").readlines()

modfiles =[]
delfiles=[]
newfiles=[]
for l in git:
    if l.find("new file:")>-1:
        newfiles.append(l.split()[3])
    if l.find("modified:")>-1:
        modfiles.append(l.split()[2])
    if l.find("deleted:")>-1:
        delfiles.append(l.split()[2])

for f in modfiles+newfiles:
    cmd = "cp -pf %s %s/%s" % (f,svn,f)
    print 'Cp:',cmd
    os.popen(cmd).readlines()

for f in newfiles:
    cmd = "cd %s ; svn add %s" % (svn,f)
    print 'svn add :',cmd
    os.popen(cmd).readlines()

for f in delfiles:
    cmd = "cd %s ; svn delete --force %s" % (svn,f)
    print 'svn del :',cmd
    os.popen(cmd).readlines()
