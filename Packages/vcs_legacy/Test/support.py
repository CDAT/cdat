import sys,random,os,shutil,time,bz2,ftplib,time
global counter,pause,diff
import urllib
import cdat_info

version=".".join(map(str,cdat_info.version()[:2]))
counter=0
bg=1
dogui=False
pause = False
diff = 1.25 # % of diff ok
ftp_site = "climate.llnl.gov"
ftp_dir = "SVG"
ftp_user = "cdat"
ftp_password = "Changeme1"

keep_local = False

def check_plot(x):
    global counter,pause,diff
    n =random.randint(0,1000)
    fnm = "tmp_%i.svg" % n
    while os.path.exists(fnm):
            n =random.randint(0,1000)
            fnm = "tmp_%i.svg" % n
    if pause:
        time.sleep(1)
    x.svg(fnm)
    try:
        os.makedirs("Good")
    except:
	pass
    try:
	os.makedirs("Good/Good_Test")
    except Exception,err:
        pass
    try:
        os.makedirs("Good/Good_Test")
    except Exception,err:
        pass
    try:
        os.makedirs("Test_Test")
    except Exception,err:
        pass
    fgood = "Good/Good_%s_%i_bg_%i_%s.svg" % (sys.argv[0][:-3],counter,bg,version)
    print 'fgood is:',fgood
    rfnm = os.path.split(fgood)[-1]
    # if bz2 file is not here try to get it from ftp
    remote = False
    if not os.path.exists(fgood+'.bz2'):
        print 'no file locally trying ftp'
        # put here code to go and try get it from ftp site
        f=open(fgood+".bz2","wb")
        try:
            ftp=ftplib.FTP(ftp_site)
            ftp.login(ftp_user,ftp_password)
            ftp.cwd(ftp_dir)
            ftp.retrbinary('RETR %s.bz2' % rfnm, f.write)
            print 'Got file from ftp'
            remote = True
            f.close()
        except Exception,err:
            print 'Error:',err
            f.close()
            os.remove(fgood+'.bz2')
            print 'File not on ftp'
            pass

        
    # First uncompress the good file
    if os.path.exists(fgood+'.bz2'):
        f=open(fgood+".bz2")
        s=f.read()
        f.close()
        s = bz2.decompress(s)
        f=open(fgood,"w")
        f.write(s)
        f.close()
        
    if os.path.exists(fgood):
        f=open(fgood)
        good = f.read()
        f.close()
        f=open(fnm)
        test=f.read()
        f.close()
        fnmtest = "Test_%s_%i.svg" % (sys.argv[0][:-3],counter) 
        shutil.copy(fnm,fnmtest)
        ## cleaning up known diff which are spaces
        good = good.replace(" ",'')
        test = test.replace(" ",'')

        ftest=open("A.txt","w")
        ftest.write(good)
        ftest.close()
        ftest=open("B.txt","w")
        ftest.write(test)
        ftest.close()
        rdiff = os.popen('diff A.txt B.txt').read()
        blocks=rdiff.split("---")
        prev = blocks[0].strip()
        same_blocks = 0
        total_diff = 0
        #print 'Ok orig diff was:',rdiff
        for block in blocks[1:]:
            # First figures out the numbers in what we kept from before
            i=prev.find("<")
            left=prev[i:].strip().replace("<"," ").split()

            # First figures out what we keep in this block for NOW
            i = block.find("<")
            right = block[:i-3] # to be before the \n
##                     print "right1:",right
            j=right.rfind("\n")
            right=right[:j]
            right=right.replace(">"," ").split()
            # Now figures out what we will keep
            prev = block[i:].strip()
            same = 0
            for i in range(min(len(left),len(right))):
                try:
                    if left[i]==right[i] :
                        same+=1
                    else:
                        try:
                            adiff = abs(float(left[i])-float(right[i]))
                            apdiff = adiff/abs(float(left[i]))*100.
                            ## print float(left[i]),adiff,apdiff
                            if apdiff<5. or adiff<1.e-3: # 5% diff or cerysmall numbers
                                same+=1
                            else:
                                total_diff+=len(left[i])
                        except Exception,err:
                            if left[i].find("Encoding")>-1 or left[i].find("CharStrings")>-1 or left[i].find("FontName")>-1:
                                pass
                            else:
                                total_diff+=len(left[i])
                            pass
                except:
                    total_diff+=len(left[i])
                    pass
            if same == len(left): same_blocks+=1
        sdiff=total_diff
        fsize = os.path.getsize(fgood)
        pdiff=float(sdiff)/fsize*50. # 50=100./2.
        os.remove("A.txt")
        os.remove("B.txt")
        if pdiff>diff:
            cwd = os.getcwd()
            raise Exception,"Error on test plot number %i, different plot, Test file name: %s differs from good: %s %% diff is %f\n" % (counter,os.path.join(cwd,fnmtest),os.path.join(cwd,fgood),pdiff)
##         elif pdiff>0.:
##             print 'Warning: slight difference in test %i (%f%%)' % (counter,pdiff)
##             os.remove(fnmtest)
        else:
            print  counter,'tested ok (%f %% difference which is within expected diff)' % (pdiff)
            os.remove(fnm)
            os.remove(fnmtest)
            
        os.remove(fgood)
        if remote and keep_local is False:
            os.remove(fgood+'.bz2')

    else:
        shutil.move(fnm,fgood)
        print 'Created "Good" file %i' % counter
        s= os.path.getsize(fgood)
        if s<100000:
            print 'Size seems extremely small check output'
        f=open(fgood)
        s=f.read()
        f.close()
        s = bz2.compress(s)
        f=open(fgood+'.bz2','w')
        f.write(s)
        f.close()
        os.remove(fgood)
        print 'Check Skipped No Original File could be found'
        
    counter+=1

def short_test():

     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     return
