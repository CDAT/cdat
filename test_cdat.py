#!/usr/bin/env python
version='%prog 1.0'
usage = "usage: %prog [options] PACKAGE1, PACKAGE2, CONTRIB1, CONTRIB2, ..."
import subprocess,os,sys
import optparse
import time
import bz2,ftplib
ftp_site = "climate.llnl.gov"
ftp_dir = "Shadow"
ftp_user = "cdat"
ftp_password = "Changeme1"

import cdat_info
default_time_format = "%Y-%m-%d %H:%M:%S"

def get_shadow_name(test_dir,test):
    fnm = os.path.join(test_dir,test)[:-3]+'.shadow.bz2'
    path = list(os.path.split(fnm))
    while path[0]!='':
        tmp = os.path.split(path.pop(0))
        path.insert(0,tmp[1])
        path.insert(0,tmp[0])
    fnm2 = '.'.join(path[1:])
    return fnm,fnm2

def get_shadow_ftp(test_dir,test):
    fnm,ftpnm = get_shadow_name(test_dir,test)
    f = open(fnm,"w")
    try:
        ftp=ftplib.FTP(ftp_site)
        ftp.login(ftp_user,ftp_password)
        ftp.cwd(ftp_dir)
        ftp.retrbinary('RETR %s' % ftpnm, f.write)
        ftp.close()
        f.close()
        f = open(fnm)
        s=f.read()
        f.close()
        s = bz2.decompress(s)
        f = open(fnm[:-4],"w") # open w/o bz2 ext
        f.write(s)
        f.close()
        os.remove(fnm)
    except Exception,err:
        f.close()
        os.remove(fnm)
        pass
    

def get_shadow_local(test_dir,test):
    fnm = os.path.join(test_dir,test)[:-3]+'.shadow'
    if os.path.exists(fnm):
        f=open(fnm,"r")
        s=f.read()
        f.close()
        shadow_dict=eval(s)
    else:
        shadow_dict={}
    return shadow_dict

def get_shadow(test_dir,test):
    # first try from ftp
    get_shadow_ftp(test_dir,test)
    return get_shadow_local(test_dir,test)

def set_shadow_local(test_dir,test,dict):
    try:
        fnm = os.path.join(test_dir,test)[:-3]+'.shadow'
        os.remove(fnm)
    except:
        pass
    try:
        fnm = os.path.join(test_dir,test)[:-3]+'.shadow.bz2'
        f=open(fnm,"w")
        s= bz2.compress(repr(dict))
        print >> f, s
        f.close()
    except Exception,err:
        pass
    return

def put_shadow_ftp(test_dir,test):
    fnm,ftpnm = get_shadow_name(test_dir,test)
    try:
        ftp=ftplib.FTP(ftp_site)
        ftp.login(ftp_user,ftp_password)
        ftp.cwd(ftp_dir)
        f=open(fnm)
        ftp.storbinary('STOR %s' % ftpnm, f)
        ftp.close()
        os.remove(fnm)
    except Exception,err:
        print 'Error putting ftp bz2',err
        pass
        
def set_shadow(test_dir,test,dict):
    set_shadow_local(test_dir,test,dict)
    if int(o.upload)>0:
        put_shadow_ftp(test_dir,test)
    return

def make_tests_string_machine(machine,dict):
    details=""
    details = "\t\t\tlast successful run: %s" % dict.get("last","never")
    if dict.has_key("time"):
        details+="\n\t\t\tduration (min,avg,max) %i, %i, %i seconds" % (dict["fastest"],dict["time"],dict["slowest"])
    if dict.has_key("count") and o.verbose>1:
        details+='\n\t\t\tSuccesfully tested %i times on at least : %i independent machines' % (dict["count"],len(dict["machines"]))
    return details

def make_tests_string(dict_all):
    details=""
    for os in dict_all.keys():
        details += "\n\t\tOS: %s" % os
        dict_os = dict_all[os]
        for v in dict_os.keys():
            details += "\n\t\t  Version: %s" % v
            dict_system = dict_os[v]
            for m in dict_system.keys():
                details += "\n\t\t   Machine: %s" % m
                dict=dict_system[m]
                details+='\n'+make_tests_string_machine(m,dict)
    return details

def run_dir(test_dir,lst):
    lst.sort()
    passed=True
    output={}
    for test in lst:
        if test[-3:]=='.py' and (test.lower()[:4]=='test' or test.lower()[:6]=='cdtest'):
            Dict_all = get_shadow(test_dir,test)
            if o.query_mode:
                output[(test_dir,test)]=Dict_all
                try:
                    fnm = os.path.join(test_dir,test)[:-3]+'.shadow'
                    os.remove(fnm)
                except:
                    pass
                continue
            myversion = ".".join(map(str,cdat_info.version()))
            dict_all = Dict_all.get(myversion,{})
            myos = os.uname()[0]
            system = os.uname()[2]
            machine = os.uname()[4]
            dict_os = dict_all.get(myos,{})
            dict_system = dict_os.get(system,{})
            dict = dict_system.get(machine,{})
            dict_system[machine] = dict
            dict_os[system] = dict_system
            dict_all[myos] = dict_os
            details = ""
            last = dict.get("last","1980-01-01 00:00:00") # ok ever ago!
            format = dict.get("format",default_time_format)
            tlast = time.strptime(last,format)
            delta = time.mktime(tlast)-time.mktime(time.strptime(o.date,o.format))
            if delta>0:
                if o.verbose>0:
                    print "\tRunning: %s" % (test)
                    print "\t\tSuccessful run newer than threshold %s vs %s " % (last,o.date)
                continue
            if o.verbose>0:
                print "\tRunning: %s" % (test)
                if o.verbose<3 or dict_all.keys()==[]:
                    details=make_tests_string_machine(machine,dict)
                else:
                    details+=make_tests_string(dict_all)
                print details
            t = time.time()
            out,err= run_test(os.path.join(test_dir,test))
            err2 = []
            for l in err:
                if l.find("Warning")>-1:
                    pass
                else:
                    err2.append(l)
            err=err2
            t2 = time.time()
            if err!=[]:
                passed = False
            if o.verbose>1:
                for l in out:
                    st='\t\t%s' % l.strip()
                    print st
            if o.verbose>0:
                if err!=[]:
                    print '\t        FAILED\n\n',err
                    if o.verbose>1:
                        for l in err:
                            st='\t\t%s' % l.strip()
                            print st
                else:
                    print '\t        PASSED\n\n'
                    runtime = int(t2-t)+1
                    fastest = dict.get("fastest",runtime+1)
                    if fastest>runtime:
                        fastest = runtime
                    dict["fastest"]=fastest
                    slowest = dict.get("slowest",runtime-1)
                    if slowest<runtime:
                        slowest = runtime
                    dict["slowest"]=slowest
                    dict["format"]=default_time_format
                    dict["last"] = time.strftime(default_time_format,time.localtime())
                    count=dict.get("count",0)
                    count+=1
                    dict["count"]=count
                    avg = dict.get("time",0.)*(count-1)
                    avg+=runtime
                    avg/=count
                    dict["time"] = avg
                    machines = dict.get("machines",[])
                    if int(o.upload)>1:
                        mymachine = os.uname()[1]
                    else:
                        mymachine = "private"
                    if not mymachine in machines:
                        machines.append(mymachine)
                        dict["machines"] = machines
                        
                    dict_system[machine] = dict
                    dict_os[system] = dict_system
                    dict_all[myos] = dict_os
                    Dict_all[myversion] = dict_all
                    output[(test_dir,test)]=dict
                    if out==[] or str(out[-1]).lower().find('skipped')==-1:
                        # ok the test havent been skipped
                        # we can replace stat file
                        set_shadow(test_dir,test,Dict_all)
                    
            if o.skip is False and passed is False:
                sys.exit()
    return output

def run_test(test):
    wd, test = os.path.split(test)
    cmd = 'cd %s ; %s %s' % (wd , os.path.join(sys.prefix,'bin','python'),test)
    if o.full_testing:
        cmd+=' --full --extended'
    if o.extended_testing:
        cmd += ' --extended'
    #print cmd
    P=subprocess.Popen(cmd,stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, close_fds=True,shell=True)
    
    out = P.stdout.readlines()
    err = P.stderr.readlines()
    rmv =[]
    for l in err:
        for e in o.error_strings:
            if l.find(e)>-1:
                rmv.append(l)
                break
    for l in rmv:
        err.remove(l)
        
    return out,err

format = default_time_format
date = time.strftime(format,time.localtime()) # Now!

p=optparse.OptionParser(version=version,usage=usage)

time_format_help_string = """format for time, default: %default                             
Format can be constructed from the following keys:                        
%a 	Locale's abbreviated weekday name.               
%A 	Locale's full weekday name. 	              
%b 	Locale's abbreviated month name.               
%B 	Locale's full month name. 	                 
%c 	Locale's appropriate date and time representation.                                      
%d 	Day of the month as a decimal number [01,31]. 	
%H 	Hour (24-hour clock) as a decimal number [00,23].                                            	
%I 	Hour (12-hour clock) as a decimal number [01,12].                                                      	
%j 	Day of the year as a decimal number [001,366]. 	                                             
%m 	Month as a decimal number [01,12]. 	                 
%M 	Minute as a decimal number [00,59]. 	                 
%p 	Locale's equivalent of either AM or PM.        
%S 	Second as a decimal number [00,61]. 	          
%U 	Week number of the year (Sunday as the first day of the week) as a decimal number [00,53]. All days in a new year preceding the first Sunday are considered to be in week 0. 	                      
%w 	Weekday as a decimal number [0(Sunday),6].                             	
%W 	Week number of the year (Monday as the first day of the week) as a decimal number [00,53]. All days in a new year preceding the first Monday are considered to be in week 0.                                           
%x 	Locale's appropriate date representation. 	         
%X 	Locale's appropriate time representation. 	            
%y 	Year without century as a decimal number [00,99].                        	              
%Y 	Year with century as a decimal number. 	              
%Z 	Time zone name (no characters if no time zone exists).                                               	
%% 	A literal "%" character.                   
"""

## Adds options to test utility
p.add_option("-a","--all","-A","--ALL",dest="all",help="Run test for ALL Packages and contributed Packages",action="store_true",default=False)
p.add_option("-P","--packages",dest="all_packages",help="Run test on all packages",action="store_true",default=False)
p.add_option("-C","--contribs",dest="all_contrib",help="Run test on all contributed packages",action="store_true",default=False)
p.add_option("-p","--package",dest="Packages",metavar="PACKAGE",help="Run test on this package",action="append",type="string",default=[])
p.add_option("-c","--contrib","--contributed",dest="Contribs",metavar="CONTRIBUTED",help="Run test on this contributed package",action="append",type="string",default=[])
p.add_option("-s","--stop","--noerror",dest="skip",help="Stop on errors (default: %default)",action="store_false",default=False)
p.add_option("-S","--nostop","--skip",dest="skip",help="Do not stop on errors",action="store_true",default=False)
p.add_option("-v","--verbose",metavar="LEVEL",dest="verbose",help="Level of verbosity (0, 1, 2 or 3), default is %default",type="choice",default="1",choices=("0","1","2","3"))
p.add_option("-u","--upload",metavar="LEVEL",dest="upload",help="Level of upload privacy (0, 1, or 2), 0 no data uploaded, 1 no private data uploaded, 2 uploads hostname, default is %default",type="choice",default="2",choices=("0","1","2"))
p.add_option("-e","--okerror",metavar="ERROR STRING",dest="error_strings",help="Identify 'none' error merror messages (removes lines in error messages containing this)",default=["ppmtogif","pnmcolormap","pnmremap","ppmtogif","ppmquant","pnmcrop","Definition of","DeprecationWarning","self.nxo"],action="append",type="string")
p.add_option("-d","--date",dest="date",type="string",help="Will run a test if last successfull time is older than 'date', default is now: %default                                      See --timeformat option for date format",default=date)
p.add_option("-f","--timeformat",dest="format",type="string",help=time_format_help_string,default=default_time_format)
p.add_option("-q","--query_mode",dest="query_mode",help="Runs a query of successfully run test only, does not execute anything",action="store_true",default=False)
p.add_option("-F","--full",dest="full_testing",help="Full testing (more detailed testing) default is %default",default=False,action="store_true")


# short test is default -jd082007
p.add_option("-E","--extended",dest="extended_testing",help="Extended testing (runs testing completely) default is %default",default=False,action="store_true")


(o,args) = p.parse_args()

if int(o.upload)==2 and o.query_mode is False:
    print 'Your upload level is set to 2\nThis means CDAT will recover your machine\'s name (only when running the test suite).\nTo turn this off use option: --upload=1 (no private data uploaded) or 0 (no data uploaded at all)'
    print "Your machine's name (%s) will be stored for statistical purposes only" % os.uname()[1]
    cont = raw_input("Do you wish to continue? (y/n) [y]")
    if not cont.lower() in ['','y','yes']:
        sys.exit()
try:
    time.strptime(o.date,o.format)
except:
    p.error('date must be in format: "%s", or specify format on command line (use --help)' % o.format)
# Handles case where nothing is passed!
if not (o.all_packages or o.all_contrib or o.all) and o.Packages==[] and o.Contribs==[] and args==[]:
    (o,args) = p.parse_args(["-h"])

if o.all:
    o.all_packages=True
    o.all_contrib=True

# Append all the Packages
packages=[]
pckgs = os.listdir("Packages")
pckgs.sort()
for pk in pckgs:
    if pk in ['cmor','cdms','regrid','Properties']:
        continue
    if os.path.isdir(os.path.join("Packages",pk)):
        lst=[]
        try:
            dr = os.path.join("Packages",pk,"Test")
            lst = os.listdir(os.path.join("Packages",pk,"Test"))
        except:
            pass
        try:
            lst = os.listdir(os.path.join("Packages",pk,"test"))
        except:
            pass
        if lst!=[]:
            packages.append(pk)
            
if o.all_packages:
    for pk in packages:
        if not pk in o.Packages:
            o.Packages.append(pk)
            
contribs=o.Contribs
if contribs==[]:
    pckgs = os.listdir("contrib")
    pckgs.sort()
    for pk in pckgs:
        if pk in ['spanlib']:
            try:
                import spanlib
            except:
                continue
        if os.path.isdir(os.path.join("contrib",pk)):
            lst=[]
            try:
                lst = os.listdir(os.path.join("contrib",pk,"Test"))
            except:
                pass
            try:
                lst = os.listdir(os.path.join("contrib",pk,"test"))
            except:
                pass
            if lst!=[] and pk not in o.Contribs:
                # first try to see if contrib has been built
                contribs.append(pk)
            
if o.all_contrib:
    for pk in contribs:
        if pk not in o.Contribs:
            o.Contribs.append(pk)

#Now adds the extra arguments
for pk in args:
    ok=False
    if pk in packages:
        ok = True
        if not pk in o.Packages:
            o.Packages.append(pk)
    if pk in contribs:
        ok = True
        if not pk in o.Contribs:
            o.Contribs.append(pk)
    if not ok:
        if o.skip:
            print 'Will skip Package:',pk
        else:
            print "Package %s does not exists or has not test suite" % pk
            print 'type "%s --help" for help and usage' % sys.argv[0]
            sys.exit()
            
        
# Ok now runs the test to see if packages are good
skipped=[]
for pk in o.Packages:
    if not pk in packages:
        if o.skip:
            print 'Will skip Package:',pk
            skipped.append(pk)
        else:
            print "Package %s does not exists or has no test suite" % pk
            print 'type "%s --help" for help and usage' % sys.argv[0]
            sys.exit()
for pk in skipped:
    o.Packages.remove(pk)
# Ok now runs the test to see if contribs are good
skipped=[]
for pk in o.Contribs:
    if not pk in contribs:
        if o.skip:
            print 'Will skip Contributed Package:',pk
            skipped.append(pk)            
        else:
            print "Contributed Package %s does not exists or has not test suite" % pk
            print 'type "%s --help" for help and usage' % sys.argv[0]
            print 'valid contributed packages: %s' % ' '.join(contribs)
            sys.exit()
for pk in skipped:
    o.Contribs.remove(pk)
o.verbose=int(o.verbose)
results ={}
for pk in o.Packages:
    print "Running Test on Official Package: %s" % pk
    test_dir = os.path.join("Packages",pk,"Test")
    try:
        lst = os.listdir(test_dir)
    except:
        test_dir = os.path.join("Packages",pk,"test")
        lst = os.listdir(test_dir)
    tmp = run_dir(test_dir,lst)
    for k in tmp.keys():
        results[k]=tmp[k]
for pk in o.Contribs:
    print "Running Test on Contributed Package: %s" % pk
    test_dir = os.path.join("contrib",pk,"Test")
    try:
        lst = os.listdir(test_dir)
    except:
        test_dir = os.path.join("contrib",pk,"test")
        lst = os.listdir(test_dir)
    tmp = run_dir(test_dir,lst)
    for k in tmp.keys():
        results[k]=tmp[k]



import cdat_info
Packages=[]
OS=[]
Versions=[]
Machines=[]
CDATVersions=[]
#code to display nicely all the results
if o.query_mode:
    for test in results.keys():
        pnm =test[0]
        if not pnm in Packages:
            Packages.append(pnm)
        CDATVersions=results[test]
        oses = CDATVersions.get(str(cdat_info.version()),{})
        for aos in oses.keys():
            if not aos in OS:
                OS.append(aos)
            versions = oses[aos]
            for v in versions.keys():
                syst = versions[v]
                for asys in syst:
                    full = "%s_%s_%s" % (aos,v,asys)
                    if not full in Versions:
                        Versions.append(full)
                    res = syst[asys]
                    machines = res["machines"]
                    for m in machines:
                        if not m in Machines:
                            Machines.append(m)
    print 'Your version:',cdat_info.version()
    print 'Total Test:',len(results.keys())
    print 'Total Packages:',len(Packages)
    print 'Total OS:',len(OS),'---',', '.join(OS)
    print 'Total OS Versions:',len(Versions)
    print 'Total Independent Machines:',len(Machines)
## else:
##     for test_dir,test in results.keys():
##          print '\n\n'
##          fn = test_dir+test
##          print fn,'--------------'
##          tr = results[test_dir,test]
##          for t in tr:
##               print '\t',t,':  ' ,tr[t] 
