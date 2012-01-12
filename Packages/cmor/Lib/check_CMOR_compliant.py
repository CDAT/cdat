import cdms2
import os
import unidata
import genutil
import numpy
import cdtime
import cmor
import time
VERBOSE=-999

#ok now we are going online to pull the file that has the control md5s
try:
    f=open("Tables/md5s")
    ctrl_md5s=eval(f.read())
except:
    try:
        import urllib2
        url=urllib2.urlopen("http://esgf.org/gitweb/?p=cmip5-cmor-tables.git;a=blob_plain;f=Tables/md5s;hb=HEAD")
        ctrl_md5s=eval(url.read())
        url.close()
        del(url)
    except:
        ctrl_md5s={}


class CMORError(Exception):
    def __init__(self,value=None):
        self.value=value
    def __str__(self):
        color=31
        msg = "%c[%d;%d;%dm%s%c[%dm" % (0X1B,2,color,47,self.value,0X1B,0)
        return msg

def hyphenize(value):
    out=str(value)
    for c in [' ',"_",'(',')','.',';',',','[',']',':','/','*','?','<','>','"',"'",'{','}','&']:
        out=out.replace(c,"-")
    return out

def drs_check_pos(i,path,fout,noerror,val,name):
    dic = { 1:"",2:"second to ",3:"third to "}
    posstr = dic.get(i,"%ith to " % i)
    if path[-i]!=val:
        return manageLog(fout, noerror, 'Error file directory does not conform to DRS structure, %slast directory should be the %s (%s vs %s)' % (posstr, name, path[-i],val))
    return 0

def addcoloring(fout,error):
    #warning blue: 34
    #error red: 31
    if error == cmor.CMOR_NORMAL:
        color=31
    elif error == cmor.CMOR_WARNING:
        color=34
    else:
        return
    print >> fout, "%c[%d;%d;%dm" % (0X1B,2,color,47)
    return

def delcoloring(fout):
    print >> fout,"%c[%dm" % (0X1B,0)
    return
    
def manageLog(fout,error,*msg_bits):
    """ Print error message or raise an exception depending on noerror flag)"""
    msg=''
    for m in msg_bits:
        msg+=" "+str(m)
    msg=msg.lstrip()
        
##     print >>fout, 'No error is:',noerror
    if error == cmor.CMOR_CRITICAL:
        raise CMORError,[msg,]
    elif error == VERBOSE:
        fout.write( msg)
        if msg_bits[-1]!="":
            print >> fout
        return
        
    addcoloring(fout,error)
    print >>fout, '@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%'
    print >>fout, '@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%'
    if error == cmor.CMOR_NORMAL:
        print >>fout, '@#%@#%@#%@#%@#%           ERROR          %@#%@#%@#%@#%@#%@#%@#%'
    elif error == cmor.CMOR_WARNING:
        print >>fout, '@#%@#%@#%@#%@#%          WARNING         %@#%@#%@#%@#%@#%@#%@#%'
    print >>fout, '@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%'
    print >>fout, '@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%'
    delcoloring(fout)
    print >>fout
    addcoloring(fout,error)
    print >>fout, msg
    delcoloring(fout)
    print >>fout
    addcoloring(fout,error)
    print >>fout, '@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%'
    print >>fout, '@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%@#%'
    delcoloring(fout)
    print >>fout
    return 1

def split_expt_ids(val):
    long_vals=[]
    short_vals = []
    for v in val:
        i = v.find("'")
        if i>-1:
            j=v[i+1:].find("'")
            if j>-1:
                long_vals.append(v[:i])
                short_vals.append(v[j+i+2:])
            else:
                long_vals.append(v)
                short_vals.append("")
        else:
            long_vals.append(v)
            short_vals.append("")
    return long_vals,short_vals

def readTable(table):
    lists_kw=['requested','bounds_requested','z_factors','z_bounds_requested','dimensions','required','ignored','optional']
    f=open(table,'r')
    import hashlib
    m5=hashlib.md5(f.read())
    m5=m5.hexdigest()
    f.seek(0)
    ln=f.readlines()
    f.close()
    header=1
    gen_attributes={'actual_md5':m5}
    while header:
        l = ln.pop(0)[:-1]
        l=l.strip()
        if l=='' or l[0]=='!':
            continue
        sp=l.split('_entry')
        if len(sp)>1:
            ln.insert(0,l+'\n')
            header=0
            continue
        sp=l.split(':')
        kw=sp[0]
        st="".join(sp[1:])
        st=st.split('!')[0].strip()
        if st[0]=="'":
            st=st[1:-1]
        if gen_attributes.has_key(kw):
            if isinstance(gen_attributes[kw],str):
                gen_attributes[kw]=[gen_attributes[kw],st]
            else:
                gen_attributes[kw].append(st)
        else:
            gen_attributes[kw]=st
    e={} # entries dictionnary
    while len(ln)>0:
        l = ln.pop(0)
        sp=l.split('_entry:')
        entry_type=sp[0]
        entry=sp[1].strip()
        if not e.has_key(entry_type):
            e[entry_type]={}
        e[entry_type][entry]=e[entry_type].get(entry,{})
            
##         print >>fout, e[entry_type][entry]
        cont=1
        while cont:
            l = ln.pop(0)[:-1]
            l=l.strip()
            if l=='' or l[0]=='!':
                if len(ln)==0:
                    cont=0
                continue
            sp=l.split('_entry:')
            if len(sp)>1:
                ln.insert(0,l+'\n')
                cont=0
            sp=l.split(':')
            kw=sp[0].strip()
            val=":".join(sp[1:]).split('!')[0].strip()
            ## print  'dic is:',e[entry_type][entry]
            if e[entry_type][entry].has_key(kw):
                if kw in lists_kw:
                    e[entry_type][entry][kw]="".join(e[entry_type][entry][kw])
                e[entry_type][entry][kw]+=' '+val
            else:
                e[entry_type][entry][kw]=val
##             print >>fout, 'After:',e[entry_type][entry][kw]
            if kw in lists_kw:
##                 print >>fout, 'splitting:',kw,e[entry_type][entry][kw].split()
                e[entry_type][entry][kw]=e[entry_type][entry][kw].split()
            if len(ln)==0:
                cont=0
    e['general']=gen_attributes
##     for type in e.keys():
##         print >>fout, 'Type:',type
##         for k in e[type].keys():
##             print >>fout, '\t Entry:',k
##             for a in e[type][k].keys():
##                 print >>fout, '\t\t',a,':',e[type][k][a]
    return e


def checkCMOR(fout,file,table,noerror=cmor.CMOR_CRITICAL,variable=None,from_bounds=None,other_tables=None,dodrs=True):

    req_glbl_att = ['title','Conventions','source','experiment','source','institution','project_id','table_id','realization',]
    opt_glbl_att = ['cmor_version','history','references','comment','parent_experiment']

    nerr=0
    nwarn=0
    ncheck=0
    IPCC_std_vars=[]
    IPCC_std_axes=[]
    
    tables=[table,]
    if other_tables is not None:
        for t in other_tables:
            if not t in tables and t is not None:
                tables.append(t)

    etables = []
    for tbl in tables:
        manageLog(fout,VERBOSE, 'Reading table:',tbl)
        e=readTable(tbl)
        etables.append(e)
        Vars=e['variable']
        Axes=e['axis']
        extra_req = e['general'].get('required_global_attributes','').split()
        for ex in extra_req:
            if not ex in req_glbl_att:
                req_glbl_att.append(ex)
        for v in Vars.keys():
            IPCC_std_vars.append(Vars[v].get('out_name',v))
        for a in Axes.keys():
            onm = Axes[a].get('out_name',a)
            if not onm in IPCC_std_axes:
                IPCC_std_axes.append(onm)
    ##         if IPCC_std_axes[-1]=='lev' :
    ##             IPCC_std_axes.pop(-1)
    ##             IPCC_std_axes.append('eta')
    ##             Axes[a]['out_name']='eta'

    ver = e['general'].get('cmor_version',2.0)
    spver = ver.split('.')
    major = int(spver[0])
    if major>1:
        req_glbl_att+=["contact",'experiment_id','physics_version','initialization_method','institute_id','institution','tracking_id','product','frequency','model_id','creation_date','frequency','modeling_realm']
    else:
        opt_glbl_att+=["contact",'experiment_id','physics_version','initialization_method','institute_id','institution','tracking_id','product','frequency','model_id','forcing','creation_date','frequency','modeling_realm']
    if isinstance(file,str):
        fnm=file
        file=cdms2.open(file)
    elif isinstance(file,cdms2.dataset.CdmsFile):
        fnm=str(file).split('file')[1].split(',')[0].strip()[1:-1]
    else:
        nerr+=manageLog(fout, noerror, 'You  must pass a file name or cdms2 file object')

    if fnm.split("_")[-1]=="clim.nc":
        is_clim = True
    else:
        is_clim = False


    pthroot, shrt_fnm = os.path.split(fnm)
    if variable is None:
        manageLog(fout,VERBOSE, 'Checking file argument',IPCC_std_axes)
        manageLog(fout,VERBOSE, 'Checking path structure for path:',pthroot)
        manageLog(fout,VERBOSE, 'Checking file structure for file:',fnm)
        print >> fout, 'Checking the file starts with variable name'
        ok = False
        for v in IPCC_std_vars:
            n = len(v)
            if shrt_fnm[:n] == v and shrt_fnm[n]=='_':
                ok = True
                break
        if not ok:
            nerr+=manageLog(fout, noerror, ''+shrt_fnm+' does not start with standard %s variable name followed by _' % file.project_id)
        vf=v
        #Check the variable is actually in the file
        if not v in file.variables.keys():
            nerr+=manageLog(fout,noerror,"Your file name says it contains variable",v,"but it is not in your file, your file contains:",file.variables.keys())
        if hasattr(file,'cmor_version'):
            up = False
            rew = False
            ver = getattr(file,'cmor_version')
            if isinstance(ver,numpy.ndarray): ver = '%g' % float(ver[0])
            spver = ver.split('.')
            fmajor = int(spver[0])
            if len(spver)>1 :
                minor = int(spver[1])
                if len(spver)>2:
                    patch = int(spver[2])
                else:
                    patch = 0
            else:
                minor=0
                patch=0
            if fmajor>cmor.CMOR_VERSION_MAJOR:
                up = True
            elif fmajor<cmor.CMOR_VERSION_MAJOR:
                rew=True
            else: # Major matches
                if minor>cmor.CMOR_VERSION_MINOR:
                    up = True
                elif minor<cmor.CMOR_VERSION_MINOR:
                    rew = True
                else: # minor matches
                    if patch>cmor.CMOR_VERSION_PATCH:
                        up = True
                    elif patch<cmor.CMOR_VERSION_PATCH:
                        rew = True

            ncheck+=1
            if up:
                if fmajor==cmor.CMOR_VERSION_MAJOR:
                    nwarn+=manageLog(fout,cmor.CMOR_WARNING,"You are using cmor version: %i.%i.%i, these files have been written with version: %i.%i.%i, you should upgrade your cmor" % (cmor.CMOR_VERSION_MAJOR,cmor.CMOR_VERSION_MINOR,cmor.CMOR_VERSION_PATCH,fmajor,minor,patch))
                else:
                    nerr+=manageLog(fout,noerror,"You are using cmor version: %i.%i.%i, these files have been written with version: %i.%i.%i, you need to upgrade cmor to check these files" % (cmor.CMOR_VERSION_MAJOR,cmor.CMOR_VERSION_MINOR,cmor.CMOR_VERSION_PATCH,fmajor,minor,patch))

            ncheck+=1
            if rew:
                nwarn+=manageLog(fout,cmor.CMOR_WARNING,"You are using cmor version: %i.%i.%i, these files have been written with version: %i.%i.%i, you should consider rewriting these files" % (cmor.CMOR_VERSION_MAJOR,cmor.CMOR_VERSION_MINOR,cmor.CMOR_VERSION_PATCH,fmajor,minor,patch))
                
        ## 32bit systems only
        ## if os.uname()[-1].find("64")==-1: # old way would fail on some system
        if int(platform.architecture()[0].replace('bit', '')) < 64:
            sz=os.path.getsize(fnm)
            manageLog(fout,VERBOSE, 'Checking file size (32bit systems only):',sz)
            if sz>2**31:
                if major>1:
                    ncheck+=1
                    nwarn+=1
                    ec =cmor.CMOR_WARNING
                else:
                    ec=noerror
                manageLog(fout,ec, 'File size too large',sz,' (>2Gb)!')
                #nerr+=manageLog(fout, noerror, 'file size too large (>2Gb)!')

        manageLog(fout,VERBOSE, 'Checking that file contains required global attributes')
        for att in req_glbl_att:
            val=e['general'].get('expt_id_ok',None)
            long_vals,shrt_vals = split_expt_ids(val)
            if not hasattr(file,att) and not att in shrt_vals:
                i = req_glbl_att.index(att)
                if i==0 or req_glbl_att[i-1] not in shrt_vals:
                    nerr+=manageLog(fout, noerror, 'File must have global attribute: '+att)
                else:
                    pr = req_gbl_att[i-1]
                    expt = getattr(file,"experiment","")
                    shrt = shrt_vals[long_vals.index(expt)]
                    if shrt == pr:
                        nerr+=manageLog(fout, noerror, 'File must have global attribute: '+att) 
            fval=getattr(file,att,'')
            if att=='experiment_id':
                ok = False
                for e in etables:
                    val=e['general'].get('expt_id_ok',None)
                    long_vals,shrt_vals = split_expt_ids(val)
                    for lv in shrt_vals:
                        if fval==lv or (lv[-4:] =='XXXX' and fval[:-4]==lv[:-4]) or (lv[-4:] =='DDHH' and fval[:-10]==lv[:-10]):
                            ok = True
                            break
                if not ok: 
                    nerr+=manageLog(fout, noerror, 'experiment_id file attribute must be one of : %s, you have: %s' % (str(shrt_vals), fval) )
            elif att=='experiment':
                ok = False
                for e in etables:
                    val=e['general'].get('expt_id_ok',None)
                    long_vals,shrt_vals = split_expt_ids(val)
                    for lv in long_vals:
                        if fval==lv or (lv[-4:] =='XXXX' and fval[:-4]==lv[:-4]) or (lv[-4:] =='DDHH' and fval[:-10]==lv[:-10]):
                            ok = True
                            break
                if not ok: 
                    nerr+=manageLog(fout, noerror, 'experiment file attribute must be one of : %s, you have: %s' % (str(long_vals), fval) )
            elif att=='parent_experiment_id':
                if fval == getattr(file,"experiment_id",""):
                    nerr+=manageLog(fout,noerror,"parent_experiment_id and experiment_id cannot be the same you have %s for both" % fval)
                ok = False
                for e in etables:
                    val=e['general'].get('expt_id_ok',None)
                    long_vals,shrt_vals = split_expt_ids(val)
                    shrt_vals.append("N/A")
                    for lv in shrt_vals:
                        if fval==lv or (lv[-4:] =='XXXX' and fval[:-4]==lv[:-4]) or (lv[-4:] =='DDHH' and fval[:-10]==lv[:-10]):
                            ok = True
                            break
                if not ok: 
                    nerr+=manageLog(fout, noerror, 'parent_experiment_id file attribute must be one of : %s, you have: %s' % (str(shrt_vals), fval) )
            elif att == 'forcing':
                sp = fval.split("(")[0].split(',')
                forcings=e['general'].get("forcings")
                for vl in sp:
                    if not vl.strip() in forcings:
                        nerr+=manageLog(fout,noerror,"file attribute forcing must be a comma separated list with values in: %s, yours is: %s (offending value: %s)" % (forcings,fval,vl.strip()))
            elif att == 'frequency':
                if not fval in ['yr','mon','day','6hr','3hr','subhr','fx','monClim']:
                    nerr+=manageLog(fout,noerror, 'frequency must be one of:','yr','mon','day','6hr','3hr','subhr','fx','monClim')
            elif att in ['realization']:
                if isinstance(fval,numpy.ndarray):
                    if len(fval)>1:
                        nerr+=manageLog(fout, noerror, 'realization attribute must be an integer')
                    fval=fval[0]
                if not isinstance(fval,(int,numpy.int,numpy.int32)):
                    nerr+=manageLog(fout, noerror, 'realization attribute must be an integer')
            elif att in ['table_id']:
                manageLog(fout,VERBOSE, '\ttable_id is: ', fval)
            elif att == "creation_date":
                # checks that it matches: YYYY-MM-DDTHH:MM:SSZ
                fmt = "%Y-%m-%dT%H:%M:%SZ"
                try:
                    t =time.strptime(fval,fmt)
                except:
                    nerr+=manageLog(fout, noerror, 'Creation Date must be in format: %s yours is: %s' % (fmt,fval))
            elif att == "branch_time":
                try:
                    myval=float(fval)
                except:
                    nerr+=manageLog(fout, noerror, 'branch_time must be convertible to float, you have %s' % (fval))
                if getattr(file,"parent_experiment_id","").strip()=="N/A":
                    if float(fval)!=0.:
                        nerr+=manageLog(fout, noerror, 'if global attribute parent_experiment_id is N/A then branch_time must be 0., you have %s' % (fval))

                
            elif att == "project_id":
                if e['general'].get("project_id") != fval:
                    nerr+=manageLog(fout, noerror, 'project_id must be %s' % (e['general'].get("project_id")))
            else:
                val=e['general'].get(att,None)
                if isinstance(fval,str) : fval=fval.strip().lower()
                if isinstance(val,str) : val=val.strip().lower()
                if val is not None:
                    if isinstance(fval,str):
                        res=fval.find(val)
                        if res==-1:
                            res=False
                        else:
                            res=True
                    else:
                        res=fval==val
                        manageLog(fout,VERBOSE, '**************',att,val,fval)
                    if not res:
                        nerr+=manageLog(fout, noerror, 'attribute '+att+' must be set to: -'+val+'- +'+fval+'+ '+str(res))


        for att in opt_glbl_att:
            ncheck+=1
            if not hasattr(file,att):
                nwarn+=1
                manageLog(fout,cmor.CMOR_WARNING, '\t\tIt is often helpful to define the global attribute: ',att)

        for att in file.attributes.keys():
            ncheck+=1
            if not att in req_glbl_att and not att in opt_glbl_att:
                nwarn+=1
                manageLog(fout,cmor.CMOR_WARNING, '\t\tYou have global attribute: %s which is neither required nor optional ' % att)


        if major>=2: # more file structure there
            try:
                tmp_tbl_nm = getattr(file,'table_id')
                tbl_id = tmp_tbl_nm.split()[1]
                tbl_date= tmp_tbl_nm.split('(')[1].split(')')[0].strip()
                try:
                    tbl_md5 = tmp_tbl_nm.split('(')[1].split(')')[1].strip()
                    if len(tbl_md5)==32:
                        ncheck+=1
                        #ok it looks like we got an md5 tag
                        if tbl_md5!=e['general']['actual_md5']:
                            nwarn+=1
                            manageLog(fout,cmor.CMOR_WARNING, '\t\tYour file claims it has been generated with a table whose md5 was %s, but you are sending a table with md5: %s to the checker ' % (tbl_md5,e['general']['actual_md5']))
                        ncheck+=1
                        pmd5s=ctrl_md5s.get(file.project_id,{})
                        if pmd5s=={}:
                            nwarn+=1
                            manageLog(fout,cmor.CMOR_WARNING, '\t\tCould not obtain any control md5s for any table for project %s ' % (file.project_id))
                        else:
                            ncheck+=1
                            tmd5s=pmd5s.get(tbl_id,{})
                            if tmd5s=={}:
                                nwarn+=1
                                manageLog(fout,cmor.CMOR_WARNING, '\t\tCould not obtain any control md5s for table %s for project %s ' % (tbl_id,file.project_id))
                            else:
                                ncheck+=1
                                ctrlmd5=tmd5s.get(tbl_date,None)
                                if ctrlmd5 is None:
                                    nwarn+=1
                                    manageLog(fout,cmor.CMOR_WARNING, '\t\tCould not obtain control md5s for table %s for project %s dated on %s, valid tables dates are: ' % (tbl_id,file.project_id,tbl_date),sorted(tmd5s.keys()))
                                else:
                                    ncheck+=1
                                    if ctrlmd5!=tbl_md5:
                                        nwarn+=1
                                        manageLog(fout,cmor.CMOR_WARNING, '\t\tYour file claims it has been ran through table id %s, dated %s for project_id %s, with an md5 of: %s, but our control files indicate the md5 should be: %s' % (tbl_id,tbl_date,file.project_id,tbl_md5,ctrlmd5))
                                        

                        
                except Exception,err:
                    #no md5 stored in file
                    pass
                ttbl_id = e['general'].get("table_id").split()[1]
                ttbl_date = e['general'].get("table_date").strip()
                if tbl_date!=ttbl_date:
                    nwarn+=1
                    ncheck+=1
                    manageLog(fout,cmor.CMOR_WARNING,"File says table date was %s, but the table you passed to the checker is dated from: %s" %( tbl_date,ttbl_date))

                if tbl_id!=ttbl_id:
                    nerr+=manageLog(fout, noerror, 'your file indicates a table id of %s while your table id is %s' % (tbl_id,ttbl_id))
                
            except:
                manageLog(fout,VERBOSE,"File says table is %s, this is not a correct name, correct format is something like: Table 3hr (24 May 2010) af8b1d3d63376942a55d779d0fb9f504" % (tmp_tbl_nm))



            sp = shrt_fnm.split(v)[1].split("_")
            t = file[v].getTime()
            if t is not None:
                n=6
                t=t.clone()
            else:
                n=4
            if is_clim:
                n+=1
            expt = getattr(file,"experiment","")
            try:
                shrt = shrt_vals[long_vals.index(expt)]
            except:
                #Ok we must be in one of the XXXX or DDHH cases...
                for i in range(len(long_vals)):
                    if long_vals[i][:-4]==expt[:-4]:
                        shrt=shrt_vals[i][:-4]+expt[-4:]
                        break
                    if long_vals[i][:-10]==expt[:-10]:
                        shrt=shrt_vals[i][:-10]+expt[-10:]
                        break

            if shrt=="":
                n-=1
                spoffset=-1
            else:
                spoffset=0
                
            print len(sp)
            if len(sp)<n:
                nerr+=manageLog(fout, noerror, 'your file name does not seem to match the profile: varid_tableid_modelid_exptid_rid[iid][pid][_startdate-enddate][_suffix][_clim].nc')
                
            if sp[1]!=tbl_id:
                nerr+=manageLog(fout, noerror, 'your file name indicates a table id of %s while your table id should be %s' % (sp[1],tbl_id))

            if sp[2]!=getattr(file,'model_id'):
                fmodid = hyphenize(getattr(file,'model_id'))
                if sp[2]!=fmodid:
                    nerr+=manageLog(fout, noerror, 'your file name indicates model_id is: %s but your file says it is: %s' % (sp[2],getattr(file,'model_id')))
            if shrt!="":
                if shrt!=sp[3]:
                    nerr+=manageLog(fout, noerror, 'your file name indicates exp_id is: %s but your file says it should be: %s' % (sp[3],shrt))

            real = sp[4+spoffset]
            rsp=real.split("p")
            if hasattr(file,"physics_version"):
                if len(rsp)==1:
                    nerr+=manageLog(fout, noerror, 'your file name does not indicate physics_version but your file says it should be: %s' % (file.physics_version))
                elif int(rsp[0].split('i')[0][1:])!=int(file.physics_version):
                    nerr+=manageLog(fout, noerror, 'your file name indicates physics_version is: %s but your file says it should be: %s' % (rsp[1],file.physics_version))
            elif len(rsp)!=1:
                    nerr+=manageLog(fout, noerror, 'your file name indicates physics_version to be %s but your file says it has not physics: %s' % (rsp[1]))

            real=rsp[0]
            rsp=real.split("i")
            if hasattr(file,"initialization_method"):
                if len(rsp)==1:
                    nerr+=manageLog(fout, noerror, 'your file name does not indicate initialization_method but your file says it should be: %s' % (file.initialization_method))
                elif int(rsp[1])!=int(file.initialization_method):
                    nerr+=manageLog(fout, noerror, 'your file name indicates initialization_method is: %s but your file says it should be: %s' % (rsp[1],file.initialization_method))
            elif len(rsp)!=1:
                    nerr+=manageLog(fout, noerror, 'your file name indicates initialization_method to be %s but your file says it has not initialization_method: %s' % (rsp[1]))
            real=rsp[0]
                
            if int(real[1:])!=int(getattr(file,'realization')):
                nerr+=manageLog(fout, noerror, 'your file name indicates realization is: %s but your file says it is: %i' % (sp[3][3:],int(getattr(file,'realization'))))
            ## skip the following if it has no time
            if t is not None:
                # here we check the clim b4 going further checking into date
                clim_att = getattr(t,'climatology',None)
                if clim_att is None:
                    has_clim = False
                else:
                    has_clim = True
                if is_clim!=has_clim:
                    if is_clim:
                        nerr+=manageLog(fout, noerror, 'your file name indicates climatology, but the time axis does not have the climatology attribute')
                    else:
                        nerr+=manageLog(fout, noerror, 'your file name does not indicates climatology (_clim.nc), but the time axis has the climatology attribute')


                if is_clim:
                    tmp = file(t.climatology,slice(0,1))
                    ft0 = tmp[0][0]
                    tmp = file(t.climatology,slice(-1,None))
                    ft1 = tmp[-1][1]
                else:
                    ft0=t[0]
                    ft1=t[-1]
                ft0= cdtime.reltime(ft0,t.units).tocomp(t.getCalendar())
                ft1= cdtime.reltime(ft1,t.units).tocomp(t.getCalendar())

                dates = sp[5+spoffset].split("-")

                # now determines the frequency
                units = t.units.split("since")[0].strip()
                interval = float(e['general'].get("approx_interval"))
                interval = genutil.udunits(interval,units).to("s").value


                # determine what the frequency drname should be
                if (interval<2500.) :
                    frequency = "subhr"
                elif (interval<15000.):
                    frequency = "3hr"
                elif (interval<30000.):
                    frequency = "6hr"
                elif (interval<100000.):
                    frequency = "day"
                elif (interval<3.E6):
                    frequency = "mon"
                else:
                    frequency  = "yr"
      
                if (interval == 0.):
                    strcpy(tmp,"fx")

                #Now checks the frequecny attribute matches the one we just decided
                if file.frequency[-4:]=='Clim':
                    frequency=frequency+"Clim"
                if file.frequency!=frequency:
                    nerr+=manageLog(fout, noerror, 'your file indicates a frequency of "%s" but the approximate_interval suggests it should be: "%s"' % (file.frequency,frequency))


                try:
                    yr0=int(dates[0][:4])
                except:
                        nerr+=manageLog(fout, noerror, 'could not convert the years section start date iun your file',dates[0][:4])

                frequency = 'yr'
                if interval<29.E6:
                    frequency='mon'
                    if len(dates[0])<6:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years only when the approximate interval says it should have months')
                    try:
                        m0 = int(dates[0][4:6])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the months section start date iun your file',dates[0][4:6])
                else:
                    m0=ft0.month

                if interval < 2E6:
                    frequency='mon'
                    if len(dates[0])<8:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years and months only when the approximate interval says it should have days')
                    try:
                        d0 = int(dates[0][6:8])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the day section start date iun your file',dates[0][6:8])

                else:
                    d0=ft0.day

                if interval < 86000:
                    if len(dates[0])<12:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years, months, days and hours only when the approximate interval says it should have minutes')
                    try:
                        h0 = int(dates[0][8:10])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the hours section start date iun your file',dates[0][8:10])
                    try:
                        mn0 = int(dates[0][10:12])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the miuntes section start date iun your file',dates[0][10:12])
                else:
                    h0= ft0.hour
                    mn0=ft0.minute

                if interval < 3000:
                    if len(dates[0])<14:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years, months, days, hours and minutes only when the approximate interval says it should have seconds')
                    try:
                        s0 = int(dates[0][12:14])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the seconds section start date iun your file',dates[0][12:14])

                else:
                    s0=ft0.second
                t0 = cdtime.comptime(yr0,m0,d0,h0,mn0,s0)
                try:
                    yr1=int(dates[1][:4])
                except:
                    nerr+=manageLog(fout, noerror, 'could not convert the years section end date iun your file',dates[1][:4])

                if interval<29.E6:
                    if len(dates[1])<6:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years only when the approximate interval says it should have months')
                    try:
                        m1 = int(dates[1][4:6])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the months section end date iun your file',dates[1][4:6])

                else:
                    m1=ft1.month

                if interval < 2.E6:
                    if len(dates[1])<8:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years and months only when the approximate interval says it should have days')
                    try:
                        d1 = int(dates[1][6:8])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the days section end date iun your file',dates[1][6:8])
                else:
                    d1=ft1.day

                if interval < 90000:
                    if len(dates[1])<10:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years, months and days only when the approximate interval says it should have hours')
                    try:
                        h1 = int(dates[1][8:10])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the hours section end date iun your file',dates[1][8:10])

                else:
                    h1=ft1.hour
                if interval < 4000:
                    if len(dates[1])<12:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years, months, days and hours only when the approximate interval says it should have minutes')
                    try:
                        mn1 = int(dates[1][10:12])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the minutes section end date iun your file',dates[1][10:12])

                else:
                    mn1=ft1.minute
                if interval < 100:
                    if len(dates[1])<14:
                        nerr+=manageLog(fout, noerror, 'your file name indicates a start time with years, months, days, hours and minutes only when the approximate interval says it should have seconds')
                    try:
                        s1 = int(dates[1][12:14])
                    except:
                        nerr+=manageLog(fout, noerror, 'could not convert the seconds section end date iun your file',dates[1][12:14])

                else:
                    s1=ft1.second
                t1 = cdtime.comptime(yr1,m1,d1,h1,mn1,s1)


                if (ft0.year!=t0.year) or (ft0.month!=t0.month) or (ft0.day!=t0.day):
                    nerr+=manageLog(fout, noerror, 'your file name indicates a start time of %i-%i-%i but the actual value in the file says: %i-%i-%i' % (t0.year,t0.month,t0.day,ft0.year,ft0.month,ft0.day))
                if (ft1.year!=t1.year) or (ft1.month!=t1.month) or (ft1.day!=t1.day):
                    nerr+=manageLog(fout, noerror, 'your file name indicates an end time of %i-%i-%i but the actual value in the file says: %i-%i-%i' % (t1.year,t1.month,t1.day,ft1.year,ft1.month,ft1.day))


        
            
        manageLog(fout,VERBOSE, 'Checking that file contains only 1 variable')
        vars=file.listvariable()
        ## removes dims and other complementary vars
        # First bounds
    ##     manageLog(fout,VERBOSE, vars)
        vars2=file.listvariable()
        vars3=[]
        vars4=[]
        for v in vars2:
    ##         manageLog(fout,VERBOSE, v)
            V=file[v]
            b=getattr(V,'bounds',None)
            if b is not None:
                vars.pop(vars.index(b))
            b=getattr(V,'ancillary_variables',None)
            if b is not None:
                for sp in b.split():
                    if sp.trip() in vars:
                        vars.pop(sp.strip())
            for ax in V.getAxisList():
                b=getattr(ax,'bounds',None)
                if b is not None:
                    if b in vars:
                        vars.pop(vars.index(b))
                    Ab=file[b] # recovers associated bounds with axis
                    f=getattr(Ab,'formula_terms',None)
                    if f is not None:
                        ## figures out the names of formula_terms
                        sp=f.split(':')
    ##                     manageLog(fout,VERBOSE, sp)
                        for t in sp:
    ##                         manageLog(fout,VERBOSE, 't is:',t)
                            t=t.split()[-1]
    ##                         manageLog(fout,VERBOSE, 'Now it is:',t)
                            sp2=f.split(t+':')[1].split()[0]
                            if sp2 in vars:
                                vars3.append(vars.pop(vars.index(sp2)))
                                vars4.append(True)
                f=getattr(ax,'formula_terms',None)
                if f is not None:
                    ## figures out the names of formula_terms
                    sp=f.split(':')
    ##                 manageLog(fout,VERBOSE, sp)
                    for t in sp:
    ##                     manageLog(fout,VERBOSE, 't is:',t)
                        t=t.split()[-1]
    ##                     manageLog(fout,VERBOSE, 'Now it is:',t)
                        sp2=f.split(t+':')[1].split()[0]
                        if sp2 in vars:
                            vars3.append(vars.pop(vars.index(sp2)))
                            vars4.append(False)


            coords=getattr(V,'coordinates',None)
            if coords is not None:
                coords = coords.split()
                for c in coords:
                    if c in vars:
                        vars.pop(vars.index(c))
            if hasattr(V,"grid_mapping"):
                if V.grid_mapping in vars:
                    vars.pop(vars.index(V.grid_mapping))
                manageLog(fout,VERBOSE, "Grid_mapping attribute found, we cannot check these files yet")
                ncheck+=1
                nwarn+=1
                return nwarn,ncheck,nerr


            t = V.getTime()
            if t is not None and hasattr(t,"climatology"):
                c = t.climatology
                if c in vars:
                    vars.pop(vars.index(c))

        if len(vars)!=1:
            nerr+=manageLog(fout, noerror, 'file must contain only 1 variable, you have: '+str(len(vars))+':'+str(vars))

        var=vars[0]
        if major>=2 and dodrs:
            #Now checks for the DRS file structure
            prj_id = file.project_id.strip().replace(" ","_")
            prod = file.product
            inst = file.institute_id
            if inst == 'not specified' :
                inst = "INSTITUTE_ID"
            mod = file.model_id
            exp = file.experiment_id
            #by pass no sht for now...
            noff=0
                
            fq = file.frequency
            realm = file.modeling_realm
            r = str(int(file.realization))
            if hasattr(file,"initialization_method"):
                r+="i"+str(int(file.initialization_method))
            if hasattr(file,"physics_version"):
                r+="p"+str(int(file.physics_version))
            path = os.path.dirname(fnm).split("/")
            nerr += drs_check_pos(1,path,fout,noerror,'r%s' % r, 'realization')
            nerr += drs_check_pos(2,path,fout,noerror,var, 'variable')
            nerr += drs_check_pos(3,path,fout,noerror,realm, 'modeling realm')
            nerr += drs_check_pos(4,path,fout,noerror,fq, 'frequency')
            if exp == file.experiment:
                noff=1
            else:
                nerr += drs_check_pos(5,path,fout,noerror,exp, 'experiment id')
            nerr += drs_check_pos(6-noff,path,fout,noerror,hyphenize(mod), 'model id')
            nerr += drs_check_pos(7-noff,path,fout,noerror,inst, 'insitution id')
            nerr += drs_check_pos(8-noff,path,fout,noerror,prod, 'product')
            nerr += drs_check_pos(9-noff,path,fout,noerror,prj_id, 'project id')
                            
    fb=False
    if variable is not None:
        var=variable
        fb=from_bounds
        manageLog(fout,VERBOSE, 'Ok user asked to check the following variable:',variable,'with from bounds =',fb)
    manageLog(fout,VERBOSE, 'Checking variable name is %s compliant' % file.project_id)
    if not var in IPCC_std_vars:
        if var in Vars.keys():
            nerr+=manageLog(fout, noerror, var+' is not valid, did you mean :'+Vars[var]['out_name']+' ?')
        else:
            nerr+=manageLog(fout, noerror, 'Variable name :'+var+' is not %s compliant' % file.project_id)
    if variable is None:
        manageLog(fout,VERBOSE, 'Checking that variable name in file matches file indications')
        if not var == vf:
            nerr+=manageLog(fout, noerror, 'File indicates it stores variable:'+vf+' but actually '+var+' is stored in it')
    
    V=file[var]

    axes=V.getAxisList()
    hasLat=0
    hasLon=0
    hasTime=0
    hasLevel=0

    ax_dict_name=[]
    for ax in axes:
        manageLog(fout,VERBOSE, 'Checking axis name is valid for:',ax.id,'on var:',V.id)
        if not ax.id in IPCC_std_axes:
            if ax.id in Axes.keys():
                if major <2:
                    ncheck+=1
                    nwarn+=1
                    nerr+=manageLog(fout,cmor.CMOR_WARNING, '\t\t'+ax.id+' is not preferred. We suggest you rename it:'+Axes[ax.id]['out_name'])
                else:
                    manageLog(fout,noerror, '\t\tAxis name '+ax.id+' is not valid. We require you rename it:'+Axes[ax.id]['out_name'])
            elif (fb == False) or (fb == True and V.rank()!=2):
                nerr+=manageLog(fout, noerror, 'axis id: '+ax.id+' is not a valid IPCC name')
        if ax.isLatitude():
            hasLat=1
        if ax.isLongitude():
            hasLon=1
        if ax.isTime():
            hasTime=1
        if ax.isLevel():
            hasLevel=1

    old_ordering=0
    o=V.getOrder(ids=1)
    if old_ordering:
        manageLog(fout,VERBOSE, 'Checking dimensions order')
        if hasTime:
            manageLog(fout,VERBOSE, '\tChecking time position')
            if o[0]=='t':
                o=o[1:]
            else:
                nerr+=manageLog(fout, noerror, 'time must be first dimension your ordering is:'+o)

        manageLog(fout,VERBOSE, '\tChecking none tzxy dims position')
        sp=o.split('(')
        if len(sp)>1:
            if o[0]!='(':
                nerr+=manageLog(fout, noerror, 'none zyx dimensions must come right after time dimension, you have:'+o)
            o=o.split(')')[-1]
        manageLog(fout,VERBOSE, '\tChecking level position')
        if hasLevel:
            if o[0]=='z':
                o=o[1:]
            else:
                nerr+=manageLog(fout, noerror, 'level must be ordered after time your order is:'+o)
        manageLog(fout,VERBOSE, '\tChecking latitude position')
        if hasLat:
            if o[0]=='y':
                o=o[1:]
            else:
                nerr+=manageLog(fout, noerror, 'latitude must be ordered after time and level your order is:'+o)
        manageLog(fout,VERBOSE, '\tChecking longitude position')
        if hasLon:
            if o[0]=='x':
                o=o[1:]
            else:
                nerr+=manageLog(fout, noerror, 'longitude must be ordered after time, level and latitude your order is:'+o)
        
    g=None
    if hasLat and hasLon:
        manageLog(fout,VERBOSE, 'Checking grid')
        g=V.getGrid()
        if not isinstance(g,cdms2.grid.AbstractRectGrid):
            nerr+=manageLog(fout, noerror, 'lat/lon variable ('+var+') must have Rectilinear grids')

    axes_nmes=Vars[var].get('dimensions',None)

    if axes_nmes is not None:
        manageLog(fout,VERBOSE, 'Checking rest of things on axes')
        axes_nmes=axes_nmes[::-1]
        axes_nmes_for_ordering=Vars[var]['dimensions'][::-1]
        coord=getattr(V,'coordinates',None)
        for nm in axes_nmes:
            req_Att=['standard_name','units']
            anm = nm
            if nm in [ 'zlevel',]+e['general'].get('generic_levels','').split():
                gnm=nm
                manageLog(fout,VERBOSE, '\tChecking special case %s, i.e' % (nm),'')
                tmpax=V.getLevel()
                print>>fout,  tmpax.id,tmpax.standard_name
                for x in Axes.keys():
                    tmp=Axes[x].get('standard_name',None)
                    if tmp is not None: tmp=tmp.strip()
                    if tmp is not None and tmp==tmpax.standard_name:
                        nm=x
                        Nm=Axes[x]['out_name']
                        anm=x
                        req_Att.append('formula')
                        req_Att.append('formula_terms')
                        if getattr(tmpax,"formula","")==Axes[x]['formula']:
                            break
                axes_nmes_for_ordering[axes_nmes_for_ordering.index(gnm)]=nm
            elif not nm in V.getAxisIds():
                try:
                    Nm=Axes[nm]['out_name']
                except:
                    nerr+=manageLog(fout, noerror, 'with axis: '+nm+' not found for variable: '+var, noerror)
            else:
                Nm=nm
            if major>1:
                if Axes[anm].get("must_have_bounds","no")=="yes":
                    req_Att.append("bounds")
                if Axes[anm].get("axis",None) is not None:
                    req_Att.append("axis")
            else:
                req_Att.append("bounds")

            if nm == 'time' and is_clim:
                req_Att.pop(req_Att.index("bounds"))
                
            manageLog(fout,VERBOSE, '\tChecking',Nm)
            axindx=V.getAxisIndex(Nm)
            val=Axes[nm].get('value',None)
            if val is not None: #singleton dimension ?
                manageLog(fout,VERBOSE, '\t\tSingleton dimension')
                if val is None:
                    nerr+=manageLog(fout, noerror, 'cannot retrieve needed axis:'+Nm)
                else:
                    val=float(val)
                    if axindx!=-1:
                        nerr+=manageLog(fout, noerror, 'singleton dimension '+Nm+' must be defined via coordinates attributes on variable '+var+' not as an axis')
                    else:
                        manageLog(fout,VERBOSE, '\t\tChecking coordinates attribute exists on '+var)
                        aval=getattr(V,'coordinates',None)
                        if aval is None:
                            nerr+=manageLog(fout, noerror, 'singleton dimension must be defined via coordinates attribute')
                        manageLog(fout,VERBOSE, '\t\tChecking coordinates attribute matches for '+var)
                        if not Nm in aval.split():
                            nerr+=manageLog(fout, noerror, 'coordinates atrtribute on '+var+' should be '+Nm+' it is '+aval)

                        ax=file[Nm]
                        mn,mx=Axes[nm].get('valid_min',None), Axes[nm].get('valid_max',None)
                        manageLog(fout,VERBOSE, '\t\tChecks for value')
                        if ax != val:
                            manageLog(fout,VERBOSE, '\t\t\tNot matching, checking if valid range is defined in table')
                            if mn is None and mx is None:
                                nerr+=manageLog(fout, noerror, 'singleton dimension value for '+Nm+' must be '+str(val))
                            manageLog(fout,VERBOSE, '\t\t\tChecking if value is within range defined in table')
                            if mn is not None:
                                if mx is not None:
                                    if not( float(mn)<ax<float(mx)):
                                        nerr+=manageLog(fout, noerror, 'invalid value for singleton dimension '+Nm+': '+str(ax)+' must be between '+mn+' and '+mx)
                                elif ax<float(mn):
                                    nerr+=manageLog(fout, noerror, 'invalid min for singleton dimension '+Nm+': '+str(ax)+' must be greater than '+mn)
                            elif ax>float(mx):
                                    nerr+=manageLog(fout, noerror, 'invalid max for singleton dimension '+Nm+': '+str(ax)+' must be less than '+mx)
                        manageLog(fout,VERBOSE, '\t\tChecking for bounds information')
                        b=getattr(ax,'bounds',None)
                        bv=Axes[nm].get('bounds_values',None)
                        if bv is not None:
                            manageLog(fout,VERBOSE, '\t\t\tBounds information defined in table, checking vs file')
                            bv=bv.split()
                            bv=float(bv[0]),float(bv[1])
                            if b is not None:
                                manageLog(fout,VERBOSE, '\t\t\tBounds information defined in file, checking if matches')
                                abv=file[b]
                                ncheck+=1
                                if abv[0]!=bv[0] or abv[1]!=bv[1]:
                                    nwarn+=1
                                    manageLog(fout,cmor.CMOR_WARNING, '\t\t\t\tbounds_value for singleton dimension '+Nm+': '+str(abv)+' do not match requested bounds:'+str(bv))
                            else:
                                nerr+=manageLog(fout, noerror, 'singleton dimension: '+Nm+' bounds required')
                        else:
                            ncheck+=1
                            if b is not None:
                                nwarn+=1
                                manageLog(fout,cmor.CMOR_WARNING, '\t\t\t\tSingleton dimension: '+Nm+' bounds should not be included')

                axes_nmes_for_ordering.pop(0)
                continue # singleton dimension checked no need to continue further

            if axindx==-1:
                nerr+=manageLog(fout, noerror, 'Variable '+var+' should have an axis called '+Axes[Nm])
            ax=V.getAxis(axindx)
            manageLog(fout,VERBOSE, '\t\tChecking that dimension order is positioned:',axes_nmes_for_ordering.index(nm)+1,axes_nmes)
            if axindx!=axes_nmes_for_ordering.index(nm):
                nerr+=manageLog(fout, noerror, 'in ordering for dimension '+nm+' position is: '+str(axindx)+' but it should be: '+str(axes_nmes_for_ordering.index(nm)))
            if ('cell_bounds' in Axes[nm].get('ignored',[])) or ('cell_bounds' in Axes[nm].get('forbidden',[])) or ('cell_bounds' in Axes[nm].get('optional',[])):
                req_Att.pop(req_Att.index('bounds'))
            if 'units' in Axes[nm].get('ignored',[]) or 'units' in Axes[nm].get('optional',[]):
                try:
                    req_Att.pop(req_Att.index('units'))
                except:
                    pass
            ## Ok here we're trying to do the region thing, i.e coordinate attribute exist
            docoord=False
            if coord is not None:
                for c in coord.split():
                    nax=file[c]
                    if ax.id in nax.getAxisIds():
                        oldax=ax
                        ax=nax
                        docoord=True
            manageLog(fout,VERBOSE, '\t\tChecking if required attributes are set:','')
            for r in req_Att:
                manageLog(fout,VERBOSE, r,'')
                val=getattr(ax,r,None)
                if val is None:
                    print >>fout
                    nerr+=manageLog(fout, noerror, 'attribute '+r+' is required for axis '+ax.id)
                if r!='units':
                    good_val=Axes[nm].get(r,None)
                    if good_val is not None:
                        if val!=good_val:
                            nerr+=manageLog(fout, noerror, 'axis attribute '+r+' should be: '+str(good_val)+' but is:'+str(val))
                if r=='formula_terms':
                    print 'Formula:',Axes[anm]['formula'],val
            print >>fout
            if not 'units' in Axes[nm].get('ignored',[]):
                if not 'units' in Axes[nm].get('optional',[]) or ('units' in Axes[nm].get('optional',[]) and hasattr(ax,'units')):
                    if not ax.isTime():
                        manageLog(fout,VERBOSE, '\t\tChecking units',ax.units)
                        if major<2:
                            u1=genutil.udunits(1,ax.units)
                            try:
                                u2=u1.to(Axes[nm]['units'])
                                if u2.value!=1:
                                    nerr+=manageLog(fout, noerror, 'units:'+ax.units+' are not compatible with required:'+Axes[nm]['units'])
                            except:
                                nerr+=manageLog(fout, noerror, 'units:'+ax.units+' are not compatible with required:'+Axes[nm]['units'])
                        else:
                            if ax.units != Axes[nm]['units']:
                                nerr+=manageLog(fout, noerror, 'units: '+ax.units+' are not the required units:'+Axes[nm]['units'])

                    else:
                        manageLog(fout,VERBOSE, '\t\tChecking units',ax.units)
                        if major>1:
                            if ax.units.lower().find("days since")==-1:
                                nerr+=manageLog(fout,noerror,'Time units must be in "days since", you have:',ax.units)
                            bnds = ax.getBounds()
                            if bnds is not None:
                                for i in range(len(ax)):
                                    if ax[i]!=(bnds[i][0]+bnds[i][1])/2.:
                                        nerr+=manageLog(fout,noerror,"Time values are not average of time bounds")
                        try:
                            u=cdtime.reltime(1,ax.units)
                        except:
                            nerr+=manageLog(fout, noerror, 'invalid time units:'+ax.units+', should be in the form: "'+Axes[nm]['units']+'"')
                        try:
                            c=ax.calendar
                        except:
                            c='none'
                        if not c in ["gregorian","standard", "proleptic_gregorian","noleap","365_day","360_day","julian","none","non_standard"]:
                            nerr+=manageLog(fout,noerror,"calendar must be one of [","gregorian","standard", "proleptic_gregorian","noleap","365_day","360_day","julian","none","non_standard ] yours is",c)
                        if c=='365_day':
                            c=cdtime.NoLeapCalendar
                        else:
                            c=ax.getCalendar()
                        manageLog(fout,VERBOSE, '\t\tView First and Last times:\t',ax.asComponentTime(c)[0],'\t',ax.asComponentTime(c)[-1])
                        tmpbnds=ax.getBounds()
                        if tmpbnds is not None:
                            manageLog(fout,VERBOSE, '\t\tView Bounds for first time:\t',cdtime.reltime(tmpbnds[0,0],ax.units).tocomp(c),'\t',cdtime.reltime(tmpbnds[0,1],ax.units).tocomp(c))
                            manageLog(fout,VERBOSE, '\t\tView Bounds for last time:\t',cdtime.reltime(tmpbnds[-1,0],ax.units).tocomp(c),'\t',cdtime.reltime(tmpbnds[-1,1],ax.units).tocomp(c))
                        else:
                            manageLog(fout,VERBOSE,'\t\tNo Bounds for time')
                            

            tp=Axes[nm].get('type','double')
            manageLog(fout,VERBOSE, '\t\tChecking axis is type',tp)

            if tp == 'double' : tp='d'
            elif tp == 'real' : tp='f'
            elif tp == 'character' : tp='c'
            elif tp == 'integer' : tp='l'
            else:
                nerr+=manageLog(fout, noerror, 'encountered unknown type:'+tp)
            if ax.typecode()!=tp:
                nerr+=manageLog(fout, noerror, 'required typecode for '+Nm+' should be '+tp+' not '+ax.typecode())

            if ax.isLongitude():
                manageLog(fout,VERBOSE, '\t\tChecking for axis attribute')
                a=getattr(ax,'axis',None)
                if a is None:
                    nerr+=manageLog(fout, noerror, 'longitude axis must have associated axis attribute')
                if a!='X':
                    nerr+=manageLog(fout, noerror, 'longitude axis must have associated axis attribute set to X not: '+a)
                manageLog(fout,VERBOSE, '\t\tChecking name')
                if not ax.id in ['lon','longitude']:
                    nerr+=manageLog(fout, noerror, 'longitude axis name must be longitude or lon (prefered) not: '+ax.id)
##                 else:
##                     ncheck+=1
##                     if ax.id=='longitude':
##                         nwarn+=1
##                         manageLog(fout,cmor.CMOR_WARNING, '\t\t\tWe recomend longitude axis name to be: "lon"')
                manageLog(fout,VERBOSE, '\t\tChecking that the first point is >= 0')
                if ax[0]<0:
                    nerr+=manageLog(fout, noerror, 'first longitude must be >= 0 degrees_east')
                manageLog(fout,VERBOSE, '\t\tChecking that the longitude are in degrees (not rads)')
                min,max=genutil.minmax(ax[:])
                if 0.<max-min<6.3:
                    nerr+=manageLog(fout, noerror, 'longitude must be stored in degree span is:'+str(max-min)+' looks like rad')
            elif ax.isLatitude():
                manageLog(fout,VERBOSE, '\t\tChecking for axis attribute')
                a=getattr(ax,'axis',None)
                if a is None:
                    nerr+=manageLog(fout, noerror, 'latitude axis must have associated axis attribute')
                if a!='Y':
                    nerr+=manageLog(fout, noerror, 'latitude axis must have associated axis attribute set to Y not: '+a)
                manageLog(fout,VERBOSE, '\t\tChecking name')
                if not ax.id in ['lat','latitude']:
                    nerr+=manageLog(fout, noerror, 'latitude axis name must be latitude or lat (prefered) not: '+ax.id)
##                 else:
##                     ncheck+=1
##                     if ax.id=='latitude':
##                         nwarn+=1
##                         manageLog(fout,cmor.CMOR_WARNING, '\t\t\tWe recomend latitude axis name to be: "lat"')
                manageLog(fout,VERBOSE, '\t\tChecking that the latitude are in degrees (not rads)')
                min,max=genutil.minmax(ax[:])
                if 0.<max-min<3.2:
                    print ax[:]
                    nerr+=manageLog(fout, noerror, 'latitude must be stored in degree span is:'+str(max-min)+' looks like rad')
            elif ax.isTime() and len(ax[:])>1:
                manageLog(fout,VERBOSE, '\t\tChecking for axis attribute')
                a=getattr(ax,'axis',None)
                if a is None:
                    nerr+=manageLog(fout, noerror, 'time axis must have associated axis attribute')
                if a!='T':
                    nerr+=manageLog(fout, noerror, 'time axis must have associated axis attribute set to T not: '+a)
                manageLog(fout,VERBOSE, '\t\tView calendar attribute: ','')
                c=getattr(ax,'calendar',None)
                if c is None:
                    print >>fout
                    nerr+=manageLog(fout, noerror, 'calendar attribute must be defined on time axis')
                else:
                    manageLog(fout,VERBOSE, c)
                    
            elif ax.isLevel():
                manageLog(fout,VERBOSE, '\t\tChecking for axis attribute')
                a=getattr(ax,'axis',None)
                if a is None:
                    nerr+=manageLog(fout, noerror, 'vertical axis must have associated axis attribute')
                if a!='Z':
                    nerr+=manageLog(fout, noerror, 'vertical axis must have associated axis attribute set to Z not: '+a)
                manageLog(fout,VERBOSE, '\t\tChecking that level are stored with first level closest from surface')
                ## ???
                manageLog(fout,VERBOSE, '\t\tChecking positive attribute')
                p=getattr(ax,'positive',None)
                if not nm in [ 'zlevel', 'alevel','olevel']:
                    ncheck+=1
                if p is None:
                    if nm in ['zlevel','alevel','olevel']:
                        nerr+=manageLog(fout, noerror, 'vertical dimensions must have positive attribute')
                    else:
                        nwarn+=1
                        manageLog(fout,cmor.CMOR_WARNING, 'The "positive" attribute should be defined for vertical dimensions.','name was:',nm,gnm)
                        
                elif not p in ['up','down']:
                    nerr+=manageLog(fout, noerror, 'positive attribute on vertical dimension must be be up or down not: '+p)
                if hasattr(ax,'formula_terms'):
                    ncheck+=1
                    if not hasattr(ax,'formula'):
                        manageLog(fout,cmor.CMOR_WARNING, '\t\t\tLevel dimension has no attribute formula!')
                        nwarn+=1
                    ncheck+=1
                    manageLog(fout,VERBOSE, '\t\tChecking that formula terms variables are stored')
                    ft=getattr(ax,'formula_terms',None)
                    ## figures out the names of formula_terms
                    sp=ft.split(':')
    ##                 manageLog(fout,VERBOSE, sp)
                    for t in sp:
    ##                     manageLog(fout,VERBOSE, 't is:',t)
                        t=t.split()[-1]
    ##                     manageLog(fout,VERBOSE, 'Now it is:',t)
                        sp2=ft.split(t+':')[1].split()[0]
                        if not sp2 in file.variables.keys():
                            nerr+=manageLog(fout, noerror, 'formula_terms attribute indicates variable '+sp2+' should be stored in file')

            if not docoord:
                dirc=Axes[nm].get('stored_direction','increasing').lower()
                if dirc == 'increasing':
                    func=numpy.greater
                elif dirc == 'decreasing':
                    func=numpy.less
                else:
                    nerr+=manageLog(fout, noerror, 'unknown value for stored_direction:',dirc)
                manageLog(fout,VERBOSE, '\t\tChecking that axis is stored '+dirc+'ly')
                prev=ax[0]
                for a in ax[1:]:
                    if not func(a,prev):
                        nerr+=manageLog(fout, noerror, 'axis values for '+Nm+' must be stored:'+dirc+'ly')
                    prev=a
                Mn,Mx=genutil.minmax(ax[:])
                mn=Axes[nm].get('valid_min',None)
                if mn is not None:
                    manageLog(fout,VERBOSE, '\t\tChecking valid min',mn)
                    if Mn<float(mn):
                        nerr+=manageLog(fout, noerror, 'axis '+Nm+' invalid min:'+str(Mn)+' cannot be less than:'+mn)
                mx=Axes[nm].get('valid_max',None)
                if mx is not None:
                    manageLog(fout,VERBOSE, '\t\tChecking valid max',mx)
                    if Mx>float(mx):
                        nerr+=manageLog(fout, noerror, 'axis '+Nm+' invalid max:'+str(Mx)+' cannot be greater than:'+mx)
            rq=Axes[nm].get('requested',None)
            if rq is not None:
                manageLog(fout,VERBOSE, '\t\tChecking that requested values are present:',rq)
                tol=float(Axes[nm].get('tol_on_requests',1.e-3))
                for ir in range(len(rq)):
                    r=rq[ir]
                    found=0
                    if docoord:
                        for iv in range(len(ax[:])):
                            v=ax[iv]
                            st=''
                            for vv in v:
                                st+=str(vv)
                            if r.strip().lower()==st.strip().lower():
                                found=1
                                ifound=iv
                                break
                    else:
                        r=float(r)
                        for iv in range(len(ax[:])):
                            v=ax[iv]
                            if abs(v-r)<tol:
                                found=1
                                ifound=iv
                                break
                    if found==0:
                        nerr+=manageLog(fout, noerror, 'on axis '+Nm+' requested value '+str(r)+' not found')
##                     elif ifound!=ir:
##                         nerr+=manageLog(fout, noerror, 'on axis '+Nm+' requested value "'+str(r)+'" present but not in the correct order, it is at position '+str(ifound)+' but should be ordered '+str(ir)+' ( actual order should be: '+str(rq)+' )')

            if 'bounds' in req_Att:
                manageLog(fout,VERBOSE, '\t\tChecking for bounds')
                b=getattr(ax,'bounds',None)
                if b is None:
    ##                 if ax.isLevel():
    ##                     manageLog(fout,cmor.CMOR_WARNING, '\t\t\tLevel dimension has no bounds!')
    ##                     nwarn+=1
    ##                     ncheck+=1
    ##                 if ax.isTime():
    ##                     manageLog(fout,cmor.CMOR_WARNING, '\t\t\tTime dimension has no bounds!')
    ##                     nwarn+=1
    ##                     ncheck+=1
    ##                 else:
                        nerr+=manageLog(fout, noerror, 'dimension:'+Nm+' has no associated bounds')
                else:
                    manageLog(fout,VERBOSE, '\t\tChecking that defined bounds variable is in file')
                    if not b in file.variables.keys():
                        nerr+=manageLog(fout, noerror, 'dimension '+Nm+' associated bounds are defined to be '+b+' but the variable is not present in file')
                    if ax.isTime():
                        interv = Axes[nm].get('interval',None)
                        tinterv = e['general'].get('approx_interval',None)
                        if float(tinterv) == 30. and interv!=0:
                            manageLog(fout,VERBOSE, '\t\tChecking that bounds are at beg and end of month')
                            bnds=ax.getBounds()
                            for ib in range(len(ax[:])):
                                b=bnds[ib]
                                c=ax.calendar
                                if c=='365_day':
                                    c=cdtime.NoLeapCalendar
                                else:
                                    c=ax.getCalendar()

##                                 print 'Used calendar:',c
                                beg=cdtime.reltime(b[0],ax.units).tocomp(c)
                                end=cdtime.reltime(b[1],ax.units).tocomp(c)
                                mid=cdtime.reltime(ax[ib],ax.units).tocomp(c)
                                if beg.month!=mid.month:
                                    nerr+=manageLog(fout, noerror, 'at time value: '+str(mid)+' (index: '+str(ib)+' ) bounds do not start in the same month than time value: '+str(beg))
                                if beg.cmp(cdtime.comptime(beg.year,beg.month))!=0:
                                    nerr+=manageLog(fout, noerror, 'at time value: '+str(mid)+' (index: '+str(ib)+' ) bounds do not start at begining of the month: '+str(beg))
                                if end.month!=mid.add(1,cdtime.Month).month:
                                    nerr+=manageLog(fout, noerror, 'at time value: '+str(mid)+' (index: '+str(ib)+' ) bounds do not end in the following month than time value: '+str(end))
                                if end.cmp(beg.add(1,cdtime.Month))!=0:
                                    nerr+=manageLog(fout, noerror, 'at time value: '+str(mid)+' (index: '+str(ib)+' ) bounds do not end at end of the month: '+str(end))

    manageLog(fout,VERBOSE, 'Checking variable:',var)
    req_var_Att = ['standard_name','units']
    opt_var_Att=['original_name','history','long_name','comment','coordinates','cell_methods','original_units',"cell_measures"]
    for etmp in etables:
        ev = etmp['variable']
        for v in ev.keys():
            if v == var:
                for p in ev[v].get("optional",[]):
                    if p in req_var_Att:
                        req_var_Att.pop(req_var_Att.index(p))
    if major >1 and variable is None:
        req_var_Att.append("associated_files")
        req_var_Att.append("associated_files")
        req_var_Att.append("long_name")
        opt_var_Att.append("missing_value")
        opt_var_Att.append("_FillValue")
    if major>1:
        for att in ['units','standard_name']:
            if ev[var].get(att,None) is None:
                req_var_Att.pop(req_var_Att.index(att))
        for att in ['cell_measures','cell_methods','flag_values']:
            if ev[var].get(att,None) is not None:
                req_var_Att.append(att)
        
        
    for o in Vars[var].get('optional',[]):
        if not o in opt_var_Att:
            opt_var_Att.append(o)
    manageLog(fout,VERBOSE, '\tChecking if required attributes are set:','')
    for r in req_var_Att:
        manageLog(fout,VERBOSE, r,'')
        val = getattr(V,r,None)
        if val is None:
            nerr+=manageLog(fout, noerror, 'Attribute '+r+' is required but not set for var '+var)
        elif r!='units' and r.strip().lower()!='standard_name':
            good_val=Vars[var].get(r,None)
            if good_val is not None:
                if val!=good_val:
                    nerr+=manageLog(fout, noerror, 'variable attribute '+r+' should be: '+str(good_val)+' but is:'+str(val))
            elif r=='associated_files':
                if val.find("baseURL:") == -1:
                    nerr+=manageLog(fout,noerror,"associated_files attributes must contain a baseURL")
                elif val.find(e['general']['baseURL'].replace("http","http:")) == -1:
                    nerr+=manageLog(fout,noerror,"wrong baseURL, should be: '%s', you have: " % e['general']['baseURL'].replace("http","http:"),val)
                if Vars[var].get("cell_measures",None) is not None:
                    sp=Vars[var].get("cell_measures").split()
                    for i in range(len(sp)/2):
                        if val.find(sp[i+1]+"_")==-1:
                            nerr+=manageLog(fout,noerror,"associated files should point to file containing:",sp[i+1],'you have:',val)
                            
    print >>fout
    tp=Vars[V.id].get('type','real')
    manageLog(fout,VERBOSE, '\tChecking Variable typecode is',tp)
    if tp == 'double' : tp='d'
    elif tp == 'real' : tp='f'
    elif tp == 'character' : tp='c'
    elif tp == 'integer' : tp='l'
    else:
        nerr+=manageLog(fout, noerror, 'encountered unknown type:'+tp)
    if V.typecode()!=tp:
        nerr+=manageLog(fout, noerror, 'variable typecode must be '+tp+', it is:'+V.typecode())

    if 'units' in req_var_Att:
        manageLog(fout,VERBOSE, '\tChecking defined units')
        U1=unidata.udunits(1,V.units)
        try:
            U2=U1.to(Vars[var]['units'])
            if U2.value!=1:
                nerr+=manageLog(fout, noerror, 'variable units:'+V.units+' do not match IPCC units:'+Vars[var]['units']+'\n'+\
                      '1 '+V.units+' is actually :'+str(U2))
        except:
            nerr+=manageLog(fout, noerror, 'variable units:'+V.units+' do not match IPCC units:'+Vars[var]['units'])
    if 'standard_name' in Vars[var].keys():
        manageLog(fout,VERBOSE, '\tChecking standard name (case independent)')
        if getattr(V,'standard_name','').lower().strip()!=Vars[var]['standard_name']:
            nerr+=manageLog(fout, noerror, 'standard_name for '+var+' should be:'+Vars[var]['standard_name'])

##     if hasattr(V,'_FillValue'):
##         manageLog(fout,VERBOSE, '\tChecking for _FillValue')
##         tp=Vars[V.id].get('type','real')
##         if tp == 'double' :
##             tpv=numpy.array([float(e['general'].get('double_missing_value',1.e20)),],'d')
##         elif tp == 'real' :
##             tpv=numpy.array([float(e['general'].get('missing_value',1.e20)),],'f')
##         elif tp == 'integer' :
##             tpv=numpy.array([int(e['general'].get('integer_missing_value',-192837)),],'i')
##         else:
##             nerr+=manageLog(fout, noerror, 'encountered unknown type:'+tp)
##         print V.id,'type is:',tp,V._FillValue,tpv
##         if getattr(V,'_FillValue')!=tpv:
##             nerr+=manageLog(fout, noerror, '_FillValue must be '+str(tpv)+' (in the typecode of variable)')

    manageLog(fout,VERBOSE, '\tChecking for warnings')
    if hasattr(V,'_FillValue'):
        if not hasattr(V,'missing_value'):
            if major<2:
                ncheck+=1        
                manageLog(fout,cmor.CMOR_WARNING, '\t\tYou defined _FillValue. We recommend you also define missing_value')
                nwarn+=1
            else:
                manageLog(fout,cmor.CMOR_WARNING, '\t\tYou defined _FillValue, You must also define missing_value')
        elif V.missing_value!=V._FillValue:
            nerr+=manageLog(fout, noerror, 'missing_value and _FillValue attributes are different')
    if hasattr(V,'missing_value'):
        if not hasattr(V,'_FillValue'):
            if major<2:
                ncheck+=1        
                manageLog(fout,cmor.CMOR_WARNING, '\t\tYou defined missing_value. We recommend you also define _FillValue')
                nwarn+=1
            else:
                manageLog(fout,cmor.CMOR_WARNING, '\t\tYou defined missing_value, You must also define _FillValue')
        elif V.missing_value!=V._FillValue:
            nerr+=manageLog(fout, noerror, 'missing_value and _FillValue attributes are different')
        elif V.missing_value!=1.e20:
            nerr+=manageLog(fout,noerror,'missing_value and _FillValue must be set to 1.e20f')
        
    if variable is None:
        cm=Vars[var].get('cell_methods',[])
        if cm!=[]:
            manageLog(fout,VERBOSE, '\tChecking for cell_methods: ','')
            if not hasattr(V,'cell_methods'):
                print >> fout
                nerr+=manageLog(fout, noerror, 'Variable '+var+' should have "cell_methods" attribute')
            else:
                 print >> fout,V.cell_methods
               
            cmv=getattr(V,'cell_methods','')
            sp=cm.split('(')
            cm=sp[0]
            for s in sp[1:]:
                cm+=s.split(')')[-1]
            sp=cmv.split('(')
            cmv=sp[0]
            for s in sp[1:]:
                cmv+=s.split(')')[-1]
            sp=cm.split(':')
            dic={}
            kw=sp[0].strip()
            for s in sp[1:]:
                dic[kw]=s.split()[0].strip()
                kw=s.split()[-1].strip()
            sp=cmv.split(':')
            dicv={}
            kw=sp[0].strip()
            for s in sp[1:]:
                dicv[kw]=s.split()[0].strip()
                kw=s.split()[-1].strip()
            for kw in dic.keys():
                if not kw in dicv.keys():
                    nerr+=manageLog(fout, noerror, 'cell_methods must include '+kw+' defined (to: '+dic[kw]+' )')
                elif not dicv[kw]==dic[kw]:
                    nerr+=manageLog(fout, noerror, 'cell_method: '+kw+' definition does not match table, it is :'+dicv[kw]+' but should be: '+dic[kw])
        for att in opt_var_Att:
            ncheck+=1
            if not hasattr(V,att):
                nwarn+=1
                manageLog(fout,cmor.CMOR_WARNING, '\t\tWhen appropriate, it is often helpful to define the variable attribute: ',att)
        for att in V.attributes.keys():
            ncheck+=1
            if not att in req_var_Att and not att in opt_var_Att:
                nwarn+=1
                manageLog(fout,cmor.CMOR_WARNING, '\t\tYou have attribute: %s, which is neither required nor optional ' % (att))

        manageLog(fout,VERBOSE, '\n**********************************************************************************************\n')
        manageLog(fout,VERBOSE, 'Done with main variable moving on to sub-variables')
        for iv in range(len(vars3)):
            manageLog(fout,VERBOSE, '\n----------------------------------------------------------------------------------------------\n')
            v=vars3[iv]
            fb=vars4[iv]
            manageLog(fout,VERBOSE, 'Now Checking sub-variable:',v)
            manageLog(fout,VERBOSE, '\n----------------------------------------------------------------------------------------------\n')
            nwarn_add,ncheck_add,nerr_add=checkCMOR(fout,file,table,variable=v,from_bounds=fb,noerror=noerror)
            nwarn+=nwarn_add
            ncheck+=ncheck_add
            nerr+=nerr
        manageLog(fout,cmor.CMOR_WARNING, '%d warnings issued out of %d checked for (%5.2f%%)' % (nwarn,ncheck,float(nwarn)/ncheck*100))
        if nerr!=0:
            print 'Nerr:',nerr
            raise CMORError,'%i CMOR errors were raise, please check output and correct your file !!!!' % (nerr)

        file.close()
    return nwarn,ncheck,nerr
            
if __name__=='__main__':
    import getopt,sys

    var=None

    file='Test/tas_diurnal_A1_pcmdi-01_run01_20300101_20300301_clim.nc'
    table='Test/climatology_test_table_A'
    nargs = len(sys.argv)
    
    grid_table = None
    
    noerror=cmor.CMOR_CRITICAL
    out='screen'
    
    help="""
    Verify that a file is CMOR compliant

    Usage:
    """+sys.argv[0]+""" [--help] [-f /--file=filename] [-t /--table=tablename]
    Where:
      --help/-?/-h this message
      --table/-t : specify table file to use
      --file/-f specify file to check
      --noerror/-e do not allow error on/off (default "on")
      --out/-o screen or file name (file sends to inputfile.out)
      """

    kw=['file=','help','table=','noerror=','out=']
    cmd='f:h?t:e:o:'
    
    opt=getopt.getopt(sys.argv[1:],cmd,kw)

    if opt[1]!=[]:
        file=opt[1][-1]
    for o,p in opt[0]:
        if o in ['--file','-f']:
            file=p
        if o in ['--table','-t']:
            table=p
        if o in ['--variable','-v']:
            var=p
        if o in ['--grid_table','-g']:
            grid_table=p
        if o in ['--help','-h','-?']:
            print help
            sys.exit()
        if o in ['--noerror','-e']:
            if p.lower() in ['off','0']:
                noerror=cmor.VERBOSE
            elif p.lower() in ['on','1']:
                noerror=cmor.CMOR_CRITICAL
        if o in ['--out','-o']:
            out=p

    print 'File is:',file,file.find('*')
    if os.path.isdir(file):
        files=os.popen('ls '+file+'/*.nc').readlines()
    elif file.find('*')>=0:
        files=os.popen('ls '+file).readlines()
    else:
        files=[file]
    print 'Files:',files
    for file in files:
        print 'Dealing with ',file.strip()
        if out.lower()=='screen':
            fout=sys.stdout
        elif out.lower()=='file':
            fout=".".join(file.strip().split('.')[:-1])+'.out'
            fout=open(fout,'w')
        else:
            fout=open(out,'w')
        if noerror==VERBOSE:
            checkCMOR(fout,file.strip(),table,other_tables=[grid_table,],noerror=noerror,variable=var)
        else:
            try:
                checkCMOR(fout,file.strip(),table,other_tables=[grid_table,],noerror=noerror,variable=var)
            except Exception,err:
                print err
           
