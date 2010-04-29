# CDMS HTTP templates
import cdms, cdtime, tempfile
import os.path
import re
import string
import Numeric

cdmsRE = re.compile('<!--CDMS_(\w*)-->')

displayLink = '<A HREF="<!--CDMS_VARPATH-->/_cdmsDisplay" TARGET="descripFrame"><!--CDMS_VARNAME--></A><BR>\n'
overviewLink = '<A HREF="<!--CDMS_DSETPATH-->/_cdmsOverview" TARGET="_top"><!--CDMS_DSETNAME--></A><BR>\n'
rowTemplate = '<TR><TD NOWRAP><!--CDMS_ATTRIBUTE_NAME-->:</TD><TD><!--CDMS_ATTRIBUTE_VALUE--></TD></TR>\n'
fileRowTemplate = '<tr><td><pre><a href="<!--CDMS_FILEPATH-->"><!--CDMS_FILENAME--></a></pre></td>\n<td><pre><!--CDMS_START_TIME--></pre></td>\n<td><pre><!--CDMS_END_TIME--></pre></td>\n<td><pre><!--CDMS_START_LEVEL--></pre></td>\n<td><pre><!--CDMS_END_LEVEL--></pre></td></tr>'
filelistLink = """
<TABLE WIDTH="50%" BORDER=0 CELLSPACING=2 CELLPADDING=3 COLS=3 ROWS=1 BGCOLOR="#ffffff"><TR>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsFilelist" TARGET="descripFrame"><B>Download</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsPlot.gif" TARGET="descripFrame"><B>View</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsAnimation.gif" TARGET="descripFrame"><B>Time Animation</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsDJF.gif" TARGET="descripFrame"><B>DJF Average</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsMAM.gif" TARGET="descripFrame"><B>MAM Average</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsJJA.gif" TARGET="descripFrame"><B>JJA Average</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsSON.gif" TARGET="descripFrame"><B>SON Average</B></A></TD>
<TD BGCOLOR="#99ccff"><A HREF="<!--CDMS_PATH-->/_cdmsSEASONS.gif" TARGET="descripFrame"><B>Seasonal Averages</B></A></TD>
</TR></TABLE>"""


def gifiate (canvas):
    """Put the canvas out as a gif."""
    gif_file = tempfile.mktemp() # Generate temporary file to store GIF image
    gif_name = canvas.gif(gif_file) # Generate temporary GIF image
    f=open(gif_name,'rb')               # Open temporary GIF image
    s=f.read()                   # Read temporary GIF image
    f.close()                    # Close GIF image
    os.remove(gif_name)                 # Remove temporary GIF image,
    return s                        # Return GIF image string


def varFiles(var):
    npart = 0
    ndim = 0
    for (axis,start,length,true_length) in var.domain:
        if hasattr(axis,'partition'):
            npart = npart+1
            if npart==1:
                part1 = axis
                npart1 = ndim
            elif npart==2:
                part2 = axis
                npart2 = ndim
            else:
                raise TooManyPartitions, variable.id
        ndim = ndim+1

    pathlist = []
    timeInterval = levelInterval = None
    # No partitioning
    if npart==0:
        matchnames = [var.id,None,None,None,None]
        template = var.getTemplate()
        filename = cdms.variable.getPathFromTemplate(template,matchnames)
        pathlist.append((filename, matchnames[:], timeInterval, levelInterval))

    elif npart==1:
        (axis,startelem,length,true_length) = var.domain[npart1]
        template = var.getTemplate()
        partition = axis.partition
        for interval in partition:
            if axis.isTime():
                time0 = axis[interval[0]]
                time1 = axis[interval[1]-1]
                isabs = (string.find(axis.units," as ")!=-1)
                if isabs:
                    start = cdtime.abstime(time0,axis.units)
                    end = cdtime.abstime(time1,axis.units)
                else:
                    cal = axis.getCalendar()
                    start = cdtime.reltime(time0,axis.units).tocomp(cal)
                    end = cdtime.reltime(time1,axis.units).tocomp(cal)
                matchnames = [var.id,start,end,None,None]
                timeInterval = interval
            else:
                start = int(axis[interval[0]])
                end = int(axis[interval[1]-1])
                matchnames = [var.id,None,None,start,end]
                levelInterval = interval

            filename = cdms.variable.getPathFromTemplate(template,matchnames)
            pathlist.append((filename, matchnames[:], timeInterval, levelInterval))

    elif npart==2:
        axis1 = var.domain[npart1][0]
        axis2 = var.domain[npart2][0]
        partition1 = axis1.partition
        partition2 = axis2.partition
        template = var.getTemplate()
        for interval1 in partition1:
            if axis1.isTime():
                isabs = (string.find(axis1.units," as ")!=-1)
                time0 = axis1[interval1[0]]
                time1 = axis1[interval1[1]-1]
                if isabs:
                    start = cdtime.abstime(time0,axis1.units)
                    end = cdtime.abstime(time1,axis1.units)
                else:
                    cal = axis1.getCalendar()
                    start = cdtime.reltime(time0,axis1.units).tocomp(cal)
                    end = cdtime.reltime(time1,axis1.units).tocomp(cal)
                matchnames = [var.id,start,end,None,None]
                timeInterval = interval1
            else:
                start = int(axis1[interval1[0]])
                end = int(axis1[interval1[1]-1])
                matchnames = [var.id,None,None,start,end]
                levelInterval = interval1

            for interval2 in partition2:
                if axis2.isTime():
                    isabs = (string.find(axis2.units," as ")!=-1)
                    time0 = axis2[interval2[0]]
                    time1 = axis2[interval2[1]-1]
                    if isabs:
                        start = cdtime.abstime(time0,axis2.units)
                        end = cdtime.abstime(time1,axis2.units)
                    else:
                        cal = axis2.getCalendar()
                        start = cdtime.reltime(time0,axis2.units).tocomp(cal)
                        end = cdtime.reltime(time1,axis2.units).tocomp(cal)
                    matchnames[1] = start
                    matchnames[2] = end
                    timeInterval = interval2
                else:
                    start = int(axis2[interval2[0]])
                    end = int(axis2[interval2[1]-1])
                    matchnames[3] = start
                    matchnames[4] = end
                    levelInterval = interval2

                filename = cdms.variable.getPathFromTemplate(template,matchnames)
                pathlist.append((filename, matchnames[:], timeInterval, levelInterval))

    return pathlist

class FSDatabase:

    def __init__(self, db, path, id):
        self.db = db
        self.path = path
        self.id = id
        result = db.searchFilter(tag="dataset", scope=cdms.Onelevel)
        dsetnames = []
        for entry in result:
            dsetnames.append(entry.attributes['id'][0])
        dsetnames.sort()
        self.dsetnames = dsetnames
        self._attname = 'Error!'        # Temporary for sub function
        self._dsetname = 'Error!'       # Ditto
        
    def display(self):

        global replaceDBDisplay
        def replaceDBDisplay(mo, self=self):
            pattern = mo.group(1)
            if pattern=='ATTRIBUTE_NAME':
                return self._attname
            elif pattern=='ATTRIBUTE_VALUE':
                return `self.db.attributes[self._attname]`
            elif pattern=='TITLE':
                return self.id
            elif pattern=='PATH':
                return self.path
            elif pattern=='FILE_LIST_LINK':
                return ''
            elif pattern=='ATTRIBUTES':
                rows = ''
                attnames = self.db.attributes.keys()
                attnames.sort()
                for self._attname in attnames:
                    if self._attname not in ['modifiersname']:
                        rows = rows + cdmsRE.sub(replaceDBDisplay,rowTemplate)
                return rows

        result = cdmsRE.sub(replaceDBDisplay, displayTemplate)
        return result

    def overview(self):

        def replace(mo, self=self):
            pattern = mo.group(1)
            if pattern=='PATH':
                return self.path
            else:
                return mo.group(0)

        result = cdmsRE.sub(replace, overviewTemplate)
        return result

    def index(self):
        
        global replaceDBIndex
        def replaceDBIndex(mo, self=self):
            pattern = mo.group(1)
            if pattern=='DSETPATH':
                return os.path.join(self.path,self._dsetname)
            elif pattern=='DSETNAME':
                return self._dsetname
            elif pattern=='PATH':
                return self.path
            elif pattern=='PARENT_PATH':
                return '/index.html'
            elif pattern=='TAG':
                return 'database'
            elif pattern=='INDEX':
                links = ''
                for self._dsetname in self.dsetnames:
                    links = links + cdmsRE.sub(replaceDBIndex, overviewLink)
                return links

        result = cdmsRE.sub(replaceDBIndex, indexTemplate)
        return result

class FSDataset:

    def __init__(self, dset, path):
        self.dset = dset
        self.path = path
        self.varnames = dset.variables.keys()
        self.varnames.sort()
        self._varname = 'Error!'        # Temporary for sub
        self._attname = 'Error!'        # Ditto

    def display(self):

        global replaceDsetDisplay
        def replaceDsetDisplay(mo, self=self):
            pattern = mo.group(1)
            if pattern=='ATTRIBUTE_NAME':
                return self._attname
            elif pattern=='ATTRIBUTE_VALUE':
                return `self.dset.attributes[self._attname]`
            elif pattern=='TITLE':
                return self.dset.id
            elif pattern=='PATH':
                return self.path
            elif pattern=='FILE_LIST_LINK':
                return ''
            elif pattern=='ATTRIBUTES':
                rows = ''
                attnames = self.dset.attributes.keys()
                attnames.sort()
                for self._attname in attnames:
                    if self._attname not in ['modifiersname']:
                        rows = rows + cdmsRE.sub(replaceDsetDisplay, rowTemplate)
                return rows
            
        result = cdmsRE.sub(replaceDsetDisplay, displayTemplate)
        return result

    def overview(self):

        def replace(mo, self=self):
            pattern = mo.group(1)
            if pattern=='PATH':
                return self.path
            else:
                return mo.group(0)

        result = cdmsRE.sub(replace, overviewTemplate)

        return result

    def index(self):

        global replaceDsetIndex
        def replaceDsetIndex(mo, self=self):
            pattern = mo.group(1)
            if pattern=='VARPATH':
                return os.path.join(self.path,self._varname)
            elif pattern=='VARNAME':
                return self._varname
            elif pattern=='PATH':
                return self.path
            elif pattern=='PARENT_PATH':
                ppath, tail = os.path.split(self.path)
                return ppath+'/_cdmsOverview'
            elif pattern=='TAG':
                return 'dataset'
            elif pattern=='INDEX':
                varnames = self.dset.variables.keys()
                varnames.sort()
                links = ''
                for self._varname in varnames:
                    links = links + cdmsRE.sub(replaceDsetIndex, displayLink)
                return links

        result = cdmsRE.sub(replaceDsetIndex, indexTemplate)
        return result

class FSVariable:

    def __init__(self, var, path):
        self.var = var
        self.path = path
        self._attname = 'Error!'        # Temporary for sub
        self._fullpath = 'Error!'       # Temporary for sub
        self._filename = 'Error!'       # Temporary for sub
        self._matches = 'Error!'        # Temporary for sub

    def display(self):

        global replaceVarDisplay
        def replaceVarDisplay(mo, self=self):
            pattern = mo.group(1)
            if pattern=='ATTRIBUTE_NAME':
                return self._attname
            elif pattern=='ATTRIBUTE_VALUE':
                return `self.var.attributes[self._attname]`
            elif pattern=='TITLE':
                return self.var.id
            elif pattern=='PATH':
                return self.path
            elif pattern=='FILE_LIST_LINK':
                return cdmsRE.sub(replaceVarDisplay, filelistLink)
            elif pattern=='ATTRIBUTES':
                rows = ''
                self.var.attributes['domain'] = map(lambda x: x[0].id, self.var.domain)
                self.var.attributes['shape'] = self.var.shape
                attnames = self.var.attributes.keys()
                attnames.sort()
                for self._attname in attnames:
                    if self._attname not in ['modifiersname']:
                        rows = rows + cdmsRE.sub(replaceVarDisplay, rowTemplate)
                return rows

        result = cdmsRE.sub(replaceVarDisplay, displayTemplate)
        return result


    def plot(self):
        """Plot self using first time and level only."""
        import vcs, cdms, VCSRegion
        canvas=vcs.init()                   # Construct VCS object to generate image
        cdms.setAutoReshapeMode('off')
        a, keyargs  = VCSRegion.getRegion(self.var, time=None, level=None, other=None)
        apply(canvas.plot, (a, 'default', 'isofill'), keyargs) # Generate VCS isofill image
        return gifiate(canvas)                        # Return GIF image string


    def plot_one_average (self, averager, template='default', graphics_mode='isofill'):
        import vcs, cdms
        canvas=vcs.init()                   # Construct VCS object to generate image
        cdms.setAutoReshapeMode('off')
        a, keyargs = averager(self.var)
        apply(canvas.plot, (a, template, graphics_mode), keyargs) # Generate VCS isofill image
        return gifiate(canvas)

    def plot_four_views (self, f1, f2, f3, f4, graphics_mode='isofill'):
        import vcs, cdms
        canvas=vcs.init()                   # Construct VCS object to generate image
        cdms.setAutoReshapeMode('off')
        
        for template, f in (('UL1of4',f1), ('UR2of4',f2), ('LL3of4', f3), ('LR4of4', f4)):
            a, keyargs = f(self.var)
            keyargs['long_name'] = 'Seasonal Averages'
            apply(canvas.plot, (a, template, graphics_mode), keyargs) # Generate VCS isofill image
        return gifiate(canvas)

    def animation(self):

        import vcs,cdms,tempfile,os

        canvas=vcs.init()                  # Construct VCS object to generate image

        # Get subset of data
        cdms.setAutoReshapeMode('on')
        if (len(self.var.shape) == 4):
           a = self.var[0:12,0]
        elif (len(self.var.shape) == 3):
           a = self.var[0:12]
        else:
           a = None
        cdms.setAutoReshapeMode('off')
        
        # Create image in background
        if (a != None):
           gif_file = tempfile.mktemp() # Generate temporary file to store GIF image
           for i in range(a.shape[0]):
              canvas.plot(a[i],'default','isofill',variable=self.var,bg=1) # Generate VCS isofill image
              gif_name = canvas.gif(gif_file)                               # Generate temporary GIF image
              canvas.clear()                # Clear the VCS Canvas for next image

           f=open(gif_name,'rb')               # Open temporary GIF image
           s=f.read()                   # Read temporary GIF image
           f.close()                    # Close GIF image
           os.remove(gif_name)          # Remove temporary GIF image,
        else:
           s=None

        return s                        # Return GIF image string

    def filelist(self):

        global replaceVarFilelist
        def replaceVarFilelist(mo, self=self):
            pattern = mo.group(1)
            if pattern=='PATH':
                return self.path
            elif pattern=='FILEPATH':
                return self._fullpath
            elif pattern=='FILENAME':
                head, tail = os.path.split(self._filename)
                return tail
            elif pattern=='START_TIME':
                st = self._matches[1]
                if st is None:
                    return '-'
                else:
                    return '%d-%d-%d %d'%(st.year,st.month,st.day,st.hour) # Component time
            elif pattern=='END_TIME':
                et = self._matches[2]
                if et is None:
                    return '-'
                else:
                    return '%d-%d-%d %d'%(et.year,et.month,et.day,et.hour) # Component time
            elif pattern=='START_LEVEL':
                startLevel = self._matches[3]
                if startLevel is None:
                    return '-'
                else:
                    return `startLevel`
            elif pattern=='END_LEVEL':
                endLevel = self._matches[4]
                if endLevel is None:
                    return '-'
                else:
                    return `endLevel`
            elif pattern=='FILELIST':
                pathlist = varFiles(self.var)
                dset = self.var.parent
                db = dset.parent
                dburl = db.url
                if type(dburl) is type([]):
                    dburl = dburl[0]
                rows = ''
                for self._filename, self._matches, timeInt, levInt in pathlist:
                    if os.path.isabs(dset.directory):
                        self._fullpath = 'file:'+os.path.join(dset.directory, self._filename)
                    else:
                        self._fullpath = os.path.join(dburl, dset.datapath, self._filename)
                    rows = rows+cdmsRE.sub(replaceVarFilelist, fileRowTemplate)
                return rows

        result = cdmsRE.sub(replaceVarFilelist, filelistTemplate)
        return result

overviewTemplate = """<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Transitional//EN""http://www.w3.org/TR/REC-html40/loose.dtd>
<!--NewPage-->
<HTML>
<HEAD>
<TITLE>
Climate Data Management System
</TITLE>
</HEAD>
<FRAMESET cols="20%,80%">
<FRAME src="<!--CDMS_PATH-->/_cdmsIndex" name="varFrame">
<FRAME src="<!--CDMS_PATH-->/_cdmsDisplay" name="descripFrame">
</FRAMESET>
<NOFRAMES>
<H2>
Frame Alert</H2>

<P>
This document is designed to be viewed using the frames feature. If you see this message, you are using a non-frame-capable web client.
<BR>
Link to <A HREF="dsetOverviewSummary.html">Non-frame version.</A></NOFRAMES>
</HTML>
""" # "------------------------------------------------------------------

indexTemplate ="""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Frameset//EN""http://www.w3.org/TR/REC-html40/frameset.dtd">
<!--NewPage-->
<HTML>
<HEAD>
<TITLE>
Index
</TITLE>
</HEAD>
<BODY BGCOLOR="white">
<TABLE WIDTH="100%" BORDER=0 CELLSPACING=0 CELLPADDING=0 COLS=1 ROWS=1 BGCOLOR="#ffffff">
<TR><TD BGCOLOR="#99ccff">
<A HREF="<!--CDMS_PARENT_PATH-->" TARGET="_top"><B>Up</B></A>
</TD></TR>
</TABLE>
<BR>
<A HREF="<!--CDMS_PATH-->/_cdmsDisplay" TARGET="descripFrame"><B><!--CDMS_TAG--> = <!--CDMS_PATH--></B></A>
<BR>

<TABLE BORDER="0" WIDTH="100%">
<TR>
<TD NOWRAP>
<!--CDMS_INDEX-->
<BR>
</TD>
</TR>
</TABLE>

</BODY>
</HTML>
""" # "------------------------------------------------------------------

displayTemplate ="""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.0 Frameset//EN""http://www.w3.org/TR/REC-html40/frameset.dtd">
<!--NewPage-->
<HTML>
<HEAD>
<TITLE>
<!--CDMS_TITLE-->
</TITLE>
<!--CDMS_FILE_LIST_LINK-->
</HEAD>
<BODY BGCOLOR="white">
<H2><!--CDMS_PATH--></H2>
<BR>
<TABLE BORDER="0" CELLSPACING="0" CELLPADDING="10" BGCOLOR="#CCCC99">
<!--CDMS_ATTRIBUTES-->
</TABLE>
</BODY>
</HTML>
""" # "------------------------------------------------------------------

filelistTemplate = """<!doctype html public "-//w3c//dtd html 4.0 transitional//en">
<html>
<head>
   <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
   <meta name="GENERATOR" content="CDMS">
</head>
<body>
<table BORDER=0 CELLSPACING=0 CELLPADDING=10 WIDTH="100%" NOSAVE >
<tr BGCOLOR="#66FFFF" NOSAVE>
<td ALIGN=CENTER COLSPAN="5"><!--CDMS_PATH--></td>
</tr>
<tr BGCOLOR="#66FFFF" NOSAVE>
<td ALIGN=CENTER NOSAVE>File Name</td>
<td ALIGN=CENTER>Start Time (yyyy-mm-dd hh)</td>
<td ALIGN=CENTER>End Time (yyyy-mm-dd hh)</td>
<td ALIGN=CENTER>Start Level</td>
<td>End Level</td>
</tr>
<!--CDMS_FILELIST-->
</table>

</body>
</html>
""" # "------------------------------------------------------------------
