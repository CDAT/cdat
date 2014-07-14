# Adapted for numpy/ma/cdms2 by convertcdms.py
"""
# Template (P) module
"""
###############################################################################
#                                                                             #
# Module:       template (P) module                                           #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command wrapper for VCS's template primary object.     #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
###############################################################################
#                                                                             #
# Import: VCS C extension module and VCS template class objects written in    #
#         Python.							      #
#                                                                             #
###############################################################################
import _vcs_legacy, copy, vcs_legacy, numpy
from Ptext import *
from Pformat import *
from Pxtickmarks import *
from Pytickmarks import *
from Pxlabels import *
from Pylabels import *
from Pboxeslines import *
from Plegend import *
from Pdata import *
from types import *
import inspect

###############################################################################
#                                                                             #
# Function:     setPmember                                                    #
#                                                                             #
# Description of Function:                                                    #
#       Private function to update the VCS canvas plot. If the canvas mode is #
#       set to 0, then this function does nothing.                            #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      setPmember(self,name,value)                                            #
#              where: self is the class (e.g., P)                             #
#                     name is the name of the member that is being changed    #
#                     value is the new value of the member (or attribute)     #
#                                                                             #
###############################################################################
def setPmember(self,member,value):
     _vcs_legacy.setPmember(self, member, value, self.parent.mode)

###############################################################################
#                                                                             #
# Function:     getPmember                                                    #
#                                                                             #
# Description of Function:                                                    #
#       Private function that retrieves the template members from the C       #
#       structure and passes it back to Python.                               #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      return_value =                                                         #
#      getPmember(self,name)                                                  #
#              where: self is the class (e.g., P)                             #
#                     name is the name of the member that is being found      #
#                                                                             #
###############################################################################
def getPmember(self,member):
     return _vcs_legacy.getPmember(self,member)

###############################################################################
#                                                                             #
# Function:     renameP                                                       #
#                                                                             #
# Description of Function:                                                    #
#       Private function that renames the name of an existing template        #
#       graphics method.                                                      #
#                                                                             #
#                                                                             #
# Example of Use:                                                             #
#      renameP(old_name, new_name)                                            #
#              where: old_name is the current name of template graphics method#
#                     new_name is the new name for the template graphics method
#                                                                             #
###############################################################################
def renameP(self, old_name, new_name):
     return _vcs_legacy.renameP(old_name, new_name)

#############################################################################
#                                                                           #
# Template (P) graphics method Class.                                       #
#                                                                           #
#############################################################################
class P(object):
    """
 Class: P                             # Template

 Description of P Class:
    The template primary method (P) determines the location of each picture
    segment, the space to be allocated to it, and related properties relevant
    to its display.

 Other Useful Functions:
               a.show('template')   	# Show predefined templates
               a.show('texttable')   	# Show predefined text table methods
               a.show('textorientation')   # Show predefined text orientation methods
               a.show('line')   	# Show predefined line methods
               a.listelements('template') # Show templates as a Python list
               a.update()               # Updates the VCS Canvas at user's request
               a.mode=1, or 0           # If 1, then automatic update, else if
                                          0, then use the update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs_legacy.init()
    To Create a new instance of boxfill use:
     box=a.createboxfill('new','quick') # Copies content of 'quick' to 'new'
     box=a.createboxfill('new')         # Copies content of 'default' to 'new'

    To Modify an existing boxfill use:
     box=a.getboxfill('AMIP_psl')

    To Create a new instance of template use:
     tpl=a.createtemplate('new','AMIP')  # Copies content of 'AMIP' to 'new'
     tpl=a.createtemplate('new')         # Copies content of 'default' to 'new'

    To Modify an existing template use:
     tpl=a.gettemplate('AMIP')
"""
    ###########################################################################
    #                                                                         #
    # Initialize the template attributes.                                     #
    #                                                                         #
    ###########################################################################
    def __init__(self, parent, Pic_name=None, Pic_name_src='default', createP=0):
        #                                                         #
        ###########################################################
        # Initialize the template class and its members           #
        #                                                         #
        # The getPmember function retrieves the values of the     #
        # template members in the C structure and passes back the #
        # appropriate Python Object.                              #
        ###########################################################
        #                                                         #
        if (createP == 0):
           if (Pic_name == None):
              raise ValueError, 'Must provide a template name.'
           else:
              if len(Pic_name)>16:
                   raise ValueError, 'Template name must be 16 characters maximum in length'
              _vcs_legacy.copyP(Pic_name_src, Pic_name)
              self.__dict__['name'] = Pic_name
              ####################################################
	      # Set the template normalization flag to 1,        #
              # only if the copy templates flag is 0             #
              ####################################################
              if _vcs_legacy._return_normalized_flag( Pic_name_src ) == 1:
                 _vcs_legacy._set_normalized_flag( Pic_name )
        else:
             if Pic_name is not None and len(Pic_name)>16:
                  raise ValueError, 'Template name must be 16 characters maximum in length'
             self.__dict__['name']=Pic_name_src
        self.__dict__['p_name']='P'
        #                                                         #
        ###########################################################
        # Keep track of the template's parent.                    #
        ###########################################################
        #                                                         #
        self.__dict__['parent']=parent
        #################################################
	# The following initializes the template's TEXT #
        #################################################
        self.__dict__['orientation']=_vcs_legacy.getPomember(self)
        self.__dict__['file']=Pt(self, self.parent, 'file')
        self.__dict__['function']=Pt(self, self.parent, 'function')
        self.__dict__['logicalmask']=Pt(self, self.parent, 'logicalmask')
        self.__dict__['transformation']=Pt(self, self.parent, 'transformation')
        self.__dict__['source']=Pt(self, self.parent, 'source')
        self.__dict__['dataname']=Pt(self, self.parent, 'dataname')
        self.__dict__['title']=Pt(self, self.parent, 'title')
        self.__dict__['units']=Pt(self, self.parent, 'units')
        self.__dict__['crdate']=Pt(self, self.parent, 'crdate')
        self.__dict__['crtime']=Pt(self, self.parent, 'crtime')
        self.__dict__['comment1']=Pt(self, self.parent, 'comment1')
        self.__dict__['comment2']=Pt(self, self.parent, 'comment2')
        self.__dict__['comment3']=Pt(self, self.parent, 'comment3')
        self.__dict__['comment4']=Pt(self, self.parent, 'comment4')
        self.__dict__['xname']=Pt(self, self.parent, 'xname')
        self.__dict__['yname']=Pt(self, self.parent, 'yname')
        self.__dict__['zname']=Pt(self, self.parent, 'zname')
        self.__dict__['tname']=Pt(self, self.parent, 'tname')
        self.__dict__['xunits']=Pt(self, self.parent, 'xunits')
        self.__dict__['yunits']=Pt(self, self.parent, 'yunits')
        self.__dict__['zunits']=Pt(self, self.parent, 'zunits')
        self.__dict__['tunits']=Pt(self, self.parent, 'tunits')
        ####################################################
	# The following initializes the template's FORMATS #
        ####################################################
        self.__dict__['xvalue']=Pf(self, self.parent, 'xvalue')
        self.__dict__['yvalue']=Pf(self, self.parent, 'yvalue')
        self.__dict__['zvalue']=Pf(self, self.parent, 'zvalue')
        self.__dict__['tvalue']=Pf(self, self.parent, 'tvalue')
        self.__dict__['mean']=Pf(self, self.parent, 'mean')
        self.__dict__['min']=Pf(self, self.parent, 'min')
        self.__dict__['max']=Pf(self, self.parent, 'max')
        #########################################################
	# The following initializes the template's X-TICK MARKS #
        #########################################################
        self.__dict__['xtic1']=Pxt(self, self.parent, 'xtic1')
        self.__dict__['xtic2']=Pxt(self, self.parent, 'xtic2')
        self.__dict__['xmintic1']=Pxt(self, self.parent, 'xmintic1')
        self.__dict__['xmintic2']=Pxt(self, self.parent, 'xmintic2')
        #########################################################
	# The following initializes the template's Y-TICK MARKS #
        #########################################################
        self.__dict__['ytic1']=Pyt(self, self.parent, 'ytic1')
        self.__dict__['ytic2']=Pyt(self, self.parent, 'ytic2')
        self.__dict__['ymintic1']=Pyt(self, self.parent, 'ymintic1')
        self.__dict__['ymintic2']=Pyt(self, self.parent, 'ymintic2')
        #####################################################
	# The following initializes the template's X-LABELS #
        #####################################################
        self.__dict__['xlabel1']=Pxl(self, self.parent, 'xlabel1')
        self.__dict__['xlabel2']=Pxl(self, self.parent, 'xlabel2')
        #####################################################
	# The following initializes the template's Y-LABELS #
        #####################################################
        self.__dict__['ylabel1']=Pyl(self, self.parent, 'ylabel1')
        self.__dict__['ylabel2']=Pyl(self, self.parent, 'ylabel2')
        ############################################################
	# The following initializes the template's BOXES and LINES #
        ############################################################
        self.__dict__['box1']=Pbl(self, self.parent, 'box1')
        self.__dict__['box2']=Pbl(self, self.parent, 'box2')
        self.__dict__['box3']=Pbl(self, self.parent, 'box3')
        self.__dict__['box4']=Pbl(self, self.parent, 'box4')
        self.__dict__['line1']=Pbl(self, self.parent, 'line1')
        self.__dict__['line2']=Pbl(self, self.parent, 'line2')
        self.__dict__['line3']=Pbl(self, self.parent, 'line3')
        self.__dict__['line4']=Pbl(self, self.parent, 'line4')
        #########################################################
	# The following initializes the template's LEGEND SPACE #
        #########################################################
        self.__dict__['legend']=Pls(self, self.parent, 'legend')
        #######################################################
	# The following initializes the template's DATA SPACE #
        #######################################################
        self.__dict__['data']=Pds(self, self.parent, 'data')
        #######################################################
	# Set the template normalization flag to 1            #
        #######################################################
        _vcs_legacy._set_normalized_flag(self.__dict__['name'])

    def __setattr__(self, name, value):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        if (self.name == 'default'):
           raise ValueError, 'You cannot modify the default template.'
        if (name == 'name'):
           if (type(value) == StringType):
              renameP(self,self.name, value)
              self.__dict__[name]=value
           else:
              raise ValueError, 'The name attribute must be a string.'
        if (name == 'orientation'):
           if (isinstance(value, IntType)):
              if value in [0,1]:
                 _vcs_legacy.setPomember(self,value)
                 self.__dict__[name]=value
              else:
                 raise ValueError, 'The orientation attribute must be an integer (i.e., 0 = landscape, 1 = portrait).'
           else:
              raise ValueError, 'The orientation attribute must be an integer (i.e., 0 = landscape, 1 = portrait).'


    ###########################################################################
    #                                                                         #
    # List out template text members (attributes).                            #
    #                                                                         #
    ###########################################################################
    def list(self, single=None):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'

        if (single == None):
           print "","----------Template (P) member (attribute) listings ----------"
           print 'Canvas Mode =',self.parent.mode
           print "method =", self.p_name
           print "name =", self.name
           print "orientation =", self.orientation
           self.file.list()
           self.function.list()
           self.logicalmask.list()
           self.transformation.list()
           self.source.list()
           self.dataname.list()
           self.title.list()
           self.units.list()
           self.crdate.list()
           self.crtime.list()
           self.comment1.list()
           self.comment2.list()
           self.comment3.list()
           self.comment4.list()
           self.xname.list()
           self.yname.list()
           self.zname.list()
           self.tname.list()
           self.xunits.list()
           self.yunits.list()
           self.zunits.list()
           self.tunits.list()
           self.xvalue.list()
           self.yvalue.list()
           self.zvalue.list()
           self.tvalue.list()
           self.mean.list()
           self.min.list()
           self.max.list()
           self.xtic1.list()
           self.xtic2.list()
           self.xmintic1.list()
           self.xmintic2.list()
           self.ytic1.list()
           self.ytic2.list()
           self.ymintic1.list()
           self.ymintic2.list()
           self.xlabel1.list()
           self.xlabel2.list()
           self.ylabel1.list()
           self.ylabel2.list()
           self.box1.list()
           self.box2.list()
           self.box3.list()
           self.box4.list()
           self.line1.list()
           self.line2.list()
           self.line3.list()
           self.line4.list()
           self.legend.list()
           self.data.list()
        elif ((single == 'text') or (single == 'Pt')):
           self.file.list()
           self.function.list()
           self.logicalmask.list()
           self.transformation.list()
           self.source.list()
           self.dataname.list()
           self.title.list()
           self.units.list()
           self.crdate.list()
           self.crtime.list()
           self.comment1.list()
           self.comment2.list()
           self.comment3.list()
           self.comment4.list()
           self.xname.list()
           self.yname.list()
           self.zname.list()
           self.tname.list()
           self.xunits.list()
           self.yunits.list()
           self.zunits.list()
           self.tunits.list()
        elif ((single == 'format') or (single == 'Pf')):
           self.xvalue.list()
           self.yvalue.list()
           self.zvalue.list()
           self.tvalue.list()
           self.mean.list()
           self.min.list()
           self.max.list()
        elif ((single == 'xtickmarks') or (single == 'Pxt')):
           self.xtic1.list()
           self.xtic2.list()
           self.xmintic1.list()
           self.xmintic2.list()
        elif ((single == 'ytickmarks') or (single == 'Pyt')):
           self.ytic1.list()
           self.ytic2.list()
           self.ymintic1.list()
           self.ymintic2.list()
        elif ((single == 'xlabels') or (single == 'Pxl')):
           self.xlabel1.list()
           self.xlabel2.list()
        elif ((single == 'ylabels') or (single == 'Pyl')):
           self.ylabel1.list()
           self.ylabel2.list()
        elif ((single == 'boxeslines') or (single == 'Pbl')):
           self.box1.list()
           self.box2.list()
           self.box3.list()
           self.box4.list()
           self.line1.list()
           self.line2.list()
           self.line3.list()
           self.line4.list()
        elif ((single == 'legend') or (single == 'Pls')):
           self.legend.list()
        elif ((single == 'data') or (single == 'Pds')):
           self.data.list()
        elif (single == 'file'):
           self.file.list()
        elif (single == 'function'):
           self.function.list()
        elif (single == 'logicalmask'):
           self.logicalmask.list()
        elif (single == 'transformation'):
           self.transformation.list()
        elif (single == 'source'):
           self.source.list()
        elif (single == 'name'):
           self.name.list()
        elif (single == 'title'):
           self.title.list()
        elif (single == 'units'):
           self.units.list()
        elif (single == 'crdate'):
           self.crdate.list()
        elif (single == 'crtime'):
           self.crtime.list()
        elif (single == 'comment1'):
           self.comment1.list()
        elif (single == 'comment2'):
           self.comment2.list()
        elif (single == 'comment3'):
           self.comment3.list()
        elif (single == 'comment4'):
           self.comment4.list()
        elif (single == 'xname'):
           self.xname.list()
        elif (single == 'yname'):
           self.yname.list()
        elif (single == 'zname'):
           self.zname.list()
        elif (single == 'tname'):
           self.tname.list()
        elif (single == 'xunits'):
           self.xunits.list()
        elif (single == 'yunits'):
           self.yunits.list()
        elif (single == 'zunits'):
           self.zunits.list()
        elif (single == 'tunits'):
           self.tunits.list()
        elif (single == 'xvalue'):
           self.xvalue.list()
        elif (single == 'yvalue'):
           self.yvalue.list()
        elif (single == 'zvalue'):
           self.zvalue.list()
        elif (single == 'tvalue'):
           self.tvalue.list()
        elif (single == 'mean'):
           self.mean.list()
        elif (single == 'min'):
           self.min.list()
        elif (single == 'max'):
           self.max.list()
        elif (single == 'xtic1'):
           self.xtic1.list()
        elif (single == 'xtic2'):
           self.xtic2.list()
        elif (single == 'xmintic1'):
           self.xmintic1.list()
        elif (single == 'xmintic2'):
           self.xmintic2.list()
        elif (single == 'ytic1'):
           self.ytic1.list()
        elif (single == 'ytic2'):
           self.ytic2.list()
        elif (single == 'ymintic1'):
           self.ymintic1.list()
        elif (single == 'ymintic2'):
           self.ymintic2.list()
        elif (single == 'xlabel1'):
           self.xlabel1.list()
        elif (single == 'xlabel2'):
           self.xlabel2.list()
        elif (single == 'ylabel1'):
           self.ylabel1.list()
        elif (single == 'ylabel2'):
           self.ylabel2.list()
        elif (single == 'box1'):
           self.box1.list()
        elif (single == 'box2'):
           self.box2.list()
        elif (single == 'box3'):
           self.box3.list()
        elif (single == 'box4'):
           self.box4.list()
        elif (single == 'line1'):
           self.line1.list()
        elif (single == 'line2'):
           self.line2.list()
        elif (single == 'line3'):
           self.line3.list()
        elif (single == 'line4'):
           self.line4.list()
        elif (single == 'legend'):
           self.legend.list()
        elif (single == 'data'):
           self.data.list()

    ###########################################################################
    #                                                                         #
    # Script out template object in VCS to a file.                            #
    #                                                                         #
    ###########################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs_legacy.scriptP

 Description of Function:
       Saves out a template object in VCS or Python script form to a designated
       file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs_legacy.init()
    templt=a.createtemplate('temp')   # create a template object
    templt.script('filename.py')      # Append to a Python file "filename.py"
    templt.script('filename.scr')     # Append to a VCS file "filename.scr"
    templt.script('filename','w')     # Create or overwrite to a Python file "filename.py"
'''
        if (script_filename == None):
          raise ValueError, 'Error - Must provide an output script file name.'

        if (mode == None):
           mode = 'a'
        elif (mode not in ('w', 'a')):
          raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

        # By default, save file in python script mode
        scr_type = script_filename[len(script_filename)-4:len(script_filename)]
        if (scr_type == '.scr'):
           print _vcs_legacy.scriptP(self.name,script_filename,mode)
        else:
           mode = mode + '+'
           py_type = script_filename[len(script_filename)-3:len(script_filename)]
           if (py_type != '.py'):
              script_filename = script_filename + '.py'

           # Write to file
           fp = open(script_filename,mode)
           if (fp.tell() == 0): # Must be a new file, so include below
              fp.write("#####################################\n")
              fp.write("#                                 #\n")
              fp.write("# Import and Initialize VCS     #\n")
              fp.write("#                             #\n")
              fp.write("#############################\n")
              fp.write("import vcs_legacy\n")
              fp.write("v=vcs_legacy.init()\n\n")

           unique_name = '__P__' + self.name
           fp.write("#----------Template (P) member (attribute) listings ----------\n")
           fp.write("p_list=v.listelements('template')\n")
           fp.write("if ('%s' in p_list):\n" % self.name)
           fp.write("   %s = v.gettemplate('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createtemplate('%s')\n" % (unique_name, self.name))
           fp.write("orientation = '%d'\n" % self.orientation)
        # Write out the TEXT template
           j=0
           a=[self.file,self.function,self.logicalmask,self.transformation,self.source,self.dataname,self.title,self.units,self.crdate,self.crtime,self.comment1,self.comment2,self.comment3,self.comment4,self.xname,self.yname,self.zname,self.tname,self.xunits,self.yunits,self.zunits,self.tunits]
           for i in ('file','function','logicalmask','transformation','source','dataname','title','units','crdate','crtime','comment1','comment2','comment3','comment4','xname','yname','zname','tname','xunits','yunits','zunits','tunits'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x = %g\n" % (unique_name,i,a[j].x))
             fp.write("%s.%s.y = %g\n" % (unique_name,i,a[j].y))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the FORMAT template
           j=0
           a=[self.xvalue,self.yvalue,self.zvalue,self.tvalue,self.mean,self.min,self.max]
           for i in ('xvalue','yvalue','zvalue','tvalue','mean','min','max'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x = %g\n" % (unique_name,i,a[j].x))
             fp.write("%s.%s.y = %g\n" % (unique_name,i,a[j].y))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].format))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the X-TICK template
           j=0
           a=[self.xtic1,self.xtic2,self.xmintic1,self.xmintic2]
           for i in ('xtic1','xtic2','xmintic1','xmintic2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.y1 = %g\n" % (unique_name,i,a[j].y1))
             fp.write("%s.%s.y2 = %g\n" % (unique_name,i,a[j].y2))
             fp.write("%s.%s.line = '%s'\n\n" % (unique_name,i,a[j].line))
             j=j+1

        # Write out the Y-TICK template
           j=0
           a=[self.ytic1,self.ytic2,self.ymintic1,self.ymintic2]
           for i in ('ytic1','ytic2','ymintic1','ymintic2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x1 = %g\n" % (unique_name,i,a[j].x1))
             fp.write("%s.%s.x2 = %g\n" % (unique_name,i,a[j].x2))
             fp.write("%s.%s.line = '%s'\n\n" % (unique_name,i,a[j].line))
             j=j+1

        # Write out the X-LABELS template
           j=0
           a=[self.xlabel1, self.xlabel2]
           for i in ('xlabel1','xlabel2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.y = %g\n" % (unique_name,i,a[j].y))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the Y-LABELS template
           j=0
           a=[self.ylabel1, self.ylabel2]
           for i in ('ylabel1','ylabel2'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x = %g\n" % (unique_name,i,a[j].x))
             fp.write("%s.%s.texttable = '%s'\n" % (unique_name,i,a[j].texttable))
             fp.write("%s.%s.textorientation = '%s'\n\n" % (unique_name,i,a[j].textorientation))
             j=j+1

        # Write out the BOXES and LINES template
           j=0
           a=[self.box1, self.box2, self.box1, self.box2, self.line1, self.line2, self.line3, self.line4]
           for i in ('box1','box2','box3','box4','line1','line2','line3','line4'):
             fp.write("# member = %s\n" % i)
             fp.write("%s.%s.priority = %g\n" % (unique_name,i,a[j].priority))
             fp.write("%s.%s.x1 = %g\n" % (unique_name,i,a[j].x1))
             fp.write("%s.%s.y1 = %g\n" % (unique_name,i,a[j].y1))
             fp.write("%s.%s.x2 = %g\n" % (unique_name,i,a[j].x2))
             fp.write("%s.%s.y2 = %g\n" % (unique_name,i,a[j].y2))
             fp.write("%s.%s.line = '%s'\n\n" % (unique_name,i,a[j].line))
             j=j+1

        # Write out the LEGEND SPACE template
           fp.write("# member = %s\n" % 'legend')
           fp.write("%s.legend.priority = %g\n" % (unique_name,self.legend.priority))
           fp.write("%s.legend.x1 = %g\n" % (unique_name,self.legend.x1))
           fp.write("%s.legend.y1 = %g\n" % (unique_name,self.legend.y1))
           fp.write("%s.legend.x2 = %g\n" % (unique_name,self.legend.x2))
           fp.write("%s.legend.y2 = %g\n" % (unique_name,self.legend.y2))
           fp.write("%s.legend.line = '%s'\n" % (unique_name,self.legend.line))
           fp.write("%s.legend.texttable = '%s'\n" % (unique_name,self.legend.texttable))
           fp.write("%s.legend.textorientation = '%s'\n\n" % (unique_name,self.legend.textorientation))

        # Write out the DATA SPACE template
           fp.write("# member = %s\n" % 'data')
           fp.write("%s.data.priority = %g\n" % (unique_name,self.data.priority))
           fp.write("%s.data.x1 = %g\n" % (unique_name,self.data.x1))
           fp.write("%s.data.y1 = %g\n" % (unique_name,self.data.y1))
           fp.write("%s.data.x2 = %g\n" % (unique_name,self.data.x2))
           fp.write("%s.data.y2 = %g\n\n" % (unique_name,self.data.y2))
           
    ## Adding the drawing functionnality to plot all these attributes on the Canvas

    def drawTicks(self,slab,gm,x,axis,number,vp,wc,bg=0):
        """Draws the ticks for the axis x number number
        using the label passed by the graphic  method
        vp and wc are from the actual canvas, they have been reset when they get here...
        """
        displays = []
        # compute the spanning in x and y, and adjust for the viewport
        if gm.datawc_x1 > 9.E19 :
             wc[0]=slab.getAxis(-1)[0]
        if gm.datawc_x2 > 9.E19 :
             wc[1]=slab.getAxis(-1)[-1]
        if gm.datawc_y1 > 9.E19 :
             wc[2]=slab.getAxis(-2)[0]
        if gm.datawc_y2 > 9.E19 :
             wc[3]=slab.getAxis(-2)[-1]

        vp=[self.data.x1,self.data.x2,self.data.y1,self.data.y2]
        dx=wc[1]-wc[0]
        dy=wc[3]-wc[2]
        dx=dx/(vp[1]-vp[0])
        dy=dy/(vp[3]-vp[2])
        # get the actual labels
        loc=copy.copy(getattr(gm,axis+'ticlabels'+number))
        # Are they set or do we need to it ?
        if (loc is None or loc=='*'):
            # well i guess we have to do it !
            if axis=='x':
                 ax=slab.getAxis(-1)
            else:
                 ax=slab.getAxis(-2)                 
            x2=ax[-1]
            x1=ax[0]
            loc=vcs_legacy.mkscale(ax[0],ax[-1])
            loc=vcs_legacy.mklabels(loc)
            if number == '2':
                for t in loc.keys():
                    loc[t]=''
        # Make sure the label passed are not outside the world coordinates
        dw1=1.E20
        dw2=1.E20
##         if getattr(gm,'datawc_'+axis+'1')<9.9E19:
##             dw1=getattr(gm,'datawc_'+axis+'1')
##             dw2=getattr(gm,'datawc_'+axis+'2')
##             for k in loc.keys():
##                 if getattr(gm,'datawc_'+axis+'2')>getattr(gm,'datawc_'+axis+'1'):
##                     if not (getattr(gm,'datawc_'+axis+'1')<=loc[k]<=getattr(gm,'datawc_'+axis+'2')):
##                         del(loc[k])
##                 else:
##                     if not (getattr(gm,'datawc_'+axis+'1')>=loc[k]>=getattr(gm,'datawc_'+axis+'2')):
##                         del(loc[k])

        if axis=='x':
             dw1,dw2=wc[0],wc[1]
        else:
             dw1,dw2=wc[2],wc[3]
        for k in loc.keys():
             if dw2>dw1:
                  if not(dw1<=k<=dw2):
                       del(loc[k])
             else:
                  if not (dw1>=k>=dw2):
                       del(loc[k])
        #The ticks
        obj=getattr(self,axis+'tic'+number)
        # the labels
        objlabl=getattr(self,axis+'label'+number)
        # the following to make sure we have a unique name,
        # i put them together assuming it would be faster
        ticks=x.createline(source=obj.line)
        tt=x.createtext(Tt_source=objlabl.texttable,To_source=objlabl.textorientation)
        ticks.priority=obj.priority
        tt.priority=objlabl.priority
        # initialize the list of values
        tstring=[]
        xs=[]
        ys=[]
        tys=[]
        txs=[]
        loc2=loc
        loc=getattr(gm,axis+'ticlabels'+number)
        if loc == '*' or loc is None:
             loc=loc2
        # set the x/y/text values
        for l in loc.keys():
          if axis=='x':
               mn,mx = vcs_legacy.minmax(wc[0],wc[1])
               if mn<=l<=mx:
                    xs.append([(l-wc[0])/dx+vp[0],(l-wc[0])/dx+vp[0]])
                    ys.append([obj.y1,obj.y2])
                    txs.append((l-wc[0])/dx+vp[0])
                    tys.append(objlabl.y)
                    tstring.append(loc[l])
          elif axis=='y':
               mn,mx = vcs_legacy.minmax(wc[2],wc[3])
               if mn<=l<=mx:
                    ys.append([(l-wc[2])/dy+vp[2],(l-wc[2])/dy+vp[2]])
                    xs.append([obj.x1,obj.x2])
                    tys.append((l-wc[2])/dy+vp[2])
                    txs.append(objlabl.x)
        # now does the mini ticks
        if getattr(gm,axis+'mtics'+number)!='':
            obj=getattr(self,axis+'mintic'+number)
            for l in getattr(gm,axis+'mtics'+number).keys():
                a=getattr(gm,axis+'mtics'+number)[l]
                if axis=='x':
                    mn,mx = vcs_legacy.minmax(wc[0],wc[1])
                    if mn<=l<=mx:
                         xs.append([(l-wc[0])/dx+vp[0],(l-wc[0])/dx+vp[0]])
                         ys.append([obj.y1,obj.y2])
                         tstring.append(a)
                elif axis=='y':
                    mn,mx = vcs_legacy.minmax(wc[2],wc[3])
                    if mn<=l<=mx:
                         ys.append([(l-wc[2])/dy+vp[2],(l-wc[2])/dy+vp[2]])
                         xs.append([obj.x1,obj.x2])
                         tstring.append(a)

        if txs!=[]:
             tt.string=tstring
             tt.x=txs
             tt.y=tys
             displays.append(x.text(tt,bg=bg))
        if xs!=[]:
             ticks.x=xs
             ticks.y=ys
             displays.append(x.line(ticks,bg=bg))
        return displays
   


    def reset(self,sub_name,v1,v2,ov1=None,ov2=None):
         """
         this function reset all the attribute who have a sub attribute named name1 or name2 or name
         also respect how far from original position you are
         i.e you move to x1,x2 from old_x1, old_x2
         if your current x1 value is not == to old_x1_value, then respect how far from it you  were
         usage:
         reset(sub_name,v1,v2,ov1=None,ov2=None)
         Example:
              t.reset('x',x1,x2,t.data.x1,t.data.x2)
              #where t is a vcs_legacy template object
         """
         savedmode=self.parent.mode
         self.parent.mode=0
         attr=vars(self).keys()
         n=len(sub_name)
         # computes the ratio
         if ov1 is not None:
              odv=ov2-ov1
              ratio=(v2-v1)/odv
         else:
              ratio=1.
         for a in attr:
              v=getattr(self,a)
              try:
                   subattr=vars(v).keys()
                   delta=0.
                   if sub_name+'1' in subattr:
                        ov=getattr(v,sub_name+'1')
                        if ov1 is not None:
                             delta=(ov-ov1)*ratio
                        setattr(v,sub_name+'1',v1+delta)
                   delta=0.
                   if sub_name+'2' in subattr:
                        ov=getattr(v,sub_name+'2')
                        if ov2 is not None:
                             delta=(ov-ov2)*ratio
                        setattr(v,sub_name+'2',v2+delta)
                   delta=0.
                   if sub_name in subattr:
                        ov=getattr(v,sub_name)
                        if ov1 is not None:
                             delta=(ov-ov1)*ratio
                        setattr(v,sub_name,v1+delta)
                        if a[-1]=='2':
                             ov=getattr(v,sub_name+'2')
                             if ov2 is not None:
                                  delta=(ov-ov2)*ratio
                             setattr(v,sub_name,v2+delta)
              except Exception,err:
                   #print err
                   pass
         self.parent.mode=savedmode

    def move(self,p,axis):
         """ move a template by p% along the axis 'x' or 'y'
         positive values of p mean movement toward right/top
         negative values of p mean movement toward left/bottom
         The reference point is t.data.x1/y1
         usage
         t.move(p,axis)
         example:
         t.move(.2,'x') # move everything right by 20%
         t.move(.2,'y') # move everything down by 20%
         """
         saved_mode=self.parent.mode
         self.parent.mode=0
         if not axis in ['x','y']:
              raise 'Error you can move the template only the x or y axis'
         #p/=100.
         ov1=getattr(self.data,axis+'1')
         ov2=getattr(self.data,axis+'2')
         v1=ov1+p
         v2=ov2+p
         self.reset(axis,v1,v2,ov1,ov2)
         self.parent.update()
         self.parent.mode=saved_mode
         
    def moveto(self,x,y):
         """ move a template along the axis 'x' or 'y' to p
         The reference point is t.data.x/y1
         usage
         t.moveto(x,y)
         example:
         t.moveto(.2,.2) # move everything so that data.x1=.2and data.y1=.2
         """
         saved_mode=self.parent.mode
         self.parent.mode=0
         #p/=100.
         ov1=getattr(self.data,'x1')
         ov2=getattr(self.data,'x2')
         v1=x
         v2=(ov2-ov1)+x
         self.reset('x',v1,v2,ov1,ov2)
         ov1=getattr(self.data,'y1')
         ov2=getattr(self.data,'y2')
         v1=y
         v2=(ov2-ov1)+y
         self.reset('y',v1,v2,ov1,ov2)
         self.parent.update()
         self.parent.mode=saved_mode
         
    def scale(self,scale,axis='xy',font=-1):
         """ scale a template along the axis 'x' or 'y' by scale
         positive values of p mean increase
         negative values of p mean decrease
         The reference point is t.data.x/y1
         usage
         t.scale(scale,axis='xy',font=-1)
         scale: any number
         axis can be 'x','y' or 'xy' (which means both)
         font can be 1/0/-1
                     0: means do not scale the fonts
                     1: means scale the fonts
                    -1: means do not scale the fonts unless axis='xy'
         Example:
         x=vcs_legacy.init()
         t=x.createtemplate('a_template')
         t.scale(.5)  # halves the template size
         t.scale(1.2) # upsize everything to 20% more than the original size
         t.scale(2,'x') # double the x axis

         reference is t.data.x1/y1
 
         """
         saved_mode=self.parent.mode
         self.parent.mode=0
         if not axis in ['x','y','xy']:
              raise 'Error you can move the template only the x or y axis'
         #p/=100.
         if axis=='xy':
              axis=['x','y']
         else:
              axis=[axis,]
         for ax in axis:
              ov1=getattr(self.data,ax+'1')
              ov2=getattr(self.data,ax+'2')
              v1=ov1
              v2=(ov2-ov1)*scale+v1
              self.reset(ax,v1,v2,ov1,ov2)
         if font==1 or (font==-1 and axis==['x','y']):
              self.scalefont(scale)
         self.parent.update()
         self.parent.mode=saved_mode
              
    def scalefont(self,scale):
         """
         Scales the tempate font by scale
         Usage:
         scalefont(scale)
         
         Example:
         x=vcs_legacy.init()
         t=x.createtemplate('a_template')
         t.scalefont(.5) # reduces the fonts size by 2
         """
         savedmode=self.parent.mode
         self.parent.mode=0
         attr=vars(self).keys()
         for a in attr:
              v=getattr(self,a)
              try:
                   to=getattr(v,'textorientation')
                   to=self.parent.createtextorientation(source=to)
                   to.height=to.height*scale
                   setattr(v,'textorientation',to)
              except:
                   pass        
         self.parent.mode=savedmode
         self.parent.update()
         
    def plot(self,slab,gm,bg=0,min=None,max=None):
        """ This plots the template stuff on the Canvas, it needs a slab and a graphic method
        returns a list containing all the displays used"""
        displays = []
        # first makes sure there is a Canvas
        x=self.parent
        # now remembers the viewport and worldcoordinates in order to reset them later
        vp=x.viewport
        wc=x.worldcoordinate
        m=x.mode
        # and resets everything to [0,1]
        x.viewport=[0,1,0,1]
        x.worldcoordinate=[0,1,0,1]
        x.mode=0 # this should disable the replot but it doesn't work....
        # figures out the min and max and set them as atributes...
        if min is None or max is None:
             mn,mx=vcs_legacy.minmax(slab)
        else:
             mn,mx=min,max
        try:
            setattr(slab,'min',mn)
            setattr(slab,'max',mx)
        except:
             pass

        attributes=['file','function','logicalmask','transformation',
                    'source','id','title','units','crdate','crtime',
                    'comment1','comment2','comment3','comment4','xname','yname',
                    'zname','tname','zunits','tunits','xvalue','yvalue','zvalue',
                    'tvalue','mean','min','max']
        if gm=='taylordiagram':
             attributes=attributes[:-3]

        for s in attributes: # loop through various section of the template object
            if hasattr(slab,s):
                if s=='id':
                    sub=self.dataname
                else:
                    sub=getattr(self,s)
                tt=x.createtext(None,sub.texttable,None,sub.textorientation)
                # Now for the min/max/mean add the name in front
                if s=='min':
                    tt.string='Min '+str(getattr(slab,s))
                elif s=='max':
                    tt.string='Max '+str(getattr(slab,s))
                elif s=='mean':
                    if not inspect.ismethod(getattr(slab,'mean')):
                         tt.string='Mean '+str(getattr(slab,s))
                    else:
                         tt.string='Mean %f'%slab.mean()
                else :
                    tt.string=str(getattr(slab,s))
                tt.x=[sub.x]
                tt.y=[sub.y]
                tt.priority=sub.priority
                displays.append(x.text(tt,bg=bg))


        # Do the tickmarks/labels
        if gm!='taylordiagram':
             displays+=self.drawTicks(slab,gm,x,axis='x',number='1',vp=vp,wc=wc,bg=bg)
             displays+=self.drawTicks(slab,gm,x,axis='x',number='2',vp=vp,wc=wc,bg=bg)
             displays+=self.drawTicks(slab,gm,x,axis='y',number='1',vp=vp,wc=wc,bg=bg)
             displays+=self.drawTicks(slab,gm,x,axis='y',number='2',vp=vp,wc=wc,bg=bg)

        # Do the boxes and lines
        b=self.box1
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.box2
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.box3
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.box4
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2,b.x2,b.x1,b.x1]
             l.y=[b.y1,b.y1,b.y2,b.y2,b.y1]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.line1
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.line2
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.line3
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))

        b=self.line4
        if b.priority!=0:
             l=x.createline(source=b.line)
             l.x=[b.x1,b.x2]
             l.y=[b.y1,b.y2]
             l.priority=b.priority
             displays.append(x.line(l,bg=bg))
        x.mode=m
        # I think i have to use dict here because it's a valid value
        # (obviously since i got it from the object itself and didn't touch it
        # but Dean doesn't allow to set it back to some of these values (None)!
        x.viewport=vp
        x.worldcoordinate=wc
        return displays

    def drawColorBar(self,colors,levels,legend=None,ext_1='n',ext_2='n',x=None,bg=0,priority=None):
         """
         This function, draws the colorbar, it needs:
         colors : The colors to be plotted
         levels : The levels that each color represent
         legend : To overwrite, saying just draw box at certain values and display some specific text instead of the value
         ext_1 and ext_2: to draw the arrows
         x : the canvas where to plot it
         bg: background mode ?
         returns a list of displays used
         """
         displays=[]
         #
         # Create legend
         #
         # First make sure we have an vcs_legacy canvas
         if x is None:
              x=self.parent

         # Now sets the priority value
         if priority is None:
              priority=self.legend.priority
         # Now resets the viewport and worldcoordinate
         vp=x.viewport  # preserve for later restore
         wc=x.worldcoordinate # preserve for later restore
         x.viewport=[0.,1.,0.,1.]
         x.worldcoordinate=[0.,1.,0.,1.]
         # Ok first determine the orientation of the legend (bottom to top  or left to right)
         dX=self.legend.x2-self.legend.x1
         dY=self.legend.y2-self.legend.y1
        # Now figure out the typical length of a box
         nbox=len(colors)
         if abs(dX)>=abs(dY):
              isH=1
              dLong=dX
              dlong = dX/nbox
              dshrt = dY
              startlong = self.legend.x1
              startshrt = self.legend.y1
         else:
              isH=0
              dLong=dY
              dlong = dY/nbox
              dshrt = dX
              startlong = self.legend.y1
              startshrt = self.legend.x1
         # initialize the fillarea coordinates
         L=[]
         S=[]
         # computes the fillarea coordinates
         iext=0 # To know if we changed the dims
         minarrow=.02 # % of the legend that the arrow must use (at least)
         if (ext_1=='y' or ext_2=='y' ) and dlong<minarrow*dLong:
              iext=1 # one mins changed ext_1
              if ext_1=='y' and ext_2=='y':
                   dlong=dLong*(1.-2.*minarrow)/(nbox-2.)
                   iext=3 # changed both side
              else:
                   dlong=dLong*(1.-minarrow)/(nbox-1.)
                   if ext_2=='y': iext=2
                   
         for i in range(nbox):
              if ext_1=='y' and i==0:
                   if iext==1 or iext==3: # did the chage the size of the arrow
                        dlongarrow=minarrow*dLong
                   else:
                        dlongarrow=dlong
                   # Draws the little arrow at the begining
                   L.append([startlong,          startlong+dlongarrow, startlong+dlongarrow])
                   S.append([startshrt+dshrt/2., startshrt+dshrt     , startshrt      ])
                   # Now readjust startlong if necessary
                   if iext==1 or iext==3:
                        startlong=startlong-dlong+minarrow*dLong
              elif ext_2=='y' and i==nbox-1:
                   if iext>1: # we changed the size of the arrow at the end
                        dlongarrow=dLong*minarrow
                   else:
                        dlongarrow=dlong
                   # Draws the little arrow at the end
                   L.append([startlong+dlong*i+dlongarrow, startlong+dlong*i, startlong+dlong*i])
                   S.append([startshrt+dshrt/2.   , startshrt+dshrt  , startshrt      ])
              else:
                   # Draws a normal box
                   #print i,dlong,dshrt,startlong,startshrt
                   L.append([startlong+dlong*i, startlong+dlong*(i+1), startlong+dlong*(i+1), startlong+dlong*i])
                   S.append([startshrt        , startshrt            , startshrt+dshrt      , startshrt+dshrt])
                    
         try:
              fa=x.createfillarea('__leg')
         except:
              pass
         fa=x.getfillarea('__leg')
         fa.color=colors
         fa.style='solid'
         fa.priority=priority
         if isH:
              fa.x=L
              fa.y=S
         else:
              fa.x=S
              fa.y=L
##          fa.list()
         displays.append(x.fillarea(fa,bg=bg))
         # Now draws the legend
         # Fisrt of all make sure we draw the arrows
         Sl=[]
         Ll=[]
         St=[]
         Lt=[]
         Tt=[]
         dD=dLong # length of the levels area
         if ext_1=='y':
              startlong=startlong+dlong
              Sl.append(S[0])
              Ll.append(L[0])
              # we need to close the arrow
              Sl[0].append(Sl[0][0])
              Ll[0].append(Ll[0][0])
              levels.pop(0)
              if iext==1 or iext==3:
                   dD=dD-dLong*minarrow
              else:
                   dD=dD-dlong
         if ext_2=='y':
              Sl.append(S[-1])
              Ll.append(L[-1])
              # we need to close the arrow
              Sl[0].append(Sl[0][0])
              Ll[0].append(Ll[0][0])
              levels.pop(-1)
              if iext>1:
                   dD=dD-dLong*minarrow
              else:
                   dD=dD-dlong
         # adds the coordinate for the box around the legend
         Sl.append([startshrt      , startshrt         , startshrt+dshrt   , startshrt+dshrt, startshrt])
         Ll.append([startlong      , startlong+dD      , startlong+dD      , startlong      , startlong])
         # Now make sure we have a legend
         if legend is None:
              legend=vcs_legacy.mklabels(levels)
         if levels[0]<levels[1]:
              ecompfunc=numpy.less_equal
              compfunc=numpy.less             
         else:
              ecompfunc=numpy.greater_equal
              compfunc=numpy.greater
##          legend[levels[0]]=vcs_legacy.mklabels([levels[0]],output='list')[0]
##          legend[levels[-1]]=vcs_legacy.mklabels([levels[-1]],output='list')[0]
##          legend=vcs_legacy.mklabels(legend.keys())
         for l in legend.keys():
              if not compfunc(l,levels[0]) and not compfunc(levels[-1],l):
                   for i in range(len(levels)-1):
                        if ecompfunc(levels[i],l) and ecompfunc(l,levels[i+1]):
                             # Ok we're between 2 levels, let's add the legend
                             # first let's figure out where to put it
                             loc=i*dlong # position at beginnig of level
                             loc+=(l-levels[i])/(levels[i+1]-levels[i])*dlong # Adds the distance from beginnig of level box
                             loc+=startlong ## Figures out the begining
##                              loc=((l-levels[0])/(levels[-1]-levels[0]))*dD+startlong
                             Ll.append([loc,loc])
                             Sl.append([startshrt,startshrt+dshrt])
                             Lt.append(loc)
                             St.append(startshrt+dshrt*1.4)
                             Tt.append(legend[l])
                             break
         # ok now creates the line object and text object
         ln=x.createline(source=self.legend.line)
         txt=x.createtext(To_source=self.legend.textorientation,Tt_source=self.legend.texttable)
         ln.priority=priority+1
         txt.priority=priority+1
         txt.string=Tt
         if isH:
              ln.x=Ll
              ln.y=Sl
              txt.x=Lt
              txt.y=St
         else:
              ln.x=Sl
              ln.y=Ll
              txt.x=St
              txt.y=Lt
         
         # Now reset the viewport and worldcoordiantes
         displays.append(x.line(ln,bg=bg))
         displays.append(x.text(txt,bg=bg))
         x.viewport=vp
         x.worldcoordinate=wc
         return displays

    def ratio_linear_projection(self,lon1,lon2,lat1,lat2,Rwished=None,Rout=None,box_and_ticks=0):
         '''
         Computes ratio to shrink the data area of a template in order that the overall area
         has the least possible deformation in linear projection

         Version: 1.1
         Notes: Thanks to Karl Taylor for the equation of "optimal" ratio

         Necessary arguments:
           lon1, lon2: in degrees_east  : Longitude spanned by plot
           lat1, lat2: in degrees_north : Latitude  spanned by plot
         Optional arguments:
           Rwished: Ratio y/x wished, None=automagic
           Rout: Ratio of output (default is US Letter=11./8.5)
                 Also you can pass a string: "A4","US LETTER", "X"/"SCREEN", the latest uses the window information
           box_and_ticks: Also redefine box and ticks to the new region
         Returned:
           vcs_legacy template object

         Usage example:
           ## USA
           t.ratio_linear_projection(-135,-50,20,50)
         '''
         

         # Converts lat/lon to rad
         Lat1=lat1/180.*numpy.pi
         Lat2=lat2/180.*numpy.pi
         Lon1=lon1/180.*numpy.pi
         Lon2=lon2/180.*numpy.pi

         if (Lon1==Lon2) or (Lat1==Lat2):
              return
         
         
         if Rwished is None:
             Rwished=float(2*(numpy.sin(Lat2)-numpy.sin(Lat1))/(Lon2-Lon1)/(1.+(numpy.sin(2*Lat2)-numpy.sin(2*Lat1))/2./(Lat2-Lat1)))
         self.ratio(Rwished,Rout,box_and_ticks)
         return

    
    def ratio(self,Rwished,Rout=None,box_and_ticks=0):
         '''
         Computes ratio to shrink the data area of a template to have an y/x ratio of Rwished
         has the least possible deformation in linear projection

         Version: 1.1

         Necessary arguments:
           Rwished: Ratio y/x wished
         Optional arguments:
           Rout: Ratio of output (default is US Letter=11./8.5)
                 Also you can pass a string: "A4","US LETTER", "X"/"SCREEN", the latest uses the window information
           box_and_ticks: Also redefine box and ticks to the new region
         Returned:
           vcs_legacy template object

         Usage example:
           ## y is twice x
           t.ratio(2)
         '''

         x=self.parent
         if isinstance(Rout,str):
              if Rout.lower()=='a4':
                   Rout=29.7/21.
                   if x.isportrait():
                        Rout=1./Rout
              elif Rout.lower() in ['us letter','letter','us_letter','usletter']:
                   Rout=11./8.5
                   if x.isportrait():
                        Rout=1./Rout
              elif Rout.lower() in ['x','x11','screen']:
                   if x.iscanvasdisplayed(): # do we have the canvas opened ?
                        info=x.canvasinfo()
                        Rout=float(info['width'])/float(info['height'])
                   else: # Not opened yet, assuming default size: 959/728
                        Rout=1./.758800507
                        if x.isportrait():
                             Rout=1./Rout
         elif Rout is None:
              Rout=11./8.5

         
         t=x.createtemplate(source=self.name)

         # Output ratio
         if x.isportrait():
              Rout=1/Rout
         # Computes the template ratio
         Rt=(self.data.y2-self.data.y1)/(self.data.x2-self.data.x1)

         # Actual ratio template and output format combined
         Ra=Rt/Rout

         if Rwished>Ra:
              t.scale(Ra/Rwished,axis='x')
         else:
              t.scale(Rwished/Ra,axis='y')
         ndx=t.data.x2-t.data.x1
         ndy=t.data.y2-t.data.y1

         odx=self.data.x2-self.data.x1
         ody=self.data.y2-self.data.y1

         self.data.x1=t.data.x1
         self.data.x2=t.data.x2
         self.data.y1=t.data.y1
         self.data.y2=t.data.y2
     ##     print odx,ndx
         if odx!=ndx:
             self.data.x1+=(odx-ndx)/2.
             self.data.x2+=(odx-ndx)/2.
         else:
             self.data.y1+=(ody-ndy)/2.
             self.data.y2+=(ody-ndy)/2.

         if box_and_ticks:
              # Box1 resize
              self.box1.x1=self.data.x1
              self.box1.x2=self.data.x2
              self.box1.y1=self.data.y1
              self.box1.y2=self.data.y2
              # xLabel distance save
              dY1=self.xlabel1.y-self.xtic1.y1
              dY2=self.xlabel2.y-self.xtic2.y1
              # xLabel distance save
              dX1=self.ylabel1.x-self.ytic1.x1
              dX2=self.ylabel2.x-self.ytic2.x1
              # X tic
              dy=self.xtic1.y2-self.xtic1.y1
              self.xtic1.y1=self.data.y1
              self.xtic1.y2=self.xtic1.y1+dy
              dy=self.xtic2.y2-self.xtic2.y1
              self.xtic2.y1=self.data.y2
              self.xtic2.y2=self.xtic2.y1+dy
              # Xmin tic
              dy=self.xmintic1.y2-self.xmintic1.y1
              self.xmintic1.y1=self.data.y1
              self.xmintic1.y2=self.xtic1.y1+dy
              dy=self.xmintic2.y2-self.xmintic2.y1
              self.xmintic2.y1=self.data.y2
              self.xmintic2.y2=self.xmintic2.y1+dy
              # Y tic
              dx=self.ytic1.x2-self.ytic1.x1
              self.ytic1.x1=self.data.x1
              self.ytic1.x2=self.ytic1.x1+dx
              dx=self.ytic2.x2-self.ytic2.x1
              self.ytic2.x1=self.data.x2
              self.ytic2.x2=self.ytic2.x1+dx
              # Ymin tic
              dx=self.ymintic1.x2-self.ymintic1.x1
              self.ymintic1.x1=self.data.x1
              self.ymintic1.x2=self.ymintic1.x1+dx
              dx=self.ymintic2.x2-self.ymintic2.x1
              self.ymintic2.x1=self.data.x2
              self.ymintic2.x2=self.ymintic2.x1+dx
              # Xlabels
              self.xlabel1.y=self.xtic1.y1+dY1
              self.xlabel2.y=self.xtic2.y1+dY2
              # Ylabels
              self.ylabel1.x=self.ytic1.x1+dX1
              self.ylabel2.x=self.ytic2.x1+dX2
              self.data._ratio = -Rwished
         else:
              self.data._ratio = Rwished
              
              

         return

         


###############################################################################
#                                                                             #
#        END OF FILE                                                          #
#                                                                             #
###############################################################################

