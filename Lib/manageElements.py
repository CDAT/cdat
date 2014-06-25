## This file aims at removing elets creation from dpeending on a Canvas, we will try to simply have 
## b = vcs.createboxfill()
## rather than
## x=vcs.init()
## b=x.createboxfill()
import vcs
import boxfill,meshfill,isofill,isoline,unified1D,template,projection
import fillarea,marker,line,texttable,textorientation,textcombined
from xmldocs import plot_keywords_doc,graphics_method_core,axesconvert,xaxisconvert,yaxisconvert, plot_1D_input, plot_2D_input, plot_output, plot_2_1D_input, create_GM_input, get_GM_input, boxfill_output, isofill_output, isoline_output, yxvsx_output, xyvsy_output, xvsy_output, scatter_output, outfill_output, outline_output, plot_2_1D_options
import random
import warnings
from error import vcsError
import dv3d

def check_name_source(name,source,typ):
  """makes ure it is a unique name for this type or generates a name for user"""
  elts = vcs.listelements(typ)
  if name is None:
      rnd = random.randint(0,1000000000000000)
      name = '__%s_%i' % (typ,rnd)
      while name in elts:
          rnd = random.randint(0,1000000000000000)
          name = '__%s_%i' % (typ,rnd)
  if not isinstance(name,str):
      raise vcsError, '%s object name must be a string or %s name' % (typ,typ)

  if not isinstance(source,str):
      exec("ok = vcs.is%s(source)" % (typ,))
  else:
      ok=0
  if (not isinstance(source,str)) and ok==0:
      raise vcsError,'Error %s object source must be a string or a %s object' % (typ,typ)
  elif ok:
      source=source.name

  if name in elts:
      raise vcsError, "Error %s object named %s already exists" % (typ,name)
  if not source in elts and typ!="display":
      raise vcsError, "Error source %s object (%s) does not exist!" % (typ,name)
  return name,source

def createtemplate(name=None, source='default'):
    """
Function: createtemplate                  # Construct a new template

Description of Function:
Create a new template given the the name and the existing template to copy
the attributes from. If no existing template name is given, then the default
template will be used as the template to which the attributes will be copied
from.

If the name provided already exists, then a error will be returned. Template
names must be unique.

Example of Use:
con=vcs.createtemplate('example1') # create 'example1' template from 'default' template 
vcs.listelements('template')                       # Show all the existing templates
con=vcs.createtemplate('example2','quick') # create 'example2' from 'quick' template
"""
    name,source = check_name_source(name,source,'template')

    return template.P(name, source)

def gettemplate(Pt_name_src='default'):
    """
Function: gettemplate                       # Construct a new template

Description of Function:
VCS contains a list of predefined templates. This function will create a
template class object from an existing VCS template. If no template name
is given, then template 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createtemplate function.)

Example of Use:
vcs.listelements('template')                  # Show all the existing templates
templt=vcs.gettemplate()              # templt instance of 'default' template
templt2=vcs.gettemplate('quick')      # templt2 contains 'quick' template
"""
    # Check to make sure the argument passed in is a STRING
    if not isinstance(Pt_name_src,str):
       raise vcsError, 'The argument must be a string.'

    if not Pt_name_src in vcs.elements["template"].keys():
      raise ValueError, "template '%s' does not exists" % Pt_name_src
    return vcs.elements["template"][Pt_name_src]


def createprojection(name=None, source='default'):
    """
Function: createprojection                # Construct a new projection method

Description of Function:
Create a new projection method given the the name and the existing
projection method to copy the attributes from. If no existing
projection method name is given, then the default projection
method will be used as the projection method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Projection
method names must be unique.

Example of Use:
vcs.show('projection')
p=vcs.createprojection('example1',)
vcs.show('projection')
p=vcs.createprojection('example2','quick')
vcs.show('projection')
"""

    name,source = check_name_source(name,source,'projection')
    return projection.Proj(name, source)

def getprojection(Proj_name_src='default'):
    """
Function: getprojection                    # Construct a new projection method

Description of Function:
VCS contains a list of graphics methods. This function will create a
projection class object from an existing VCS projection method. If
no projection name is given, then projection 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a 
different name can be modified. (See the createprojection function.)

Example of Use:
vcs.show('projection')                   # Show all the existing projection methods
p=vcs.getprojection()                  # box instance of 'default' projection
                                    # method
p2=vcs.getprojection('quick')          # box2 instance of existing 'quick' projection
                                    #         graphics method
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Proj_name_src,str):
       raise vcsError, 'The argument must be a string.'

    if not Proj_name_src in vcs.elements["projection"]:
      raise vcsError,"No such projection '%s'" % Proj_name_src
    return vcs.elements["projection"][Proj_name_src]

def createboxfill(name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createboxfill                # Construct a new boxfill graphics method

Description of Function:
Create a new boxfill graphics method given the the name and the existing
boxfill graphics method to copy the attributes from. If no existing
boxfill graphics method name is given, then the default boxfill graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
vcs.show('boxfill')
box=vcs.createboxfill('example1',)
vcs.show('boxfill')
box=vcs.createboxfill('example2','quick')
vcs.show('boxfill')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createboxfill Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

    name,source = check_name_source(name,source,'boxfill')
    return boxfill.Gfb(name, source)
createboxfill.__doc__ = createboxfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, boxfill_output) 

def getboxfill(Gfb_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::
Function: getboxfill                        # Construct a new boxfill graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
boxfill class object from an existing VCS boxfill graphics method. If
no boxfill name is given, then boxfill 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a 
different name can be modified. (See the createboxfill function.)

Example of Use:
vcs.show('boxfill')                   # Show all the existing boxfill graphics methods
box=vcs.getboxfill()                  # box instance of 'default' boxfill graphics
                                    # method
box2=vcs.getboxfill('quick')          # box2 instance of existing 'quick' boxfill
                                    #         graphics method
######################################################################################################################
###########################################                            ###############################################
########################################## End getboxfill Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfb_name_src,str):
       raise vcsError, 'The argument must be a string.'

    if not Gfb_name_src in vcs.elements["boxfill"].keys():
      raise "The boxfill method: '%s' does not seem to exist"
    return vcs.elements["boxfill"][Gfb_name_src]
getboxfill.__doc__ = getboxfill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, boxfill_output) 

def createtaylordiagram(name=None, source='default'):
    """
Function: createtaylordiagram  # Construct a new taylordiagram graphics method

Description of Function:
Create a new taylordiagram graphics method given the the name and the existing
taylordiagram graphics method to copy the attributes from. If no existing
taylordiagram graphics method name is given, then the default taylordiagram graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
vcs.show('taylordiagram')
td=vcs.createtaylordiagram('example1',)
vcs.show('taylordiagram')
td=vcs.createtaylordiagram('example2','quick')
vcs.show('taylordiagram')
"""
    
    name,source = check_name_source(name,source,'taylordiagram')
    if name in vcs.elements["taylordiagram"].keys():
      raise vcsError, 'Error creating taylordiagram graphic method: '+Gtd_name+' already exist'
    if not source in vcs.elements["taylordiagram"].keys():
      raise vcsError, 'Error creating taylordiagram graphic method '+Gtd_name_src+' does not exist'
    n=vcs.taylor.Gtd()
    n._name=name
    m = vcs.elements["taylordiagram"][source]
    n.max=m.max
    n.quadrans=m.quadrans
    n.skillValues=m.skillValues
    n.skillColor=m.skillColor
    n.skillDrawLabels=m.skillDrawLabels
    n.skillCoefficient=m.skillCoefficient
    n.detail=m.detail
    n.referencevalue=m.referencevalue
    n.Marker=copy.deepcopy(m.Marker)
    n.arrowlength=m.arrowlength
    n.arrowangle=m.arrowangle
    n.arrowbase=m.arrowbase
    n.xticlabels1=m.xticlabels1
    n.xmtics1=m.xmtics1
    n.yticlabels1=m.yticlabels1
    n.ymtics1=m.xmtics1
    n.cticlabels1=m.cticlabels1
    n.cmtics1=m.xmtics1
            
    vcs.elements["taylordiagram"][name]=n
    n.Marker.equalize()
    return n

def gettaylordiagram(Gtd_name_src='default'):
    """
Function: gettaylordiagram                     # Construct a new taylordiagram graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
taylordiagram class object from an existing VCS taylordiagram graphics method. If
no taylordiagram name is given, then taylordiagram 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a 
different name can be modified. (See the createboxfill function.)

Example of Use:
vcs.show('taylordiagram')                    # Show all the existing taylordiagram graphics methods
td=vcs.gettaylordiagram()                    # td instance of 'default' taylordiagram graphics
                                           # method
td2=vcs.gettaylordiagram('default')          # td2 instance of existing 'default' taylordiagram
                                           #         graphics method
                                    """
    
    
    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gtd_name_src,str):
        raise vcsError, 'The argument must be a string.'
    
    for m in vcs.taylordiagrams:
        if m.name==Gtd_name_src:
##                 n=copy.copy(m)
            n=m
            #vcs.taylordiagrams.append(n)
            n.Marker.equalize()
            return n
    warnings.warn("Possible implementation issue here trying to access %s" % (Gtd_name_src))
    return 

def createmeshfill(name=None, source='default'):
    """
Function: createmeshfill                # Construct a new meshfill graphics method

Description of Function:
Create a new meshfill graphics method given the the name and the existing
meshfill graphics method to copy the attributes from. If no existing
meshfill graphics method name is given, then the default meshfill graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
vcs.show('meshfill')
mesh=vcs.createmeshfill('example1',)
vcs.show('meshfill')
mesh=vcs.createmeshfill('example2','quick')
vcs.show('meshfill')
"""
    name,source = check_name_source(name,source,'meshfill')
    return meshfill.Gfm(name, source)

def getmeshfill(Gfm_name_src='default'):
    """
Function: getmeshfill                        # Construct a new meshfill graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
meshfill class object from an existing VCS meshfill graphics method. If
no meshfill name is given, then meshfill 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a 
different name can be modified. (See the createmeshfill function.)

Example of Use:
a=vcs.init()
a.show('meshfill')                   # Show all the existing meshfill graphics methods
mesh=a.getmeshfill()                  # mesh instance of 'default' meshfill graphics
                                    # method
mesh2=a.getmeshfill('quick')          # mesh2 instance of existing 'quick' meshfill
                                    #         graphics method
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfm_name_src,str):
       raise vcsError, 'The argument must be a string.'

    if not Gfm_name_src in vcs.elements["meshfill"]:
      raise ValueError,"meshfill '%s' does not exists" % Gfm_name_src

    return vcs.elements["meshfill"][Gfm_name_src]

def createisofill(name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createisofill  # Construct a new isofill graphics method

Description of Function:
Create a new isofill graphics method given the the name and the existing
isofill graphics method to copy the attributes from. If no existing
isofill graphics method name is given, then the default isofill graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
vcs.show('isofill')
iso=vcs.createisofill('example1',)
vcs.show('isofill')
iso=vcs.createisofill('example2','quick')
vcs.show('isofill')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createisofill Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

    name,source = check_name_source(name,source,'isofill')
    return isofill.Gfi(name, source)
createisofill.__doc__ = createisofill.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, isofill_output)

def getisofill(Gfi_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: getisofill          Construct a new isofill graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
isofill class object from an existing VCS isofill graphics method. If
no isofill name is given, then isofill 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createisofill function.)

Example of Use:
vcs.show('isofill')                   # Show all the existing isofill graphics methods
iso=vcs.getisofill()                  # iso instance of 'default' isofill graphics
                                    #       method
iso2=vcs.getisofill('quick')          # iso2 instance of existing 'quick' isofill
                                    #       graphics method
######################################################################################################################
###########################################                            ###############################################
########################################## End getisofill Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfi_name_src,str):
       raise vcsError, 'The argument must be a string.'

    if not Gfi_name_src in vcs.elements["isofill"]:
      raise ValueError,"The isofill '%s' does not exists" % Gfi_name_src
    return vcs.elements["isofill"][Gfi_name_src]
getisofill.__doc__ = getisofill.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, isofill_output)

def createisoline(name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createisoline                # Construct a new isoline graphics method

Description of Function:
Create a new isoline graphics method given the the name and the existing
isoline graphics method to copy the attributes from. If no existing
isoline graphics method name is given, then the default isoline graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:

vcs.show('isoline')
iso=vcs.createisoline('example1',)
vcs.show('isoline')
iso=vcs.createisoline('example2','quick')
vcs.show('isoline')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createisoline Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""

    name,source = check_name_source(name,source,'isoline')
    return isoline.Gi(name, source)
createisoline.__doc__ = createisoline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, isoline_output)

def getisoline(Gi_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: getisoline                        # Construct a new isoline graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
isoline class object from an existing VCS isoline graphics method. If
no isoline name is given, then isoline 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createisoline function.)

Example of Use:
vcs.show('isoline')                   # Show all the existing isoline graphics methods
iso=vcs.getisoline()                  # iso instance of 'default' isoline graphics
                                    #       method
iso2=vcs.getisoline('quick')          # iso2 instance of existing 'quick' isoline
                                    #       graphics method
######################################################################################################################
###########################################                            ###############################################
########################################## End getisoline Description ################################################
#########################################                            #################################################
######################################################################################################################
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gi_name_src,str):
       raise vcsError, 'The argument must be a string.'
    if not Gi_name_src in vcs.elements["isoline"]:
       raise ValueError,"The isoline '%s' does not exists" % Gi_name_src
    return vcs.elements["isoline"][Gi_name_src]
getisoline.__doc__ = getisoline.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, isoline_output)

def createoneD(name=None,source='default'):
    name,source = check_name_source(name,source,'oned')
    return unified1D.G1d(name,source)

def getoneD(name):
    # Check to make sure the argument passed in is a STRING
    if not isinstance(name,str):
       raise vcsError, 'The argument must be a string.'

    if not name in vcs.elements["oned"]:
      raise ValueError,"The oneD '%s' graphics method does not exists" % name
    return vcs.elements["oned"][name]

def createxyvsy(name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createxyvsy                  # Construct a new Xyvsy graphics method

Description of Function:
Create a new Xyvsy graphics method given the the name and the existing
Xyvsy graphics method to copy the attributes from. If no existing
Xyvsy graphics method name is given, then the default Xyvsy graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:

a=vcs.init()
vcs.show('xyvsy')
xyy=vcs.createxyvsy('example1',)
vcs.show('xyvsy')
xyy=vcs.createxyvsy('example2','quick')
vcs.show('xyvsy')

#######################################################################################################################
###########################################                             ###############################################
########################################## End createxyvsy Description ################################################
#########################################                             #################################################
#######################################################################################################################

"""

    warnings.warn("the createxyvsy method is now obsolete, 1D graphics method have been unified, to avoid your code breaking in the future please change it to use: createoneD and set the 'flip' option to True")
    if source[-7:]=="_xyvsy_":
      source = source[:-7]
    name,source = check_name_source(name,source,'xyvsy')

    gm = unified1D.G1d(name+"_xyvsy_", source+"_xyvsy_")
    gm.flip = True
    vcs.elements["xyvsy"][name]=gm
    return gm
createxyvsy.__doc__ = createxyvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, xyvsy_output) 

def getxyvsy(GXy_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: getxyvsy        # Construct a new Xyvsy graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
Xyvsy class object from an existing VCS Xyvsy graphics method. If
no Xyvsy name is given, then Xyvsy 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createxyvsy function.)

Example of Use:
a=vcs.init()
vcs.show('xyvsy')                     # Show all the existing Xyvsy graphics methods
xyy=vcs.getxyvsy()                    # xyy instance of 'default' Xyvsy graphics
                                    #       method
xyy2=vcs.getxyvsy('quick')            # xyy2 instance of existing 'quick' Xyvsy
                                    #       graphics method
####################################################################################################################
###########################################                          ###############################################
########################################## End getxyvsy Description ################################################
#########################################                          #################################################
####################################################################################################################

"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(GXy_name_src,str):
       raise vcsError, 'The argument must be a string.'
    if GXy_name_src[-7:]=="_xyvsy_":
      GXy_name_src = GXy_name_src[:-7]

    if not GXy_name_src in vcs.elements["xyvsy"]:
      raise ValueError,"The xyvsy '%s' graphics method does not exists" % GXy_name_src
    return vcs.elements["xyvsy"][GXy_name_src]
getxyvsy.__doc__ = getxyvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, get_GM_input, xyvsy_output) 

def createyxvsx(name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createyxvsx                  # Construct a new Yxvsx graphics method

Description of Function:
Create a new Yxvsx graphics method given the the name and the existing
Yxvsx graphics method to copy the attributes from. If no existing
Yxvsx graphics method name is given, then the default Yxvsx graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:

a=vcs.init()
vcs.show('yxvsx')
yxx=vcs.createyxvsx('example1',)
vcs.show('yxvsx')
yxx=vcs.createyxvsx('example2','quick')
vcs.show('yxvsx')

#######################################################################################################################
###########################################                             ###############################################
########################################## End createyxvsx Description ################################################
#########################################                             #################################################
#######################################################################################################################

"""

    warnings.warn("the createyxvsx method is now obsolete, 1D graphics method have been unified,to avoid your code breaking in the future please change it to use: createoneD")
    if source[-7:]=="_yxvsx_":
      source = source[:-7]
    name,source = check_name_source(name,source,'yxvsx')

    gm = unified1D.G1d(name+"_yxvsx_", source+"_yxvsx_")
    vcs.elements["yxvsx"][name]=gm
    return gm
createyxvsx.__doc__ = createyxvsx.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, yxvsx_output) 

def getyxvsx(GYx_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: getyxvsx                     # Construct a new Yxvsx graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
Yxvsx class object from an existing VCS Yxvsx graphics method. If
no Yxvsx name is given, then Yxvsx 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createyxvsx function.)

Example of Use:
a=vcs.init()
vcs.show('yxvsx')                     # Show all the existing Yxvsx graphics methods
yxx=vcs.getyxvsx()                    # yxx instance of 'default' Yxvsx graphics
                                    #       method
yxx2=vcs.getyxvsx('quick')            # yxx2 instance of existing 'quick' Yxvsx
                                    #       graphics method
####################################################################################################################
###########################################                          ###############################################
########################################## End getyxvsx Description ################################################
#########################################                          #################################################
####################################################################################################################

"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(GYx_name_src,str):
       raise vcsError, 'The argument must be a string.'
    if GYx_name_src[-7:] == "_yxvsx_":
      GYx_name_src=GYx_name_src[:-7]
    if not GYx_name_src in vcs.elements["yxvsx"]:
      raise ValueError,"The Yxvsx '%s' graphics method does not exists" % GYx_name_src
    return vcs.elements["yxvsx"][GYx_name_src]
getyxvsx.__doc__ = getyxvsx.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, get_GM_input, yxvsx_output) 

def createxvsy( name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createxvsy                      # Construct a new XvsY graphics method

Description of Function:
Create a new XvsY graphics method given the the name and the existing
XvsY graphics method to copy the attributes from. If no existing
XvsY graphics method name is given, then the default XvsY graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
a=vcs.init()
vcs.show('xvsy')
xy=vcs.createxvsy('example1',)
vcs.show('xvsy')
xy=vcs.createxvsy('example2','quick')
vcs.show('xvsy')

######################################################################################################################
###########################################                            ###############################################
########################################## End createxvsy Description ################################################
#########################################                            #################################################
######################################################################################################################

"""

    warnings.warn("the createxvsy method is now obsolete, 1D graphics method have been unified,to avoid your code breaking in the future please change it to use: createoneD")
    if source[-6:]=="_xvsy_":
      source = source[:-6]
    name,source = check_name_source(name,source,'xvsy')
    gm = unified1D.G1d(name+"_xvsy_", source+"_xvsy_")
    vcs.elements["xvsy"][name]=gm
    return gm
createxvsy.__doc__ = createxvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, create_GM_input, xvsy_output) 

def getxvsy(GXY_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: getxvsy                   # Construct a new XvsY graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
XvsY class object from an existing VCS XvsY graphics method. If
no XvsY name is given, then XvsY 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createxvsy function.)

Example of Use:
a=vcs.init()
vcs.show('xvsy')                      # Show all the existing XvsY graphics methods
xy=vcs.getxvsy()                      # xy instance of 'default' XvsY graphics
                                    #       method
xy2=vcs.getxvsy('quick')              # xy2 instance of existing 'quick' XvsY
                                    #       graphics method

###################################################################################################################
###########################################                         ###############################################
########################################## End getxvsy Description ################################################
#########################################                         #################################################
###################################################################################################################

"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(GXY_name_src,str):
       raise vcsError, 'The argument must be a string.'
    if GXY_name_src[-6:] == "_xvsy_":
      GXY_name_src=GXY_name_src[:-6]
    if not GXY_name_src in vcs.elements["xvsy"]:
      raise ValueError,"The xvsy '%s' graphics method does not exists" % GXY_name_src

    return vcs.elements["xvsy"][GXY_name_src]
getxvsy.__doc__ = getxvsy.__doc__ % (plot_keywords_doc, graphics_method_core, axesconvert, get_GM_input, xvsy_output) 

def createvector( name=None, source='default'):
    """
Function: createvector                # Construct a new vector graphics method

Description of Function:
Create a new vector graphics method given the the name and the existing
vector graphics method to copy the attributes from. If no existing
vector graphics method name is given, then the default vector graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
a=vcs.init()
vcs.show('vector')
vec=vcs.createvector('example1',)
vcs.show('vector')
vec=vcs.createvector('example2','quick')
vcs.show('vector')
"""
    name,source = check_name_source(name,source,'vector')
    return vector.Gv(name, source)

def getvector(Gv_name_src='default'):
    """
Function: getvector                   # Construct a new vector graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
vector class object from an existing VCS vector graphics method. If
no vector name is given, then vector 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createvector function.)

Example of Use:
a=vcs.init()
vcs.show('vector')                   # Show all the existing vector graphics methods
vec=vcs.getvector()                  # vec instance of 'default' vector graphics
                                    #       method
vec2=vcs.getvector('quick')          # vec2 instance of existing 'quick' vector
                                    #       graphics method
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gv_name_src,str):
       raise vcsError, 'The argument must be a string.'
    if not Gv_name_src in vcs.elements["vector"]:
      raise ValueError, "The vector '%s' does not exist" % Gv_name_src
    return vcs.elements["vector"][Gv_name_src]

def createscatter( name=None, source='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: createscatter                # Construct a new scatter graphics method

Description of Function:
Create a new scatter graphics method given the the name and the existing
scatter graphics method to copy the attributes from. If no existing
scatter graphics method name is given, then the default scatter graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
a=vcs.init()
vcs.show('scatter')
sct=vcs.createscatter('example1',)
vcs.show('scatter')
sct=vcs.createscatter('example2','quick')
vcs.show('scatter')

#########################################################################################################################
###########################################                               ###############################################
########################################## End createscatter Description ################################################
#########################################                               #################################################
#########################################################################################################################

"""
    warnings.warn("the createscatter method is now obsolete, 1D graphics method have been unified,to avoid your code breaking in the future please change it to use: createoneD")
    if source[-9:] == "_scatter_":
      source=source[:-9]
    name,source = check_name_source(name,source,'scatter')

    gm = unified1D.G1d(name+"_scatter_", source+"_scatter_")
    vcs.elements["scatter"][name]=gm
    return gm
createscatter.__doc__ = createscatter.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, create_GM_input, scatter_output)

def getscatter(GSp_name_src='default'):
    """
Options:::
%s
%s
%s
:::
Input:::
%s
:::
Output:::
%s
:::

Function: getscatter                   # Construct a new scatter graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
scatter class object from an existing VCS scatter graphics method. If
no scatter name is given, then scatter 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a
different name can be modified. (See the createscatter function.)

Example of Use:
a=vcs.init()
vcs.show('scatter')                   # Show all the existing scatter graphics methods
sct=vcs.getscatter()                  # sct instance of 'default' scatter graphics
                                    #       method
sct2=vcs.getscatter('quick')          # sct2 instance of existing 'quick' scatter
                                    #       graphics method

######################################################################################################################
###########################################                            ###############################################
########################################## End getscatter Description ################################################
#########################################                            #################################################
######################################################################################################################

"""
    # Check to make sure the argument passed in is a STRING
    if not isinstance(GSp_name_src,str):
       raise vcsError, 'The argument must be a string.'
    if GSp_name_src[-9:] == "_scatter_":
      GSp_name_src=GSp_name_src[:-9]

    if not GSp_name_src in vcs.elements["scatter"]:
      raise ValueError,"The scatter '%s' graphics method does not exists" % GSp_name_src
    return vcs.elements["scatter"][GSp_name_src]
getscatter.__doc__ = getscatter.__doc__ % (plot_keywords_doc,graphics_method_core,axesconvert, get_GM_input, scatter_output)

def createline(name=None, source='default', ltype=None, 
             width=None, color=None, priority=1,
             viewport=None, worldcoordinate=None,
             x=None, y=None, projection='default'):
    """
Function: createline                       # Construct a new line secondary method

Description of Function:
Create a new line secondary method given the the name and the existing
line secondary method to copy the attributes from. If no existing line
secondary method name is given, then the default line secondary method
will be used as the secondary method to which the attributes will be
copied from.

If the name provided already exists, then a error will be returned. 
Secondary method names must be unique.

Example of Use:
a=vcs.init()
vcs.show('line')
ln=vcs.createline('example1',)
vcs.show('line')
ln=vcs.createline('example2','black')
vcs.show('line')
ln2=vcs.createline(name='new', name_src='red',ltype='dash', width=2,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of line object 'red'
vcs.line(ln2)                      # Plot using specified line object
"""

    name,source = check_name_source(name,source,'line')

    ln = line.Tl(name, source)
    if (ltype is not None):
        ln.type = ltype
    if (width is not None):
        ln.width = width
    if (color is not None):
        ln.color = color
    if (priority is not None):
        ln.priority = priority
    if (viewport is not None):
        ln.viewport = viewport
    if (worldcoordinate is not None):
        ln.worldcoordinate = worldcoordinate
    if (x is not None):
        ln.x = x
    if (y is not None):
        ln.y = y
    ln.projection=projection
    return ln

def getline(name='default', ltype=None, width=None, color=None,
             priority=None, viewport=None,
             worldcoordinate=None,
             x=None, y=None):
    """
Function: getline        # Construct a new line secondary method

Description of Function:
VCS contains a list of secondary methods. This function will create a
line class object from an existing VCS line secondary method. If
no line name is given, then line 'default' will be used.

Note, VCS does not allow the modification of `default' attribute sets.
However, a `default' attribute set that has been copied under a
different name can be modified. (See the createline function.)

Example of Use:
a=vcs.init()
vcs.show('line')                   # Show all the existing line secondary methods
ln=vcs.getline()                   # ln instance of 'default' line secondary
                                 #       method
ln2=vcs.getline('quick')           # ln2 instance of existing 'quick' line
                                 #       secondary method
ln3=vcs.getline(name='red', ltype='dash', width=2,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of line object 'red'
vcs.line(ln3)                      # Plot using specified line object
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name,str):
       raise vcsError, 'The argument must be a string.'

    if not name in vcs.elements["line"]:
      raise ValueError,"The line '%s' does not exists" % name
    ln = vcs.elements["line"][name]
    if ltype is not None and ln.name!='default':
        ln.type=ltype
    if width is not None and ln.name!='default':
        ln.width = width
    if color is not None and ln.name!='default':
        ln.color=color
    if priority is not None and ln.name!='default':
        ln.priority=priority
    if viewport is not None and ln.name!='default':
        ln.viewport=viewport
    if worldcoordinate is not None and ln.name!='default':
        ln.worldcooridnate = worldcoordinate
    if viewport is not None and ln.name!='default':
        ln.viewport = viewport
    if x is not None and ln.name!='default':
        ln.x = x
    if y is not None and ln.name!='default':
        ln.y = y
    return ln

def createmarker(name=None, source='default', mtype=None,
             size=None, color=None,priority=1,
             viewport=None, worldcoordinate=None,
             x=None, y=None,projection=None):
    """
Function: createmarker                   # Construct a new marker secondary method

Description of Function:
Create a new marker secondary method given the the name and the existing
marker secondary method to copy the attributes from. If no existing marker
secondary method name is given, then the default marker secondary method
will be used as the secondary method to which the attributes will be
copied from.

If the name provided already exists, then a error will be returned.
Secondary method names must be unique.

Example of Use:
a=vcs.init()
vcs.show('marker')
mrk=vcs.createmarker('example1',)
vcs.show('marker')
mrk=vcs.createmarker('example2','black')
vcs.show('boxfill')
mrk2=vcs.createmarker(name='new', name_src='red',mtype='dash', size=2,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of marker object 'red'
vcs.marker(mrk2)                      # Plot using specified marker object
"""

    name,source = check_name_source(name,source,'marker')

    mrk = marker.Tm(name, source)
    if (mtype is not None):
        mrk.type = mtype
    if (size is not None):
        mrk.size = size 
    if (color is not None):
        mrk.color = color
    if (priority is not None):
        mrk.priority = priority
    if (viewport is not None):
        mrk.viewport = viewport
    if (worldcoordinate is not None):
        mrk.worldcoordinate = worldcoordinate
    if (x is not None):
        mrk.x = x
    if (y is not None):
        mrk.y = y
    if (projection is not None):
        mrk.projection=projection
    return mrk

def getmarker(name='default', mtype=None, size=None, color=None,
             priority=None, viewport=None,
             worldcoordinate=None,
             x=None, y=None):
    """
Function: getmarker                      # Construct a new marker secondary method

Description of Function:
VCS contains a list of secondary methods. This function will create a
marker class object from an existing VCS marker secondary method. If
no marker name is given, then marker 'default' will be used.

Note, VCS does not allow the modification of `default' attribute sets.
However, a `default' attribute set that has been copied under a
different name can be modified. (See the createmarker function.)

Example of Use:
a=vcs.init()
vcs.show('marker')                    # Show all the existing marker secondary methods
mrk=vcs.getmarker()                   # mrk instance of 'default' marker secondary
                                    #       method
mrk2=vcs.getmarker('quick')           # mrk2 instance of existing 'quick' marker
                                    #       secondary method
mrk3=vcs.getmarker(name='red', mtype='dash', size=2,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of marker object 'red'
vcs.marker(mrk3)                      # Plot using specified marker object
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name,str):
       raise vcsError, 'The argument must be a string.'

    if not name in vcs.elements["marker"]:
      raise ValueError,"The marker object '%s' does not exists"
    mrk = vcs.elements["marker"][name]
    if (mtype is not None) and (mrk.name != "default"):
        mrk.type = mtype
    if (size is not None) and (mrk.name != "default"):
        mrk.size = size
    if (color is not None) and (mrk.name != "default"):
        mrk.color = color
    if (priority is not None) and (mrk.name != "default"):
        mrk.priority = priority
    if (viewport is not None) and (mrk.name != "default"):
        mrk.viewport = viewport
    if (worldcoordinate is not None) and (mrk.name != "default"):
        mrk.worldcoordinate = worldcoordinate
    if (x is not None) and (mrk.name != "default"):
        mrk.x = x
    if (y is not None) and (mrk.name != "default"):
        mrk.y = y
    return mrk

def createfillarea(name=None, source='default', style=None,
             index=None, color=None, priority=1,
             viewport=None, worldcoordinate=None,
             x=None, y=None):
    """
Function: createfillarea     # Construct a new fillarea secondary method

Description of Function:
Create a new fillarea secondary method given the the name and the existing
fillarea secondary method to copy the attributes from. If no existing fillarea
secondary method name is given, then the default fillarea secondary method
will be used as the secondary method to which the attributes will be
copied from.

If the name provided already exists, then a error will be returned.
Secondary method names must be unique.

Example of Use:
vcs.show('fillarea')
fa=vcs.createfillarea('example1',)
vcs.show('fillarea')
fa=vcs.createfillarea('example2','black')
vcs.show('fillarea')
fa2=vcs.createmarker(name='new', name_src='red',style=1, index=1,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of fill area object 'red'
vcs.fillarea(fa2)                      # Plot using specified fill area object
"""

    name,source = check_name_source(name,source,'fillarea')

    fa = fillarea.Tf(name, source)
    if (style is not None):
        fa.style = style
    if (index is not None):
        fa.index = index
    if (color is not None):
        fa.color = color
    if (priority is not None):
        fa.priority = priority
    if (viewport is not None):
        fa.viewport = viewport
    if (worldcoordinate is not None):
        fa.worldcoordinate = worldcoordinate
    if (x is not None):
        fa.x = x
    if (y is not None):
        fa.y = y
    return fa


def getfillarea(name='default', style=None,
             index=None, color=None,
             priority=None, viewport=None,
             worldcoordinate=None,
             x=None, y=None):
    """
Function: getfillarea              # Construct a new fillarea secondary method

Description of Function:
VCS contains a list of secondary methods. This function will create a
fillarea class object from an existing VCS fillarea secondary method. If
no fillarea name is given, then fillarea 'default' will be used.

Note, VCS does not allow the modification of `default' attribute sets.
However, a `default' attribute set that has been copied under a
different name can be modified. (See the createfillarea function.)

Example of Use:
vcs.show('fillarea')                 # Show all the existing fillarea secondary methods
fa=vcs.getfillarea()                 # fa instance of 'default' fillarea secondary
                                   #       method
fa2=vcs.getfillarea('quick')         # fa2 instance of existing 'quick' fillarea
                                    #       secondary method
fa3=vcs.createmarker(name='new', name='red',style=1, index=1,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of fill area object 'red'
vcs.fillarea(fa3)                      # Plot using specified fill area object
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name,str):
       raise vcsError, 'The argument must be a string.'
    if not name in vcs.elements["fillarea"].keys():
        raise vcsError,"Fillarea '%s' doe not exists" % (name)

    fa = vcs.elements["fillarea"][name]
    if (style is not None) and (fvcs.name != "default"):
        fvcs.style = style
    if (index is not None) and (fvcs.name != "default"):
        fvcs.index = index
    if (color is not None) and (fvcs.name != "default"):
        fvcs.color = color
    if (priority is not None) and (fvcs.name != "default"):
        fvcs.priority = priority
    if (viewport is not None) and (fvcs.name != "default"):
        fvcs.viewport = viewport
    if (worldcoordinate is not None) and (fvcs.name != "default"):
        fvcs.worldcoordinate = worldcoordinate
    if (x is not None) and (fvcs.name != "default"):
        fvcs.x = x
    if (y is not None) and (fvcs.name != "default"):
        fvcs.y = y
    return fa

def createtexttable(name=None, source='default', font=None,
             spacing=None, expansion=None, color=None, priority=1,
             viewport=None, worldcoordinate=None,
             x=None, y=None):
    """
Function: createtexttable            # Construct a new texttable secondary method

Description of Function:
Create a new texttable secondary method given the the name and the existing
texttable secondary method to copy the attributes from. If no existing texttable
secondary method name is given, then the default texttable secondary method
will be used as the secondary method to which the attributes will be
copied from.

If the name provided already exists, then a error will be returned.
Secondary method names must be unique.

Example of Use:
a=vcs.init()
vcs.show('texttable')
tt=vcs.createtexttable('example1',)
vcs.show('texttable')
tt=vcs.createtexttable('example2','black')
vcs.show('texttable')
tt=vcs.createtexttable(name='new',name_src='red',font=1,spacing=1,expansion=1,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of texttable object 'new'
vcs.texttable(tt)                      # Plot using specified texttable object
"""

    name,source = check_name_source(name,source,'texttable')

    tt = texttable.Tt(name, source)
    try:
       if (font is not None):
          tt.font = font
       if (spacing is not None):
          tt.spacing = spacing
       if (expansion is not None):
          tt.expansion = expansion
       if (color is not None):
          tt.color = color
       if (priority is not None):
          tt.priority = priority
       if (viewport is not None):
             tt.viewport = viewport
       if (worldcoordinate is not None):
             tt.worldcoordinate = worldcoordinate
       if (x is not None):
          tt.x = x
       if (y is not None):
          tt.y = y
       return tt
    except:
       pass

def gettexttable(name='default', font=None,
             spacing=None, expansion=None, color=None,
             priority=None, viewport=None,
             worldcoordinate=None,
             x=None, y=None):
    """
Function: gettexttable           # Construct a new texttable secondary method

Description of Function:
VCS contains a list of secondary methods. This function will create a
texttable class object from an existing VCS texttable secondary method. If
no texttable name is given, then texttable 'default' will be used.

Note, VCS does not allow the modification of `default' attribute sets.
However, a `default' attribute set that has been copied under a
different name can be modified. (See the createtexttable function.)

Example of Use:
a=vcs.init()
vcs.show('texttable')              # Show all the existing texttable secondary methods
tt=vcs.gettexttable()              # tt instance of 'default' texttable secondary
                                 #       method
tt2=vcs.gettexttable('quick')      # tt2 instance of existing 'quick' texttable
                                 #       secondary method
tt3=vcs.gettexttable(name='red', font=1, spacing=1,expansion=1,
              color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
              worldcoordinate=[0,100, 0,50]
              x=[0,20,40,60,80,100],
              y=[0,10,20,30,40,50] )      # Create instance of texttable object 'red'
vcs.texttable(tt3)                      # Plot using specified texttable object
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name,str):
       raise vcsError, 'The argument must be a string.'

    if not name in vcs.elements["texttable"]:
      raise ValueError,"The texttable '%s' does not exists" % name
    return vcs.elements["texttable"][name]

def createtextorientation(name=None, source='default'):
    """
Function: createtextorientation   # Construct a new textorientation secondary method

Description of Function:
Create a new textorientation secondary method given the the name and 
the existing textorientation secondary method to copy the attributes
from. If no existing textorientation secondary method name is given,
then the default textorientation secondary method will be used as the
secondary method to which the attributes will be copied from.

If the name provided already exists, then a error will be returned.
Secondary method names must be unique.

Example of Use:
vcs.show('textorientation')
to=vcs.createtextorientation('example1',)
vcs.show('textorientation')
to=vcs.createtextorientation('example2','black')
vcs.show('textorientation')
"""

    name,source = check_name_source(name,source,'textorientation')

    return textorientation.To(name, source)

def gettextorientation(To_name_src='default'):
    """
Function: gettextorientation       # Construct a new textorientation secondary method

Description of Function:
VCS contains a list of secondary methods. This function will create
a textorientation class object from an existing VCS textorientation
secondary method. If no textorientation name is given, then 
textorientation 'default' will be used.

Note, VCS does not allow the modification of `default' attribute sets.
However, a `default' attribute set that has been copied under a
different name can be modified. (See the createtextorientation function.)

Example of Use:
a=vcs.init()
vcs.show('textorientation')    # Show all the existing textorientation secondary methods
to=vcs.gettextorientation()    # to instance of 'default' textorientation secondary
                             #       method
to2=vcs.gettextorientation('quick')  # to2 instance of existing 'quick' textorientation
                                   #       secondary method
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(To_name_src,str):
       raise vcsError, 'The argument must be a string.'

    if not To_name_src in vcs.elements["textorientation"]:
      raise ValueError,"The textorientation '%s' does not exists" % To_name_src
    return vcs.elements["textorientation"][To_name_src]

def createtextcombined(Tt_name=None, Tt_source='default', To_name=None, To_source='default', font=None, spacing=None, expansion=None, color=None, priority=1, viewport=None, worldcoordinate=None, x=None, y=None, height=None, angle=None, path=None, halign=None, valign=None, projection=None):
    """
Function: createtext or createtextcombined  # Construct a new text combined secondary method

Description of Function:
Create a new textcombined secondary method given the the names and 
the existing texttable and textorientation secondary methods to copy
the attributes from. If no existing texttable and textorientation
secondary method names are given, then the default texttable and
textorientation secondary methods will be used as the secondary method
to which the attributes will be copied from.

If the name provided already exists, then a error will be returned.
Secondary method names must be unique.

Example of Use:
vcs.show('texttable')
vcs.show('textorientation')
tc=vcs.createtextcombined('example1','std','example1','7left')
vcs.show('texttable')
vcs.show('textorientation')
"""
    ## Check if to is defined
    if To_name is None:
        To_name=Tt_name
    Tt_name,Tt_source = check_name_source(Tt_name,Tt_source,'texttable')
    To_name,To_source = check_name_source(To_name,To_source,'textorientation')

    tc = textcombined.Tc(Tt_name, Tt_source, To_name, To_source)
    if (font is not None):
        tc.font = font
    if (spacing is not None):
        tc.spacing = spacing
    if (expansion is not None):
        tc.expansion = expansion
    if (color is not None):
        tc.color = color
    if (priority is not None):
        tc.priority = priority
    if (viewport is not None):
        tc.viewport = viewport
    if (worldcoordinate is not None):
        tc.worldcoordinate = worldcoordinate
    if (x is not None):
        tc.x = x
    if (y is not None):
        tc.y = y
    if (height is not None):
        tc.height = height
    if (angle is not None):
        tc.angle = angle
    if (path is not None):
        tc.path = path
    if (halign is not None):
        tc.halign = halign
    if (valign is not None):
        tc.valign = valign
    if (projection is not None):
        tc.projection = projection
    return tc
#
# Set alias for the secondary createtextcombined.
createtext = createtextcombined

def gettextcombined(Tt_name_src='default', To_name_src='default', string=None, font=None, spacing=None, expansion=None, color=None, priority=None, viewport=None, worldcoordinate=None , x=None, y=None, height=None, angle=None, path=None, halign=None, valign=None):
    """
Function: gettext or gettextcombined   # Construct a new textcombined secondary method

Description of Function:
VCS contains a list of secondary methods. This function will create
a textcombined class object from an existing VCS texttable secondary
method and an existing VCS textorientation secondary method. If no 
texttable or textorientation names are given, then the 'default' names
will be used in both cases.

Note, VCS does not allow the modification of `default' attribute sets.
However, a `default' attribute set that has been copied under a
different name can be modified. (See the createtextcombined function.)

Example of Use:
a=vcs.init()
vcs.show('texttable')                  # Show all the existing texttable secondary methods
vcs.show('textorientation')            # Show all the existing textorientation secondary methods
tc=vcs.gettextcombined()               # Use 'default' for texttable and textorientation
tc2=vcs.gettextcombined('std','7left') # Use 'std' texttable and '7left' textorientation
if istextcombined(tc):               # Check to see if tc is a textcombined
   tc.list()                         # Print out all its attriubtes
"""


    # Check to make sure the arguments passed in are a STRINGS
    if not isinstance(Tt_name_src,str):
        raise vcsError, 'The first argument must be a string.'
    if not isinstance(To_name_src,str):
        raise vcsError, 'The second argument must be a string.'
    
    tc = vcs.elements["textcombined"].get("%s:::%s" % (Tt_name_src,To_name_src),None)
    if tc is None:
      raise Exception,"No usch text combined: %s:::%s" % (Tt_name_src,To_name_src)


    if (string is not None) and (tc.Tt_name != "default"):
        tc.string = string
    if (font is not None) and (tc.Tt_name != "default"):
        tc.font = font
    if (spacing is not None) and (tc.Tt_name != "default"):
        tc.spacing = spacing
    if (expansion is not None) and (tc.Tt_name != "default"):
        tc.expansion = expansion
    if (color is not None) and (tc.Tt_name != "default"):
        tc.color = color
    if (priority is not None) and (tc.Tt_name != "default"):
        tc.priority = priority
    if (viewport is not None) and (tc.Tt_name != "default"):
        tc.viewport = viewport
    if (worldcoordinate is not None) and (tc.Tt_name != "default"):
        tc.worldcoordinate = worldcoordinate
    if (x is not None) and (tc.To_name != "default"):
        tc.x = x
    if (y is not None) and (tc.To_name != "default"):
        tc.y = y
    if (height is not None) and (tc.To_name != "default"):
        tc.height = height
    if (angle is not None) and (tc.To_name != "default"):
        tc.angle = angle
    if (path is not None) and (tc.To_name != "default"):
        tc.path = path
    if (halign is not None) and (tc.To_name != "default"):
        tc.halign = halign
    if (valign is not None) and (tc.To_name != "default"):
        tc.valign = valign
    return tc
#
# Set alias for the secondary gettextcombined.
gettext = gettextcombined

def getdv3d(Gfdv3d_name_src='default'):
    """
Function: getdv3d                        # Construct a new dv3d graphics method

Description of Function:
VCS contains a list of graphics methods. This function will create a
dv3d class object from an existing VCS dv3d graphics method. If
no dv3d name is given, then dv3d 'default' will be used.

Note, VCS does not allow the modification of `default' attribute
sets. However, a `default' attribute set that has been copied under a 
different name can be modified. (See the createdv3d function.)

Example of Use:
a.show('dv3d')                   # Show all the existing dv3d graphics methods
plot=vcs.getdv3d()                  # plot instance of 'default' dv3d graphics
                                    # method
plot2=vcs.getdv3d('quick')          # plot2 instance of existing 'quick' dv3d
                                    #         graphics method
"""

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfdv3d_name_src,str):
        raise vcsError, 'The argument must be a string.'

    if not Gfdv3d_name_src in vcs.elements["dv3d"]:
        raise ValueError,"dv3d '%s' does not exists" % Gfdv3d_name_src

    return vcs.elements["dv3d"][Gfdv3d_name_src]


def createdv3d(name=None, source='default'):
    """
Function: createdv3d                # Construct a new dv3d graphics method

Description of Function:
Create a new dv3d graphics method given the the name and the existing
dv3d graphics method to copy the attributes from. If no existing
dv3d graphics method name is given, then the default dv3d graphics
method will be used as the graphics method to which the attributes will
be copied from.

If the name provided already exists, then a error will be returned. Graphics
method names must be unique.

Example of Use:
a=vcs.init()
a.show('dv3d')
plot=a.createdv3d()
"""
    name,source = check_name_source(name,source,'dv3d')
    return dv3d.Gfdv3d(name, source)
