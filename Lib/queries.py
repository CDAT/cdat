#!/usr/bin/env python
#
# The VCS query controls -  query module
#
#################################################################################
#                                                                               #
# Module:       query module                                                    #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Functions which get information about vcs graphics objects      #
#               such as graphics methods and templates.                         #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################

"""
###########################################################################################
#                                                                                         #
# Functions which get information about vcs graphics objects such as graphics methods.    #
#                                                                                         #
###########################################################################################
"""
import boxfill, isofill, isoline, outfill, outline, taylor, meshfill
import unified1D, vector, continents, line, marker, fillarea
import texttable, textorientation, textcombined, template
import displayplot, projection
import vcs
import vcsaddons

from error import vcsError

def isgraphicsmethod(gobj):
        """
 Function: isgraphicsmethod

 Description of Function:
    Indicates if the entered argument is one of the following graphics
    methods: boxfill, isofill, isoline, outfill, outline, continents,
    scatter, vector, xvsy, xyvsy, yxvsx.

    Returns a 1, which indicates true, if the argment is one of the above.
    Otherwise, it will return a 0, indicating false.

 Example of Use:
    a=vcs.init()
    box=a.getboxfill('quick')  # To Modify an existing boxfill use:
    ...

    if vcs.isgraphicsmethod(box):
       box.list()
"""
        if (isinstance(gobj,boxfill.Gfb)):
            return 1
        elif (isinstance(gobj,isofill.Gfi)):
            return 1
        elif (isinstance(gobj,isoline.Gi)):
            return 1
        elif (isinstance(gobj,outfill.Gfo)):
            return 1
        elif (isinstance(gobj,outline.Go)):
            return 1
        elif (isinstance(gobj,continents.Gcon)):
            return 1
        elif (isinstance(gobj,vector.Gv)):
            return 1
        elif (isinstance(gobj,unified1D.G1d)):
            return 1
        elif (isinstance(gobj,taylor.Gtd)):
            return 1
        elif (isinstance(gobj,meshfill.Gfm)):
            return 1
	elif isinstance(gobj,vcsaddons.core.VCSaddon):
	    return 1
        else:
           return 0

def graphicsmethodlist():
        """
 Function: graphicsmethodlist

 Description of Function:
        Will return a list of available grapics methods (i.e., boxfill, isofill, isoline, outf
ill,
        outline, continents, scatter, vector, xvsy, xyvsy, yxvsx, taylordiagram ).

 Example of Use:
    a=vcs.init()
    gm_list=a.graphicsmethodlist()  # Return graphics method list
"""
        return [ 'boxfill',  'isofill',  'isoline',  'meshfill', 'outfill', 'outline', 'continents', 'scatter', 'vector', 'xvsy', 'xyvsy', 'yxvsx', 'taylordiagram', 'oneD' ]

def graphicsmethodtype(gobj):
        """
 Function: graphicsmethodtype

 Description of Function:
        Will return the grapics method's type: boxfill, isofill, isoline, outfill,
        outline, continents, scatter, vector, xvsy, xyvsy, or yxvsx, taylordiagram.

        Returns a None if the object is not a graphics method.

 Example of Use:
    a=vcs.init()
    box=a.getboxfill('quick')  # Get an existing boxfill graphics method in VCS
    iso=a.getisofill('quick')  # Get an existing isofill graphics method in VCS
    ln=a.getline('quick')      # Get an existing line element in VCS
    ...

    print vcs.graphicsmethodtype(box)         # Will print 'boxfill'
    print vcs.graphicsmethodtype(iso)         # Will print 'isofill'
    print vcs.graphicsmethodtype(ln)          # Will print None, because ln is not a
                                              #         graphics method
"""
        if (isinstance(gobj,boxfill.Gfb)):
            return 'boxfill'
        elif (isinstance(gobj,isofill.Gfi)):
            return 'isofill'
        elif (isinstance(gobj,isoline.Gi)):
            return 'isoline'
        elif (isinstance(gobj,outfill.Gfo)):
            return 'outfill'
        elif (isinstance(gobj,outline.Go)):
            return 'outline'
        elif (isinstance(gobj,continents.Gcon)):
            return 'continents'
        elif (isinstance(gobj,vector.Gv)):
            return 'vector'
        elif (isinstance(gobj,unified1D.G1d)):
          nm = gobj.name.split("_")[-1]
          if nm in ["yxvsx","xvsy","xyvsy","scatter"]:
            nm = "_".join(gobj.name.split("_")[:-1])
            if nm in vcs.elements["scatter"]: 
                return 'scatter'
            elif nm in vcs.elements["xvsy"]: 
                return 'xvsy'
            elif nm in vcs.elements["xyvsy"]: 
                return 'xyvsy'
            elif nm in vcs.elements["yxvsx"]: 
                return 'yxvsx'
          else:
            return "oneD"
        elif (isinstance(gobj,taylor.Gtd)):
            return 'taylordiagram'
        elif (isinstance(gobj,meshfill.Gfm)):
            return 'meshfill'
	elif isinstance(gobj,vcsaddons.core.VCSaddon):
	    return gobj
        else:
           raise vcsError, 'The object passed is not a graphics method object.'

#############################################################################
#                                                                           #
# Is this a display plot object in VCS?                                     #
#                                                                           #
#############################################################################

def isplot(pobj):
    """
 Function: isplot

 Description of Function:
    Indicates if the entered argument a display plot. 

    Returns a 1 if the argment true.
    Otherwise, it will return a 0, indicating false.

 Example of Use:
    a=vcs.init()
    ...
    a.show('plot')			# show all the plot objects on the VCS Canvas
    p1=a.getplot('dpy_plot_1')  	# Get an existing plot object named 'dpy_plot_1'
    p2=a.plot(s)			# Create a new display plot object
    ...

    if a.isplot(p1):
       p1.list()               		# If it is a plot then list its members
"""
    if (isinstance(pobj,displayplot.Dp)):
        return 1
    else:
        return 0

#############################################################################
#                                                                           #
# Is this a secondary colormap in VCS?                                      #
#                                                                           #
#############################################################################
def iscolormap(obj):
    """
 Function: iscolormap

 Description of Function:
    Check to see if this object is a VCS secondary colormap.

 Example of Use:
    a=vcs.init()
    ln=a.getcolormap("quick")  # To Modify an existing colormap object
    ...

    if a.iscolormap(ln):
       ln.list()

"""
    if (isinstance(obj,vcs.colormap.Cp)):
        return 1
    else:
        return 0

def istemplate(gobj):
    """
 Function: istemplate

 Description of Function:
    Indicates if the entered argument a template.

    Returns a 1 if the argment true.
    Otherwise, it will return a 0, indicating false.

 Example of Use:
    a=vcs.init()
    templt=a.gettemplate('quick')  # Modify an existing template named 'quick'
    ...

    if vcs.istemplate(templt):
       templt.list()               # If it is a template then list its members
"""
    if (isinstance(gobj, template.P)):
        return 1
    else:
        return 0

def issecondaryobject(sobj):
    """
 Function: issecondaryobject

 Description of Function:

In addition, detailed specification of the primary elements' (or
primary class elements'), attributes is provided by eight secondary
elements or (secondary class elements): 

 1.) colormap: specification of combinations of 256 available
               colors 
 2.) fill area: style, style index, and color index 
 3.) format: specifications for converting numbers to display
               strings 
 4.) line: line type, width, and color index 
 5.) list: a sequence of pairs of numerical and character values 
 6.) marker: marker type, size, and color index 
 7.) text table: text font type, character spacing, expansion, and 
               color index 
 8.) text orientation: character height, angle, path, and
               horizontal/vertical alignment 
 9.) projections


 Example of Use:
a=vcs.init()
line=a.getline('red')  # To Modify an existing line object
...

if queries.issecondaryobject(line):
   box.list()
"""
    if (isinstance(sobj,line.Tl)):
        return 1
    elif (isinstance(sobj,marker.Tm)):
        return 1
    elif (isinstance(sobj,fillarea.Tf)):
        return 1
    elif (isinstance(sobj,texttable.Tt)):
        return 1
    elif (isinstance(sobj,textorientation.To)):
        return 1
    elif (isinstance(sobj,textcombined.Tc)):
        return 1
    elif (isinstance(sobj,marker.Tm)):
        return 1
    elif (isinstance(sobj,projection.Proj)):
        return 1
    elif (isinstance(sobj,vcs.colormap.Cp)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary object, projectionmethod in VCS?                      #
#                                                                           #
#############################################################################
def isprojection(obj):
    """
 Function: isprojection

 Description of Function:
Check to see if this object is a VCS secondary projection graphic object .

 Example of Use:
a=vcs.init()
p=a.getprojection("default")  # To Modify an existing taylor object
...

if queries.isprojection(p):
   p.list()

"""
    if (isinstance(obj,projection.Proj)):
        return 1
    else:
       return 0
#############################################################################
#                                                                           #
# Is this a primary taylor diagram graphics method in VCS?                         #
#                                                                           #
#############################################################################
def istaylordiagram(obj):
    """
 Function: istaylordiagram

 Description of Function:
Check to see if this object is a VCS primary taylordiagram graphics method.

 Example of Use:
a=vcs.init()
td=a.gettaylordiagram("default")  # To get an existing taylor object
...

if queries.istaylordiagram(td):
   td.list()

"""
    if (isinstance(obj,taylor.Gtd)):
        return 1
    else:
       return 0
#############################################################################
#                                                                           #
# Is this a primary meshfill graphics method in VCS?                        #
#                                                                           #
#############################################################################
def ismeshfill(obj):
    """
 Function: ismeshfill

 Description of Function:
Check to see if this object is a VCS primary meshfill graphics method.

 Example of Use:
a=vcs.init()
mesh=a.getmeshfill("quick")  # To Modify an existing taylor object
...

if queries.ismeshfill(mesh):
   mesh.list()

"""
    if (isinstance(obj,meshfill.Gfm)):
        return 1
    else:
       return 0
#############################################################################
#                                                                           #
# Is this a primary boxfill graphics method in VCS?                         #
#                                                                           #
#############################################################################
def isboxfill(obj):
    """
 Function: isboxfill

 Description of Function:
Check to see if this object is a VCS primary boxfill graphics method.

 Example of Use:
a=vcs.init()
box=a.getboxfill("quick")  # To Modify an existing boxfill object
...

if queries.isboxfill(box):
   box.list()

"""
    if (isinstance(obj,boxfill.Gfb)):
        return 1
    else:
       return 0


#############################################################################
#                                                                           #
# Is this a primary isofill graphics method in VCS?                         #
#                                                                           #
#############################################################################
def isisofill(obj):
    """
 Function: isisofill

 Description of Function:
Check to see if this object is a VCS primary isofill graphics method.

 Example of Use:
a=vcs.init()
iso=a.getisofill("quick")  # To Modify an existing isofill object
...

if queries.isisofill(iso):
   iso.list()

"""
    if (isinstance(obj,isofill.Gfi)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary isoline graphics method in VCS?                         #
#                                                                           #
#############################################################################
def isisoline(obj):
    """
 Function: isisoline

 Description of Function:
Check to see if this object is a VCS primary isoline graphics method.

 Example of Use:
a=vcs.init()
iso=a.getisoline("quick")  # To Modify an existing isoline object
...

if queries.isisoline(iso):
   iso.list()

"""
    if (isinstance(obj,isoline.Gi)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary outfill graphics method in VCS?                         #
#                                                                           #
#############################################################################
def isoutfill(obj):
    """
 Function: isoutfill

 Description of Function:
Check to see if this object is a VCS primary outfill graphics method.

 Example of Use:
a=vcs.init()
out=a.getoutfill("quick")  # To Modify an existing outfill object
...

if queries.isoutfill(out):
   out.list()

"""
    if (isinstance(obj,outfill.Gfo)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary outline graphics method in VCS?                         #
#                                                                           #
#############################################################################
def isoutline(obj):
    """
 Function: isoutline

 Description of Function:
Check to see if this object is a VCS primary outline graphics method.

 Example of Use:
a=vcs.init()
out=a.getoutline("quick")  # To Modify an existing outline object
...

if queries.isoutline(out):
   out.list()

"""
    if (isinstance(obj,outline.Go)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary scatter graphics method in VCS?                         #
#                                                                           #
#############################################################################
def isscatter(obj):
    """
 Function: isscatter

 Description of Function:
Check to see if this object is a VCS primary scatter graphics method.

 Example of Use:
a=vcs.init()
scr=a.getscatter("quick")  # To Modify an existing scatter object
...

if queries.isscatter(scr):
   scr.list()

"""
    if (isinstance(obj,unified1D.G1d)) and obj.name.split("_")[-1]=="scatter" and "_".join(obj.name.split("_")[:-1]) in vcs.elements["scatter"]:
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary Xyvsy graphics method in VCS?                           #
#                                                                           #
#############################################################################
def isxyvsy(obj):
    """
 Function: isxyvsy

 Description of Function:
Check to see if this object is a VCS primary Xyvsy graphics method.

 Example of Use:
a=vcs.init()
xyy=a.getxyvsy("quick")  # To Modify an existing Xyvsy object
...

if queries.isxyvsy(xyy):
   xyy.list()

"""
    if (isinstance(obj,unified1D.G1d)) and obj.name.split("_")[-1]=="xyvsy" and "_".join(obj.name.split("_")[:-1]) in vcs.elements["xyvsy"]:
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary Yxvsx graphics method in VCS?                           #
#                                                                           #
#############################################################################
def isyxvsx(obj):
    """
 Function: isyxvsx

 Description of Function:
Check to see if this object is a VCS primary yxvsx graphics method.

 Example of Use:
a=vcs.init()
yxx=a.getyxvsx("quick")  # To Modify an existing yxvsx object
...

if queries.isyxvsx(yxx):
   yxx.list()

"""
    if (isinstance(obj,unified1D.G1d)) and obj.name.split("_")[-1]=="yxvsx" and "_".join(obj.name.split("_")[:-1]) in vcs.elements["yxvsx"]:
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary XvsY graphics method in VCS?                            #
#                                                                           #
#############################################################################
def isxvsy(obj):
    """
 Function: isxvsy

 Description of Function:
Check to see if this object is a VCS primary xvsy graphics method.

 Example of Use:
a=vcs.init()
xy=a.getxvsy("quick")  # To Modify an existing xvsy object
...

if queries.isxvsy(xy):
   xy.list()

"""
    if (isinstance(obj,unified1D.G1d)) and obj.name.split("_")[-1]=="xvsy" and "_".join(obj.name.split("_")[:-1]) in vcs.elements["xvsy"]:
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary oneD graphics method in VCS?                            #
#                                                                           #
#############################################################################
def isoneD(obj):
    """
 Function: isxvsy

 Description of Function:
Check to see if this object is a VCS primary xvsy graphics method.

 Example of Use:
a=vcs.init()
xy=a.getxvsy("quick")  # To Modify an existing xvsy object
...

if queries.isxvsy(xy):
   xy.list()

"""
    if (isinstance(obj,unified1D.G1d)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary continents graphics method in VCS?                      #
#                                                                           #
#############################################################################
def iscontinents(obj):
    """
 Function: iscontinents

 Description of Function:
Check to see if this object is a VCS primary continents graphics method.

 Example of Use:
a=vcs.init()
con=a.getcontinents("quick")  # To Modify an existing continents object
...

if queries.iscontinents(con):
   con.list()

"""
    if (isinstance(obj,continents.Gcon)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a primary vector graphics method in VCS?                          #
#                                                                           #
#############################################################################
def isvector(obj):
    """
 Function: isvector

 Description of Function:
Check to see if this object is a VCS primary vector graphics method.

 Example of Use:
a=vcs.init()
vec=a.getvector("quick")  # To Modify an existing vector object
...

if queries.isvector(vec):
   vec.list()

"""
    if (isinstance(obj,vector.Gv)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary line method in VCS?                                   #
#                                                                           #
#############################################################################
def isline(obj):
    """
 Function: isline

 Description of Function:
Check to see if this object is a VCS secondary line.

 Example of Use:
a=vcs.init()
ln=a.getline("red")  # To Modify an existing line object
...

if queries.isline(ln):
   ln.list()

"""
    if (isinstance(obj,line.Tl)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary marker method in VCS?                                 #
#                                                                           #
#############################################################################
def ismarker(obj):
    """
 Function: ismarker

 Description of Function:
Check to see if this object is a VCS secondary marker.

 Example of Use:
a=vcs.init()
mk=a.getmarker("red")  # To Modify an existing marker object
...

if queries.ismarker(mk):
   mk.list()

"""
    if (isinstance(obj,marker.Tm)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary fillarea method in VCS?                               #
#                                                                           #
#############################################################################
def isfillarea(obj):
    """
 Function: isfillarea

 Description of Function:
Check to see if this object is a VCS secondary fillarea.

 Example of Use:
a=vcs.init()
fa=a.getfillarea("def37")  # To Modify an existing fillarea object
...

if queries.isfillarea(fa):
   fa.list()

"""
    if (isinstance(obj,fillarea.Tf)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary text table  method in VCS?                            #
#                                                                           #
#############################################################################
def istexttable(obj):
    """
 Function: istexttable

 Description of Function:
Check to see if this object is a VCS secondary text table.

 Example of Use:
a=vcs.init()
tt=a.gettexttable("std")  # To Modify an existing text table object
...

if queries.istexttable(tt):
   tt.list()

"""
    if (isinstance(obj,texttable.Tt)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary text orientation method in VCS?                       #
#                                                                           #
#############################################################################
def istextorientation(obj):
    """
 Function: istextorientation

 Description of Function:
Check to see if this object is a VCS secondary text orientation.

 Example of Use:
a=vcs.init()
to=a.gettextorientation("7left")  # To Modify an existing text orientation object
...

if queries.istextorientation(to):
   to.list()

"""
    if (isinstance(obj,textorientation.To)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Is this a secondary text combined method in VCS?                          #
#                                                                           #
#############################################################################
def istextcombined(obj):
    """
 Function: istextcombined

 Description of Function:
Check to see if this object is a VCS secondary text combined.

 Example of Use:
a=vcs.init()
tc=a.gettextcombined("std", "7left")  # To Modify existing text table and orientation objects
...

if istextcombined(tc):
   tc.list()
if istexttable(tc):
   tc.list()
if istextorientation(tc):
   tc.list()

"""
    if (isinstance(obj,textcombined.Tc)):
        return 1
    else:
       return 0

#############################################################################
#                                                                           #
# Set an alias for the secondary text combined method in VCS.               #
# This is much easier to type than 'textcombined'.                          #
#                                                                           #
#############################################################################
istext=istextcombined


#################################################################################
#        END OF FILE                                                            #
#################################################################################
