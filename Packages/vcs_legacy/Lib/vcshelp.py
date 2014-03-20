"""
# VCS help module
"""
###############################################################################
#                                                                             #
# Module:       VCS help (Vh) module                                          #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Python command for VCS's help.       			      #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
###############################################################################
#                                                                             #
# Import: VCS Python module.                                                  #
#                                                                             #
###############################################################################
def objecthelp(*arg):
    """ Function:     objecthelp                                         
                                                                               
 Description of Function:                                                      
       Print the documentation of each object in the argument list.
       Prints a blank line if no documentation.
                                                                               
 Example of Use:                                                               
        import vcs_legacy                                                             
       vcs_legacy.objecthelp(object)  #  where: object is the Python object          
"""                                                                           
    for x in arg:
        print getattr(x, "__doc__", "")

###############################################################################
#                                                                             #
# Import: VCS and query module.                                               #
#                                                                             #
###############################################################################
import vcs_legacy, queries

def help(*arg):

        if (len(arg) == 0):
          b = ['fillarea', 'line', 'marker', 'textcombined', 'textorientation',
               'texttable','projection']

          t = ['template']

          p = ['plot', 'boxfill', 'continents', 'isofill', 'isoline','meshfill',
               'outfill', 'outline', 'scatter', 'vector', 'xvsy',
               'xyvsy', 'yxvsx']

          h = ['isboxfill', 'iscolormap', 'iscontinents', 'isfillarea',
               'isgraphicsmethod', 'isisofill', 'isisoline', 'isline', 'ismarker',
               'ismeshfill', 'isoutfill', 'isoutline', 'isplot', 'isprojection',
               'isscatter', 'issecondaryobject',
               'istemplate', 'istextcombined', 'istextorientation', 'istexttable',
               'isvector', 'isxvsy', 'isplot', 'islandscape', 'isportrait']

          c = ['createboxfill', 'createcolormap', 'createcontinents',
               'createfillarea', 'createisofill', 'createisoline',
               'createline', 'createmeshfill', 'createmarker', 'createoutfill',
               'createoutline', 'createprojection', 
               'createscatter', 'createtemplate', 'createtextcombined',
               'createtextorientation', 'createtexttable', 'createvector',
               'createxvsy', 'createxyvsy', 'createyxvsx']

          g = ['getboxfill', 'getcolorcell', 'getcolormap', 'getcolormapname',
               'getcolors', 'getcontinents', 'getcontinentstype', 'getfillarea',
               'getisofill', 'getisoline', 'getline', 'getmeshfill', 'getmarker',
               'getoutfill', 'getoutline', 'getplot', 'getprojection', 'getscatter',
               'gettemplate', 'gettextcombined', 'gettextorientation',
               'gettexttable', 'getvector', 'getxvsy', 'getxyvsy', 'getyxvsx']

          f = ['animate', 'cgm', 'clear', 'close', 'colormapgui',  'eps',
               'flush', 'geometry', 'gif', 'graphicsmethodtype', 'gs',
               'help', 'init', 'landscape', 'listelements', 'mkevenlevels',
               'mklabels', 'mkscale', 'mode', 'objecthelp', 'open', 'orientation',
               'page', 'pause', 'portrait', 'postscript', 'printer', 'pstogif',
               'raster', 'set', 'setcolorcell', 'setcolormap',
               'setcontinentstype', 'show', 'update', 'vcs_legacyError']

          s = ['scriptobject', 'scriptrun', 'scriptstate', 'saveinitialfile']

          r = ['removeobject']

          print 'The Visualization and Control System (VCS) help command\n'
          print '--- VCS Model ---'
          print 'The VCS model is defined by a trio of named attribute sets, designated the "Primary Objects" (also known as "Primary Elements"). These include: the data, which specifies what is to be displayed and are obtained from the "cdms" module; the graphics method, which specifies the display technique; and the picture template, which determines the appearance of each segment of the display.\n'
          print '--- VCS Primary Objects ---'
          print 'Graphics Method:', (p[0]+'object'),(p[1]+'object'),(p[2]+'object'),(p[3]+'object')
          print '                ', (p[4]+'object'),(p[5]+'object'),(p[6]+'object'),(p[7]+'object')
          print '                ', (p[8]+'object'),(p[9]+'object'),(p[10]+'object'),(p[11]+'object')
          print '                ', (p[12]+'object')
          print 'Template:       ', (t[0]+'object')
          print 'Data:            see the cdms, ma, and numpy modules for data extraction and manipulation and additional documentation.'
          print '\n--- VCS Secondary Objects ---'
          print 'Color Map:       Note: Color maps are object, but they are not referenced like other secondary objects.'
          print 'Fill Areas:     ', (b[0]+'object')
          print 'Line:           ', (b[1]+'object')
          print 'Markers:        ', (b[2]+'object')
          print 'Text:           ', (b[3]+'object'),(b[4]+'object'),(b[5]+'object')
          print 'Projection:     ', (b[6]+'object (use projection objet __doc__ string for more info on the types of projection available)')
          print '\n--- Initialize VCS ---'
          print 'Initialize VCS: ', 'init'
          print '\n--- VCS Canvas Functions ---'
          print 'Plotting:      ', p[0],p[1],p[2],p[3],p[4],p[5]
          print '               ', p[6],p[7],p[8],p[9],p[10]
          print '               ', p[11],p[12]
          print 'Querying:      ', h[0],h[1],h[2],h[3],h[4]
          print '               ', h[5],h[6],h[7],h[8],h[9],h[10]
          print '               ', h[11],h[12],h[13],h[14],h[15]
          print '               ', h[16],h[17],h[18],h[19],h[20]
          print '               ', h[21],h[22],h[23],h[24]
          print 'Creating:      ', c[0],c[1],c[2],c[3]
          print '               ', c[4],c[5],c[6],c[7]
          print '               ', c[8],c[9],c[10],c[11]
          print '               ', c[12],c[13],c[14],c[15]
          print '               ', c[16],c[17],c[18],c[19]
          print '               ', c[20]
          print 'Getting:       ', g[0],g[1],g[2],g[3],g[4]
          print '               ', g[5],g[6],g[7],g[8],g[9]
          print '               ', g[10],g[11],g[12],g[13],g[14]
          print '               ', g[15],g[16],g[17],g[18],g[19]
          print '               ', g[20],g[21],g[22],g[23],g[24]
          print '               ', g[25]
          print 'Removing:      ', r[0]
          print 'Scripting:     ', s[0],s[1],s[2],s[3]
          print 'Operating:     ', f[0],f[1],f[2],f[3],f[4],f[5],f[6],f[7]
          print '               ', f[8],f[9],f[10],f[11],f[12],f[13],f[14]
          print '               ', f[15],f[16],f[17],f[18],f[19],f[20],f[21]
          print '               ', f[22],f[23],f[24],f[25],f[26],f[27],f[28],f[29]
          print '               ', f[30],f[31],f[32],f[33],f[34],f[35]

          print '\n--- Examples Using Help ---'
          print "vcs_legacy.help()             - prints VCS Model and Primary and Secondary Object list"
          print "vcs_legacy.help('init')       - prints boxfill description and example of use"
          print "vcs_legacy.help('plot')       - prints boxfill description and example of use"
          print "vcs_legacy.help('lineobject') - prints boxfill description and example of use"
          print "vcs_legacy.help('init','plot) - prints both descriptions and example of use"
        else:
          for i in range(len(arg)):
             # Template Objects
             if (arg[i] == 'templateobject'):
                print vcs_legacy.template.P.__doc__
             # Primary Objects
             elif (arg[i] == 'boxfillobject'):
                print vcs_legacy.boxfill.Gfb.__doc__
             elif (arg[i] == 'isofillobject'):
                print vcs_legacy.isofill.Gfi.__doc__
             elif (arg[i] == 'isolineobject'):
                print vcs_legacy.isoline.Gi.__doc__
             elif (arg[i] == 'continentsobject'):
                print vcs_legacy.continents.Gcon.__doc__
             elif (arg[i] == 'meshfillobject'):
                print vcs_legacy.meshfill.Gfm.__doc__
             elif (arg[i] == 'outfillobject'):
                print vcs_legacy.outfill.Gfo.__doc__
             elif (arg[i] == 'outlineobject'):
                print vcs_legacy.outline.Go.__doc__
             elif (arg[i] == 'scatterobject'):
                print vcs_legacy.scatter.GSp.__doc__
             elif (arg[i] == 'vectorobject'):
                print vcs_legacy.vector.Gv.__doc__
             elif (arg[i] == 'xvsyobject'):
                print vcs_legacy.xvsy.GXY.__doc__
             elif (arg[i] == 'xyvsyobject'):
                print vcs_legacy.xyvsy.GXy.__doc__
             elif (arg[i] == 'yxvsxobject'):
                print vcs_legacy.yxvsx.GYx.__doc__
             # Secondary Objects
             elif (arg[i] == 'lineobject'):
                print vcs_legacy.line.Tl.__doc__
             elif (arg[i] == 'markerobject'):
                print vcs_legacy.marker.Tm.__doc__
             elif (arg[i] == 'fillareaobject'):
                print vcs_legacy.fillarea.Tf.__doc__
             elif (arg[i] == 'texttableobject'):
                print vcs_legacy.texttable.Tt.__doc__
             elif (arg[i] == 'textorientationobject'):
                print vcs_legacy.textorientation.To.__doc__
             elif (arg[i] == 'textcombinedobject'):
                print vcs_legacy.textcombined.Tc.__doc__
             elif (arg[i] == 'projectionobject'):
                print vcs_legacy.projection.Proj.__doc__
             elif (arg[i] == 'init'):
                print vcs_legacy.init.__doc__
             elif (arg[i] == 'plot'):
                print vcs_legacy.Canvas.Canvas.plot.__doc__
             elif (arg[i] == 'boxfill'):
                print vcs_legacy.Canvas.Canvas.boxfill.__doc__
             elif (arg[i] == 'clear'):
                print vcs_legacy.Canvas.Canvas.clear.__doc__
             elif (arg[i] == 'close'):
                print vcs_legacy.Canvas.Canvas.close.__doc__
             elif (arg[i] == 'cgm'):
                print vcs_legacy.Canvas.Canvas.cgm.__doc__
             elif (arg[i] == 'meshfill'):
                print vcs_legacy.Canvas.Canvas.meshfill.__doc__
             elif (arg[i] == 'isofill'):
                print vcs_legacy.Canvas.Canvas.isofill.__doc__
             elif (arg[i] == 'isoline'):
                print vcs_legacy.Canvas.Canvas.isoline.__doc__
             elif (arg[i] == 'continents'):
                print vcs_legacy.Canvas.Canvas.continents.__doc__
             elif (arg[i] == 'outfill'):
                print vcs_legacy.Canvas.Canvas.outfill.__doc__
             elif (arg[i] == 'outline'):
                print vcs_legacy.Canvas.Canvas.outline.__doc__
             elif (arg[i] == 'scatter'):
                print vcs_legacy.Canvas.Canvas.scatter.__doc__
             elif (arg[i] == 'vector'):
                print vcs_legacy.Canvas.Canvas.vector.__doc__
             elif (arg[i] == 'xvsy'):
                print vcs_legacy.Canvas.Canvas.xvsy.__doc__
             elif (arg[i] == 'xyvsy'):
                print vcs_legacy.Canvas.Canvas.xyvsy.__doc__
             elif (arg[i] == 'yxvsx'):
                print vcs_legacy.Canvas.Canvas.yxvsx.__doc__
             elif (arg[i] == 'isboxfill'):
                print queries.isboxfill.__doc__
             elif (arg[i] == 'iscontinents'):
                print queries.iscontinents.__doc__
             elif (arg[i] == 'isfillarea'):
                print queries.isfillarea.__doc__
             elif (arg[i] == 'isgraphicsmethod'):
                print queries.isgraphicsmethod.__doc__
             elif (arg[i] == 'ismeshfill'):
                print queries.ismeshfill.__doc__
             elif (arg[i] == 'isisofill'):
                print queries.isisofill.__doc__
             elif (arg[i] == 'isisoline'):
                print queries.isisoline.__doc__
             elif (arg[i] == 'iscolormap'):
                print queries.iscolormap.__doc__
             elif (arg[i] == 'isplot'):
                print queries.isplot.__doc__
             elif (arg[i] == 'isportrait'):
                print vcs_legacy.Canvas.Canvas.isportrait.__doc__
             elif (arg[i] == 'islandscape'):
                print vcs_legacy.Canvas.Canvas.islandscape.__doc__
             elif (arg[i] == 'isline'):
                print queries.isline.__doc__
             elif (arg[i] == 'isprojection'):
                print queries.isprojection.__doc__
             elif (arg[i] == 'ismarker'):
                print queries.ismarker.__doc__
             elif (arg[i] == 'isoutfill'):
                print queries.isoutfill.__doc__
             elif (arg[i] == 'isoutline'):
                print queries.isoutline.__doc__
             elif (arg[i] == 'isscatter'):
                print queries.isscatter.__doc__
             elif (arg[i] == 'issecondaryobject'):
                print queries.issecondaryobject.__doc__
             elif (arg[i] == 'istemplate'):
                print queries.istemplate.__doc__
             elif (arg[i] == 'istextcombined'):
                print queries.istextcombined.__doc__
             elif (arg[i] == 'istexttable'):
                print queries.istexttable.__doc__
             elif (arg[i] == 'istextorientation'):
                print queries.istextorientation.__doc__
             elif (arg[i] == 'isscatter'):
                print queries.isscatter.__doc__
             elif (arg[i] == 'isvector'):
                print queries.isvector.__doc__
             elif (arg[i] == 'isxvsy'):
                print queries.isxvsy.__doc__
             elif (arg[i] == 'isxyvsy'):
                print queries.isxyvsy.__doc__
             elif (arg[i] == 'isyxvsx'):
                print queries.isyxvsx.__doc__
             elif (arg[i] == 'colormapgui'):
                print vcs_legacy.Canvas.Canvas.colormapgui.__doc__
             elif (arg[i] == 'eps'):
                print vcs_legacy.Canvas.Canvas.eps.__doc__
             elif (arg[i] == 'createboxfill'):
                print vcs_legacy.Canvas.Canvas.createboxfill.__doc__
             elif (arg[i] == 'createcolormap'):
                print vcs_legacy.Canvas.Canvas.createcolormap.__doc__
             elif (arg[i] == 'createcontinents'):
                print vcs_legacy.Canvas.Canvas.createcontinents.__doc__
             elif (arg[i] == 'createfillarea'):
                print vcs_legacy.Canvas.Canvas.createfillarea.__doc__
             elif (arg[i] == 'createmeshfill'):
                print vcs_legacy.Canvas.Canvas.createmeshfill.__doc__
             elif (arg[i] == 'createprojection'):
                print vcs_legacy.Canvas.Canvas.createprojection.__doc__
             elif (arg[i] == 'createisofill'):
                print vcs_legacy.Canvas.Canvas.createisofill.__doc__
             elif (arg[i] == 'createisoline'):
                print vcs_legacy.Canvas.Canvas.createisoline.__doc__
             elif (arg[i] == 'createline'):
                print vcs_legacy.Canvas.Canvas.createline.__doc__
             elif (arg[i] == 'createmarker'):
                print vcs_legacy.Canvas.Canvas.createmarker.__doc__
             elif (arg[i] == 'createoutfill'):
                print vcs_legacy.Canvas.Canvas.createoutfill.__doc__
             elif (arg[i] == 'createoutline'):
                print vcs_legacy.Canvas.Canvas.createoutline.__doc__
             elif (arg[i] == 'createscatter'):
                print vcs_legacy.Canvas.Canvas.createscatter.__doc__
             elif (arg[i] == 'createtemplate'):
                print vcs_legacy.Canvas.Canvas.createtemplate.__doc__
             elif (arg[i] == 'createtextcombined'):
                print vcs_legacy.Canvas.Canvas.createtextcombined.__doc__
             elif (arg[i] == 'createtextorientation'):
                print vcs_legacy.Canvas.Canvas.createtextorientation.__doc__
             elif (arg[i] == 'createtexttable'):
                print vcs_legacy.Canvas.Canvas.createtexttable.__doc__
             elif (arg[i] == 'createvector'):
                print vcs_legacy.Canvas.Canvas.createvector.__doc__
             elif (arg[i] == 'createxvsy'):
                print vcs_legacy.Canvas.Canvas.createxvsy.__doc__
             elif (arg[i] == 'createxyvsy'):
                print vcs_legacy.Canvas.Canvas.createxyvsy.__doc__
             elif (arg[i] == 'createyxvsx'):
                print vcs_legacy.Canvas.Canvas.createyxvsx.__doc__
             elif (arg[i] == 'getboxfill'):
                print vcs_legacy.Canvas.Canvas.getboxfill.__doc__
             elif (arg[i] == 'getcolormap'):
                print vcs_legacy.Canvas.Canvas.getcolormap.__doc__
             elif (arg[i] == 'getcolormapname'):
                print vcs_legacy.Canvas.Canvas.getcolormapname.__doc__
             elif (arg[i] == 'getcolorcell'):
                print vcs_legacy.Canvas.Canvas.getcolorcell.__doc__
             elif (arg[i] == 'getcolors'):
                print vcs_legacy.getcolors.__doc__
             elif (arg[i] == 'getcontinents'):
                print vcs_legacy.Canvas.Canvas.getcontinents.__doc__
             elif (arg[i] == 'getcontinentstype'):
                print vcs_legacy.Canvas.Canvas.getcontinentstype.__doc__
             elif (arg[i] == 'getfillarea'):
                print vcs_legacy.Canvas.Canvas.getfillarea.__doc__
             elif (arg[i] == 'getmeshfill'):
                print vcs_legacy.Canvas.Canvas.getmeshfill.__doc__
             elif (arg[i] == 'getprojection'):
                print vcs_legacy.Canvas.Canvas.getprojection.__doc__
             elif (arg[i] == 'getisofill'):
                print vcs_legacy.Canvas.Canvas.getisofill.__doc__
             elif (arg[i] == 'getisoline'):
                print vcs_legacy.Canvas.Canvas.getisoline.__doc__
             elif (arg[i] == 'getline'):
                print vcs_legacy.Canvas.Canvas.getline.__doc__
             elif (arg[i] == 'getmarker'):
                print vcs_legacy.Canvas.Canvas.getmarker.__doc__
             elif (arg[i] == 'getoutfill'):
                print vcs_legacy.Canvas.Canvas.getoutfill.__doc__
             elif (arg[i] == 'getoutline'):
                print vcs_legacy.Canvas.Canvas.getoutline.__doc__
             elif (arg[i] == 'getplot'):
                print vcs_legacy.Canvas.Canvas.getplot.__doc__
             elif (arg[i] == 'getscatter'):
                print vcs_legacy.Canvas.Canvas.getscatter.__doc__
             elif (arg[i] == 'gettemplate'):
                print vcs_legacy.Canvas.Canvas.gettemplate.__doc__
             elif (arg[i] == 'gettextcombined'):
                print vcs_legacy.Canvas.Canvas.gettextcombined.__doc__
             elif (arg[i] == 'gettext'):
                print vcs_legacy.Canvas.Canvas.gettextcombined.__doc__
             elif (arg[i] == 'gettextorientation'):
                print vcs_legacy.Canvas.Canvas.gettextorientation.__doc__
             elif (arg[i] == 'gettexttable'):
                print vcs_legacy.Canvas.Canvas.gettexttable.__doc__
             elif (arg[i] == 'getvector'):
                print vcs_legacy.Canvas.Canvas.getvector.__doc__
             elif (arg[i] == 'getxvsy'):
                print vcs_legacy.Canvas.Canvas.getxvsy.__doc__
             elif (arg[i] == 'getxyvsy'):
                print vcs_legacy.Canvas.Canvas.getxyvsy.__doc__
             elif (arg[i] == 'getyxvsx'):
                print vcs_legacy.Canvas.Canvas.getyxvsx.__doc__
             elif (arg[i] == 'removeobject'):
                print vcs_legacy.Canvas.Canvas.removeobject.__doc__
             elif (arg[i] == 'saveinitialfile'):
                print vcs_legacy.Canvas.Canvas.saveinitialfile.__doc__
             elif (arg[i] == 'scriptobject'):
                print vcs_legacy.Canvas.Canvas.scriptobject.__doc__
             elif (arg[i] == 'scriptrun'):
                print vcs_legacy.Canvas.Canvas.scriptrun.__doc__
             elif (arg[i] == 'scriptstate'):
                print vcs_legacy.Canvas.Canvas.scriptstate.__doc__
             elif (arg[i] == 'animate'):
                print vcs_legacy.Canvas.animate_obj.__doc__
             elif (arg[i] == 'flush'):
                print vcs_legacy.Canvas.Canvas.flush.__doc__
             elif (arg[i] == 'flushcanvas'):
                print vcs_legacy.Canvas.Canvas.flushcanvas.__doc__
             elif (arg[i] == 'geometry'):
                print vcs_legacy.Canvas.Canvas.geometry.__doc__
#             elif (arg[i] == 'get'):
#                print vcs_legacy.Canvas.Canvas.get.__doc__
             elif (arg[i] == 'gif'):
                print vcs_legacy.Canvas.Canvas.gif.__doc__
             elif (arg[i] == 'graphicsmethodtype'):
                print vcs_legacy.Canvas.graphicsmethodtype.__doc__
             elif (arg[i] == 'gs'):
                print vcs_legacy.Canvas.Canvas.gs.__doc__
             elif (arg[i] == 'help'):
                help__doc__()
#             elif (arg[i] == 'grid'):
#                print vcs_legacy.Canvas.Canvas.grid.__doc__
             elif (arg[i] == 'landscape'):
                print vcs_legacy.Canvas.Canvas.landscape.__doc__
             elif (arg[i] == 'listelements'):
                print vcs_legacy.Canvas.Canvas.listelements.__doc__
             elif (arg[i] == 'mkevenlevels'):
                print vcs_legacy.mkevenlevels.__doc__
             elif (arg[i] == 'mklabels'):
                print vcs_legacy.mklabels.__doc__
             elif (arg[i] == 'mkscale'):
                print vcs_legacy.mkscale.__doc__
             elif (arg[i] == 'mode'):
                mode__doc__()
             elif (arg[i] == 'objecthelp'):
                print objecthelp.__doc__
             elif (arg[i] == 'open'):
                print vcs_legacy.Canvas.Canvas.open.__doc__
             elif (arg[i] == 'orientation'):
                print vcs_legacy.Canvas.Canvas.orientation.__doc__
             elif (arg[i] == 'page'):
                print vcs_legacy.Canvas.Canvas.page.__doc__
             elif (arg[i] == 'pause'):
                print vcs_legacy.pause.__doc__
             elif (arg[i] == 'portrait'):
                print vcs_legacy.Canvas.Canvas.portrait.__doc__
             elif (arg[i] == 'postscript'):
                print vcs_legacy.Canvas.Canvas.postscript.__doc__
             elif (arg[i] == 'printer'):
                print vcs_legacy.Canvas.Canvas.printer.__doc__
             elif (arg[i] == 'pstogif'):
                print vcs_legacy.Canvas.Canvas.pstogif.__doc__
#             elif (arg[i] == 'put'):
#                print vcs_legacy.Canvas.Canvas.put.__doc__
             elif (arg[i] == 'raster'):
                print vcs_legacy.Canvas.Canvas.raster.__doc__
             elif (arg[i] == 'refreshcanvas'):
                print vcs_legacy.Canvas.Canvas.refreshcanvas.__doc__
             elif (arg[i] == 'resetgrid'):
                print vcs_legacy.Canvas.Canvas.resetgrid.__doc__
             elif (arg[i] == 'set'):
                print vcs_legacy.Canvas.Canvas.set.__doc__
             elif (arg[i] == 'setcolorcell'):
                print vcs_legacy.Canvas.Canvas.setcolorcell.__doc__
             elif (arg[i] == 'setcolormap'):
                print vcs_legacy.Canvas.Canvas.setcolormap.__doc__
             elif (arg[i] == 'setcontinentstype'):
                print vcs_legacy.Canvas.Canvas.setcontinentstype.__doc__
#             elif (arg[i] == 'setminmax'):
#                print vcs_legacy.Canvas.Canvas.setminmax.__doc__
             elif (arg[i] == 'show'):
                print vcs_legacy.Canvas.Canvas.show.__doc__
             elif (arg[i] == 'update'):
                print vcs_legacy.Canvas.Canvas.update.__doc__
             elif (arg[i] == 'vcs_legacyError'):
                print vcs_legacy.vcs_legacyError.__doc__
             elif (arg[i] == 'projection'):
                 print vcs_legacy.Canvas.projection.Proj.__doc__
             else:
                print 'Error - VCS does not have the command (%s).' % arg[i]
                raise ValueError, "Try using vcs_legacy.objecthelp(pass_object)"

def mode__doc__():
    print " Function: mode                         # Update the VCS Canvas."
    print ""
    print "  Description of Function:"
    print "    Updating of the graphical displays on the VCS Canvas can be"
    print "    deferred until a later time. This is helpful when generating"
    print "    templates or displaying numerous plots. If a series of commands"
    print "    are given to VCS and the Canvas Mode is set to manual (i.e., 0),"
    print "    then no updating of the VCS Canvas occurs until the 'update' "
    print "    function is executed."
    print "    "
    print "    Note, by default the VCS Canvas Mode is set to `Automatic', which"
    print "    means VCS will update the VCS Canvas as necessary without prompting"
    print "    from the user."
    print ""
    print " Example of Use:"
    print "    ..."
    print ""
    print "    a=vcs_legacy.init()"
    print "    a.mode=0                                    # Set updating to manual mode"
    print "    a.plot(s,'default','boxfill','quick')"
    print ""
    print "    box=x.getboxfill('quick')"
    print "    box.color_1=100"
    print "    box.xticlabels('lon30','lon30')"
    print "    box.xticlabels('','')"
    print "    box.datawc(1e20,1e20,1e20,1e20)"
    print "    box.datawc(-45.0, 45.0, -90.0, 90.0)"
    print ""
    print "    a.update()                                  # Update the changes manually"

def help__doc__():
    print " Function: help"
    print ""
    print "  Description of Function:"
    print "    Prints brief descriptions of VCS commands."
    print "    Type: 'vcs_legacy.help()' for more information."
    print ""




###############################################################################
#        END OF FILE                                                          #
###############################################################################

