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
        import vcs                                                             
       vcs.objecthelp(object)  #  where: object is the Python object          
"""                                                                           
    for x in arg:
        print getattr(x, "__doc__", "")

###############################################################################
#                                                                             #
# Import: VCS and query module.                                               #
#                                                                             #
###############################################################################
import vcs, queries

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
               'setcontinentstype', 'show', 'update', 'vcsError']

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
          print "vcs.help()             - prints VCS Model and Primary and Secondary Object list"
          print "vcs.help('init')       - prints boxfill description and example of use"
          print "vcs.help('plot')       - prints boxfill description and example of use"
          print "vcs.help('lineobject') - prints boxfill description and example of use"
          print "vcs.help('init','plot) - prints both descriptions and example of use"
        else:
          for i in range(len(arg)):
             # Template Objects
             if (arg[i] == 'templateobject'):
                print vcs.template.P.__doc__
             # Primary Objects
             elif (arg[i] == 'boxfillobject'):
                print vcs.boxfill.Gfb.__doc__
             elif (arg[i] == 'isofillobject'):
                print vcs.isofill.Gfi.__doc__
             elif (arg[i] == 'isolineobject'):
                print vcs.isoline.Gi.__doc__
             elif (arg[i] == 'continentsobject'):
                print vcs.continents.Gcon.__doc__
             elif (arg[i] == 'meshfillobject'):
                print vcs.meshfill.Gfm.__doc__
             elif (arg[i] == 'outfillobject'):
                print vcs.outfill.Gfo.__doc__
             elif (arg[i] == 'outlineobject'):
                print vcs.outline.Go.__doc__
             elif (arg[i] == 'scatterobject'):
                print vcs.scatter.GSp.__doc__
             elif (arg[i] == 'vectorobject'):
                print vcs.vector.Gv.__doc__
             elif (arg[i] == 'xvsyobject'):
                print vcs.xvsy.GXY.__doc__
             elif (arg[i] == 'xyvsyobject'):
                print vcs.xyvsy.GXy.__doc__
             elif (arg[i] == 'yxvsxobject'):
                print vcs.yxvsx.GYx.__doc__
             # Secondary Objects
             elif (arg[i] == 'lineobject'):
                print vcs.line.Tl.__doc__
             elif (arg[i] == 'markerobject'):
                print vcs.marker.Tm.__doc__
             elif (arg[i] == 'fillareaobject'):
                print vcs.fillarea.Tf.__doc__
             elif (arg[i] == 'texttableobject'):
                print vcs.texttable.Tt.__doc__
             elif (arg[i] == 'textorientationobject'):
                print vcs.textorientation.To.__doc__
             elif (arg[i] == 'textcombinedobject'):
                print vcs.textcombined.Tc.__doc__
             elif (arg[i] == 'projectionobject'):
                print vcs.projection.Proj.__doc__
             elif (arg[i] == 'init'):
                print vcs.init.__doc__
             elif (arg[i] == 'plot'):
                print vcs.Canvas.Canvas.plot.__doc__
             elif (arg[i] == 'boxfill'):
                print vcs.Canvas.Canvas.boxfill.__doc__
             elif (arg[i] == 'clear'):
                print vcs.Canvas.Canvas.clear.__doc__
             elif (arg[i] == 'close'):
                print vcs.Canvas.Canvas.close.__doc__
             elif (arg[i] == 'cgm'):
                print vcs.Canvas.Canvas.cgm.__doc__
             elif (arg[i] == 'meshfill'):
                print vcs.Canvas.Canvas.meshfill.__doc__
             elif (arg[i] == 'isofill'):
                print vcs.Canvas.Canvas.isofill.__doc__
             elif (arg[i] == 'isoline'):
                print vcs.Canvas.Canvas.isoline.__doc__
             elif (arg[i] == 'continents'):
                print vcs.Canvas.Canvas.continents.__doc__
             elif (arg[i] == 'outfill'):
                print vcs.Canvas.Canvas.outfill.__doc__
             elif (arg[i] == 'outline'):
                print vcs.Canvas.Canvas.outline.__doc__
             elif (arg[i] == 'scatter'):
                print vcs.Canvas.Canvas.scatter.__doc__
             elif (arg[i] == 'vector'):
                print vcs.Canvas.Canvas.vector.__doc__
             elif (arg[i] == 'xvsy'):
                print vcs.Canvas.Canvas.xvsy.__doc__
             elif (arg[i] == 'xyvsy'):
                print vcs.Canvas.Canvas.xyvsy.__doc__
             elif (arg[i] == 'yxvsx'):
                print vcs.Canvas.Canvas.yxvsx.__doc__
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
                print vcs.Canvas.Canvas.isportrait.__doc__
             elif (arg[i] == 'islandscape'):
                print vcs.Canvas.Canvas.islandscape.__doc__
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
                print vcs.Canvas.Canvas.colormapgui.__doc__
             elif (arg[i] == 'eps'):
                print vcs.Canvas.Canvas.eps.__doc__
             elif (arg[i] == 'createboxfill'):
                print vcs.Canvas.Canvas.createboxfill.__doc__
             elif (arg[i] == 'createcolormap'):
                print vcs.Canvas.Canvas.createcolormap.__doc__
             elif (arg[i] == 'createcontinents'):
                print vcs.Canvas.Canvas.createcontinents.__doc__
             elif (arg[i] == 'createfillarea'):
                print vcs.Canvas.Canvas.createfillarea.__doc__
             elif (arg[i] == 'createmeshfill'):
                print vcs.Canvas.Canvas.createmeshfill.__doc__
             elif (arg[i] == 'createprojection'):
                print vcs.Canvas.Canvas.createprojection.__doc__
             elif (arg[i] == 'createisofill'):
                print vcs.Canvas.Canvas.createisofill.__doc__
             elif (arg[i] == 'createisoline'):
                print vcs.Canvas.Canvas.createisoline.__doc__
             elif (arg[i] == 'createline'):
                print vcs.Canvas.Canvas.createline.__doc__
             elif (arg[i] == 'createmarker'):
                print vcs.Canvas.Canvas.createmarker.__doc__
             elif (arg[i] == 'createoutfill'):
                print vcs.Canvas.Canvas.createoutfill.__doc__
             elif (arg[i] == 'createoutline'):
                print vcs.Canvas.Canvas.createoutline.__doc__
             elif (arg[i] == 'createscatter'):
                print vcs.Canvas.Canvas.createscatter.__doc__
             elif (arg[i] == 'createtemplate'):
                print vcs.Canvas.Canvas.createtemplate.__doc__
             elif (arg[i] == 'createtextcombined'):
                print vcs.Canvas.Canvas.createtextcombined.__doc__
             elif (arg[i] == 'createtextorientation'):
                print vcs.Canvas.Canvas.createtextorientation.__doc__
             elif (arg[i] == 'createtexttable'):
                print vcs.Canvas.Canvas.createtexttable.__doc__
             elif (arg[i] == 'createvector'):
                print vcs.Canvas.Canvas.createvector.__doc__
             elif (arg[i] == 'createxvsy'):
                print vcs.Canvas.Canvas.createxvsy.__doc__
             elif (arg[i] == 'createxyvsy'):
                print vcs.Canvas.Canvas.createxyvsy.__doc__
             elif (arg[i] == 'createyxvsx'):
                print vcs.Canvas.Canvas.createyxvsx.__doc__
             elif (arg[i] == 'getboxfill'):
                print vcs.Canvas.Canvas.getboxfill.__doc__
             elif (arg[i] == 'getcolormap'):
                print vcs.Canvas.Canvas.getcolormap.__doc__
             elif (arg[i] == 'getcolormapname'):
                print vcs.Canvas.Canvas.getcolormapname.__doc__
             elif (arg[i] == 'getcolorcell'):
                print vcs.Canvas.Canvas.getcolorcell.__doc__
             elif (arg[i] == 'getcolors'):
                print vcs.getcolors.__doc__
             elif (arg[i] == 'getcontinents'):
                print vcs.Canvas.Canvas.getcontinents.__doc__
             elif (arg[i] == 'getcontinentstype'):
                print vcs.Canvas.Canvas.getcontinentstype.__doc__
             elif (arg[i] == 'getfillarea'):
                print vcs.Canvas.Canvas.getfillarea.__doc__
             elif (arg[i] == 'getmeshfill'):
                print vcs.Canvas.Canvas.getmeshfill.__doc__
             elif (arg[i] == 'getprojection'):
                print vcs.Canvas.Canvas.getprojection.__doc__
             elif (arg[i] == 'getisofill'):
                print vcs.Canvas.Canvas.getisofill.__doc__
             elif (arg[i] == 'getisoline'):
                print vcs.Canvas.Canvas.getisoline.__doc__
             elif (arg[i] == 'getline'):
                print vcs.Canvas.Canvas.getline.__doc__
             elif (arg[i] == 'getmarker'):
                print vcs.Canvas.Canvas.getmarker.__doc__
             elif (arg[i] == 'getoutfill'):
                print vcs.Canvas.Canvas.getoutfill.__doc__
             elif (arg[i] == 'getoutline'):
                print vcs.Canvas.Canvas.getoutline.__doc__
             elif (arg[i] == 'getplot'):
                print vcs.Canvas.Canvas.getplot.__doc__
             elif (arg[i] == 'getscatter'):
                print vcs.Canvas.Canvas.getscatter.__doc__
             elif (arg[i] == 'gettemplate'):
                print vcs.Canvas.Canvas.gettemplate.__doc__
             elif (arg[i] == 'gettextcombined'):
                print vcs.Canvas.Canvas.gettextcombined.__doc__
             elif (arg[i] == 'gettext'):
                print vcs.Canvas.Canvas.gettextcombined.__doc__
             elif (arg[i] == 'gettextorientation'):
                print vcs.Canvas.Canvas.gettextorientation.__doc__
             elif (arg[i] == 'gettexttable'):
                print vcs.Canvas.Canvas.gettexttable.__doc__
             elif (arg[i] == 'getvector'):
                print vcs.Canvas.Canvas.getvector.__doc__
             elif (arg[i] == 'getxvsy'):
                print vcs.Canvas.Canvas.getxvsy.__doc__
             elif (arg[i] == 'getxyvsy'):
                print vcs.Canvas.Canvas.getxyvsy.__doc__
             elif (arg[i] == 'getyxvsx'):
                print vcs.Canvas.Canvas.getyxvsx.__doc__
             elif (arg[i] == 'removeobject'):
                print vcs.Canvas.Canvas.removeobject.__doc__
             elif (arg[i] == 'saveinitialfile'):
                print vcs.Canvas.Canvas.saveinitialfile.__doc__
             elif (arg[i] == 'scriptobject'):
                print vcs.Canvas.Canvas.scriptobject.__doc__
             elif (arg[i] == 'scriptrun'):
                print vcs.Canvas.Canvas.scriptrun.__doc__
             elif (arg[i] == 'scriptstate'):
                print vcs.Canvas.Canvas.scriptstate.__doc__
             elif (arg[i] == 'animate'):
                print vcs.Canvas.animate_obj.__doc__
             elif (arg[i] == 'flush'):
                print vcs.Canvas.Canvas.flush.__doc__
             elif (arg[i] == 'flushcanvas'):
                print vcs.Canvas.Canvas.flushcanvas.__doc__
             elif (arg[i] == 'geometry'):
                print vcs.Canvas.Canvas.geometry.__doc__
#             elif (arg[i] == 'get'):
#                print vcs.Canvas.Canvas.get.__doc__
             elif (arg[i] == 'gif'):
                print vcs.Canvas.Canvas.gif.__doc__
             elif (arg[i] == 'graphicsmethodtype'):
                print vcs.Canvas.graphicsmethodtype.__doc__
             elif (arg[i] == 'gs'):
                print vcs.Canvas.Canvas.gs.__doc__
             elif (arg[i] == 'help'):
                help__doc__()
#             elif (arg[i] == 'grid'):
#                print vcs.Canvas.Canvas.grid.__doc__
             elif (arg[i] == 'landscape'):
                print vcs.Canvas.Canvas.landscape.__doc__
             elif (arg[i] == 'listelements'):
                print vcs.Canvas.Canvas.listelements.__doc__
             elif (arg[i] == 'mkevenlevels'):
                print vcs.mkevenlevels.__doc__
             elif (arg[i] == 'mklabels'):
                print vcs.mklabels.__doc__
             elif (arg[i] == 'mkscale'):
                print vcs.mkscale.__doc__
             elif (arg[i] == 'mode'):
                mode__doc__()
             elif (arg[i] == 'objecthelp'):
                print objecthelp.__doc__
             elif (arg[i] == 'open'):
                print vcs.Canvas.Canvas.open.__doc__
             elif (arg[i] == 'orientation'):
                print vcs.Canvas.Canvas.orientation.__doc__
             elif (arg[i] == 'page'):
                print vcs.Canvas.Canvas.page.__doc__
             elif (arg[i] == 'pause'):
                print vcs.pause.__doc__
             elif (arg[i] == 'portrait'):
                print vcs.Canvas.Canvas.portrait.__doc__
             elif (arg[i] == 'postscript'):
                print vcs.Canvas.Canvas.postscript.__doc__
             elif (arg[i] == 'printer'):
                print vcs.Canvas.Canvas.printer.__doc__
             elif (arg[i] == 'pstogif'):
                print vcs.Canvas.Canvas.pstogif.__doc__
#             elif (arg[i] == 'put'):
#                print vcs.Canvas.Canvas.put.__doc__
             elif (arg[i] == 'raster'):
                print vcs.Canvas.Canvas.raster.__doc__
             elif (arg[i] == 'refreshcanvas'):
                print vcs.Canvas.Canvas.refreshcanvas.__doc__
             elif (arg[i] == 'resetgrid'):
                print vcs.Canvas.Canvas.resetgrid.__doc__
             elif (arg[i] == 'set'):
                print vcs.Canvas.Canvas.set.__doc__
             elif (arg[i] == 'setcolorcell'):
                print vcs.Canvas.Canvas.setcolorcell.__doc__
             elif (arg[i] == 'setcolormap'):
                print vcs.Canvas.Canvas.setcolormap.__doc__
             elif (arg[i] == 'setcontinentstype'):
                print vcs.Canvas.Canvas.setcontinentstype.__doc__
#             elif (arg[i] == 'setminmax'):
#                print vcs.Canvas.Canvas.setminmax.__doc__
             elif (arg[i] == 'show'):
                print vcs.Canvas.Canvas.show.__doc__
             elif (arg[i] == 'update'):
                print vcs.Canvas.Canvas.update.__doc__
             elif (arg[i] == 'vcsError'):
                print vcs.vcsError.__doc__
             elif (arg[i] == 'projection'):
                 print vcs.Canvas.projection.Proj.__doc__
             else:
                print 'Error - VCS does not have the command (%s).' % arg[i]
                raise ValueError, "Try using vcs.objecthelp(pass_object)"

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
    print "    a=vcs.init()"
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
    print "    Type: 'vcs.help()' for more information."
    print ""




###############################################################################
#        END OF FILE                                                          #
###############################################################################

