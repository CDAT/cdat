# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Outfill (Gfo) module
#
############################################################################
#                                                                          #
# Module:       testoutfill module                                         #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's outfill graphics method.                #
#                                                                          #
# Version:      4.0                                                        #
#                                                                          #
############################################################################
#
#
#
############################################################################
#                                                                          #
# Import: VCS and cdms modules.                                            #
#                                                                          #
############################################################################


def test():
    import vcs
    import cdms2 as cdms
    import time
    import os
    import sys
    import support          # import vcs and cdms

    bg = support.bg
    f = cdms.open(os.path.join(vcs.sample_data, 'clt.nc'))  # open clt file
    s = f('clt')                           # get slab clt
    x = vcs.init()                         # construct vcs canvas

    x.plot(s, 'default', 'outfill', 'ASD_map', bg=bg)  # plot slab the old way
    support.check_plot(x)
    if bg == 0:
        # change the geometry and location
        x.geometry(450, 337, 100, 0)
        support.check_plot(x)
        x.flush()
        support.check_plot(x)

    s = f('u')	                        # get u slab
    x.clear()                            # clear the VCS Canvas
    x.plot(s, 'default', 'outfill', 'ASD_map', bg=bg)  # plot the surface data
    support.check_plot(x)

    a = x.createoutfill('quick')		# create 'quick' outfill
    # test object 'a' for graphics method
    if not vcs.isgraphicsmethod(a):
        raise Exception("Error did not retrieve the gm")
    else:
        if not vcs.isoutfill(a):		# check for isofill
            raise Exception("Error gm is not right type")

    a.script('test', 'w')			# save 'quick' outfill as a Python script

    a.xticlabels('lon30', 'lon30')        # change the x-axis
    support.check_plot(x)
    a.xticlabels('', '')                  # remove the x-axis
    support.check_plot(x)
    a.xticlabels('*')                    # put the x-axis
    support.check_plot(x)
    a.datawc(-45.0, 45.0, -90.0, 90.0)   # change the region
    support.check_plot(x)
    a.datawc(1e20, 1e20, 1e20, 1e20)        # put the region back
    support.check_plot(x)

    cont = False
    for anarg in sys.argv:
        if anarg in ['--extended', '--full', '-E', '-F']:
            cont = True
            break
    if cont is False:
        print '\n************* PARTIAL TEST *****************'
        print 'FOR COMPLETE TEST OF THIS MODULE USE '
        print '   -F (--full) or -E (--extended) option'
        print '************* PARTIAL TEST *****************\n'
        sys.exit()

    x.clear()                            # clear the VCS Canvas
    # plot outfill using 'default' template
    x.outfill(s, a, 'default', bg=bg)

    a.fillareastyle = 'hatch'		# change the fill style to hatch
    support.check_plot(x)
    a.fillareaindex = 11			# change the hatch index pattern
    support.check_plot(x)
    a.fillareacolor = (77)			# change the hatch color
    support.check_plot(x)
    a.fillareacolor = 16			# chnage the hatch color
    support.check_plot(x)
    a.fillareacolor = 44			# same as a.fillareacolor=(44)
    support.check_plot(x)
    a.fillareacolor = None			# use the default hatch color (black)
    support.check_plot(x)
    a.outfill = ([0])			# set the outfill value
    support.check_plot(x)
    a.outfill = ([1])			# set the outfill value
    support.check_plot(x)
    a.outfill = ([0, 1])			# set the outfill value
    support.check_plot(x)
    a.outfill = ([0])			# set the outfill value
    support.check_plot(x)

    support.check_plot(x)

    # get the list of templates
    objs = x.listelements('template')
    # create template 'test' from 'default' template
    t = x.createtemplate('test')
    # test whether 't' is a template or not
    if not vcs.istemplate(t):
        raise Exception("Error template not created")
    else:
        # get the list of templates
        a2 = x.listelements('template')
        if objs == a2:
            raise Exception("Error template not created or added to list")

    x.clear()				# clear the VCS Canvas
    # plot outfill template 't', outfill 'a', and array 's'
    x.plot(a, t, s, bg=bg)
    support.check_plot(x)
    x.clear()				# clear the VCS Canvas
    # plot using outfill 'a', array 's', and template 't'
    x.outfill(a, s, t, bg=bg)
    support.check_plot(x)

    # show the list of fillarea secondary objects
    objs = x.listelements('fillarea')
    f = x.getfillarea('AuTo_1')                	# get fillarea 'red'
    # check to see if it is a secondary object
    if not vcs.issecondaryobject(f):
        raise Exception("Error did not get fillarea")
    else:
        # check to see if it is a fillarea
        if not vcs.isfillarea(f):
            raise Exception("Error object created is not fillarea")
    a.fillareastyle = f
    support.check_plot(x)
    f.color = 44                           # change the fillarea object's color
    support.check_plot(x)
    # change the fillarea object's fill style
    f.style = 'hatch'
    support.check_plot(x)

    a = x.listelements('outfill')                      # show list of gm
    r = x.createoutfill('test2', 'quick')     # create xyvsy 'test2'
    a2 = x.listelements('outfill')                      # show list of gm
    if a2 == a:
        raise "error gm not created or not added to list"
    x.removeobject(r)                    # remove xyvsy 'test2'
    a3 = x.listelements('outfill')                      # show list of gm
    if a3 != a:
        raise "error gm not removed"

    ##########################################################################
    # to see how x.update and x.mode work, see testoutfill.py                       #
    ##########################################################################
    # x.update()
    # x.mode=1
    # x.mode=0
    print '*************************************************************************************'
    print '******                                                                         ******'
    print '******   O U T F I L L   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
    print '******                                                                         ******'
    print '*************************************************************************************'
    return x, s

if __name__ == "__main__":
    canvas, slab = test()
