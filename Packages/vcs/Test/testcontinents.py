#
# Test Continents (Gcon) module
#
############################################################################
#                                                                          #
# Module:       testcontinents module                                      #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's continents graphics method.             #
#                                                                          #
#               run with "-i" option:   python -i testboxfill.py           #
#                                                                          #
#                                                                          #
# Version:      4.0                                                        #
#                                                                          #
############################################################################
#
#
#
############################################################################
#                                                                          #
# Import: VCS modules.                                                     #
#                                                                          #
############################################################################


def test():
    import vcs
    import time
    import support
    import sys
    import os                      # import vcs and cu
    bg = support.bg

    x = vcs.init()                         # construct vcs canvas

    x.plot('default', 'continents', 'ASD', bg=bg)  # plot slab the old way
    support.check_plot(x)
    if bg == 0:
        # change the geometry and location
        x.geometry(450, 337, 100, 0)
        x.flush()
        support.check_plot(x)

    # show list of continents
    obj = x.listelements('continents')
    a = x.createcontinents('quick')         	# get 'quick' continents
    # test object 'a' for graphics method
    if not vcs.isgraphicsmethod(a):
        raise Exception("Error not a gm")
    else:
        if not vcs.iscontinents(a):             # test object 'a' if continents
            raise Exception("Error wrong type of gm")
    x.clear()
    x.plot(a, bg=bg)
    support.check_plot(x)

    # save 'quick' continents as a Python script
    a.script('test', 'w')

    a.xticlabels('', '')                  # remove the x-axis
    support.check_plot(x)
    a.xticlabels('lon30', 'lon30')        # change the x-axis
    support.check_plot(x)
    a.xticlabels('*')                    # put the x-axis
    support.check_plot(x)
    a.datawc(-45.0, 45.0, -90.0, 90.0)   # change the region
    support.check_plot(x)
    a.datawc(1e20, 1e20, 1e20, 1e20)        # put the region back
    support.check_plot(x)

    if not '--extended' in sys.argv:
        print '\n************* PARTIAL TEST *****************'
        print 'FOR COMPLETE TEST OF THIS MODULE USE '
        print '   -F (--full) or -E (--extended) option'
        print '************* PARTIAL TEST *****************\n'
        sys.exit()

    # same as 'dash', change the line style
    a.line = 1
    support.check_plot(x)
    # same as 'dot', change the line style
    a.line = 2
    support.check_plot(x)
    # same as 'dash-dot', change the line style
    a.line = 3
    support.check_plot(x)
    # same as 'solid', change the line style
    a.line = 0
    support.check_plot(x)
    # same as 'long-dash', change the line style
    a.line = 4
    support.check_plot(x)
    a.linecolor = (77)                     # change the line color
    support.check_plot(x)
    a.linecolor = 16                       # change the line color
    support.check_plot(x)
    a.linecolor = 44                       # same as a.linecolor=(44)
    support.check_plot(x)
    a.linecolor = None                     # use the default line color, black
    support.check_plot(x)
    # use default line style, solid black line
    a.line = None
    support.check_plot(x)

    x.clear()                            # clear the VCS Canvas
    # plot continents using 'default' template
    x.continents(a, 'default', bg=bg)
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

    x.clear()                            # clear the VCS Canvas
    # plot continents using template 't', and continents 'a'
    x.plot(t, a, bg=bg)
    support.check_plot(x)
    x.clear()                            # clear the VCS Canvas
    x.continents(a, t, bg=bg)			# plot continents
    support.check_plot(x)

    #########################################################################
    # Create line object 'l' from the default line                          #
    #########################################################################
    # show the list of line secondary objects
    objs = x.listelements('line')
    l = x.getline('red')                	# get line 'red'
    # check to see if it is a secondary object
    if not vcs.issecondaryobject(l):
        raise Exception("Error did not get line")
    else:
        if not vcs.isline(l):                  	# check to see if it is a line
            raise Exception("Error object created is not line")

    #########################################################################
    # Use the create line object 'l' from above and modify the line object  #
    #########################################################################
    a.line = l                             # use the line object
    support.check_plot(x)
    l.color = 44                         # change the line color
    support.check_plot(x)
    l.type = 'dash'                       # change the line type
    support.check_plot(x)

    a = x.listelements('continents')                      # show list of xyvsy
    r = x.createcontinents('test2', 'quick')     # create xyvsy 'test2'
    a2 = x.listelements('continents')                      # show list of xyvsy
    if a2 == a:
        raise "error gm not created or not added to list"
# x.removeobject(r)                    # remove xyvsy 'test2'
# a3 = x.listelements('continents')                      # show list of xyvsy
# if a3!=a:
##       raise "error gm not removed"

    # get 'quick' boxfill graphics method
    a = x.getcontinents('quick')
    x.clear()
    x.plot(a, bg=bg)
    support.check_plot(x)
    if support.dogui:
        x.graphicsmethodgui(
            'continents',
            'quick')  # display the continents graphics method GUI

    ##########################################################################
    # to see how x.update and x.mode work, see testoutline.py                       #
    ##########################################################################
    # x.update()
    # x.mode=1
    # x.mode=0

    print '*******************************************************************************************'
    print '******                                                                               ******'
    print '******   C O N T I N E N T S   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
    print '******                                                                               ******'
    print '*******************************************************************************************'

if __name__ == "__main__":
    test()
