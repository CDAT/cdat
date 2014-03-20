# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test page orientation module
#
############################################################################
#                                                                          #
# Module:       testorientation module                                     #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's page orientation.                       #
#                                                                          #
# Version:      4.0                                                        #
#                                                                          #
############################################################################
#
#
#
############################################################################
#                                                                          #
# Import: VCS  and cu modules.                                             #
#                                                                          #
############################################################################
def test():
   import vcs_legacy,cdms2,time,os,sys,support          # import vcs_legacy and cdms

   # In the initial.attribute file the page orientation is set to
   # portrait. The test is to convert from portrait to landscape.
   # See your initial.attribute file and set "Page(portrait)".
   # This should be located at the top of the initial.attribute
   # file.
   x=vcs_legacy.init()
   x.pause_time = 2
   
   # Check for the canvas orientation, which should be by default 'landscape'
   if x.islandscape() == 1:              # will return 1 if true or 0 if false
      print 'landscape page mode'
   if x.isportrait() == 1:		      # will return 1 if true or 0 if false
      print 'portrait page mode'
   print 'The page orientation is ( %s ). ' % x.orientation()  # will return 'landscape' or 'portrait'
   
   # Change the page orientation to portrait and print the test cases
   print '\nChange the page orientation to portrait and test'
   x.portrait()			# Change page orientation to portrait
   if x.islandscape() == 1:
      print 'landscape page mode'
   if x.isportrait() == 1:
      print 'portrait page mode'
   print 'The page orientation is ( %s ). ' % x.orientation()  # will return 'landscape' or 'portrait'
   
   # Change the page orientation to landscape and print the test cases
   print '\nChange the page orientation to landscape and test'
   x.landscape()
   if x.islandscape() == 1:              # will return 1 if true or 0 if false
      print 'landscape page mode'
   if x.isportrait() == 1:		      # will return 1 if true or 0 if false
      print 'portrait page mode'
   print 'The page orientation is ( %s ). ' % x.orientation()  # will return 'landscape' or 'portrait'
   

   #
   # prints in location mode
   # I feel this is correct!
   #
   f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   s=f('clt')
   x=vcs_legacy.init()
   t=x.createtemplate('new')
   to=x.createtextorientation('test')
   to.height = 30
   t.mean.y=0.5
   t.mean.x=0.5
   t.mean.textorientation = to 
   t.mean.list()
   x.plot(s,t,bg=support.bg)
   support.check_plot(x)
   x.flush()
   support.check_plot(x)
   
   #
   x.portrait()           # This will clear the screen
   support.check_plot(x)
   x.plot(s,t,bg=support.bg)
   support.check_plot(x)

   #
   x.landscape()           # This will clear the screen
   support.check_plot(x)
   x.plot(s,t,bg=support.bg)
   support.check_plot(x)
   
   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   P A G E  O R I E N T A T I O N   T E S T   C O M P L E T E D          ******'
   print '******   S U C E S S F U L L Y    ! ! !                                        ******'
   print '******                                                                         ******'
   print '*************************************************************************************'

if __name__=="__main__":
   test()

