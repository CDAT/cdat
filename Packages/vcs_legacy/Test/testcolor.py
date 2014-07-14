# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Colormap (Cp) module
#
############################################################################
#                                                                          #
# Module:       testcolormap module                                        #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's color map.                		   #
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
# Import: VCS and cdms modules.                                            #
#                                                                          #
############################################################################
def test():
   import vcs_legacy,cdms2 as cdms,time,os,sys,support                 # import vcs_legacy and cu

   bg=support.bg
   
   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   s=f('clt')                           # get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas

   x.plot(s,'default','isofill','quick',bg=bg)# plot slab the old way
   support.check_plot(x)
   if bg==0:
      x.geometry(450,337,0,0)              # change the geometry
      x.flush()
      support.check_plot(x)
   x.getcolormapname()			# get the active colormap name

   x.setcolormap("AMIP")		# change the colormap
   support.check_plot(x)


   cont = False
   for anarg in sys.argv:
      if anarg in ['--extended', '--full','-E','-F']:
         cont = True
         break
   if cont is False:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()


   ############################################################
   # Change the color map cell 31.			      #
   ############################################################
   colors = {}
   cols = [31,47,63,79,95,111,127,143,159,175,191]
   for c in cols:
      colors[c]=[int(float(c-16)/(191-16)*100.),]*3
   for c in cols:
      vals=colors[c]
      x.setcolorcell(c,vals[0],vals[1],vals[2])
      support.check_plot(x)
         
   for c in colors.keys():
      okvals=colors[c]
      vals = x.getcolorcell(c)
      if vals!=okvals:
         raise Exception,"Error setting color %i, should have been : %s, but is %s" % (c,str(okvals),str(vals))
   
   ##################################################################
   # Retrieve the color map RGB values for the given cell numbers.  #
   ##################################################################

   ##################################################################
   # Using colormap objects.                                        #  
   ##################################################################
   objs = x.listelements('colormap')                      	# show the list of line secondary objects
   c2=x.getcolormap()             	# get 'default' colormap object
   c=x.getcolormap('AMIP')             # get 'quick' colormap object
   if not vcs_legacy.issecondaryobject(c):           # check to see if it is a secondary object
      raise Exception,"Error did not get colormap as secondary obj"
   else:
      if not vcs_legacy.iscolormap(c):                  	# check to see if it is a line
         raise Exception, "Error object created is not colormap"

   a = x.listelements('colormap')                      # show list of xyvsy
   r=x.createcolormap('new')     # create colormap
   a2 = x.listelements('colormap')                      # show list of xyvsy
   if a2==a:
      raise "error cm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('colormap')                      # show list of xyvsy
   if a3!=a:
      raise Exception,"error cm not removed"
   c3=x.createcolormap('new')		# create a new colormap object from default
   x.setcolormap('new')			# change the colormap to 'new' colormap
   c3.index				# show just the colormap RGB values
   for c in colors.keys():
      vals=colors[c]
      c3.index[c]=vals                  # change color cell
      
   for c in colors.keys():
      okvals=colors[c]
      vals = x.getcolorcell(c)
      if vals!=okvals:
         raise Exception,"Error setting color %i, should have been : %s, but is %s" % (c,str(okvals),str(vals))
   
   c3.script('test.scr','w')		# save 'new' colormap as a VCS script
   c3.script('test','w')		# save 'new' colormap as a Python script
   if support.dogui is True:
      x.colormapgui()			# display the colormap GUI



   print '***************************************************************************************'
   print '******                                                                           ******'
   print '******   C O L O R M A P   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                           ******'
   print '***************************************************************************************'


if __name__=="__main__":
   test()
