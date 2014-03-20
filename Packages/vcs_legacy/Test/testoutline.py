# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Outline (Go) module
#
############################################################################
#                                                                          #
# Module:       testoutline module                                         #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's outline graphics method.                #
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
   import vcs_legacy,cdms2 as cdms,time,os,sys,support          # import vcs_legacy and cu

   bg= support.bg
   
   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc')) # open clt file
   s=f('clt')                           # get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas
   

   a=x.createoutline('test')           # create 'test' outline
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isoutline(a):		# check for outline
         raise Exception, "Error gm is not right type"

   x.plot(s,'default','outline','test',bg=bg)# plot slab the old way
   support.check_plot(x)
   if bg==0:
      x.geometry(450,337,100,0)            # change the geometry and location
      x.flush()
      support.check_plot(x)
   
   s=f('u')	                        # get u slab
   x.clear()                            # clear the VCS Canvas
   x.plot(s,'default','outline','test',bg=bg)# plot the surface data
   support.check_plot(x)
   
   
   a.script('test','w')			# save 'test' outline as a Python script
   
   a.xticlabels('lon30','lon30')        # change the x-axis
   support.check_plot(x)
   a.xticlabels('','')                  # remove the x-axis
   support.check_plot(x)
   a.xticlabels('*')                    # put the x-axis
   support.check_plot(x)
   a.datawc(-45.0, 45.0, -90.0, 90.0)   # change the region
   support.check_plot(x)
   a.datawc(1e20,1e20,1e20,1e20)        # put the region back
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

   a.outline=([0])			# set the outline value
   support.check_plot(x)
   a.line=0        			# same as 'solid', change the line style
   support.check_plot(x)
   a.line=1        			# same as 'dash', change the line style
   support.check_plot(x)
   a.line=2        			# same as 'dot', change the line style
   support.check_plot(x)
   a.line=3        			# same as 'dash-dot', change the line style
   support.check_plot(x)
   a.line=4        			# same as 'long-dash', change the line style
   support.check_plot(x)
   a.linecolor=(77)			# change the line color
   support.check_plot(x)
   a.linecolor=16			# change the line color
   support.check_plot(x)
   a.linecolor=44  			# same as a.linecolor=(44)
   support.check_plot(x)
   a.linecolor=None			# use the default line color, black
   support.check_plot(x)
   a.line=None				# use default line style, solid black line
   support.check_plot(x)

   a.outline=([1])			# set the outline value
   support.check_plot(x)
   a.outline=([0,1])			# set the outline value
   support.check_plot(x)
   a.outline=([0])			# set the outline value
   support.check_plot(x)
   
   x.clear()                            # clear the VCS Canvas
   x.outline(s,a,'default',bg=bg)             # plot outline using 'default' template
   support.check_plot(x)
   
   objs =x.listelements('template')                   # get the list of templates
   t=x.createtemplate('test')           # create template 'test' from 'default' template
   if not vcs_legacy.istemplate(t):                  # test whether 't' is a template or not
      raise Exception,"Error template not created"
   else:
      a2 =x.listelements('template')                   # get the list of templates
      if objs==a2:
         raise Exception,"Error template not created or added to list"
   
   x.clear()				# clear the VCS Canvas
   x.plot(t,a,s,bg=bg)			# plot outline template 't', outline 'a', and array 's'
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.outline(a,s,t,bg=bg)			# plot using outline 'a', array 's', and template 't'
   support.check_plot(x)
   
   #########################################################################
   # Create line object 'l' from the default line                          #
   #########################################################################
   #########################################################################
   objs = x.listelements('line')                      	# show the list of line secondary objects
   l=x.createline('test')
   if not vcs_legacy.issecondaryobject(l):           # check to see if it is a secondary object
      raise Exception,"Error did not get line"
   else:
      if not vcs_legacy.isline(l):                  	# check to see if it is a line
         raise Exception, "Error object created is not line"
   
   #########################################################################
   # Use the create line object 'l' from above and modify the line object  #
   #########################################################################
   a.line=l				# use the line object
   support.check_plot(x)
   l.color = 44                         # change the line color
   support.check_plot(x)
   l.type ='dash'                       # change the line type
   support.check_plot(x)
   
   
   a = x.listelements('outline')                      # show list of gm
   r=x.createoutline('test2','quick')     # create xyvsy 'test2'
   a2 = x.listelements('outline')                      # show list of gm
   if a2==a:
      raise "error gm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('outline')                      # show list of gm
   if a3!=a:
      raise "error gm not removed"
   
   
   #################################################################################
   # to see how x.update and x.mode work, see testoutline.py                       #
   #################################################################################
   #x.update()                             
   #x.mode=1
   #x.mode=0
   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   O U T F I L L   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                         ******'
   print '*************************************************************************************'
   
if __name__=="__main__":
   test()
