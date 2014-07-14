# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Boxfill (Gfb) module
#
############################################################################
#                                                                          #
# Module:	testboxfill module				   	   #
#									   #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.				   #
#									   #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#									   #
# Description:	Used to test VCS's boxfill graphics method.		   #
#                                                                          #
#               run with "-i" option:   python -i testboxfill.py           #
#                                                                          #
#               This function now has three different types:               #
#                    linear - (DEFAULT) VCS will use compute or list       #
#                                 legend values                            #
#                    log10  - VCS will plot the data using log10           #
#                    custom - VCS will compute legend using custom         #
#                                 values and display legend values evenly  #
#									   #
# Version:	4.0							   #
#                                                                          #
############################################################################
#
#
#
############################################################################
#                                                                          #
# Import: VCS  and cdms modules.                                           #
#                                                                          #
############################################################################
def test():
   import vcs_legacy,cdms2 as cdms,time,os,sys,support		# import vcs_legacy and cdms
   bg=support.bg

   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   s=f('clt')			        # get slab clt
   x=vcs_legacy.init()				# construct vcs_legacy canvas
   
   x.plot(s,'default','boxfill','quick',bg=bg)# plot slab the old way
   #support.check_plot(x)
   if bg==0:
      x.geometry(450,337)                  # change the geometry
      x.flush()
      #support.check_plot(x)
   
   a=x.getboxfill('quick') 		# get 'quick' boxfill graphics method
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isboxfill(a):		# check for boxfill
         raise Exception, "Error gm is not right type"

   # Change boxfill's legend
   a.legend={0:'Great',20:'Outstanding',40:'Wonderful',60:'Happy',80:'Exciting!',100:'Best'}
   support.check_plot(x)
   a.legend={0:'100',20:'80',40:'60',60:'40',80:'20',100:'0'}
   support.check_plot(x)
   a.legend={0:'Blue',20:'Green',40:'Yellow',60:'Red',80:'Burgundy',100:'Magenta'}
   support.check_plot(x)
   a.legend=(90)
   support.check_plot(x)
   a.legend=[10,90]
   support.check_plot(x)
   a.legend=(1.5,45.8,89.7)
   support.check_plot(x)
   a.legend=None
   support.check_plot(x)

   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()

   a.color_1=50				# change color_1 index attribute
   support.check_plot(x)
   
   a.xticlabels('lon30','lon30')	# change xlabels attribute
   support.check_plot(x)
   a.xticlabels('','')			# change remove xlables from plot
   support.check_plot(x)
   a.datawc(-45.0, 45.0, -90.0, 90.0) 	# change region
   support.check_plot(x)
   a.datawc(1e20,1e20,1e20,1e20)	# change region back
   support.check_plot(x)
   a.xticlabels('*')			# change attribute labels back
   support.check_plot(x)
   
   x.mode=0				# turn atomatic update off
   a.color_1=100			# change color_1 attribute
   a.color_2=200			# change color_2 index value
   a.xticlabels('lon30','lon30')	# change attribute
   a.yticlabels('','')			# change y-labels off attribute
   a.datawc(-45.0, 45.0, -90.0, 90.0) 	# change region
   support.check_plot(x)
   x.update()				# view changes now
   support.check_plot(x)
   
   # Test log10 boxfill plot
   x.clear()				# clear the VCS Canvas
   a.boxfill_type = 'log10'             # Change the Boxfill type to log10
   a.datawc(1e20,1e20,1e20,1e20) 	# change region
   a.level_1=1e20			# change level_1
   a.level_2=1e20			# change level_2
   a.color_1=16				# change color_1 attribute
   a.color_2=239			# change color_2 index value
   x.boxfill(s,a,bg=bg)		        # plot using default template
   support.check_plot(x)


   # Test custom boxfill plot
   x.clear()				# clear the VCS Canvas
   a.boxfill_type = 'custom'            # Change the Boxfill type to custom 
   a.levels=(0,20,35,40,75,100)         # Set the custom ranges
   a.fillareacolors=(16,25,38,55,100,166) # Set the color indices
   x.boxfill(s,a,bg=bg)		        # plot using default template
   support.check_plot(x)

   # Test legend with fillareacolors boxfill plot
   x.clear()				# clear the VCS Canvas
   a.boxfill_type = 'linear'            # Change the Boxfill type back to linear 
   a.fillareacolors=(254,)           # Set the color indices
   a.level_1=0				# change level_1
   a.level_2=0				# change level_2
   x.boxfill(s,a,bg=bg)		        # plot using default template
   support.check_plot(x)
   
   a.script('test','w')			# save 'quick' boxfill as a Python script
   
   x.mode=1				# turn atomatic update mode back on
   a.color_1=16				# change color_1 attribute
   support.check_plot(x)
   a.color_2=239			# change color_2 index value
   support.check_plot(x)
   a.level_1=20				# change level_1
   support.check_plot(x)
   a.level_2=80				# change level_2
   support.check_plot(x)
   a.datawc(1e20,1e20,1e20,1e20)	# change region back
   support.check_plot(x)
   a.yticlabels('*')                    # change y-labels attribute
   support.check_plot(x)
   
   x.scriptobject(a,'test', 'a')	# append 'quick' to the existing file 
   a.script('test.scr','w')		# save 'quick' as a VCS script file
   
   objs =x.listelements('template')                   # get the list of templates
   t=x.createtemplate('test')           # create template 'test' from 'default' template
   if not vcs_legacy.istemplate(t):                  # test whether 't' is a template or not
      raise Exception,"Error template not created"
   else:
      a2 =x.listelements('template')                   # get the list of templates
      if objs==a2:
         raise Exception,"Error template not created or added to list"
   
   x.clear()				# clear the VCS Canvas
   x.boxfill(s,a,'default',bg=bg)		# plot using default template
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.boxfill(a,'default',s,bg=bg)		# plot using default template, but, reverse the order
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.boxfill(s,a,t,bg=bg)			# plot using template 'test'
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.boxfill(a,s,t,bg=bg)			# plot using template 'test', but reverse the objects
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.boxfill(t,a,s,bg=bg)			# plot using template 'test', but reverse the objects
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   
   x.plot(t,a,s,bg=bg)			# plot using the new way
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.plot(a,t,s,bg=bg)			# plot using the new way
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.plot(s,t,a,bg=bg)			# plot using the new way
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.plot('default',a,s,bg=bg)		# plot using the new way
   support.check_plot(x)
   x.clear()				# clear the VCS Canvas
   x.plot('default',s,bg=bg)			# plot using the new way
   support.check_plot(x)
   
   a = x.listelements('boxfill')                      # show list of xyvsy
   r=x.createboxfill('test2','quick')     # create xyvsy 'test2'
   a2 = x.listelements('boxfill')                      # show list of xyvsy
   if a2==a:
      raise "error gm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('boxfill')                      # show list of xyvsy
   if a3!=a:
      raise "error gm not removed"

   a=x.getboxfill('quick')              # get 'quick' boxfill graphics method
   x.clear()
   x.plot(s,a,bg=bg)
   support.check_plot(x)
   if support.dogui:
      x.graphicsmethodgui('boxfill','quick')	# display the boxfill graphics method GUI
      raw_input("Press enter when done with gui testing")
   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   B O X F I L L   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                         ******'
   print '*************************************************************************************'
   

if __name__=="__main__":
   test()
