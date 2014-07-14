# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Display Plot (Dp) module
#
############################################################################
#                                                                          #
# Module:	testdisplayplot module				   	   #
#									   #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.				   #
#									   #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#									   #
# Description:	Used to test VCS's display plots.		   	   #
#									   #
# Version:	4.0							   #
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
   import vcs_legacy,cdms2 as cdms,time,os,sys,support			# import vcs_legacy and cdms

   bg=support.bg
   
   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   s1=f.getslab('clt')			# get slab clt
   s2=f.getslab('clt',4,4,-45,45,-90,90)
   s3=f.getslab('clt',6,6,0,90,0,180)
   x=vcs_legacy.init()				# construct vcs_legacy canvas
   
   p1=x.plot(s1,bg=bg)			# plot slab the old way
   support.check_plot(x)
   x.plot(s2,bg=bg)				# plot slab the old way
   support.check_plot(x)
   
   p2=x.getplot('dpy_plot_2')		# get display plot s2

   if not vcs_legacy.queries.isplot(p1):			# check for display plot object
      raise exception, "Error, p1 not a display!"


   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()


   p2.name='p2_name'			# change display plot's (p2) name
   p2.off=1				# undisplay display plot (p2)
   support.check_plot(x)
   p2.off=0				# redisplay display plot (p2)
   support.check_plot(x)

   p1.priority=1			# bring display plot (p1) to the front
   support.check_plot(x)

   
   p2.off=1				# undisplay display plot (p2)
   p1.template='default_dud'		# change display plot (p1) template
   support.check_plot(x)

   x.mode=0				# turn atomatic update off
   p1.template='default'		# change display plot (p1) template
   p1.g_type='isoline'			# change display plot (p1) graphics type
   support.check_plot(x)
   x.update()				# view changes now
   support.check_plot(x)

   x.mode=1				# turn atomatic update mode back on
   p1.g_type='isofill'			# change display plot (p1) graphics type
   support.check_plot(x)
   p1.g_name='quick'                    # change display plot (p1) graphics name
   support.check_plot(x)

   p1.array=['plot_2']			# change display plot (p1) array object
   support.check_plot(x)
   p1.array=['plot_1']			# change display plot (p1) array object
   support.check_plot(x)
   p1.array=[s3]			# change display plot (p1) array object
   support.check_plot(x)


   #########################################################################
   #								           #
   # Produce an overlay plot.                                              #
   #								           #
   #########################################################################
   x.mode=0				# turn atomatic update mode back on
   p1.array=['plot_1']			# change display plot (p1) array object
   p2.off=0                             # redisplay display plot (p2)
   p2.priority=2			# bring display plot (p2) to the front
   p2.array=[s1]                  # change display plot (p2) array object
   p2.g_type='isoline'                  # change display plot (p2) graphics type
   p2.g_name='default'                  # change display plot (p1) graphics name
   x.update()				# view changes now
   support.check_plot(x)

   x.mode=1				# turn atomatic update mode back on
   x.removeobject(p1)			# remove the display plot (p1)
   support.check_plot(x)

   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   D I S P L A Y   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                         ******'
   print '*************************************************************************************'
   
if __name__=="__main__":
   test()
