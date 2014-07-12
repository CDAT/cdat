# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Template (P) module
#
############################################################################
#                                                                          #
# Module:       testtemplate module                                        #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's template object.                        #
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
   import vcs_legacy,cdms2 as cdms,time,os,sys,support 		# import vcs_legacy and cdms
   bg=support.bg

   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc')) # open clt file
   s=f('clt')                   	# get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas

   tt=x.createtext()
   

   a = x.listelements('template')                   # show the list of templates
   t=x.createtemplate('test','ASD')    # create template 'test' from ASD

   t.ylabel1.texttable=tt
   t.ylabel1.textorientation=tt
   
   a2 = x.listelements('template')                   # show the list of templates
   if a2==a:
      raise Excpetion,"Error, template not added to list"
   if not vcs_legacy.istemplate(t):
      raise Exception,"Error obj created is not a template!"
   
   t.script('test','w')			# save test template as a Python script

   g=x.createisofill('test')           	# create isofill 'test' from 'default' isofill

   x.plot(g,s,t,bg=bg)			# make isofill plot
   support.check_plot(x)
   
   #############################################################################
   # Show the many different ways to show the template members (attributes)    #
   # and their values.                                                         #
   #############################################################################
##    t.list()				# list the templates members
##    t.list('text')			# list only text members, same as t.list('Pt')
##    t.list('format')			# list only format members, same as t.list('Pf')
##    t.list('xtickmarks')			# list only xtickmarks members, same as t.list('Pxt')
##    t.list('ytickmarks')			# list only ytickmarks members, same as t.list('Pyt')
##    t.list('xlabels')			# list only xlabels members, same as t.list('Pxl')
##    t.list('ylabels')			# list only ylabels members, same as t.list('Pyl')
##    t.list('boxeslines')			# list only boxeslines members, same as t.list('Pbl')
##    t.list('legend')			# list only legend member, same as t.list('Pls')
##    t.list('data')			# list only data member, same as t.list('Pds')
##    t.list('file')			# list only file member and its values
##    t.file.list()			# list only file member and its values
##    t.list('mean')			# list only mean member and its values
##    t.mean.list()			# list only mean member and its values

   #############################################################################
   # The screen x and y positions on the screen are normalized between 0 and 1 #
   # for both the x and y axis.                                                #
   #############################################################################
   t.mean.priority = 0			# remove the "Mean" text from the plot
   support.check_plot(x)
   t.mean.priority = 1			# re-display the "Mean" text on the plot
   support.check_plot(x)
   t.mean.x=0.5				# move the "Mean" text to x-axis center
   support.check_plot(x)
   t.mean.y=0.5				# move the "Mean" text to y-axis center
   support.check_plot(x)
   


   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()


 
   #############################################################################
   # Position the data in front of the "Mean" text, then move the "Mean" text  #
   # in front of the data.                                                     #
   #############################################################################
   t.data.priority = 2
   support.check_plot(x)
   t.data.priority=3
   support.check_plot(x)
#  The above does not work. I will fix this later when time permits.
# seems to work now 2007-07-16
   t.data.priority=1 # back to normal
   support.check_plot(x)

   #############################################################################
   # Change the font representation for the "Mean" text.                       #
   # You can set the text by using text objects or text names.                 #
   #############################################################################
   tt = x.createtexttable('test')
   to = x.createtextorientation('test')
   t.mean.texttable = tt		# set texttable by using texttable object
   support.check_plot(x)
   t.mean.textorientation = 'test'	# set textorientation by using textorientation name
   support.check_plot(x)
   t.mean.list()                        # show the mean member and their new values
   tt.font=2				# change the font 
   support.check_plot(x)
   to.height=40				# change the height
   support.check_plot(x)

   #############################################################################
   # Change the legend space.                                                  #
   #############################################################################
   t.legend.list()		       	# list the legend members
   x.mode=0				# turn the automatic update off
   t.legend.x1=0.85
   support.check_plot(x)
   t.legend.y1=0.90
   support.check_plot(x)
   t.legend.x2=0.95
   support.check_plot(x)
   t.legend.y2=0.16
   support.check_plot(x)
   x.update()
   support.check_plot(x)
#  The above does not work. I will fix this later when time permits.

   print '***************************************************************************************'
   print '******                                                                           ******'
   print '******   T E M P L A T E   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                           ******'
   print '***************************************************************************************'

if __name__=="__main__":
   test()

