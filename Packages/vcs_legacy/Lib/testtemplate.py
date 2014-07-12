"""
# Test Template (P) module
"""
###############################################################################
#                                                                             #
# Module:       testtemplate module                                           #
#                                                                             #
# Copyright:    2000, Regents of the University of California                 #
#               This software may not be distributed to others without        #
#               permission of the author.                                     #
#                                                                             #
# Author:       PCMDI Software Team                                           #
#               Lawrence Livermore NationalLaboratory:                        #
#               support@pcmdi.llnl.gov                                        #
#                                                                             #
# Description:  Used to test VCS's template object.                           #
#                                                                             #
# Version:      4.0                                                           #
#                                                                             #
###############################################################################
#
#
#
############################################################################
#                                                                          #
# Import: VCS  and cu modules.                                             #
#                                                                          #
############################################################################
def test():
   import vcs_legacy,cu                        # import vcs_legacy and cu

   f=cu.open('clt.nc')                  # open clt file
   s=f.getslab('clt')                   # get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas

   x.show('template')                   # show the list of templates
   t=x.createtemplate('test','AMIP')    # create template 'test' from AMIP
   x.show('template')                   # show the list of templates

   t.script('test','w')			# save test template as a Python script

   g=x.createisofill('test')           	# create isofill 'test' from 'default' isofill

   x.plot(g,s,t)			# make isofill plot
#   x.isofill(g,s,t)			# make isofill plot

   
   #############################################################################
   # Show the many different ways to show the template members (attributes)    #
   # and their values.                                                         #
   #############################################################################
   t.list()				# list the templates members
   t.list('text')			# list only text members, same as t.list('Pt')
   t.list('format')			# list only format members, same as t.list('Pf')
   t.list('xtickmarks')			# list only xtickmarks members, same as t.list('Pxt')
   t.list('ytickmarks')			# list only ytickmarks members, same as t.list('Pyt')
   t.list('xlabels')			# list only xlabels members, same as t.list('Pxl')
   t.list('ylabels')			# list only ylabels members, same as t.list('Pyl')
   t.list('boxeslines')			# list only boxeslines members, same as t.list('Pbl')
   t.list('legend')			# list only legend member, same as t.list('Pls')
   t.list('data')			# list only data member, same as t.list('Pds')
   t.list('file')			# list only file member and its values
   t.file.list()			# list only file member and its values
   t.list('mean')			# list only mean member and its values
   t.mean.list()			# list only mean member and its values

   #############################################################################
   # The screen x and y positions on the screen are normalized between 0 and 1 #
   # for both the x and y axis.                                                #
   #############################################################################
   t.mean.priority = 0			# remove the "Mean" text from the plot
   t.mean.priority = 1			# re-display the "Mean" text on the plot
   t.mean.x=0.5				# move the "Mean" text to x-axis center
   t.mean.y=0.5				# move the "Mean" text to y-axis center
   
   #############################################################################
   # Position the data in front of the "Mean" text, then move the "Mean" text  #
   # in front of the data.                                                     #
   #############################################################################
   t.data.priority = 2
   t.data.priority=3
#  The above does not work. I will fix this later when time permits.

   #############################################################################
   # Change the font representation for the "Mean" text.                       #
   # You can set the text by using text objects or text names.                 #
   #############################################################################
   tt = x.createtexttable('test')
   to = x.createtextorientation('test')
   t.mean.texttable = tt		# set texttable by using texttable object
   t.mean.textorientation = 'test'	# set textorientation by using textorientation name
   t.mean.list()                        # show the mean member and their new values
   tt.font=2				# change the font 
   to.height=40				# change the height

   #############################################################################
   # Change the legend space.                                                  #
   #############################################################################
   t.legend.list()		       	# list the legend members
   x.mode=0				# turn the automatic update off
   t.legend.x1=0.85
   t.legend.y1=0.90
   t.legend.x2=0.95
   t.legend.y2=0.16
   x.update()
#  The above does not work. I will fix this later when time permits.

   print '***************************************************************************************'
   print '******                                                                           ******'
   print '******   T E M P L A T E   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                           ******'
   print '***************************************************************************************'

if __name__=="__main__":
   test()

