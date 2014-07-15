# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Isoline (Gi) module
#
############################################################################
#                                                                          #
# Module:       testisoline module                                         #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's isoline graphics method.                #
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
   import vcs_legacy,cdms2 as cdms,time,os,sys,support          # import vcs_legacy and cu
   bg=support.bg
   
   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc')) # open clt file
   s=f('clt')                   	# get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas
   
   x.plot(s,'default','isoline','quick',bg=support.bg)# plot slab the old way
   support.check_plot(x)
   if bg == 0:
      x.geometry(450,337,0,0)		# change geometry and location
      support.check_plot(x)
      x.geometry(900,675,10,0)		# change geometry and location
      support.check_plot(x)
      x.flush()
      support.check_plot(x)

   
   a=x.getisoline('quick') 		# get 'quick' isoline graphics method
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isisoline(a):		# check for isoline
         raise Exception, "Error gm is not right type"
   
   a.script('test','w')                 # save 'quick' isoline as a Python script
   
   a.xticlabels('lon30','lon30')	# change the x-axis
   support.check_plot(x)
   a.xticlabels('','')                  # remove the x-axis
   support.check_plot(x)
   a.xticlabels('*')                    # put the x-axis back
   support.check_plot(x)
   a.datawc(-45.0, 45.0, -90.0, 90.0)   # change the region
   support.check_plot(x)
   a.datawc(1e20,1e20,1e20,1e20)        # put the region back
   support.check_plot(x)
	
 
   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()

  
   #########################################################################
   # Set the isoline level vales	     				   #
   #########################################################################
   a.level=([20,0.0],[30,0],[50,0],[60,0])	# change the isoline values
   support.check_plot(x)
   a.level=[[20,0.0],[30,0],[50,0]]	# change the isoline values
   support.check_plot(x)
   a.level=((20,0.0),(30,0),(50,0),(60,0),(70,0)) # change the isoline values
   support.check_plot(x)
   a.level=(25,35,45,55)		# change the isoline values
   support.check_plot(x)
   a.level=[(22,33,44,55,66)]		# change the isoline values
   support.check_plot(x)
   a.level=[(23,32,45,50,76),]		# change the isoline values
   support.check_plot(x)
   a.level=[0]      			# same as a.level=(0,)
   support.check_plot(x)
   a.level=[[0,1e20]] 			# same as a.level=((0,1e20),), use default settings
   support.check_plot(x)

   
   #########################################################################
   # Turn on and off the isoline level labels  				   #
   #########################################################################
   a.label='y'       			# same as a.label=1
   support.check_plot(x)
   a.label='n'       			# same as a.label=0
   support.check_plot(x)
   
   #########################################################################
   # Set the line style and line color        		 		   #
   #########################################################################
   a.level=((20,0.0),(30,0),(50,0),(60,0),(70,0)) # change the isoline values
   support.check_plot(x)
   a.line=[1,3,0,4]			# same as a.line=(1,3,0,4)
   support.check_plot(x)
   a.line=(['dash','long-dash','solid'])# same as a.line=([2,4,0])
   support.check_plot(x)
   a.line=[2,4,1,3,2,0]
   support.check_plot(x)
   a.linecolors=([22,33,44,55,66,77])	# change the line color
   support.check_plot(x)
   a.linecolors=(16,19,33,44)		# change the line color
   support.check_plot(x)
   a.linecolors=None			# use the default line color
   support.check_plot(x)
   a.line=None				# use the default line style, which is solid
   support.check_plot(x)
   
   #########################################################################
   # Set the text font and text color         		 		   #
   #########################################################################
   a.label='y'                          # same as a.label=1
   support.check_plot(x)
   a.text=(1,2,3,4,5,6,7,8,9)		# select fonts from 1 through 9
   support.check_plot(x)
   a.text=[9,8,7,6,5,4,3,2,1]
   support.check_plot(x)
   a.text=([1,3,5,6,9,2])
   support.check_plot(x)
   a.textcolors=([22,33,44,55,66,77])	# set the text color
   support.check_plot(x)
   a.textcolors=(16,19,33,44)
   support.check_plot(x)
   a.textcolors=None			# use default text color, black
   support.check_plot(x)
   a.text=None				# use default font, 1
   support.check_plot(x)
   
   #########################################################################
   # Create template 'test' from the default template			   #
   #########################################################################
   objs =x.listelements('template')                   # get the list of templates
   t=x.createtemplate('test')           # create template 'test' from 'default' template
   if not vcs_legacy.istemplate(t):                  # test whether 't' is a template or not
      raise Exception,"Error template not created"
   else:
      a2 =x.listelements('template')                   # get the list of templates
      if objs==a2:
         raise Exception,"Error template not created or added to list"
   
   #########################################################################
   # Create line object 'l' from the default line    			   #
   #########################################################################
   l=x.createline('test')
   objs = x.listelements('line')                      	# show the list of line secondary objects
   if not vcs_legacy.issecondaryobject(l):           # check to see if it is a secondary object
      raise Exception,"Error did not get line"
   else:
      if not vcs_legacy.isline(l):                  	# check to see if it is a line
         raise Exception, "Error object created is not line"
   
   x.clear()                            # clear the VCS Canvas
   x.isoline(s,a,t,bg=support.bg)			# plot the array using the template and isoline object
   support.check_plot(x)
   x.clear()                            # clear the VCS Canvas
   x.plot(t,a,s,bg=support.bg)			# plot again using the new way
   support.check_plot(x)
   
   #########################################################################
   # Use the create line object 'l' from above and modify the line object  #
   #########################################################################
   a.line=[1,3,0,4]        		# same as a.line=(1,3,0,4)
   support.check_plot(x)
   a.line=([2,4,0])        		# same as a.line=(['dash', 'long-dash', 'solid'])
   support.check_plot(x)
   a.line=(l,4,l,0)			# use the line object
   support.check_plot(x)
   a.line=(l,3,4,2,0)
   support.check_plot(x)
   l.color = 44				# change the line color
   support.check_plot(x)
   l.type ='dash'			# change the line type
   support.check_plot(x)
   
   #########################################################################
   # Create the three types of text objects                                #
   #########################################################################
   tc = x.createtextcombined('testc','std', 'testc','7left')
   if not vcs_legacy.istextcombined(tc):
      raise Exception,"Error not textcombined!"
   
   tt = x.createtexttable('testt', 'default')
   if not vcs_legacy.istexttable(tt):
      raise Exception,"Error not texttable"
   
   to = x.createtextorientation('testo')
   if not vcs_legacy.istextorientation(to):
      raise Exception,"Error not textorientation"
   
   #########################################################################
   # Use the text objects in the isoline plot                              #
   #########################################################################
   a.label='y'				# make sure that the labels are turn on
   support.check_plot(x)
   a.text=([1,3,5,6,9,2])		# set the font
   support.check_plot(x)
   a.text=([tc,tt,to,6,9,2])		# use the created text objects and fonts
   support.check_plot(x)
   
   #########################################################################
   # Change the text object values                                         #
   #########################################################################
   tc.font = 3				# changing isoline level 20
   support.check_plot(x)
   tc.height=15
   support.check_plot(x)
   tc.angle=180
   support.check_plot(x)
   tc.color=242
   support.check_plot(x)
   tt.font=2				# changing isoline level 30
   support.check_plot(x)
   tt.spacing=20
   support.check_plot(x)
   to.height=15				# changing isoline level 50
   support.check_plot(x)
   to.path='down'
   support.check_plot(x)
   
   a.text=None				# use default font, which is font 1
   support.check_plot(x)
   a.line=None				# use default line, which is solid
   support.check_plot(x)
   
   a = x.listelements('isoline')                      # show list of gm
   r=x.createisoline('test2','quick')     # create xyvsy 'test2'
   a2 = x.listelements('isoline')                      # show list of gm
   if a2==a:
      raise "error gm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('isoline')                      # show list of gm
   if a3!=a:
      raise "error gm not removed"
   
   #################################################################################
   # to see how x.update and x.mode work, see testisoline.py                       #
   #################################################################################
   #x.update()
   #x.mode=1
   #x.mode=0
   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   I S O L I N E   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                         ******'
   print '*************************************************************************************'
   
if __name__=="__main__":
   test()
