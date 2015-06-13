# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test XvsY (GXY) module
#
############################################################################
#                                                                          #
# Module:       testxvsy module                                            #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's XvsY graphics method.                   #
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

   f=cdms.open(os.path.join(vcs.sample_data,'clt.nc')) # open clt file
   u=f('u')  				# get slab u
   v=f('v') 				# get slab v
   x=vcs_legacy.init()                         # construct vcs_legacy canvas
   
   a=x.createxvsy('quick')		# create 'quick' xvsy
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isxvsy(a):                   # test object 'a' if xvsy
         raise Exception, "Error gm is not right type"

   x.plot(u, v, 'default','xvsy','quick',bg=bg)	# plot slabs the old way
   support.check_plot(x)
   if bg==0:
      x.geometry(450,337,100,0)		# change the geometry and location
      x.flush()                            
      support.check_plot(x)
   
   
   a.script('test','w')                 # save 'quick' xvsy as a Python script
   
   a.xticlabels('','')                  # remove the x-axis
   support.check_plot(x)
   a.xticlabels('*')                    # put the x-axis
   support.check_plot(x)
   
   ############################################################################
   # Change the xvsy line                                                     #
   ############################################################################
   a.line=0        			# same as 'solid'
   support.check_plot(x)
   a.line=1        			# same as 'dash'
   support.check_plot(x)
   a.line=2        			# same as 'dot'
   support.check_plot(x)
   a.line=3        			# same as 'dash-dot'
   support.check_plot(x)
   a.line=4        			# same as 'long-dash'
   support.check_plot(x)
   

   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()



   ############################################################################
   # Change the xvsy line color                                               #
   ############################################################################
   a.linecolor=(77)
   support.check_plot(x)
   a.linecolor=16
   support.check_plot(x)
   a.linecolor=44  			# same as a.color=(44)
   support.check_plot(x)
   a.linecolor=None
   support.check_plot(x)
   
   ############################################################################
   # Change the xvsy marker                                                   #
   ############################################################################
   a.marker=1                        	# Same as a.marker='dot'
   support.check_plot(x)
   a.marker=2                       	# Same as a.marker='plus'
   support.check_plot(x)
   a.marker=3                       	# Same as a.marker='star'
   support.check_plot(x)
   a.marker=4                       	# Same as a.marker='circle'
   support.check_plot(x)
   a.marker=5                        	# Same as a.marker='cross'
   support.check_plot(x)
   a.marker=6                        	# Same as a.marker='diamond'
   support.check_plot(x)
   a.marker=7                        	# Same as a.marker='triangle_up'
   support.check_plot(x)
   a.marker=8                        	# Same as a.marker='triangle_down'
   support.check_plot(x)
   a.marker=9                        	# Same as a.marker='triangle_left'
   support.check_plot(x)
   a.marker=10                       	# Same as a.marker='triangle_right'
   support.check_plot(x)
   a.marker=11                       	# Same as a.marker='square'
   support.check_plot(x)
   a.marker=12                       	# Same as a.marker='diamond_fill'
   support.check_plot(x)
   a.marker=13                       	# Same as a.marker='triangle_up_fill'
   support.check_plot(x)
   a.marker=14                       	# Same as a.marker='triangle_down_fill'
   support.check_plot(x)
   a.marker=15                       	# Same as a.marker='triangle_left_fill'
   support.check_plot(x)
   a.marker=16                      	# Same as a.marker='triangle_right_fill'
   support.check_plot(x)
   a.marker=17                       	# Same as a.marker='square_fill'
   support.check_plot(x)
   a.marker=None                     	# Draw no markers
   support.check_plot(x)
   
   ############################################################################
   # Change the xvsy marker color                                             #
   ############################################################################
   a.marker='dot'
   support.check_plot(x)
   a.markercolor=16
   support.check_plot(x)
   a.markercolor=44        		# same as a.markercolor=(44)
   support.check_plot(x)
   a.markercolor=None
   support.check_plot(x)
   
   ############################################################################
   # Change the xvsy marker size                                              #
   ############################################################################
   a.markersize=5
   support.check_plot(x)
   a.markersize=55
   support.check_plot(x)
   a.markersize=10
   support.check_plot(x)
   a.markersize=100
   support.check_plot(x)
   a.markersize=300
   support.check_plot(x)
   a.markersize=None
   support.check_plot(x)
   
   x.clear()                            # clear the VCS Canvas
   x.xvsy(u, v, a,'default',bg=bg)		# plot xvsy using 'default' template
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
   
   x.clear()                            # clear the VCS Canvas
   x.plot(t,a,u,v,bg=bg)                      # plot xvsy template 't', outline 'a', and arrays 'u':'v'
   support.check_plot(x)
   x.clear()                            # clear the VCS Canvas
   x.xvsy(a,u,v,t,bg=bg)                      # plot using outline 'a', array 'u':'v', and template 't'
   support.check_plot(x)
   
   l=x.getline('red')                	# get line 'red'
   if not vcs_legacy.issecondaryobject(l):           # check to see if it is a secondary object
      raise Exception,"Error did not get line"
   else:
      if not vcs_legacy.isline(l):                  	# check to see if it is a line
         raise Exception, "Error object created is not line"
   
   ###########################################################################
   # Use the create line object 'm' from above and modify the line object    #
   ###########################################################################
   a.line=l                             # use the line object
   support.check_plot(x)
   l.color = 44                         # change the line color
   support.check_plot(x)
   l.type ='dot'                       # change the line type
   support.check_plot(x)
   l.width=4 				# change the line size
   support.check_plot(x)
   
   m=x.getmarker('red')                 # get marker 'red'
   if not vcs_legacy.issecondaryobject(m):           # check to see if it is a secondary object
      raise Exception,"Error did not get marker"
   else:
      if not vcs_legacy.ismarker(m):                  	# check to see if it is a line
         raise Exception, "Error object created is not marker"

   ###########################################################################
   # Use the create marker object 'm' from above and modify the line object  #
   ###########################################################################
   a.marker=m                           # use the marker object
   support.check_plot(x)
   m.color = 44                         # change the marker color
   support.check_plot(x)
   m.type ='square'                     # change the marker type
   support.check_plot(x)
   m.size=20                            # change the marker size
   support.check_plot(x)
   
   a = x.listelements('xvsy')                      # show list of gm
   r=x.createxvsy('test2','quick')     # create xyvsy 'test2'
   a2 = x.listelements('xvsy')                      # show list of gm
   if a2==a:
      raise "error gm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('xvsy')                      # show list of gm
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
   print '******   X v s Y   T E S T   C O M P L E T E D   S U C E S S F U L L Y         ******'
   print '******                                                                         ******'
   print '*************************************************************************************'

if __name__=="__main__":
   test()
