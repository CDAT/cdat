# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Scatter (GSp) module
#
############################################################################
#                                                                          #
# Module:       testscatter module                                         #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's scatter graphics method.                #
#                                                                          #
# Version:      5.0                                                        #
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
   import vcs_legacy,cdms2,time,os,sys,support                # import vcs_legacy and cu
   bg=support.bg
   f=cdms2.open(os.path.join(cdms2.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   u=f('u')                             # get slab u
   v=f('v')                             # get slab v
   x=vcs_legacy.init()                         # construct vcs_legacy canvas
   
   x.plot(u, v, 'default','scatter','quick',bg=bg)	# plot slab the old way
   support.check_plot(x)
   if bg==0:
      x.mode=1
      x.geometry(450,337,100,0)		# change the geometry and location
      x.flush()
      support.check_plot(x)

   
   a=x.getscatter('quick')		# get 'quick' scatter
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isscatter(a):                # test object 'a' if scatter
         raise Exception, "Error gm is not right type"
   
   a.script('test','w')                 # save 'quick' scatter as a Python script
   
   print 'yepp'
   a.xticlabels('','')                  # remove the x-axis
   support.check_plot(x)
   a.xticlabels('*')                    # put the x-axis
   support.check_plot(x)
   
   ############################################################################
   # Change the scatter marker type                                           #
   ############################################################################
   #a.marker=0
   a.marker=1				# same as a.marker='dot'
   support.check_plot(x)
   a.marker=2				# same as a.marker='plus'
   support.check_plot(x)
   a.marker=3				# same as a.marker='star'
   support.check_plot(x)
   a.marker=4				# same as a.marker='circle'
   support.check_plot(x)
   a.marker=5				# same as a.marker='cross'
   support.check_plot(x)
   a.marker=6				# same as a.marker='diamond'
   support.check_plot(x)
   a.marker=7				# same as a.marker='triangle_up'
   support.check_plot(x)
   a.marker=8				# same as a.marker='triangle_down'
   support.check_plot(x)
   a.marker=9				# same as a.marker='triangle_left'
   support.check_plot(x)
   a.marker=10				# same as a.marker='triangle_right'
   support.check_plot(x)
   a.marker=11				# same as a.marker='square'
   support.check_plot(x)
   a.marker=12				# same as a.marker='diamond_fill'
   support.check_plot(x)
   a.marker=13				# same as a.marker='triangle_up_fill'
   support.check_plot(x)
   a.marker=14				# same as a.marker='triangle_down_fill'
   support.check_plot(x)
   a.marker=15				# same as a.marker='triangle_left_fill'
   support.check_plot(x)
   a.marker=16				# same as a.marker='triangle_right_fill'
   support.check_plot(x)
   a.marker=17				# same as a.marker='square_fill'
   support.check_plot(x)
   
   ############################################################################
   # Change the scatter marker size                                           #
   ############################################################################
   a.markersize=5
   support.check_plot(x)
   a.markersize=55
   support.check_plot(x)
   a.markersize=100
   support.check_plot(x)
   a.markersize=300
   support.check_plot(x)
   a.markersize=15
   support.check_plot(x)
   
   ############################################################################
   # Change the scatter marker color                                          #
   ############################################################################
   a.markercolor=(77)
   support.check_plot(x)
   a.markercolor=16
   support.check_plot(x)
   a.markercolor=44			# same as a.markercolor=(44)
   support.check_plot(x)
   
   ############################################################################
   # Change the scatter settings to default                                   #
   ############################################################################
   a.markercolor=None
   support.check_plot(x)
   a.markersize=None
   support.check_plot(x)
   a.marker=None
   support.check_plot(x)
   
   a.marker=1                           # same as a.marker='dot'
   support.check_plot(x)
   
   x.clear()                            # clear the VCS Canvas
   x.scatter(u, v, a,'default',bg=bg)		# plot scatter using 'default' template
   support.check_plot(x)
   
   t=x.createtemplate('test')           # create template 'test' from 'default' template
   
   x.clear()                            # clear the VCS Canvas
   x.plot(t,a,u,v,bg=bg)                      # plot scatter template 't', outline 'a', and arrays 'u':'v'
   support.check_plot(x)
   x.clear()                            # clear the VCS Canvas
   x.scatter(a,u,v,t,bg=bg)                   # plot using outline 'a', array 'u':'v', and template 't'
   support.check_plot(x)
   
   m=x.getmarker('red')                	# get marker 'red'
   if vcs_legacy.issecondaryobject(m):           # check to see if it is a secondary object
      if not vcs_legacy.ismarker(m):                 # check to see if it is a fill area
         raise Exception, "Error: this is not a marker object."
   else:
      raise Exception, "Error: this is not a sceondary object."
   
   ###########################################################################
   # Use the create marker object 'm' from above and modify the line object  #
   ###########################################################################
   a.marker=m                           # use the marker object
   support.check_plot(x)
   m.color = 44                         # change the marker color
   support.check_plot(x)
   m.type ='square'                     # change the marker type
   support.check_plot(x)
   m.size=20				# change the marker size
   support.check_plot(x)
   
   e = x.listelements('scatter')         # show list of scatter
   r=x.createscatter('test2','quick')   # create scatter 'test2'
   e2 = x.listelements('scatter')         # show list of scatter
   if e2==e:
      raise Exception,"Error new scatter does not appear in list"
   e3=list(e)
   e3.append("test2")
   e3.sort()
   if e3!=e2:
      raise "Error new scatter created but not matching old list of scatter"
   x.removeobject(r)                    # remove scatter 'test2'
   e4 = x.listelements('scatter')         # show list of scatter
   if e4!=e:
      raise Exception,"Error method not removed"
   
   #################################################################################
   # to see how x.update and x.mode work, see testoutline.py                       #
   #################################################################################
   #x.update()
   #x.mode=1
   #x.mode=0
   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   S C A T T E R   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                         ******'
   print '*************************************************************************************'

if __name__=="__main__":
   test()
