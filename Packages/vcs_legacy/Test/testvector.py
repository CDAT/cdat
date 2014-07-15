# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Vector (Gv) module
#
############################################################################
#                                                                          #
# Module:       testvector module                                          #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov                                     #
#                                                                          #
# Description:  Used to test VCS's vector graphics method.                 #
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
   u=f.getslab('u',':',':',-10.,0.,0,10)# get slab u
   v=f.getslab('v',':',':',-10.,0.,0,10)# get slab v
   x=vcs_legacy.init()                         # construct vcs_legacy canvas
   
   x.plot(u, v, 'default','vector','quick',bg=bg)	# plot slab the old way
   support.check_plot(x)
   if bg==0:
      x.geometry(450,337,100,0)		# change the geometry and location
      x.flush()
      support.check_plot(x)
   
   a=x.getvector('quick')		# get 'quick' vector
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isvector(a):                 # test object 'a' if vector
         raise Exception, "Error gm is not right type"
   
   a.script('test','w')                 # save 'quick' vector as a Python script
   
   
   a.xticlabels('','')                  # remove the x-axis
   support.check_plot(x)
   a.xticlabels('*')                    # put the x-axis
   support.check_plot(x)
   
   ############################################################################
   # Change the vector scale                                                  #
   ############################################################################
   a.scale=2.0
   support.check_plot(x)
   a.scale=5.0
   support.check_plot(x)
   a.scale=1.5
   support.check_plot(x)
   a.scale=0.0
   support.check_plot(x)
   a.scale=-1.5
   support.check_plot(x)
   a.scale=-2.0
   support.check_plot(x)
   a.scale=-5.0
   support.check_plot(x)
   
   ############################################################################
   # Change the vector typeiiiiiii                                            #
   ############################################################################
   a.type=0				# same as a.type = 'arrows'
   support.check_plot(x)
   a.type=1				# same as a.type = 'barbs'
   support.check_plot(x)
##    a.type=2				# same as a.type = 'solidarrows'
##    support.check_plot(x)
##    raw_input("Done...")


   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()


 



   ############################################################################
   # Change the vector reference                                              #
   ############################################################################
   a.reference=10.
   support.check_plot(x)
   a.reference=100.
   support.check_plot(x)
   a.reference=4.
   support.check_plot(x)
   a.reference=5.
   support.check_plot(x)
   
   ############################################################################
   # Change the vector alignment                                              #
   ############################################################################
   a.alignment='head'      		# same as a.alignment=0
   support.check_plot(x)
   a.alignment='center'    		# same as a.alignment=1
   support.check_plot(x)
   a.alignment='tail'      		# same as a.alignment=2
   support.check_plot(x)
   
   ############################################################################
   # Change the vector line                                                   #
   ############################################################################
   a.line=0        			# same as 'solid'
   support.check_plot(x)
   a.line=1       			# same as 'dash'
   support.check_plot(x)
   a.line=2        			# same as 'dot'
   support.check_plot(x)
   a.line=3        			# same as 'dash-dot'
   support.check_plot(x)
   a.line=4        			# same as 'long-dash'
   support.check_plot(x)
   a.line=None        			# use default line   
   support.check_plot(x)
   
   ############################################################################
   # Change the vector line color                                             #
   ############################################################################
   a.linecolor=(77)
   support.check_plot(x)
   a.linecolor=16
   support.check_plot(x)
   a.linecolor=44  # same as a.color=(44)
   support.check_plot(x)
   a.linecolor=None
   support.check_plot(x)
   
   x.clear()                            # clear the VCS Canvas
   x.vector(u, v, a,'default',bg=bg)		# plot vector using 'default' template
   support.check_plot(x)
   
   objs =x.listelements('template')                   # get the list of templates
   t=x.createtemplate('test')           # create template 'test' from 'default' template
   if not vcs_legacy.istemplate(t):                  # test whether 't' is a template or not
      raise Exception,"Error template not created"
   else:
      a2 =x.listelements('template')                   # get the list of templates
      if objs==a2:
         raise Exception,"Error template not created or added to list"
   
   x.clear()                            # clear the VCS Canvas
   x.plot(t,a,u,v,bg=bg)                      # plot vector template 't', outline 'a', and arrays 'u':'v'
   support.check_plot(x)
   x.clear()                            # clear the VCS Canvas
   x.vector(a,u,v,t,bg=bg)                    # plot using outline 'a', array 'u':'v', and template 't'
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
   l.type ='dot'                    # change the line type
   support.check_plot(x)
   l.width=4 				# change the line size
   support.check_plot(x)
   
   a = x.listelements('vector')                      # show list of gm
   r=x.createvector('test2','quick')     # create xyvsy 'test2'
   a2 = x.listelements('vector')                      # show list of gm
   if a2==a:
      raise "error gm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('vector')                      # show list of gm
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
   print '******   V E C T O R   T E S T   C O M P L E T E D   S U C E S S F U L L Y     ******'
   print '******                                                                         ******'
   print '*************************************************************************************'

if __name__=="__main__":
   test()
