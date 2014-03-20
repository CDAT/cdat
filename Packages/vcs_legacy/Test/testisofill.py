# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Isofill (Gfi) module
#
############################################################################
#                                                                          #
# Module:       testisofill module  	                                   #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Author:       Dean N. Williams, Lawrence Livermore National Laboratory   #
#               williams13@llnl.gov                                        #
#                                                                          #
# Description:  Used to test VCS's isofill graphics method.                #
#                                                                          #
# Version:      4.0                                                        #
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
   import vcs_legacy,cdms2 as cdms,time,os,sys,support                      # import vcs_legacy and cdms

   bg = support.bg

   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   s=f('clt')                           # get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas
   
   x.plot(s,'default','isofill','quick',bg=bg)# plot slab the old way
   support.check_plot(x)

   # Check the legend
   g=x.createisofill('vcs_legacymoduletest')
   x.clear()				# clear the VCS Canvas
   x.plot(s,g,bg=bg)				# plot slab the new way
   support.check_plot(x)
   g.legend = (10, 30, 70, 120)
   support.check_plot(x)
   g.legend={0:'test string1', 50:'test string2', 100:'test string3'}
   support.check_plot(x)
   g.legend = None
   support.check_plot(x)

   if bg==0:
      x.geometry(450,337,100,0)		# change the geometry and location
      x.flush()
      support.check_plot(x)
   
   # open and plot a missing data with isofill
   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','tas_cru_1979.nc'))
   s=f('tas',longitude=(-180, 180),latitude=(-90,90))+273.15
   x.clear()				# clear the VCS Canvas
   x.plot(s,'default','isofill','quick',bg=bg)# plot missing value slabe the old way
   support.check_plot(x)
   
   a=x.getisofill('quick') 		# get 'quick' isofill graphics method
   if not vcs_legacy.isgraphicsmethod(a):            # test object 'a' for graphics method
      raise Exception, "Error did not retrieve the gm"
   else:
      if not vcs_legacy.isisofill(a):		# check for isofill
         raise Exception, "Error gm is not right type"
   
   a.script('test','w')			# save 'quick' isofill as a Python script
   
   x.setcolormap("AMIP")		# change the colormap from default to AMIP
   
   a.missing=241			# change the missing background color to black
   support.check_plot(x)
   


   if not '--extended' in sys.argv:
        print '\n************* PARTIAL TEST *****************'
        print 'FOR COMPLETE TEST OF THIS MODULE USE '
        print '   -F (--full) or -E (--extended) option'
        print '************* PARTIAL TEST *****************\n'
        sys.exit()


   
   a.xticlabels('lon30','lon30')	# change the x-axis
   support.check_plot(x)
   a.xticlabels('','')			# remove the x-axis
   support.check_plot(x)
   a.xticlabels('*')			# put the x-axis
   support.check_plot(x)
   a.datawc(-45.0, 45.0, -90.0, 90.0)	# change the region
   support.check_plot(x)
   a.datawc(1e20,1e20,1e20,1e20)	# put the region back
   support.check_plot(x)
   
   a.levels=([0,220],[230,240],[250,260])	# change the isofill levels
   support.check_plot(x)
   a.levels=([0,220,225,230,235,240],[230,240],[250,260])	# change the isofill levels
   support.check_plot(x)
   a.levels=([0,220,225,230,235,240],)	# change the isofill levels
   support.check_plot(x)
   a.levels=([0,220,225,230,235,240,245,250])	# change the isofill levels
   support.check_plot(x)
   a.levels=[0,220,225,230,235,240] 	# change the isofill levels
   support.check_plot(x)
   a.levels=(0.0,220.0,225.0,230.0,235.0,240.0,250.0)		# change the isofill levels
   support.check_plot(x)
   a.levels=([1e20],)			# change back to default settings
   support.check_plot(x)
   a.levels=(0,220,225,230,235,240,250,260,270)	# change the isofill levels
   support.check_plot(x)


   #########################################################################################
   # Below will produce an error. Later, if needed, I will add this functionality.	   #
   #a.levels=('0','20','25','30')			# this will produce an error	   #
   #########################################################################################
   
   a.ext_1='y'				# add the extended legend arrow to the left
   support.check_plot(x)
   a.ext_1='n'				# remove the extended legend arrow to the left
   support.check_plot(x)
   a.ext_2='y'				# add the extended legend arrow to the right
   support.check_plot(x)
   a.ext_2='n'				# remove the extended legend arrow to the right
   support.check_plot(x)
   a.exts('y','y')			# add the extended legend arrow to left and right
   support.check_plot(x)
   a.exts('n','n')			# remove the extended legend arrow to left and right
   support.check_plot(x)
   
   a.fillareastyle='pattern'		# change the fill style to pattern
   support.check_plot(x)
   a.fillareastyle='hatch'		# change the fill style to hatch
   support.check_plot(x)
   a.fillareaindices=([1,3,5,6,9,18])	# set the hatch index patterns
   support.check_plot(x)
   
   a.fillareacolors=([22,33,44,55,66,77]) 	# set the fill area color indices
   support.check_plot(x)
   a.fillareacolors=None			# use default color indices
   support.check_plot(x)
   a.fillareastyle='solid'			# change the fill style back to solid
   support.check_plot(x)
   
   x.clear()				# clear the VCS Canvas
   x.plot(s,a,'default',bg=bg)		# plot isofill using 'default' template
   support.check_plot(x)
   
   objs =x.listelements('template')                   # get the list of templates
   t=x.createtemplate('test')           # create template 'test' from 'default' template
   if not vcs_legacy.istemplate(t):                  # test whether 't' is a template or not
      raise Exception,"Error template not created"
   else:
      a2 =x.listelements('template')                   # get the list of templates
      if objs==a2:
         raise Exception,"Error template not created or added to list"
   
   objs = x.listelements('fillarea')                      	# show the list of fillarea secondary objects
   f=x.getfillarea('AuTo_1')                	# get fillarea 'red'
   if not vcs_legacy.issecondaryobject(f):           # check to see if it is a secondary object
      raise Exception,"Error did not get fillarea"
   else:
      if not vcs_legacy.isfillarea(f):                  	# check to see if it is a fillarea
         raise Exception, "Error object created is not fillarea"
   
   a.levels=(220,225,230,235,240,250,260,270,280,290,300,310)	# change the isofill levels
   x.clear()				# clear the VCS Canvas
   x.plot(a,t,s,bg=bg)			# plot array using isofill 'a' and template 't'
   support.check_plot(x)
   a.fillareaindices=(3,4,7,9,11)	# set the indices
   support.check_plot(x)
   a.fillareaindices=(f,f,f,f,f,f)	# set the indices using the fillarea object
   support.check_plot(x)
   a.fillareaindices=(f,2,4,7)		# reset the indices using the fillarea object
   support.check_plot(x)
   a.fillareaindices=(7,f,f,f,8)	# resett the indices using the fillare object
   support.check_plot(x)
   
   f.color=44				# change the fillarea object's color
   support.check_plot(x)
   f.style='hatch'			# change the fillarea object's fill style
   support.check_plot(x)
   
   x.scriptobject(a,'test')		# save 'quick' isofill as a Python script
   x.scriptobject(f,'test')		# save 'def37' fill area as a Python script
   
   a = x.listelements('isofill')                      # show list of gm
   r=x.createisofill('test2','quick')     # create xyvsy 'test2'
   a2 = x.listelements('isofill')                      # show list of gm
   if a2==a:
      raise "error gm not created or not added to list"
   x.removeobject(r)                    # remove xyvsy 'test2'
   a3 = x.listelements('isofill')                      # show list of gm
   if a3!=a:
      raise "error gm not removed"
   
   #################################################################################
   # to see how x.update and x.mode work, see testisofill.py			   #
   #################################################################################
   #x.update()				
   #x.mode=1
   #x.mode=0
   print '*************************************************************************************'
   print '******                                                                         ******'
   print '******   I S O F I L L   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                         ******'
   print '*************************************************************************************'

if __name__=="__main__":
   test()
