# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# Test Hardcopy module
#
############################################################################
#                                                                          #
# Module:       testhardcopy module                                        #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Authors:      PCMDI Software Team                                        #
#               Lawrence Livermore NationalLaboratory:                     #
#               support@pcmdi.llnl.gov
#                                                                          #
# Description:  Used to test VCS's hard copy.                		   #
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
   import vcs_legacy,cdms2 as cdms,os,sys               # import vcs_legacy and cu

   f=cdms.open(os.path.join(cdms.__path__[0],'..','..','..','..','sample_data','clt.nc'))
   s=f.getslab('clt')                   # get slab clt
   x=vcs_legacy.init()                         # construct vcs_legacy canvas

   x.plot(s,'default','isofill','quick', bg=1) # plot slab the old way, but in background
   x.gif('test')			# generate gif file
   x.postscript('test')		        # generate postscript file
   x.cgm('test')		        # generate cgm file

   x.setcolormap("ASD")		# change the colormap
   x.clear()				# clear all segments

   t1=x.createtemplate('test1','ASD1_of_2') # create template 'test' from AMIPDUD
   t2=x.createtemplate('test2','ASD2_of_2') # create template 'test' from AMIPDUD
   isof=x.createisofill('test')		# create isofill graphics method from default
   isol=x.createisoline('test')		# create isoline graphics method from default

   ######################################################################
   # draw isofill plot on top, then an isoline plot on the bottom       #
   ######################################################################
   x.plot(s,t1,isof, bg=1)		# generate isofill plot in background
   x.plot(s,t2, isol, bg=1)		# generate isoline plot in background
   x.gif('test2.gif')			# generate gif file
   x.postscript('test2.ps')		# generate postscript file
   x.cgm('test2.cgm')			# generate cgm file

   x.clear()				# clear all segments

   ######################################################################
   # draw isofill plot, then overlay an isoline plot                    #
   ######################################################################
   x.plot(s, isof, bg=1)		# generate isofill plot
   x.plot(s, isol, 'default', bg=1)	# generate isoline plot
   x.gif('test3.gif')			# generate gif file


   x.clear()                            # clear all segments
   x.setcolormap("default")		# change colormap to default
   x.plot(s,bg=1)			# plot boxfill
   x.postscript('test4.ps')		# create a postscript file
   x.pstogif('test4.ps')		# generate a gif file from the postscript file
   x.gs('test.png')                     # generate ghostscript png output file
   x.gs('test.tif', device = 'tiff24nc', orientation = 'l', resolution = '172.x172.')# generate ghostscript tiff output file

   print '***************************************************************************************'
   print '******                                                                           ******'
   print '******   H A R D C O P Y   T E S T   C O M P L E T E D   S U C E S S F U L L Y   ******'
   print '******                                                                           ******'
   print '***************************************************************************************'


if __name__=="__main__":
   test()
