#
# Test low-level primitives
#
############################################################################
#                                                                          #
# Module:       testprimitives module                                      #
#                                                                          #
# Copyright:    2000, Regents of the University of California              #
#               This software may not be distributed to others without     #
#               permission of the author.                                  #
#                                                                          #
# Author:       Dean N. Williams, Lawrence Livermore National Laboratory   #
#               williams13@llnl.gov                                        #
#                                                                          #
# Description:  Used to test VCS's low_level primitives.                   #
#                                                                          #
# Version:      1.0                                                        #
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

   import vcs_legacy,support,sys
   bg=support.bg
   
   x=vcs_legacy.init()
   
   # For plotting of each test case, set the pause time to 1.
   x.pause_time=1
   
   ############################ First Test ###############################
   
   # Set the global viewport values only.
   #
   # Remember, must set global x.viewport (vp) and/or x.world coordinate (wc)
   # values before calling { [x.createline, x.createmarker, x.createfillarea,
   # x.createtext], [x.getline, x.getmarker, x.getfillarea, x.gettext],
   # [x.drawline, x.drawmarker, x.drawfillarea, x.drawtext] }.
   #
   # If the global viewport (vp) and world coordinate (wc) values are set 
   # and the, functions, listed above, do not specify them, then the global
   # settings will override the default settings.
   #
   # If the functions listed above have vp and wc set as an argument, then 
   # these vp and wc values will be used instead of the global vp and wc
   # values.
   #
   # Draw text on the screen.
   tex=x.createtext('nTtex','std','nToex','7left')
   tex.height = 25
   tex.x=[0.1, 0.1]
   tex.y=[0.97, 0.93]
   tex.string=['Example of drawing VCS low-level primitives.', 'Also, an example of using the viewport to clip graphics.']
   x.plot(tex,bg=bg)
   support.check_plot(x)
   #
   # At this point, let the world coordinate values remain at the default
   # settings [0, 1, 0, 1], but change the viewport.
   x.viewport=[0.3,0.7,0.3,0.7]
   support.check_plot(x)
   
   # Create the line, marker, fill area, and text objects
   ln=x.createline('new')
   mk=x.createmarker('new')
   fa=x.createfillarea('new')
   t=x.createtext('newTt','std','newTo','7left')
   
   # Draw a box around the entire VCS Canvas viewport. It will use the
   # global viewport set above and use the default world coordinate values:
   # Remember the default values for vp and wc are:
   #            viewport         = [0.0, 1.0, 0.0, 1.0]
   #            worldcoordinate  = [0.0, 1.0, 0.0, 1.0]
   ln.x=[0, 1, 1, 0, 0]		# x line positions
   ln.y=[0, 0, 1, 1, 0]		# y line positions
   ln.width=4  			# test width
   ln.color = 242 			# test color
   ln.type = 4			# test line type
   x.plot(ln,bg=bg)			# plot lines
   support.check_plot(x)
   
   # Draw text on the screen. Use global vp and default wc.
   t.x = [0.2, 0.7]		# x text positions
   t.y = [0.7, 0.7]		# y text positions
   t.string=['PCMDI', 'CDAT']	# text strings
   t.color=243			# text color
   t.height = 90			# text size
   x.plot(t,bg=bg)			# plot text
   support.check_plot(x)
   
   # Draw fill area on screen. Use global vp and default wc.
   fa.x=[[0.0,0.5,0.5,0.0],[0.5,1.0,0.75]]  # x fill area positions
   fa.y=[[0.0,0.0,0.5,0.5],[0.5,0.5,0.0]]   # x fill area positions
   
   # fa.color=[241,243] cannot do this! How important is this?
   
   fa.color=241				# fill area color
   fa.style='solid'			# fill area style
   fa.index=3				# fill area index
   x.plot(fa,bg=bg)				# plot fill area
   support.check_plot(x)
  
   
   # Draw marker on screen. Use global vp and default wc.
   mk.x=[[0.5,0.2],[0.3,0.7,1.0]]	# x marker positions
   mk.y=[[0.5,0.2],[0.3,0.7,0.5]]	# y marker positions
   mk.color=242			# marker color
   mk.size=50			# marker size
   mk.type='dot'			# marker type
   x.plot(mk,bg=bg)			# plot marker
   support.check_plot(x)
   
   ############################ First Test completed ######################

 
   if not '--extended' in sys.argv:
     print '\n************* PARTIAL TEST *****************'
     print 'FOR COMPLETE TEST OF THIS MODULE USE '
     print '   -F (--full) or -E (--extended) option'
     print '************* PARTIAL TEST *****************\n'
     sys.exit()

   
   ############################ Second Test ###############################
   # Reset the global viewport values.
   x.viewport=[0.2,0.8,0.2,0.8]
   support.check_plot(x)
   
   # Redraw a box around the drawing area, but use 'x.line' instead of 'x.plot'.
   ln = x.getline('new')
   x.line(ln,bg=bg)
   support.check_plot(x)
   
   # Redraw markers, but use 'x.marker', instead of 'x.plot'.
   mk = x.getmarker('new')
   x.marker(mk,bg=bg)
   support.check_plot(x)
   
   # Redraw fill area, but use 'x.fillarea', instead of 'x.plot'.
   fa = x.getfillarea('new')
   x.fillarea(fa,bg=bg)
   support.check_plot(x)
   
   # Redraw text, but use 'x.text', instead of 'x.plot'.
   t= x.gettext('newTt', 'newTo')
   x.text(t,bg=bg)
   support.check_plot(x)

   ############################ Second Test completed ######################
   
   ############################ Third Test ###############################
   # Change the drawing area's world coordinate from [0,1,0,1] to [0,2,0,2]
   # via primitive's graphics method, then redraw. The graphics will be drawn
   # in the lower left corner. This is because the line x and y values are 
   # between (0 and 1) and the wc values are now between (0 and 2).
   
   ln.worldcoordinate=[0,2, 0,2]	# set wc
   support.check_plot(x)
   x.plot(ln,bg=bg)
   support.check_plot(x)
   
   # Note how the most right marker is not clipped
   mk.worldcoordinate=[0,2, 0,2]	# set wc
   support.check_plot(x)
   x.plot(mk,bg=bg)
   
   fa.worldcoordinate=[0,2, 0,2]
   support.check_plot(x)
   fa.color = 240			# change color to white
   support.check_plot(x)
   x.plot(fa,bg=bg)
   support.check_plot(x)
   
   t.worldcoordinate=[0,2, 0,2]	# set wc
   x.plot(t,bg=bg)
   support.check_plot(x)

   ############################ Third Test completed ######################
   
   
   ############################ Fourth Test ###############################
   #
   # Clear the VCS Canvas and redraw graphics using Python list of lists.
   #
   # Remember, the viewport and world coordinates for the graphics primitives
   # are:
   #       viewport         = [0.2, 0.8, 0.2, 0.8]
   #       worldcoordinate  = [0, 2, 0, 2]
   x.clear()
   
   # Draw multiple line segments
   ln.x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]]	# x line positions
   ln.y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]] # y line positions
   ln.type='solid'
   x.plot(ln,bg=bg)
   support.check_plot(x)
   
   # Draw multiple marker segments
   mk.x=[[0.8,1.3],[0.5,1.0,1.5]]  # x marker positions
   mk.y=[[1.5,1.5],[0.5,0.5,0.5]]  # y marker positions
   x.plot(mk,bg=bg)
   support.check_plot(x)
   
   # Draw multiple fill area segments
   fa.x=[[0.2,0.5,0.5,0.2],[1.2,1.8,1.8,1.2]]  # x fill area positions
   fa.y=[[0.2,0.2,0.5,0.5],[1.2,1.2,1.8,1.8]]   # x fill area positions
   fa.color=241
   x.plot(fa,bg=bg)
   support.check_plot(x)
   
   # Draw multiple text segments
   t.x = [0.5, 1.17]                # x text positions
   t.y = [0.8, 0.8]                # y text positions
   t.string=['Text 1', 'Text 2']   # text strings
   t.color=244                     # text color
   t.height = 90                   # text size
   x.plot(t,bg=bg)                       # plot text
   support.check_plot(x)

   
   
   ############################ Fourth Test completed ######################
   
   ############################ Fifth Test ###############################
   # Change the viewport only to a different shape and replot.
   # 1st change
   ln.viewport=[0.0,0.2,0.0,0.8]
   support.check_plot(x)
   x.plot(ln,bg=bg)
   support.check_plot(x)

   mk.viewport=[0.0,0.2,0.0,0.8]
   support.check_plot(x)
   x.plot(mk,bg=bg)
   support.check_plot(x)
   fa.viewport=[0.0,0.2,0.0,0.8]
   support.check_plot(x)
   x.plot(fa,bg=bg)
   support.check_plot(x)
   t.viewport=[0.0,0.2,0.0,0.8]
   support.check_plot(x)
   x.plot(t,bg=bg)
   support.check_plot(x)
   
   # 2nd change
   ln.viewport=[0.0,0.8,0.0,0.2]
   support.check_plot(x)
   x.plot(ln,bg=bg)
   support.check_plot(x)
   mk.viewport=[0.0,0.8,0.0,0.2]
   support.check_plot(x)
   x.plot(mk,bg=bg)
   support.check_plot(x)
   fa.viewport=[0.0,0.8,0.0,0.2]
   support.check_plot(x)
   x.plot(fa,bg=bg)
   support.check_plot(x)
   t.viewport=[0.0,0.8,0.0,0.2]
   support.check_plot(x)
   x.plot(t,bg=bg)
   support.check_plot(x)
   
   ############################ Fifth Test completed ######################
   
   ############################ Sixth Test ###############################
   # Clear the VCS Canvas and redraw the graphics, but this time use the
   # draw functions. That is, user drawline, drawmarker, drawfillarea, and
   # drawtext.
   x.clear()
   ln=x.drawline('new',
                 ltype= 1,
                 width= 10,
                 color=244,
                 worldcoordinate=[0.0,2.0,0.0,2.0],
                 x=[[0.0,2.0,2.0,0.0,0.0], [0.5,1.5]],
                 y=[[0.0,0.0,2.0,2.0,0.0], [1.0,1.0]],
                 bg=bg
                )
   support.check_plot(x)

   mk=x.drawmarker('new',
                   mtype= 'square_fill',
                   size= 40,
                   color=243,
                   viewport=[0.0,1.0,0.0,1.0],
                   worldcoordinate=[0.0,2.0,0.0,2.0],
                   x=[[0.0,1.0], [1.0]],
                   y=[[0.0,1.0], [2.0]],
                   bg=bg
                  )
   support.check_plot(x)

   fa=x.drawfillarea('new',
                   style='hatch',
                   index=2,
                   color=246,
                   viewport=[0.0,1.0,0.0,1.0],
                   worldcoordinate=[0.0,2.0,0.0,2.0],
		   x=[[0.5,1.0,1.0,0.5],[0.5,1.0,0.75]],
		   y=[[0.0,0.0,0.5,0.5],[1.0,1.0,0.5]],
                     bg=bg
                  )
   support.check_plot(x)

   t=x.drawtext(Tt_name='newTt',To_name='newTo',
                   font = 4,
                   color = 242,
                   height = 70,
                   angle=45,
                   string=[ 'Quad 1', 'Quad 2', 'Quad 3', 'Quad 4' ],
                   viewport=[0.0,1.0,0.0,1.0],
                   worldcoordinate=[0.0,2.0,0.0,2.0],
                   x=[[1.5,1.5], [0.5,0.5]],
                   y=[[0.5,1.5], [1.5, 0.5]],
                bg=bg
                  )
   support.check_plot(x)

   
   ############################ Sixth Test completed ######################
   
   ############################ Seventh Test ###############################
   # Clear the VCS Canvas and test the priority of each primitive.
   #
   x.clear()
   # Set the global viewport and worldcoordinate values.
   x.viewport=[0.3,0.7,0.3,0.7]
   x.worldcoordinate=[0,1,0,1]
   
   ln2=x.createline('new2')
   ln3=x.createline('new3')
   mk2=x.createmarker('new2')
   mk3=x.createmarker('new3')
   fa2=x.createfillarea('new2')
   fa3=x.createfillarea('new3')
   t2=x.createtext('new12','std','new22','7left')
   
   # Draw a box around the VCS Canvas drawing area.
   ln.x=[0, 1, 1, 0, 0]
   ln.y=[0, 0, 1, 1, 0]
   ln.viewport=[0.3,0.7,0.3,0.7]
   ln.worldcoordinate=[0,1,0,1]
   ln.width=4
   ln.color = 241
   ln.type = 0
   ln.priority=10		# Set the priority high so it will always be seen
   x.plot(ln,bg=bg)
   support.check_plot(x)
   
   ln2.x=[0,0.5]
   ln2.y=[0.5,0.5]
   ln2.priority=2		# Set the priority higher than the next line
   ln2.width=10
   ln2.color=243
   x.plot(ln2,bg=bg)
   support.check_plot(x)
   
   ln3.x=[0,1]
   ln3.y=[0.5,0.5]
   ln3.priority=1
   ln3.width=10
   ln3.color=244
   x.plot(ln3,bg=bg)
   support.check_plot(x)
   
   mk2.x=[0.5]
   mk2.y=[0.7]
   mk2.color=242
   mk2.size=50
   mk2.priority=2
   x.plot(mk2,bg=bg)
   support.check_plot(x)
   
   mk3.x=[0.55]
   mk3.y=[0.7]
   mk3.color=246
   mk3.size=40
   mk3.type='square_fill'
   mk3.priority=1
   x.plot(mk3,bg=bg)
   support.check_plot(x)
   
   fa2.x=[0.3,0.7,0.7,0.3]
   fa2.y=[0.0,0.0,0.4,0.4]
   fa2.color=247
   fa2.style='solid'
   fa2.index=3
   fa2.priority = 2
   x.plot(fa2,bg=bg)
   support.check_plot(x)
   
   fa3.x=[0.5,0.9,0.7]
   fa3.y=[0.4,0.4,0.0]
   fa3.style='solid'
   fa3.color=248
   fa3.index=15
   fa3.priority = 1
   x.plot(fa3,bg=bg)
   support.check_plot(x)
   
   t2.x = [0.2, 0.6]
   t2.y = [0.4, 0.4]
   t2.string=['PCMDI', 'CDAT']
   t2.color=241
   t2.font=2
   t2.height = 120
   t2.priority = 1
   x.plot(t2,bg=bg)
   support.check_plot(x)
   
   ############################ Seventh Test completed ######################
   
   ############################ Eighth Test ###############################
   # Clear the VCS Canvas and test the Python list for the primitive attributes.
   #
   x.clear()
   ln.x=[[0,1], [1,1], [1,0],[0,0]]
   ln.y=[[0,0], [0,1], [1,1],[1,0]]
   ln.width=[1,10,20,30]
   ln.color = [242, 243, 244, 245]
   ln.type = [1,2,3,0]
   ln.viewport=[0.2,0.8,0.2,0.8]
   ln.worldcoordinate=[0.0, 1.0, 0.0, 1.0]
   x.plot(ln,bg=bg)
   support.check_plot(x)
   
   mk.x=[[0.5,0.2],[0.3,0.7],[1.0]]
   mk.y=[[0.5,0.2],[0.3,0.7],[0.5]]
   mk.color=[242,243,244]
   mk.size=[30,50,70]
   mk.type=['dot', 'star', 'square_fill']
   mk.viewport=[0.2,0.8,0.2,0.8]
   mk.worldcoordinate=[0.0, 1.0, 0.0, 1.0]
   x.plot(mk,bg=bg)
   support.check_plot(x)
   
   fa.x=[[0.0,0.5,0.5,0.0],[0.5,1.0,0.75], [0.3,0.7,0.5]]
   fa.y=[[0.0,0.0,0.5,0.5],[0.5,0.5,0.0], [0.52,0.52,0.95]]
   fa.color=[241,242,243]
   fa.style=['hatch','solid']
   fa.index=[ 1, 2, 3]
   fa.viewport=[0.2,0.8,0.2,0.8]
   fa.worldcoordinate=[0.0, 1.0, 0.0, 1.0]
   x.plot(fa,bg=bg)
   support.check_plot(x)
   
   ############################ Eighth Test completed ######################
   
   print '*****************************************************************************************'
   print '******                                                                             ******'
   print '******   L O W - L E V E L   P R I M I T I V E S ( I . E . ,  L I N E S ,          ******'
   print '******           M A R K E R S ,   F I L L A R E A S ,   A N D   T E X T )         ******'
   print '******                                                                             ******'
   print '******               C O M P L E T E D   S U C E S S F U L L Y   ! ! !             ******'
   print '******                                                                             ******'
   print '*****************************************************************************************'


if __name__=="__main__":
   test()

###################################################################################
#                                   End of File                                   #
###################################################################################
