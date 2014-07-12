#!/usr/bin/env python
#
# The VCS Colormap GUI controls -  colormapgui module
#
#################################################################################
#                                                                               #
# Module:       colormapgui module                                              #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI's VCS GUI colormap browser and editor. Tkinter version.   #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################

#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw, tkFileDialog
import os, types, string, sys
import colormap
import gui_support
from browser import vcs_legacy_function
from error import vcs_legacyError
from browser import gui_control

# Maximum intensity value for R,G,B or C,M,Y color index values
psz = 100.0

#
# Create the Tkinter/Pmw Colormap editor interface
#
class ColormapGui:
    def __init__( self, vcs_legacy=None, gui_parent=None, transient=0):
        if vcs_legacy is None:
            import Canvas
            vcs_legacy = Canvas.Canvas()
        # initialize the color index list and the copy buffer list
        self.color_indices = []
        self.copy_buffer = []

        # create the colormap main toplevel menu
        title = "VCS's Colormap Editor: Editing -- " + vcs_legacy.canvas.getcolormapname()
        self.dialog = gui_support.VcsDialog(title=title,
                                            buttons=(),
                                            )

        self.dialog.dialog.withdraw()
        if gui_support.root_exists():
            root = gui_support.root()
            self.top_parent = root
        else:
            root = gui_support.root()
            self.top_parent = None
        self.root = root
        parent = self.dialog.interior()
        self.parent = parent
        self.cmain_menu = Pmw.MenuBar(parent,
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon = gui_support.balloon
                )
        self.cmain_menu.pack(side='top', fill='both')

        # Set the tear off value
        tear_it = 1
        if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): tear_it = 0

        #-------------------------------------------
        # menu 1 -- 'File'
        #-------------------------------------------
        create_colormap_file_menu( self.cmain_menu, self, parent, root, vcs_legacy, tear_it )

        #-------------------------------------------
        # menu 2 -- 'Edit'
        #-------------------------------------------
        create_edit_menu( self, self.cmain_menu, parent, vcs_legacy, tear_it )

        #-------------------------------------------
        # menu 3 -- 'Options'
        #-------------------------------------------
        create_options_menu( self, self.cmain_menu, parent, vcs_legacy, tear_it )


        #-------------------------------------------
        # menu 4 -- 'Help'
        #-------------------------------------------
        create_colormap_help_menu( self.cmain_menu, self, parent, tear_it)

        ########################################################################
        #                                                                      #
        #  Create the rest of the colormap GUI                                 #
        #                                                                      #
        ########################################################################

        #
        #-------------------------------------------
        # Specify the indices be viewed in the colormap
        #-------------------------------------------
        #
        font = apply(Pmw.logicalfont, ('Helvetica', 0), {'weight' : 'bold'})
	self.numColors = Pmw.EntryField(parent,
		labelpos = 'w',
                entry_background = 'white',
                entry_foreground = 'black',
		label_text = 'View color indices:',
		entry_width = 5,
                entry_font = font,
		command = E_Command(self.get_color_indices, vcs_legacy)
                )
        gui_support.balloon.bind( self.numColors, 'View color map color indices by provide index ranges or individual color indices. For example: \n\t0:255\n\t16,32,48,64,80,96\n\t16:32,50,60,70,80,90,160:200' )
        self.numColors.setentry( '0:255' )
        self.numColors.pack( side='top', fill='both', pady=10 )

        #
        #-------------------------------------------
        # Create the colormap selection canvas and the colormap spectrum canvas
        #-------------------------------------------
        #
        framea = Tkinter.Frame( parent )
	self.width = 475
	self.height = 400
	self.width2 = 50
        self.borderwidth = 8
	self.canvas = Tkinter.Canvas(framea, borderwidth=self.borderwidth, relief = 'raised',
		width = self.width, height = self.height)
        gui_support.balloon.bind( self.canvas, 'With the left mouse button:\n\tselect once to set the first index range;\n\tmove the pointer to another index and select again for the second index range;\n\tuse the menu "Edit" options to blend, copy, and paste selection.\n\nUse the Red, Green, and Blue (or Cyan, Magenta, Yellow) (or Hue, Saturation, Value)\nrows below to alter the selected color index.\n\nNote: Index 0 controls the background color of the VCS Canvas. Indices 240 through 255\nare reserved colors.' )
        self.canvas.pack( side='left',  fill = 'both' )
        self.canvas.bind( '<1>', E_Command( self.select_color_index_left, vcs_legacy ) )

	self.canvas2 = Tkinter.Canvas(framea, borderwidth=1, relief = 'raised',
		width = self.width2, height = self.height)
        gui_support.balloon.bind( self.canvas2, 'View of color map spectrum' )
        self.canvas2.pack( side='left', padx=10 )
        framea.pack( side='top', fill='both' )

        #
        #-------------------------------------------
        # Create the R,G,B index sliders
        #-------------------------------------------
        #
        framergb = Tkinter.Frame( parent )
        zero_to_100 = []
        for i in range( 0, int(psz+1) ): zero_to_100.append( i )

        # Create the red slider
        self.frameb = Tkinter.Frame( framergb )
        self.red_comb = Pmw.ComboBox( self.frameb,
           scrolledlist_items = zero_to_100,
           labelpos = 'w',
           label_text = 'Red:',
           entry_width = 10,
           entry_background = 'red',
           entry_foreground = 'black',
           selectioncommand = E_Command( self.evt_change_color, vcs_legacy, 1 )
           )
        gui_support.balloon.bind( self.red_comb, 'Enter or select values ranging from 0 to %d, or use the slider to set the red (or cyan) intensity value.'% psz )
        self.red_comb.component( 'arrowbutton' ).configure( background = 'red' )
        self.red_comb.component( 'listbox' ).configure( background = 'red' )
        self.red_comb.pack( side='left', fill='both', padx=10, pady=2 )
        self.red_scl= Tkinter.Scale( self.frameb,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 255,
           troughcolor      = 'red',
           from_            = 0,
           to               = psz,
           command          = E_Command( self.evt_change_color, vcs_legacy, 2 )
          )
        gui_support.balloon.bind( self.red_scl, 'Enter or select values ranging from 0 to %d, or use the slider to set the red (or cyan) intensity value.' % psz)
        self.red_scl.pack( side='left' )
        self.frameb.pack( side='top', fill='both' )

        # Create the green slider
        self.framec = Tkinter.Frame( framergb )
        self.green_comb = Pmw.ComboBox( self.framec,
           scrolledlist_items = zero_to_100,
           labelpos = 'w',
           label_text = 'Green:',
           entry_width = 10,
           entry_background = 'green',
           entry_foreground = 'black',
           selectioncommand = E_Command( self.evt_change_color, vcs_legacy, 1 )
           )
        gui_support.balloon.bind( self.green_comb, 'Enter or select values ranging from 0 to %d, or use the slider to set the green (or magenta) intensity value.' % psz )
        self.green_comb.component( 'arrowbutton' ).configure( background = 'green' )
        self.green_comb.component( 'listbox' ).configure( background = 'green' )
        self.green_comb.pack( side='left', fill='both', padx=10, pady=2 )
        self.green_scl= Tkinter.Scale( self.framec,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 255,
           troughcolor      = 'green',
           from_            = 0,
           to               = psz,
           command          = E_Command( self.evt_change_color, vcs_legacy, 2 )
          )
        gui_support.balloon.bind( self.green_scl, 'Enter or select values ranging from 0 to %d, or use the slider to set the green (or magenta) intensity value.' % psz )
        self.green_scl.pack( side='left' )
        self.framec.pack( side='top', fill='both' )

        # Create the blue slider
        self.framed = Tkinter.Frame( framergb )
        self.blue_comb = Pmw.ComboBox( self.framed,
           scrolledlist_items = zero_to_100,
           labelpos = 'w',
           label_text = 'Blue:',
           entry_width = 10,
           entry_background = 'blue',
           entry_foreground = 'white',
           selectioncommand = E_Command( self.evt_change_color, vcs_legacy, 1 )
           )
        gui_support.balloon.bind( self.blue_comb, 'Enter or select values ranging from 0 to %d, or use the slider to set the blue (or yellow) intensity value.' % psz )
        self.blue_comb.component( 'arrowbutton' ).configure( background = 'blue' )
        self.blue_comb.component( 'listbox' ).configure( background='blue', foreground='white' )
        self.blue_comb.pack( side='left', fill='both', padx=10, pady=2 )
        self.blue_scl= Tkinter.Scale( self.framed,
           orient           = 'horizontal',
           width            = 12,
           showvalue        = 0,
           length           = 255,
           troughcolor      = 'blue',
           from_            = 0,
           to               = psz,
           command          = E_Command( self.evt_change_color, vcs_legacy, 2 )
          )
        gui_support.balloon.bind( self.blue_scl, 'Enter or select values ranging from 0 to %d, or use the slider to set the blue (or yellow) intensity value.' % psz )
        self.blue_scl.pack( side='left' )
        self.framed.pack( side='top', fill='both' )
        # Create Hue counter
        self.frameb_hsv = Tkinter.Frame( framergb )
        self.hue = Pmw.Counter(self.frameb_hsv,
                labelpos = 'w',
                label_text = 'Hue:',
                entryfield_value = '0.0',
                entry_background = 'grey',
                datatype = {'counter' : 'real', 'separator' : '.'},
                entryfield_validate = {'validator' : 'real',
                        'min' : '-0.01', 'max' : '360.0',
                        'separator' : '.'},
                increment = 0.1,
                entryfield_modifiedcommand = E_Command( self.evt_change_color, vcs_legacy, 3, None )
                )
        gui_support.balloon.bind( self.hue, 'Hue or H is a measure of the angle around the vertical axis in the hexcone color model.' )
        self.hue.pack( side='left', fill='both' ) 

        # Create Saturation counter
        self.framec_hsv = Tkinter.Frame( framergb )
        self.saturation = Pmw.Counter(self.framec_hsv,
                labelpos = 'w',
                label_text = 'Saturation:',
                entryfield_value = '0.000',
                entry_background = 'grey',
                datatype = {'counter' : 'real', 'separator' : '.'},
                entryfield_validate = {'validator' : 'real',
                        'min' : '0.0', 'max' : '1.0',
                        'separator' : '.'},
                increment = 0.01,
                entryfield_modifiedcommand = E_Command( self.evt_change_color, vcs_legacy, 3, None )
                )
        self.saturation.pack( side='left', fill='both' )
        gui_support.balloon.bind( self.saturation, 'Saturation is a measure relative to the color gamut of the hexcone color model.' )

        # Create Value counter
        self.framed_hsv = Tkinter.Frame( framergb )
        self.value = Pmw.Counter(self.framed_hsv,
                labelpos = 'w',
                label_text = 'Value:',
                entryfield_value = '0.000',
                entry_background = 'grey',
                datatype = {'counter' : 'real', 'separator' : '.'},
                entryfield_validate = {'validator' : 'real',
                        'min' : '0.0', 'max' : '1.0',
                        'separator' : '.'},
                increment = 0.01,
                entryfield_modifiedcommand = E_Command( self.evt_change_color, vcs_legacy, 3, None )
                )
        self.value.pack( side='left', fill='both' )
        gui_support.balloon.bind( self.value, "Value represents the hexcone's height with the apex at the origin. The apex value of 0 is black." )
        Pmw.alignlabels((self.hue, self.saturation, self.value))


        # Line up R, G, B text
	Pmw.alignlabels((self.red_comb, self.green_comb, self.blue_comb))

        framergb.pack( side='left', after = framea, expand = 1, fill='both',  pady = 10 )

        #
        #-------------------------------------------
        # Create the third canvas (i.e., the index canvas)
        #-------------------------------------------
        #
        framee = Tkinter.Frame( parent )
	self.canvas3 = Tkinter.Canvas(framee, borderwidth=8, relief = 'raised',
		width = 65, height = 65)
        self.canvas3.create_polygon(8, 8, 85, 8, 85, 85, 8, 85 )
#                                   fill = 'red', outline = 'red')
        self.canvas3.bind( '<1>', E_Command( self.evt_select_similiar_color, self, parent, vcs_legacy ))
        gui_support.balloon.bind( self.canvas3, 'Select enlarged color index to\nview and select similiar alterative\ncolors.' )
        font = apply(Pmw.logicalfont, ('Helvetica', 5), {'weight' : 'bold'})
        self.canvas3.create_text(43, 45, anchor = 'center',
		text = 0, font = font )
        self.canvas3.pack( side='top', padx=20 )
        framee.pack( side='top', fill='both',  pady = 10 )

	parent.grid_columnconfigure(0, weight = 1)
	parent.grid_columnconfigure(1, weight = 1)
	parent.grid_rowconfigure(0, weight = 1)

        #-------------------------------------------
	# Set initial values for all entries.
        #-------------------------------------------
        vcs_legacy.canvas.flush()
        self.get_VCS_colormap( vcs_legacy )
	self.execute( )

        #-------------------------------------------
        # Set the third color canvas
        #-------------------------------------------
        background = self.colorList[ 0 ]
        r, g, b = self.root.winfo_rgb(background)
        r =  r/256; g = g/256; b = b/256
        self.red_scl.set( r )
        self.green_scl.set( g )
        self.blue_scl.set( b )

        # Decide where to put it on the screen
        if gui_parent is None:
            d=[self.dialog.dialog.winfo_screenwidth()/4,
               self.dialog.dialog.winfo_screenheight()/4
              ]
        else:
            g=gui_parent.geometry()
            d = g.split('+')[1:]
            
        self.dialog.geometry("+%s+%s"% (d[0],d[1]))
        
        self.dialog.deiconify() # Show the colormap editor GUI

        if (gui_parent is not None) and (transient == 1):
           self.dialog.dialog.transient(gui_parent)

    #############################################################################
    #                                                                           #
    # End the creation of the colormap GUI calls and start the GUI's call back  #
    # functions.	                                                        #
    #                                                                           #
    #############################################################################

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Retrieve the appropriate color indices via the user's requests.           #
    #                                                                           #
    #############################################################################
    def get_color_indices(self, vcs_legacy):
        if self.numColors.get( ) != '':
            str = string.replace( self.numColors.get( ), ' ', ',' )
            while string.find(str,',,',0) != -1: str=string.replace(str,',,',',')
            while string.find(str,',:',0) != -1: str=string.replace(str,',:',':')
            while string.find(str,'::',0) != -1: str=string.replace(str,'::',':')
            while string.find(str,':,',0) != -1: str=string.replace(str,':,',':')

            self.color_indices = []
            str = string.split( str, ',' )
            for x in str:
               try:
                 num = string.atoi(x)
                 if num in range(0, 256): self.color_indices.append(num)
               except:
                 str2 = string.split( x, ':' )
                 f = string.atoi(str2[0])
                 l = string.atoi(str2[1])
                 if f not in range(0, 256): break
                 if l not in range(0, 256): break
                 if f <= l:
                   for i in range(f, l+1):
                      self.color_indices.append(i)
                 else:
                   for i in range(l, f+1):
                      self.color_indices.append(f+l-i)

            self.execute( )

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Select the color index by outlining the box and showing its R,G,B values. #
    #                                                                           #
    #############################################################################
    def select_color_index_left( self, vcs_legacy, event):

        # Get the color index
        x = self.canvas.canvasx(event.x)
	y = self.canvas.canvasy(event.y)
        color_index = None
	for i in self.color_indices:
          if ( (x in range(int(self.canvas.bbx[i][0][0]), int(self.canvas.bbx[i][0][2]))) and
               (y in range(int(self.canvas.bbx[i][0][1]), int(self.canvas.bbx[i][0][3]))) ):
             color_index = i
             break

        # Must be outside the user specified color range, so return without selecting
        if (color_index is None) or (color_index > 239): return

        # Show the color index values in the R, G, B comboBox and scale widgets
        r, g, b = self.root.winfo_rgb(self.canvas.bbx[color_index][1])
        r =  int( r/256./255.*psz ); g = int( g/256./255.*psz ); b = int( b/256./255.*psz )
        cmy_r = psz - r; cmy_g = psz - g; cmy_b = psz - b     # Save CMY color values
        hsv_r = float( r/psz ); hsv_g = float( g/psz ); hsv_b = float( b/psz ) # Save HSV color
        self.red_comb.setentry( r )
        self.green_comb.setentry( g )
        self.blue_comb.setentry( b )
        self.red_scl.set( r )
        self.green_scl.set( g )
        self.blue_scl.set( b )

        # Remove the selected from clip board and set new clip board
        if self.canvas.b1ct == 0:
           if (self.canvas.b1ct2 > 1):
              self.execute( )
              self.canvas.b1ct2 = 1
           else:
              self.canvas.b1ct2 += 1

           # Highlight color index
           if color_index != None:
              r, g, b = self.root.winfo_rgb(self.canvas.bbx[color_index][1])
              r =  r/256; g = g/256; b = b/256
              outline = 'black'
              if g <= 179: outline = 'white'
	      self.canvas.create_rectangle(self.canvas.bbx[color_index][0], 
                                           outline = outline)

           # Reset clip_board
           self.canvas.clip_board = {}
           self.canvas.clip_board[0] = (color_index, self.canvas.bbx[color_index] )
           self.canvas.b1ct += 1
        else:
           self.canvas.b1ct = 0
           if (self.canvas.b1ct2 > 1):
              self.execute( )
           else:
              self.canvas.b1ct2 += 1
           self.canvas.direction = 1
           if (self.canvas.clip_board[0][0] > color_index): self.canvas.direction = 0
           f = self.canvas.clip_board[0][0]
           l = color_index
           ct = 0
           if f <= l:
              for i in range(f, l+1):
                 rs, gs, bs = self.root.winfo_rgb(self.canvas.bbx[i][1])
                 r =  rs/256; g = gs/256; b = bs/256
                 rf=int(rs/256./255.*psz); gf=int(gs/256./255.*psz); bf=int(bs/256./255.*psz)
                 outline = 'black'
                 if g <= 179: outline = 'white'
	         self.canvas.create_rectangle(self.canvas.bbx[i][0], 
                                              outline = outline)
                 self.canvas.clip_board[ct] = (i, self.canvas.bbx[i], (rf,gf,bf) )
                 ct += 1
           else:
              for i in range(l, f+1):
                 rs, gs, bs = self.root.winfo_rgb(self.canvas.bbx[f+l-i][1])
                 r =  rs/256; g = gs/256; b = bs/256
                 rf=int(rs/256./255.*psz); gf=int(gs/256./255.*psz); bf=int(bs/256./255.*psz)
                 outline = 'black'
                 if g <= 179: outline = 'white'
	         self.canvas.create_rectangle(self.canvas.bbx[f+l-i][0], 
                                              outline = outline)
                 self.canvas.clip_board[ct] = (f+l-i, self.canvas.bbx[f+l-i], (rf,gf,bf) )
                 ct += 1

        self.canvas.selected_color_index = color_index

	# Set the sliders to the Cyan, Magenta, Yellow (CMY) color model. The 
	# CMY is importan when dealing with hardcopy devices that deposit colored
        # pigments onto paper.
        if self.cmain_menu.color_mode == 1:
           self.red_comb.setentry( cmy_r )
           self.green_comb.setentry( cmy_g )
           self.blue_comb.setentry( cmy_b )
           self.red_scl.set( cmy_r )
           self.green_scl.set( cmy_g )
           self.blue_scl.set( cmy_b )
        elif self.cmain_menu.color_mode == 2:
           h,s,v = RGB_to_HSV(hsv_r,hsv_g,hsv_b)
           if h is not None:
              self.hue.setentry( h )
           else:
              self.hue.setentry( -0.01 ) # This represents None or Undefined
           self.saturation.setentry( s )
           self.value.setentry( v )
           r,g,b = HSV_to_RGB(h,s,v)


    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Change the color index to user specified entry.                           #
    #                                                                           #
    #############################################################################
    def evt_change_color( self, vcs_legacy, called_from, event):
        if called_from == 1:
           rv = int( self.red_comb.get () )
           if rv > psz: rv = psz
           elif rv < 0: rv = 0
           if self.cmain_menu.color_mode == 1: rv = psz - rv     # Show CMY value
           self.red_comb.setentry( rv )
           self.red_scl.set( rv )
   
           gv = int( self.green_comb.get () )
           if (gv > psz): gv = psz
           elif (gv < 0): gv = 0
           if self.cmain_menu.color_mode == 1: gv = psz - gv     # Show CMY value
           self.green_comb.setentry( gv )
           self.green_scl.set( gv )

           bv = int( self.blue_comb.get () )
           if bv > psz: bv = psz
           elif bv < 0: bv = 0
           if self.cmain_menu.color_mode == 1: gv = psz - gv     # Show CMY value
           self.blue_comb.setentry( bv )
           self.blue_scl.set( bv )
        elif called_from == 3:                                   # Called from HSV
           try:
              h = float( self.hue.get() )
              s = float( self.saturation.get() )
              v = float( self.value.get() )
              if h < 0.0: h = None
              rv,gv,bv = HSV_to_RGB(h,s,v)
           except:
              return
           rv*=psz; gv*=psz; bv*=psz
        else:
           rv = cmy_rv = self.red_scl.get()
           if self.cmain_menu.color_mode == 1:
              rv = psz - rv     # Show CMY value
              self.red_comb.setentry( cmy_rv )
           else:
              self.red_comb.setentry( rv )
           gv = cmy_gv = self.green_scl.get()
           if self.cmain_menu.color_mode == 1:
              gv = psz - gv     # Show CMY value
              self.green_comb.setentry( cmy_gv )
           else:
              self.green_comb.setentry( gv )
           bv = cmy_bv = self.blue_scl.get()
           if self.cmain_menu.color_mode == 1:
              bv = psz - bv     # Show CMY value
              self.blue_comb.setentry( cmy_bv )
           else:
              self.blue_comb.setentry( bv )

        r = int( rv ) / psz * 255.
        g = int( gv ) / psz * 255.
        b = int( bv ) / psz * 255.
        rc = psz - rv
        gc = psz - gv
        bc = psz - bv
        bbx = self.canvas.bbx[ self.canvas.selected_color_index ]
        background = "#%02x%02x%02x" % ( r, g, b )
        font1 = apply(Pmw.logicalfont, ('Helvetica', 0), {'weight' : 'bold'})
        font2 = apply(Pmw.logicalfont, ('Helvetica', 5), {'weight' : 'bold'})
        outline = 'black'
        if g <= 179: outline = 'white'
        twoffset = (bbx[0][2] - bbx[0][0])* 0.5
        thoffset = (bbx[0][3] - bbx[0][1])* 0.5

	# Store the colormap value in the colorList
        self.colorList[ self.canvas.selected_color_index ] = background

        # Set the color in the second canvas
        hstart = self.canvas2.hsize2 * self.canvas.selected_color_index
        hend = hstart + self.canvas2.hsize2

	self.canvas.create_polygon( bbx[0][0], bbx[0][1], bbx[0][2], bbx[0][1],
                                    bbx[0][2], bbx[0][3], bbx[0][0], bbx[0][3],
                                    fill = background, outline = outline)
	self.canvas2.create_polygon(0, hstart, self.width2, hstart, self.width2, hend,
                                       0, hend, fill = background, outline = background)
        self.canvas.create_text(bbx[0][0]+twoffset, bbx[0][1]+thoffset, anchor = 'center',
		text = self.canvas.selected_color_index, font = font1, fill=outline )
        self.canvas3.create_polygon(8, 8, 85, 8, 85, 85, 8, 85,
                                   fill = background, outline = background )
        font = apply(Pmw.logicalfont, ('Helvetica', 5), {'weight' : 'bold'})
        self.canvas3.create_text(43, 45, anchor = 'center', font = font2,
		text = self.canvas.selected_color_index, fill=outline )

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Get color that is similiar to selected color. Popup yet another GUI.      #
    #                                                                           #
    #############################################################################
    def evt_select_similiar_color( self, eself, parent, vcs_legacy, event):
        # Initialize variables
        color_index = eself.canvas.selected_color_index
        color = eself.colorList[color_index]
        font = apply(Pmw.logicalfont, ('Helvetica', 0), {'weight' : 'bold'})
        width = 450; height = 400; borderwidth = 7
        self.dred = 1; self.dgreen = 1; self.dblue = 1

	# Create the dialog.
        self.dialog2 = Pmw.Dialog( self.canvas3,
            buttons = ('OK', 'Apply', 'Dismiss'),
            defaultbutton = 'OK',
            title = "Select a Similiar Color",
            command = E_Command(self.execute_similiar_color, eself, vcs_legacy) )

	# Create and pack a RadioSelect widget, with radiobuttons.
        framea = Tkinter.Frame( self.dialog2.interior() )
        framea.pack( side='top', fill='both' )
        gui_support.balloon.bind( framea, "The viewing style allows the user to specify all three\ncolor RGB values or to omit one of them. 'RGB'\nindicates the tuple RGB intensity values will be used\nto represent the colors. 'Red' indicates that the red\nvalue will be omitted when displaying colors. 'Green'\nindicates that the green values will be omitted when\ndisplaying colors. 'Blue' indicates that the blue\nvalues will be omitted when displaying colors." )
	self.radiobuttons = Pmw.RadioSelect(framea,
		buttontype = 'radiobutton',
		orient = 'horizontal',
		labelpos = 'w',
		command = E_Command(self.toggle_callback, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font),
		label_text = 'Viewing Style:',
		hull_borderwidth = 2,
		hull_relief = 'ridge',
	)
	self.radiobuttons.pack(side = 'left', expand = 1, padx = 10, pady = 10)

	# Add some buttons to the radiobutton RadioSelect.
	for text in ('RGB', 'Red', 'Green', 'Blue'):
	    self.radiobuttons.add(text)

	self.canvas5 = Tkinter.Canvas(framea,
                      borderwidth=1, relief = 'raised', width = 20, height = 17)
        self.canvas5.create_polygon( 0, 0, psz, 0, psz, psz, 0, psz,
                                    fill = color)
        r, g, b = self.root.winfo_rgb(color)
        r =  r/256; g = g/256; b = b/256
        r1 =  int(r/255.*psz); g1 = int(g/255.*psz); b1 = int(b/255.*psz)
        outline = 'black'
        if g <= 179: outline = 'white'
        self.canvas5.create_text(20, 25, anchor = 'center',
                text = color_index, font = font, fill=outline )
        self.canvas5.pack( side ='left', expand = 1, fill = 'both', padx=10, pady=5 )

        # Add the controls section to the colormapgui
        frameb = Pmw.Group(self.dialog2.interior(), tag_text='Controls')
        frameb.pack(side='top', after=framea)
        frameba = Tkinter.Frame( frameb.interior() )
        frameba.pack(side='top')
        lbl=Tkinter.Label(frameba,
                text = "RGB Values",
                justify = 'left',
                anchor = 'w',
               )
        lbl.pack(side='left' )
        self.R = Pmw.Counter( frameba,
                labelpos = 'w',
                label_text = 'R:',
                orient = 'horizontal',
                entry_background = 'red',
                entry_width = 3,
                entry_state = 'normal',
                entryfield_value = r1,
        #        entryfield_command = E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "R", 0),
                entryfield_validate = {'validator' : 'integer',
                        'min' : 0, 'max' : psz}
               )
        self.R.pack(side='left', padx=5, pady=5)
        gui_support.balloon.bind( self.R, 'The red intensity value of the color ranging from\n0 to %d.' % psz )
        #self.R.component('entryfield').bind('<Return>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "R", 0))
        self.R.component('uparrow').bind('<1>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "R", 1))
        self.R.component('downarrow').bind('<1>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "R", -1))
        self.G = Pmw.Counter( frameba,
                labelpos = 'w',
                label_text = 'G:',
                orient = 'horizontal',
                entry_background = 'green',
                entry_width = 3,
                entry_state = 'normal',
                entryfield_value = g1,
                entryfield_validate = {'validator' : 'integer',
                        'min' : 0, 'max' : psz}
               )
        self.G.pack(side='left', padx=5, pady=5)
        gui_support.balloon.bind( self.G, 'The green intensity value of the color ranging from\n0 to %d.' %psz )
        #self.G.component('entry').bind('<Return>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "G", 0))
        self.G.component('uparrow').bind('<1>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "G", 1))
        self.G.component('downarrow').bind('<1>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "G", -1))
        self.B = Pmw.Counter( frameba,
                labelpos = 'w',
                label_text = 'B:',
                orient = 'horizontal',
                entry_foreground = 'white',
                entry_background = 'blue',
                entry_state = 'normal',
                entry_width = 3,
                entryfield_value = b1,
                entryfield_validate = {'validator' : 'integer',
                        'min' : 0, 'max' : psz}
               )
        self.B.pack(side='left', padx=5, pady=5)
        gui_support.balloon.bind( self.B, 'The blue intensity value of the color ranging from\n0 to %d.' %psz )
        #self.B.component('entry').bind('<Return>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "B", 0))
        self.B.component('uparrow').bind('<1>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "B", 1))
        self.B.component('downarrow').bind('<1>', E_Command(self.update_color, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, "B", -1))

        framebb = Tkinter.Frame( frameb.interior() )
        framebb.pack(side='top')
        self.rows = Pmw.Counter( framebb,
                labelpos = 'w',
                label_text = 'Number of Columns:',
                orient = 'horizontal',
                entry_width = 2,
                entryfield_value = 8,
                entryfield_validate = {'validator' : 'integer',
                        'min' : 1, 'max' : 16}
               )
        self.rows.pack(side='left', padx=0, pady=5)
        gui_support.balloon.bind( self.rows, 'Reset the number of columns from 1 to 16.' )
        #self.rows.component('entry').bind('<Return>', E_Command(self.update_row, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, 0))
        self.rows.component('uparrow').bind('<1>', E_Command(self.update_row, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, 1))
        self.rows.component('downarrow').bind('<1>', E_Command(self.update_row, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, -1))

        self.cols = Pmw.Counter( framebb,
                labelpos = 'w',
                label_text = 'Number of Rows:',
                orient = 'horizontal',
                entry_width = 2,
                entryfield_value = 8,
                entryfield_validate = {'validator' : 'integer',
                        'min' : 1, 'max' : 16}
               )
        self.cols.pack(side='left', padx=10, pady=5)
        gui_support.balloon.bind( self.cols, 'Reset the number of rows from 1 to 16.' )
        #self.cols.component('entry').bind('<Return>', E_Command(self.update_col, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, 0))
        self.cols.component('uparrow').bind('<1>', E_Command(self.update_col, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, 1))
        self.cols.component('downarrow').bind('<1>', E_Command(self.update_col, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, -1))

	self.canvas4 = Tkinter.Canvas(self.dialog2.interior(),
                      borderwidth=borderwidth, relief = 'raised',
		      width = width, height = height)
        self.canvas4.pack( expand = 1, fill = 'both', padx=10, pady=5 )
        self.canvas4.bind( '<1>', E_Command( self.select_color_index_left2, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font ) )
        self.canvas4.selected_color_index = 0

        gui_support.balloon.bind( self.canvas4, 'With the left mouse button select a desired color.' )

        # Update the color map
	self.radiobuttons.invoke('RGB')

        # Position dialog2 popup
        parent_geom = eself.root.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.dialog2.geometry( "+%d+%d" % (d1, d2) )


    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Get color the toggle preference.                                          #
    #                                                                           #
    #############################################################################
    def toggle_callback(self, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, tag):
#        print 'tag = ', tag
        if tag == 'RGB':
           eself.dred   = 1
           eself.dgreen = 1
           eself.dblue  = 1
        elif tag == 'Red':
           eself.dred   = 1
           eself.dgreen = 0
           eself.dblue  = 0
        elif tag == 'Green':
           eself.dred   = 0
           eself.dgreen = 1
           eself.dblue  = 0
        elif tag == 'Blue':
           eself.dred   = 0
           eself.dgreen = 0
           eself.dblue  = 1

        # Update the color map
        color_index = eself.canvas4.selected_color_index
        eself.execute2(eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font)

        eself.canvas4.selected_color_index = color_index

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Select the color index with the left mouse button.                        #
    #                                                                           #
    #############################################################################
    def select_color_index_left2(self, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, event):
        x = self.canvas4.canvasx(event.x)
        y = self.canvas4.canvasy(event.y)

        color_index = None
        numIndices = len( self.canvas4.bbx )
        for i in range(numIndices):
          if ( (x in range(int(self.canvas4.bbx[i][0][0]), int(self.canvas4.bbx[i][0][2]))) and
               (y in range(int(self.canvas4.bbx[i][0][1]), int(self.canvas4.bbx[i][0][3]))) ):
             color_index = i
             break

        # Must be outside the user specified color range, so return without selecting
        if color_index is None: return

        # Update the color map
        eself.execute2(eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font)

        r, g, b = self.root.winfo_rgb(self.canvas4.bbx[ color_index ][1])
        r =  r/256; g = g/256; b = b/256
        outline = 'black'
        if g <= 179: outline = 'white'
        self.canvas4.create_rectangle(self.canvas4.bbx[ color_index ][0], width=8,
                                              outline = outline)

        self.canvas4.selected_color_index = color_index

    def update_color(self, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, color_type, num, event):
       if (color_type == "R"):
          Rsize=string.atoi( self.R.get() ) + num
          self.R.setentry( Rsize )
       elif (color_type == "G"):
          Gsize=string.atoi( self.G.get() ) + num
          self.G.setentry( Gsize )
       elif (color_type == "B"):
          Bsize=string.atoi( self.B.get() ) + num
          self.B.setentry( Bsize )

       # Update the color map
       eself.execute2(eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font)

       return "break"

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Change the number of rows visible in the popup color selection.           #
    #                                                                           #
    #############################################################################
    def update_row(self, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, num, event):
       wsize=string.atoi( self.rows.get() ) + num
       self.rows.setentry( wsize )

       # Update the color map
       eself.execute2(eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font)

       return "break"

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Change the number of columns visible in the popup color selection.        #
    #                                                                           #
    #############################################################################
    def update_col(self, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font, num, event):
       hsize=string.atoi( self.cols.get() ) + num
       self.cols.setentry( hsize )

       # Update the color map
       eself.execute2(eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font)

       return "break"

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Show the requested similiar colors                                        #
    #                                                                           #
    #############################################################################
    def execute2(self, eself, parent, vcs_legacy, width, height, borderwidth, color, color_index, font):
        wsize=string.atoi( self.rows.get() )
        hsize=string.atoi( self.cols.get() )
        numIndices = wsize * hsize
        numIndices2 = (wsize * hsize) / 2
        hsize2=height/float( numIndices )
        wpos = (width) / float( wsize )
        wstart = borderwidth
        wend   = wstart + wpos
        hpos = (height) / float( hsize )
        hstart = borderwidth
        hend   = hstart + hpos
        twoffset = (wend - wstart)* 0.5
        thoffset = (hend - hstart)* 0.5
        hstart2 = 0
        hend2 = hsize2
        ct = 1

        self.canvas4.selected_color_index = numIndices2

        rf = 0.0; gf = 0.0; bf = 0.0
        rl = string.atoi( self.R.get() )
        gl = string.atoi( self.G.get() )
        bl = string.atoi( self.B.get() )
        try:
           fltR = abs( rl - rf ) / float(numIndices2-1)
           fltG = abs( gl - gf ) / float(numIndices2-1)
           fltB = abs( bl - bf ) / float(numIndices2-1)
        except:
           return
        colorList2=[]
        for i in range( numIndices2 ):
            if self.dred == 1:
               R = rf+i*fltR
            else:
               R = rl
            if self.dgreen == 1:
               G = gf+i*fltG
            else:
               G = gl
            if self.dblue == 1:
               B = bf+i*fltB
            else:
               B = bl
            colorList2.append(  "#%02x%02x%02x" % ( R*2.55, G*2.55, B*2.55 ) )

        rf =  rl; gf = gl; bf = bl
        rl = psz; gl = psz; bl = psz
        fltR = abs( rl - rf ) / float(numIndices2-1)
        fltG = abs( gl - gf ) / float(numIndices2-1)
        fltB = abs( bl - bf ) / float(numIndices2-1)
        for i in range( numIndices2 ):
            if self.dred == 1:
               R = rf+i*fltR
            else:
               R = rf
            if self.dgreen == 1:
               G = gf+i*fltG
            else:
               G = gf
            if self.dblue == 1:
               B = bf+i*fltB
            else:
               B = bf
            colorList2.append(  "#%02x%02x%02x" % ( R*2.55, G*2.55, B*2.55 ) )

        self.canvas4.bbx = {}
        for i in range( numIndices ):
            try:
                background = colorList2[ i ]
                r, g, b = self.root.winfo_rgb(background)
            except:
                background = (  "#%02x%02x%02x" % ( 255, 255, 255 ) )
                r, g, b = self.root.winfo_rgb(background)
            r =  r/256; g = g/256; b = b/256
            outline = 'black'
            if g <= 179: outline = 'white'
            self.canvas4.create_polygon(wstart, hstart, wend, hstart, wend, hend,
                                       wstart, hend, fill = background, outline = outline)
            self.canvas4.bbx[ i ] = ( (wstart, hstart, wend, hend) , background )
            self.canvas4.create_text(wstart+twoffset, hstart+thoffset, anchor = 'center',
                text = i, font = font, fill=outline )
            hstart2 = hend2
            hend2 += hsize2
            if (ct == wsize):
               wstart = borderwidth
               wend = wstart + wpos
               hstart = hend
               hend += hpos
               ct = 1
            else:
               wstart = wend
               wend += wpos
               ct += 1

    #############################################################################
    #                                                                           #
    # Function:                                                                 #
    # Show the requested similiar colors                                        #
    #                                                                           #
    #############################################################################
    def execute_similiar_color( self, eself, vcs_legacy, result):
        save_color_index = eself.canvas.selected_color_index
        if (result == 'Apply') or (result == 'OK'):
           new_color = self.canvas4.bbx[ self.canvas4.selected_color_index ][ 1 ]
           eself.colorList[ save_color_index ] = new_color
           eself.execute( ) # Update the color map
           eself.canvas.selected_color_index = save_color_index
        else:
           numIndices2 = len( self.canvas4.bbx ) / 2
           new_color = self.canvas4.bbx[ numIndices2 ] [ 1 ]
           eself.colorList[ save_color_index ] = new_color
           eself.execute( ) # Update the color map

        self.canvas.selected_color_index = save_color_index

        self.canvas3.create_polygon(8, 8, 85, 8, 85, 85, 8, 85,
                                   fill = new_color, outline = new_color )
        font = apply(Pmw.logicalfont, ('Helvetica', 5), {'weight' : 'bold'})
        r, g, b = self.root.winfo_rgb( new_color )
        r =  r/256; g = g/256; b = b/256
        outline = 'black'
        if g <= 179: outline = 'white'
        self.canvas3.create_text(43, 45, anchor = 'center', font = font,
                text = self.canvas.selected_color_index, fill=outline )

        # Show the color index values in the R, G, B widgets
        r, g, b = self.root.winfo_rgb( new_color )
        r =  int( r/256./255.*psz ); g = int( g/256./255.*psz ); b = int( b/256./255.*psz)
        if self.cmain_menu.color_mode == 1: r = psz - r; g = psz - g; b = psz - b     # Show CMY value
        self.red_comb.setentry( r )
        self.green_comb.setentry( g )
        self.blue_comb.setentry( b )
        self.red_scl.set( r )
        self.green_scl.set( g )
        self.blue_scl.set( b )


        if (result == 'Dismiss') or (result == 'OK'):
           self.dialog2.destroy()

    #############################################################################
    #                                                                           #
    # Get the VCS colormap values and convert to Hex value names                #
    #                                                                           #
    #############################################################################
    def get_VCS_colormap( self, vcs_legacy ):
        # Get the VCS colormap name and colormap, then create the color list for each index
        cname = vcs_legacy.getcolormapname()
        cmap  = vcs_legacy.getcolormap( cname )
        self.colorList = []
        for i in range(0,256):
           self.colorList.append( "#%02x%02x%02x" % ( (cmap.index[i][0]*2.55),
                                                 (cmap.index[i][1]*2.55),
                                                 (cmap.index[i][2]*2.55) ) )

        # Only do the following if called from the command-line
        # and not in 8-bit pseudo-color mode
        if ((self.top_parent is None) and (self.parent.winfo_depth() != 8)):
           vcs_legacy.canvas.flush()
           vcs_legacy.canvas.backing_store()

        # Update the Editor's frame title
        title = "VCS's Colormap Editor: Editing -- " + cname
        self.dialog.title( title )


    #############################################################################
    #                                                                           #
    # Show the requested colormap on the canvas.                                #
    #                                                                           #
    #############################################################################
    def execute( self ):
        # Get the number of indices, e.g., 1, 55, 256, etc.
        numColors = len(self.color_indices)
        if numColors <=0:
           numColors = 256
           for i in range(0,256): self.color_indices.append(i)
           self.numColors.setentry( '0:255' )

        # This is how I clear both canvases, by setting the background color to white
	self.canvas.create_polygon(0, 0, self.width+self.borderwidth, 0,
                                   self.width+self.borderwidth, self.height+self.borderwidth,
                                   0, self.height+self.borderwidth,
		                   fill = 'white', outline = 'white')
	self.canvas2.create_polygon(0, 0, self.width2, 0,
                                   self.width2, self.height,
                                   0, self.height,
		                   fill = 'white', outline = 'white')

        # Determine the number of rows and columns to create for the colormap.
        i = 1
        j = 2
        wsize = hsize = 1
        hsize2 = self.canvas2.hsize2=self.height/float(numColors)
        while (1):
           if numColors == 1: break
           if (i**2 < numColors) and (j**2 >= numColors):
              wsize = j
              if (numColors < (j**2 -j + 1)):
                 hsize = j -1
              else:
                 hsize = j
              break
           else:
              i += 1
              j += 1

        # Display the specified colormap.
        wpos = (self.width) / float( wsize )
	wstart = self.borderwidth
        wend   = wstart + wpos
        hpos = (self.height) / float( hsize )
	hstart = self.borderwidth
        hend   = hstart + hpos
        twoffset = (wend - wstart)* 0.5
        thoffset = (hend - hstart)* 0.5
        hstart2 = 0
        hend2 = hsize2
        ct = 1
        font = apply(Pmw.logicalfont, ('Helvetica', 0), {'weight' : 'bold'})
        self.canvas.bbx = {}
        self.canvas.clip_board = {}
        self.canvas.b1ct = 0
        self.canvas.b1ct2 = 0
        self.canvas.selected_color_index = 0
	for index in self.color_indices:
	    background = self.colorList[index]
            r, g, b = self.root.winfo_rgb(background)
            r =  r/256; g = g/256; b = b/256
            outline = 'black'
            if g <= 179: outline = 'white'
	    item = self.canvas.create_polygon(wstart, hstart, wend, hstart, wend, hend,
                                       wstart, hend, fill = background, outline = background)
            self.canvas.bbx[ index ] = ( (wstart, hstart, wend, hend) , background )
	    self.canvas2.create_polygon(0, hstart2, self.width2, hstart2, self.width2, hend2,
                                       0, hend2, fill = background, outline = background)
            self.canvas.create_text(wstart+twoffset, hstart+thoffset, anchor = 'center',
		text = index, font = font, fill=outline )
            hstart2 = hend2
            hend2 += hsize2
            if (ct == wsize):
               wstart = self.borderwidth
               wend = wstart + wpos
	       hstart = hend
               hend += hpos
               ct = 1
            else:
	       wstart = wend
               wend += wpos
               ct += 1

###########################################################################################
###########################################################################################
##                                                                                       ##
## Start the classes and functions necessary to operate the VCS colormap editor.         ##
##                                                                                       ##
###########################################################################################
###########################################################################################

###########################################################################################
# The HSV Color Model -
#
# The RGB, CMY, and YIQ models are hardware-oriented. By contrast, Smith's HSV (hue, 
# saturation, value) model (also called HSB model, with B for brightness) is user-
# oriented, being based on the intuitive appeal of the artist's tint. shade, and tone. 
# The coordinate system is cylindrical, and the subset of the space within which the
# model is defined is a hexcone, or six-sided pyramid. (See the Computer Graphics
# Principles and Practice (second Edition) for details on algorithms below.)
#
###########################################################################################
#---------------------------------------------------------------------------------
# Function that coverts RGB to HSV color space.
#---------------------------------------------------------------------------------
def RGB_to_HSV(R, G, B):
   max_rgb = max( R, G, B)
   min_rgb = min( R, G, B)
   V = max_rgb                   # this is the brightness value

   # Now calculate the saturation (i.e., S)
   if max_rgb != 0:
      S = (max_rgb - min_rgb) / max_rgb
   else:
      S = 0                      # Note S is 0 only if R,G, and B are 0

   # Now the hard part! Calculate the hue - H
   if S == 0:
      H = None                   # We'll let this None represent undefined
   else:
      delta = max_rgb - min_rgb
      if R == max_rgb:
         H = (G - B) / delta     # Resulting color is between yellow and magenta
      elif G == max_rgb:
         H = 2 + (B - R) / delta # Resulting color is between cyan and yellow
      elif B == max_rgb:
         H = 4 + (R - G) / delta # Resulting color is between magenta and cyan

      H = H * 60                 # Convert hue to degrees
      if H < 0: H = H + 360      # Make sure the hue is non-negative

   return  H, S, V

#---------------------------------------------------------------------------------
# Function that coverts HSV color space to RGB
#
# Note: psz is the maximum value of the RGB and S and V must be normalized between [0, 1]
#---------------------------------------------------------------------------------
def HSV_to_RGB(H, S, V):
   import math
   if S == 0:                    # The color is on the black-and-white center line
      if H is None:              # Achromatic color: There is no hue
         R = V; G = V; B = V
      else:
         raise vcs_legacyError,  "Cannot convert HSV color space to RGB."
   else:
      if H == 360: H = 0         # 360 degrees is eqqivalent to 0 degrees
      H = H/60                   # H is now in [0,6]
      I = math.floor( H )        # Return the largest intergal value <= H
      F = H - I                  # F is the fractional part of H
      P = V * ( 1 - S )
      Q = V * ( 1 - ( S * F ) )
      T = V * ( 1 - (S * ( 1 - F ) ) )

      if I == 0:
         R, G, B = V, T, P
      elif I == 1:
         R, G, B = Q, V, P
      elif I == 2:
         R, G, B = P, V, T
      elif I == 3:
         R, G, B = P, Q, V
      elif I == 4:
         R, G, B = T, P, V
      elif I == 5:
         R, G, B = V, P, Q
   return R,G,B

#---------------------------------------------------------------------------------
# Event handling function that will allow the passing of arguments
#---------------------------------------------------------------------------------
class E_Command:
   def __init__(self, func, *args, **kw):
      self.func = func
      self.args = args
      self.kw = kw

   def __call__(self, *args, **kw):
      args = self.args + args
      kw.update(self.kw)
      return apply(self.func, args, kw)

#----------------------------------------------------------------------------------------
# Create the colormap File menu and its menu items
#----------------------------------------------------------------------------------------
class create_colormap_file_menu:
   def __init__( self, main_menu, eself, parent, root, vcs_legacy, tear_it ):
      file_name = 'File'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): file_name = 'File '
      main_menu.addmenu(file_name, 'Open/Save VCS Colormap', tearoff = tear_it)
      #
      # Create the "Open File" menu item
      main_menu.addmenuitem(file_name, 'command', 'Open colormap file',
                         label = 'Open Colormap File',
                         command = E_Command(self.evt_open_file, parent, vcs_legacy)
                        )

      main_menu.addmenuitem(file_name, 'separator')

      #
      # Create the cascade "Save Colormap" menu and its items
      main_menu.addmenuitem(file_name, 'command', 'Open colormap file',
                         label = 'Save (i.e., Apply Changes)',
                         command = E_Command(self.evt_save_colormap, eself, parent, vcs_legacy)
                        )
      main_menu.addmenuitem(file_name, 'command', 'Open colormap file',
                         label = 'Save As...',
                         command = E_Command(self.evt_save_as, eself, parent, vcs_legacy)
                        )
      main_menu.addmenuitem(file_name, 'command', 'Open colormap file',
                         label = 'Save To File',
                         command = E_Command(self.evt_save_to_file, eself, parent, vcs_legacy)
                        )

      # Create the cascade "Exit" menu
      main_menu.addmenuitem(file_name, 'separator')
      main_menu.addmenuitem(file_name, 'command', 'Close Colormap Editor',
                          label = "Exit Colormap Editor",
                          command = eself.dialog.destroy
                         )

   # Open VCS file
   #
   def evt_open_file( self, parent, vcs_legacy ):
      # search for the following files
      filetypes = [
        ("VCS script files", "*.scr"),
#        ("Python files", "*.py *.pyw" ),
        ("All files", "*"),
        ]

      # Show the popup directory dialog
      dialog = tkFileDialog.Open(master=parent,
                           filetypes=filetypes,
                           title = 'VCS Colormap File Select')

      dirfilename=dialog.show(initialdir=os.getcwd())

      # Run VCS script
      if len(dirfilename) > 0:
         vcs_legacy.scriptrun(dirfilename)

   # Save VCS colormap under the same name
   #
   def evt_save_colormap( self, eself, parent, vcs_legacy ):
      mode = vcs_legacy.mode
      vcs_legacy.mode = 0
      c_name = vcs_legacy.getcolormapname()
      if c_name == 'default':
         raise vcs_legacyError,  "Cannot modify the default colormap."
      for i in range(len(eself.colorList)):
        if i>239: break # only save first 239 colors, the last row are reserved
        r, g, b = eself.root.winfo_rgb( eself.colorList[ i ] )
        R = int(r/256./255.*psz)
        G = int(g/256./255.*psz)
        B = int(b/256./255.*psz)
        vcs_legacy.setcolorcell( i, R, G, B )
      colormap.copyCp( c_name, c_name )
      if (mode == 1): vcs_legacy.update()
      vcs_legacy.mode = mode

      if parent.winfo_depth() == 8: # for pseudo-color only
         vcs_legacy.setcolormap( c_name )
      else:
         vcs_legacy.canvas.BLOCK_X_SERVER()
         vcs_legacy.canvas.flush()
         eself.get_VCS_colormap( vcs_legacy )
         eself.execute( )
         vcs_legacy.canvas.UNBLOCK_X_SERVER()
 
         try:
            if ( (eself.top_parent is not None) and
                 (eself.top_parent.panelDM.var3 is not None)):
               vcs_legacy_function.re_plot( eself.top_parent, 0 )
         except:
            pass

   # Save VCS colormap as a new name
   #
   def evt_save_as( self, eself, parent, vcs_legacy ):
      create_entry_popup(eself, parent, vcs_legacy, 'sa',('Save VCS colormap ( %s ) as:' % vcs_legacy.getcolormapname()),'Enter the new name of the colormap:\n\tFor example:\n\t\tnewcolormapname')

   # Save VCS Color map to a file
   #
   def evt_save_to_file( self, eself, parent, vcs_legacy ):
      filetypes = [ ("VCS Script File", ".scr") ]
      # Show the popup directory dialog
      sfile = tkFileDialog.asksaveasfilename( master=parent, filetypes = filetypes,
                           title = 'Save VCS colormap to a File' )
      if sfile == '': return
      if sfile[-4:] != '.scr': sfile += '.scr'

      try:
         fp = open( sfile, 'w')
         fp.write( 'C_%s(\n' % vcs_legacy.getcolormapname() )
         for i in range(len(eself.colorList)):
            if i>239: break # only save first 239 colors, last row are reserved
            if i>0: fp.write( ',\n' )
            r, g, b = eself.root.winfo_rgb( eself.colorList[ i ] )
            rf = int(r/256./255.*psz)
            gf = int(g/256./255.*psz)
            bf = int(b/256./255.*psz)
            fp.write( '%g,%g,%g' % (rf, gf,bf) )
         fp.write( ')\n' )
         fp.close()
      except:
         pass


#----------------------------------------------------------------------------------------
# Create the colormap entry popup window to save colormap to output file or save as 
# a new colormap
#----------------------------------------------------------------------------------------
class create_entry_popup:
   def __init__(self, eself, parent, vcs_legacy, type, title, text, entry_str = ''):
        self.parent = parent
        self.type = type
        self.dialog = Pmw.Dialog( parent,
            title = title,
            buttons = ('OK', 'Dismiss'),
            defaultbutton = 'OK',
            command = E_Command(self.execute, eself, parent, vcs_legacy) )

        self.dialog.transient( self.parent ) # draw widget on top of its parent

        lbl=Tkinter.Label(self.dialog.interior(),
            text = text,
            justify = 'left',
            anchor = 'w',
            )
        lbl.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        self.eny1=Pmw.EntryField(self.dialog.interior(),
            labelpos = 'w',
            entry_background = 'white',
            entry_foreground = 'black',
            value = entry_str,
            entry_width =  50,
            )
        self.eny1.pack( expand = 1, fill = 'both', padx=10, pady=5 )

        # Position dialog popup
        parent_geom = eself.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.dialog.geometry( "+%d+%d" % (d1, d2) )

   # Save, copy, and write colormap to a file
   #
   def execute(self, eself, parent, vcs_legacy, result):
      R = []; G = []; B = []
      if result == 'OK':
         if self.eny1.get( ) == '':
            pass
         else:
            if self.type == 'sa':
               for i in range(len(eself.colorList)):
                 if i>239: break # only save first 239 colors, the last row are reserved
                 r, g, b = eself.root.winfo_rgb( eself.colorList[ i ] )
                 R.append( int(r/256./255.*psz) )
                 G.append( int(g/256./255.*psz) )
                 B.append( int(b/256./255.*psz) )
               colormap.copyCp( vcs_legacy.getcolormapname(), self.eny1.get( ) )
               vcs_legacy.canvas.flush()
               vcs_legacy.setcolormap( self.eny1.get() )
               eself.get_VCS_colormap( vcs_legacy )
               vcs_legacy.setcolormap( self.eny1.get() )
               for i in range(len(eself.colorList)):
                 if i>239: break # only save first 239 colors, the last row are reserved
                 vcs_legacy.setcolorcell( i, R[i], G[i], B[i] )
                 eself.colorList[i] = "#%02x%02x%02x" % ( R[i]*2.55, G[i]*2.55, B[i]*2.55 )
               vcs_legacy.update()

      self.dialog.destroy()


#----------------------------------------------------------------------------------------
# Create the Colormap Edit menu and its menu items
#----------------------------------------------------------------------------------------
class create_edit_menu:
   def __init__( self, eself, main_menu, parent, vcs_legacy, tear_it ):
      edit_name = 'Edit'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): edit_name = 'Edit '
      # Initialize the Edit pull down:
      main_menu.addmenu(edit_name, 'Edit VCS Colormap', tearoff = tear_it)

      main_menu.addmenuitem(edit_name, 'command', 'copy color indices',
                       label = 'Blend Colors',
                       command = E_Command(self.evt_blend_colors, eself, parent, vcs_legacy)
                      )

      main_menu.addmenuitem(edit_name, 'separator')

      main_menu.addmenuitem(edit_name, 'command', 'copy color indices',
                       label = 'Copy',
                       command = E_Command(self.evt_copy_selected_colors, eself, parent, vcs_legacy)
                      )

      main_menu.addmenuitem(edit_name, 'command', 'paste color indices',
                       label = 'Paste',
                       command = E_Command(self.evt_paste_selected_indices, eself, parent, vcs_legacy)
                      )

   def evt_blend_colors( self, eself, parent, vcs_legacy ):
      numIndices = len(eself.canvas.clip_board)
      if numIndices <= 1: return

      f_index = eself.canvas.clip_board[0][1]
      start_index = eself.canvas.clip_board[0][0]
      r, g, b = eself.root.winfo_rgb( eself.colorList[ start_index ] )
      rf =  int(r/256./255.*psz); gf = int(g/256./255.*psz); bf = int(b/256./255.*psz)
      l_index = eself.canvas.clip_board[numIndices-1][1]
      end_index = eself.canvas.clip_board[numIndices-1][0]
      r, g, b = eself.root.winfo_rgb( eself.colorList[ end_index ] )
      rl =  int(r/256./255.*psz); gl = int(g/256./255.*psz); bl = int(b/256./255.*psz)

      fltR = abs( rl - rf ) / float(numIndices-1)
      fltG = abs( gl - gf ) / float(numIndices-1)
      fltB = abs( bl - bf ) / float(numIndices-1)

      if (eself.canvas.direction) > 0:
         rd = 1
         if (rl - rf) < 0: rd = 0
         gd = 1
         if (gl - gf) < 0: gd = 0
         bd = 1
         if (bl - bf) < 0: bd = 0
      else:
         rd = 0 
         if (rl - rf) < 0: rd = 1
         gd = 0
         if (gl - gf) < 0: gd = 1
         bd = 0
         if (bl - bf) < 0: bd = 1

      cms_colors = []
      for i in range(numIndices):
          if (eself.canvas.direction) > 0:
             if (rd == 1):
                R = rf+i*fltR
             else:
                R = rf-i*fltR
             if (gd == 1):
                G = gf+i*fltG
             else:
                G = gf-i*fltG
             if (bd == 1):
                B = bf+i*fltB
             else:
                B = bf-i*fltB
          else:
             if (rd == 1):
                R = rf-i*fltR
             else:
                R = rf+i*fltR
             if (gd == 1):
                G = gf-i*fltG
             else:
                G = gf+i*fltG
             if (bd == 1):
                B = bf-i*fltB
             else:
                B = bf+i*fltB

          new_color = "#%02x%02x%02x" % ( R/psz*255, G/psz*255, B/psz*255 )
          if (eself.canvas.direction) > 0:
             eself.colorList[ start_index+i ] = new_color
          else:
             eself.colorList[ start_index-i ] = new_color

      # Update the color map
      eself.execute( )
          

   def evt_copy_selected_colors( self, eself, parent, vcs_legacy ):
      numIndices = len(eself.canvas.clip_board)

      eself.copy_buffer = []
      for i in range( numIndices ):
          eself.copy_buffer.append( eself.canvas.clip_board[i][1][1] )

      # Update the color map
      eself.execute( )

   def evt_paste_selected_indices( self, eself, parent, vcs_legacy ):
      numIndices = len( eself.copy_buffer )
      start_index = eself.canvas.clip_board[0][0]
      for i in range( numIndices ):
          eself.colorList[ start_index + i ] = eself.copy_buffer[ i ]

      # Update the color map
      eself.execute( )

#----------------------------------------------------------------------------------------
# Create the Colormap Options menu and its menu items
#----------------------------------------------------------------------------------------
class create_options_menu:
   def __init__( self, eself, main_menu, parent, vcs_legacy, tear_it ):
      opt_name = 'Options'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): opt_name = 'Options   '
      main_menu.addmenu(opt_name, 'Set VCS Colormap Preferences', tearoff = tear_it)

      main_menu.color_mode = 0
      main_menu.color_mode_toggle=[]
      for i in range(3):
         main_menu.color_mode_toggle.append( Tkinter.IntVar() )
         main_menu.color_mode_toggle[i].set( 0 )
      main_menu.color_mode_toggle[ 0 ].set( 1 )

      main_menu.addcascademenu(opt_name, 'Colormodel', 'VCS Colormap Mode',
                label = "Color Model",
                traverseSpec = 'z', tearoff = tear_it
              )
      main_menu.addmenuitem('Colormodel', 'checkbutton', 'Toggle, Colormap Model',
                          label = 'RGB Mode',
                          selectcolor=gui_support.gui_color.one,
                          variable = main_menu.color_mode_toggle[0],
                          command = E_Command(self.evt_color_mode, eself, main_menu, parent, 0)
                         )
      main_menu.addmenuitem('Colormodel', 'checkbutton', 'Toggle, Colormap Model',
                          label = 'CMY Mode',
                          selectcolor=gui_support.gui_color.one,
                          variable = main_menu.color_mode_toggle[1],
                          command = E_Command(self.evt_color_mode, eself, main_menu, parent, 1)
                         )
      main_menu.addmenuitem('Colormodel', 'checkbutton', 'Toggle, Colormap Model',
                          label = 'HSV Mode',
                          selectcolor=gui_support.gui_color.one,
                          variable = main_menu.color_mode_toggle[2],
                          command = E_Command(self.evt_color_mode, eself, main_menu, parent, 2)
                         )

      main_menu.addmenuitem(opt_name, 'separator')


      main_menu.addmenuitem(opt_name, 'command', 'view colormap values',
                       label = 'Select Colormap',
                       command = E_Command(self.evt_select_colormap, eself, parent, vcs_legacy)
                      )
      main_menu.addmenuitem(opt_name, 'command', 'view colormap values',
                       label = 'Delete Colormap',
                       command = E_Command(self.evt_delete_colormap, eself, parent, vcs_legacy)
                      )
      main_menu.addmenuitem(opt_name, 'command', 'view colormap values',
                       label = 'View Colormap Values',
                       command = E_Command(self.evt_view_colormap_values, eself, parent, vcs_legacy)
                      )

   # Update the color model look and feel
   def evt_color_mode(self, eself, main_menu, parent, color_mode ):
      # Set the toggles
      for i in range(len(main_menu.color_mode_toggle)):
          main_menu.color_mode_toggle[i].set( 0 )
      main_menu.color_mode_toggle[color_mode].set( 1 )
      prev_color_mode = main_menu.color_mode
      main_menu.color_mode = color_mode

      if color_mode == 0:
         eself.frameb_hsv.pack_forget( ); eself.framec_hsv.pack_forget( ); eself.framed_hsv.pack_forget( )
         eself.red_comb.component( 'label' ).configure( text = 'Red:' )
         eself.green_comb.component( 'label' ).configure( text = 'Green:' )
         eself.blue_comb.component( 'label' ).configure( text = 'Blue:' )
	 Pmw.alignlabels((eself.red_comb, eself.green_comb, eself.blue_comb))

         eself.red_comb.component( 'entry' ).configure( background = 'red' )
         eself.green_comb.component( 'entry' ).configure( background = 'green' )
         eself.blue_comb.component( 'entry' ).configure( background = 'blue' )
         eself.blue_comb.component( 'entry' ).configure( foreground = 'white' )

         eself.red_comb.component( 'listbox' ).configure( background = 'red' )
         eself.green_comb.component( 'listbox' ).configure( background = 'green' )
         eself.blue_comb.component( 'listbox' ).configure( background = 'blue', foreground='white' )
         eself.blue_comb.component( 'scrolledlist' ).component( 'listbox' ).configure( background = 'blue', foreground='white' )

         eself.red_comb.component( 'arrowbutton' ).configure( background = 'red' )
         eself.green_comb.component( 'arrowbutton' ).configure( background = 'green' )
         eself.blue_comb.component( 'arrowbutton' ).configure( background = 'blue' )

         eself.red_scl.configure( troughcolor = 'red' )
         eself.green_scl.configure( troughcolor = 'green' )
         eself.blue_scl.configure( troughcolor = 'blue' )

         eself.frameb.pack( side='top', fill='both' )
         eself.framec.pack( side='top', fill='both' )
         eself.framed.pack( side='top', fill='both' )
      elif color_mode == 1:
         eself.frameb_hsv.pack_forget( ); eself.framec_hsv.pack_forget( ); eself.framed_hsv.pack_forget( )
         eself.red_comb.component( 'label' ).configure( text = 'Cyan:' )
         eself.green_comb.component( 'label' ).configure( text = 'Magenta:' )
         eself.blue_comb.component( 'label' ).configure( text = 'Yellow:' )
	 Pmw.alignlabels((eself.red_comb, eself.green_comb, eself.blue_comb))

         eself.red_comb.component( 'entry' ).configure( background = 'cyan' )
         eself.green_comb.component( 'entry' ).configure( background = 'magenta' )
         eself.blue_comb.component( 'entry' ).configure( background = 'yellow' )
         eself.blue_comb.component( 'entry' ).configure( foreground = 'black' )

         eself.red_comb.component( 'listbox' ).configure( background = 'cyan' )
         eself.green_comb.component( 'listbox' ).configure( background = 'magenta' )
         eself.blue_comb.component( 'listbox' ).configure( background = 'yellow', foreground='black' )
         eself.blue_comb.component( 'scrolledlist' ).component( 'listbox' ).configure( background = 'yellow', foreground='black' )

         eself.red_comb.component( 'arrowbutton' ).configure( background = 'cyan' )
         eself.green_comb.component( 'arrowbutton' ).configure( background = 'magenta' )
         eself.blue_comb.component( 'arrowbutton' ).configure( background = 'yellow' )


         eself.red_scl.configure( troughcolor = 'cyan' )
         eself.green_scl.configure( troughcolor = 'magenta' )
         eself.blue_scl.configure( troughcolor = 'yellow' )

         eself.frameb.pack( side='top', fill='both' )
         eself.framec.pack( side='top', fill='both' )
         eself.framed.pack( side='top', fill='both' )
      elif color_mode == 2:
         eself.frameb.pack_forget( ); eself.framec.pack_forget( ); eself.framed.pack_forget( )

         eself.frameb_hsv.pack( side='top', fill='both' )
         eself.framec_hsv.pack( side='top', fill='both' )
         eself.framed_hsv.pack( side='top', fill='both' )

      # Update the RGB/CYM selections
      # Get the current color index values in the (RGB or CMY)  scale widgets
      if color_mode == prev_color_mode: return  # Do nothing
      if color_mode in [0,1]:
         if prev_color_mode != 2:
            r = eself.red_scl.get( )
            g = eself.green_scl.get( )
            b = eself.blue_scl.get( )
            r = psz - r; g = psz - g; b = psz - b     # Convert color values
         else:
            h = float(eself.hue.get( )); s = float(eself.saturation.get( )); v = float(eself.value.get( ))
            if h < 0.0: h = None
            r, g, b = HSV_to_RGB( h, s, v )
            r*=psz; g*=psz; b*=psz
            if color_mode == 1:
               r = psz - r; g = psz - g; b = psz - b     # Convert color values
   
         eself.red_comb.setentry( r )
         eself.green_comb.setentry( g )
         eself.blue_comb.setentry( b )
         eself.red_scl.set( r )
         eself.green_scl.set( g )
         eself.blue_scl.set( b )
      else:
         r = float(eself.red_scl.get( )); g = float(eself.green_scl.get( )); b = float(eself.blue_scl.get( ))
         if prev_color_mode == 1: r = psz - r; g = psz - g; b = psz - b
         r/=psz; g/=psz; b/=psz
         h, s, v = RGB_to_HSV( r, g, b )
         if h is not None:
            eself.hue.setentry( h )
         else:
              eself.hue.setentry( -0.01 ) # This represents None or Undefined
         eself.saturation.setentry( s )
         eself.value.setentry( v )

   # Set to a new colormap
   def evt_select_colormap ( self, eself, parent, vcs_legacy ):
      c_names = vcs_legacy.listelements('colormap')
      c_names.sort()
      cname = vcs_legacy.getcolormapname()

      # Create the dialog.
      self.dialog = Pmw.ComboBoxDialog( eself.root,
	    title = 'Select VCS Colormap',
	    buttons = ('OK', 'Apply', 'Cancel'),
	    defaultbutton = 'OK',
	    combobox_labelpos = 'n',
            entry_background = 'white',
            entry_foreground = 'black',
	    label_text = 'Enter or Select VCS Colormap:',
	    scrolledlist_items = c_names,
            command = E_Command(self.execute_colormap_selection, eself, vcs_legacy) )
      self.dialog.setentry( cname )

      self.dialog.transient( eself.root ) # draw widget on top of its parent

      # Position dialog popup
      parent_geom = eself.dialog.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      self.dialog.geometry( "+%d+%d" % (d1, d2) )

   # Set the colormap selection
   def execute_colormap_selection ( self, eself, vcs_legacy, result ):
      if (result == 'OK') or (result == 'Apply'):
         vcs_legacy.canvas.BLOCK_X_SERVER()
         vcs_legacy.canvas.flush()
         vcs_legacy.setcolormap( self.dialog.get() )
         eself.get_VCS_colormap( vcs_legacy )
         eself.execute( )
         vcs_legacy.canvas.UNBLOCK_X_SERVER()
         try:
            if ( (eself.top_parent is not None) and
                 (eself.top_parent.panelDM.var3 is not None)):
               eself.top_parent.panelGC.evt_plot( eself.top_parent )
               #vcs_legacy_function.re_plot( eself.top_parent, 0 )
         except:
            pass
      if result == 'OK':
         self.dialog.destroy()
      elif result == 'Apply':
         pass
      elif result in ['Cancel', None]:
         self.dialog.destroy()

   # Delete colormap
   def evt_delete_colormap ( self, eself, parent, vcs_legacy ):
      c_names = vcs_legacy.listelements('colormap')
      c_names.sort()
      cname = vcs_legacy.getcolormapname()

      # Create the dialog.
      self.dialog = Pmw.ComboBoxDialog( eself.root,
            title = 'Delete VCS Colormap',
            buttons = ('OK', 'Apply', 'Cancel'),
            defaultbutton = 'OK',
            combobox_labelpos = 'n',
            entry_background = 'white',
            entry_foreground = 'black',
            label_text = 'Enter or Select VCS Colormap to be Deleted:',
            scrolledlist_items = c_names,
            command = E_Command(self.execute_colormap_deletion, eself, vcs_legacy) )
      self.dialog.setentry( cname )

      # Position dialog popup
      parent_geom = eself.dialog.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      self.dialog.geometry( "+%d+%d" % (d1, d2) )

   # Remove the colormap
   def execute_colormap_deletion ( self, eself, vcs_legacy, result ):
      if (result == 'OK') or (result == 'Apply'):
         if self.dialog.get() == 'default':
            raise vcs_legacyError,  "Cannot remove the default colormap."
         elif self.dialog.get() == vcs_legacy.getcolormapname():
            raise vcs_legacyError,  "Cannot remove the currently used colormap."
         colormap.removeCp( self.dialog.get() )
         c_names = vcs_legacy.listelements('colormap')
         c_names.sort()
         self.dialog.setlist( c_names )

      if result == 'OK':
         self.dialog.destroy()
      elif result == 'Apply':
         pass
      elif result in ['Cancel', None]:
         self.dialog.destroy()

   # View colormap values
   def evt_view_colormap_values ( self, eself, parent, vcs_legacy ):
      cname = vcs_legacy.getcolormapname()

      # Create the dialog text widget
      dialog = Pmw.TextDialog(eself.root, scrolledtext_labelpos = 'n',
		title = 'VCS Colormap Values',
                buttons = ('Dismiss',),
                defaultbutton = 'Dismiss')

      if eself.cmain_menu.color_mode == 0:
	 dialog.configure( label_text = ('( ' + cname + ' ) Colormap R, G, B Intensity Values') )
      elif eself.cmain_menu.color_mode == 1:
         dialog.configure( label_text = ('( ' + cname + ' ) Colormap C, M, Y Intensity Values') )
      elif eself.cmain_menu.color_mode == 2:
         dialog.configure( label_text = ('( ' + cname + ' ) Colormap H, S, V Intensity Values') )

      dialog.transient( eself.root ) # draw widget on top of its parent

      # Position dialog popup
      parent_geom = eself.dialog.geometry()
      geom = string.split(parent_geom, '+')
      d1 = string.atoi( geom[1] )
      d2 = string.atoi( geom[2] )
      dialog.geometry( "350x500+%d+%d" % (d1, d2) )


      # Get the VCS colormap name and colormap, then create the color list for each index
      cmap  = vcs_legacy.getcolormap( cname )
      for i in range(0,256):
         r_cmap = (cmap.index[i][0])/100.*psz; g_cmap = (cmap.index[i][1])/100.*psz; b_cmap = (cmap.index[i][2])/100.*psz
         if eself.cmain_menu.color_mode == 0:
            tex = "index (%g):       [ %g, %g, %g ]\n" % (i, r_cmap, g_cmap, b_cmap)
         elif eself.cmain_menu.color_mode == 1:
            tex = "index (%g):       [ %g, %g, %g ]\n" % (i, psz-r_cmap, psz-g_cmap, psz-b_cmap)
         elif eself.cmain_menu.color_mode == 2:
            r = r_cmap/psz; g = g_cmap/psz; b = b_cmap/psz
            h, s, v = RGB_to_HSV( r, g, b )
            tex = "index (%g):       [ %s, %g, %g ]\n" % (i, h, s, v)
         dialog.insert( 'end', tex )
      dialog.configure(text_state = 'disabled')


#----------------------------------------------------------------------------------------
# Create the colormap Help menu and its menu items
#----------------------------------------------------------------------------------------
class create_colormap_help_menu:
   def __init__( self, main_menu, eself, parent, tear_it ):
      help_name = 'Help'
      if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)): help_name = 'Help '
      main_menu.addmenu(help_name, 'VCS Colormap Help', side='right', tearoff = tear_it)
      gui_support.add_balloon_help(main_menu, help_name)
      main_menu.addmenuitem(help_name, 'separator')

      main_menu.addmenuitem(help_name, 'command', 'Help About',
                            label = 'About the Colormap Editor',
                            command = E_Command(self.evt_about_dialog, eself, parent)
                           )
   # Create about colormap dialog.
   def evt_about_dialog( self, eself, parent ):
        Pmw.aboutversion(sys.prefix)
        Pmw.aboutcopyright('\nCopyright:    2001, Regents of the University of California\nThis software may not be distributed to others without\npermission of the authors.\nAll rights reserved.')
        Pmw.aboutcontact(
            """Go to cdat.sourceforge.net for documentation, support, bug reporting, and releases.  
Program for Climate Model Diagnosis and Intercomparison
Lawrence Livermore National Laboratory Livermore, CA 94550 """)
        self.about = Pmw.AboutDialog(parent, applicationname = 'The Visualization Control System - (VCS) Colormap Editor')
        self.about.transient( eself.root ) # draw widget on top of its parent
        parent_geom = eself.dialog.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
        self.about.geometry( "+%d+%d" % (d1, d2) )

# Create/Popup colormap gui editor for VCS.
def create(canvas=None, gui_parent=None, transient=0, max_intensity=None):
    global psz
    if (max_intensity is not None) and (type(max_intensity) in [types.FloatType, types.IntType]):
       psz = float( max_intensity )
    else:
       psz = 100.0
    ColormapGui(canvas, gui_parent, transient)

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
