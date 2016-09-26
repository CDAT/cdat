#####################################################################
#####################################################################
#                                                                   #
#  File:  OverviewCanvas.py                                         #
#  Date:  05-May-2006                                               #
#  Desc:  Canvas for CDAT overview.                                 #
#                                                                   #
#####################################################################
#####################################################################

import Tkinter
import Pmw
import sys
import os

def overviewCanvas (canvas):

    text = Tkinter.Label (canvas.interior(),
                          bg='white',
                          width=canvas['hull_width'],
                          )

    # Pass the text object to Canvas.create_window().
    win = canvas.create_window (0,0, window=text)


    lab_00 = Tkinter.Label (text,
                            bg='white',
                            justify='left',
                            wraplength=text['width'],
                            text='\
CDAT makes use of an open-source, object-oriented, easy-to-learn \
scripting language (Python) to link together separate software \
subsystems and packages to form an integrated environment for data \
analysis. Outside collaborators work independently and contribute \
on an equal basis with PCMDI.')
    lab_00.grid(row=0, sticky='w')


    fname = os.path.join (sys.exec_prefix, 'bin/images', 'CDAT_modularity.gif')
    logo_01 = Tkinter.PhotoImage (file = fname)
    lab_01 = Tkinter.Label (text,
                            relief='sunken',
                            bg='white',
                            image=logo_01)
    lab_01.image = logo_01
    lab_01.grid(row=1, columnspan=2, sticky='w')

    
    lab_02 = Tkinter.Label (text,
                            bg='white',
                            justify='left',
                            wraplength=text['width'],
                            text='\
CDAT\'s  major subsystems are:\n\n\
    * cdms - Climate Data Management System (file I/O,  variables, \
types, metadata, grids)\n\
    * cdutil - Climate Data Specific Utilities (spatial and \
temporal averages, custom seasons, climatologies)\n\
    * genutil - General Utilities (statistical and other \
convenience functions)\n\
    * numPy - Numerical Python (large-array numerical operations)\n\
    * vcs  - Visualization and Control System  (manages graphical \
window: picture template, graphical methods, data)')
    lab_02.grid(row=2, sticky='w')
    

    fname = os.path.join (sys.exec_prefix, 'bin/images', 'CDAT_architecture.gif')
    logo_03 = Tkinter.PhotoImage (file = fname)
    lab_03 = Tkinter.Label (text,
                            relief='sunken',
                            bg='white',
                            image=logo_03)
    lab_03.image = logo_03
    lab_03.grid(row=3, columnspan=3, sticky='w')


    lab_04 = Tkinter.Label (text,
                            bg='white',
                            justify='left',
                            wraplength=text['width'],
                            text='\
VCDAT is the graphical user interface for CDAT and helps users \
become familiar with CDAT by translating every button press \
and keystroke into Python scripts. VCDAT does not require \
learning Python and the CDAT software.')
    lab_04.grid(row=4, sticky='w')







    
