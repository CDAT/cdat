#####################################################################
#####################################################################
#                                                                   #
#  File:  MainMenu.py                                               #
#  Date:  16-Mar-2006                                               #
#  Desc:  Form for the main menu of the CDAT demo application.      #
#                                                                   #
#####################################################################
#####################################################################

import Tkinter
import Pmw
import sys
import os

import DemoSet
import About

#############################################################
# Class: MainMenu                                           #
# Descr: The main for of the CDAT demo app.                 #
#############################################################
class MainDisplay (Tkinter.Frame):
        
    def __init__ (self, master, background_fname, buttons, onBtnLeave):
        
        Tkinter.Frame.__init__ (self, master)
        
        # Add the background image.
        self._bgImage = Tkinter.PhotoImage (file = background_fname)        
        self._bgPhoto = Tkinter.Label (self, image = self._bgImage, relief='sunken')
        self._bgPhoto.grid (rowspan=40, columnspan=11)

        # Create buttons.
        self._btn = {}
        for name, col, row, cback, enter in buttons:
            self._btn [name] = Tkinter.Button (self, width = 20, text = name, command = cback)
            self._btn [name].bind ('<Enter>', enter)
            self._btn [name].bind ('<Leave>', onBtnLeave)
            self._btn [name].grid (row=row, column=col, sticky='w')
        
    def _onDemoClose (self, type):
        self._demos[type].withdraw()

    def getWidth (self):
        return self._bgImage.width()

    def getHeight (self):
        return self._bgImage.height()
    
    def getButton (self, name):
        return self._btn[name]
    

        

