/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


%pythoncode %{

gVisusGuiMaster = VisusFLTKGui()
try: 
  import gui_support # See whether the vcdat gui is running
except:
  pass

def createWindow(root):
  global gVisusGuiMaster

  window = gVisusGuiMaster.createWindow(root)

  # If this is the first active window we create 
  if (window == 0):
    try: 
      # If vcdat is running we attch ourself to the tkinter mainloop  
      gui_support.root().after_idle(_fltkIdle,gVisusGuiMaster) 
        
    except: # If vcdat is not running
      pass

  return gVisusGuiMaster.window(window)

def _fltkIdle(master_gui):

  active = master_gui.update();
  gui_support.root().update_idletasks()
  gui_support.root().update()

  if active:
    gui_support.root().after_idle(_fltkIdle,master_gui)
        

def startFLTKMainLoop():
  """
This function will start the fltk main loop. It allows to use pyvisus
from python in a standalone fashion. Note that this function will only
return once all pyvisus windows are closed. Furthermore, the mainloop
must be run from the main thread of the program as it will not process
events correctly when running in a child process.
"""
  
  gVisusGuiMaster.mainLoop()
  return

  if gui_support.root_exists():      
    gui_support.root().mainloop()
  else: 
    gVisusGuiMaster.mainLoop()

  
#
# This is code that works great without vcdat but not with :-(
#_gVisusGuiMaster = VisusFLTKThread()
#_gVisusGuiMaster.runInBackground()
#  
# 
#def createWindow(root):
#  global _gVisusGuiMaster
#
#  _gVisusGuiMaster.createWindow(root)
#
#def _fltkIdle(master_gui):
#
#  active = master_gui.update();
#  gui_support.root().update_idletasks()
#  gui_support.root().update()
#
#  if active:
#    gui_support.root().after_idle(_fltkIdle,master_gui)
#       
#
  
%}

