"""FTP Progress dialog"""

from wxPython.wx import *
from cdms import cache

_progressParent = None                  # Parent frame of progress gui

def setProgressParent(parent):
    """
    Enable the FTP progress GUI, and set the parent frame.
    Usage: setProgressParent(parent)
    """
    global _progressParent
    _progressParent = parent
    cache.useWindow()                   # Notify cache module that window dialogs should be used.

def getProgressParent():
    """
    Get the parent frame for the FTP progress dialog.
    Usage: getProgressParent()
    """
    return _progressParent

def updateProgressGui(blocknum, blocksize, size, prog):
    """
    Callback function for the FTP progress dialog.
    Usage: updateProgressGui(blocknum, blocksize, size, prog)
    <blocknum> is the 0-origin block number
    just read. <blocksize> is in bytes. <size> is the file size in bytes.
    <prog> is the progress dialog.

    Return: 0 to signal that a cancel has been received, 1 to continue reading.
    """
    sizekb = size/1024L
    percent = min(100,int(100.0*float(blocknum*blocksize)/float(size)))
    if percent<100:
        noInterrupt = prog.Update(percent,"Read: %3d%% of %dK"%(percent,sizekb))
    else:
        noInterrupt = 1                    # Don't interrupt - finish up cleanly
        prog.Destroy()
    if noInterrupt==0:
        prog.Destroy()
    return noInterrupt

class CdProgressDialog(wxProgressDialog):

    # <frame> is the parent frame.
    # filename is the file being read.
    def __init__(self, frame, filename):
        wxProgressDialog.__init__(self,"FTP: %s"%filename,
                         "Connecting ...",
                         100,
                         frame,
                         wxPD_CAN_ABORT | wxPD_APP_MODAL | wxPD_REMAINING_TIME)

