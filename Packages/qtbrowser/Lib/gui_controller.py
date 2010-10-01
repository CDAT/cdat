from PyQt4 import QtGui, QtCore
import vcs
import os

class GuiController(QtCore.QObject):
    """ GuiController calls vistrails functions and handles recording and
    displaying teaching commands.

    The two most important functions that GuiController provides are:
    'createModule' and 'updateModule'.  'createModule' creates a new box.
    'updateModule' updates the input of a box.  Widgets interact with
    GuiController by sending signals (which is why GuiController inherits
    from QObject).
    """

    def __init__(self, fileWidget, defVarWidget, varWidget):
        """ __init__(fileWidget: QCDATFileWidget, defVarWidget:QDefinedVariable,
                     varWidget: QVariableView)
        """
        QtCore.QObject.__init__(self)
        self.teachingCommands = ''
        self.editorPid = 0
        self.recordCommands = True


        # X coordinates of open, variable, and plot related modules
        self.openX = -300
        self.variableX = -300
        self.plotX = -320
        
        self.m_open = None 
        self.m_variable = None 
        self.m_graphics_method = None 
        self.m_cdat_cell = None


    def plot(self, var1, var2):
        """ Connect / disconnect the necessary ports and exec workflow -> plot
        into the cell
        """
        print 'plotting !',var1,var2
        return
    

    def initTeachingCommands(self):
        """  The initial teaching commands still have 4 canvases like the old
        vcdat.  This allows you to run the teaching commands independently
        of vistrails' spreadsheets.
        """
        self.teachingCommands += 'import cdms2, vcs, cdutil, genutil, os, sys\n'
        self.teachingCommands += 'import MV2\n'

        self.teachingCommands += '\n# Initialize the four VCS Canvases by creating\n'
        self.teachingCommands += '# a list to hold the 4 VCS Canvas\n'
        self.teachingCommands += 'vcs_canvas_list = []\n'

        self.teachingCommands += '\n# Loop (from 0 to 3) to create VCS Canvas 1, 2, 3, and 4\n'
        self.teachingCommands += 'for i in range(4):\n'
        self.teachingCommands += '    vcs_canvas_list.append(vcs.init())\n'

        self.teachingCommands += '\n# Set the Command Line VCS Canvas hooks\n'
        self.teachingCommands += 'vcs_hook1 = vcs_canvas_list[0]\n'
        self.teachingCommands += 'vcs_hook2 = vcs_canvas_list[1]\n'
        self.teachingCommands += 'vcs_hook3 = vcs_canvas_list[2]\n'
        self.teachingCommands += 'vcs_hook4 = vcs_canvas_list[3]\n'

        self.writeTeachingCommands()

    def recordTeachingCommand(self, command):
        if (self.recordCommands == True):
            self.teachingCommands += command
            self.writeTeachingCommands()

    def setRecordCommands(self, recordCommands):
        self.recordCommands = recordCommands

    def writeTeachingCommands(self):
        try:
            fn = '%s/PCMDI_GRAPHICS' % os.environ['HOME']
        except:
            print "Could not find the $HOME directory. Set your environment variable 'HOME'"
            print "to your home directory. (e.g., 'setenv HOME /home/user')."
            sys.exit()
        
        # Create PCMDI_GRAPHICS directory if it does not exist                                  
        if (os.access(fn, os.X_OK) == 0):
            try:
                os.mkdir(fn)
            except:
                print 'Do not have write permission for home directory. Must have write permissions.'
                sys.exit()

        # Write teaching commands to vcdat_recording_script_file.py
        self.teachingScript = fn + '/vcdat_recording_script_file.py'
        file = open(self.teachingScript, 'w')
        file.write(self.teachingCommands)
        file.flush()

    def viewTeachingCommands(self):
        """ Open the teaching commands script in a child process """
        self.editorPid = os.fork()

        if (self.editorPid == 0):
            editor = 'idle'

            # If idle editor is found, view teaching commands with idle
            for path in os.environ["PATH"].split(os.pathsep):
                file = os.path.join(path, editor)
                if (os.path.exists(file)):
                    args = (editor, self.teachingScript)
                    os.execvp(editor, args)
                    return

            # If idle editor is not found, use default editor
            secondaryEditor = os.environ['EDITOR']
            args = (secondaryEditor, self.teachingScript)
            os.execvp(secondaryEditor, args)

    def closeTeachingCommands(self):
        if (self.editorPid != 0):
            os.kill(self.editorPid, 9)
            self.editorPid = 0
