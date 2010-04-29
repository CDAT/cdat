import os, sys, time
import cdat_info
import Tkinter, Pmw
import gui_color
if not os.environ.has_key('HOME'):
    print """
The environment variable HOME must be set in order to run CDAT.
A directory PCMDI_GRAPHICS will be created in $HOME to hold 
the permanent state of CDAT.
"""
    raise SystemExit, 1
fn = os.path.join(os.environ['HOME'], 'PCMDI_GRAPHICS')
sys.path.append(fn)


def buildDate():
    """ Returns the date python was built, and the date VCDAT was built """
    fnm = os.path.join(sys.prefix,'bin','python')
    a=os.stat(fnm)[8]
    p=time.asctime(time.localtime(a))
    fnm = os.path.join(sys.prefix,'bin','cdat')
    a=os.stat(fnm)[8]
    return p, time.asctime(time.localtime(a))

def version():
    "Return the version number from the cdat_info script"
    v=[]
    for nb in cdat_info.version():
        v.append(str(nb))
    return ".".join(v)

balloons_on = 'balloon'  # alternates are status, both
balloons_off = 'none'
_dialogs=[]
balloon=None
# Manage just one balloon flag for all dialogs
def balloon_state():
    "Return the state of the global balloon setting."
    if  _balloon_var.get():
        return balloons_on
    else:
        return balloons_off

def toggle_balloon_state ():
    "Set the balloon help flag."
    balloon.configure(state=balloon_state())


# One common root for all dialogs
_root = None
def root ():
    "Return the root"
    global _root, _balloon_var, balloon
    if _root is None:
        # Use the localhost:0.0 for the DISPLAY and screen number
        try:
           _root = Pmw.initialise()
        except:
           _root = Pmw.initialise(root=Tkinter.Tk(":0.0"))
        _root.withdraw()
        balloon = Pmw.Balloon(_root)
        _balloon_var = Tkinter.IntVar(_root)
        try:
            import vcdat_initial
            iebf = getattr(vcdat_initial.set, 'enable_balloons_flg', 1)
        except ImportError:
            iebf = 1
        _balloon_var.set(iebf)
        if iebf:
            balloon.configure(state=balloons_on)
        else:
            balloon.configure(state=balloons_off)
        _root.balloon = balloon
    return _root

def root_exists():
    "Does the root window exist yet?"
    return _root is not None

def set_root (root):
    "Set the root externally."
    global _root
    if root_exists():
        raise RuntimeError, 'gui_support set_root called illegally.'
    _root = root

class VcsDialog: # should inherit, had some trouble packing though -- pfd
    def __init__ (self, **kw):
        self.am_root = not root_exists()
        kw['parent'] = root()
        if not kw.has_key('command'):
            kw['command'] = self.execute
        self.dialog = Pmw.Dialog(**kw)
        self.title = self.dialog.title
        self.interior = self.dialog.interior
        self.geometry = self.dialog.geometry
        self.component = self.dialog.component
        self.config = self.dialog.config
        self.option_add = self.dialog.option_add
        self.balloon=balloon
        self.transient= self.dialog.transient
        self.withdraw = self.dialog.withdraw
        self.deiconify = self.dialog.deiconify
        _dialogs.append(self)

    def position_popup (self, popup, transient=1):
        "Position a dialog over this one."
        if transient: 
            popup.transient(self.interior())
        g = self.geometry()
        d = g.split('+')[1:]
        popup.geometry("+%s+%s" % (d[0],d[1]))

    def position_over (self, gui_parent):
        "Position this dialog over the gui_parent"
        g = gui_parent.geometry()
        d = g.split('+')[1:]
        self.geometry("+%s+%s" % (d[0],d[1]))

    def execute (self, name):
        if name is None:
            self.destroy()
            return

    def destroy (self):
        global _root
        for i in range(len(_dialogs)):
            if _dialogs[i] is self:
                del _dialogs[i]
                break
        self.dialog.destroy()
        if self.am_root:
            _root.destroy()
            _root = None

def add_balloon_help (menu, menu_title, font = None):
    "Add a balloon help toggle to menu."
    menu.addmenuitem(menu_title, 'checkbutton', 
                        'Toggle balloon help on/off',
                        label = 'Show Balloons',
                        selectcolor=gui_color.one,
                        variable = _balloon_var,
                        font = font,
                        command = toggle_balloon_state
                       )
     
if __name__ == '__main__':
    class T1 (VcsDialog):
        def __init__ (self, **kw):
            kw.update({'command':self.execute})
            VcsDialog.__init__(self, **kw)
            self.main_menu = Pmw.MenuBar(self.interior(),
                hull_relief='raised',
                hull_borderwidth = 2,
                balloon = self.balloon
                )
            self.main_menu.pack(side='top', fill='x')
            self.main_menu.addmenu('Help', 'Help', side='right', tearoff = 1)
            add_balloon_help(self.main_menu, 'Help')

        def start3 (self):
            self.d3 = T1(title='Test 3', buttons=('OK','Cancel'))
            self.position_popup(self.d3)
            balloon.bind(self.d3.interior(), 'This is the T3 test balloon')

        def execute (self, name):
            print 'Executing action for ', name
            if name is None:
                self.destroy()
                return
            if name != 'Apply':
                self.destroy()

    m = T1(title='Test 1', buttons=('OK','Apply', 'Cancel'),buttonboxpos='s')
    w = T1(buttons=('OK', 'Cancel'))
    w.title('Test 2')
    Tkinter.Label(m.interior(), text='T1').pack()
    Tkinter.Label(w.interior(), text='T2').pack()
    Tkinter.Button(w.interior(), text='3', command=w.start3).pack()
    balloon.bind(m.interior(), 'This is the T1 test balloon.')
    balloon.bind(w.interior(), 'This is the T2 test balloon.')
    root().mainloop()
