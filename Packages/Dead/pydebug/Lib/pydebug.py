__doc__ = """
Python - Debugger

The graphical Python - Debugger

Copyright (C) Ulrich Herold 1997/1998

Version 1.5.4

E-Mail:Ulrich.Herold@ProConsult-Online.com

WWW:http://www.t-online.de/home/Ulrich.Herold
"""

#
# PyDebug is based upon tkdb from Daniel Larsson
#
#
# TODOs:
# - fonts, sizes from environment file
# - speeding up
# - post mortem, ...
#
# Bugs:
# - Debugger.run(...) does not work ?
# - ObjectBrowser scans the sourcetext and recognises
#   classes and functions embedded in strings
#
#

__version__ = "PyDebug 1.5.4"

import bdb
import Tkinter
import sys
import os
import string
import re

# check out whether bdb has conditional breakpoints
pyv = sys.version[:3]
if string.atof(pyv) >= 1.5:
    hasCondBreaks = 1
else:
    hasCondBreaks = 0


###############################################
### widget helper classes
### shamelessly stolen from uitools written by Mitch Chapman
### i needed TextWidgets with horiz. and vert. Scollbar
### so i changed some bits
###
### PMW 0.4 is too slow for many machines
###############################################

class ScrollBase:
    ##################################################################
    # Specify the view to be scrolled by self.
    ##################################################################
    def _setView(self, newView):
        self.view = newView
        self.view.pack(in_=self.borderframe, fill='both', expand='yes')
        self.vsPacking['before'] = self.view
        # Attach the scrollbars.  Note that scrollable must support
        # these methods:  xview, yview, xscrollcommand, yscrollcommand.
        self.vscroll['command'] = self.view.yview
        self.hscroll['command'] = self.view.xview
        self.view['xscrollcommand'] = self.hscroll.set
        self.view['yscrollcommand'] = self.vscroll.set
        # Make sure the view is visible to the user
        # Gotta explicitly use Tk.Misc.tkraise, because if the view
        # is a canvas it defines its own tkraise -- whose purpose
        # is to change the stacking order of canvas items.
        Tkinter.Misc.tkraise(self.view, self.borderframe)

    ##################################################################
    # Initialize a new instance.
    ##################################################################
    def __init__(self, master=None, view=None):
        self.master = master
        frame = self.frame = Tkinter.Frame(master)
        
        # The inner frame is provided because the Tk Text widget
        # (<= Tk 4.1) wasn't smart enough to keep embedded windows from
        # drawing over its border.  This inner frame can take the place
        # of the Text border in subclasses.
        self.borderframe = Tkinter.Frame(frame, relief='sunken', bd=3, \
            highlightthickness=2)

        # Create the scrollbars.  Record their packing rules for later
        # use in dynamically mapping/unmapping scrollbars.
        self.vsPacking = {'side':'right', 'fill':'y'}
        vs = self.vscroll = Tkinter.Scrollbar(frame, orient='vertical', width=12)

        # The horizontal scrollbar goes into its own frame at the
        # bottom.  This offers the opportunity to stick a "spacer"
        # frame to the right of the horizontal bar, so both scrollbars
        # appear to stop at the edge of the contained view.
        # This is stolen from "Practical Programming in Tcl and Tk," by
        # Brent B. Welch.
        padsize = self.padsize = (string.atoi(vs['width']) + \
            2 *(string.atoi(vs['bd']) + \
            string.atoi(vs['highlightthickness'])))
        hsFrame = self.hsFrame = Tkinter.Frame(frame)
        # Here's the "spacer" frame.
        hsPad = self.hsPad = Tkinter.Frame(hsFrame, width=padsize, height=padsize)

        hs = self.hscroll = Tkinter.Scrollbar(hsFrame, \
            orient='horizontal',
            width=12)
        hs.pack(side='bottom', fill='x')
        hsPad.pack(side='right', before=self.hscroll)
        # This time, the packing is for self.hsFrame
        self.hsFramePacking = {'before':vs,'side':'bottom', 'fill':'x', 'expand':'no'}

        apply(vs.pack, (), self.vsPacking)
        self.borderframe.pack(fill='both', expand='yes')
        apply(hsFrame.pack, (), self.hsFramePacking)
	
        self.view = None
        self.hsPacked = 1
        self.vsPacked = 1

        # Bad move:  initializer invoking another method on self.
        # At least _setView is named so as to indicate that subclasses
        # should not override it directly...
        if view:
            self._setView(view)

    ##################################################################
    # Install a scrollable thingy as the view for self.
    # Note that you can change views on the fly, though it's up to
    # you to remove any old views before setting the new one.
    # newView should be a widget.  It must provide Tkinter.Text-style
    # methods xview() and yview(), and have configurable
    # xscrollcommand and yscrollcommand attributes.
    ##################################################################
    def setView(self, newView):
        self._setView(newView)


######################################################################
# This is a scrolled text class.
######################################################################

class ScrolledText(ScrollBase):
    ##################################################################
    # Initialize a new instance.
    ##################################################################
    def __init__(self, master=None):
        self.text = Tkinter.Text(master, relief='flat')
        ScrollBase.__init__(self, master, self.text)

######################################################################
# This is a scrolled canvas class.
######################################################################
class ScrolledCanvas(ScrollBase):
    ##################################################################
    # Initialize a new instance.
    ##################################################################
    def __init__(self, master=None):
        self.canvas = Tkinter.Canvas(master)
        ScrollBase__init__(self, master, self.canvas)

class ScrolledList(ScrollBase):
    ##################################################################
    # Initialize a new instance.
    ##################################################################
    def __init__(self, master=None):
        self.listbox = Tkinter.Listbox(master)
        ScrollBase.__init__(self, master, self.listbox)

#################
## Dialogs
#################
class PromptDialog:
    def __init__(self,title="please enter value",text = "label",default = ""):
        self.top = Tkinter.Toplevel()
        self.top.title(title)
        self.frame = Tkinter.Frame(self.top)
        self.label = Tkinter.Label(self.frame,text = text)
	if type(default) == StringType:
	    self.input = Tkinter.StringVar(default)
	else:
	    self.input = Tkinter.StringVar(__builtins__.str(default))
        self.entry = Tkinter.Entry(self.frame,width="40",textvariable = self.input)
        self.label.pack(side="top",anchor = "w",padx = 2, pady = 2,fill = "x")
        self.entry.pack(side = "top",anchor = "w",padx = 2, pady = 2,fill = "x")
        f2 = Tkinter.Frame(self.frame)
        self.ok = Tkinter.Button(f2,text = "OK",command = self._gotok)
        self.cancel = Tkinter.Button(f2,text = "Cancel",command = self._gotcancel)
        self.ok.pack(side = "left",expand = 1,fill = "x")
        self.cancel.pack(side = "left",expand = 1,fill = "x")
        f2.pack(expand = 1,fill="both")
        self.frame.pack(expand = 1,fill = "both")
        self.answer = ""
        self.button = ""

    def _gotok(self):
        self.button = "OK"
        self.answer = self.entry.get()
        self.frame.destroy()

    def _gotcancel(self):
        self.button = "Cancel"
        self.answer = self.entry.get()
        self.frame.destroy()

    def domodal(self):
        self.top.grab_set()
        self.frame.wait_window()
        self.top.grab_release()
        self.top.destroy()


class MessageDialog:
    def __init__(self,title="",text = "message",cancelbutton=1):
        self.top = Tkinter.Toplevel()
        self.top.title(title)
        self.frame = Tkinter.Frame(self.top)
        self.label = Tkinter.Label(self.frame,text = text)
        self.label.pack(side="top",anchor = "w",padx = 2, pady = 2,fill = "x")
        f2 = Tkinter.Frame(self.frame)
        self.ok = Tkinter.Button(f2,text = "OK",command = self._gotok)
        self.ok.pack(side = "left",expand = 1,fill = "x")
        if cancelbutton:
            self.cancel = Tkinter.Button(f2,text = "Cancel",command = self._gotcancel)
            self.cancel.pack(side = "left",expand = 1,fill = "x")
        else:
            self.cancel = None
        #
        f2.pack(expand = 1,fill="both")
        self.frame.pack(expand = 1,fill = "both")
        self.button = ""

    def _gotok(self):
        self.button = "OK"
        self.frame.destroy()

    def _gotcancel(self):
        self.button = "Cancel"
        self.frame.destroy()

    def domodal(self):
        self.top.grab_set()
        self.frame.wait_window()
        self.top.grab_release()
        self.top.destroy()



class ButtonBox:
    def __init__(self,master = None,side = "left"):
        self.frame = Tkinter.Frame(master)
        self.idx = 0
        self.buttons = {}
        self.side = side

    def add(self,name = "",text = "", command = None):
        if name in self.buttons.keys():
            raise KeyError
        b = Tkinter.Button(self.frame,text = text,command = command)
        b.pack(side=self.side,fill = "both",expand = 1,padx = 1, pady = 1)
        self.buttons[name] = b

    def buttonconfigure(self,name,**kw):
        self.buttons[name].configure(kw)
	
 
######################################
### the real work !!!
######################################

from types import *

BasicTypes = [ ListType, TupleType, DictType, IntType ,SliceType ,
	       EllipsisType, LongType, FloatType, StringType, None]
topList = {}

def getInfo(x):
    """
    getInfo
    
    Returns all informations about an object.
    """
    sum = []
    if type(x) in BasicTypes:
        return sum

    dictkeys = []
    if hasattr(x,'__dict__'):
	dictkeys = x.__dict__.keys()

    members = []
    if hasattr(x,'__members__'):
	members = x.__members__

    methods = []
    if hasattr(x,'__methods__'):
	methods = x.__methods__

    sum = dictkeys + methods + members

    if hasattr(x,'__class__'):
	sum = sum + getInfo(x.__class__)

    if hasattr(x,'__bases__'):
	# It's a derived class; add the base class attributes
	for base in x.__bases__:
	    sum = sum + getInfo(base)

    sum.sort()

    # Remove duplicate entries
    i = 0
    while i < len(sum) - 1:
	if sum[i] == sum[i + 1]:
	    del sum[i + 1]
	else:
	    i = i + 1

    return sum

class Show:
    """
    Window to browse an item

    The item is evaluated in the current namespace.
    Its name and value is shown in the listbox.
    If the item has attributes, these attributes and their
    evaluatons are also shown in the listbox.
    You can browse the listbox items with a double click and
    another browser will open up.
    The close button closes one browser. 
    The 'Close All' button closes ALL browsers!
    """
    __version__ = 0.9

    def __init__(self,varname,locals = None,globals = None):
        if type(varname) != type(""):
            print "internal error 4. please mail author"
            return
        self.ntop = None
        self.varname = varname
        if locals == None or globals == None:
            try:
                raise "ohoh"
            except:
                self.locals = sys.exc_traceback.tb_frame.f_back.f_locals
                self.globals = sys.exc_traceback.tb_frame.f_back.f_globals
            #
        #
        else:
            self.locals = locals
            self.globals = globals
        #
        self.newBox(varname)

    def search(self):
        pass

    def close(self,obj = None):
        global topList
        if obj == None:
            obj = self
        tkys = topList.keys()
        topList[obj].remove(obj.ntop)
        if len(topList[obj]) == 0:
            del topList[obj]
	    
        try:
            obj.ntop.destroy()
        except:
            pass
        if len(topList.keys()) == 0:
            Tkinter._default_root.quit()

    def close_all(self):
        global topList
        for x in topList.keys():
            self.close(x)
	
    def _newBox(self,*args):
        self.activeBox = self
        idx = self.nbox.listbox.curselection()[0]
        sel = self.nbox.listbox.get(idx)
        pos = string.find(sel," = ")
        if pos > 0:
            sel = sel[:pos]
        #
        try:
            z = eval(self.varname + "." + sel,self.globals,self.locals)
        except:
            return
        #
        Show(self.varname + "." + sel,self.locals,self.globals)
	    
    def newBox(self,vname):
        global topList

        try:
            object = eval(vname,self.globals,self.locals)
        except:
            return

        ntop = Tkinter.Toplevel()
        ntop.title("Attributes of '%s'" % vname)
        self.ntop = ntop

        bframe = Tkinter.Frame(ntop)
        bframe.pack(side="bottom",expand=0,fill="x")

        but2 = Tkinter.Button(bframe,text = "Close",command=self.close)
        but2.pack(side="left",fill="x",expand = 1)
	
        but3 = Tkinter.Button(bframe,text = "Close All",command=self.close_all)
        but3.pack(side="left",fill="x",expand = 1)


        nbox = ScrolledList(ntop)
        nbox.listbox.configure(background = "white", \
            foreground = "black",
            width = "40",
            height = "10")

        nbox.listbox.bind("<Double-Button-1>",self._newBox)

        self.nbox = nbox
        nbox.frame.pack(side = "top",fill = "both",expand = 1)


        try:
            topList[self].append(ntop)
        except:
            topList[self] = [ntop]
        #
    
	try:
	    atts = getInfo(object)
	    self.fill(vname,nbox,atts)
	except:
	    pass
    #

    def activate(self):
        try:
            self.ntop.deiconify()
        except:
            raise "instance must be reinitialised"
	
    def standalone(self):
        root.withdraw()
        self.activate()
        root.mainloop()
	
	
    def fill(self,vname,box,attlist):
        box.listbox.delete("0","end")
        if len(attlist) == 0:
            try:
                box.listbox.insert("end","%s = %s" %(vname,\
                    eval(vname,
                    self.globals,
                    self.locals)))
            except:
                try:
                    ret = eval(x,self.globals,self.locals)
                    box.listbox.insert("end","%s = %s" % (x,ret))
                except:
                    box.listbox.insert("end",vname)
                #
            #
            root.update()
            return
            
        # attlist with entries
        for x in attlist:
            try:
                z = eval(vname + "." + x,self.globals,self.locals)
                try:
                    xx = eval(z,self.globals,self.locals)
                    box.listbox.insert("end","%s = %s" % (z,xx))
                except:
                    box.listbox.insert("end","%s = %s" % (x,z))
            except:
                try:
                    ret = eval(x,self.globals,self.locals)
                    box.listbox.insert("end","%s = %s" % (x,ret))
                except:
                    box.listbox.insert("end",x)
                #
            #
            root.update()
	#
    #
#

########################################################
	
class SourceText:
    """
    Text widget showing source code
    
    Click with right Mousebutton will set/release a Breakpoint
    on that line. If the click happens on a conditional breakpoint,
    the condition will be showed in a panel and you will be asked
    whether to remove the breakpoint or not.
    Doubleclick with right Mousebutton will set/release
    a conditional Breakpoint on that line.
    Current active Selection (with drag or double click) will
    be used by 'Print' and 'Browse'
    Control-Keys:
      Ctrl-S -> Step - command
      Ctrl-N -> Next - command
      Ctrl-C -> Continue - command
      Ctrl-R -> Return - command
    Cursor-Keys:
      UP -> Back in Command-History
      DOWN -> Forward in Command-History
    Editing:
      Ctrl-K -> Delete to end of line
      Ctrl-A -> Begin of line
      Ctrl-E -> End of line
      ...
    """
    __version__ = 1.1

    def __init__(self, master=None, debugger=None, filename=None):
        self.debugger = debugger

        self.text = ScrolledText(master)
        self.text.text.configure(background = "white",\
            foreground = "black",
            width = "70",
            height = "20",
            wrap = "none")

        self.file = filename
        self.setup()

        self.text.text.bind("<3>",self.set_break_event,add=1)
	if hasCondBreaks:
	    self.text.text.bind("<Double-Button-3>",self.set_cond_break_event,add = 1)

        self.setBindings()
        self.text.text.tag_configure("curpos",background="red")
    #

    def setBindings(self):
        self.text.text.bind_all("<Control-s>", self.debugger.step_command,add=1)
        self.text.text.bind_all("<Control-n>", self.debugger.next_command,add=1)
        self.text.text.bind_all("<Control-c>", self.debugger.continue_command,add = 1)
        self.text.text.bind_all("<Control-r>", self.debugger.return_command,add = 1)
        self.text.text.bind_all("<Alt-q>", self.debugger.quit_command,add = 1)
    #

    def getTextWidget(self):
        return self.text.text

    #
    def reset(self):
        self.text.text.configure(state="normal")
        self.text.text.delete('1.0', Tkinter.END)
        self.file = None

    #
    def setup(self):
        self.text.text.configure(state="normal")
        if self.file in ['<string>', '<stdin>']:
            commandListing = """Following commands are implemented:
- STEP: do one step, following calls to functions
- NEXT: do one step, don't follow calls 
- CONTINUE: continue execution until next breakpoint
- RETURN: continue execution until current stack frame will be left
- PRINT VAR: print the selected variable in I/O window
- BROWSER: browse selected object
- SHOW/HIDE IO: show or hide I/O window
- QUIT: exit PyDebug
"""
            self.text.text.insert(Tkinter.END, commandListing)
            return

        from string import expandtabs
        file = open(self.file)
        lines = file.readlines()
        file.close()

        bmap = self.debugger.get_all_breaks()
        if bmap.has_key(self.file):
            blist = bmap[self.file]
            br = 1
        else:
            br = 0
        #
        # local speed
        l_stti = self.text.text.insert
        l_stta = self.text.text.tag_add
        l_sttc = self.text.text.tag_configure
        for lno in xrange(1, len(lines)+1):
            if br and lno in blist:
                mkBreak = 1
            else:
                mkBreak = 0
            line = "....  " + `lno` + ': ' + expandtabs(lines[lno-1])
            l_stti(Tkinter.END, line)
            if mkBreak:
                tag = 'break_%d' % (lno)
                l_stta(tag, '%d.0' % (lno), '%d.end' % (lno))
                try:
                    cond = self.debugger.cbreaks[self.file,lno]
                    color = "blue"
                except:
                    color = "green"
                #
                l_sttc(tag,background=color)


    def set_lineno(self, lineno,file=None):
        if file:
            self.file = file
        if self.file in ['<string>', '<stdin>']:
            return
        elif lineno > 0:
            self.text.text.configure(state="normal")
            self.text.text.mark_set('insert', '%d.0' % lineno)
            try:
                start, stop = self.text.text.tag_ranges('curpos')
                self.text.text.delete(start, stop)
                self.text.text.insert(start, '..')
                self.text.text.tag_remove('curpos')
            except: pass
            #
            self.text.text.delete('%d.1' % lineno, '%d.3' % lineno)
            self.text.text.insert('%d.1' % lineno, '->')
            self.text.text.tag_add('curpos', '%d.1' % lineno, '%d.3' % lineno)
            self.text.text.see('insert')

    def set_break_event(self, event):
        self.text.text.configure(state="normal")
        idx = str(self.text.text.index("current"))
        lineno = string.atoi(string.splitfields(idx,".")[0])
        tag_name = "break_%d" % lineno
        try:
            cond = self.debugger.cbreaks[self.file,lineno]
            dia = MessageDialog(title = "conditional breakpoint",text = cond + "\nRemove ?")
            dia.domodal()
            if dia.button != "OK" :
                return
        except: pass
        #
        if self.debugger.get_break(self.file,lineno):
            self.debugger.clear_break(self.file, lineno)
            text = '.'
            self.text.text.tag_delete(tag_name)
        else:
            self.debugger.set_break(self.file, lineno)
            text = '.'
            self.text.text.tag_add(tag_name,"%d.0" % lineno,"%d.end" % lineno)
            self.text.text.tag_configure(tag_name,background = "green")
        #
        self.text.frame.update_idletasks()
    #
        
    def set_cond_break_event(self, event):
        self.text.text.configure(state="normal")
        idx = str(self.text.text.index("current"))
        lineno = string.atoi(string.splitfields(idx,".")[0])
        tag_name = "break_%d" % lineno
        if self.debugger.get_break(self.file,lineno):
            self.debugger.clear_break(self.file, lineno)
            text = '.'
            self.text.text.tag_delete(tag_name)
        else:
            # ask for condition
            condText = ""
            while 1:
                dia = PromptDialog(title = "Breakpoint",text = "enter condition for breakpoint",default = condText)
                dia.domodal()
                but = dia.button
                cond = dia.answer
                if but == "OK":
                    condText = cond
                    try:
                        eval(cond, self.debugger.curframe.f_globals, \
                            self.debugger.curframe.f_locals)
                    except NameError:
                        dia2 = MessageDialog(title = "Breakpoint",\
                            text = "NameError in condition. Do you really want to set this condition ?")
                        dia2.domodal()
                        if dia2.button != "OK":
                            continue
                    except SyntaxError:
                        dia2 = MessageDialog(title = "Breakpoint",\
                            text = "SyntaxError in condition. Please change condition",cancelbutton = 0)
                        dia2.domodal()
                        continue
                    #
                    color = "blue"
                else:
                    cond = None
                    color = "green"
                #
                break
            #
            self.debugger.set_break(self.file, lineno,cond)
            text = '.'
            self.text.text.tag_add(tag_name,"%d.0" % lineno,"%d.end" % lineno)
            self.text.text.tag_configure(tag_name,background = color)
        #
        self.text.frame.update_idletasks()
        return
#

################################################
class VarsDialog:
    """
    ListBox showing local variables

    Double-Click with left Button will give you a dialog to
    change the selected variable. 
    Be warned, this dialog is updated after each step !
    """
    __version__ = 1.0
    def __init__(self, debugger):
	self.toplevel = Tkinter.Toplevel()
	self.toplevel.withdraw()
	self.toplevel.update_idletasks()
	self.toplevel.protocol('WM_DELETE_WINDOW', self.going_away)
	self.debugger = debugger
	self.toplevel.title('Local Variables')
	self.create_widgets()
	self.no_value_allowed = 0
	self.toplevel.geometry("-0+0")
	self.toplevel.deiconify()

    # virtual methods
    def showHelp(self):
        raise "pure virtual method"

    def _setvar(self,vname,value):
        raise "pure virtual method"

    def _getvars(self,frame):
        raise "pure virtual method"

    def going_away(self,*args):
        raise "pure virtual method"

    def __del__(self):
	global root
	root.update_idletasks()

    def getpos(self):
	return self.toplevel.geometry()

    def setpos(self,pos):
	self.toplevel.geometry(pos)
	self.toplevel.update()

    def create_widgets(self):
	global root
        bframe = Tkinter.Frame(self.toplevel)
        bframe.pack(side="bottom",expand=0,fill="x")
        but = Tkinter.Button(bframe,text = "Close",command=self.going_away)
        but.pack(side="left",fill="x",expand = 1)
	
	but = Tkinter.Button(bframe,
			     text = "Help",
			     command = self.showHelp)
	but.pack(side = "left",expand = 1,fill = "x")

	self.vars = ScrolledList(self.toplevel)

        self.vars.listbox.configure(background = "white", \
            foreground = "black",
            width = "40",
            height = "10")

        self.vars.listbox.bind("<Double-Button-1>",self.edit_value_event)

        self.vars.frame.pack(side = "top",fill = "both",expand = 1)



    def update(self, frame):
	global root
	
	self.vars.listbox.delete(0, 'end')
	vars = self._getvars(frame)

	kys = vars.keys()
	kys.sort()
	
	# local speed
	l_svti = self.vars.listbox.insert
	l_str = __builtins__.str
	for key in kys:
	    try:
		value = l_str(vars[key])
	    except:
		try:
		    value = vars[key].__str__()
		except:
		    print "VarsDialog:no value for key =",key
		    continue
	    line = key + ' = ' + value
	    l_svti('end', line)

	root.update_idletasks()

    def edit_value_event(self, event):
	self.no_value_allowed = 1
        index = self.vars.listbox.index('@%d,%d' % (event.x, event.y))
	vname = self.vars.listbox.get(index)
	try:
	    vname = string.split(vname," = ")[0]
	except:
	    print "no ' = ' found"
	    return
	#
	dia = PromptDialog(title="Enter new value for:",
			   text=vname)
	dia.domodal()
	if dia.button == "OK":
	    nval = dia.answer
	    self._setvar(vname,nval)

	self.update(self.debugger.curframe)


class LocalVarsDialog(VarsDialog):
    """
    ScrolledList showing local variables

    Double-Click with left Button will give you a dialog to
    change the selected variable.
    Be warned, this dialog is updated after each step !
    """

    __version__ = 1.1

    def __init__(self,debugger):
	VarsDialog.__init__(self,debugger)
	self.toplevel.title('local variables')
	self.toplevel.geometry("-0-0")

    def going_away(self):
	self.debugger.vars_dlg = None
	try:
	    self.toplevel.destroy()
	except:
	    pass

    def _getvars(self,frame):
	return frame.f_locals

    def _setvar(self,vname,nval):
	self.debugger.curframe.f_locals[vname] = nval

    def showHelp(self):
	self.debugger.helpLocalVariableWindow()




################################################

class GlobalVarsDialog(VarsDialog):
    """
    ScrolledList showing global variables
    Double-Click with left Button will give you a dialog to
    change the selected variable.
    Be warned, this dialog is updated after each step !
    """
    __version__ = 1.0

    def __init__(self,debugger):
	VarsDialog.__init__(self,debugger)
	self.toplevel.title('global variables')
	self.toplevel.geometry("-0-0")

    def going_away(self):
	self.debugger.globvars_dlg = None
	try:
	    self.toplevel.destroy()
	except:
	    pass

    def _getvars(self,frame):
	return frame.f_globals

    def _setvar(self,vname,nval):
	self.debugger.curframe.f_globals[vname] = nval

    def showHelp(self):
	self.debugger.helpGlobalVariableWindow()

#

    
################################################

class StackDialog:
    """
    ListBox which shows the active stack

    Double-Click with left button will show the Sourcetext of the
    selected file
    """
    __version__ = 1.0
    def __init__(self, debugger):
	self.toplevel = Tkinter.Toplevel()
	self.toplevel.geometry("-0+300")
	self.toplevel.withdraw()
	self.toplevel.update_idletasks()
	self.toplevel.protocol('WM_DELETE_WINDOW', self.going_away)
	self.toplevel.title('Stack')
	self.debugger = debugger
	self.create_widgets()

	self.toplevel.deiconify()

    def getpos(self):
	return self.toplevel.geometry()

    def setpos(self,pos):
	self.toplevel.geometry(pos)
	self.toplevel.update()

    def going_away(self, *args):
	self.debugger.stack_dlg = None
	try:
	    self.toplevel.destroy()
	except:
	    pass

    def select_command(self, *args):
	from string import atoi
	i = self.stack_box.listbox.index("active")
	self.debugger.show_frame(self.debugger.stack[i][0])

    def create_widgets(self):
        bframe = Tkinter.Frame(self.toplevel)
        bframe.pack(side="bottom",expand=0,fill="x")

	but = Tkinter.Button(bframe,
			     text = "Close",
			     command = self.going_away)
	but.pack(side = "left",expand = 1,fill = "x")

	but = Tkinter.Button(bframe,
			     text = "Help",
			     command = self.showHelp)
	but.pack(side = "left",expand = 1,fill = "x")

	self.stack_box = ScrolledList(self.toplevel)
	self.stack_box.listbox.configure(background="pink",
					 foreground = "black",
					 width = "40",
					 height = "6",
					 selectmode = "single")

	self.stack_box.listbox.bind("<Double-Button-1>",self.select_command)
	self.update()

	self.stack_box.frame.pack(side = "top",expand=1,fill=Tkinter.BOTH)


    def showHelp(self):
	self.debugger.helpStackWindow()

    def update(self):
        import linecache, string, os
        self.stack_box.listbox.delete(0, 'end')
        
        try:
            for frame, lineno in self.debugger.stack:
                filename = os.path.basename(frame.f_code.co_filename)
                s = filename + '(' + `lineno` + ')'
                if frame.f_code.co_name:
                    s = s + frame.f_code.co_name
                else:
                    s = s + "<lambda>"
                #
                if frame.f_locals.has_key('__args__'):
                    args = frame.f_locals['__args__']
                else:
                    args = None
                #
                if args:
                    s = s + __builtins__.str(args)
                else:
                    s = s + '()'
                #
                if frame.f_locals.has_key('__return__'):
                    rv = frame.f_locals['__return__']
                    s = s + '->'
                    s = s + __builtins__.str(rv)
                #
                line = linecache.getline(filename, lineno)
                if line:
                    s = s + ': ' + string.strip(line)
                #
                self.stack_box.listbox.insert('end', s)
            #
        except:
            pass

################################################
class FunctionDialog:
    """
    ListBox which shows all classes, methods and functions in Sourcetext

    class names	 begin with "Class :"
    names of methods begin with "Meth  :"
    all function names begin with "func	 :".
    The list is sorted.
    Double-Click with left button will position Sourcetext to
    the selected class,method or function
    """
    __version__ = 1.0

    def __init__(self, debugger):
	self.toplevel = Tkinter.Toplevel()
	self.toplevel.geometry("+550+0")
	self.toplevel.withdraw()
	self.toplevel.update_idletasks()
	self.toplevel.protocol('WM_DELETE_WINDOW', self.going_away)
	self.toplevel.title('Functions')
	self.debugger = debugger
	self.create_widgets()
	self.toplevel.deiconify()

    def getpos(self):
	return self.toplevel.geometry()

    def setpos(self,pos):
	self.toplevel.geometry(pos)
	self.toplevel.update()

    def going_away(self, *args):
	self.debugger.function_dlg = None
	try:
	    self.toplevel.destroy()
	except: 
	    pass

    def select_command(self, *args):
	from string import atol,split
	idx = self.function_box.listbox.curselection()[0]
	sel = self.function_box.listbox.get(idx)
	slist = split(sel," : ")
	try:
	    fname = self.debugger.filename
	    lno = atol(slist[2])
	    self.debugger.cursource.getTextWidget().see("%d.0" % lno)
	    root.update()
	except:
	    pass

    def create_widgets(self):
        bframe = Tkinter.Frame(self.toplevel)
        bframe.pack(side="bottom",expand=0,fill="x")

	but = Tkinter.Button(bframe,
			     text = "Close",
			     command = self.going_away)
	but.pack(side = "left",expand = 1,fill = "x")

	but = Tkinter.Button(bframe,
			     text = "Help",
			     command = self.showHelp)
	but.pack(side = "left",expand = 1,fill = "x")

	self.function_box = ScrolledList(self.toplevel)
	self.function_box.listbox.configure(background = "white",
					    foreground = "black",
					    width = "40",
					    height = "20",
					    selectmode = "single")
	
	self.function_box.listbox.bind("<Double-Button-1>",self.select_command)

	self.update()
	self.function_box.frame.pack(side = "top",expand=1,fill=Tkinter.BOTH)

    def showHelp(self):
	self.debugger.helpFuncWindow()

    def update(self,filename = None):
	defline = re.compile("""^def \([^(]+\)(""")
	methodline = re.compile("""^[ \t]+def \([^(]+\)(""")
	classline = re.compile("""^[ \t]*class \([^:(]+\)""")
	try:
	    if filename:
		tx = filename
	    else:
		tx = self.debugger.filename
	    #
	    fd = open(tx)
	except:
	    return

	fmap = {}
	line = fd.readline()
	lno = 1
	classname = ""
	l_cls = classline.search
	l_mls = methodline.search
	l_dls = defline.search
	l_fdread = fd.readline
	l_fmapHasKey = fmap.has_key
	while line:
	    if classline.search(line) >= 0:
		classname = classline.group(1)
		mapname = "Class : " + classname
	    #
	    elif methodline.search(line) >= 0:
		methodname = methodline.group(1)
		if classname:
		    methodname = classname + "." + methodname
		    mapname = "Meth  : " + methodname
		#
	    #
	    elif defline.search(line) >= 0:
		defname = defline.group(1)
		if classname:
		    classname = None
		mapname = "func	 : " + defname
	    #
	    else:
		line = fd.readline()
		lno = lno + 1
		continue
	    #
	    idx = 0
	    orgname = mapname
	    while fmap.has_key(mapname):
		mapname = orgname + "_" + `idx`
		idx = idx + 1
	    #
	    fmap[mapname] = lno
	    line = fd.readline()
	    lno = lno + 1
	#
	
	fd.close()
	kys = fmap.keys()
	kys.sort()

	# local speed
	l_sfli = self.function_box.listbox.insert
	for x in kys:
	    # self.function_box.listbox.insert('end', "%s : %s" % (x,fmap[x]))
	    l_sfli('end', "%s : %s" % (x,fmap[x]))
	#
	tx = os.path.basename(tx)
	self.toplevel.title("%d Classes/Functions in %s" % (len(kys),tx))


################################################


class ExceptionDialog:
    """
    Listbox which shows the exceptions

    All exceptions which occur will be inserted in this list.
    No more action is defined with this window
    """
    __version__ = 1.0
    def __init__(self, debugger):
	self.toplevel = Tkinter.Toplevel()
	self.toplevel.geometry("-0+550")
	self.toplevel.withdraw()
	self.toplevel.update_idletasks()
	self.toplevel.protocol('WM_DELETE_WINDOW', self.going_away)
	self.toplevel.title('Exceptions')
	self.debugger = debugger
	self.create_widgets()
	self.toplevel.deiconify()

    def getpos(self):
	return self.toplevel.geometry()

    def setpos(self,pos):
	self.toplevel.geometry(pos)
	self.toplevel.update()

    def going_away(self, *args):
	self.debugger.exception_dlg = None
	try:
	    self.toplevel.destroy()
	except:
	    pass

    def create_widgets(self):
        bframe = Tkinter.Frame(self.toplevel)
        bframe.pack(side="bottom",expand=0,fill="x")

 	but = Tkinter.Button(bframe,
			     text = "Close",
			     command = self.going_away)

	but.pack(side = "left",expand = 1,fill = "x")

	but = Tkinter.Button(bframe,
			     text = "Help",
			     command = self.showHelp)
	but.pack(side = "left",expand = 1,fill = "x")

	self.exception_box = ScrolledList(self.toplevel)
	EB = self.exception_box
	EB.listbox.configure(background = "white",
			     width = "40",
			     height = "4",
			     foreground = "red",
			     selectmode = "single")

	self.exception_box.frame.pack(side = "top",expand=1,fill=Tkinter.BOTH)
	
	
    def showHelp(self):
	self.debugger.helpExceptionWindow()

    def update(self,str):
	self.exception_box.listbox.insert("end",str)

################################################


class HelpDialog:
    def __init__(self,text,title,version):
	self.toplevel = Tkinter.Toplevel()
	self.toplevel.withdraw()
	but = Tkinter.Button(self.toplevel,
			     text = "Close",
			     command = self.close)
	
	but.pack(side = "bottom",fill = "x")

	lab = Tkinter.Label(self.toplevel,
			    text = "Help for '%s' (%s)" % (title,version))

	lab.pack(side = "top",fill="both")
	self.text = ScrolledText(self.toplevel)
	# text analyse
	xl = string.split(text,"\012")
	maxx = 0
	for x in xl:
	    lll = len(x)
	    if lll > maxx:
		maxx = lll
	#
	if len(xl) > 14:
	    maxy = 14
	else:
	    maxy = len(xl)
	maxx = maxx + 4 # one TAB
	self.text.text.configure(width = maxx,
				 height = maxy,
				 wrap = "none",
				 foreground = "black",
				 background = "white")

	self.text.frame.pack(side = "top",expand = 1, fill = "both")

	self.text.text.insert("end",text)
	tag = 'title'
	self.text.text.tag_add(tag, '1.0','3.end')
	self.text.text.tag_configure(tag, justify = "center")

	self.text.text.configure(state="disabled")
	self.toplevel.title("Help")
	self.toplevel.deiconify()
	root.update()

    def close(self):
	try:
	    self.toplevel.destroy()
	except:
	    pass

################################################

class Debugger(bdb.Bdb):
    """Main debugger window"""

    def __init__(self,bp_file = None):
	import sys
	global root
	import os
	self.osname = os.name

	bdb.Bdb.__init__(self)

	root = Tkinter.Tk(className = "PyDebug")

	root.withdraw()
	root.update_idletasks()
	
	globalMenuFont = "-adobe-helvetica-bold-o-normal-*-*-120-*-*-p-*-iso8859-1"
	globalTextFont = "-misc-fixed-bold-r-normal--*-120-*-*-c-*-iso8859-1"

	root.option_add("*Font",globalTextFont,"widgetDefault")
	root.option_add("*Menu*Font",globalMenuFont,"widgetDefault")
	root.option_add("*Menubutton*Font",globalMenuFont,"widgetDefault")

	# early Breakpoints
	if bp_file:
	    self.readBreakpoints(bp_file)
	#
	self.bp_file = bp_file

	self.master = Tkinter.Toplevel()
	self.master.withdraw()
	self.master.update_idletasks()
	self.create_widgets()

	self.master.protocol('WM_DELETE_WINDOW', self._on_delete_window)
        
	self.master.title("PyDebug")
	
	self.history = []
	self.histact = 0
	self.closed = 0
	self.cont = 0
	self.stack_dlg = None
	self.function_dlg = None
	self.vars_dlg = None
	self.globvars_dlg = None
	self.exception_dlg = None
	self.source_wins = {}
	self.cursource = None
	self.quit = 0
	self.save_stdout = sys.stdout
	self.save_stderr = sys.stderr
	self.lastcommand = ""
	self.cleanedup = 0
	self.show_lower = 1
	self.ov = None
	self.old_tb = None
	
	self.master.bind_all("<Up>",self.prevhist_command)
	self.master.bind_all("<Down>",self.nexthist_command)

	self.debugger_pos = ""
	self._initEnv()

    def _on_delete_window (self):
        self.quit = 1

    def _initEnv(self):
	self.envConfig = self.readConfig()

	for k in self.envConfig.keys():
	    if k == "pos_debugger":
		    self.debugger_pos = self.envConfig["pos_debugger"]
	    elif k[:4] == "dlg_" and self.envConfig[k] == "1":
		if k == "dlg_stack":
		    #pass
		    self.stack_dlg = StackDialog(self)
		    self.stack_dlg.setpos(self.envConfig["pos_stack"])
		elif k == "dlg_exceptions":
		    self.exception_dlg = ExceptionDialog(self)
		    self.exception_dlg.setpos(self.envConfig["pos_exceptions"])
		elif k == "dlg_locals":
		    self.vars_dlg = LocalVarsDialog(self)
		    self.vars_dlg.setpos(self.envConfig["pos_locals"])
		elif k == "dlg_globals":
		    self.globvars_dlg = GlobalVarsDialog(self)
		    self.globvars_dlg.setpos(self.envConfig["pos_globals"])
		elif k == "dlg_functions":
		    self.function_dlg = FunctionDialog(self)
		    self.function_dlg.setpos(self.envConfig["pos_functions"])
		#
	    #
	#

    def getpos(self):
	return self.master.geometry()

    def setpos(self,pos):
	self.master.geometry(pos)
	self.master.update()

    def re_init(self):
	global root

	try:
	    root = Tkinter.Tk(className = "PyDebug")

	    root.withdraw()
	    root.update_idletasks()
	except:
	    pass
	#
	bdb.Bdb.__init__(self)

	self.master = Tkinter.Toplevel()
	self.master.withdraw()
	self.master.update_idletasks()
	self.create_widgets()

	self.master.title("PyDebug")
	
	self.history = []
	self.histact = 0
	self.closed = 0
	self.cont = 0
	self.stack_dlg = None
	self.function_dlg = None
	self.vars_dlg = None
	self.globvars_dlg = None
	self.exception_dlg = None
	self.source_wins = {}
	self.cursource = None
	self.quit = 0
	self.lastcommand = ""
	self.cleanedup = 0
	self.show_lower = 1
	self.ov = None
	self.old_tb = None
	
	self.master.bind_all("<Up>",self.prevhist_command)
	self.master.bind_all("<Down>",self.nexthist_command)
	self._initEnv()
  
    def write(self,line):
	self.output.text.configure(state = "normal")
	self.output.text.insert("end",line)
	self.output.text.see('end')
	self.output.text.configure(state = "disabled")
	self.save_stdout.write(line)

    def flush(self):
	self.output.text.configure(state = "normal")
	self.output.text.see('end')
	self.output.text.configure(state = "disabled")
	self.save_stdout.flush()

    def writelines(self,lines):
	self.output.text.configure(state = "normal")
	for l in lines:
	    self.output.text.insert("end",line)
	    self.save_stdout.write(line)
	#
	self.output.text.see('end')
	self.output.text.configure(state = "disabled")

    def close(self):
        pass

    def prevhist_command(self,*args):
	self.input.set(self.prev_hist())

    def nexthist_command(self,*args):
	self.input.set(self.next_hist())


    def prev_hist(self):
	if len(self.history) == 0:
	    return "-- empty --"
	self.histact = self.histact - 1
	if self.histact < 0:
	    self.histact = 0
	    print "-- begin of history -- "
	ret = self.history[self.histact]
	return ret
	    

    def next_hist(self):
	if len(self.history) == 0:
	    return "-- empty --"
	self.histact = self.histact + 1
	if self.histact > (len(self.history) - 1):
	    self.histact = len(self.history) - 1
	    print "-- end of history -- "
	ret = self.history[self.histact]
	return ret

    def reset(self):
	if self.closed: raise RuntimeError, 'already closed'
	bdb.Bdb.reset(self)
	self.forget()
	self.set_step()
	
    def forget(self):
	self.lineno = None
	self.stack = []
	self.curindex = 0
	self.curframe = None
	
    def setup(self, frame, traceback):
	self.forget()
	self.stack, self.curindex = self.get_stack(frame, traceback)
	frame = self.stack[self.curindex][0]
	self.show_frame(frame)
	if self.stack_dlg:
	    self.stack_dlg.update()

    def _updateVars(self,frame = None):
	if not frame:
	    frame = self.curframe
	if self.vars_dlg:
	    self.vars_dlg.update(frame)
	if self.globvars_dlg:
	    self.globvars_dlg.update(frame)
	#
	
    def show_frame(self, frame):
	self.curframe = frame

	self._updateVars(frame)

	filename = frame.f_code.co_filename
	lineno = frame.f_lineno
	if self.setup_source(filename, lineno):
	    self.sourcemenu.add('command',
				label=filename,
				command=lambda s=self, f=filename:s.view_file(f))
	    if self.function_dlg:
		self.function_dlg.update(filename)

	self.master.title("PyDebug (%s)" % filename)
	root.update_idletasks()

    def create_widgets(self):
	from Tkinter import Frame, BOTH
	self.frame = Frame(self.master)
	self.create_menubar()
	self.create_srcwin()
	self.create_buttons()
	self.create_lower()
	self.create_input()
	self.frame.pack(expand=1,fill=BOTH)

    def __del__(self):
	sys.stdout = self.save_stdout
	self.cleanup()

    def _genPathname4Breakpoints(self,filename):
	import os
	
	tmptempl = "BP_PYDEBUG"
	if self.osname == "posix":
	    under = ".BP_"
	else:
	    under = "_BP_"
	#
	filename = under + os.path.basename(filename)
	try:
	    tmpdir = os.environ[tmptempl]
	except:
	    tmpdir = ""
	#
	filename = os.path.join(tmpdir,filename)
	return filename

    def _genPathname4Environment(self):
	import os
	
	try:
	    envfile = os.environ["ENV_PYDEBUG"]
	except:
	    try:
		if self.osname == "posix":
		    envfile = os.environ["HOME"]
		else:
		    envfile = os.environ["TEMP"]
		#
	    except:
		envfile = ""
	    #
	#
	if self.osname == "posix":
	    under = ".ENV_PYDEBUG"
	else:
	    under = "_ENV_PYDEBUG"
	#
	if os.path.isdir(envfile):
	    filename = os.path.join(envfile,under)
	elif os.path.isfile(envfile):
	    filename = envfile
	else:
	    filename = under
	#
	return filename

    def writeBreakpoints(self,filename):
        import shelve
        filename = self._genPathname4Breakpoints(filename)
        try:
            os.unlink(filename + ".dir")
            os.unlink(filename + ".dat")
            os.unlink(filename)
        except:
            pass
        brks = self.get_all_breaks()
        kys = brks.keys()
        if len(kys) == 0:
            return
        try:
            sh = shelve.open(filename)
        except:
            self.bp_file = None
            return
        print "writing breakpoints to file",filename
        for f in kys:
            llist = brks[f]
            lnos = []
            for lno in llist:
                try:
                    add = "#" + self.cbreaks[(f,lno)]
                except:
                    add = "#"
                #
                lnos.append(__builtins__.str(lno) + add)
            #
            sh[f] = lnos
        #
        try:
            sh.close()
            del sh
        except: pass
        self.bp_file = None
	
    def readBreakpoints(self,filename):
        import shelve,os
        filename = self._genPathname4Breakpoints(filename)
        try:
            sh = shelve.open(filename)
        except:
            return
        #
        # OK shelve found
        print "reading breakpoints from file",filename
        for k in sh.keys():
            bplist = sh[k]
            for lno in bplist:
                ln,cond = string.split(lno,"#")
                if len(cond) == 0:
                    cond = None
                self.set_break(k,string.atoi(ln),cond)
        #
        sh.close()
    #
    
    def cleanup(self):
	global topList
	import sys
	self.writeConfig()
	if self.vars_dlg:
	    self.vars_dlg.going_away()
	if self.globvars_dlg:
	    self.globvars_dlg.going_away()
	if self.stack_dlg:
	    self.stack_dlg.going_away()
	if self.exception_dlg:
	    self.exception_dlg.going_away()
	if self.function_dlg:
	    self.function_dlg.going_away()
	if len(topList.keys()) > 0:
	    Show.close_all(topList.keys()[0])
	self.vars_dlg = self.globvars_dlg = 0
	self.exception_dlg = self.stack_dlg = self.function_dlg = 0
	if self.bp_file:
	    self.writeBreakpoints(self.bp_file)
	self.set_quit()
	try:
	    self.master.withdraw()
	except: pass
	try:
	    self.master.destroy()
	except: pass
	#
	try:
	    root.withdraw()
	except: pass
	    
	self.master = None
	sys.stdout = self.save_stdout

    def create_lower(self):
	self.lower_frame = Tkinter.Frame(self.frame)
	self.lower_frame.pack(side=Tkinter.BOTTOM, fill=Tkinter.BOTH,expand=0)
	return

    def create_menubar(self):
	# Create menu bar, menus, and menu entries
	# Also create the Stop button (which lives in the menu)
	from Tkinter import Frame,RAISED,X,LEFT,RIGHT
	from Tkinter import Menu,Menubutton

	# Create menu bar
	self.mbar = Frame(self.frame,
			  relief=RAISED,
			  borderwidth=2)
	self.mbar.pack(fill=X)

	# Create File menu
	self.filebutton = Menubutton(self.mbar, text="File")
	self.filebutton.pack(side=LEFT)

	self.filemenu = Menu(self.filebutton)
	self.filebutton['menu'] = self.filemenu

	self.filemenu.add('command',
			  label="View source...",
			  command=self.view_source_command,
			  underline=0)
	self.filemenu.add_separator()
	self.filemenu.add('command',
			  label="Quit",
			  command=self.quit_command,
			  underline=0, accelerator="Alt-Q")
	self.frame.bind("<Alt-q>", self.quit_command)
	self.frame.bind("<Alt-Q>", self.quit_command)

	# Create Source menu
	self.sourcebutton = Menubutton(self.mbar, text="Source")
	self.sourcebutton.pack(side=LEFT)

	self.sourcemenu = Menu(self.sourcebutton)
	self.sourcebutton['menu'] = self.sourcemenu


	# Create View menu
	self.viewbutton = Menubutton(self.mbar, text="View")
	self.viewbutton.pack(side=LEFT)

	self.viewmenu = Menu(self.viewbutton)
	self.viewbutton['menu'] = self.viewmenu

	self.viewmenu.add('command',
			  label="View local variables",
			  command=self.view_vars_command,
			  underline=5)

	self.viewmenu.add('command',
			  label="View global variables",
			  command=self.view_globvars_command,
			  underline=5)

	self.viewmenu.add('command',
			  label="View Classes/Functions",
			  command=self.function_command,
			  underline=5)

	self.viewmenu.add('command',
			  label="View stack",
			  command=self.stack_command,
			  underline=5)

	self.viewmenu.add('command',
			  label="View exceptions",
			  command=self.exception_command,
			  underline=5)


	# Create Breakpoints menu
	self.breakbutton = Menubutton(self.mbar, text="Breakpoints")
	self.breakbutton.pack(side=LEFT)

	self.breakmenu = Menu(self.breakbutton)
	self.breakbutton['menu'] = self.breakmenu


	self.breakmenu.add('command',
			  label="Show/Delete Breakpoints...",
			  command=self.showAllBreaks,
			  underline=0)

	self.breakmenu.add('command',
			  label="Delete All Breakpoints...",
			  command=self.delAllBreaks,
			  underline=0)


	# Create Help menu (on far right)
	self.helpbutton = Menubutton(self.mbar, text="Help")
	self.helpbutton.pack(side=RIGHT)

	self.helpmenu = Menu(self.helpbutton)
	self.helpbutton['menu'] = self.helpmenu

	self.helpmenu.add('command',
			  label="Help with Sourcetext window",
			  underline=10,
			  command=self.helpSourceWindow)

	self.helpmenu.add('command',
			  label="Help with Browser window",
			  underline=10,
			  command=self.helpBrowserWindow)

	self.helpmenu.add('command',
			  label="Help with local Variables window",
			  underline=10,
			  command=self.helpLocalVariableWindow)

	self.helpmenu.add('command',
			  label="Help with global Variables window",
			  underline=10,
			  command=self.helpGlobalVariableWindow)

	self.helpmenu.add('command',
			  label="Help with classes/functions window",
			  underline=10,
			  command=self.helpFuncWindow)

	self.helpmenu.add('command',
			  label="Help with Stack window",
			  underline=10,
			  command=self.helpStackWindow)

	self.helpmenu.add('command',
			  label="Help with Exception Window",
			  underline=10,
			  command=self.helpExceptionWindow)

	self.helpmenu.add_separator()
	self.helpmenu.add('command',
			  label="Readme",
			  underline=0,
			  command=self.helpReadme)

	self.helpmenu.add_separator()
	self.helpmenu.add('command',
			  label="About PyDebug",
			  underline=0,
			  command=self.about_command)
                          
	self.helpmenu.add_separator()
	self.helpmenu.add('command',
			  label="Disclaimer",
			  underline=0,
			  command=self.disclaimer_command)


    def helpReadme(self):
	doc = """
        README for PyDebug

Introduction
PyDebug is a debugger for python scripts with a graphical
user interface. It's inherited from "bdb", a debugger class
which comes with python. 
The idea is based upon "tkdb" (Daniel Larsson), an early 
try of a tk based debugger, but much too slow to be useful.
Scrolling widgets are taken from "uitools" written by 
Mitch Chapman.

PyDebug's home location is found at:

        http://www.t-online.de/home/Ulrich.Herold
        (follow the link to PyDebug)

PyDebug is delivered as source code.

Prerequisites
You need python 1.4 (or better) and _tkinter for Tk 4.x 
(or better) installed.

Installation of PyDebug
PyDebug.py should be installed in your python path.
If you don't know your python path, call your python program
and enter following commands:
>>> import sys
>>> print sys.path
This will print out your python path.
If you don't have permission to move "PyDebug.py" into 
the python path, you can setup/change your PYTHONPATH 
environment variable. For info call "python -h".

PyDebug-Environment
PyDebug uses the environment variables "BP_PYDEBUG" and
"ENV_PYDEBUG". It's been tested on various UNIX platforms
and with NT 4.0.

BP_PYDEBUG:
It contains the name of a directory, where all breakpoint-
files should be saved. If "BP_PYDEBUG" is empty or absent
then the breakpoint files will be stored in the directory,
where PyDebug has been called.
On NT my BP_PYDEBUG is "c:\\temp"
On UNIX my BP_PYDEBUG is set in ".profile" to "$HOME/.BP_dir".
(.BP_dir must already exist!)

ENV_PYDEBUG:
It contains the name of a file, where all window positions
(in case they are open) are stored. If this variable does
not exist, PyDebug tries to generate the filename from
the name of the "HOME" directory (UNIX) or for NT from the
environment variable "TEMP" or "C:\\TEMP".
On NT my ENV_PYDEBUG is "c:\\temp\\_env_pydebug"
On Linux my ENV_PYDEBUG is set in ".profile" to "$HOME/.env_pydebug".

Commands
Following commands are implemented:
- STEP: do one step, following calls to functions
- NEXT: do one step, don't follow calls 
- CONTINUE: continue execution until next breakpoint
- RETURN: continue execution until current stack frame will be left
- PRINT VAR: print the selected variable in I/O window
- BROWSER: browse selected object
- SHOW/HIDE IO: show or hide I/O window
- QUIT: exit PyDebug
If a command requests a selection and there is none, a panel will
ask you to enter a valid name.
The HIDE I/O command will delete the text in the I/O window.

Bindings
Following key bindings exist:
- CTRL-S: STEP command
- CTRL-N: NEXT command
- CTRL-C: CONTINUE command
- CTRL-R: RETURN command
- ALT-Q: QUIT command
- RETURN: evaluate expression in command entry
- CURSOR-UP: show previous command in history
- CURSOR-DOWN: show next command in history
- CTRL-E: cursor to end of line in command entry
- CTRL-A: cursor to begin of line in command entry
- CTRL-K: delete to end of line in command entry

Breakpoints file
PyDebug will save all breakpoints to a file. The name of
the breakpoint file is a construction of a prependix
("_BP_" on non-posix machines and ".BP_" on posix machines)
and a parameter which can be used by instantiation of a
"Debugger" object (see below).

Running PyDebug
You can run PyDebug from commandline and from inside a script.
Running from commandline will name the breakpoint file like the
scriptfile to run (with the prependix mentioned above). 
Example of debugging script "mytest.py" from commandline:

        python PyDebug.pyc mytest.py

If you want to call PyDebug from the interactive loop (or from
script), enter following commands (incl. starting python):

python
>>> import PyDebug
>>> import mytest
>>> dbg = PyDebug.Debugger("name_of_breakpoint_file")
>>> dbg.run("mytest.main()")

If you enter
>>> dbg = PyDebug.Debugger()
then no breakpoint file will be used!

"run" is the only method to be used from a "Debugger" object!

class Debugger
Following lines describes the interface to class "Debugger" 
in Module PyDebug.

method "__init__(bp_file = None)"
        bpfile is "basename" of breakpoint file

method "run(cmd,globals=None,locals=None)"
        cmd = the command to be executed 
        globals = global namespace 
        locals = local namespace

Module PyDebug
Following lines describes the interface of module PyDebug:

Function "run(cmd,globals=None,locals=None,bp_name=None)"
calls         "PyDebug.Debugger(bp_name).run(cmd, globals, locals)"

If you want to execute the command in a special namespace, you
can hand "globals" and "locals" to PyDebug.
"""
	HelpDialog(doc,
		   "Readme",
		   __version__)


    def create_srcwin(self):
	from Tkinter import Frame, BOTH
	self.source_frame = Frame(self.frame)
	self.source_frame.pack(fill=BOTH,expand = 1)


    def _getsel(self,prompt = ""):
	text = self.cursource.text.text
	try:
	    idx1,idx2 = text.tag_ranges("sel")
	    svar = None
	    if idx1 and idx2:
		svar = string.strip(text.get(idx1,idx2))
	except:
	    svar = None
	#
	if svar == None:
	    # Prompt Dialog
	    self.answertext = ""
	    dlg = PromptDialog(title = "Enter name to %s" % prompt,text = "")

	    dlg.domodal()
	    if dlg.button == "OK" and dlg.answer:
		svar = dlg.answer[:]
	    #
	#
	return svar

    def print_command(self):
	svar = self._getsel("print")
	if svar:
	    try:
		self.re_eval("print %s" % svar)
	    except:
		pass

    def create_buttons(self):
	xf = Tkinter.Frame(self.frame)
	xf.pack(expand=0,fill = "both")
	bb1 = ButtonBox(xf,"top")
	bb2 = ButtonBox(xf,"top")
	bb3 = ButtonBox(xf,"top")
	bb4 = ButtonBox(xf,"top")

	bb1.add("Step",text="Step",command=self.step_command)
	bb1.add("Print",text="Print Var",command=self.print_command)

	bb2.add("Next",text="Next",command=self.next_command)
	bb2.add("Browser",text="Browser",command=self.showBrowser)

	bb3.add("Continue",text="Continue",command=self.continue_command)
	bb3.add("ShowLower",text="Hide I/O",command=self.show_io_command)

	bb4.add("Return",text="Return",command=self.return_command)
	bb4.add("Quit",text="Quit",command=self.quit_command)

	bb1.frame.pack(side = "left",fill = 'both', expand = 1)
	bb2.frame.pack(side = "left",fill = 'both', expand = 1)
	bb3.frame.pack(side = "left",fill = 'both', expand = 1)
	bb4.frame.pack(side = "left",fill = 'both', expand = 1)

	self.bb1 = bb1
	self.bb2 = bb2
	self.bb3 = bb3
	self.bb4 = bb4

    def create_input(self):
	frame = Tkinter.Frame(self.lower_frame)
	label = Tkinter.Label(frame, text='>>>')
	label.pack(side=Tkinter.LEFT)
	self.input = Tkinter.StringVar(frame)
	entry = Tkinter.Entry(frame, textvariable=self.input, width=40)
	entry.bind('<Return>', self.input_event)
	entry.pack(side=Tkinter.LEFT,fill = Tkinter.X,expand = 1)
	frame.pack(side=Tkinter.TOP,fill=Tkinter.X,expand=0)

	frame2 = Tkinter.Frame(self.lower_frame)

	self.output = ScrolledText(frame2)
	self.output.text.configure(state = "disabled",
				   width = "50",
				   height = "10",
				   wrap = "none",
				   foreground = "black",
				   background = "white")

	self.output.frame.pack(side=Tkinter.TOP,fill = Tkinter.X,expand=1)
	frame2.pack(side=Tkinter.TOP,fill=Tkinter.X,expand=1)

    def user_line(self, frame):
	# This method is called when we stop or break at this line
	self.interaction(frame, None)
	
    def user_return(self, frame, return_value):
	# This method is called when a return trap is set here
	frame.f_locals['__return__'] = return_value
	self.interaction(frame, None)
	
    def user_exception(self, frame, (exc_type, exc_value, exc_traceback)):
        # This method is called if an exception occurs,
        # but only if we are to stop at or just below this level
        frame.f_locals['__exception__'] = exc_type, exc_value
        if type(exc_type) == type(''):
            exc_type_name = exc_type
        else:
            exc_type_name = exc_type.__name__
        #
        if self.exception_dlg:
            self.exception_dlg.update(exc_type_name + ":" + __builtins__.str(exc_value))
        #
        self.interaction(frame, exc_traceback)
	
    def interaction(self, frame, traceback):
	global root
	import time
	
	self.cont = 0
	self.setup(frame, traceback)

	root.update()
	    
	while not self.cont and not self.quit:
	    Tkinter._default_root.tk.dooneevent(0)
	
	if self.quit:
	    self.cleanup()
	#


    def about_command(self):
	d = MessageDialog(title = __version__,
			  text = __doc__,
			  cancelbutton = 0)

	d.domodal()

    def disclaimer_command(self):
	d = MessageDialog(title = __version__,
			  text = __disclaimer__,
			  cancelbutton = 0)

	d.domodal()



    def helpSourceWindow(self):
	doc = SourceText.__doc__
	HelpDialog(doc,
		   "source window",
		   SourceText.__version__)

    def helpStackWindow(self):
	doc = StackDialog.__doc__
	HelpDialog(doc,
		   "stack window",
		   StackDialog.__version__)
	
    def helpFuncWindow(self):
	doc = FunctionDialog.__doc__
	HelpDialog(doc,
		   "classes/functions window",
		   FunctionDialog.__version__)
	
    def helpLocalVariableWindow(self):
	HelpDialog(LocalVarsDialog.__doc__,
		   "local variables window",
		   LocalVarsDialog.__version__)

    def helpGlobalVariableWindow(self):
	doc = GlobalVarsDialog.__doc__
	HelpDialog(doc,
		   "global variables window",
		   GlobalVarsDialog.__version__)
	
    def helpExceptionWindow(self):
	doc = ExceptionDialog.__doc__
	HelpDialog(doc,
		   "exception window",
		   ExceptionDialog.__version__)
	
    def helpBrowserWindow(self):
	doc = Show.__doc__
	HelpDialog(doc,
		   "browser window",
		   Show.__version__)
	
    def quit_command(self,*args):
	#self.frame.quit()
	self.done = self.quit = 1

    def view_vars_command(self):
	if not self.vars_dlg:
	    self.vars_dlg = LocalVarsDialog(self)
	    self.vars_dlg.update(self.curframe)

    def view_globvars_command(self):
	if not self.globvars_dlg:
	    self.globvars_dlg = GlobalVarsDialog(self)
	    self.globvars_dlg.update(self.curframe)

    def view_source_command(self):
	import FileDialog
	fd = FileDialog.LoadFileDialog(self.frame)
	filename = fd.go(pattern='*.py')
	if filename:
	    if self.setup_source(filename, 1):
		self.sourcemenu.add('command',
				    label=filename,
				    command=lambda s=self, f=filename:s.view_file(f))
		root.update_idletasks()


    ekeys = ["dlg_locals","dlg_exceptions","dlg_globals","dlg_functions","dlg_stack",\
	     "pos_debugger","pos_locals","pos_exceptions","pos_globals","pos_functions","pos_stack",\
	     ]

    def readConfig(self):
	expr = re.compile("\(.*\)=\(.*\)")

	env = {}

	for k in Debugger.ekeys:
	    env[k] = ""

	fname = self._genPathname4Environment()

	try:
	    lines = open(fname,"r").readlines()
	except:
	    return env

	for line in lines:
	    line = string.strip(line)
	    if line[-1] == '\n':
		line = line[:-1]
	    x = expr.search(line)
	    if x < 0:
		continue
	    left = string.lower(string.strip(expr.group(1)))
	    rig = string.strip(expr.group(2))
	    if left in Debugger.ekeys:
		env[left] = rig
	    #
	#
	return env

    def writeConfig(self):
        fname = self._genPathname4Environment()

        env = {}
        for k in Debugger.ekeys:
            env[k] = 0
	
        if self.globvars_dlg:
            env["dlg_globals"] = 1
            env["pos_globals"] = self.globvars_dlg.getpos()
        #
        if self.vars_dlg:
            env["dlg_locals"] = 1
            env["pos_locals"] = self.vars_dlg.getpos()
        #
        if self.stack_dlg:
            env["dlg_stack"] = 1
            env["pos_stack"] = self.stack_dlg.getpos()
        #
        if self.exception_dlg:
            env["dlg_exceptions"] = 1
            env["pos_exceptions"] = self.exception_dlg.getpos()
        #
        if self.function_dlg:
            env["dlg_functions"] = 1
            env["pos_functions"] = self.function_dlg.getpos()
        #
        env["pos_debugger"] = self.getpos()
        #
        fd = open(fname,"w")
        print "writing environment to file",fname
        for k in env.keys():
            fd.write("%s = %s\n" % (k,env[k]))
        #
        fd.close()


    def setup_source(self, filename, lineno):
	self.filename = filename
	new = 0
	try:
	    source = self.source_wins[filename]
	except KeyError:
	    new = 1
	    source = SourceText(self.source_frame, self, filename)
	    self.source_wins[filename] = source
	if source != self.cursource:
	    if self.cursource != None:
		self.cursource.text.frame.forget()
	    self.cursource = source
	    self.cursource.text.frame.pack(fill=Tkinter.BOTH, expand=1)
	self.cursource.set_lineno(lineno,filename)
	self.frame._root().title(filename)
	return new
	
    def stack_command(self):
	if not self.stack_dlg:
	    self.stack_dlg = StackDialog(self)
	
    def function_command(self):
	if not self.function_dlg:
	    self.function_dlg = FunctionDialog(self)
	
    def exception_command(self):
	if not self.exception_dlg:
	    self.exception_dlg = ExceptionDialog(self)
	
    def step_command(self, *args):
	self.set_step()
	self.cont = 1

    def trace_command(self, *args):
	self.set_trace()
	self.cont = 1

    def next_command(self, *args):
	self.set_next(self.curframe)
	self.cont = 1

    def continue_command(self, *args):
	self.set_continue()
	self.cont = 1

    def return_command(self, *args):
	self.set_return(self.curframe)
	self.cont = 1

    def show_io_command(self):
	if not self.show_lower:
	    self.bb3.buttonconfigure("ShowLower",text = "Hide I/O")
	    self.output.text.configure(state="normal")
	    self.output.text.delete("1.0","end")
	    self.output.text.configure(state="disabled")
	    self.lower_frame.pack(side=Tkinter.BOTTOM, fill=Tkinter.X,expand = 1)
	    self.show_lower = 1
	else:
	    self.bb3.buttonconfigure("ShowLower",text = "Show I/O")
	    self.lower_frame.forget()
	    self.show_lower = 0
	
    def input_event(self, event):
	line = self.input.get()
	if not line:
	    line = self.lastcommand
	self.re_eval(line)
	
    def re_eval(self, line):
	import sys, string
	line = string.strip(line)
	if line[:1] == '!': line = line[1:]
	locals = self.curframe.f_locals
	globals = self.curframe.f_globals
	globals['__privileged__'] = 1
	try:
	    code = compile(line + '\n', '<stdin>', 'single')
	    exec code in globals, locals
	except:
	    if type(sys.exc_type) == type(''):
		exc_type_name = sys.exc_type
	    else: exc_type_name = sys.exc_type.__name__
	    print '***', exc_type_name + ':', sys.exc_value
	#
	if line != self.lastcommand:
	    self.lastcommand = line
	self.history.append(line)
	self.histact = len(self.history)
	self.input.set("")

	self._updateVars()
	
    def view_file(self, filename):
	self.setup_source(filename, 1)
	root.update_idletasks()
	
    def helpBreakpoints(self):
	title = "breakpoints window"
	text = """
	Window which shows all breakpoints

	This window is modal; it is waiting for your actions
	until you close it (so maybe you can't close this
	helpwindow).
	Each breakpoint can be deleted by double click
	in the list. There is no confirmation!
	When leaving PyDebug all remaining Breakpoints will 
	be stored in a breakpoint file.
	"""
	version = 1.0
	HelpDialog(text,title,version)

    def _delBreak(self,*args):
	from regsub import split
	idx = self.bwin.listbox.curselection()[0]
	sel = self.bwin.listbox.get(idx)
	ret = split(sel," : ")
	try:
	    fname = ret[0]
	    lno = string.atol(ret[1])
	    x = self.clear_break(fname,lno)
	    # print x
	    self.bwin.listbox.delete("active")
	    self._delBreaksFromText([lno])
	    root.update()
	except:
	    pass

    def _showAllBreaksClose(self,*args):
        try:
            self.bwin.listbox.destroy()
        except:
            pass

    def showAllBreaks(self):
        bmap = self.get_all_breaks()
        top = Tkinter.Toplevel()

        bframe = Tkinter.Frame(top)
        bframe.pack(side="bottom",expand=0,fill="x")

        but = Tkinter.Button(bframe,
			     text = "Close",
			     command=self._showAllBreaksClose)
        #
        but.pack(side = "left",expand = 1,fill = "x")
        but = Tkinter.Button(bframe,
			     text = "Help",
			     command = self.helpBreakpoints)
        #
        but.pack(side = "left",expand = 1,fill = "x")

        self.bwin = ScrolledList(top)
        self.bwin.listbox.configure(background = "white", \
            foreground = "black",
            width = "60",
            height = "10",
            selectmode = "single")
        #
        self.bwin.listbox.bind("<Double-Button-1>",self._delBreak)

        kys = bmap.keys()
        kys.sort()
        for x in kys:
            xl = bmap[x]
            for y in xl:
                try:
                    cond = self.cbreaks[x,y]
                    out = " >>> " + cond
                except:
                    out = ""
                #
                self.bwin.listbox.insert("end","%s : %s%s" % (x,y,out))
            #
        #
        self.bwin.frame.pack(side = "top",expand = 1,fill = "both")

        self.bwin.listbox.wait_window()
        try:
            top.destroy()
        except:
            pass

    def showBrowser(self):
        svar = self._getsel("browse")
        if svar:
            locals = self.curframe.f_locals
            globals = self.curframe.f_globals
            if not (locals.has_key(svar) or globals.has_key(svar)):
                print "selection or entry not valid"
                return
            #
            br = Show(svar,locals,globals)
            br.activate()

    def _delBreaksFromText(self,flist):
	if type(flist) == type([]):
	    try:
		tx = self.source_wins[self.filename].getTextWidget()
	    except KeyError:
		return
	    tx.configure(state="normal")
	    for b in flist:
		tx.tag_delete("break_%d" % b)
	    #
	    tx.configure(state="disabled")
	#
	elif type(flist) == type({}):
	    for x in flist.keys():
		blist = flist[x]
		try:
		    tx = self.source_wins[x].getTextWidget()
		except KeyError:
		    continue
		tx.configure(state="normal")
		for b in blist:
		    tx.tag_delete("break_%d" % b)
		#
		tx.configure(state="disabled")
	    #
	#
	elif type(flist) == type(()):
	    fname = flist[0]
	    lno = flist[1]
	    try:
		tx = self.source_wins[fname].getTextWidget()
	    except KeyError:
		return
	    tx.configure(state="normal")
	    tx.tag_delete("break_%d" % lno)
	    tx.configure(state="disabled")
	#
	root.update()

    def delBreaks(self):
	dia = MessageDialog(title="Breakpoints",
			    text="Delete all breakpoints in file '%s' ?" % self.filename)
		
	dia.domodal()
	if dia.button == "OK":
	    self.clear_all_file_breaks(self.filename)
	    self._delBreaksFromText(self.get_file_breaks(self.filename))
	#

    def delAllBreaks(self):
	dia = MessageDialog(title="Breakpoints",
			    text="Delete all breakpoints in all files ?")

	dia.domodal()
	if dia.button == "OK":
	    self._delBreaksFromText(self.get_all_breaks())
	    self.clear_all_breaks()


    def run(self,cmd,globals=None,locals=None):
	if self.master is None:
	    self.re_init()

	# link output to our new output window
	sys.stdout = sys.stderr = self
	self.master.deiconify()
	if self.debugger_pos:
	    self.setpos(self.debugger_pos)

	print "PyDebug: Please click STEP to enter Sourcetext"

 	#try:
 	#    bdb.Bdb.run(self,cmd,globals,locals)
 	#except DebugError:
 	#    pass
	
	bdb.Bdb.run(self,cmd,globals,locals)
	# get output back to old device
	sys.stdout = self.save_stdout
	sys.stderr = self.save_stderr
	try:
	    self.cleanup()
	except: pass
    #
#


def run(cmd, globals=None, locals=None,bp_name = None):
    Debugger(bp_name).run(cmd, globals, locals)

#def runeval(expression, globals=None, locals=None):
#	x = Debugger()
#	try: return x.runeval(expression, globals, locals)
#	finally: x.close()

#def runcall(*args):
#	x = Debugger()
#	try: return apply(x.runcall, args)
#	finally: x.close()

#def set_trace():
#    Debugger().set_trace()

# Post-Mortem interface

#def post_mortem(traceback):
#    x = Debugger()
#    x.reset()
#    x.interaction(None, traceback)


#def pm():
#    import sys
#    post_mortem(sys.last_traceback)


__disclaimer__ = """
####################################################################

 Copyright 1997/1998 by Ulrich Herold, All rights reserved.
	
 Permission to use, copy, modify, and distribute this
 software and its documentation for any purpose and without
 fee is hereby granted, provided that the above copyright
 notice appear in all copies and that both that copyright
 notice and this permission notice appear in supporting
 documentation, and that the name of the author not be used
 in advertising or publicity pertaining to distribution of
 the software without specific, written prior permission. 

 THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
 SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE AUTHOR
 BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES
 OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
 OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH
 THE USE OR PERFORMANCE OF THIS SOFTWARE.

 The author requests notification of any modifications or 
 extensions to this software or its documentation. Notice
 should be sent, in the form of a modified version of the file
 and/or a context difference file, to:

	Ulrich.Herold@ProConsult-Online.com

#####################################################################
"""

if __name__ == '__main__':
    if not sys.argv[1:]:
	print "Filename is missing"
	raise SystemExit
    #
    
    filename = sys.argv[1]  # Get script filename

    del sys.argv[0]	# Hide "PyDebug.py" from argument list
    # Insert script directory in front of module search path
    sys.path.insert(0, os.path.dirname(filename))

    run(cmd = 'execfile(' + `filename` + ')', 
        globals = {'__name__': '__main__'},
        bp_name = filename)


















