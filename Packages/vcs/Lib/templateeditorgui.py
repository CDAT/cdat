# Adapted for numpy/ma/cdms2 by convertcdms.py
#
# The VCS Template GUI controls -  templateeditorgui module
#
#################################################################################
#                                                                               #
# Module:       templateeditorgui module                                        #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  PCMDI's VCS GUI template editor.                                #
#                                                                               #
#                                                                               #
#################################################################################

import Tkinter, Pmw
from tkMessageBox import showerror, showinfo, askyesno
from tkSimpleDialog import askstring
import os, string, sys
from error import vcsError
import Ptext, Pformat, Pxtickmarks, Pytickmarks, Pboxeslines
import Plegend, Pdata, Pxlabels, Pylabels
import gui_support
import Canvas, cdms2 as cdms, numpy
#
# Create the Tkinter/Pmw template editor interface.
#
class TemplateEditor:
    "Template editor gui."
    balloon_text = \
"""This is the VCS template editor. It can edit the position and attributes
of the parts of a plot. Templates are identified by their name. 
Many of them are available and you can use this editor to create new ones.
You can apply changes to the current canvas or save the changes so that 
any future uses of that template will incorporate them.
The Revert button will restore values to what they were after the template
was last opened.
"""
    command_summary = """\
Open:   Work on a new template; close the current if there is one.

Open in new window: 
        Work on another template in new window

Close:  Close the current template. If you have changed it, you will 
        be asked if you want to save the changes.

Apply:  Apply any changes. This effects only this canvas for this session.
        Applying changes to 'default' or 'default_dud' will request a SaveAs
        instead.
        
        This effects only this canvas for this session unless you do
        a Save Session afterwards.

SaveAs: Save any changes under a new template name.
        This effects only this canvas for this session unless you do
        a Save Session afterwards.

Revert:  Undo any changes since this template was last opened.
        
Save Session: Save the state of this canvas to the initial.attributes file
              so they will be in effect for future runs.

Exit:   Exit the template editor.

There are 4 values for each quantity:
(1) the current value displayed on the screen;
(2) the value currently in the template on this canvas;
(3) the value originally in the template on this canvas when 
    the template editor last opened it;
(4) the value stored in the initial.attributes file in the PCMDI_GRAPHICS
    subdirectory of your home directory.
Apply sets (2) to (1)
Revert sets (2) and (1) to (3)
Session Save sets (1), (2), and (4) to (1)
"""
    def __init__( self, canvas=None, template_name='', gui_parent=None,
                  gm = 'isofill', gm_name='default'):
        self.gm = gm
        self.gm_name = gm_name #these are used for sample plots
        if canvas is None:
            canvas = Canvas.Canvas()
        self.canvas = canvas
        canvas.canvas_template_editor = self
        self.dialog = gui_support.VcsDialog(command=self.button_dispatch,
                            defaultbutton='Apply',
                            buttons=('Apply','Apply & Exit','Save Session',
                                     'Revert')
                           )
        gui_support.balloon.bind(self.dialog.component('buttonbox'),
                                 TemplateEditor.command_summary
                                 )
        self.dialog.dialog.withdraw()
        self.gui_parent = gui_parent
        self.template_name = ''
        gui_support.balloon.bind(self.dialog.interior(), TemplateEditor.balloon_text)   
        # create the templateeditor menus
        main_menu = Pmw.MenuBar(self.dialog.interior(),
                hull_relief = 'raised',
                hull_borderwidth = 2,
                balloon=gui_support.balloon
                )
        main_menu.pack(side='top', fill='both')
        self.create_filemenu(main_menu, canvas )
        self.create_help_menu(main_menu)
        self.create_dialogs()
        self.editors = []
        self._create_data()  #sample variable
        
        # Create the notebook, with 'lazy' page creation.
        self.notebook = Pmw.NoteBook(self.dialog.interior(), createcommand=self.makepages)
        self.notebook.pack(side='top', fill='both', expand = 1, 
                           padx = 3, pady = 3)
        # Decide where to put it on the screen
        if gui_parent is None:
            d=[int(self.dialog.dialog.winfo_screenwidth()/6),
               int(self.dialog.dialog.winfo_screenheight()/6)
              ]
            self.dialog.geometry("+%s+%s" % (d[0],d[1]))
        else:
            self.dialog.position_over(gui_parent)
        
        self.dialog.dialog.deiconify()
        if template_name == '':
            self.open()
        else:
            self._open_template(template_name)
        self.dialog.dialog.lift()
        

    def _create_data(self):
        a1 = numpy.arange(10) * 18.0 - 90.
        a2 = numpy.arange(10) * 36.0 - 180.
        d = numpy.outerproduct(numpy.sin(a1*numpy.pi/360.), 
                            numpy.cos(a2*numpy.pi/360.))
        d.shape=(1,1,10,10)
        a1a = cdms.createAxis(a1)
        a1a.designateLatitude()
        a2a = cdms.createAxis(a2)
        a2a.designateLongitude()
        a3a = cdms.createAxis([1979.])
        a3a.designateTime()
        a4a = cdms.createAxis([0.5])
        a4a.designateLevel()
        data = cdms.MV2.array(d, axes=[a3a, a4a, a1a ,a2a])
        self.data = data

    def create_dialogs(self):
        "Create the dialogs to be raised and lowered as required."
        self.template_selector = Pmw.SelectionDialog(self.dialog.interior(),
                                 title="Template selection",
                                 buttons=('OK','Cancel'),
                                 defaultbutton='OK',
                                 label_text="Choose a template to edit.",
                                 scrolledlist_labelpos = 'n',
                                 command=self.process_template_choice
                                )
        self.template_selector.component('scrolledlist').component('listbox').configure(selectmode='single')
        self.template_selector.component('hull').transient(self.dialog.interior())
        self.template_selector.withdraw()
        if self.gui_parent is None:
           # no point in a simple save, would be lost.
           # Don't change labels unless you change ask_save_execute too
            buttons=('Save Template Changes Permanently',
                    'Discard Changes',
                    'Cancel',
                    'Help',
                    )
        else:
           # Don't change labels unless you change ask_save_execute too
            buttons=('Save Template Changes to Canvas',
                    'Save Template Changes Permanently',
                    'Discard Changes',
                    'Cancel',
                    'Help',
                    )
        self.asksavedialog = Pmw.Dialog(self.dialog.interior(),
                       title = "Save Changes?",
                       buttonboxpos='w',
                       buttons = buttons,
                       command=self.ask_save_execute
                      )
        self.asksavedialog.withdraw()
        self.asksavetb = None

    def update_template_selector_list(self):
        choices = vcs.listelements('template')
        choices = map(lambda x: x.strip(), choices)
        choices.sort()
        slb = self.template_selector.component('scrolledlist')
        slb.setlist(choices)

    def _open_template (self, template_name):
        "Assumes we are closed."
        if not template_name: return
        self.template_name = template_name
        self.template = self.canvas.gettemplate(template_name)
        self.editors = []
        self.show()
    # Note, if you change one of these names also need to change in makepages.
    # The individual editors on the  pages are not created unless the user selects that page.
    # The loops for updating, etc. are over the editors that *have* been created.
        self.create_page('Data')
        self.create_page('Metadata')
        self.create_page('X')
        self.create_page('Y')
        self.create_page('T & Z')
        self.create_page('Comments')
        self.create_page('Boxes & Lines')
        self.set_title()

    def show(self):
        "Show the current situation on the canvas"
        self.canvas.clear()
        if not self.is_opened(): 
            return
        self.canvas.plot(self.data, self.template_name, self.gm, self.gm_name,
            name = self.template_name,
            comment1 = 'comment1',
            comment2 = 'comment2',
            comment3 = 'comment3',
            comment4 = 'comment4',
            file_comment = 'file_comment',
            long_name = 'long_name',
            units = 'units',
            xunits = 'xunits',
            yunits = 'yunits',
            tunits = 'tunits',
            zunits = 'zunits',
            xname = 'xname',
            yname = 'yname',
            tname = 'tname',
            zname = 'zname'
        )


    def create_page (self, name):
        self.notebook.add(name)
        gui_support.balloon.bind(self.notebook.tab(name), 
"""This page contains individual editors for some attributes of the template
   related to %s. Click the tab to bring the page to the front.
""" % name)

    def set_title(self):
        if self.template_name:
            title = "VCS Template Editor: " + self.template_name 
        else:
            title = "VCS Template Editor" 
        self.dialog.title(title)

    def makepages(self, name):
        "This is called the first time a given page is selected."
        if name == 'Data':
            self.editor_page(name,
                         [['dataname', 'title'], 
                          ['min','max','mean'],
                          ['data','legend']
                         ]
                       )
        elif name == 'X':
            self.editor_page(name,
                         [['xname','xvalue'],
                          ['xlabel1', 'xtic1', 'xmintic1'],
                          ['xlabel2', 'xtic2', 'xmintic2'],
                         ]
                       )
        elif name == 'Y':
            self.editor_page(name,
                         [['yname','yvalue'],
                          ['ylabel1', 'ytic1', 'ymintic1'],
                          ['ylabel2', 'ytic2', 'ymintic2'],
                         ]
                       )
        elif name == 'T & Z':
            self.editor_page(name,
                         [['zname', 'zvalue'],
                          ['tname', 'tvalue']
                         ]
                       )
        elif name == 'Metadata':
            self.editor_page(name,
                         [\
                          ['source','file','crdate'],
                          ['units','tunits','crtime'], 
                          ['xunits', 'yunits','zunits'],
                         ]
                       )
        elif name == 'Comments':
            self.editor_page(name,
                         [['comment1','comment2'],
                          ['comment3', 'comment4']
                         ]
                       )
        elif name == 'Boxes & Lines':
            self.editor_page(name,
                         [['box1', 'box2','box3'],
                          ['box4', 'line1','line2'],
                          ['line3','line4']
                         ]
                       )
        self.notebook.setnaturalsize()

    def editor_page(self, name, list2d):
        f = Tkinter.Frame(self.notebook.page(name))
        f.pack(side='top', anchor='nw')
        editors=[]
        rows = 0
        columns = 0
        for y in list2d:
            c = 0
            for x in y:
                if c > columns: columns = c
                v = getattr(self.template, x)
                if v.__class__ is Ptext.Pt:
                    members = ['x','y','textorientation','texttable']
                elif v.__class__ is Pformat.Pf:
                    members = ['x','y','format']
                elif v.__class__ is Pxtickmarks.Pxt:
                    members = ['y1','y2','line']
                elif v.__class__ is Pytickmarks.Pyt:
                    members = ['x1','x2','line']
                elif v.__class__ is Pboxeslines.Pbl:
                    members = ['x1','x2','y1','y2','line']
                elif v.__class__ is Plegend.Pls:
                    members = ['x1','x2','y1','y2','line','textorientation',
                               'texttable']
                elif v.__class__ is Pdata.Pds:
                    members = ['x1','x2','y1','y2']
                elif v.__class__ is Pxlabels.Pxl:
                    members = ['y','textorientation','texttable']
                elif v.__class__ is Pylabels.Pyl:
                    members = ['x','textorientation','texttable']
                else:
                    raise vcsError, 'Internal error in template editor gui.'
                e = TemplateAttributeEditor(f, self, v, members)
                e.grid(row=rows, column=c, padx=2, sticky='nw')
                editors.append(e)
                c += 1   
            rows += 1
        self.editors += editors

    def create_filemenu(self, main_menu, canvas):
        main_menu.addmenu('Template', TemplateEditor.command_summary, tearoff = 1)
        main_menu.addmenuitem('Template', 'command', 
                  "Open: Work on a new template; close the current if there is one.",
                  label = "Open", command = self.open,
                          )
        main_menu.addmenuitem('Template', 'command',  
                  "Open in new window: Work on another template in new window.",
                  label = "Open in new window", command = self.new_window,
                  )
        main_menu.addmenuitem('Template', 'command', 
                  """Close: close the current template. If you have changed it, you will 
                     be asked if you want to save the changes.""",
                  label = "Close", command = self.close,
                  )
        main_menu.addmenuitem('Template', 'command', 
                  """Apply any changes. This effects only this canvas for this session.""",
                  label = "Apply", command = self.save,
                  )
        main_menu.addmenuitem('Template', 'command', 
                  """SaveAs: save any changes under a new template name. 
                     This effects only this canvas for this session.""",
                  label = "Save As", command = self.saveas,
                  )
        main_menu.addmenuitem('Template', 'command', 
                  """Revert: restore values to what they were when this
                     template was opened.
                     This effects only this canvas for this session.""",
                  label = "Revert", command = self.revert,
                  )
        main_menu.addmenuitem('Template', 'separator')
        main_menu.addmenuitem('Template', 'command', 
                  """Save Session: save any changes under a new template name. 
                     This effects only this canvas for this session.""",
                  label = "Save Session", command = self.savesession,
                  )
        main_menu.addmenuitem('Template', 'separator')
        main_menu.addmenuitem('Template', 'command', 
                          'Exit: exit the template editor',
                          label = "Exit",
                          command = self.exit_editor
                         )


    def button_dispatch (self, name):
        if name is None: self.exit_editor()
        elif name == 'Apply': self.save()
        elif name == 'Apply & Exit': self.ok()
        elif name == 'Save Session': self.savesession()
        elif name == 'Revert': self.revert()

    def ok(self):
        self.save()
        self.exit_editor()

    def apply(self):
        changes = 0
        mode = self.canvas.mode
        self.canvas.mode = 0
        for e in self.editors:
            changes += e.apply()
        self.mode = mode
        self.show()

    def refresh_data(self, attr_name, mem_name, value):
        a =  dir(self.editors) # This function is need to flush the events. I don't undstanad why?
        for i in range(len(self.editors)):
            if self.editors[i].attribute_name == attr_name:
               self.editors[i].entries[mem_name].set(value)

#    def refresh_data(self, attr_name, mem_name, value):
##        print "I am here, I have ", attr_name, mem_name, value
##        print "dir self = ", dir(self)
##        # Get the template values
##        v = getattr(self.template, attr_name)
##        print 'legend values = ', v.priority, v.x1, v.x2, v.y1, v.y2, v.line, v.textorientation, v.texttable
#
#        # Set the template values
##        print 'self.editors = ', dir(self.editors[0]), self.editors[0], len(self.editors)
##        print dir(self.editors)
#        a =  dir(self.editors) # This function is need to flush the events. I don't undstanad why?
#        for i in range(len(self.editors)):
##            print 'attribute_name = ', self.editors[i].attribute_name, self.editors[i].members
##            print 'entries = ', self.editors[i].entries
#            if self.editors[i].attribute_name == attr_name:
#               self.editors[i].entries[mem_name].set(value)
##               print "I am here in the set", mem_name, value, i
            
    def is_opened (self):
        "Is this editor open now?"
        return not (self.template_name == '')

    def is_changed (self):
        "Has a modification been made?"
        changes = 0
        for e in self.editors:
            changes += e.is_changed()
        return changes > 0

    def is_changed_from_original (self):
        "Has a modification been made from the original?"
        changes = 0
        for e in self.editors:
             changes += e.is_changed_from_original()
        return changes > 0
 
    def save (self):
        "Apply to template. Returns 1 if save was legal."
        if not self.is_opened():
            return
        if not self.is_changed():
            return
        if self.template_name == 'default' or \
            self.template_name == 'default_dud':
            self.saveas()
            return 
        self.apply()
        return 

    def saveas(self):
        if not self.is_opened():
            return 0
        s = askstring('Template Name', "Save as?")
        if s is None: 
            return
        if s in vcs.listelements('template'):
            showerror('Error Message', 'There already is a template named '+s)
            return
        self.canvas.createtemplate(s, self.template_name)
        tnew = self.canvas.gettemplate(s)
        for e in self.editors:
            e.apply_to (tnew)
        self._raw_close()
        self._open_template(s)
        

    def savesession(self):
        if not self.is_opened():
            return 
        if self.template_name == 'default' or \
            self.template_name == 'default_dud':
            showerror('Save Session Error', 'You cannot modify default or default_dud.')
            return 
        result = askyesno('Save Session?', 'Save these values permanently?')
        if result:
            self._savesession()

    def _savesession(self):
        dotdir=self.canvas.getdotdirectory()[0]
        self.apply()
        self.canvas.saveinitialfile()
        s = Pmw.TextDialog(self.dialog.interior(), title='Session saved.')
        s.transient(self.dialog.interior())
        self.dialog.position_popup(s)
        s.insert('end',
"""Session saved.
The old script file 
%s/%s/initial.attributes
has been saved as
%s/%s/initial.attributes%%.
""" % (os.environ['HOME'], dotdir,
       os.environ['HOME'], dotdir)
        )

    def exit_editor(self):
        "Invoked for exit and the cancel button"
        result = self.ask_save()
        if result is None or result == 'Cancel':
            return
        self.dialog.destroy()

    def new_window (self, template_name=''):
        if template_name != '' and template_name == self.template_name:
            showerror('Error message', template_name + ' is already open.')
            return
        TemplateEditor(canvas=self.canvas, template_name=template_name)

    def ask_save2 (self):
        print 'Voila!'
    def ask_save (self):
        """Ask user if he wants to save changes, if necessary."""
        print 'ok we are in!'
        if not self.is_opened():
            print 'no isopened'
            return 'OK'
        if self.is_changed() or self.is_changed_from_original():
            g = self.dialog.geometry().split('+')[1:]
            result = self.asksavedialog.activate(geometry="+%s+%s" %(g[0],g[1]))
            self.asksavedialog.withdraw()
            print 'did something'
            return result
        print 'last return'
        return 'OK'

    def ask_save_execute (self, name):
        if name is None:
            self.asksavedialog.deactivate(name)
        elif name == 'Cancel':  # this return value used by callers to cancel op
            self.asksavedialog.deactivate(name)
        elif name == 'Discard Changes':
            self.asksavedialog.deactivate(name)
        elif name == 'Save Template Changes to Canvas':
            self.save()
            self.asksavedialog.deactivate(name)
        elif name == 'Save Template Changes Permanently':
            self.savesession()
            self.asksavedialog.deactivate(name)
        elif name == 'Help':
            if self.asksavetb is not None: return
            self.asksavetb = Pmw.ScrolledText(self.asksavedialog.interior())
            self.asksavetb.pack(side='left', anchor='n', padx=5)
            if self.gui_parent is None:
                canvas_changes_msg="""\
You must save your choices permanently
or they will be lost. """
            else:
                canvas_changes_msg = """\
Save Template Changes to Canvas 
    Sends the changes to the template on this canvas 
    but does not yet make the change permanent. """ 
            self.asksavetb.settext("""\
You have made changes in the display that are not yet
applied to the canvas and/or you have made changes 
to the canvas that are not yet saved in your permanent file. 

%s

Save Template Changes Permanently
     Permanently changes the initial.attributes file. 
     You will be asked to confirm this choice.

Discard Changes 
     Closes the current template without saving any changes you 
     have made in the editor's display.

Cancel 
     Cancels the entire current operation. 
     The changes you've made are still in the display.
"""% canvas_changes_msg)
        
    def close(self):
        if not self.is_opened():
            return
        result = self.ask_save()
        if result is None or result == 'Cancel':
            return
        self._raw_close()

    def _raw_close (self):
        for e in self.editors:
            e.destroy()
        self.editors=[]
        self.notebook.delete(*self.notebook.pagenames())
        self.set_title()
        self.canvas.clear()
        self.canvas.close()
        
    def open (self):
        self.close()
        self.update_template_selector_list()
        g = self.dialog.geometry()
        d = g.split("+")[1:]
        placement = "+%s+%s" % (d[0], d[1])
        self.template_selector.activate(geometry=placement)

    def process_template_choice (self, result):
        if result == 'OK':
            sl = self.template_selector.component('scrolledlist')
            sels = sl.getcurselection()
            if len(sels) ==1:
                self._open_template(sels[0])
        self.template_selector.deactivate()

    def revert(self):
        if not self.is_opened():
            return
        for e in self.editors:
            e.revert()
        self.show()
        
    def create_help_menu( self, main_menu):
        main_menu.addmenu('Help', TemplateEditor.command_summary, 
                           side='right', tearoff = 1)
        gui_support.add_balloon_help(main_menu, 'Help')
        main_menu.addmenuitem('Help', 'command', 'Command Summary',
                            label = 'Command Summary',
                            command = self.evt_command_summary
                           )
        main_menu.addmenuitem('Help', 'separator')
        main_menu.addmenuitem('Help', 'command', 'Help About',
                            label = 'About the Template Editor',
                            command = self.evt_about_dialog
                           )

    def evt_command_summary (self):
        s = Pmw.TextDialog(self.dialog.interior(), title='Command summary')
        s.transient(self.dialog.interior())
        self.dialog.position_popup(s)
        s.insert('end', TemplateEditor.command_summary)

    def evt_about_dialog(self):
        Pmw.aboutversion(sys.prefix)
        Pmw.aboutcopyright('Copyright:    2001, Regents of the University of California\n')
        Pmw.aboutcontact(
"""Go to cdat.sourceforge.net for documentation, support, bug reporting, 
   and releases.

   Program for Climate Model Diagnosis and Intercomparison
   Lawrence Livermore National Laboratory Livermore, CA 94550 
""")
        about = Pmw.AboutDialog(self.dialog.interior(), 
                                applicationname = 'VCS Template Editor')
        about.transient(self.dialog.interior()) 
        self.dialog.position_popup(about)

class SingleEditor (Pmw.LabeledWidget):
    """An editor for a single quantity."""
    def __init__ (self, parent, name, editor):
        Pmw.LabeledWidget.__init__ (self, parent,labelpos='w',
                                    label_text=entry_labels[name])
        self.name = name
        self.editor = editor
        self.original_value = getattr(editor.template_attribute, name)
        self.value_in_template = self.original_value
        msg = \
"""Modifying this sets a new value for the %s field in attribute %s of 
   template %s.  
""" % \
                ( self.name, 
                  self.editor.template_attribute.member, 
                  self.editor.template_editor.template_name
                )
        gui_support.balloon.bind(self.interior(), msg)

# Children to define get and set to get and set the value displayed.

    def revert (self):
        if self.is_changed_from_original():
            self.set(self.original_value)
            ta = self.editor.template_attribute
            setattr(ta, self.name, self.original_value)
        self.value_in_template = self.original_value
        
    def is_changed(self):
        return self.get() != self.value_in_template

    def is_changed_from_original(self):
        return self.get() != self.original_value

    def apply (self):
        if self.is_changed():
            v = self.get()
            ta = self.editor.template_attribute
            setattr(ta, self.name, v)
            self.value_in_template = v
            return 1
        else:
            return 0

    def apply_to (self, ta):
        v = self.get()
        setattr(ta, self.name, v)

    def color_field (self, color):
        pass

class CounterEditor (SingleEditor):
    def __init__ (self, parent, name, editor):
        SingleEditor.__init__ (self, parent, name, editor)
        self.widget = Pmw.Counter(self.interior(), 
                           entry_width=5,
                           entryfield_value = self.original_value, 
                           entryfield_modifiedcommand=self.changed,
                           entryfield_validate = {'validator':'integer', 
                                       'min':0, 'minstrict':1
                                      }
                         )
        self.widget.pack(fill='both', expand=1)
        self.widget.component('entry').unbind("<Return>")

    def changed(self):
        if self.name == 'priority':
             self.editor.priority_check()

    def get (self):
        "Return value"
        return int(self.widget.component('entryfield').component('entry').get())

    def set (self, value):
        self.widget.component('entryfield').setentry(str(value))

    def color_field (self, color):
        self.widget.component('entryfield').component('entry').configure(fg=color)

class SelectionEditor (SingleEditor):
    def __init__ (self, parent, name, editor):
        SingleEditor.__init__ (self, parent, name, editor)
        choices = self.editor.canvas.listelements(name)
        choices = map(lambda x: x.strip(), choices)
        if self.original_value not in choices:
            showerror('Error Message', 
                      self.editor.name + ' does not have a choice ' +
                      self.original_value + ' for ' + name + """.
                      Will change to first choice available.""")
            self.original_value = choices[0]
            
        w = 8
        for x in choices:
            w = max(w, len(x))
        self.widget = Pmw.ComboBox(self.interior(), 
                           history=0,
                           scrolledlist_items = choices,
                           selectioncommand=self.changed,
                         )
            
        self.widget.selectitem(self.original_value)
        self.widget.component('entry').configure(width=w,state='disabled')
        self.widget.pack(fill='both', expand=1)
        self.widget.component('entry').unbind("<Return>")
        self.set(self.original_value)

    def changed(self, value):
        pass

    def get (self):
        "Return value"
        return self.widget.get()

    def set (self, value):
        self.widget.selectitem(value)

    def color_field (self, color):
        self.widget.component('entryfield').component('entry').configure(fg=color)

class FloatEditor (SingleEditor):
    def __init__ (self, parent, name, editor):
        SingleEditor.__init__ (self, parent, name, editor)
        v = self.format(self.original_value)
        self.widget = Pmw.EntryField(self.interior(), 
                           value=v,
                           modifiedcommand=self.changed,
                           validate={'validator':'real'}
                         )
        self.widget.component('entry').configure(width=8)
        self.normalbackground = self.widget.component('entry').cget('background')
        self.colorize(self.original_value)
        self.widget.pack(fill='both', expand=1)
        self.widget.component('entry').unbind("<Return>")

    def get (self):
        "Return value"
        return string.atof(self.widget.get())
    
    def is_changed(self):
        return abs(self.get()-self.value_in_template) >= .0015
        
    def is_changed_from_original(self):
        return abs(self.get()-self.original_value) >= .0015
        
    def changed(self):
        if self.widget.valid():
            v = float(self.get())
            self.colorize(v)
        else:
            self.colorize(-1.)

    def format (self, value):
        v = "%6.3f" % value
        return v

    def set(self, value):
        v = self.format(value)
        self.widget.setentry(v)

    def colorize (self, value):
        if value < 0.0 or value > 1.0:
            self.widget.component('entry').configure(bg='pink')
        else:
            self.widget.component('entry').configure(bg=self.normalbackground)

    def color_field (self, color):
        self.widget.component('entry').configure(fg=color)

# Some global dictionaries to map names to editors, labels, help messages
entry_editors = { 
                'priority': CounterEditor,
                'format': SelectionEditor,
                'line': SelectionEditor,
                'textorientation': SelectionEditor,
                'texttable': SelectionEditor,
                'x': FloatEditor,
                'y': FloatEditor,
                'x1': FloatEditor,
                'y1': FloatEditor,
                'x2': FloatEditor,
                'y2': FloatEditor,
                }
entry_labels = { 
                'priority': 'Priority',
                'format': 'Format',
                'line': 'Line',
                'textorientation': 'Orientation',
                'texttable': 'Properties',
                'x': 'X',
                'y': 'Y',
                'x1': 'X1',
                'y1': 'Y1',
                'x2': 'X2',
                'y2': 'Y2',
                }
attribute_labels = {
    'title': 'long_name',
    'source': 'file_comment',
    }
attribute_explanations = {
    'dataname': 'The name of the variable',
    'title': 'Full name of the variable or other title',
    'legend': 'The colorbar or scale and its annotations',
    'source': 'Comment about the origin of the data.',
    'comment1': 'One of four available comment lines.',
    'comment2': 'One of four available comment lines.',
    'comment3': 'One of four available comment lines.',
    'comment4': 'One of four available comment lines.',
    'crdate': 'Data creation date',
    'file': 'Data file',
    'crtime': 'Data creation time',
    'units': 'The units of the variable',
    'xunits': 'The units of the x-axis',
    'yunits': 'The units of the y-axis',
    'tunits': 'The units of the t-axis',
    'zunits': 'The units of the z-axis',
    'min': 'The minimum value of the data',
    'max': 'The maximum value of the data',
    'mean': 'The mean value of the data',
    'data': 'The body of the plot',
    'box1': 'One of four available boxes you can draw',
    'box2': 'One of four available boxes you can draw',
    'box3': 'One of four available boxes you can draw',
    'box4': 'One of four available boxes you can draw',
    'line1': 'One of four available auxilliary lines you can draw',
    'line2': 'One of four available auxilliary lines you can draw',
    'line3': 'One of four available auxilliary lines you can draw',
    'line4': 'One of four available auxilliary lines you can draw',
    'xname': 'The name of the x-axis',
    'yname': 'The name of the y-axis',
    'tname': 'The label connected with the tvalue',
    'tvalue': 'The value of t; if priority 0, tname not shown either.',
    'zname': 'The label connected with the zvalue',
    'zvalue': 'The value of z; if priority 0, zname not shown either.',
    }

class TemplateAttributeEditor (Pmw.Group):
    "An entry widget for an attribute of the template."
    def __init__ (self, parent, template_editor, v, members):
        Pmw.Group.__init__ (self, parent,
                            tag_text = attribute_labels.get(v.member, v.member)
                            )
        gui_support.balloon.bind(self.component('tag'),
                                 attribute_explanations.get(v.member, v.member)
                                 )
        self.template_editor = template_editor
        self.template_attribute = v
        self.members = ['priority'] + members
        self.canvas = template_editor.canvas
        self.entries = {}
        self.attribute_name = v.member
        f = self.interior()
        r = 0
        for itemname in self.members:
            if entry_editors.has_key(itemname):
                e = entry_editors[itemname](self.interior(), itemname, self)
            else:
                print  'Template editor internal error, missed', itemname
            e.pack(side='top', expand=1, fill='x')
            self.entries[itemname] = e
            r += 1
        Pmw.alignlabels(self.entries.values())
        self.priority_check()

    def priority_check (self):
        v = self.entries['priority'].get()
        for name, e in self.entries.items():
            if name == 'priority': continue
            if v == 0:
                e.component('label').configure(fg='gray')
                e.color_field('gray')
            else:
                e.component('label').configure(fg='black')
                e.color_field('black')
                
    def apply (self):
        "Save the value from the display into the template."
        changes = 0
        for e in self.entries.values():
            changes += e.apply()
        return changes
    
    def apply_to (self, t):
        "Save the value from the display into the template t."
        ta = getattr(t, self.attribute_name)
        for e in self.entries.values():
            e.apply_to(ta)
    
    def is_changed (self):
        "Are any of the entries of this attribute changed?"
        changes = 0
        for name, e in self.entries.items():
            changes += e.is_changed()
        return changes

    def is_changed_from_original (self):
        "Are any of the entries of this attribute changed?"
        changes = 0
        for name, e in self.entries.items():
            changes += e.is_changed_from_original()
        return changes

    def revert (self):
        "Set everything back to the original value."
        for e in self.entries.values():
            e.revert()

# Create/Popup template editor for VCS.
def create(canvas=None, template_name='', gui_parent=None):
    TemplateEditor(canvas, template_name, gui_parent)
