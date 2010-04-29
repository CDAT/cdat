import gui_message
import gui_control
import Pmw
import string
import genutil.filters
import gui_busy
import cdutil

class create:
    def __init__(self, parent,bounds_type):
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
                                  title = "%s Question to User"%bounds_type,
                                  buttons = ('OK', 'Dismiss'),
                                  defaultbutton = 'OK',
                                  command = gui_control.Command(self.execute, parent, bounds_type) )
        
        if parent.menu.popup_window_settings_flg == 1:
            self.dialog.transient( self.parent ) # Keep widget on top of its parent

        if bounds_type=='Xhourly':
            label="Frequency"
        if label!="":
            self.entry=Pmw.EntryField(self.dialog.interior(),
                                      validate = { 'validator':'integer','min':1,'max':24},
                                      labelpos='w',
                                      label_text=label)
            self.entry.pack()

        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )
        
    def execute(self,parent,type,button):
        if button=='OK':
            if type=='Xhourly':
                entry=self.entry.getvalue()
                if entry!='':
                    entry=eval(self.entry.getvalue())
                else:
                    gui_message.error('Please Fill the Entry Field')
                    return
                
                gui_busy.busyStart( self, parent )
                fqcy=24./int(entry)
                try:
                    var=parent.panelDV.lst1[ parent.panelDV.selected]
                except Exception,err:
                    var=None
                if var is None: 
                    gui_busy.busyEnd( self, parent )
                    gui_message.error( "The 'Set Bounds Time Tool' could not complete its function, because a variable was not selected.\nError:\n"+str(err) )
                    return
                cdutil.setTimeBoundsDaily(var,frequency=fqcy)
                gui_control.record_command( parent, "\n# X-Daily Data Bounds Set Function" , 1 )
                gui_control.record_command( parent, "cdutil.times.setTimeBoundsDaily( %s, frequency=%s )" % (var.id,str(fqcy)), 1 )            
                
            gui_busy.busyEnd( self, parent )
            self.dialog.destroy()
        else:
            self.dialog.destroy()
        return
    
