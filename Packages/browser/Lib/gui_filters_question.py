import gui_message
import gui_control
import Pmw
import string
import genutil.filters
import gui_busy

class create:
    def __init__(self, parent, filter_type):
        lst = parent.panelDV.selected_list.keys()
        if len(lst)!=1:
            gui_message.error( "The '"+filter_type+" Filter  Tool' could not complete its function, only one variable at a time please" )
            return
        var=parent.panelDV.lst1[ parent.panelDV.selected_list[ lst[0] ] ]
        self.parent = parent
        self.dialog = Pmw.Dialog( parent,
                                  title = "%s Question to User"%filter_type,
                                  buttons = ('OK', 'Dismiss'),
                                  defaultbutton = 'OK',
                                  command = gui_control.Command(self.execute, parent, filter_type, var) )
        
        if parent.menu.popup_window_settings_flg == 1:
            self.dialog.transient( self.parent ) # Keep widget on top of its parent

        if filter_type=='RunningAverage':
            label="Length"
        elif filter_type=="121":
            label=""
        elif filter_type=='Custom':
            label="Filter (Enter as a list)"
        if label!="":
            self.entry=Pmw.EntryField(self.dialog.interior(),
                                      labelpos='w',
                                      label_text=label)
            self.entry.pack()

        self.radio=Pmw.RadioSelect(self.dialog.interior(),
                                   labelpos='nw',
                                   label_text='Choose which dimension to apply the filter on',
                                   buttontype = 'radiobutton',
                                   orient='vertical',
                                   )
        self.radio.pack()
        for text in var.listdimnames():
            self.radio.add(text)
        self.radio.invoke(0)
        # Position dialog popup
        parent_geom = self.parent.geometry()
        geom = string.split(parent_geom, '+')
        d1 = string.atoi( geom[1] )
        d2 = string.atoi( geom[2] )
	self.dialog.geometry( "+%d+%d" % (d1, d2) )
        
    def execute(self,parent,filter_type,var,button):
        if button=='OK':
            dim=self.radio.index(self.radio.getvalue())

            if filter_type!='121':
                entry=self.entry.getvalue()
                if entry!='':
                    entry=eval(self.entry.getvalue())
                else:
                    gui_message.error('Please Fill the Entry Field')
                    return
                
            gui_busy.busyStart( self, parent )
           
            if filter_type=='RunningAverage':
                slab=genutil.filters.runningaverage(var,entry,axis=dim)
                filter_name='RunningAverage_'+str(entry)+'_'+self.radio.getvalue()
                options=str(entry)+', axis =  '+str(dim)
                fname='runningaverage'
            elif filter_type=='Custom':
                slab=genutil.filters.custom1D(var,entry,axis=dim)
                filter_name='Custom_'+self.radio.getvalue()
                options='filter = '+str(entry)+', axis =  '+str(dim)
                fname='custom1D'
            elif filter_type=='121':
                slab=genutil.filters.smooth121(var,axis=dim)
                filter_name='121_'+self.radio.getvalue()
                options='axis =  '+str(dim)
                fname='smooth121'

                
            gui_busy.busyEnd( self, parent )
            parent.menu.pcmdi_tools_menu.evt_filters(parent,slab,filter_name,fname,var.id,options)
            self.dialog.destroy()
        else:
            self.dialog.destroy()
        return
    
