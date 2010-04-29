#!/usr/bin/env python
#
# The OPeNDAP server control interface -  gui_opendap module
#
#################################################################################
#                                                                               #
# Module:       gui_opendap module                                              #
#                                                                               #
# Copyright:    "See file Legal.htm for copyright information."                 #
#                                                                               #
# Authors:      PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  This module will allow for the user to select OPeNDAP servers;  #
#               Authenticate to the OPeNDAP server if necessary, display the    #
#               OPeNDAP tree for browsing; perform google like search on the    #
#               OPeNDAP server; and display metadata information.               #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################

#---------------------------------------------------------------------
# NOTE: need to use version of Python that imports Tkinter and Pmw
#---------------------------------------------------------------------
import Tkinter, Pmw
import gui_tree
import gui_support
import gui_control
import gui_message
import gui_select_variable
import http_thredds
import http_opendap
import urllib2


#---------------------------------------------------------------------------------
# Custom Tree node that captures click events better
# This improves the way the tree nodes are selected when clicked
#---------------------------------------------------------------------------------
class CustomNode(gui_tree.Node):
    def __init__(self, *args, **kw_args):
        apply(gui_tree.Node.__init__, (self,)+args, kw_args)
        
        self.widget.tag_bind(self.symbol, '<1>', self.evt_click_node)
        self.widget.tag_bind(self.label, '<1>', self.evt_click_node)

    def evt_click_node(self, event):
        self.widget.move_cursor(self)

#---------------------------------------------------------------------------------
# Create the Tkinter/Pmw OPeNDAP browser and search interface
#---------------------------------------------------------------------------------
class create:
    def __init__( self, gui_parent=None, transient=0):
        title = "THREDDS/OPeNDAP Catalog Browser"
        self.dialog = gui_support.VcsDialog(title=title,
                                 buttons=(),
                                 )
        self.dialog.dialog.withdraw()
        parent = self.dialog.interior()
        self.parent = parent
        
        self.gui_parent = gui_parent
        
        #-------------------------------------------------------------------------
        # Create browsing widgets (catalog combo box, browse/search, search field
        #-------------------------------------------------------------------------
        self.topFrame = Tkinter.Frame(parent)
        
        
        self.servers = {
        #            'CCSM' : {
        #                'thredds_url' : 'http://dataportal.ucar.edu/metadata/cgd/ccsm/thredds/ccsm.thredds'
        #                },
        #            'CCSM POP' : {
        #                'thredds_url' : 'http://dataportal.ucar.edu/metadata/cgd/pop/thredds/ucar.cgd.pop.thredds'
        #                },
        #            'PCM' : {
        #                'thredds_url' : 'http://dataportal.ucar.edu/metadata/cgd/pcm/thredds/ucar.cgd.pcm.thredds'
        #                },
        #            'POP' : {
        #                'thredds_url' : 'http://dataportal.ucar.edu/metadata/lanl/pop/thredds/lanl.pop.thredds'
        #                },
        #            'ESG2-TEST' : {
        #                'thredds_url' : 'http://esg2.llnl.gov:8000/opendap/data/?thredds'
        #                },
        #            'ANANKE-TEST' : {
        #                'thredds_url' : 'http://ananke.llnl.gov:8000/tmp/index.thredds'
        #                },
                    'IPCC AR4' : {
                        'thredds_url' : 'http://esgcet.llnl.gov/dap/ipcc4/?thredds'
                        }
                }
        
        self.passwordMgr = urllib2.HTTPPasswordMgrWithDefaultRealm()
        authHandler = urllib2.HTTPBasicAuthHandler(self.passwordMgr)
        opener = urllib2.build_opener(authHandler)
        urllib2.install_opener(opener)
         
        self.ptls = Pmw.ComboBox(
                            self.topFrame,
	                        labelpos = 'n',
                            label_text = 'THREDDS Catalogs:',
                            entryfield_value = self.servers.keys()[0],
                            entry_background = 'white',
                            entry_foreground = 'black',
                            selectioncommand = self.evt_select_opendap_server,
                            scrolledlist_items = self.servers.keys()
                            )
        self.ptls.pack(side=Tkinter.LEFT, padx=5)        
        gui_parent.balloon.bind(self.ptls, 'Select the THREDDS Catalog to display the Directory Tree')
        
        
        #-------------------------------------------------------------------------
        # Create and pack a RadioSelect widget, with radiobuttons.
        #-------------------------------------------------------------------------
        #self.selmode = Pmw.RadioSelect(
        #                    self.topFrame,
		#                    buttontype = 'radiobutton',
        #                    orient = 'vertical',
        #                    #command = self.callback,
        #                    hull_borderwidth = 2,
        #                    hull_relief = 'ridge',
        #                    )
        #self.selmode.pack(side=Tkinter.LEFT, padx=5)
        #gui_parent.balloon.bind( self.selmode, 'Choose which method to search for data.' )

        # Add some buttons to the radiobutton RadioSelect.
        #for text in ('Browse', 'Search'): self.selmode.add(text)
        #self.selmode.invoke('Browse')

        #-------------------------------------------------------------------------
        # Add search entry window
        #-------------------------------------------------------------------------
        #self.searchtxt = Pmw.EntryField(
        #                    self.topFrame, 
        #                    labelpos = 'n',
        #                    label_text = 'Search',
        #                    entry_background = 'white',
        #                    entry_foreground = 'black'
        #                    )
        #self.searchtxt.pack(side=Tkinter.LEFT, fill=Tkinter.X, expand=Tkinter.YES, padx=5)
        #gui_parent.balloon.bind( self.searchtxt, 'Enter the search text.' )

        
        self.topFrame.place(relx=0.0, rely=0.0, relwidth=1.0, relheight=0.2)
        
        
        #-------------------------------------------------------------------------
        # Create the labels
        #-------------------------------------------------------------------------
        self.middleFrame = Tkinter.Frame(parent)
        
        self.lbl_tree = Tkinter.Label(
                        self.middleFrame,
                        text="THREDDS Catalog Directory Tree",
                        )
        self.lbl_tree.place(relx=0.0, rely=0.0, relwidth=0.5, relheight=1.0)
        
        self.lbl_metadata = Tkinter.Label(
                        self.middleFrame,
                        text="File Metadata",
                        )
        self.lbl_metadata.place(relx=0.5, rely=0.0, relwidth=0.5, relheight=1.0)
        
        self.middleFrame.place(relx=0.0, rely=0.2, relwidth=1.0, relheight=0.05)
        
        #-------------------------------------------------------------------------
        # Create the Tree control and metadata windows
        #-------------------------------------------------------------------------
        self.bottomFrame = Tkinter.Frame(parent)
        self.bottomFrame.grid_rowconfigure(0, weight=1)
        self.bottomFrame.grid_columnconfigure(0, weight=1)
        
        # The tree control
        self.trb1 = gui_tree.Tree(
                                master=self.bottomFrame,
                                background = 'orange',
                                relief = 'sunken',
                                borderwidth=2,
                                root_id=0,
                                root_label='',
                                get_contents_callback=self.evt_expand_node,
                                node_class=CustomNode
                                ) 

        self.trb1.root.set_label('Select a THREDDS Catalog.')
        self.trb1.first()
        
        self.trb1.grid(row=0, column=0, sticky='nsew')
        
        sb = Tkinter.Scrollbar(parent)
        self.trb1.configure(yscrollcommand=sb.set)
        sb.configure(command=self.trb1.yview)
        sb.place(relx=0.468, rely=0.25, relwidth=0.032, relheight=0.62)
        
        sb2 = Tkinter.Scrollbar(parent, orient=Tkinter.HORIZONTAL)
        self.trb1.configure(xscrollcommand=sb2.set)
        sb2.configure(command=self.trb1.xview)
        sb2.place(relx=0.0, rely=0.866, relwidth=0.47, relheight=0.054)

        gui_parent.balloon.bind(self.trb1, 'Browse the THREDDS catalog for data.')
   
        self.bottomFrame.place(relx=0.0, rely=0.25, relwidth=0.468, relheight=0.616)

        #-------------------------------------------------------------------------
        # Capture left mouse clicks on the tree
        #-------------------------------------------------------------------------
        self.trb1.bind('<1>', self.evt_click_tree)

        #-------------------------------------------------------------------------
        # Create the metadata information scroll window
        #-------------------------------------------------------------------------
        self.txt = Pmw.ScrolledText(parent,
                               text_background = 'white',
                               text_foreground = 'black',
                              )
        self.txt.place(relx=0.5,rely=0.25,relwidth=0.5,relheight=0.67)
        gui_parent.balloon.bind(self.txt, 'File metadata.')
    
        # ------------------------------------------------------------------------
        # Button bar
        # ------------------------------------------------------------------------
        self.buttonFrame = Tkinter.Frame(parent)
        
        self.btn_cancel = Tkinter.Button(
                    self.buttonFrame,
                    text='Cancel',
                    command=self.evt_btn_cancel
                   )
        self.btn_cancel.pack(side=Tkinter.RIGHT, padx=5, pady=4)
        gui_parent.balloon.bind(self.btn_cancel, 'Cancel.')
        
        self.btn_select = Tkinter.Button(
                    self.buttonFrame,
                    text='Open File & Exit',
                    command=gui_control.Command( self.evt_btn_select_file, "exit")
                   )
        self.btn_select.pack(side=Tkinter.RIGHT, padx=5, pady=4)
        gui_parent.balloon.bind(self.btn_select, 'Open the selected file in VCDAT and exit.')
        
        self.btn2_select = Tkinter.Button(
                    self.buttonFrame,
                    text='Open File',
                    command=gui_control.Command( self.evt_btn_select_file, None)
                   )
        self.btn2_select.pack(side=Tkinter.RIGHT, padx=5, pady=4)
        gui_parent.balloon.bind(self.btn2_select, 'Open the selected file in VCDAT.')
        
        self.buttonFrame.place(relx=0.0, rely=0.92, relwidth=1.0, relheight=0.08)

        self.evt_select_opendap_server( self.servers.keys()[0] )
        
        self.add_password_flg = 0 # Flag to determine if user needs to authenticate.

        #-------------------------------------------------------------------------
        # Decide where to put the window
        #-------------------------------------------------------------------------
        g = gui_parent.geometry()
        d = g.split('+')[1:]
        c = g.split('+')[0].split('x')
        e = int(d[0])-int(c[0])
        if e < 0: e=0
        self.dialog.geometry("%sx%s+%s+%s"% (c[0], str(int(int(c[1])*0.5)), str(e),d[1]))

        self.dialog.deiconify()

        if (transient == 1): self.dialog.dialog.transient(gui_parent)


    #-------------------------------------------------------------------------
    # Event handlers
    #-------------------------------------------------------------------------

    def evt_btn_cancel(self):
        self.dialog.withdraw()
    
    
    def evt_btn_select_file(self, exit):
        cur_server = self.ptls.get()
        cur_node = self.trb1.cursor_node()
        
        try:
            thredds_url = cur_node.parent_node.id
            thredds_xml = http_thredds.get(thredds_url)
            thredds_obj = http_thredds.parse(thredds_xml)
            opendap_url_base = [cur_service['base'] for cur_service in thredds_obj.services if cur_service['serviceType'] == 'DODS'][0]
            
            if (opendap_url_base[:7] == 'http://'):
            
                url = opendap_url_base + str(cur_node.id)
                
                if (self.auth_realm and self.auth_host):
                    username, password = self.passwordMgr.find_user_password(self.auth_realm, self.auth_host)
                    
                    if (username is not None and password is not None):
                        url = url.replace('http://', 'http://' + str(username) + ':' + str(password) + '@')
                
                if (exit == "exit"): self.dialog.withdraw()
                        
                self.gui_parent.panelSV.tin3.setentry(url)
                gui_select_variable.evt_enter_file(self.gui_parent, None)
                
            else:
                raise Exception('Unknown protocol for DODS/OPeNDAP access.')
        
        except Exception, e:
            gui_message.error(e)
    
    
    def evt_select_opendap_server(self, result):
        self.trb1.clear()
        
        self.auth_realm = None
        self.auth_host = None
        self.auth_url = None
        self.auth_callback = None
        self.auth_callback_handler = None
        
        if (self.gui_parent.auth_realm is not None):
            self.auth_realm = self.gui_parent.auth_realm
        if (self.gui_parent.auth_host is not None):
            self.auth_host = self.gui_parent.auth_host
        
        try:
            thredds_url = self.servers[result]['thredds_url']
            thredds_xml = http_thredds.get(thredds_url)
            thredds_obj = http_thredds.parse(thredds_xml)
            
            root_node = self.trb1.root
            root_node.id = thredds_url
            
            root_node.set_label(thredds_obj.dataset_name)
            self.trb1.move_cursor(self.trb1.root)
            
        except Exception, e:
            self.trb1.root.set_label('Select a THREDDS Catalog.')
            self.trb1.root.id = 0
            self.trb1.first()
            gui_message.error(e)
    
    
    def evt_expand_node(self, node):
        cur_server = self.ptls.get()
        if (cur_server != ''):
            try:
                thredds_url = node.id
                thredds_xml = http_thredds.get(thredds_url)
                thredds_obj = http_thredds.parse(thredds_xml)
                
                for cur_catalog_ref in thredds_obj.catalog_refs:
                    self.trb1.add_node(id=cur_catalog_ref['href'], name=cur_catalog_ref['title'], flag=True)
                    
                for cur_file in thredds_obj.files:
                    self.trb1.add_node(id=cur_file['url_suffix'], name=cur_file['name'], flag=False)
            except AttributeError:
                pass
            except Exception, e:
                gui_message.error(e)
    
    
    def evt_click_tree(self, event):
        cur_server = self.ptls.get()
        cur_node = self.trb1.cursor_node()
        
        if not cur_node.expandable():
            if (self.add_password_flg == 0):
               if ((self.gui_parent.username is not None) or (self.gui_parent.password is not None) or
                   (self.gui_parent.auth_realm is not None) or (self.gui_parent.auth_host is not None)): 
                    self.passwordMgr.add_password(self.gui_parent.auth_realm, self.gui_parent.auth_host, 
                                            self.gui_parent.username, self.gui_parent.password)
                    self.add_password_flg = 1

            try:
                # get url base from parent node thredds
                thredds_url = cur_node.parent_node.id
                
                thredds_xml = http_thredds.get(thredds_url)
                thredds_obj = http_thredds.parse(thredds_xml)
                
                opendap_url_base = [cur_service['base'] for cur_service in thredds_obj.services if cur_service['serviceType'] == 'DODS'][0]
                
                if (opendap_url_base[:7] == 'http://'):
                    opendap_url = opendap_url_base + str(cur_node.id)
                    opendap_das = http_opendap.get_das(opendap_url, self.passwordMgr, self)
                    if (opendap_das is not None):
                        self.handle_das(opendap_das)
                else:
                    raise Exception('Unknown protocol for DODS/OPeNDAP access.')
            
            except Exception, e:
                gui_message.error(e)
    
    
    def auth_prompt(self, realm, host, url, callback, callback_handler):
        
        self.gui_parent.auth_realm = self.auth_realm = realm
        self.gui_parent.auth_host = self.auth_host = host
        self.auth_url = url
        self.auth_callback = callback
        self.auth_callback_handler = callback_handler
        
        # Create the username/password dialog.
        self.auth_dialog = Pmw.Dialog(
            self.parent,
            buttons = ('OK', 'Cancel'),
            defaultbutton = 'OK',
            title = 'Authentication Required',
            command = self.handle_auth_prompt
            )
        
        self.auth_dialog.withdraw()
        
        self.txt_username = Pmw.EntryField(
                            self.auth_dialog.interior(),
                            labelpos = Tkinter.W,
                            label_text = 'Username:',
                            entry_background = 'white',
                            entry_foreground = 'black',
                            validate = None
                            )
        self.txt_username.pack(side=Tkinter.TOP, padx=5, pady=2)
        
        self.txt_password = Pmw.EntryField(
                            self.auth_dialog.interior(),
                            labelpos = Tkinter.W,
                            label_text = 'Password:',
                            entry_background = 'white',
                            entry_foreground = 'black',
                            validate = None,
                            entry_show = '*',
                            )
        self.txt_password.pack(side=Tkinter.TOP, padx=5, pady=2)
        
        self.auth_dialog.activate(geometry = 'centerscreenalways')
        
        
    def handle_auth_prompt(self, result):
        if (result is None or result == 'Cancel'):
            self.auth_dialog.deactivate(result)
        else:
            try:
                self.gui_parent.username = username = self.txt_username.get()
                self.gui_parent.password = password = self.txt_password.get()

                self.auth_dialog.deactivate(result)
                
                self.passwordMgr.add_password(self.auth_realm, self.auth_host, username, password)
                self.add_password_flg = 1
                
                output = self.auth_callback(self.auth_url, self.passwordMgr, self)
                if (output):
                    self.auth_callback_handler(output)
                    
            except Exception, e:
                gui_message.error(e)
    
    
    def handle_dds(self, dds):
        if (dds is not None):
            self.txt.settext(dds)
    
    
    def handle_das(self, das):
        if (das is not None):
            self.txt.settext(das)
    
