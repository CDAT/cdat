import vcs
import numpy
import os
import time
import thread

def showerror(msg):
  raise Exception,msg


#############################################################################
#                                                                           #
# Animate wrapper for VCS.                                                  #
#                                                                           #
#############################################################################
class animate_obj_old(object):
   """
 Function: animate

 Description of Function:
    Animate the contents of the VCS Canvas. The animation can also be controlled from
    the animation GUI. (See VCDAT for more details.)
 
    See the animation GUI documenation located at URL:
        http://www-pcmdi.llnl.gov/software/vcs

 Example of Use:
    a=vcs.init()
    a.plot(array,'default','isofill','quick')
    a.animate()

"""
    
   ##############################################################################
   # Initialize the animation flags						#
   ##############################################################################
   def __init__(self, vcs_self):
      self.vcs_self = vcs_self
      self.gui_popup = 0
      self.create_flg = 0
      self.run_flg = 0
      self.continents_value = 0
      self.continents_hold_value = 1
      
   ##############################################################################
   # Create the animation images. If min or max is None, then			#
   # the animator will find the min and max values from the dataset.		#
   # If min and max are set to 1e20, then no min and max animation		#
   # value is used (i.e., each animation frame will have different		#
   # min and max values. If min and max are set by the user, then		#
   # these values are used for the animation min and max.			#
   #										#
   # If you are running animation from a program, set thread_it to 0.		#
   # This will cause the Python program to wait for the create function		#
   # to finish before moving onto the next command line.			#
   ##############################################################################
   def create( self, parent=None, min=None, max=None, save_file=None, thread_it = 1, rate=None, bitrate=None, ffmpegoptions='' ):
      from vcs import minmax
      from numpy.ma import maximum,minimum
      ##from tkMessageBox import showerror

      # Cannot "Run" or "Create" an animation while already creating an animation
      if self.run_flg == 1: return
      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.vcs_self.animate_info == []:
         str = "No data found!"
         showerror( "Error Message to User", str )
         return
      finish_queued_X_server_requests( self.vcs_self )
      self.vcs_self.canvas.BLOCK_X_SERVER()

      # Stop the (thread) execution of the X main loop (if it is running).
      self.vcs_self.canvas.stopxmainloop( )

      # Force VCS to update its orientation, needed when the user changes the
      # VCS Canvas size.
      self.vcs_self.canvas.updateorientation()

      # Make sure the animate information is up-to-date for creating images
      if ((self.gui_popup == 1) and (self.create_flg == 0)):
         self.update_animate_display_list( )

      # Save the min and max values for the graphics methods.
      # Will need to restore values back when animation is done.
      self.save_original_min_max()

      # Set up the animation min and max values by changing the graphics method
      # Note: cannot set the min and max values if the default graphics method is set.
      do_min_max = 'yes'
      try:
         if (parent is not None) and (parent.iso_spacing == 'Log'):
            do_min_max = 'no'
      except:
         pass

      # Draw specified continental outlines if needed.
      self.continents_hold_value = self.vcs_self.canvas.getcontinentstype( )
      self.vcs_self.canvas.setcontinentstype( self.continents_value )

      if ( do_min_max == 'yes' ):
         minv = []
         maxv=[]
         if (min is None) or (max is None):
            for i in range(len(self.vcs_self.animate_info)):
               minv.append( 1.0e77 )
               maxv.append( -1.0e77 )
            for i in range(len(self.vcs_self.animate_info)):
               dpy, slab = self.vcs_self.animate_info[i]
               mins, maxs = minmax(slab)
               minv[i] = float(minimum(float(minv[i]), float(mins)))
               maxv[i] = float(maximum(float(maxv[i]), float(maxs)))
         if isinstance(min,list) or isinstance(max,list):
            for i in range(len(self.vcs_self.animate_info)):
               try:
                  minv.append( min[i] )
               except:
                  minv.append( min[-1] )
               try:
                  maxv.append( max[i] )
               except:
                  maxv.append( max[-1] )
         else:
            for i in range(len(self.vcs_self.animate_info)):
                minv.append( min )
                maxv.append( max )

         # Set the min an max for each plot in the page. If the same graphics method is used
         # to display the plots, then the last min and max setting of the data set will be used.
         for i in range(len(self.vcs_self.animate_info)):
            try:
               self.set_animation_min_max( minv[i], maxv[i], i )
            except Exception,err:
               pass # if it is default, then you cannot set the min and max, so pass.

      if save_file is None or save_file.split('.')[-1].lower()=='ras':
          if thread_it == 1:
              thread.start_new_thread( self.vcs_self.canvas.animate_init, (save_file,) )
              self.mythread=QAnimThread(None,self.vcs_self.canvas.animate_init,save_file)
              self.mythread.start()
          else:
              self.vcs_self.canvas.animate_init( save_file )
      else: # ffmpeg stuff
          save_info = self.vcs_self.animate_info
          animation_info = self.animate_info_from_python()
          slabs=[]
          templates=[]
          dpys=[]
          for i in range(len(self.vcs_self.animate_info)):
              dpy, slab = self.vcs_self.animate_info[i]
              slabs.append(slab)
              dpys.append(dpy)
              templates.append(dpy.template)
          sh =slabs[0].shape
          if dpy.g_type in ['boxfill', 'isofill', 'isoline', 'meshfill', 'outfill', 'outline', 'taylordiagram', 'vector', ]:
              r=len(sh)-2
          else:
              r=len(sh)-1
          # now create the list of all previous indices to plot
          indices=[]
          for i in range(r):
              this = list(range(sh[i]))
              tmp=[]
              if indices == []:
                  for k in this:
                      indices.append([k,])
              else:
                  for j in range(len(indices)):
                      for k in this:
                          tmp2=copy.copy(indices[j])
                          tmp2.append(k)
                          tmp.append(tmp2)
                  indices=tmp
          count=1
          white_square=self.vcs_self.createfillarea()
          white_square.color=240
          white_square.x=[0,1,1,0]
          white_square.y=[0,0,1,1]
          new_vcs=self.vcs_self
          if self.vcs_self.orientation()=='portrait':
              new_vcs.portrait()
          #self.vcs_self.close()

          d = Pmw.Dialog(title="Creating Frames")
          d.geometry("200x150+0+0")
          S=genutil.Statusbar(d.interior(),ycounter=50)
          S.pack(expand=1,fill='both')
          n=float(len(indices))/100.
          for index in indices:
              S.show(count/n)
              new_vcs.clear()
              new_vcs.plot(white_square,bg=1)
              for i in range(len(save_info)):
                  slab=slabs[i]
                  template=templates[i]
                  gtype = animation_info["gtype"][i].lower()
                  gname = animation_info["gname"][i]
                  exec("gm = new_vcs.get%s('%s')" % (gtype,gname))
                  for j in index:
                      slab=slab[j]
                  new_vcs.plot(slab,gm,new_vcs.gettemplate(template),bg=1)
              new_vcs.png("tmp_anim_%i" % count)
              count+=1
          new_vcs.ffmpeg(save_file,"tmp_anim_%d.png",bitrate=bitrate,rate=rate,options=ffmpegoptions)
          for i in range(count-1):
              os.remove("tmp_anim_%i.png" % (i+1))
          d.destroy()
          del(new_vcs)
      self.create_flg = 1

      self.vcs_self.canvas.UNBLOCK_X_SERVER()

   def animate_info_from_python(self):
       gtype = []
       gname = []
       tmpl = []
       for i in self.vcs_self.animate_info:
            d=i[0]
            tmpl.append(d.template)
            gtype.append(d.g_type)
            gname.append(d.g_name)
       return {"template":tmpl,"gtype":gtype,"gname":gname}

   ##############################################################################
   # Save original min and max values    					#
   ##############################################################################
   def save_original_min_max( self ):
      animation_info = self.animate_info_from_python()
      self.save_min = {}
      self.save_max = {}
      self.save_legend = {}
      self.save_levels = {}
      self.save_mean_veloc = {}
      for i in range(len(self.vcs_self.animate_info)):
         gtype = animation_info["gtype"][i].lower()
         if gtype == "boxfill":
            gm=self.vcs_self.getboxfill(animation_info['gname'][i])
            self.save_min[i] = gm.level_1
            self.save_max[i] = gm.level_2
#            self.save_legend[i] = gm.legend
         elif ( gtype == "meshfill" ):
            gm=self.vcs_self.getmeshfill(animation_info['gname'][i])
            self.save_levels[i] = gm.levels
         elif ( gtype == "isofill" ):
            gm=self.vcs_self.getisofill(animation_info['gname'][i])
            self.save_levels[i] = gm.levels
         elif ( gtype == "isoline" ):
            gm=self.vcs_self.getisoline(animation_info['gname'][i])
            self.save_levels[i] = gm.levels
         elif ( gtype == "yxvsx" ):
            gm=self.vcs_self.getyxvsx(animation_info['gname'][i])
            self.save_min[i] = gm.datawc_y1
            self.save_max[i] = gm.datawc_y2
         elif ( gtype == "xyvsy" ):
            gm=self.vcs_self.getxyvsy(animation_info['gname'][i])
            self.save_min[i] = gm.datawc_x1
            self.save_max[i] = gm.datawc_x2
         elif ( gtype == "vector" ):
            gm=self.vcs_self.getvector(animation_info['gname'][i])
            self.save_mean_veloc[i] = gm.reference

   ##############################################################################
   # Restore min and max values                                                 #
   ##############################################################################
   def restore_min_max( self ):
      animation_info = self.animate_info_from_python()
      try:
       for i in range(len(self.vcs_self.animate_info)):
         gtype = animation_info["gtype"][i].lower()
         if gtype == "boxfill":
            gm=self.vcs_self.getboxfill(animation_info['gname'][i])
            gm.level_1 = self.save_min[i]
            gm.level_2 = self.save_max[i]
#            gm.legend = self.save_legend[i]
         elif ( gtype == "meshfill" ):
            gm=self.vcs_self.getmeshfill(animation_info['gname'][i])
            gm.levels = self.save_levels[i]
         elif ( gtype == "isofill" ):
            gm=self.vcs_self.getisofill(animation_info['gname'][i])
            gm.levels = self.save_levels[i]
         elif ( gtype == "isoline" ):
            gm=self.vcs_self.getisoline(animation_info['gname'][i])
            gm.levels = self.save_levels[i]
         elif ( gtype == "yxvsx" ):
            gm=self.vcs_self.getyxvsx(animation_info['gname'][i])
            gm.datawc_y1 = self.save_min[i]
            gm.datawc_y2 = self.save_max[i]
         elif ( gtype == "xyvsy" ):
            gm=self.vcs_self.getxyvsy(animation_info['gname'][i])
            gm.datawc_x1 = self.save_min[i]
            gm.datawc_x2 = self.save_max[i]
         elif ( gtype == "vector" ):
            gm=self.vcs_self.getvector(animation_info['gname'][i])
            gm.reference = self.save_mean_veloc[i]
      except:
          pass
   
   ##############################################################################
   # Set the animation min and max values    					#
   ##############################################################################
   def set_animation_min_max( self, min, max, i ):
      from vcs import mkscale, mklabels, getcolors
      animation_info = self.animate_info_from_python()
      gtype = animation_info["gtype"][i].lower()
      levs = mkscale(min,max)
      dic = mklabels(levs)
      cols = getcolors(levs)
      if gtype == "boxfill":
         gm=self.vcs_self.getboxfill(animation_info['gname'][i])
         if gm.boxfill_type == 'custom':
             gm.fillareacolors = cols
             gm.levels = levs
         else:
             gm.level_1=levs[0]
             gm.level_2=levs[-1]
             gm.legend=None
      elif ( gtype == "meshfill" ):
         gm=self.vcs_self.getmeshfill(animation_info['gname'][i])
         if (min == 1e20) and (max ==1e20):
            gm.levels=(1e20,1e20)
         else:
            gm.levels = levs
            gm.fillareacolors = cols
      elif ( gtype == "isofill" ):
         gm=self.vcs_self.getisofill(animation_info['gname'][i])
         if (min == 1e20) and (max ==1e20):
            gm.levels=(1e20,1e20)
         else:
            gm.levels = levs
            gm.fillareacolors = cols
      elif ( gtype == "isoline" ):
         gm=self.vcs_self.getisoline(animation_info['gname'][i])
         if (min == 1e20) and (max ==1e20):
            gm.levels=(1e20,1e20)
         else:
            gm.levels = levs
            gm.fillareacolors = cols
      elif ( gtype == "yxvsx" ):
         gm=self.vcs_self.getyxvsx(animation_info['gname'][i])
         if (min != 1e20) and (max !=1e20):
            gm.yticlabels1=dic
            gm.yticlabels2=dic
            min = levs[0]
            max = levs[-1]
         gm.datawc_y1 = min
         gm.datawc_y2 = max
      elif ( gtype == "xyvsy" ):
         gm=self.vcs_self.getxyvsy(animation_info['gname'][i])
         if (min != 1e20) and (max !=1e20):
            gm.xticlabels1=dic
            gm.xticlabels2=dic
            min = levs[0]
            max = levs[-1]
         gm.datawc_x1 = min
         gm.datawc_x2 = max
      elif ( gtype == "vector" ):
         gm=self.vcs_self.getvector(animation_info['gname'][i])
         mean_veloc = 1e20
         if (min != 1e20) and (max !=1e20):
            mean_veloc = float( int( numpy.sqrt( (min**2)+(max**2) ) ) )
         gm.reference = mean_veloc
      animation_info['gname'][i] = gm.name

   ##############################################################################
   # Return the animation min and max values                                    #
   ##############################################################################
   def return_animation_min_max( self ):
      dpy, slab = self.vcs_self.animate_info[0]
      return vcs.minmax(slab)

   ##############################################################################
   # Load animation from a stored Raster file.   				#
   ##############################################################################
   def load_from_file( self, parent=None, load_file=None, thread_it = 1 ):
      ##from tkMessageBox import showerror
      if os.access(load_file, os.R_OK) == 0:
         showerror( "Error Message to the User", "The specfied file does not have read permission or does not exist. Please check the availability of the file.")
         return

      finish_queued_X_server_requests( self.vcs_self )
      self.vcs_self.canvas.BLOCK_X_SERVER()

      # Stop the (thread) execution of the X main loop (if it is running).
      self.vcs_self.canvas.stopxmainloop( )

      if thread_it == 1:
          thread.start_new_thread( self.vcs_self.canvas.animate_load, (load_file,) )
      else:
          self.vcs_self.canvas.animate_init( load_file )
      self.create_flg = 1

      self.vcs_self.canvas.UNBLOCK_X_SERVER()

   ##############################################################################
   # Creating animation flag                 					#
   ##############################################################################
   def creating_animation_flg( self ):
      return self.vcs_self.canvas.creating_animation()

   ##############################################################################
   # Run animation flag                 					#
   ##############################################################################
   def run_animation_flg( self ):
      return self.run_flg

   ##############################################################################
   # Run or start the animation              					#
   ##############################################################################
   def run( self ):
      # Cannot "Create" an animation while running an animation.
      if self.vcs_self.canvas.creating_animation() == 1: return

      if ((self.create_flg == 1) and (self.run_flg == 0)):
         self.run_flg = 1
         #thread.start_new_thread( self.vcs_self.canvas.animate_run,( ) )
         print self.vcs_self.canvas.animate_run
         self.vcs_self.canvas.animate_run()

   ##############################################################################
   # Stop the animation creation                                                #
   ##############################################################################
   def stop_create( self ):
      if (self.create_flg == 1):
         self.vcs_self.canvas.animate_stop_create()

   ##############################################################################
   # Stop the animation                                 			#
   ##############################################################################
   def stop( self ):
      if (self.create_flg == 1) and (self.run_flg == 1):
         self.run_flg = 0
         self.vcs_self.canvas.animate_stop()
      elif (self.create_flg == 1):
         self.vcs_self.canvas.animate_stop_create()
	
   ##############################################################################
   # View the specified animation frame                          		#
   ##############################################################################
   def frame( self, value=1 ):
      if (self.create_flg == 1) and (self.run_flg == 0):
         self.vcs_self.canvas.animate_frame( value )

   ##############################################################################
   # Return the number of animate frames                                    	#
   ##############################################################################
   def number_of_frames( self ):
      if self.create_flg == 1:
         return self.vcs_self.canvas.animate_number_of_frames( )

   ##############################################################################
   # Pause the animation loop                                               	#
   # Value ranges from 0 to 100                                                 #
   ##############################################################################
   def pause( self, value=1 ):
      if (((not isinstance(value, int))) or (value not in range(0, 101))):
         raise vcsError, "Pause value must be between an integer between 0 and 100."

      if (self.create_flg == 1) and (self.run_flg == 1):
         self.vcs_self.canvas.animate_pause( value )

   ##############################################################################
   # Zoom in on the animation                                               	#
   # Value ranges from 0 to 20                                                  #
   ##############################################################################
   def zoom( self, value=1 ):
      if (((not isinstance(value, int))) or (value not in range(1, 21))):
         raise vcsError, "Zoom value must be between an integer between 1 and 20."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_zoom( value )

   ##############################################################################
   # Pan the zoomed animation or frame in the x (or horizontal) direction   	#
   # Value ranges from -100 to 100						#
   ##############################################################################
   def horizontal( self, value=0 ):
      if (((not isinstance(value, int))) or (value not in range(-100, 101))):
         raise vcsError, "Horizontal pan value must be between an integer between -100 and 100."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_horizontal( value )

   ##############################################################################
   # Pan the zoomed animation or frame in the y (or vertical) direction   	#
   # Value ranges from -100 to 100						#
   ##############################################################################
   def vertical( self, value=0 ):
      if (((not isinstance(value, int))) or (value not in range(-100, 101))):
         raise vcsError, "Vertical pan value must be between an integer between -100 and 100."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_vertical( value )

   ##############################################################################
   # Set the direction of the animation:                                        #
   # Value 1 -> forward, 2 -> backward       	                                #
   ##############################################################################
   def direction( self, value=1 ):
      if (((not isinstance(value, int))) or (value not in range(1, 3))):
         raise vcsError, "Direction value must be between either 1='forward' or 2='backward'."

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_direction( value )

   ##############################################################################
   # Mode sets the cycle, forth and back, or animate once                   	#
   # Value: 1 -> cycle, 2 -> animate once, and 3 -> forth and back              #
   ##############################################################################
   def mode( self, value=1 ):
      if (((not isinstance(value, int))) or (value not in [1, 3])):
         raise vcsError, "Mode value must be between either 1 or 3."

      if value == 2:
         self.run_flg = 0

      if self.vcs_self.canvas.creating_animation() == 1: return

      if self.create_flg == 1:
         self.vcs_self.canvas.animate_mode( value )

   ##############################################################################
   # Update the animation display list                                      	#
   ##############################################################################
   def update_animate_display_list( self ):
        current_display_list = self.vcs_self.return_display_names()
         
        temp_list = []
        for i in range(len(self.vcs_self.animate_info)):
           if self.vcs_self.animate_info[i][0].name in current_display_list:
              temp_list.append( (self.vcs_self.animate_info[i][0],
                                self.vcs_self.animate_info[i][1]) )
        self.vcs_self.animate_info = temp_list

   ##############################################################################
   # Close the animate session                                              	#
   ##############################################################################
   def close( self ):
      if self.create_flg == 1:
         self.vcs_self.canvas.animate_close()
         self.gui_popup = 0
         self.create_flg = 0
         self.run_flg = 0
         self.vcs_self.canvas.getcontinentstype( self.continents_hold_value )
         self.continents_value = 0
         self.continents_hold_value = 1
      self.vcs_self.animate_info = []

      # Now that the animation is completed, restore the graphics methods min and max values.
      self.restore_min_max()

   ##############################################################################
   # Pop up the animation GUI                                              	#
   ##############################################################################
   def gui( self, gui_parent=None, transient=0):
      if self.gui_popup == 0:
         self.gui_popup = 1
         a = _animationgui.create(self, gui_parent, transient)
         return a

class RT:
  def __init__(self,nextFunc,parent):
    self.next = nextFunc
    self.running = True
    self.parent = parent
  def start(self):
    self.runnnig= True
    while self.running:
      self.next()
      time.sleep(1./self.parent.frames_per_second)
  def stop(self):
    self.running = False

class animate_obj(animate_obj_old):

    def __init__(self, vcs_self):
        animate_obj_old.__init__(self,vcs_self)
        self.zoom_factor = 1.
        self.vertical_factor = 0
        self.horizontal_factor = 0
        self.allArgs = []
        self.canvas = None
        self.animation_seed = None
        self.animation_files = []
        self.frames_per_second = 100.
        self.creating_animation = False
        self.fps(100) #sets runTimer interval
        self.runTimer = RT(self.next,self)
        self.current_frame = 0
        self.loop = True


    def create( self, parent=None, min=None, max=None, save_file=None, thread_it = 1, rate=5., bitrate=None, ffmpegoptions='', axis=0):
        if self.canvas is None:
          #self.canvas = vcs.init()
          self.canvas = self.vcs_self
        self.current_frame = 0
        self.creating_animation = True
        if thread_it:
          self.thread = thread.start_new_thread(self._actualCreate,
                (parent,min,max,save_file,rate,bitrate,ffmpegoptions,axis)
                )
        else:
          self.thread = None
          self._actualCreate(parent,min,max,save_file,rate,bitrate,ffmpegoptions,axis)
        return 

    def _actualCreate( self, parent=None, min=None, max=None, save_file=None, rate=5., bitrate=None, ffmpegoptions='', axis=0, sender=None):
        alen = None
        dims = self.vcs_self.canvasinfo()
        if dims['height']<500:
            factor = 2
        else:
            factor=1
        if dims["width"]<dims["height"]:
            self.canvas.portrait(width=dims["width"],height=dims["height"])
        self.canvas.setbgoutputdimensions(width = dims['width']*factor,height=dims['height']*factor,units='pixel')
        truncated = False
        vcs_ai = list(self.vcs_self.animate_info)
        self.vcs_self.clear()
        for I in vcs_ai:
            if alen is None:
                alen = I[1][0].shape[axis]
            else:
                l = I[1][0].shape[axis]
                if l!=alen:
                    alen = numpy.minimum(alen,l)
                    truncated = True
        if truncated:
            warnings.warn("Because of inconsistent shapes over axis: %i, the animation length will be truncated to: %i\n" % (axis,alen))
        if self.animation_seed is not None:
            if self.animation_files != []:
                for fnm in self.animation_files:
                    os.remove(fnm)
        self.animation_seed = None
        self.animation_files = []
        # Save the min and max values for the graphics methods.
        # Will need to restore values back when animation is done.
        self.save_original_min_max()
        # Note: cannot set the min and max values if the default graphics method is set.
        do_min_max = 'yes'
        try:
           if (parent is not None) and (parent.iso_spacing == 'Log'):
              do_min_max = 'no'
        except:
           pass
        if ( do_min_max == 'yes' ):
             minv = []
             maxv=[]
             if (min is None) or (max is None):
                for i in range(len(self.vcs_self.animate_info)):
                   minv.append( 1.0e77 )
                   maxv.append( -1.0e77 )
                for i in range(len(self.vcs_self.animate_info)):
                   dpy, slab = self.vcs_self.animate_info[i]
                   mins, maxs = vcs.minmax(slab)
                   minv[i] = float(numpy.minimum(float(minv[i]), float(mins)))
                   maxv[i] = float(numpy.maximum(float(maxv[i]), float(maxs)))
             elif isinstance(min,list) or isinstance(max,list):
                for i in range(len(self.vcs_self.animate_info)):
                   try:
                      minv.append( min[i] )
                   except:
                      minv.append( min[-1] )
                   try:
                      maxv.append( max[i] )
                   except:
                      maxv.append( max[-1] )
             else:
                for i in range(len(self.vcs_self.animate_info)):
                    minv.append( min )
                    maxv.append( max )
             # Set the min an max for each plot in the page. If the same graphics method is used
             # to display the plots, then the last min and max setting of the data set will be used.
             for i in range(len(self.vcs_self.animate_info)):
                try:
                   self.set_animation_min_max( minv[i], maxv[i], i )
                except Exception,err:
                   pass # if it is default, then you cannot set the min and max, so pass.

        self.allArgs = []
        for i in range(alen):
            #y.clear()
            frameArgs = []
            for I in vcs_ai:
                d=I[0]
                kw={}
                n = len(I[1][0].shape)
                for j,id in enumerate(I[1][0].getAxisIds()):
                    if j!=axis and j<n-2:
                        kw[id]=slice(0,1)
                    elif j==axis:
                        kw[id]=slice(i,i+1)
                    else:
                        break
                args = [I[1][0](**kw),]
                if I[1][1] is not None:
                    kw={}
                    n = len(I[1][1].shape)
                    for j,id in enumerate(I[1][1].getAxisIds()):
                        if j!=axis and j<n-2:
                            kw[id]=slice(0,1)
                        elif j==axis:
                            kw[id]=slice(i,i+1)
                        else:
                            break
                    args.append(I[1][1](**kw))
                args += [d.template,d.g_type,d.g_name]
                #b=y.getboxfill(d.g_name)
                #y.plot(*args,bg=1)
                frameArgs.append(args)
            self.allArgs.append(frameArgs)

        if sender is None:
            for i in xrange(len(self.allArgs)):
                self.renderFrame(i)
            self.restore_min_max()
            self.animationCreated()
        else:
            sender.animationTimer.start(0, sender)
            sender.dialog.setRange(0, len(self.allArgs))
            sender.dialog.show()
        self.creating_animation = False

    def animationCreated(self):
        self.create_flg = 1
        self.restore_min_max()

    def animationCanceled(self):
        self.create_flg = 0
        self.restore_min_max()

    def renderFrame(self, i):
        if self.animation_seed is None:
            self.animation_seed = numpy.random.randint(10000000000)
        frameArgs = self.allArgs[i]
        fn = os.path.join(os.environ["HOME"],".uvcdat","__uvcdat_%i_%i.png" % (self.animation_seed,i))
        self.animation_files.append(fn)

        #BB: this clearing and replotting somehow fixes vcs internal state
        # and prevents segfaults when running multiple animations
        #self.vcs_self.replot()



        #self.canvas.clear()
        #self.vcs_self.plot(*frameArgs[0],bg=1)
        self.canvas.clear()
        for args in frameArgs:
            self.canvas.plot(*args, bg=0)
        self.canvas.png(fn,draw_white_background=1)
        #self.canvas.png("sample")
        
    # def runner(self):
    #     self.runit = True
    #     while self.runit:
    #         for fn in self.animation_files:
    #             if not self.runit:
    #                 self.run_flg = 0
    #                 break
    #             self.vcs_self.canvas.put_png_on_canvas(fn,self.zoom_factor,self.vertical_factor,self.horizontal_factor)
    #             import time
    #             time.sleep(self.pause_value)

    def next(self):
        """Draws next frame of animation
        """
        if self.create_flg == 1:
            if self.current_frame < len(self.animation_files)-1:
                self.current_frame += 1
            elif self.loop or self.first_run:
                self.current_frame = 0
            else:
                self.pause_run()
            self.draw(self.current_frame)
        else:
            self.pause_run()
        if self.first_run:
            self.first_run = False

    def run(self,*args,**kargs):
      """Runs the animation"""
      thread.start_new_thread(self._run,args,kargs)
      
    def _run(self,*args):
      """Runs the animation"""
      self.vcs_self.open()
      while self.create_flg == 0:
        pass
      if self.create_flg == 1 and self.run_flg == 0:
          self.first_run = True
          self.run_flg = 1
          self.runTimer.start()

    def pause_run(self):
      """ Pauses the animation """
      self.run_flg = 0
      self.runTimer.stop()

    def draw(self, frame):
      """Render a specific Frame"""
      if self.create_flg == 1:
          self.current_frame = frame
          self.vcs_self.backend.clear()
          self.vcs_self.put_png_on_canvas(self.animation_files[frame],
                  self.zoom_factor, self.vertical_factor, self.horizontal_factor)
        
    def frame(self, frame):
      """Render a specific Frame"""
      self.draw(frame)

    def save(self,movie,bitrate=1024, rate=None, options=''):
      """Save animation to a file"""
      if self.create_flg == 1:
          fnms = os.path.join(os.environ["HOME"],".uvcdat","__uvcdat_%i_%%d.png" %      (self.animation_seed))
          if rate is None:
              rate = self.fps()
          self.vcs_self.ffmpeg(movie, fnms, bitrate, rate, options)

    def number_of_frames(self):
      """Returns the number of frames"""
      return len(self.animation_files)

    def stop(self):
      """Stop animation when runnning"""
      self.pause_run()
      self.current_frame = 0

    def pause(self, value):
      """Time between frames when animating"""
      value = max(value, 0.0001)
      self.fps(1/value)

    def zoom(self,value):
      """Zoom factor for the animation"""
      self.zoom_factor = value

    def horizontal(self,value):
      """ Pan the window horizontaly (when zoomed). 100% means move so you can see the furthest right part of the picture"
      """
      if value>100.:
        raise Exception("Horizontal Factor cannot be greater than 100%")
      if value<-100.:
        raise Exception("Horizontal Factor cannot be less than 100%")
      self.horizontal_factor = value

    def vertical(self,value):
      """ Pan the window verticaly (when zoomed). 100% means move so you can see the top part of the picture"
      """
      if value>100.:
        raise Exception("Vertical Factor cannot be greater than 100%")
      if value<-100.:
        raise Exception("Vertical Factor cannot be less than 100%")
      self.vertical_factor = value

    def fps(self, value=None):
      """ Animation desired number of frame per seconds (might not be achievable depending on your system)"""
      if value is not None:
          value = max(value, 0.0001)
          self.frames_per_second = value
          return self
      return self.frames_per_second

      
############################################################################
#        END OF FILE                                                       #
############################################################################
