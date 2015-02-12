# VTK Backend Animation Module
## Author:  Charles Doutriaux
import animate_helper
import vcs
import time
import random
import hashlib
import os

class VTKAnimationCreate(animate_helper.StoppableThread):
  def __init__(self, controller):
    animate_helper.StoppableThread.__init__(self)
    self.controller = controller

  def run(self):
    #self.describe()
    self.controller.animation_created = True
    self.controller._unique_prefix=hashlib.sha1(time.asctime()+str(random.randint(0,10000))).hexdigest()
    #print "NFRAMES:",self.controller.number_of_frames()

  def describe(self):
    for info in self.controller.animate_info:
      disp = info[0]
      print "BACKEND:",disp.backend
      print "TYPE:",disp.g_type
      print "Name:",disp.g_name
      if info[1][0] is not None:
        print "Array:",info[1][0].shape
      else:
        print "No Array"

class VTKAnimationPlayback(animate_helper.AnimationPlayback):
  def __init__(self, controller):
    animate_helper.AnimationPlayback.__init__(self,controller)
  pass

class VTKAnimate(animate_helper.AnimationController):
    def __init__(self,vcs_self):
        animate_helper.AnimationController.__init__(self,vcs_self)
        self.AnimationCreate = VTKAnimationCreate
        self.AnimationPlayback = VTKAnimationPlayback
        import atexit
        atexit.register(self.close)
    def draw_frame(self):
      #print "Drawing frame:",self.frame_num,self._unique_prefix
      png_name=os.path.join(os.environ["HOME"],".uvcdat",self._unique_prefix,"anim_%.10i" % self.frame_num)
      if os.path.exists(png_name) and self.playback_params.zoom_factor!=1:
        ## Ok we have the pngs and we need to zoom, need to use png
        ## maybe the zoom factor thing can be taken off, not sure what's faster
        self.vcs_self.put_png_on_canvas(
          png_name,
          self.playback_params.zoom_factor,
          self.playback_params.vertical_factor,
          self.playback_params.horizontal_factor)
      else: # Ok no pngs let's update the arrays and redraw
        ## Ok let's loop through the arrays and figure out the slice needed and update
        for i,info in enumerate(self.vcs_self.animate_info):
          disp,slabs = info
          slab = slabs[0]
          if slab is None:
            continue # nothing to do
          #Ok we have a slab, let's figure which slice it is
          args=[]
          N=1
          for a in slab.getAxisList()[:-self._number_of_dims_used_for_plot][::-1]:
             n=self.frame_num/N % len(a)
             N*=len(a)
             args.append(slice(n,n+1))
          args=args[::-1]
          #if self.frame_num  == 0:
            #print "NFrame <-> args: %i <-> %s" % (self.frame_num,args)
            #print "BE ANIM:",disp.backend
          if slabs[1] is None:
              self.vcs_self.backend.update_input(disp.backend,slab(*args),update=True)
          else:
              self.vcs_self.backend.update_input(disp.backend,slab(*args),slabs[1](*args),update=True)
        self.vcs_self.backend.renWin.Render()
        if not os.path.exists(png_name):
            if not os.path.exists(os.path.dirname(png_name)):
                os.makedirs(os.path.dirname(png_name))
            self.vcs_self.png(png_name)
      if self.signals is not None:
        self.signals.drawn.emit(self.frame_num)
