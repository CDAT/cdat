# VTK Backend Animation Module
## Author:  Charles Doutriaux
import animate_helper
import time
import random
import hashlib
import os
import shutil
import glob
import vcs


def update_input(canvas, dimensions, frame_num, update=True):
    ## Ok let's loop through the arrays and figure out the slice needed and update
    for i,info in enumerate(canvas.animate_info):
      disp,slabs = info
      slab = slabs[0]
      if slab is None:
          continue # nothing to do
      #Ok we have a slab, let's figure which slice it is
      args = []
      Ntot = 1
      for a in slab.getAxisList()[:-dimensions][::-1]:
          n = frame_num / Ntot % len(a)
          Ntot *= len(a)
          args.append(slice(n,n+1))
      args = args[::-1]
      if slabs[1] is None:
          canvas.backend.update_input(disp.backend, slab(*args), update=update)
      else:
          canvas.backend.update_input(disp.backend, slab(*args), slabs[1](*args), update=update)

class VTKAnimationCreate(animate_helper.StoppableThread):
  def __init__(self, controller):
    animate_helper.StoppableThread.__init__(self)
    self.controller = controller
    self.create_prefix()
    self.canvas = vcs.init()
    self.canvas.bgX, self.canvas.bgY = controller.vcs_self.backend.renWin.GetSize()
    # Animation resizing is broken right now; this will give us some buffer space to work with.
    self.canvas.bgX *= 2
    self.canvas.bgY *= 2
    self.controller.animation_created = True
    import atexit
    atexit.register(self.close)

  def create_prefix(self):
    self.controller._unique_prefix = hashlib.sha1(time.asctime()+str(random.randint(0,10000))).hexdigest()

  def run(self):
    pass

  def get_frame_name(self, frame_num):
    png_name = os.path.join(os.environ["HOME"],".uvcdat",self.controller._unique_prefix,"anim_%i.png" % frame_num)
    if not os.path.exists(os.path.dirname(png_name)):
        os.makedirs(os.path.dirname(png_name))
    return png_name

  def get_frame(self, frame_num):
    png_name = self.get_frame_name(frame_num)

    if not os.path.exists(png_name):
        self.draw_frame(frame_num, png_name)

    self.controller.animation_files = sorted(glob.glob(os.path.join(os.path.dirname(png_name),"*.png")))

    return png_name

  def draw_frame(self, frame_num, png_name):
    """
    Draw the specified frame on the offscreen canvas, render to png_name, add to controller's animation_files
    """
    update_input(self.canvas, self.controller._number_of_dims_used_for_plot, frame_num, update=False)

    self.canvas.png(png_name)


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
  def close(self):
      self.canvas.close()

class VTKAnimationPlayback(animate_helper.AnimationPlayback):
  def __init__(self, controller):
    animate_helper.AnimationPlayback.__init__(self,controller)

  def run(self):
      self.controller.frame_num = 0
      if self.controller.signals is not None:
          self.controller.signals.stopped.emit(False)
      self.controller.playback_running = True
      while not self.is_stopped():
          self.wait_if_paused()

          self.controller.draw_frame(allow_static = True, render_offscreen = False, main_window_png = True)

          self.controller.frame_num += 1
          if self.controller.frame_num >= self.controller.number_of_frames():
              if self.controller.playback_params.loop:
                  self.controller.frame_num = 0
              else:
                  break
          time.sleep(1./self.controller.playback_params.frames_per_second)
      self.controller.playback_running = False
      if self.controller.signals is not None:
          self.controller.signals.stopped.emit(True)

class VTKAnimate(animate_helper.AnimationController):
    def __init__(self,vcs_self):
        animate_helper.AnimationController.__init__(self,vcs_self)
        self.AnimationCreate = VTKAnimationCreate
        self.AnimationPlayback = VTKAnimationPlayback
        self.cleared = False
        self.renderers = []
        self.last_size = None
        self.modified_listener = None
        import atexit
        atexit.register(self.close)

    def modified(self, obj, event):
        # Use this to sync canvas sizes and to prevent configureEvent from blowing things up
        new_size = obj.GetSize()
        if self.last_size == new_size:
            return

        self.last_size = new_size
        # Resizing a background window doesn't work great right now- see https://github.com/UV-CDAT/uvcdat/issues/1148
        #self.create_thread.canvas.backend.renWin.SetSize(new_size)
        self.create_thread.canvas.backend.configureEvent(self.create_thread.canvas, "ModifiedEvent")

        if self.renderers is None or len(self.renderers) > 0:
            # If we're displaying a PNG, we'll skip the configureEvent action on vcs_self's backend so it doesn't break anything
            self.vcs_self.backend._lastSize = new_size
            # All of the images are now the wrong size; need to blow them all away.
            if self.animation_files:
                shutil.rmtree(os.path.dirname(self.animation_files[0]))
                self.animation_files = []
            # We'll use None as a sentinel value to tell us to replot in retrieve_renderers
            self.renderers = None

    def plot_to_canvas(self, canvas, displays, **kargs):
        # Store in case clear is nuking the canvas that holds the displays
        real_displays = [vcs.elements['display'][d] for d in displays]
        canvas.clear()
        for d in real_displays:
            parg = []
            for a in d.array:
                if a is not None:
                    parg.append(a)
            parg.append(d._template_origin)
            parg.append(d.g_type)
            parg.append(d.g_name)
            karg = {}
            karg.update(kargs)
            if d.ratio is not None:
                karg.update({"ratio":d.ratio})

            canvas.plot(*parg, **karg)

    def extract_renderers(self):
        """
        Pulls all non-background renderers from the main window
        and stores them in a list to re-add after animation stops.
        """
        if self.cleared:
            return
        self.cleared = True
        if self.modified_listener is None:
          self.modified_listener = self.vcs_self.backend.renWin.AddObserver("ModifiedEvent", self.modified, 25)

        be = self.vcs_self.backend

        if be.renWin is None:
            return

        be.hideGUI()

        self.plot_to_canvas(self.create_thread.canvas, self.vcs_self.display_names, bg=1)

        renderers = be.renWin.GetRenderers()
        renderers.InitTraversal()
        ren = renderers.GetNextItem()
        while ren is not None:
            if not ren.GetLayer() == 0:
                be.renWin.RemoveRenderer(ren)
                self.renderers.append(ren)
            ren = renderers.GetNextItem()

        # We don't want to render yet, because we are going to put a PNG on the screen first.
        be.showGUI(render=False)

    def reclaim_renderers(self):
        """
        Returns all renderers to the main window.
        """
        if not self.cleared:
            return
        if self.modified_listener is not None:
          self.vcs_self.backend.renWin.RemoveObserver(self.modified_listener)
          self.modified_listener = None
        self.cleared = False

        be = self.vcs_self.backend

        if be.renWin is None:
            return

        be.hideGUI()

        renderers = be.renWin.GetRenderers()
        renderers.InitTraversal()
        ren = renderers.GetNextItem()
        while ren is not None:
            if ren.GetLayer() != 0:
                be.renWin.RemoveRenderer(ren)
            ren = renderers.GetNextItem()

        if self.renderers is not None:
            for ren in self.renderers:
                be.renWin.AddRenderer(ren)
        else:
            self.plot_to_canvas(self.vcs_self, self.vcs_self.display_names)
        self.renderers = []
        be.showGUI()
        be.renWin.Render()

    def draw_frame(self, frame_num = None, render_offscreen=True, allow_static=True, main_window_png=False):
      """
      Draws a frame on the canvas
        frame_num: Which frame to draw– defaults to self.frame_num
        render_offscreen: Whether or not we allow rendering offscreen (when run in threads, there are issues)
        allow_static: Whether or not we allow the drawn frame to be a static image
        main_window_png: Whether or not to render the canvas into a PNG file to use later
      """

      if frame_num is None:
        frame_num = self.frame_num
      else:
        self.frame_num = frame_num

      if render_offscreen or (allow_static and len(self.animation_files) == self.number_of_frames()):
        # Attempt to extract the renderers and place them onto the create thread
        self.extract_renderers()
        # Retrieve the frame from the create thread and place it on the canvas
        self.vcs_self.put_png_on_canvas(
          self.create_thread.get_frame(self.frame_num),
          self.playback_params.zoom_factor,
          self.playback_params.vertical_factor,
          self.playback_params.horizontal_factor)
      else:
        self.reclaim_renderers()

        update_input(self.vcs_self, self._number_of_dims_used_for_plot, frame_num)

        self.vcs_self.backend.renWin.Render()

        if main_window_png:
            png_name = self.create_thread.get_frame_name(self.frame_num)
            self.vcs_self.png(png_name)
            self.animation_files = sorted(glob.glob(os.path.join(os.path.dirname(png_name),"*.png")))

      if self.signals is not None:
        self.signals.drawn.emit(self.frame_num)

    def stop(self):
        super(VTKAnimate, self).stop()
        self.reclaim_renderers()

    def reset(self):
        if self.create_thread:
            if self.animation_files:
                shutil.rmtree(os.path.dirname(self.animation_files[0]))
                self.animation_files = []
            self.create_thread.create_prefix()
            self.reclaim_renderers()

    def frame(self, frame):
        self.draw_frame(frame_num = frame, allow_static = False, render_offscreen = False)