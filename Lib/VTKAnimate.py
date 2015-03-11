# VTK Backend Animation Module
## Author:  Charles Doutriaux
import animate_helper
import time
import random
import hashlib
import os
import glob
import vtk


def update_input(controller, update=True):
    ## Ok let's loop through the arrays and figure out the slice needed and update
    for i,info in enumerate(controller.vcs_self.animate_info):
      disp,slabs = info
      slab = slabs[0]
      if slab is None:
        continue # nothing to do
      #Ok we have a slab, let's figure which slice it is
      args=[]
      Ntot=1
      for a in slab.getAxisList()[:-controller._number_of_dims_used_for_plot][::-1]:
         n=controller.frame_num/Ntot % len(a)
         Ntot*=len(a)
         args.append(slice(n,n+1))
      args=args[::-1]
      if slabs[1] is None:
          controller.vcs_self.backend.update_input(disp.backend,slab(*args),update=update)
      else:
          controller.vcs_self.backend.update_input(disp.backend,slab(*args),slabs[1](*args),update=update)

class VTKAnimationCreate(animate_helper.StoppableThread):
  def __init__(self, controller):
    animate_helper.StoppableThread.__init__(self)

    self.controller = controller

    # this all happens in the init function because in interaction mode, run basically never happens
    self.offscreen_window = vtk.vtkRenderWindow()
    self.offscreen_window.SetOffScreenRendering(1)
    self.controller._unique_prefix=hashlib.sha1(time.asctime()+str(random.randint(0,10000))).hexdigest()
    self.bg_ren = vtk.vtkRenderer()
    self.offscreen_window.SetNumberOfLayers(2)
    self.bg_ren.SetLayer(0)
    self.offscreen_window.AddRenderer(self.bg_ren)
    self.controller.animation_created = True

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

    return png_name

  def draw_frame(self, frame_num, png_name):
    """
    Draw the specified frame on the offscreen window, render to png_name, add to controller's animation_files
    """
    update_input(self.controller, update=False)

    self.offscreen_window.Render()

    imgfiltr = vtk.vtkWindowToImageFilter()
    imgfiltr.SetInput(self.offscreen_window)
    imgfiltr.Update()
    writer = vtk.vtkPNGWriter()
    writer.SetInputConnection(imgfiltr.GetOutputPort())
    writer.SetFileName(png_name)
    writer.Write()
    self.controller.animation_files = sorted(glob.glob(os.path.join(os.path.dirname(png_name),"*.png")))

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

  def run(self):
      self.controller.frame_num = 0
      if self.controller.signals is not None:
          self.controller.signals.stopped.emit(False)
      self.controller.playback_running = True
      while not self.is_stopped():
          self.wait_if_paused()
          self.controller.draw_frame(allow_static = False, main_window_png = True)

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
        import atexit
        atexit.register(self.close)

    def extract_renderers(self):
        """
        Pulls all non-background renderers from the main window, and moves them
        to the offscreen window in the creation thread.
        """
        if self.cleared:
            return
        self.cleared = True
        be = self.vcs_self.backend
        if be.renWin is None: #Nothing to clear
            return
        renderers = be.renWin.GetRenderers()
        renderers.InitTraversal()
        ren = renderers.GetNextItem()

        be.hideGUI()
        self.create_thread.offscreen_window.SetNumberOfLayers(be.renWin.GetNumberOfLayers())
        while ren is not None:
            if not ren.GetLayer() == 0:
                be.renWin.RemoveRenderer(ren)
                self.create_thread.offscreen_window.AddRenderer(ren)
            else:
                self.create_thread.bg_ren.SetBackground(*ren.GetBackground())
            ren = renderers.GetNextItem()
        # We don't want to render yet, because we are going to put a PNG on the screen first.
        be.showGUI(render=False)


        self.create_thread.offscreen_window.SetSize(*be.renWin.GetSize())

    def reclaim_renderers(self):
        """
        Returns all renderers from the offscreen window to the main window.
        """
        if not self.cleared:
            return
        self.cleared = False

        be = self.vcs_self.backend

        if be.renWin is None: #Nothing to clear
            return

        be.hideGUI()
        renderers = be.renWin.GetRenderers()
        renderers.InitTraversal()
        ren = renderers.GetNextItem()
        while ren is not None:
            if ren.GetLayer() != 0:
                be.renWin.RemoveRenderer(ren)
            ren = renderers.GetNextItem()
        renderers = self.create_thread.offscreen_window.GetRenderers()
        renderers.InitTraversal()
        ren = renderers.GetNextItem()
        while ren is not None:
            if not ren.GetLayer() == 0:
                self.create_thread.offscreen_window.RemoveRenderer(ren)
                be.renWin.AddRenderer(ren)
            ren = renderers.GetNextItem()
        be.showGUI()
        be.renWin.Render()

    def draw_frame(self, frame_num = None, allow_static=True, main_window_png=False):
      if frame_num is None:
        frame_num = self.frame_num
      else:
        self.frame_num = frame_num

      if allow_static:
        # Attempt to extract the renderers and place them onto the create thread
        self.extract_renderers()

        # Retrieve the frame from the create thread and place it on the canvas
        self.vcs_self.put_png_on_canvas(
          self.create_thread.get_frame(self.frame_num),
          self.playback_params.zoom_factor,
          self.playback_params.vertical_factor,
          self.playback_params.horizontal_factor)
      else:
        # Attempt to reclaim the renderers from the create thread
        self.reclaim_renderers()

        update_input(self)

        self.vcs_self.backend.renWin.Render()

        if main_window_png:
            self.vcs_self.png(self.create_thread.get_frame_name(self.frame_num))

      if self.signals is not None:
        self.signals.drawn.emit(self.frame_num)
