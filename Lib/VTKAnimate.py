# VTK Backend Animation Module
## Author:  Charles Doutriaux
import animate_helper
import vcs

class VTKAnimationCreate(animate_helper.StoppableThread):
  def __init__(self, controller):
    animate_helper.StoppableThread.__init__(self)
    self.controller = controller

  def run(self):
    self.describe()
    if self.controller.create_params.a_min is not None:
      gms = []
      for info in self.controller.animate_info:
         d = info[0]
         if not d.g_type in ["text",]:
            exec("gm = self.controller.vcs_self.create%s(source='%s')" % (d.g_type,d.g_name))
            if vcs.isboxfill(gm) and gm.boxfill_type!="custom":
              gm.level_1 = self.controller.create_params.a_min
              gm.level_2 = self.controller.create_params.a_max
            elif hasattr(gm,"levels"):
              levs = vcs.mkevenlevels(self.controller.create_params.a_min,self.controller.create_params.a_max)
              gm.levels=levs
              gm.fillareacolors=vcs.getcolors(levs)
            else: # probably 1D ?
              if gm.flip:
                gm.datawc_x1=self.controller.create_params.a_min
                gm.datawc_x2=self.controller.create_params.a_max
              else:
                gm.datawc_y1=self.controller.create_params.a_min
                gm.datawc_y2=self.controller.create_params.a_max
         else:
           gm = None
         gms.append(gm)

      #Ok we are stocking this for frame updating later
      self.controller.create_params.gms=gms
    self.animation_created = True

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

class VTKAnimationPlayback:
  pass
class VTKAnimate(animate_helper.AnimationController):
    def __init__(self,vcs_self):
        animate_helper.AnimationController.__init__(self,vcs_self)
        self.AnimationCreate = VTKAnimationCreate
        self.AnimationPlayback = VTKAnimationPlayback
    pass
