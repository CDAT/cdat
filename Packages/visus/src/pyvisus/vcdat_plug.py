import sys
from browser import  gui_control,gui_select_variable
import gui_support
import Tkinter 
import Pmw
import VisusColorMapSphere
import VisusCartesianSlice
import VisusFourSliceScene
import VisusMovieScene
import pyvisus

class create_visus_tools_menu:
   def __init__( self, main_menu, parent, tear_it ):
     self.parent = parent
     
     PT_name = 'Visus'
     if ((sys.platform == 'darwin') and (gui_control.do_aqua == 1)):
       PT_name = 'Visus  '
       
     main_menu.addmenu(PT_name, 'Visus', tearoff = tear_it)
     
     #
     # Create the cascade "Cartesian Views" menu and its items
     #main_menu.addcascademenu(PT_name, 'CartesianViews',
     #                         'Create Cartesian Views',
     #                         label = 'Cartesian Views',
     #                          traverseSpec = 'z', tearoff = tear_it
     #                         )
     
     main_menu.addmenuitem(PT_name, 'command', 'Create a window showing a cartesian view of the data',
                           label = 'Cartesian view',
                           command = gui_control.Command(self.createScene, VisusCartesianSlice.CartesianSlice)
                           )
     main_menu.addmenuitem(PT_name, 'command', 'Create a window showing a color mapped sphere potentially with an iso-surface',
                           label = 'Spherical view',
                           command = gui_control.Command(self.createScene, VisusColorMapSphere.ColorMapSphere)
                           )
     
     main_menu.addmenuitem(PT_name, 'command', 'Create a window showing four labeled slices',
                           label = 'Four Slices',
                           command = gui_control.Command(self.createScene, VisusFourSliceScene.FourSliceScene)
                           )
     
     main_menu.addmenuitem(PT_name, 'command', 'Create a window showing a spherical view with slices on the side',
                           label = 'Movie Scene',
                           command = gui_control.Command(self.createScene, VisusMovieScene.MovieScene)
                           )
     
     main_menu.addmenuitem(PT_name, 'command', 'Load and animate the demo scene',
                           label = 'Demo animation',
                           command = gui_control.Command(self.demoScene)
                           )
     
   def singleSlice(self,parent,*args):
     from browser import gui_wk
     print parent.panelDM.fid2
     print parent.panelDM.var3
     
     
     #print gui_wk.get_vars(parent)
      
   def infoRequest(self,scene):
      
     root = gui_support.root()
     
     self.dialog = Tkinter.Toplevel()
     Tkinter.Label(self.dialog,text=scene._interface_info).pack(padx=10,pady=10)
     Tkinter.Button(self.dialog,text="Cancel",command=self.dialog.destroy,padx=5,pady=5).pack(side="left",padx=10,pady=10)
     Tkinter.Button(self.dialog,text="OK",command=lambda x = scene: self.createScene(x),
                    padx=5,pady=5).pack(side="right",padx=10,pady=10)
     
     width = root.winfo_width()
     height = root.winfo_height()
     posx = root.winfo_rootx()
     posy = root.winfo_rooty()
     
     self.dialog.wm_geometry('+%d+%d' % (posx+width,posy))

   def createScene(self,scene):
     from browser import gui_wk

     print self.parent
     vars = gui_wk.get_vars(self.parent)


     if len(vars) < scene._min_variables or len(vars) > scene._max_variables:
       self.infoRequest(scene)
       return
     
     if len(vars) == 1:
       pyvisus.gVCDATScenes += [scene(vars[0])]
     elif len(vars) == 2:
       pyvisus.gVCDATScenes += [scene(vars[0],vars[1])]
     elif len(vars) == 3:
       pyvisus.gVCDATScenes += [scene(vars[0],vars[1],vars[2])]
     elif len(vars) == 4:
       pyvisus.gVCDATScenes += [scene(vars[0],vars[1],vars[2],vars[3])]

     try:
       self.dialog.destroy()
       self.dialog = None
     except:
       pass
  
      

   def demoScene(self):
     from sys import executable
     from os.path import split,join
     import cdms2
     
     sample_dir = join(split(executable)[0],"..","sample_data")
     
     mean = cdms2.open(join(sample_dir,"mean_1900_1_1900_12.nc"))['tas']
     std  = cdms2.open(join(sample_dir,"std_1900_1_1900_12.nc"))['tas']
     
     cloudiness = cdms2.open(join(sample_dir,"cl_A1.20C3M_1.CCSM.atmm.1900-01_cat_1909-12.nc"))['cl']
     
     mean = mean(longitude=(-180,180))
     std = std(longitude=(-180,180))
     
     mean.getTime().toRelativeTime(cloudiness.getTime().units)
     std.getTime().toRelativeTime(cloudiness.getTime().units)
     
     t = mean.getTime()
     t = [t[0],t[-1]]
     
     cloudiness = cloudiness(longitude=(-180,180),time=t)
     
     
     scene = VisusMovieScene.MovieScene(mean, std, cloudiness)
     
     pyvisus.gVCDATScenes += [scene]




if __name__ == "__main__":
  #global gRootNode

  from sys import executable
  from os.path import split,join
  import cdms2
  
  sample_dir = join(split(executable)[0],"..","sample_data")
  
  mean = cdms2.open(join(sample_dir,"mean_1900_1_1900_12.nc"))['tas']
  std  = cdms2.open(join(sample_dir,"std_1900_1_1900_12.nc"))['tas']
  
  cloudiness = cdms2.open(join(sample_dir,"cl_A1.20C3M_1.CCSM.atmm.1900-01_cat_1909-12.nc"))['cl']
  
  mean = mean(longitude=(-180,180))
  std = std(longitude=(-180,180))
  
  mean.getTime().toRelativeTime(cloudiness.getTime().units)
  std.getTime().toRelativeTime(cloudiness.getTime().units)
  

  t = cloudiness.getTime()[0:len(mean.getTime())]
  t = [t[0],t[-1]]
  
  cloudiness = cloudiness(longitude=(-180,180),time=t)

  cloudiness.setAxis(0,mean.getTime())
  
  scene = VisusMovieScene.MovieScene(mean, std, cloudiness)


