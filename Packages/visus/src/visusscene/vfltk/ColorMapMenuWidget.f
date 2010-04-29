# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class ColorMapMenuWidget {open : {public SharedValueMenu<VisusColorMap>}
} {
  Function {ColorMapMenuWidget(pVisusGroup node,fltk::Window* parent=NULL) : SharedValueMenu<VisusColorMap>(node)} {open
  } {
    code {createMenu(parent);} {}
  }
  Function {ColorMapMenuWidget(VisusColorMap& map,fltk::Window* parent=NULL) : SharedValueMenu<VisusColorMap>(map)} {selected
  } {
    code {createMenu(parent);} {}
  }
  Function {~ColorMapMenuWidget()} {} {}
  Function {createMenu(fltk::Window* parent)} {open
  } {
    {fltk::Window} mMenu {
      xywh {630 291 130 50} resizable
      extra_code {\#include "vfltk/SharedValueMenu.h"
\#include "VisusColorMap.h"
\#include "VisusDefaultColorMaps.h"
\#include "vfltk/FLTKSubWindow.h"}
      class FLTKSubWindow non_modal visible
    } {
      {fltk::Choice} mStyle {
        label {Color Map} open
        xywh {14 15 101 25} align 1 labelfont 1
      } {
        {fltk::Item} {} {
          label Banded
          callback {value() = VisusDefaultColorMaps::banded();
commit();}
          }
        {fltk::Item} {} {
          label Greyscale
          callback {value() = VisusDefaultColorMaps::grey_scale();
commit();}
          }
        {fltk::Item} {} {
          label {Grey Ramp}
          callback {value() = VisusDefaultColorMaps::grey_ramp();
commit();}
          }
        {fltk::Item} {} {
          label {Blue Green Red Yellow}
          callback {value() = VisusDefaultColorMaps::bgry();
commit();}
          }
        {fltk::Item} {} {
          label {Blue Red Yellow}
          callback {value() = VisusDefaultColorMaps::bry();
commit();}
          }
        {fltk::Item} {} {
          label Gamma
          callback {value() = VisusDefaultColorMaps::gamma();
commit();}
          }
        {fltk::Item} {} {
          label Hot1
          callback {value() = VisusDefaultColorMaps::hot1();
commit();}
          }
        {fltk::Item} {} {
          label Hot2
          callback {value() = VisusDefaultColorMaps::hot2();
commit();}
          }
        {fltk::Item} {} {
          label Ice
          callback {value() = VisusDefaultColorMaps::ice();
commit();}
          }
        {fltk::Item} {} {
          label {Light Hues}
          callback {value() = VisusDefaultColorMaps::lighthues();
commit();}
          }
        {fltk::Item} {} {
          label {LUT 16}
          callback {value() = VisusDefaultColorMaps::lut16();
commit();}
          }
        {fltk::Item} {} {
          label Rich
          callback {value() = VisusDefaultColorMaps::rich();
commit();}
          }
        {fltk::Item} {} {
          label {Smooth Rich}
          callback {value() = VisusDefaultColorMaps::smooth_rich();
commit();}
          }
        {fltk::Item} {} {
          label Custom
          callback {value() = VisusDefaultColorMaps::banded();
commit();}
          }
      }
    }
    code {mMenu->show_inside(parent);} {}
  }
} 
