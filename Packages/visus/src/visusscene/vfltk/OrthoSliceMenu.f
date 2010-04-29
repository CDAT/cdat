# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class OrthoSliceMenu {open : {public FLTKNodeMenu<pVisusOrthogonalSlice>}
} {
  decl {\#include "ColorMapMenuWidget.h"} {}
  Function {createWindow()} {open
  } {
    {fltk::Window} mMenuWindow {
      label {Orthogonal Slice Menu} open
      tooltip {Set label attributes in this window}
      xywh {818 318 280 100} resizable
      extra_code {\#include "FLTKNodeMenu.h"
\#include "VisusOrthogonalSlice.h"} visible
    } {
      {fltk::Button} {} {
        label Done
        callback {mMenuWindow->destroy();}
        xywh {86 62 113 28}
      }
      {fltk::Group} mColorMapWidget {open
        xywh {120 5 150 50} labeltype EMBOSSED_LABEL textsize 11
      } {}
      {fltk::Choice} {} {
        label Orientation open
        xywh {5 25 100 25} align 5 resizable
      } {
        {fltk::Item} {} {
          label {XY-Plane}
          horizontal
        }
        {fltk::Item} {} {
          label {XZ-Plane}
          vertical
        }
        {fltk::Item} {} {
          label {YZ-Plane}
          }
      }
    }
    code {mColorMapWidget->begin();} {}
    code {addSubWindow(new ColorMapMenuWidget(mNode,mMenuWindow));} {}
    code {mColorMapWidget->end();} {}
  }
  Function {OrthoSliceMenu(pVisusOrthogonalSlice slice) : FLTKNodeMenu<pVisusOrthogonalSlice>(slice)} {open
  } {
    code {createWindow();
mMenuWindow->show();} {}
  }
  Function {~OrthoSliceMenu()} {open attributes virtual
  } {
    code {//delete mMenuWindow;
//delete mTextEditor;} {}
  }
  Function {isActive()} {open return_type bool
  } {
    code {return mMenuWindow->active();} {}
  }
} 
