# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class TickMarksMenu {open : {public FLTKNodeMenu<pVisusTickMarks>}
} {
  decl {\#include "BorderAxisMenuWidget.h"} {}
  Function {TickMarksMenu(pVisusTickMarks node) : FLTKNodeMenu<pVisusTickMarks>(node)} {open
  } {
    code {createWindow();
mMenuWindow->show();} {}
  }
  Function {~TickMarksMenu()} {open attributes virtual
  } {
    code {//delete mMenuWindow;
//delete mTextEditor;} {}
  }
  Function {createWindow()} {open
  } {
    {fltk::Window} mMenuWindow {
      label {Tick Marks Menu} open
      tooltip {Set tick marks attributes in this window}
      xywh {784 343 350 400} resizable
      extra_code {\#include "FLTKNodeMenu.h"
\#include "VisusTickMarks.h"} visible
    } {
      {fltk::Group} mBorderAxisWidget {
        xywh {0 0 350 350} vertical labeltype EMBOSSED_LABEL textsize 11
      } {}
      {fltk::Button} {} {
        label Done
        callback {mMenuWindow->destroy();}
        xywh {104 365 140 27}
      }
    }
    code {mBorderAxisWidget->begin();} {}
    code {addSubWindow(new BorderAxisMenuWidget(mNode->mAxis,mMenuWindow));} {}
    code {mBorderAxisWidget->end();} {}
  }
  Function {isActive()} {open return_type bool
  } {
    code {return mMenuWindow->active();} {}
  }
} 
