# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class ColorMapMenu {open : {public FLTKNodeMenu<pVisusColorMap>}
} {
  decl {\#include "FontMenuWidget.h"} {}
  Function {createWindow()} {open
  } {
    {fltk::Window} mMenuWindow {
      label {Label Node Menu} selected
      tooltip {Set label attributes in this window}
      xywh {1041 165 267 185} vertical resizable
      extra_code {\#include "FLTKNodeMenu.h"
\#include "VisusColorMap.h"} visible
    } {
      {fltk::Button} {} {
        label Done
        callback {mMenuWindow->destroy();}
        xywh {79 150 107 30}
      }
      {fltk::Group} mFontWidget {
        xywh {0 50 267 97} resizable labeltype EMBOSSED_LABEL textsize 11
      } {
        {fltk::Choice} {} {
          label {Color Map:} open
          xywh {110 15 104 25}
        } {
          {fltk::Item} {} {
            label Banded
            }
          {fltk::Item} {} {
            label {Blue Green Red Yellow}
            }
          {fltk::Item} {} {
            label {Blue Red Yellow}
            }
          {fltk::Item} {} {
            label Gamma
            }
          {fltk::Item} {} {
            label Hot1
            }
          {fltk::Item} {} {
            label Hot2
            }
          {fltk::Item} {} {
            label Ice
            }
          {fltk::Item} {} {
            label {Light Hues}
            }
          {fltk::Item} {} {
            label {LUT 16}
            }
          {fltk::Item} {} {
            label Rich
            }
          {fltk::Item} {} {
            label {Smooth Rich}
            }
        }
      }
    }
    code {mTextEditor->text(mNode->text());} {}
    code {mFontWidget->begin();} {}
    code {addSubWindow(new FontMenuWidget(mNode,mMenuWindow));} {}
    code {mFontWidget->end();} {}
  }
  Function {ColorMapMenu(pVisusColorMap colormap) : FLTKNodeMenu<pVisusColorMap>(colormap)} {} {
    code {createWindow();
mMenuWindow->show();} {}
  }
  Function {~ColorMapMenu()} {attributes virtual
  } {
    code {//delete mMenuWindow;
//delete mTextEditor;} {}
  }
  Function {isActive()} {open return_type bool
  } {
    code {return mMenuWindow->active();} {}
  }
} 
