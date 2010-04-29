# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class LabelNodeMenu {open : {public FLTKNodeMenu<pVisusLabelNode>}
} {
  decl {\#include "FontMenuWidget.h"} {}
  Function {createWindow()} {open
  } {
    {fltk::Window} mMenuWindow {
      label {Label Node Menu} open
      tooltip {Set label attributes in this window}
      xywh {598 341 250 280} resizable
      extra_code {\#include "FLTKNodeMenu.h"
\#include "VisusLabelNode.h"} visible
    } {
      {fltk::TextEditor} mTextEditor {
        label Label
        callback {mNode->text(((fltk::TextEditor*)o)->text());}
        tooltip {Text to be displayed by the node}
        xywh {35 10 200 18} when CHANGED box ENGRAVED_BOX
      }
      {fltk::Button} {} {
        label Done
        callback {mMenuWindow->destroy();}
        xywh {70 247 100 28}
      }
      {fltk::Group} mFontWidget {
        label {Text Attributes} open
        xywh {0 46 250 200} resizable labeltype EMBOSSED_LABEL textsize 11
      } {}
    }
    code {mTextEditor->text(mNode->text());} {}
    code {mFontWidget->begin();} {}
    code {addSubWindow(new FontMenuWidget(mNode,mMenuWindow));} {}
    code {mFontWidget->end();} {}
  }
  Function {LabelNodeMenu(pVisusLabelNode label) : FLTKNodeMenu<pVisusLabelNode>(label)} {open
  } {
    code {createWindow();
mMenuWindow->show();} {}
  }
  Function {~LabelNodeMenu()} {open attributes virtual
  } {
    code {//delete mMenuWindow;
//delete mTextEditor;} {}
  }
  Function {isActive()} {open return_type bool
  } {
    code {return mMenuWindow->active();} {}
  }
} 
