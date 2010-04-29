# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class FontMenuWidget {open : {public SharedValueMenu<VisusFont>}
} {
  Function {FontMenuWidget(pVisusGroup node,fltk::Window* parent=NULL) : SharedValueMenu<VisusFont>(node,parent)} {open
  } {
    code {createMenu(parent);} {}
  }
  Function {FontMenuWidget(VisusFont& font,fltk::Window* parent=NULL) : SharedValueMenu<VisusFont>(font,parent)} {open
  } {
    code {createMenu(parent);} {}
  }
  Function {~FontMenuWidget()} {open
  } {}
  Function {createMenu(fltk::Window* parent)} {open
  } {
    {fltk::Window} mMenu {open
      xywh {616 283 250 200} resizable
      extra_code {\#include "vfltk/SharedValueMenu.h"
\#include "VisusFont.h"
\#include "vfltk/FLTKSubWindow.h"}
      class FLTKSubWindow non_modal visible
    } {
      {fltk::Group} mBase {open
        xywh {5 0 240 200} resizable
      } {
        {fltk::FileBrowser} mFontFile {
          callback {value().fontFile(((fltk::FileBrowser*)o)->directory());
commit();}
          xywh {0 32 240 120} align 5 when CHANGED labelfont 1 labelsize 11 textsize 11
        }
        {fltk::Output} mFont {
          label Font
          callback {value().fontFile(((fltk::Input*)o)->text());
commit();}
          xywh {35 2 200 25} when ENTER_KEY_ALWAYS labelfont 1 textsize 11
          class {fltk::Input}
        }
        {fltk::ValueInput} mFontSize {
          label {Font Size}
          callback {value().fontSize(((fltk::ValueInput*)o)->value());
commit();}
          xywh {5 168 48 24} align 1 maximum 99 step 1
        }
        {fltk::Choice} mStyle {
          label {Font Style}
          callback {value().fontStyle((VISUS_FONT_STYLE)(mStyle->value()));
commit();} open
          xywh {75 168 72 24} align 5
        } {
          {fltk::Item} {} {
            label Bitmap
            }
          {fltk::Item} {} {
            label Pixmap
            }
          {fltk::Item} {} {
            label Outline
            }
          {fltk::Item} {} {
            label Polygon
            }
          {fltk::Item} {} {
            label Extrude
            }
          {fltk::Item} {} {
            label Texture
            }
        }
        {fltk::Button} {} {
          label {Font Color}
          callback {float r,g,b;
fltk::color_chooser("FontColor",r,g,b);
value().fontColor(255*r,255*g,255*b);
fprintf(stderr,"%f  %f  %f\\n",r,g,b);
commit();}
          xywh {160 167 65 25} align 32
          extra_code {\#include "fltk/ColorChooser.h"}
        }
      }
    }
    code {mFontFile->filter("*.ttf");} {}
    code {mFontFile->load(VISUS_FONTDIR);} {}
    code {mFont->text(VISUS_FONT_FILE);
mFont->position(mFont->line_end(0));} {}
    code {mStyle->value(value().fontStyle());} {}
    code {mFontSize->value(value().fontSize());} {}
    code {mMenu->show_inside(parent);} {}
  }
} 
