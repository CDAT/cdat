# data file for the FLTK User Interface Designer (FLUID)
version 2.1000 
images_dir header_name 
header_name {.h} 
code_name {.cpp} 
gridx 5 
gridy 5 
snap 3
class BorderAxisMenuWidget {open : {public ValueMenu<VisusBorderAxis>}
} {
  Function {BorderAxisMenuWidget(VisusBorderAxis& axis,fltk::Window* parent=NULL) : ValueMenu<VisusBorderAxis>(axis)} {open
  } {
    code {createMenu(parent);} {}
  }
  Function {~BorderAxisMenuWidget()} {} {}
  Function {createMenu(fltk::Window* parent)} {open
  } {
    {fltk::Window} mMenu {open
      xywh {885 146 350 350} resizable buttonbox EMBOSSED_BOX
      extra_code {\#include "vfltk/ValueMenu.h"
\#include "VisusBorderAxis.h"
\#include "vfltk/FLTKSubWindow.h"}
      class FLTKSubWindow non_modal visible
    } {
      {fltk::Group} {} {
        label Legend open
        xywh {5 19 335 102} box ENGRAVED_BOX labelfont 1
      } {
        {fltk::Input} mLegend {
          callback {value().legendText(((fltk::Input*)o)->text());
commit();}
          xywh {15 9 160 30} align 5 when CHANGED labelfont 1
        }
        {fltk::Button} {} {
          label {Change Font}
          callback {VisusFont font;
FontMenuWidget* popup;

font = value().legendFont();
popup = new FontMenuWidget(font);
popup->mMenu->hotspot(popup->mMenu);
popup->mMenu->exec();
value().legendFont(font);
commit();

delete popup;}
          xywh {180 9 90 30} box EMBOSSED_BOX
          extra_code {\#include "FontMenuWidget.h"}
        }
        {fltk::ValueInput} mLOffset {
          label Offset
          callback {value().legendOffset(((fltk::ValueInput*)o)->value());
commit();}
          xywh {15 64 45 25} align 5 maximum 10 step 0.1
        }
        {fltk::Choice} mLPos {
          label Position
          callback {value().legendPosition((AXISSide)(mLPos->value()));
commit();} open
          xywh {65 64 95 25} align 5
        } {
          {fltk::Item} {} {
            label {Low Side}
            }
          {fltk::Item} {} {
            label {High Side}
            vertical
          }
          {fltk::Item} {} {
            label {Both Sides}
            }
        }
        {fltk::Choice} mLAlign {
          label Alignment
          callback {value().legendAlignment((AXISAlignment)(mLAlign->value()));
commit();} open
          xywh {180 64 110 25} align 5
        } {
          {fltk::Item} {} {
            label Center
            }
          {fltk::Item} {} {
            label Left
            vertical
          }
          {fltk::Item} {} {
            label Right
            }
        }
        {fltk::CheckButton} mLActive {
          label Show
          callback {value().drawLegend(mLActive->value());
commit();}
          xywh {310 9 25 30} align 4
        }
      }
      {fltk::Group} {} {
        label {Tick Marks} open
        xywh {5 138 340 202} box ENGRAVED_BOX labelfont 1
      } {
        {fltk::Output} mTickTemplate {
          callback {value().labelText(mTickTemplate->text());
commit();}
          xywh {10 14 160 31} align 5 when CHANGED labelfont 1 textsize 11
          class {fltk::Input}
        }
        {fltk::Button} {} {
          label {Change Font}
          callback {VisusFont font;
FontMenuWidget* popup;

font = value().labelFont();
popup = new FontMenuWidget(font);
popup->mMenu->hotspot(popup->mMenu);
popup->mMenu->exec();
value().labelFont(font);
commit();

delete popup;} selected
          xywh {180 15 85 30} box EMBOSSED_BOX
        }
        {fltk::ValueInput} mTOffset {
          label Offset
          callback {value().labelOffset(mTOffset->value());
commit();}
          xywh {10 70 45 25} align 5 maximum 10 step 0.1
        }
        {fltk::Choice} mTPos {
          label Position
          callback {value().labelPosition((AXISSide)mTPos->value());
commit();} open
          xywh {65 70 95 25} align 5
        } {
          {fltk::Item} {} {
            label {Low Side}
            }
          {fltk::Item} {} {
            label {High Side}
            vertical
          }
          {fltk::Item} {} {
            label {Both Sides}
            }
        }
        {fltk::Choice} mTAlign {
          label Alignment
          callback {value().labelAlignment((AXISAlignment)mTAlign->value());
commit();}
          xywh {170 70 110 25} align 5
        } {
          {fltk::Item} {} {
            label Center
            }
          {fltk::Item} {} {
            label Left
            vertical
          }
          {fltk::Item} {} {
            label Right
            }
        }
        {fltk::CheckButton} mTActive {
          label Show
          callback {value().drawLabels(mTActive->value());
commit();}
          xywh {305 15 25 30} align 4
        }
        {fltk::Group} {} {
          label {Major Ticks  } open
          xywh {90 115 250 31} align 4
        } {
          {fltk::ValueInput} mMTCount {
            label Count
            callback {value().majorTicks(mMTCount->value());
commit();}
            xywh {0 0 55 25} align 5 step 1
          }
          {fltk::ValueInput} mMTLength {
            label Length
            callback {value().majorTickLength(mMTLength->value());
commit();}
            xywh {65 0 55 25} align 5 step 0.01
          }
          {fltk::ValueInput} mMTWidth {
            label Width
            callback {value().majorTickThickness(mMTWidth->value());
commit();}
            xywh {135 0 55 25} align 5 step 0.01
          }
        }
        {fltk::Group} {} {
          label {Minor Ticks  } open
          xywh {90 166 200 25} align 4
        } {
          {fltk::ValueInput} mTCount {
            label Count
            callback {value().minorTicks(mTCount->value());
commit();}
            xywh {0 0 55 25} align 5 step 1
          }
          {fltk::ValueInput} mTLength {
            label Length
            callback {value().minorTickLength(mTLength->value());
commit();}
            xywh {65 0 55 25} align 5 step 0.01
          }
          {fltk::ValueInput} mTWidth {
            label Width
            callback {value().minorTickThickness(mTWidth->value());
commit();}
            xywh {135 0 55 25} align 5 step 0.01
          }
        }
      }
    }
    code {initLegend();} {}
    code {initTickMarks();} {}
  }
  Function {initLegend()} {} {
    code {mLegend->text(value().legendText());} {}
    code {mLOffset->value(value().legendOffset());} {}
    code {mLPos->value(value().legendPosition());} {}
    code {mLAlign->value(value().legendAlignment());} {}
    code {mLActive->value(value().drawLegend());} {}
  }
  Function {initTickMarks()} {open
  } {
    code {mTickTemplate->text(value().labelText());} {}
    code {mTOffset->value(value().labelOffset());} {}
    code {mTPos->value(value().labelPosition());} {}
    code {mTAlign->value(value().labelAlignment());} {}
    code {mTActive->value(value().drawLabels());} {}
    code {mMTCount->value(value().majorTicks());} {}
    code {mMTLength->value(value().majorTickLength());} {}
    code {mMTWidth->value(value().majorTickThickness());} {}
    code {mTCount->value(value().minorTicks());} {}
    code {mTLength->value(value().minorTickLength());} {}
    code {mTWidth->value(value().minorTickThickness());} {}
  }
} 
