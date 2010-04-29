/***********************************************************************
*
* Copyright (C) 2008, Lawrence Livermore National Security, Llc.  
* Produced At The Lawrence Livermore National Laboratory  
* Written By Bremer5@Llnl.Gov,Pascucci@Sci.Utah.Edu.  
* Llnl-Code-406031.  
* All Rights Reserved.  
*   
* This File Is Part Of "Simple And Flexible Scene Graph Version 2.0."
* Please Also Read Bsd_Additional.Txt.
*   
* Redistribution And Use In Source And Binary Forms, With Or Without
* Modification, Are Permitted Provided That The Following Conditions Are
* Met:
*   
* @ Redistributions Of Source Code Must Retain The Above Copyright
*   Notice, This List Of Conditions And The Disclaimer Below.
* @ Redistributions In Binary Form Must Reproduce The Above Copyright
*   Notice, This List Of Conditions And The Disclaimer (As Noted Below) In
*   The Documentation And/Or Other Materials Provided With The
*   Distribution.
* @ Neither The Name Of The Llns/Llnl Nor The Names Of Its Contributors
*   May Be Used To Endorse Or Promote Products Derived From This Software
*   Without Specific Prior Written Permission.
*   
*  
* This Software Is Provided By The Copyright Holders And Contributors
* "As Is" And Any Express Or Implied Warranties, Including, But Not
* Limited To, The Implied Warranties Of Merchantability And Fitness For
* A Particular Purpose Are Disclaimed.  In No Event Shall Lawrence
* Livermore National Security, Llc, The U.S. Department Of Energy Or
* Contributors Be Liable For Any Direct, Indirect, Incidental, Special,
* Exemplary, Or Consequential Damages (Including, But Not Limited To,
* Procurement Of Substitute Goods Or Services; Loss Of Use, Data, Or
* Profits; Or Business Interruption) However Caused And On Any Theory Of
* Liability, Whether In Contract, Strict Liability, Or Tort (Including
* Negligence Or Otherwise) Arising
*
***********************************************************************/

#include "FontMenu.h"
#include "VisusFont.h"

void FontMenu::fontStyle(VISUS_FONT_STYLE style)
{
  VisusFont font;
  mNode->getValue(font);
  
  font.fontStyle(style);
  mNode->setValue(font);
}

void FontMenu::fontSize(int size)
{
  VisusFont font;
  mNode->getValue(font);

  font.fontSize(size);
  mNode->setValue(font);
}

void FontMenu::fontFile(const char *file)
{
  VisusFont font;
  mNode->getValue(font);

  font.fontFile(file);
  mNode->setValue(font);
}

void FontMenu::fontColor(float r, float g, float b)
{
  VisusFont font;
  mNode->getValue(font);

  font.fontColor(r*255,g*255,b*255);
  mNode->setValue(font);
}

VISUS_FONT_STYLE FontMenu::fontStyle() const
{
  VisusFont font;
  mNode->getValue(font);
  
  return font.fontStyle();
}

int FontMenu::fontSize() const
{
  VisusFont font;
  mNode->getValue(font);

  return font.fontSize();
}

const unsigned char *FontMenu::fontColor() const
{
  VisusFont font;
  mNode->getValue(font);

  return font.fontColor();
}
