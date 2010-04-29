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

#ifndef FLTKSUBWINDOW_H
#define FLTKSUBWINDOW_H

#include <fltk/Window.h>

class FLTKSubWindow : public fltk::Window
{
public:

  FLTKSubWindow(int w, int h, const char *label=NULL) : Window(0,0,w,h,label) {}

  ~FLTKSubWindow() {}
};

#endif
