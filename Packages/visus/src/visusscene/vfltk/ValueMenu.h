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


#ifndef VALUEMENU_H
#define VALUEMENU_H

#include "fltk/Window.h"
#include "VisusGroup.h"

class ValueMenuInterface
{
public:

  ValueMenuInterface() {}

  virtual ~ValueMenuInterface() {}
};

/*! A ValueClass encapsulates two interfaces. In the first mode the
 *  input is a reference to a value and the value() call simply
 *  returns a reference to this value. In the second mode the input is
 *  the pointer to a node and the vlaue class will return the result
 *  of a getValue on that node. In this case it is important to also
 *  perform a commit after setting the value to call the setValue
 *  function on the node. Calling commit in the reference mode is a
 *  no-op. This joined interface allows to implement menus as (a) do
 *  not have to wrap each and every function call of the value class;
 *  and (b) allow the same menu to work for local and shared
 *  variables.
 */
template <class ValueClass>
class ValueMenu : public ValueMenuInterface
{
public:

  //! Empty constructor call
  ValueMenu(fltk::Window* parent=NULL);

  //! Constructor call of the reference mode
  ValueMenu(ValueClass& value, fltk::Window* parent=NULL);

  //! Destructor
  ~ValueMenu() {}

  //! Return the reference to the current value
  virtual ValueClass& value() {return mValue;}
  
  //! Commit the current value to the system
  virtual void commit() {}

protected:

  //! Local copy of the value 
  ValueClass mLocalValue;

  //! Reference to the value
  ValueClass& mValue;

  fltk::Window* mParent;
};


template<class ValueClass>
ValueMenu<ValueClass>::ValueMenu(fltk::Window* parent) : 
  ValueMenuInterface(), mValue(mLocalValue), mParent(parent)
{
}


template<class ValueClass>
ValueMenu<ValueClass>::ValueMenu(ValueClass& value, fltk::Window* parent) : 
  ValueMenuInterface(), mValue(value), mParent(parent)
{
}


#endif
