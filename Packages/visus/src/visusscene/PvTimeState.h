/***********************************************************************
*
* Copyright (c) 2008, Lawrence Livermore National Security, LLC.  
* Produced at the Lawrence Livermore National Laboratory  
* Written by bremer5@llnl.gov,pascucci@sci.utah.edu.  
* LLNL-CODE-406031.  
* All rights reserved.  
*   
* This file is part of "Simple and Flexible Scene Graph Version 2.0."
* Please also read BSD_ADDITIONAL.txt.
*   
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are
* met:
*   
* @ Redistributions of source code must retain the above copyright
*   notice, this list of conditions and the disclaimer below.
* @ Redistributions in binary form must reproduce the above copyright
*   notice, this list of conditions and the disclaimer (as noted below) in
*   the documentation and/or other materials provided with the
*   distribution.
* @ Neither the name of the LLNS/LLNL nor the names of its contributors
*   may be used to endorse or promote products derived from this software
*   without specific prior written permission.
*   
*  
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
* A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL LAWRENCE
* LIVERMORE NATIONAL SECURITY, LLC, THE U.S. DEPARTMENT OF ENERGY OR
* CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
* EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
* PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
* PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
* LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
* NEGLIGENCE OR OTHERWISE) ARISING
*
***********************************************************************/


#ifndef PV_TIME_INCLUDED
#define PV_TIME_INCLUDED

#include <string>

/////////////////////////////////////////////////////////////////////////////
///
/// class PvTimeState encodes a time information about a data source time state
///
/// This class describes a time state for use in GUI and by data extractor
/// At this time the design is to have the DataSource interface be the
/// primary means of getting this object, and of modifying it.  This way
/// the DataSource can ensure that the time state is always valid.
///
// An alternative design would be to allow subclassing of this object, and
// to pass it via pointer into the data source to query data.  The data source
// would have methods to return valid time state objects for various input
// values (like string representations of dats for climate data).
//
// Still another design choice would be to allow anyone to set values on this
// object and have the data Source validate the object when it is passed
// into the interface.
//
// Finally, a third design would have this object become fat, in that it
// would represent an abstract interface to time states, and would have
// methods for advancing the time state, mapping time states to strings,
// and other things.  It would ensure that it was always valid for a given
// data source.  This does bring up problems of what happens if the time
// state becomes mismatched with the data source.
//
/////////////////////////////////////////////////////////////////////////////
class PvTimeState {

    public:

        static const char* XML_TAG;

        // This are copy constructors, and can initalize and construct 
        // objects by assignment:  e.g., PvTimeState t = 1.2 is 
        // equivalent to: PvTimeState t(1.2).

        PvTimeState(int    timeIndex);
        PvTimeState(double timeValue);

        PvTimeState(const PvTimeState& other);
        PvTimeState& operator=(const PvTimeState& other);
        
        /// index return integer time state
        //
        int    index(void) const;

        /// value return floating point time representation state
        //
        double value(void) const;

        /// stringValue string representation of the time state
        //
        std::string stringValue(void) const;

        ////////////////////////////////////////////////////////////////
        //
        // Time State modification:  these functions should only be
        // used by DataSource to return valid time values based on
        // requests by users based on int, double, and string time
        // representations.
        //
        ////////////////////////////////////////////////////////////////

        /// index set the time state
        /// @param[in] i the time state to set. This object is a value type
        ///            and contains no error checking.  Thus, only set the
        ///            time state if you know it is valid.
        //
        void   index(int i);

        /// value set the time state
        /// @param[in] t the time state to set. This object is a value type
        ///            and contains no error checking.  Thus, only set the
        ///            time state if you know it is valid.
        /// @param[in] interpolate request that 't' be interpolated between
        ///            valid time states of the DataSource.
        ///            Note that this means that the index() will be
        ///            undefined and should not be accessed.
        //
        void   value(double t, bool interpolate = false);

        /// value set the time state
        /// @param[in] v the time state to set. This object is a value type
        ///            and contains no error checking.  Thus, only set the
        ///            time state if you know it is valid.
        /// @param[in] interpolate request that 'v' be interpolated between
        ///            valid time states of the DataSource.  The
        ///            index() and value() should be treated as 
        ///            undefined.
        //
        void stringValue(std::string v, bool interpolate = false);

        bool   interpolate(void) const;

        //! Build XML instance data from XML tree
        bool fromXML(XMLNode& node);

        //! Build XML instance data into XML tree
        void toXML(XMLNode& parent) const;

    private:

        /// mInterpolate indicates that this time state should be interpolated
        ///              by the DataSource object.  If this is true
        ///              the index value is defined to be INT_MIN.
        bool mInterpolate;

        int         mIndex;
        double      mValue;
        std::string mStringValue;

        PvTimeState();  // no default construction allowed
};

#endif // include guard

