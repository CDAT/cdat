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


#ifndef PVDATATYPE_INCLUDED
#define PVDATATYPE_INCLUDED

//! Data types recognized by a data source
/*! List of data types used to indicate the size and encoding of the
  stored data. Int16, Float64 etc. should be self explanatory. The RAW
  format exists to allow arbitrarily sized data to be passed through
  the interface. The two generic INT and FLOAT types exist to support
  portability of incore methods between different platforms. For
  example, a python array of int's on will likely be an array of INT32
  on a 32-bit machine and an array of INT64 on a 64-bit
  architecture. Having a data type indiacting "the current system int"
  should allow portable code in these (rare) case. Nevertheless, the
  explicit versions (e.g. INT32) are strongly prefered.
 */
enum PvDataType {
    PV_RAW     = 0, //! raw data of (potentially) unknown type and of any size 

    PV_CHAR    = 10, //! signed char 
    PV_UCHAR   = 11, //! unsigned char

    PV_INT     = 20, //! "standard" int on the current architecture
    PV_INT16   = 21, //! signed 16-bit integer
    PV_UINT16  = 22, //! unsigned 16bit integer
    PV_INT32   = 23, //! signed 32-bit
    PV_UINT32  = 24, //! unsigned 32-bit interger
    PV_INT64   = 25, //! signed 64-bit integer
    PV_UINT64  = 26, //! unsigned 64-bit integer

    PV_FLOAT   = 30, //! "standard" float on the current architecture
    PV_FLOAT32 = 31, //! 32-bit float
    PV_FLOAT64 = 32, //! 64-bit float

    PV_RGB     = 40, //! 24-bit color value 
    PV_RGBA    = 41, //! 32-bit color value
};

/*! Return the number of bytes the given PVDataType occupies on its
  own. This should be equivalent to a sizeof() the corresponding native data
  type. By convention the PV_RAW format will return -1.
  @param type: input type
  @return number of bytes the PVDatatype uses or -1 if type = PV_RAW
*/
inline int
pv_size_of(PvDataType type)
{
    switch (type) {
        case PV_RAW:
            return -1;
        case PV_CHAR:
        case PV_UCHAR:
            return 1;
        case PV_INT:
            return sizeof(int);
        case PV_INT16:
        case PV_UINT16:
            return 2;
        case PV_INT32:
        case PV_UINT32:
            return 4;
        case PV_INT64:
        case PV_UINT64:
            return 8;
        case PV_FLOAT:
            return sizeof(float);
        case PV_FLOAT32:
            return 4;
        case PV_FLOAT64:
            return 8;
        case PV_RGB:
            return 3;
        case PV_RGBA:
            return 4;
    }
    return -1;
}

#endif // include guard
