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


#include <string>
#include <vector>

#include "VisusDataSourceFactory.h"
#include "VisusNullDataSource.h"
#include "VisusFastLoopDataSource.h"
#include "VisusIncoreDataSource.h"
#include "VisusManagedIncoreSource.h"

// ==================================================================
//
//  The factory maintains a vector of DsMaker objects that define
//  the string ID's used to figure out which object to make, and
//  a function that calls the constructor.
//
// To add a new Data Source:  Copy and Paste one of the existing
// DsMaker objects, and edit it to reflect the new DataSource.
// add it to the end of the sDsMaker vector.
// ==================================================================


// ANONYMOUS NAMESPACE:  enclose all DsMaker derived classes inside
//                       this namespace.  This restricts these 
//                       declarations and defintions to this file only.
//                       The closing brace of this namespace is clearly
//                       labeled below.

namespace {
  
  struct DataSourceMaker {
    virtual std::string Id(void) const = 0;
    virtual VisusDataSource* make(const std::string& args) const = 0;
    virtual ~DataSourceMaker(void) {}
  };

    std::vector<const DataSourceMaker*> sMakers;

    // ==================================================================
    // NullMaker
    // ==================================================================

    struct NullMaker : public DataSourceMaker {
        NullMaker() {}
        ~NullMaker() {}
        std::string Id(void) const { return std::string("NullDataSource"); }
        VisusDataSource* make(const std::string& args) const {
            return new VisusNullDataSource(args);
        }
    };

  
  // ==================================================================
  // IncoreMaker
  // ==================================================================

  struct IncoreMaker : public DataSourceMaker {
    IncoreMaker() {}
    ~IncoreMaker() {}
    std::string Id(void) const { return std::string("Incore"); }
    VisusDataSource* make(const std::string& args) const {
      return new VisusIncoreDataSource(args);
    }
  };
  
  // ==================================================================
  // ManagedIncoreMaker
  // ==================================================================

  struct ManagedIncoreMaker : public DataSourceMaker {
    ManagedIncoreMaker() {}
    ~ManagedIncoreMaker() {}
    std::string Id(void) const { return std::string("Managed"); }
    VisusDataSource* make(const std::string& args) const {
      return new VisusManagedIncoreSource(args);
    }
  };
  
    // ==================================================================
    // IdxMaker
    // ==================================================================

    struct IdxMaker : public DataSourceMaker {
        IdxMaker() {}
        ~IdxMaker() {}
        std::string Id(void) const { return std::string("Idx"); }
        VisusDataSource* make(const std::string& args) const {
            return new VisusFastLoopDataSource(args);
        }
    };


}  // ********* CLOSE ANONYMOUS NAMESPACE
   // ********* NO DataSourceMaker DEFINTIONS SHOULD OCCUR AFTER THIS


// ==================================================================
// The Factory function
// ==================================================================

VisusDataSource* 
VisusDataSourceFactory::make(const std::string& dataDescription)
{
    if (sMakers.size() == 0) {
        // First call to factory, create maker objects
        sMakers.push_back(new NullMaker());
        sMakers.push_back(new IncoreMaker());
        sMakers.push_back(new ManagedIncoreMaker());
        sMakers.push_back(new IdxMaker());
    }

    std::string dd(dataDescription);

    size_t j = dd.find_first_of(":");
    if (j == std::string::npos) {
        // Attempt to infer the data source type requested
        bool unrecognized = true;
        size_t k = dd.find_first_of(".idx");
        if (k != std::string::npos) {
            dd.insert(0, "Idx:");
            unrecognized = false;
        }

        if (unrecognized) {
            return new VisusNullDataSource("NO_DESCRIPTOR_UNABLE_TO_INFER_DATA_SOURCE_ID");
        }
    }

    j = dd.find_first_of(":");
    if (j + 1 >= dd.length()) {
        return new VisusNullDataSource("NOTHING_AFTER_COLON_IN_DESCRIPTION");
    }

    // Split string into the ID and arguments.  The colon is
    // not copied to either ID or arguments.
    std::string Id(dd, 0, j);
    std::string args(dd, j+1);

    //fprintf(stderr,"Id is >%s<\nfile is >%s<\n", Id.c_str(), args.c_str());

    for (size_t k = 0; k < sMakers.size(); ++k) {
        if (sMakers[k]->Id().compare(Id) == 0) {
            return sMakers[k]->make(args);
        }
    }

    std::string s("BAD ID: ");
    s += Id;
    return new VisusNullDataSource(s);
}
