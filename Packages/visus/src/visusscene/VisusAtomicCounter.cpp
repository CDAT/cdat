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



#ifndef VISUS_DISABLE_HARDWARE_COUNTER


namespace atomicCounter {


#ifdef WIN32

  void __fastcall Inc (volatile int*)
  {
    __asm {lock inc dword ptr [ECX]}
  }
  

  void __fastcall Inc (volatile short*)
  {
    __asm {lock inc word ptr [ECX]}
  }
  
  void __fastcall Inc (volatile char*)
  {
    __asm {lock inc byte ptr [ECX]}
  }

  int __fastcall Dec (volatile int*)
  {
    __asm {
      lock dec dword ptr [ECX]
      mov EAX, [ECX]
    }
  }
  

  short __fastcall Dec (volatile short*)
  {
    __asm {
      lock dec word ptr [ECX] 
      mov AX, [ECX]
    }
  }
  
  char __fastcall Dec (volatile char*)
  {
    __asm {
      lock dec byte ptr [ECX]
      mov AL, [ECX]
    }
  }

  int __fastcall CompareAndSwap (volatile int* dest, int source, int comparend)
 {
   __asm {
     mov EAX, [ESP+4]
     lock cmpxchg dword ptr [ECX], EDX   ;// if([ECX]==EAX){ZF=1;[ECX]=EDX;}else ZF=0;
     //setZ AL  // return boolean based on Z flag.

    // that makes more sense than always returning comparend, regardless!
    ret 4
       }
 }


  short __fastcall CompareAndSwap (volatile short* dest, short source, short comparend)
 {
   __asm {
     mov EAX, [ESP+4]
     lock cmpxchg word ptr [ECX], DX   ;// if([ECX]==EAX){ZF=1;[ECX]=EDX;}else ZF=0;
     //setZ AL  // return boolean based on Z flag.

    // that makes more sense than always returning comparend, regardless!
    ret 4
       }
 }

  char __fastcall CompareAndSwap (volatile char* dest, char source, char comparend)
 {
   __asm {
     mov EAX, [ESP+4]
     lock cmpxchg byte ptr [ECX], DL   ;// if([ECX]==EAX){ZF=1;[ECX]=EDX;}else ZF=0;
     //setZ AL  // return boolean based on Z flag.

    // that makes more sense than always returning comparend, regardless!
    ret 4
       }
 }


#else
#ifdef LINUX_64

#else
   void Inc (volatile int* counter)
  {    
    __asm__ ("lock\n\t"
             "incl (%0)" : : "r" (counter));
  }

  
  void  Inc (volatile short* counter)
  {
    __asm__ ("lock\n\t"
             "incw (%0)" : : "r" (counter));
  }
  
  void  Inc (volatile char* counter)
  {
    __asm__ ("lock\n\t"
             "incb (%0)" : : "r" (counter));
  }


  int Dec (volatile int* counter)
  {    
    __asm__ ("lock\n\t"
             "decl (%0)\n\t" 
             "movl (%0), %%eax" : : "r" (counter));
  }

  
  short Dec (volatile short* counter)
  {
    __asm__ ("lock\n\t"
             "decw (%0)\n\t" 
             "movw (%0), %%ax\n\t" : : "r" (counter));
  }
  
  char  Dec (volatile char* counter)
  {
    __asm__ ("lock\n\t"
             "decb (%0)\n\t" 
             "movb (%0), %%al\n\t" : : "r" (counter));
  }

  int CompareAndSwap (volatile int* dest, int source, int comparend)
  {
    __asm__ ("movl %2, %%eax\n\t"
             "lock\n\t"
             "cmpxchgl %1, (%0)\n\t"// if((%0)==%1){ZF=1;(%0)=%1;}else ZF=0;
             : 
             : "r" (dest), "r" (source), "r" (comparend));
  }

  short CompareAndSwap (volatile short* dest, int source, int comparend)
  {
    __asm__ ("movl %2, %%eax\n\t"
             "lock\n\t"
             "cmpxchgw %1, (%0)\n\t"// if((%0)==%1){ZF=1;(%0)=%1;}else ZF=0;
             : 
             : "r" (dest), "r" (source), "r" (comparend));
  }

  char CompareAndSwap (volatile char* dest, int source, int comparend)
  {
    __asm__ ("movl %2, %%eax\n\t"
             "lock\n\t"
             "cmpxchgb %1, (%0)\n\t"// if((%0)==%1){ZF=1;(%0)=%1;}else ZF=0;
             : 
             : "r" (dest), "r" (source), "r" (comparend));
  }

#endif
#endif

} // end namepsace



#else //  VISUS_DISABLE_HARDWARE_COUNTER


#endif
