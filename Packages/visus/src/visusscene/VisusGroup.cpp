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

 
#include "VisusGroup.h"
#include "VisusAssert.h"
#include "VisusScreenShot.h"
#include "VisusSharedBoundingBox.h"
#include "VisusSharedTransformation3D.h"
#include "VisusSharedTransformation2D.h"
#include "VisusSharedOpenGLState.h"
#include "VisusStdInt.h"
#include <algorithm>

#ifdef WIN32
#include <windows.h>
#endif	
#include "glew.h"

#include "xmlParser.h"

const char* VisusGroup::XML_TAG = "VisusGroup";
bool VisusGroup::sDirtyFlag = true;
VisusMutex VisusGroup::sDirtyFlagMutex;
VisusReadWriteLock VisusGroup::sGraphLock;
VisusAtomicCounter<short> VisusGroup::sGraphMutexAccessCounter = 0;

VisusRenderMode VisusGroup::sRenderMode = VISUS_3D_RENDER;

#ifdef VISUS_NODE_COUNT
int VisusGroup::sNodeCount = 0;
#endif

template<>
std::string toString<VisusGroup>(const VisusGroup* group)
{
  std::ostringstream ss;
  ss << "VisusGroup(" << group << "): type(" << group->type() << ")";
  return ss.str();
}

bool compare(const VisusNodeType & t0, const VisusNodeType &t1)
{
  // If the two types are bitwise identical
  if ((int)t0 == (int)t1)
    return true; // Then they count as equal

  // If either of the two type has an undefined node type
  if (((t0 | VISUS_UNDEFINED_NODE_CATEGORY) == (int)VISUS_UNDEFINED_NODE) 
      || ((t1 | VISUS_UNDEFINED_NODE_CATEGORY) == (int)VISUS_UNDEFINED_NODE)) {
    
    // And their node categories match
    if ((t0 & VISUS_UNDEFINED_NODE_CATEGORY) == (t1 & VISUS_UNDEFINED_NODE_CATEGORY))
      return true; // Then they count as equal
  }

  // If either of the two types has an undefined category
  if (((t0 & VISUS_UNDEFINED_NODE_CATEGORY) == (int)VISUS_UNDEFINED_NODE_CATEGORY)
      || ((t1 & VISUS_UNDEFINED_NODE_CATEGORY) == (int)VISUS_UNDEFINED_NODE_CATEGORY))
    return true; // Then they count as equal

  // In all other cases the types count as different
  return false;
}


int VisusGroup::lockToRead()
{
  return sGraphLock.readLock();

  /*
  if (sGraphMutex.lock() == 0) {
    vwarning("Could not obtain graph mutex to enter critcal section ... proceed at your own risk");
    return 0;
  }

  sGraphMutexAccessCounter++;
  //vthread_msg("VisusGroup::lockToRead  accessCounter %d\n",sGraphMutexAccessCounter);
  fprintf(stderr,"VisusGroup::lockToRead  accessCounter %d\n",sGraphMutexAccessCounter);

  if (sGraphMutex.unlock() == 0) {
    vwarning("Could not unlock graph mutex after entering critcal section ... proceed at your own risk");
    return 0;
  }
  
  return 1;
  */
}

int VisusGroup::unlockAfterRead()
{
  return sGraphLock.unlock();

  /*
  sGraphMutexAccessCounter--;

  //vthread_msg("VisusGroup::unlockAfterRead  accessCounter %d\n",sGraphMutexAccessCounter);
  fprintf(stderr,"VisusGroup::unlockAfterRead  accessCounter %d\n",sGraphMutexAccessCounter);
  if (sGraphMutexAccessCounter < 0) {
    vwarning("Graph access counter inconsistent... potential threading hazard");
    return 0;
  }
  
  return 1;
  */
}

int VisusGroup::lockToWrite()
{
  return sGraphLock.writeLock();

  /*
  // Try to obtain the lock
  if (sGraphMutex.lock() == 0) {
    // If we could not get the lock we print a warning 
    vwarning("Could not obtain graph lock");

    return 0;
  }

  // Indicate to all other nodes that we would like to lock the tree
  sGraphLockRequest = true;

  // Now wait until everyone has stoped or declared himself to be in a
  // non-critical section. Note, that even though we have the mutex
  // other nodes can still leave their critical sections. However,
  // they are prevented from entering one.
  while (sGraphMutexAccessCounter > 0) {

    vthread_msg("VisusGroup waiting for write lock accessCounter %d\n",(short)sGraphMutexAccessCounter);
    sleepmillisec(VISUS_MUTEX_WAIT);
  }

  return 1;
  */
}

int VisusGroup::unlockAfterWrite()
{
  return sGraphLock.unlock();

  /*
  // indicate that we no longer need the lock
  sGraphLockRequest = false;

  if (sGraphMutex.unlock() == 0) {
    vwarning("Could not unlock graph mutex");
    return 0;
  }
 
  return 1;
  */
}

pVisusGroup VisusGroup::instantiate()
{
  return gObjectFactory.instantiate<VisusGroup>();
}


/// Default constructor of an empty node.
VisusGroup::VisusGroup(VisusNodeType node_type) : VisusManagedObject(), mNodeType(node_type),
                                                  mDrawBoundingBox(false),mBoundingBoxMode(VISUS_3D_RENDER),
                                                  mVisible(true), mFrozen(false)
{ 
  declareParameter<VisusSharedBoundingBox>();
  declareParameter<VisusSharedTransformation3D>();
  declareParameter<VisusSharedOpenGLState>();

  mBoundingBoxColor = VisusColor(1,1,1);

  
#ifdef VISUS_NODE_COUNT
  sNodeCount++;
#endif
}


VisusGroup::~VisusGroup() 
{ 
  destroySubTree(false);

#ifdef VISUS_NODE_COUNT
  sNodeCount--;
#endif
}

//! Overloaded address operator to return a smart pointer
pVisusGroup VisusGroup::operator&()
{
  return gObjectFactory.createNodeReference<VisusGroup>(this);
}

pVisusGroup VisusGroup::self()
{
  if (referenceCount()->constraintInc() == 0) 
    return pVisusGroup();
  else
    return pVisusGroup(this,referenceCount());
}

pVisusGroup VisusGroup::child(int index, bool lock_graph) const 
{
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading");
    return pVisusGroup();
  }

  if ((index < 0) || (index >= (int)mChildren.size())) {
    vwarning("Index out of range no such child");
    return pVisusGroup();
  }

  pVisusGroup tmp = mChildren[index];
  
  if (lock_graph && (unlockAfterRead() == 0)) 
    vwarning("Could not unlock graph after reading");
  
  return tmp;
}

int VisusGroup::getChildIndex(pVisusGroup child, bool lock_graph )
{
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading");
    return -1;
  }

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) {
    if (*it == child) {
      int index;

      index = (it - mChildren.begin());
      if (lock_graph && (unlockAfterRead() == 0)) 
        vwarning("Could not unlock graph after reading");
      
      return index;
    }
  }

  if (lock_graph && (unlockAfterRead() == 0)) 
    vwarning("Could not unlock graph after reading");

  return -1;
}

int VisusGroup::attachSubTree(pVisusGroup child, bool lock_graph)
{
  //vmessage("Enter VisusGroup::attachSubTree", 0);
  
  if (child == NULL) {
    fprintf(stderr,"Warning: %s:%u: Cannot attach an empty subtree\n",__FILE__,__LINE__);
    return 0;
  }

  // Note, that the lock_graph variable will be evaluated first and if
  // not set the graph will not be locked
  if (lock_graph && (lockToWrite() == 0)) {
    vwarning("Could not lock graph to attach subtree ... ignoring");
    return 0;
  }

  addChild(child);

  if (linkParameters(child) == 0) {
    vwarning("When attaching a subtree could not link parameters");
    if (lock_graph && (unlockAfterWrite() == 0))
      vwarning("Could not unlock graph");
    return 0;
  }
  
  if (lock_graph && (unlockAfterWrite() == 0)) {
    vwarning("Could not unlock graph");
    return 0;
  }
  
  //vmessage("Leave VisusGroup::attachSubTree", 0);
  return 1;
}

int VisusGroup::detachSubTree(int child_index, bool lock_graph)
{
  if (lock_graph && (lockToWrite() == 0)) {
    vwarning("Could not lock graph to detach subtree ... ignoring");
    return 0;
  }  
  
  if ((child_index < 0) && (child_index >= (int)mChildren.size())) {
    vwarning("Child index out of range\n");
    return 0;
  }

  if (unlinkParameters(child(child_index,false)) == 0) {
    vwarning("When detaching a subtree could not unlink parameters");
    if (lock_graph && (unlockAfterWrite() == 0))
      vwarning("Could not unlock graph");
    return 0;
  }
  
  removeChild(child_index);
  
  if (lock_graph && (unlockAfterWrite() == 0)) {
    vwarning("Could not unlock graph");
    return 0;
  }
  
  return 1;
}

int VisusGroup::detachSubTree(pVisusGroup child, bool lock_graph)
{
  int index = getChildIndex(child,lock_graph);

  return detachSubTree(index,lock_graph);
}

//! Clear all local data from the tree rooted at node
int VisusGroup::destroySubTree(bool lock_graph)
{
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading...subtree not destroyed");
    return 0;
  }

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++)
    (*it)->destroySubTree();

  // Notice, that normally one would delete all children and
  // parameters here. However, using smart pointers with reference
  // counting should take care of the memory clean up. If we are the
  // last person with a pointer to a child/shared value deleting the
  // pointer will call the destructor.
  mChildren.clear();

  // Simply setting the shared value pointer to NULL will take care of
  // the memory. Either we inherited this value in which case other
  // nodes contain pointer to the same value and we are fine. If we
  // did not inherit we should be the last node with a pointer (since
  // we just "deleted" all our children) and setting the pointer to
  // NULL will call the destructor of the shared value
  for (PIterator it=mParameters.begin();it!=mParameters.end();it++) 
    it->element(pVisusSharedValue());

  if (lock_graph && (unlockAfterRead() == 0)) {
    vwarning("Could not unlock graph after reading");
    return 0;
  }
   
  return 1;
}

pVisusGroup VisusGroup::nextNode(VisusNodeCategory category)
{
  if (lockToRead() == 0) {
    vwarning("Could not lock graph for traversal.");
    return pVisusGroup();
  }

  pVisusGroup pointer;

  pointer =  nextNode(self(),(VisusNodeType)(category | ~VISUS_UNDEFINED_NODE_CATEGORY));

  if (unlockAfterRead() == 0) 
    vwarning("Could not unlock graph after traversal.");

  return pointer;
}

pVisusGroup VisusGroup::nextNode(VisusNodeType type)
{
  if (lockToRead() == 0) {
    vwarning("Could not lock graph for traversal.");
    return pVisusGroup();
  }

  pVisusGroup pointer;

  pointer =  nextNode(self(),type);

  if (unlockAfterRead() == 0) 
    vwarning("Could not unlock graph after traversal.");

  return pointer;
}

bool VisusGroup::hasSharedValue(int type_index)
{
  type_index_check(type_index,false);

  return (sharedValue(type_index) != NULL);
}

bool VisusGroup::hasSharedValue(const pVisusSharedValue& value)
{
  verror(value == NULL,"Expect pointer to a shared value. Got NULL",
         false);

  return (sharedValue(value) != NULL);
}

bool VisusGroup::inherit(int type_index)
{
  type_index_check(type_index,false);

  return parameter(type_index).inherit();
}

bool VisusGroup::inherit(const pVisusSharedValue& value)
{
  if (value == NULL) {
    vwarning("Expect pointer to a shared value. Got NULL");
    return false;
  }

  return parameter(value->getIndex()).inherit();
}

int VisusGroup::inherit(int type_index, bool flag, bool lock_graph)
{
  type_index_check(type_index,0);

  // Try to obtain the lock for the graph
  if (lock_graph && lockToWrite() == 0) {
    vwarning("Could not lock graph to set inheritance");
    return 0;
  } 

  // If the current flag and the desired one are the same 
  if (inherit(type_index) == flag) {
    // unlock and exit (Note, that when the unlock fails there is
    // nothing we can do but exit and hope for the best
    if (lock_graph && unlockAfterWrite() == 0) {
      vwarning("Could not unlock graph");
      return 0;
    }
    else
      return 1;
  }

  // If there is no such value our structure is inconsistent and we
  // attempt to correct it
  if (parameter(type_index).element() == NULL) {
    vwarning("No such shared value %d. Ignoring inheritence",type_index);
    parameter(type_index).inherit(false);
    if (lock_graph && unlockAfterWrite() == 0) 
      vwarning("Could not unlock graph");
        
    return 0;
  }
  
  // If the current flag is true but we need to set it to false
  if (!flag) {

    // If we inherit but we have no parent the structure is
    // inconsistent and we try to correct it
    if (parent() == NULL) { 
      vwarning("Inherit flag set at root node. Unsetting the flag");
      parameter(type_index).inherit(false);
      if (lock_graph && unlockAfterWrite() == 0) 
        vwarning("Could not unlock graph");

      return 0;
    }
      
    // If we inherit but our pointer is different from that of our
    // parent the structur was inconsistent and setting the
    // inheritence flag to false "should" correct it. Print a warning
    // anyway. 
    if (sharedValue(type_index) != parent()->sharedValue(type_index)) {
      vwarning("Unsetting inheritence flag seems to correct an inconsistent structure."); 
      parameter(type_index).inherit(false);
      if (lock_graph && unlockAfterWrite() == 0) {
        vwarning("Could not unlock graph");
        return 0;
      }
      else
        return 1;
    }
 

    // Otherwise, we must create a new value (which we do by cloning
    // the existing one)
    parameter(type_index).element(sharedValue(type_index)->clone());

    // And fix the pointers in the subtree
    pushDown(type_index);
    
  } // end-if (!flag)
  else { // If we currently do not inherit but we should be inheriting
 
    // If we are the root we cannot inherit and this is a (valid)
    // no-op
    if (parent() == NULL) {
      vwarning("The root of the graph cannot inherit. Setting inheritence ignored.");
      if (lock_graph && unlockAfterWrite() == 0) {
        vwarning("Could not unlock graph");
        return 0;
      }
      else
        return 1;
    }

    // If we do not inherit but carry the same pointer as our parent
    // the structure is inconsistent and we attempt to correct it
    if (sharedValue(type_index) == parent()->sharedValue(type_index)) {
      vwarning("Non-inherited value shared with parent. Correcting inheritence.");
      parameter(type_index).inherit(true);

      if (lock_graph && unlockAfterWrite() == 0) 
        vwarning("Could not unlock graph");
      return 0;
    }

    // Store our current value to delete it later
    // No need to store anything because of the smart pointers
    // pVisusSharedValue tmp = sharedValue(type_index);

    // Set our new value and inheritence flag
    parameter(type_index).element(parent()->sharedValue(type_index));
    parameter(type_index).inherit(true);

    // Push the changes down the tree 
    pushDown(type_index);

    // Delete the old value
    //delete tmp;

  }
    
    
  if (lock_graph && unlockAfterWrite() == 0) {
    vwarning("Could not unlock graph");
    return 0;
  }
  else
    return 1;
}

int VisusGroup::inherit(const pVisusSharedValue& value, bool flag)
{
  if (value == NULL) {
    vwarning("Inherit called with NULL pointer .. ignoring");
    return 0;
  }

  return inherit(value->getIndex(),flag);
}


void VisusGroup::markAsDirty()
{
  //fprintf(stderr,"VisusGroup::markAsDirty()  %d\n",sDirtyFlag);
  if (!sDirtyFlag) {
    if (sDirtyFlagMutex.lock() == 0) {
      vwarning("Could not lock dirty flag mutex.");
      sDirtyFlag = true;
      return;
    }

    sDirtyFlag = true;

    if (sDirtyFlagMutex.unlock() == 0) 
      vwarning("Could not unlock dirty flag mutex.");
  }
}

bool VisusGroup::readClearDirty()
{
  bool flag;

  //fprintf(stderr,"VisusGroup::readClearDirty() %d\n",VisusGroup::sDirtyFlag);
  if (sDirtyFlagMutex.lock() == 0) {
    vwarning("Could not lock dirty flag mutex.");
    flag = sDirtyFlag;
    sDirtyFlag = false;
    return flag;
  }
  
  flag = sDirtyFlag;
  sDirtyFlag = false;
  
  if (sDirtyFlagMutex.unlock() == 0) 
    vwarning("Could not unlock dirty flag mutex.");
  
  return flag;
}

int VisusGroup::declareParameter(const VisusSharedValue& value, bool inherit_flag)
{
  if (!hasSharedValue(value.getIndex())) {
    declareParameter(value.getIndex(),inherit_flag);
  }

  inherit(value.getIndex(),inherit_flag);

  return 1;
}

void VisusGroup::addChild(pVisusGroup child)
{
  vwarn(!locked(),"When changing the tree layout the tree should be locked");

  mChildren.push_back(child);
  
  child->parent(self());
  //child->parent(&(*this));
}

int VisusGroup::removeChild(int index)
{
  vwarn(!locked(),"When changing the tree layout the tree should be locked");

  if ((index < 0) || (index >= (int)mChildren.size())) {
    vwarning("Index out of range bo such child");
    return 0;
  } 
  else {
    std::swap(mChildren[index],mChildren.back());
    mChildren.pop_back();
    return 1;
  }
}

int VisusGroup::removeChild(const pVisusGroup& child)
{
  vwarn(!locked(),"When changing the graph layout the graph should be locked");

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) {
    if (*it == child) {
      std::swap(*it,mChildren.back());
      mChildren.pop_back();
      return 1;
    }
  }
  
  vwarning("Cannot remove child. No such child not found");
  return 0;
}

pVisusGroup VisusGroup::nextNode(const pVisusGroup& source, VisusNodeType type)
{
  if (source == NULL)
    return pVisusGroup();

  // If I have at least one child try that child first
  if (nrOfChildren() > 0) {
    // If we have tried every node but have arrived back at the source
    if (mChildren[0] == source)
      return source; 

    // If the types match
    if (compare(mChildren[0]->type(),type)) 
      return mChildren[0];
    else
      return mChildren[0]->nextNode(source,type);
  }

  // If I don't have a child try my sibling

  // If I don't have a father the graph consists only of myself
  if (mParent == NULL)
    return source;

  int index;
  pVisusGroup p = mParent;
  pVisusGroup c = self();
  
  while (true) {
    index = p->getChildIndex(c,false);
    if (index == -1) {
      vwarning("Graph structure inconsistent. Child not part of the parent.");
      return pVisusGroup();
    }
    
    // If I am the last sibling
    if (index == p->nrOfChildren()-1) {
      // Move the traversal to the previous level

      c = p;
      p = p->parent();
      
      // If we have arrived at the root.
      if (p == NULL) {
        // Start the traversal anew from the root
        if (c == source)
          return source;
        
        if (compare(c->type() ,type))
          return c;
        else
          return c->nextNode(source,type);
      }
    }
    else {
      // Try the next sibling
      c = p->child(index+1);

      if (c == source)
        return source;
      
      if (compare(c->type(),type))
        return c;
      else
        return c->nextNode(source,type);
    }
  }
           
  return pVisusGroup();
}

int VisusGroup::linkParameters(pVisusGroup child)
{
  vwarn(!locked(),"When changing the graph layout the graph should be locked");
  
  int flag = 1;

  for (int i=0;i<VisusSharedValue::numTypes();i++) {
    flag *= linkParameter(child,i);

    // If the child actually contains this shared value and it is a
    // shared value that by default is inherited we mark it as such
    if ((child->sharedValue(i) != NULL) && child->sharedValue(i)->inheritsByDefault()) {
      child->inherit(i,true,false);
    }
  }
  
  return flag;
}

int VisusGroup::linkParameter(pVisusGroup child, int type_index)
{
	//vmessage("Entered linkParameter... %d\n", type_index);
	
  if (child == NULL) {
    vwarning("Expected pointer to valid node got NULL");
    return 0;
  }

  type_index_check(type_index,0);

  // This function adjusts the secondary hierarchy to reflect the new
  // link from (*this) down to (*child). There exist six distinct
  // cases depending on whether the parameter exists in either the
  // child or the parent and in case of the child whether it is
  // inherited
  //
  //          1.    2.       3.              4.             5.        6
  // parent | no | yes |  no          |  no          | yes         | yes
  // child  | no |  no | yes(inherit) | yes(not inh) | yes(inherit)| yes(not inh)
  //
  

  pVisusSharedValue parent_value;
  pVisusSharedValue child_value;
  bool inherit;

  parent_value = sharedValue(type_index);
  child_value = child->sharedValue(type_index);
  inherit = child->inherit(type_index);

  // First we perform a few sanity checks and if possible attempt to
  // correct any problems we find
  if ((child_value != NULL) && inherit) {
    vwarning("Node inherits non-existent shared value. Reseting inheritance");
    child->parameter(type_index).inherit(false);
  }


  // 
  // Case 1+2+4+6: If the child does not have this parameter or
  // contains its own personal copy there is nothing to do
  // 
  if ((child_value == NULL) || !inherit) {
		//vmessage("Leave linkParameter... %d... child(%s) or not inherited(%s)\n", type_index, child_value==NULL?"null":"ptr", inherit?"true":"false");
    return 1;
  }


  //
  // Case 3: The parent does not contain the value but the child does
  // and further more the child used to inherit this value. In this
  // case the child must create a new parameter as it is not the top
  // node and cannot inherit anymore. This value must then be passed
  // down to all nodes in the sub-tree
  //
  if ((parent_value == NULL) && (child_value != NULL) && inherit) {
    
    // First we clone the shared value
    child->parameter(type_index).element(child_value->clone());
    
    // Then we mark it as non-inherited
    child->parameter(type_index).inherit(false);

    // Finally, we fix all nodes in the subtree
    child->pushDown(type_index);

		//vmessage("Leave linkParameter... %d... parent=NULL, child!=NULL, inherit true\n", type_index);
    return 1;
  }

  //
  // Case 5: The parent contains the value and so does the
  // child. Furthermore, the child used to inherit the value from its
  // "old" parent.
  // 
  if ((parent_value != NULL) && (child_value != NULL) && inherit) {
    
    // Replace the values in the subtree
    pushDown(type_index);
    
		//vmessage("Leave linkParameter... %d... parent!=NULL, child!=NULL, inherit true\n", type_index);
    return 1;
  }
    
  // If we get here something is wrong
  vwarning("Implementation error no all cases covered");
  return 0;
}

int VisusGroup::unlinkParameters(pVisusGroup child)
{
  vwarn(!locked(),"When changing the graph layout the graph should be locked");

  if (child == NULL) {
    vwarning("Expected pointer to valid node got NULL");
    return 0;
  }

  int flag = 1;

  for (int i=0;i<numTypes();i++) {
    if ((child->sharedValue(i) != NULL) && child->inherit(i))
      flag *= child->inherit(i,false);
  }
  
  return flag;
}


int VisusGroup::pushDown(int type_index)
{
  vwarn(!locked(),"When changing the graph layout the graph should be locked");

  if (parameter(type_index).element() == NULL) {
    vwarning("Cannot push nonexistent parameter");
    return 0;
  }
  
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) {

    if (((*it)->sharedValue(type_index) != NULL)  // If the child contains this value
        && (*it)->parameter(type_index).inherit() // Inherits from me
        && ((*it)->sharedValue(type_index) != sharedValue(type_index))) { // But points to the 
                                                                          // wrong value
      // Change the parameter association of the child parameter
      (*it)->parameter(type_index).element(sharedValue(type_index));
      
      // Recurse
      (*it)->pushDown(type_index);
    }
  }

  return 1;
}

int VisusGroup::pushUp(int type_index)
{
  vwarn(!locked(),"When changing the graph layout the graph should be locked");

  if (parameter(type_index).element() == NULL) {
    vwarning("Cannot push nonexistent parameter");
    return 0;
  }


  if (parameter(type_index).inherit()) {
    vwarning("Pushing an inherited parameter upwards has no effect");
    return 0;
  }
    
  if (parent() == NULL)
    return 1;

  if (parent()->sharedValue(type_index) == NULL) { // If my parent does not contain this value
    parent()->parameter(type_index).element(sharedValue(type_index)); // Set its value to my value
    parent()->parameter(type_index).inherit(false); // Indicate that my parent now owns the value
    parameter(type_index).inherit(true); // Indicate that I no longer own the value
  }
  else if (parent()->parameter(type_index).inherit()) { // If my parent contains this value but 
                                                        // does not own it
    parent()->parameter(type_index).element(sharedValue(type_index)); // Set the values
    parent()->parameter(type_index).inherit(false); // Pass on the ownership
    parameter(type_index).inherit(true); // Release the ownership      
    
    // Inform all my siblings that the parent has changed
    parent()->pushDown(type_index);
  }
  else {
    // Notice that we purposefully do not care about the old
    // parent()->sharedValue(type_index pointer. Normally, one would
    // have to delete the shared value but the smart pointers should
    // handle this internally

    // Replace it with our own (Note that the inheritance flag of
    // parent() is false)
    parent()->parameter(type_index).element(sharedValue(type_index));

    // We now inherit from parent()
    parameter(type_index).inherit(true);
    
    // Finally we must make sure to remove all pointers to the old
    // shared value
    parent()->pushDown(type_index);

  }

  return 1;
}

int VisusGroup::updateParameter(int index)
{
  type_index_check(index,0);

  if (parameter(index).element() == NULL) {
    vwarning("Cannot update non-existent shared value.");
    return 0;
  }

  // Get the pointer to the array of traversal states
  const TraversalState *steps = parameter(index).element()->theSteps();

  for (int i=0;i<parameter(index).element()->nSteps();i++){
    switch (steps[i]){
    case UPDATE_THE_CHILDREN: 
      for (CIterator  child=mChildren.begin();child!=mChildren.end( );child++)
        (*child)->updateParameter(index);
      break;
    case SYNC_WITH_CHILDREN: 
      for (CIterator  child=mChildren.begin();child!=mChildren.end( );child++) {
        if (((*child)->parameter(index).element() != NULL) && // If this child has such a value
            !(*child)->parameter(index).inherit()) // And does not inherit it from us
          {        
            mParameters[index].element()->syncWithChild((*child)->parameter(index).element());
          }
      }
      break;
    case UPDATE_THE_PARENT: 
      if (mParent != NULL)
        mParent->updateParameter(index);
      break;
    case SYNC_WITH_PARENT:
      mParameters[index].element()->syncWithParent(mParent->parameter(index).element());
      break;
    default:
      vwarning("Unrecognize traversal directive");
      break;
    }
  }
  return 1;
}

int VisusGroup::updateParameterAllParameters()
{
  int flag = 1;

  for (int i=0;i<VisusSharedValue::numTypes(); i++)
    flag *= updateParameter(i);

  return flag;
};

int VisusGroup::declareParameter(int type_index, bool inherit)
{
  // If we already inherit a value or own our onw shared value
  // something went wrong
  if (parameter(type_index).inherit() || parameter(type_index).element() != NULL) {
    vwarning("Value declared multiple times ... ignoring all but the first");
    return 0;
  }

  // If we should inherit when possible. The additional condition
  // prevents us to go into the if-block during a constructor. The
  // class is not fully formed yet and this will lead to a seg
  // fault. However, since during tyhe constructor parent()==NULL
  // anyway this is the expected behavior.
  if (inherit && (this->mReferenceCount != NULL)) {
    pVisusGroup node = &(*this);

    // Walk up the tree in search of a value to inherit
    while ((node->parent() != NULL) 
           && (node->parent()->sharedValue(type_index) == NULL)) {
      node = node->parent(); 
    }
    
    // If we have a node whose parent has a value we can inherit 
    if (node->parent() != NULL) {
      // We copy the shared value 
      parameter(type_index).element(node->parent()->sharedValue(type_index));
      parameter(type_index).inherit(true);
      
      // Finally, we walk up the tree again to set all intermediate
      // nodes to also inherit
      node =  &(*this);
      while (node->parent()->sharedValue(type_index) == NULL) {
        node->parent()->parameter(type_index).element(sharedValue(type_index));
        node->parent()->parameter(type_index).inherit(true);
        node = node->parent();
      }
      
      return 1;
    }
  }

  // If we were not supposed to inherit or could not find a node to
  // inherit from we create a default value
  parameter(type_index).element(gObjectFactory.constructSharedValue(type_index));
  return 1;
}

int VisusGroup::propagateUpwards(int type_index)
{
  if (parent() != NULL) {
    pushUp(type_index);
    parent()->propagateUpwards(type_index);
  }

  return 1;
}

int VisusGroup::propagateDownwards(const VisusSharedValue& value, bool lock_graph) 
{
  return propagateDownwards(value.getIndex(),lock_graph);
}

int VisusGroup::propagateDownwards(int type_index, bool lock_graph)
{  
  // Try to obtain the lock for the graph
  if (lock_graph && (lockToWrite() == 0)) {
    vwarning("Could not lock graph to propagate parameter downwards");
    return 0;
  } 

  int flag = 1;
  
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) {
    
    if (!(*it)->hasSharedValue(type_index))
      flag *= (*it)->declareParameter(type_index,true);
    else
      flag *= (*it)->inherit(type_index,true,false);

    flag *= (*it)->propagateDownwards(type_index,false);
  }

   if (lock_graph && (unlockAfterWrite() == 0)) {
    vwarning("Could not unlock graph after propagating parameter downwards");
    return 0;
  }
 

  return flag;
}

pVisusSharedValue VisusGroup::sharedValue(int type_index) const
{
  type_index_check(type_index,pVisusSharedValue());
  
  return mParameters[type_index].element();
}

pVisusSharedValue VisusGroup::sharedValue(const pVisusSharedValue& value) const
{
  if (value == NULL)
    return pVisusSharedValue();

  return mParameters[value->getIndex()].element();
}

pVisusSharedValue VisusGroup::dummySharedValue(int type_index) const
{
  return VisusSharedValue::dummySharedValue(type_index);
}

VisusParameter& VisusGroup::parameter(int type_index)
{
  type_index_check(type_index,VisusParameter::nullReference());
  
  return mParameters[type_index];
}

const VisusParameter& VisusGroup::parameter(int type_index) const
{
  type_index_check(type_index,VisusParameter::nullReference());
  
  return mParameters[type_index];
}


void VisusGroup::mapToWorldBox(const VisusBoundingBox& worldLocation)
{
  VisusTransformation3D local;
  VisusBoundingBox box;

  getValue(local);
  getValue(box);

  VisusTranslationStyle t_style;

  t_style = local.translationStyle();
  
  local = translationMatrix((worldLocation[3] + worldLocation[0])/2,
                            (worldLocation[4] + worldLocation[1])/2,
                            (worldLocation[5] + worldLocation[2])/2);
  
  // We want to scale the local bounding box such that it's largest
  // extent fits within with world box. If the world box contains only
  // a single point we do not scale at all
  float scale = 10e34 + 1;

  for (int i=0;i<3;i++) {
    // This test should probably be done relative  
    if (box[i+3] - box[i] > 0.000001) 
      scale = std::min(scale,(worldLocation[i+3] - worldLocation[i]) / (box[i+3] - box[i]));
  }

  // If we could determine a scaling factor we use it
  if (scale < 10e34) 
    local *= scaleMatrix(scale,scale,scale);
  

  local *= box.translateToCenter();
  
  local.translationStyle(t_style);
  setValue(local);
}


void VisusGroup::rotate2D(float x, float y)
{
  if (sharedValue<VisusSharedTransformation2D>() == NULL) {
    vwarning("Node contains no 2D Transformation cannot rotate2D");
    return;
  }
  
  VisusTransformation2D local;

  y *= -1;
  //fprintf(stderr,"VisusGroup::rotate2D  x=%f  y=%f\n",x,y);

  getValue(local);
  local.rotate(x,y);
  setValue(local);
}

void VisusGroup::translate2D(float x, float y)
{
  if (sharedValue<VisusSharedTransformation2D>() == NULL) {
    vwarning("Node contains no 2D Transformation cannot translate2D");
    return;
  }

  VisusTransformation2D local;
  
  y *= -1;
  //fprintf(stderr,"VisusGroup::translate2D  x=%f  y=%f\n",x,y);

  getValue(local);
  local.translate(x,y);
  setValue(local);
}

void VisusGroup::rotate3D(float x, float y)
{
  if (sharedValue<VisusSharedTransformation3D>() == NULL) {
    vwarning("Node contains no 3D Transformation cannot rotate3D");
    return;
  }
  
  VisusTransformation3D local;
  VisusBoundingBox box;

  float rotation_center[4];

  // First accumulate the transformations from the root to this node
  VisusTransformation3D acc;
  accumulate3D(acc);

  y *= -1;
  //fprintf(stderr,"VisusGroup::rotate3D  x=%f  y=%f\n",x,y);

  getValue(local);
  getValue(box);

  box.center(rotation_center);
  //fprintf(stderr,"VisusGroup::rotate3D rotation center[ %f %f %f %f ]\n", rotation_center[0],rotation_center[1],rotation_center[2],rotation_center[3]);
  local.rotate(acc,rotation_center,x,y);
  setValue(local);
}

void VisusGroup::translate3D(float x, float y)
{
  if (sharedValue<VisusSharedTransformation3D>() == NULL) {
    vwarning("Node contains no 3D Transformation cannot translate3D");
    return;
  }
  
  VisusTransformation3D local;
  VisusTransformation3D acc;
  VisusOpenGLState state;

  accumulate3D(acc);

  y *= -1;
  //fprintf(stderr,"VisusGroup %d::translate3D  x=%f  y=%f\n",this,x,y);

  getValue(local);
  getValue(state);

  local.translate(acc,state,x,y);
  setValue(local);

  //VisusTransformation3D mat =  acc; 
  //fprintf(stderr,"%f %f %f %f\n%f %f %f %f\n%f %f %f %f\n\n",mat[0],mat[4],mat[8],mat[12],
  //        mat[1],mat[5],mat[9],mat[13],mat[2],mat[6],mat[10],mat[14]);
}


void VisusGroup::scale3D(float x, float y)
{
  if (sharedValue<VisusSharedTransformation3D>() == NULL) {
    vwarning("Node contains no 3D Transformation cannot translate3D");
    return;
  }
  
  VisusTransformation3D local;
  VisusBoundingBox box;

  float scale_center[4];

  //fprintf(stderr,"VisusGroup::translate3D  x=%f  y=%f\n",x,y);

  
  getValue(box);
  box.center(scale_center);

  getValue(local);
  local.scale(scale_center,x,y);
  setValue(local);
}

void VisusGroup::freeze()
{

  if (hasSharedValue(VisusSharedTransformation2D::sTypeIndex)) {
    getValue(mFrozen2D);
    accumulate2D(mFrozen2D,true);
  }

  getValue(mFrozen3D);
  accumulate3D(mFrozen3D,true);

  mFrozen = true;
}

void VisusGroup::unfreeze()
{
  mFrozen = false;
}
  

void VisusGroup::display(bool lock_graph)
{  
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in the display function");
    return;
  }

  sRenderMode = VISUS_3D_RENDER;
  
  // First we draw the 3D geometry
  display3D();

  sRenderMode = VISUS_2D_RENDER;

  // Now we reset the projections etc. to 2D drawing mode
  enter2DRenderMode();

  // And start the 2D render traversal
  display2D();

  // Finally, we return to the default 3D projections
  exit2DRenderMode();

  if (lock_graph && (unlockAfterRead() == 0)) 
    vwarning("Could not unlock graph after reading");
}
  
void VisusGroup::display3D(VisusTransformation3D model_view_3D)
{  
  VisusTransformation3D local;

  if (!frozen()) {
    getValue(local);
    model_view_3D *= local;
  }
  else
    model_view_3D = mFrozen3D;

  glMatrixMode(GL_MODELVIEW);
  
  glPushMatrix();
  glLoadMatrixf(model_view_3D);

  if (mVisible) {

    glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
    
    glColor3f(1,1,1);
    
    if (mDrawBoundingBox && (mBoundingBoxMode == sRenderMode))
      displayBoundingBox();
  }

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display3D(model_view_3D);
  
  glPopMatrix();
}
  
void VisusGroup::display2D(VisusTransformation2D model_view_2D)
{  
  // The default for a node is to neither draw nor accumulated a 2D
  // transformation
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display2D(model_view_2D);
}
  
void VisusGroup::displayBoundingBox() const
{
  VisusBoundingBox box;

  getValue(box);

  mBoundingBoxColor.glColor();
  box.display();
}

int VisusGroup::accumulate2D(VisusTransformation2D& t, bool lock_graph)
{
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in accumulate2D");
    return 0;
  }
 
  pVisusGroup node = parent();
  VisusTransformation2D local; // local matrix

  while (node != NULL) {
    if (node->sharedValue<VisusSharedTransformation2D>() != NULL) {
      // If the current node is frozen we can stop the traversal an
      // only look at its frozen matrix
      if (node->frozen()) {

        t.leftMultiply(node->mFrozen2D);
      }
      else { // If the node is not frozen we accumulate its local variable

        // We use a local variable here rather than passing the shared
        // value through directly since using the set/get functions
        // makes things automatically thread-safe.
        node->getValue(local);
        t.leftMultiply(local);
      }
    }
    
    if (node->frozen()) 
      node = pVisusGroup();
    else
      node = node->parent();
  }  

  if (lock_graph && (unlockAfterRead() == 0)) {
    vwarning("Could not unlock graph after reading");
    return 0;
  }

  return 1;
}

int VisusGroup::accumulate3D(VisusTransformation3D& t, bool lock_graph)
{
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in accumulate3D");
    return 0;
  }
 
  pVisusGroup node = parent();
  VisusTransformation3D local; // local matrix

  while (node != NULL) {
    if (node->sharedValue<VisusSharedTransformation3D>() != NULL) {
      
      // If the current node is frozen we can stop the traversal an
      // only look at its frozen matrix
      if (node->frozen()) {

        t.leftMultiply(node->mFrozen3D);
      }
      else { // If the node is not frozen we accumulate its local variable

        // The scene node must be treated differently since it takes its
        // info from its camera rather than its modeview matrix
        if (compare(node->type(),VISUS_SCENE_NODE)) {
          VisusCamera cam;
          node->getValue(cam);
          t.leftMultiply(cam.modelView());
        }
        else {
          // We use a local variable here rather than passing the shared
          // value through directly since using the set/get functions
          // makes things automatically thread-safe.
          node->getValue(local);
          t.leftMultiply(local);
        }
      }
    }
    
    if (node->frozen())
      node = pVisusGroup();
    else
      node = node->parent();
  }  

  if (lock_graph && (unlockAfterRead() == 0)) {
    vwarning("Could not unlock graph after reading");
    return 0;
  }

  return 1;
}

int VisusGroup::enter2DRenderMode()
{
  GLint viewport[4];
  // get the current viewport
  glGetIntegerv(GL_VIEWPORT,viewport);
  GLint width = viewport[2];
  GLint height= viewport[3];

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();
  //glOrtho(-10,10,-10*width/(float)height,10*width/(float)height,-100,100);
  //glOrtho(-1,1,-1*width/(float)height,1*width/(float)height,-100,100);
  //glOrtho(-1,1,-1,1,-100,100);
  glOrtho(-width/(float)height,width/(float)height,-1,1,-100,100);
  gluLookAt(0,0,1,0,0,0,0,1,0);

  glClear(GL_DEPTH_BUFFER_BIT);
  glMatrixMode(GL_MODELVIEW);

  //glDisable(GL_DEPTH_TEST);
  glEnable(GL_LIGHTING);

  return 1;
}

int VisusGroup::exit2DRenderMode()
{
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();

  glMatrixMode(GL_MODELVIEW);
  glEnable(GL_DEPTH_TEST);
  return 1;
}

void VisusGroup::recurse(VisusTransformation3D& model_view_3D)
{
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display3D(model_view_3D);
} 

void VisusGroup::recurse(VisusTransformation2D& model_view_2D)
{
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display2D(model_view_2D);
} 

void VisusGroup::screenShot(const char* filename)
{
  GLint viewport[4];  // x, y, width, height

  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

  display();
  
  glGetIntegerv(GL_VIEWPORT,viewport);

  VisusScreenShot ss(viewport[2], viewport[3]);
  ss.dumpScreen(filename);
}

VisusNodeType VisusGroup::validateXML(XMLNode& node)
{
  if (strcmp(node.getName(), XML_TAG)) {
    vwarning("invalid child of top level Visus. Expected a VisusGroup for a scene graph");
    return VISUS_UNDEFINED_NODE;
  }

  VisusNodeType type = (VisusNodeType) xmltoi(node.getAttribute("nodeType"), VISUS_UNDEFINED_NODE);
  vverbose("  VisusGroup type is (%d)\n", VISUS_XML_VERBOSE, type);
  return type;
}

bool VisusGroup::fromXMLLocalVariables(XMLNode& group)
{
  return true;
}

#define XML_PARAMETER "Parameter"


bool VisusGroup::fromXML(XMLNode& group, bool lock_graph)
{
  // Lock Graph
  if (lock_graph && (lockToWrite() == 0)) {
    vwarning("Could not lock graph for writing no further recursion in the toXML function");
    return false;
  }

  // Read Node
  mDrawBoundingBox = xmltobool(group.getAttribute("drawBBox"), 1);
  mVisible         = xmltobool(group.getAttribute("visible"), 1);
  vverbose("  read draw bbox %d and visible %d\n", VISUS_XML_VERBOSE, 
    mDrawBoundingBox, mVisible);

  // Get Attrs
  XMLNode bboxColor = group.getChildNode("boundingBoxColor");
  XMLNode n = bboxColor.getChildNode(0);
  if (! mBoundingBoxColor.fromXML(n))
    return false;

  vverbose("  read bounding box color\n", VISUS_XML_VERBOSE);

  if (!fromXMLLocalVariables(group)) {
    vwarning("failed to read derived local variables");
    return false;
  }

  // Initialize all shared's to not inherit (in case do not have them)
  for (int i=0; i<VisusParameterList::sNumTypes; ++i)
    parameter(i).inherit(false);

  // Get Shared Attributes
  int numParams = group.nChildNode(XML_PARAMETER);
  vverbose("  there are (%d) Parameters\n", VISUS_XML_VERBOSE, numParams);
  for (int i=0; i<numParams; ++i)
  {
    XMLNode node = group.getChildNode(XML_PARAMETER, i);
    const int type = xmltoi(node.getAttribute("type"), -1);
    if (type < 0)
      break;

    vverbose("  read XML Parameter node for type (%d)\n", VISUS_XML_VERBOSE, type);

    const bool inherit = xmltobool(node.getAttribute("inherit"), false);
  
    if (! inherit) 
    {
      if (! hasSharedValue(type)) 
      {
        std::stringstream ss;
        ss << "VG(" << this->type() << ") attempt to load non-inherited shared value type (" 
           << type << ") which we do not possess";
        vwarning(ss.str().c_str());
        continue;
      }
      node = node.getChildNode(0);
      vverbose("  load XML parameter data\n", VISUS_XML_VERBOSE);
      if (! parameter(type).element()->fromXML(node))
        return false;
    }
    else {
      this->inherit(type, inherit, false);
    }
  }

  // Get Children
  int numGroups = group.nChildNode(XML_TAG);
  vverbose("  there are (%d) VisusGroup children\n", VISUS_XML_VERBOSE, numGroups);
  for (int i=0; i<numGroups; ++i)
  {
    XMLNode node = group.getChildNode(XML_TAG, i);

    VisusNodeType type = (VisusNodeType) xmltoi(node.getAttribute("nodeType"));
    vverbose("  child %d is type %d\n", VISUS_XML_VERBOSE, i, type);

    pVisusGroup child  = gObjectFactory.createNode(type);  
    attachSubTree(child, false);
    if (! child->fromXML(node, false)) {
      return false;
    }
  }

  // Unlock Graph
  if (lock_graph && (unlockAfterWrite() == 0)) {
    vwarning("Could not unlock graph after writing");
    return false;
  }  
  return true;
}

void VisusGroup::toXMLLocalVariables(XMLNode& parent)
{
}

void VisusGroup::toXML(XMLNode& parent, bool lock_graph)
{
  // Lock Graph
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in the toXML function");
    return;
  }

  // Write Node
  XMLNode group = parent.addChild(XML_TAG);
  group.addAttribute("nodeType", mNodeType);
  group.addAttribute("drawBBox", mDrawBoundingBox);
  group.addAttribute("visible", mVisible);

  // Write Attrs
  XMLNode bboxColor = group.addChild("boundingBoxColor");
  mBoundingBoxColor.toXML(bboxColor);
  toXMLLocalVariables(group);

  // Write Shared Attributes
  for (int i=0; i<mParameters.size(); ++i)
  {
    if (hasSharedValue(i))
    {
      pVisusSharedValue param = parameter(i).element();

      XMLNode p = group.addChild(XML_PARAMETER);
      p.addAttribute("type", i);
      p.addAttribute("inherit", parameter(i).inherit());
      if (!parameter(i).inherit()) 
      {
        param->toXML(p, lock_graph);
      }
    }
  }

  // Write Out Children
  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->toXML(group, false);

  // Unlock Graph
  if (lock_graph && (unlockAfterRead() == 0)) {
    vwarning("Could not unlock graph after reading");
    return;
  }  
}
