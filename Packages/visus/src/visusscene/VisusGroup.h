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


#ifndef VISUSGROUP_H
#define VISUSGROUP_H

#include <cstdlib>
#include <cstdio>
#include "VisusManagedObject.h"
#include "VisusSmartPointer.h"
#include "VisusParameter.h"
#include "VisusParameterList.h"
#include "VisusAssert.h"
#include "VisusSharedIsoValue.h"
#include "VisusSharedFont.h"
#include "VisusSharedBoundingBox.h"
#include "VisusSharedTransformation3D.h"
#include "VisusSharedTransformation2D.h"
#include "VisusSharedDataRequest.h"
#include "VisusSharedDataDescription.h"
#include "VisusSharedFieldIndex.h"
#include "VisusSharedColorMap.h"
#include "VisusSharedColor.h"
#include "VisusSharedCamera.h"
#include "VisusSharedEarthRadius.h"
#include "VisusSharedGlobalTime.h"
#include "VisusSharedOpenGLState.h"
#include "VisusMutex.h"
#include "VisusReadWriteLock.h"
#include "VisusAtomicCounter.h"

#include <iostream>
struct XMLNode;

#include <vector>

#include "VisusNodeTypes.h"

enum VisusRenderMode {
  VISUS_3D_RENDER = 0,
  VISUS_2D_RENDER = 1,
  VISUS_NUM_RENDER_MODE = 2,
};

/* The following code would allow the comparison of enums using the
   operator== without the problem of ambiguity with the int==int
   version. However, this is only used for the searching in the
   hierarchy and otherwise is really unituitive. Therefore I have
   removed this for now (ptb)

template<class EnumClass>
class VisusNodeTypeCompare
{
public:
  VisusNodeTypeCompare(EnumClass e) {mEnum = e;}
  operator EnumClass() {return mEnum;}

  EnumClass mEnum;
};
bool operator==(const VisusNodeTypeCompare<VisusNodeType> & t0, const VisusNodeTypeCompare<VisusNodeType> & t1);
*/


bool compare(const VisusNodeType & t0, const VisusNodeType & t1);

#include "VisusString.h"

template<>
std::string toString<VisusGroup>(const VisusGroup* group);

class VisusGroup;
typedef VisusSmartPointer<VisusGroup> pVisusGroup;

//Group node containing a list of parameters that can be inherited
//from the parent and passed to the children.
class VisusGroup : public VisusManagedObject
{
  
public:
 
  /***************************************************************
   ******       Static Members/Functions                 *********
   **************************************************************/  
  
  //! Name of XML Tag
  static const char* XML_TAG;

  //! Overall number of declared shared types
  static const int sNumTypes = VISUS_NUM_TYPEIDS;
 
  //! Return the number of declared shared values
  static int numTypes() {return sNumTypes;}

  //! Return whether the tree is currently locked;
  static int locked() {return true;}//sGraphLockRequest;}

  //! Indicate that we are reading the graph structure
  /*! Indicate that we are reading/traversing the graph structure and
   *  noone should change the node pointers
   *  @return 1 if successful, 0 otherwise
   */
  static int lockToRead();

  //! Indicate that we are done reading the graph structure
  /*! Indicate that you are now done reading and you no
   *  longer depend on the graph information.
   *  @return 1 if successful, 0 otherwise
   */
  static int unlockAfterRead();

  //! Locking the graph for writing
  static int lockToWrite();
  
  //! Unlocking the graph after writing
  static int unlockAfterWrite();

  //! Create an instance of VisusGroup
  static pVisusGroup instantiate();

#ifdef VISUS_NODE_COUNT
  static VisusAtomicCounter<short> sNodeCount;
#endif
  
  //! Flag to indicate the currently active render mode
  static VisusRenderMode sRenderMode;

  /***************************************************************
   ******       Constructors/Destructors                 *********
   **************************************************************/  
  
  //! Default constructor
  VisusGroup(VisusNodeType node_type=VISUS_GROUP_NODE);
	
  //! Default destructor
  /*! Default destructor performing a deep destruction. This means the
   *  node will first destruct all its children (and presumably they
   *  will do the same). Second the node determines wether it owns any
   *  of the parameters (meaning does not inherit them) and delete
   *  those it finds. Finally, the node will free its own memory.
   */
  virtual ~VisusGroup();
	
  //! Overloaded address operator to return a smart pointer
  pVisusGroup operator&();

  //! Return an info string identifying the node
  virtual std::string infoString() const {return std::string("VisusGroup");}

  /***************************************************************
   ******       Hierarchy Functionality                  *********
   **************************************************************/
  
  //! Return smart pointer to self
  pVisusGroup self();

  //! Return the parent pointer
  /*! Return a smart pointer to the parent node. Since this function
   *  returns the pointer by value and the smart pointer can be
   *  treated as atomic unit (i.e. no structs, arrays etc.) this
   *  function does not lock the graph. 
   *  @return: Smart pointer to the parent node or NULL if the node
   *           is the root
   */
  pVisusGroup parent() const {return mParent;}

  //! Return the number of children
  int nrOfChildren() const {return mChildren.size();}
  
  //! Return the child with given index
  /*! Return the child (smart) pointer with the given index or print a warning
   *  and return NULL if no such child exists
   *  @param[in] i: The index of the child we are interested in
   *  @param[out] lock_graph: flag to indicate whether the graph should be locked
   *                          before access
   *  @return The pointer to the i'th child or NULL if no such child
   *          exists 
   */
  pVisusGroup child(int i, bool lock_graph = true) const;

  //! Return the index of the given child or -1
  /*! Return the index of the given child in the array or -1 if no
   *  such child exists.
   *  @param child: The pointer to the child we are looking for
   *  @param[out] lock_graph: flag to indicate whether the graph should be locked
   *                          before access
   *  @return Index of the given child or -1 
   */
  int getChildIndex(pVisusGroup child, bool lock_graph = true); 

  //! Attach a subtree to this node
  /*! Attach the subtree rooted at child to the current node. The
   *  function will add child to the children list of the node and if
   *  necessary link the parameters. By defaultm, the function will
   *  lock the graph for writing before accessing/modifying the
   *  graph. The locking can be prevented by passing
   *  lock_graph=false. This allows the user to manually lock the
   *  graph to perform several consecutive modification without the
   *  danger of unwanted threading issues.
   *  @param[in] child: smart pointer to the root of the subtree that
   *                    we want to attach
   *  @param[in] lock_graph: flag to indicate whether the graph should
   *                         be locked before accessing it.
   *  @return: 1 if successfull; 0 otherwise
   */
  int attachSubTree(pVisusGroup child, bool lock_graph = true);
  
  //! Detach subtree from from this node
  /*! Remove the subtree rooted at the child with the given index from
   *  this node. The function will unlink the parameters of the
   *  sub-tree and remove the child from the list of children. The
   *  sub-tree itself will not be changed expect in order to make the
   *  parameter structure consistent. By default the function will
   *  lock the tree for writing before reading/modifying the graph.
   *  @param[in] child_index: index of the child whose subtree should
   *                          be detached
   *  @param lock_graph: flag to indicate whether the graph should
   *                         be locked before accessing it.
   *  @return 1 if sucessful; 0 otherwise
   */
  int detachSubTree(int child_index, bool lock_graph = true);

  //! Detach subtree from from this node
  int detachSubTree(pVisusGroup child, bool lock_graph = true);

  //! Clear all local data from the tree rooted at node
  int destroySubTree(bool lock_graph = true);

  //! Get the next node of the given category in depth first order
  /*! Return a pointer to the next node of the given category. The
   *  order is defined by the depth first traversal of the graph. If
   *  no node of the given category exists return a pointer to the
   *  original node.
   *  @param[in] category: the category we are interested in
   *  @return a pointer to the next node of the given category or 
   *          "this" if no such node exists.
   */
  pVisusGroup nextNode(VisusNodeCategory category);

  //! Get the next node of the given type in depth first order
  pVisusGroup nextNode(VisusNodeType type);
  

  /***************************************************************
   ******       Public Access                            *********
   **************************************************************/

  //! Capture the hierarchy and save in PPM file
  void screenShot(const char* filename);

  //! Return the node type
  VisusNodeType type() const {return mNodeType;}

  //! Return the node category
  VisusNodeCategory category() const {return (VisusNodeCategory)(mNodeType & VISUS_UNDEFINED_NODE_CATEGORY);}

  //! Return whether we currently draw the bounding box
  bool drawBoundingBox() {return mDrawBoundingBox;}

  //! Return whether we are currently visible
  bool visible() {return mVisible;}

  //! Return whether this group contains a copy of this shared value
  bool hasSharedValue(int type_index);

  //! Return whether this group contains a copy of this shared value
  bool hasSharedValue(const pVisusSharedValue& value);

  //! Return whether the shared value of the given index is inherited
  bool inherit(int type_index);

  //! Return whether the shared value of the given type is inherited
  bool inherit(const pVisusSharedValue& value);

  //! Return whether the shared value of the given type is inherited
  template<class SharedValueClass>
  bool inherit() {return inherit(SharedValueClass::sTypeIndex);}

  //! Indicate whether we should draw the bounding box
  void drawBoundingBox(bool flag) {mDrawBoundingBox = flag;}

  //! Indicate whether this node should be drawn
  void visible(bool v) {mVisible = v;}

  //! Set the inherit flag of the shared value
  int inherit(int type_index, bool flag, bool lock_graph=true);
  
  //! Set the inherit flag of the shared value
  int inherit(const pVisusSharedValue& value, bool flag);

  //! Set the inherit flag of the shared value
  template<class SharedValueClass>
  int inherit(bool flag) {return inherit(SharedValueClass::sTypeIndex,flag);}

  //! Set the color of the bounding box
  /*! Set the bounding box color to the given value. For now the
   *  bounding box color is not a shared value. It seems unlikely that
   *  one wants to inherit this attribute and it would only bloat the
   *  shared parameter structure. Furthermore, protecting the color
   *  against threaded access seems overkill.
   */
  void boundingBoxColor(const VisusColor& color) {mBoundingBoxColor = color;}

  template <class SharedValueClass>
  int getValue(typename SharedValueClass::VisusValueClass& value) const;

  template <class SharedValueClass>
  int setValue(const typename SharedValueClass::VisusValueClass& value, bool mark_dirty = true);

  //! Indicate that some part of the scenegrapph has changed
  /*! Mark this scenegraph as dirty to force a re-draw. Note that this
   *  is currently implemented using a static redraw flag. If multiple
   *  independent scene graphs become a serious use case the
   *  ipmlementation need to change to a per graph bases
   */
  void markAsDirty();

  //! Return the current dirty flag and reset it to false as an atomic
  //! operation
  bool readClearDirty();

  //! Return the current dirty flag
  bool isDirty() {return sDirtyFlag;}

  /***************************************************************
   ******    Shared Value Access                         *********
   **************************************************************/
  
  //! Declare that this node has the given parameter
  int declareParameter(const VisusSharedValue& value, bool inheri_flag = false);

  //! Indicate that this class uses the given template parameter
  int declareParameter(int type_index, bool inherit = false);

  //! Propagate a shared value upwards to the root of the tree
  int propagateUpwards(const VisusSharedValue& value) {return propagateUpwards(value.getIndex());}

  //! Propagate a shared value upwards to the root of the tree
  int propagateUpwards(int type_index);
  
  //! Propagate a shared value downwards to all children
  int propagateDownwards(const VisusSharedValue& value, bool lock_graph=true); 

  //! Propagate a shared value downwards to all children
  int propagateDownwards(int type_index, bool lock_graph=true);
  
  /* The getValue/setValue inline functions used to be defined in a separate header file.
   * SWIG did not automatically follow the include and a %extend CPPName had to be added
   * to the SWIG bindings.  Note that, this only allowed the bindings to be directly
   * wrapped onto the VisusGroup class, as when SWIG parsed the included file once, it
   * ignores it on subsequent includes.  This resulted in the SmartPointer and derived
   * classes not having getValue and setValue.  Since the derived classes pick up getValue
   * and setValue from VisusGroup, that, was seemingly okay.  But, in order to get SmartPointer
   * to have functions, %feature(shadow) had to be used to paste those functions on python
   * class.  All said and done... this worked fine on Windows, but under Fedora with g++ 4.1
   * and 4.3 when getValue or setValue was called from a derived smart pointer class (e.g.
   * pVisusTickMarks) the dynamic cast failed warning would trip up in VisusSharedValue::
   * get(set)InternalValue..... So long story, these functions are now inlined here and
   * SWIG magically wraps everything.  No extra loops, and most importantly it works; 
   * everywhere that we know of :-)
   * */
  
  //! Get iso value
  int getValue(VisusIsoValue& value) const {return getValue<VisusSharedIsoValue>(value);}
  //! Set iso value
  int setValue(const VisusIsoValue& value) {return setValue<VisusSharedIsoValue>(value);}


  //! Get bounding box
  int getValue(VisusBoundingBox& value) const {return getValue<VisusSharedBoundingBox>(value);}
  //! Set bounding box
  int setValue(const VisusBoundingBox& value, bool mark_dirty=true) {return setValue<VisusSharedBoundingBox>(value,mark_dirty);}


  //! Get font
  int getValue(VisusFont& value) const {return getValue<VisusSharedFont>(value);}
  //! Set font
  int setValue(const VisusFont& value) {return setValue<VisusSharedFont>(value);}


  //! Get 2D tranformation
  int getValue(VisusTransformation2D& value) const {return getValue<VisusSharedTransformation2D>(value);}
  //! Set 2D transformation
  int setValue(const VisusTransformation2D& value) {return setValue<VisusSharedTransformation2D>(value);}


  //! Get 3D tranformation
  int getValue(VisusTransformation3D& value) const {return getValue<VisusSharedTransformation3D>(value);}
  //! Set 3D transformation
  int setValue(const VisusTransformation3D& value) {return setValue<VisusSharedTransformation3D>(value);}


  //! Get data request
  int getValue(VisusDataRequest& request) const {return getValue<VisusSharedDataRequest>(request);}
  //! Set data request
  int setValue(const VisusDataRequest& request) {return setValue<VisusSharedDataRequest>(request);}


  //! Get data description
  int getValue(VisusDataDescription& s) const {return getValue<VisusSharedDataDescription>(s);}
  //! Set data description
  int setValue(const VisusDataDescription& s) {return setValue<VisusSharedDataDescription>(s);}


  //! Get field index
  int getValue(VisusFieldIndex& i) const {return getValue<VisusSharedFieldIndex>(i);}
  //! Set field index
  int setValue(const VisusFieldIndex& i) {return setValue<VisusSharedFieldIndex>(i);}

  //! Get color map
  int getValue(VisusColorMap& map) const {return getValue<VisusSharedColorMap>(map);}
  //! Set color map
  int setValue(const VisusColorMap& map) {return setValue<VisusSharedColorMap>(map);}

  //! Get color
  int getValue(VisusColor& color) const {return getValue<VisusSharedColor>(color);}
  //! Set color
  int setValue(const VisusColor& color) {return setValue<VisusSharedColor>(color);}

  //! Get camera
  int getValue(VisusCamera& camera) const {return getValue<VisusSharedCamera>(camera);}
  //! Set camera
  int setValue(const VisusCamera& camera) {return setValue<VisusSharedCamera>(camera);}

  //! Get earth radius
  int getValue(VisusEarthRadius& radius) const {return getValue<VisusSharedEarthRadius>(radius);}
  //! Set earth radius
  int setValue(const VisusEarthRadius& radius) {return setValue<VisusSharedEarthRadius>(radius);}

  //! Get global time
  int getValue(VisusGlobalTime& value) const {return getValue<VisusSharedGlobalTime>(value);}
  //! Set global time
  int setValue(const VisusGlobalTime& value) {return setValue<VisusSharedGlobalTime>(value);}

  //! Get global time
  int getValue(VisusOpenGLState& value) const {return getValue<VisusSharedOpenGLState>(value);}
  //! Set global time
  int setValue(const VisusOpenGLState& value, bool mark_dirty=true) {return setValue<VisusSharedOpenGLState>(value,mark_dirty);}

  //#include "VisusGroupSharedValueAccess.h"

  /***************************************************************
   ******         Display Functionality                  *********
   **************************************************************/
  
  //! Accumulate the 2D model view matrices from the root to this node
  int accumulate2D(VisusTransformation2D& t, bool _lock_graph = true);
  
  //! Accumulate the 3D model view matrices from the root to this node
  int accumulate3D(VisusTransformation3D& t, bool _lock_graph = true);

  //! Map the bounding box to the given world bounding box
  /*! Set the internal transformation matrxi to map the current
   *  bounding box of this node to the given world bounding box.
   *  @param[in]: worldLocation bounding box into which the curren
   *              bounding box should be mapped
   */
  void mapToWorldBox(const VisusBoundingBox& worldLocation);

  //! Default rotation function 
  /*! Virtual rotation function meant to be overridden by derived
   *  classes. This function will be called when the user moves the
   *  mouse with the corresponding change x- and y-direction given in
   *  pixels.
   *  @param[in] x: horizontal change in mouse position in pixel units
   *  @param[in] y: vertical change in mouse position in pixel units
   */
  virtual void rotate(float x, float y) {rotate3D(x,y);}

  //! Default translation function 
  /*! Virtual translation function meant to be overridden by derived
   *  classes. This function will be called when the user moves the
   *  mouse with the corresponding change x- and y-direction given in
   *  pixels.
   *  @param[in] x: horizontal change in mouse position in pixel units
   *  @param[in] y: vertical change in mouse position in pixel units
   */
  virtual void translate(float x, float y) {translate3D(x,y);}

  //! Default scaling(zoom) function 
  /*! Virtual scaling function meant to be overridden by derived
   *  classes. This function will be called when the user moves the
   *  mouse with the corresponding change x- and y-direction given in
   *  pixels.
   *  @param[in] x: horizontal change in mouse position in pixel units
   *  @param[in] y: vertical change in mouse position in pixel units
   */
  virtual void scale(float x, float y) {scale3D(x,y);}


  //! Freeze the current transformation
  /*! This call copies the current transformation matrices into their
   *  frozen counter parts and sets the fozen flag. When a node is
   *  frozen it will use these frozen matrices for local drawing. Note
   *  that the call freezes the accumulated transformations *not* the
   *  local ones. Note that all nodes that derive special display
   *  functions are responsible for their own frozen behavior.
   */
  virtual void freeze();

  //! Unfreeze the current node
  virtual void unfreeze();

  //! Indicate wether a node is frozen
  bool frozen() {return mFrozen;}

  //! Default display function
  /*! Virtual function to draw the node and all its children into the
   *  current OpenGL context. This call will itnernally call the 2D
   *  and 3D drawing function in the correct order setting up the
   *  corresponding projections accordingly. Unless specifically
   *  requested the traversal will read-lock the hierarchy during the
   *  drawing process.
   */
  virtual void display(bool lock_graph = true);
  
  //! Display the bounding box
  /*! Display the bounding box. This is a seperate function rather
   *  than being part of the general display call since different node
   *  are likely to define their bounding boxes differently. For
   *  example, an extractor node would draw its current request rather
   *  than the bounding box of its corresponding data set.
   */
  virtual void displayBoundingBox() const;

  //! Validate and retrieve VisusNodeType
  static VisusNodeType validateXML(XMLNode& node);
 
  //! Load instance data with data from XML
  bool fromXML(XMLNode& node, bool lock_graph=true);

  //! Store XML representation to given stream
  void toXML(XMLNode& parent, bool lock_graph=true);

protected:

  //! Store Attributes into XML stream
  virtual void toXMLLocalVariables(XMLNode& parent);

  //! Load instance with XML attributes
  virtual bool fromXMLLocalVariables(XMLNode& node);

  typedef std::vector<pVisusGroup>::iterator CIterator;
  typedef VisusParameterList::iterator PIterator;

  /***************************************************************
   ******       Protected Variables                      *********
   **************************************************************/

  //! Node type
  VisusNodeType mNodeType;

  //! Array of parameters. By default each parameter is inherited (not
  //! copied) from the parent node.
  VisusParameterList mParameters;

  //! References to the parent pointer.
  pVisusGroup mParent;

  //! References to the children
  std::vector<pVisusGroup> mChildren;
  
  //! Flag to determiner whether to draw the bounding box
  bool mDrawBoundingBox;

  //! At which rendering stage should hte bounding box be drawn
  VisusRenderMode mBoundingBoxMode;

  //! Flag to determine whether this node should be drawn at all
  bool mVisible;

  //! Flag indicating whether this node is currently frozen
  bool mFrozen;

  //! The frozen 2D tranformatio matrix
  VisusTransformation2D mFrozen2D;

  //! The frozen 3D tranformation matrix
  VisusTransformation3D mFrozen3D;

  /***************************************************************
   ******       Protected Functions                      *********
   **************************************************************/

  //! Set the parent pointer
  void parent(pVisusGroup parent) {mParent = parent;}

  //! Add a child to the list
  void addChild(pVisusGroup child);
  
  //! Remove a child indentified by its index
  int removeChild(int i);
  
  //! Remove a child identified by its pointer
  int removeChild(const pVisusGroup& child);

  //! Return the next node of a given category and/or type
  pVisusGroup nextNode(const pVisusGroup& source, VisusNodeType type); 

  //! Link the shared parameters of the child to this parent
  int linkParameters(pVisusGroup child); 
  
  //! Link one shared parameter of the child to this parent 
  int linkParameter(pVisusGroup child, int type_index);

  //! Break the links between myself ans this child node
  int unlinkParameters(pVisusGroup child);

  //! Perpetuate a change in a shared value down the tree
  int pushDown(int type_index);
  
  //! Perpetuate a change in shared value up the tree
  int pushUp(int type_index);  

  //! Update the given parameter
  /*! This function updates the parameter with the given index by
   *  starting a (recursive) traversal. The actual traversal order is
   *  guided by the corresponding shared value's traversalSteps array.
   *  @param index: index of the parameter that should be updated
   *  @return 1 if successful, 0 otherwise
   */
  int updateParameter(int index);

  //! Sequentially update all parameters
  /*! This function iterates through all possible parameters and
   *  updates each one in order. 
   *  @return 1 if successful, 0 otherwise
   */
  int updateParameterAllParameters();

  //! Indicate that this class uses the given template parameter
  /*! Declare (usually in the constructor) that the calling node uses a
   *  shared value of the given class. If the inherit flag is set the
   *  function will walk up the graph trying to find an ancestor to
   *  inherit from. If such an ancestor is found all intermediat nodes
   *  inherit its value. Otherwise, the node creates its own default
   *  value.
   *  @param inherit: indicate whether the shared value should be 
   *                  inherited if possible
   *  @return: 1 if successful 0 otherwise
   */
  template<class ParameterClass>
  int declareParameter(bool inherit = false);

  //! Pointer to a shared value 
  /*! This function returns a pointer to a shared value of the given
   *  index or NULL if no such value exists.
   *  @param type_index: type index of the shared value to be returned
   *  @return: pointer to the shared value of the given index or NULL 
   *           if no such value exists.
   */
  pVisusSharedValue sharedValue(int type_index) const;
  
  //! Return a pointer to a shared value 
  pVisusSharedValue sharedValue(const pVisusSharedValue& value) const;

  //! The shared value of given type
  /*! This function returns a pointer to a shared value. However,
   *  unlike the sharedValue(int type_index) type function it return a
   *  pointer of the correct type rather than a pointer to the
   *  baseclass.
   *  @return pointer to a shared value of the given type
   */
  template<class SharedValueClass> 
  VisusSmartPointer<SharedValueClass> sharedValue();

  //! The shared value of given type
  /*! This function returns a const pointer to a shared value. However,
   *  unlike the sharedValue(int type_index) type function it return a
   *  pointer of the correct type rather than a pointer to the
   *  baseclass.
   *  @return pointer to a shared value of the given type
   */
  template<class SharedValueClass> 
  const VisusSmartPointer<SharedValueClass> sharedValue() const;

  //! Return a pointer to a dummy shared value 
  pVisusSharedValue dummySharedValue(int type_index) const;

  int lockToRead(int type_index) {return VisusSharedValue::lockToRead(type_index);}
  int unlockAfterRead(int type_index) {return VisusSharedValue::unlockAfterRead(type_index);}
  int lockToWrite(int type_index) {return VisusSharedValue::lockToWrite(type_index);}
  int unlockAfterWrite(int type_index) {return VisusSharedValue::unlockAfterWrite(type_index);}

  //! Return the parameter with given index
  VisusParameter& parameter(int type_index);

  //! Return the parameter with given index
  const VisusParameter& parameter(int type_index) const;

  //! Bounding box color
  VisusColor mBoundingBoxColor;
  
  //! 2D rotation 
  void rotate2D(float x, float y);

  //! 2D translation
  void translate2D(float x, float y);

  //! 3D rotation
  void rotate3D(float x, float y);

  //! 3D translation
  void translate3D(float x, float y);

  //! 3D scaling
  void scale3D(float x, float y);

  //! Display function drawing the 3D Geometry
  /*! Virtual function to draw the 3D aspects of the node into the
   *  current OpenGL context. It carries a 3D local transformation
   *  matrix to properly accumulate the transformations down the
   *  hierarchy. By default a node will accumulate a 3D transformatio
   *  nmatrix and given the appropriate flag call its drawBoundingBox
   *  function.
   *  @param[in] model_view_3D: current 3D model view matrix
   */
  virtual void display3D(VisusTransformation3D model_view_3D = VisusTransformation3D());

  //! Display function drawing the 2D Geometry
  /*! Virtual function to draw the 2D aspects of the node into the
   *  current OpenGL context. It carries a 2D local transformation
   *  matrix to properly accumulate the transformations down the
   *  hierarchy. By default a node will draw no 2D geometry and
   *  neither carries nor accumulates a 2D transformation.
   *  @param[in] model_view_3D: current 3D model view matrix
   */
  virtual void display2D(VisusTransformation2D model_view_2D = VisusTransformation2D());

  //! Enter the 2D drawing mode
  /*! This function will push the current projection matrix on the
   *  stack and load a default matrix instead. This should be called
   *  before drawing 2D content that is not supposed to be affected by
   *  the camera transform and the only way to align different pieces
   *  of 2D content with the screen.
   */
  int enter2DRenderMode();

  //! Exit 2D drawing mode
  int exit2DRenderMode();

  
  //! Recurse the 3D rendering into the children
  /*! There seems to be a problem with calling display3D from derived
   *  classes. Therefore, we must factor out the recursion.
   */
  void recurse(VisusTransformation3D& model_view_3D);

  //! Recurse the 2D rendering into the children
  /*! There seems to be a problem with calling display2D from derived
   *  classes. Therefore, we must factor out the recursion.
   */
  void recurse(VisusTransformation2D& model_view_2D);

private:

  //! Flag indicating that the scene graph has changed 
  static bool sDirtyFlag;

  //! Mutex to protect the dirty flag
  static VisusMutex sDirtyFlagMutex;

  //! Static mutex used to guard the complete scene graph
  static VisusReadWriteLock sGraphLock;

  //! Static counter indicating how many nodes might be in critical
  //! sections
  static VisusAtomicCounter<short> sGraphMutexAccessCounter;

};


template<class SharedValueClass>
VisusSmartPointer<SharedValueClass> VisusGroup::sharedValue() 
{
  return parameter(SharedValueClass::sTypeIndex).element();
}

template<class SharedValueClass>
const VisusSmartPointer<SharedValueClass> VisusGroup::sharedValue() const
{
  return parameter(SharedValueClass::sTypeIndex).element();
}


template<class ValueClass>
int VisusGroup::declareParameter(bool inherit)
{
  int index = ValueClass::sTypeIndex;

  return declareParameter(index,inherit);
  
  /*
  // If we already inherit a value or own our onw shared value
  // something went wrong
  if (parameter(index).inherit() || parameter(index).element() != NULL) {
    vwarning("Value declared multiple times ... ignoring all but the first");
    return 0;
  }

  // If we should inherit when possible 
  if (inherit) {
    pVisusGroup node = &(*this);

    // Walk up the tree in search of a value to inherit
    while ((node->parent() != NULL) 
           && (node->parent()->sharedValue(index) == NULL)) {
      node = node->parent(); 
    }

    // If we have a node whose parent has a value we can inherit 
    if (node->parent() != NULL) {
      // We copy the shared value 
      parameter(index).element(node->parent()->sharedValue(index));
      parameter(index).inherit(true);
      
      // Finally, we walk up the tree again to set all intermediate
      // nodes to also inherit
      node =  &(*this);
      while (node->parent()->sharedValue(index) == NULL) {
        node->parent()->parameter(index).element(sharedValue(index));
        node->parent()->parameter(index).inherit(true);
        node = node->parent();
      }

      return 1;
    }
  }

  // If we were not supposed to inherit or could not find a node to
  // inherit from we create a default value
  parameter(index).element(gObjectFactory.constructSharedValue<ValueClass>());
  return 1;
  */
}

template <class SharedValueClass>
int VisusGroup::getValue(typename SharedValueClass::VisusValueClass& value) const
{
  vverbose("getValue(%d):Initial: %s\n", VISUS_VSP_VERBOSE, type(), toString(&value).c_str());
	
  if (SharedValueClass::lockToReadStatic() == 0) {
     vwarning("Could not lock shared value for reading ignoring getValue");
    return 0;
  }

  const VisusSmartPointer<SharedValueClass> local(sharedValue(SharedValueClass::sTypeIndex));
  vverbose("getValue(%d):Stored: %s\n", VISUS_VSP_VERBOSE, type(), local->getString().c_str());

  if (local == NULL) {
    vwarning("getValue():: No such value %s",SharedValueClass::sTypeName);
    if (SharedValueClass::unlockAfterReadStatic() == 0)
      vwarning("Could not unlock shared value after reading");
    return 0;
  }

  // The getValueInternal code will do some heavy casting to use code
  // implemented in VisusSharedValue rather than code in the derived
  // classes. There seem to be some issues especially when this code
  // is used by multiple shared libraries. Therefore, we are removing
  // it for now
  // 08/19/08 ptb
  //
  //local->getValueInternal<SharedValueClass>(value);

  local->getValue(value);
  
  if (SharedValueClass::unlockAfterReadStatic() == 0) {
    vwarning("Could not unlock shared value after reading");
    return 0;
  }

  vverbose("getValue(%d):Final: %s\n", VISUS_VSP_VERBOSE, type(), toString(&value).c_str());
  
  return 1;
}

template <class SharedValueClass>
int VisusGroup::setValue(const typename SharedValueClass::VisusValueClass& value, bool mark_dirty)
{
  vverbose("setValue(%d):Input: %s\n", VISUS_VSP_VERBOSE, type(), toString(&value).c_str());
	
  if (SharedValueClass::lockToWriteStatic() == 0) {
     vwarning("Could not lock shared value for writing ignoring setValue");
    return 0;
  }

  VisusSmartPointer<SharedValueClass> local(sharedValue(SharedValueClass::sTypeIndex));
  vverbose("setValue(%d):Initial: %s\n", VISUS_VSP_VERBOSE, type(), local->getString().c_str());
  if (local == NULL) {
    vwarning("setValue():: No such value (%s)",SharedValueClass::sTypeName);
    if (SharedValueClass::unlockAfterWriteStatic() == 0)
      vwarning("Could not unlock shared value after writing");
    return 0;
  }

  // The setValueInternal code will do some heavy casting to use code
  // implemented in VisusSharedValue rather than code in the derived
  // classes. There seem to be some issues especially when this code
  // is used by multiple shared libraries. Therefore, we are removing
  // it for now
  // 08/19/08 ptb
  //
  //int rc = local->setValueInternal<SharedValueClass>(value);
 
  local->setValue(value);
  vverbose("setValue(%d):Final: %s\n", VISUS_VSP_VERBOSE, type(), local->getString().c_str());
 
  if (mark_dirty)
    markAsDirty();

  if (SharedValueClass::unlockAfterWriteStatic() == 0) {
    vwarning("Could not unlock shared value after reading");
    return 0;
  }
  
  return 1;
}

#endif
