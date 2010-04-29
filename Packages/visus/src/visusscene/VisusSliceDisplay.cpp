#include "VisusSliceDisplay.h"
#include "VisusSharedColorMap.h"

VisusSliceDisplay::VisusSliceDisplay() : VisusConsumer(1)
{
  declareParameter<VisusSharedColorMap>();
}

int VisusSliceDisplay::connectInput(pVisusGroup producer)
{
  if (producer == NULL) {
    vwarning("Cannot connect NULL input. Connection ignored.");
    return 0;
  }

  return connect(0,(pVisusProducer)producer,&mData);
}


void VisusSliceDisplay::display(VisusTransformation3D model_view_3D,
                                VisusTransformation2D model_view_2D,
                                bool lock_graph)
{
  VisusTransformation3D local;
  pVisusSharedColorMap color_map;

  float tex_max[2];
  float extent[3];

  color_map = sharedValue<VisusSharedColorMap>();

  // Update the modelview matrix and the color map
  getValue(local);  

  model_view_3D *= local;


  if (mDataMutex.lock() == 0) {
    vwarning("Could not lock texture for reading. Aborting rendering.");
    return;
  }

  synchronize();

  if (color_map->lockToRead() == 0) {
    vwarning("Could not lock color map for reading. Rendering aborted.");
    if (mDataMutex.unlock() == 0)
      vwarning("Could not unlock texture after reading. Aborting rendering.");
    return;
  }
  
  mData.preDraw(*color_map);
  mData.texMax(tex_max,tex_max+1);
  mData.getExtent(extent);
 
  
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadMatrixf(model_view_3D);

  glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);

  glBegin(GL_QUADS);
  {
    glTexCoord2f(0,0);                  glVertex3f(0,0,0);
    glTexCoord2f(tex_max[0],0);         glVertex3f(extent[0],0,0);
    glTexCoord2f(tex_max[0],tex_max[1]);glVertex3f(extent[0],extent[1],0);
    glTexCoord2f(0,tex_max[1]);         glVertex3f(0,extent[1],0);
  }
  glEnd();
  
  if (color_map->unlockAfterRead() == 0) {
    vwarning("Could not unlock color map after renderin. Aborting rendring.");
    if (mDataMutex.unlock() == 0) 
      vwarning("Could not unlock texture after reading. Aborting rendering.");
    return;
  }
    
  if (mDataMutex.unlock() == 0) {
    vwarning("Could not unlock texture after reading. Aborting rendering.");
    return;
  }


  mData.postDraw();
  
  if (lock_graph && (lockToRead() == 0)) {
    vwarning("Could not lock graph for reading no further recursion in the display function");
    return;
  }
  
  //vmessage("Enter VisusGroup::display recursion");

  for (CIterator it=mChildren.begin();it!=mChildren.end();it++) 
    (*it)->display(model_view_3D,model_view_2D,false);
  
  //vmessage("Leave VisusGroup::display recursion");
  
  if (lock_graph && (unlockAfterRead() == 0)) 
    vwarning("Could not unlock graph after reading");
  
  glPopMatrix();
}

    
