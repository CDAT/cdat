#ifndef VISUSSLICEDISPLAY_H
#define VISUSSLICEDISPLAY_H

#include "VisusConsumer.h"
#include "VisusSmartPointer.h"
#include "VisusTexture.h"
#include "VisusColorMap.h"

class VisusSliceDisplay;
typedef VisusSmartPointer<VisusSliceDisplay> pVisusSliceDisplay;

class VisusSliceDisplay : public VisusConsumer
{
public:

  VisusSliceDisplay();

  virtual ~VisusSliceDisplay() {}

  int connectInput(pVisusGroup producer);

  virtual void display(VisusTransformation3D model_view_3D = VisusTransformation3D(),
                       VisusTransformation2D model_view_2D = VisusTransformation2D(),
                       bool lock_graph = true);
private:

  //! The data stored as texture
  VisusTexture mData;

  //! Mutex to protect the data
  VisusMutex mDataMutex;

};

#endif
