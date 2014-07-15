# include <cairo/cairo.h>
#include "Qt/mainwindow.h"
#include <Python.h>
#include <QtCore/QEvent>
#include <QtCore/QMutex>
#include <QtCore/QWaitCondition>
#include <QtGui/QDesktopWidget>
#include <QtGui/QMenu>
#include <QtGui/QPainter>
#include <QtGui/QToolTip>
#include "Qt/vcs_legacy_events_Qt_mapping.h"
#include "vcs_legacy_canvas.h"
#include <QtCore/QString>

#include <stdio.h>

QPoint menupos;

extern "C" PyObject *getPyCanvas( int canvas_id);
extern "C" float cnorm(PyVCScanvas_Object *self, int x_or_y, float value);
extern "C" float plnorm(int x_or_y, float value);
extern "C" int get_data_coords(PyVCScanvas_Object *self,Gpoint point,struct item_list *item,struct data_point *info);
extern PyInterpreterState * mainInterpreterState;
extern PyThreadState * mainThreadState;
extern PyThreadState * myThreadState;

QMutex globalvcs_legacymutex(QMutex::Recursive);

void acquire_global_lock() {
  globalvcs_legacymutex.lock();
}
void release_global_lock() {
  globalvcs_legacymutex.unlock();
}

// extern "C" int QtWorking = 0;
static int cairoIsSetup = -1;
// extern "C" int updating;
QMutex mutex;
extern "C" Gconid_X_drawable connect_id;

// extern QWaitCondition vcs_legacythreaddone;
// extern int vcs_legacy_working;

QString prepare_info(struct data_point info) {
  char final[1024] = {};
  char buffer[1024];
  if (info.x_index != -999)
    sprintf( buffer, "X[%d]: %g", info.x_index, info.x);
  else
    {
      if (info.x == -999)
        sprintf( buffer, "X    : NaN");
      else
        sprintf( buffer, "X    : %g", info.x);
    }

  /* Second Line of Text */
  strcat(final, buffer);

  if (info.y_index != -999)
    sprintf( buffer, "\nY[%d]: %g", info.y_index, info.y);
  else
    {
      if (info.y == -999)
        sprintf( buffer, "\nY    : NaN");
      else
        sprintf( buffer, "\nY    : %g", info.y);
    }

  /* Third Line of Text */
  strcat(final, buffer);
  if (((info.x_index == -999) || (info.y_index == -999)) && info.value == -999)
    if (info.value2 == -999)
      {
        sprintf( buffer, "\0");
      }
    else
      {
        sprintf( buffer, "\nData 1:   N/A");
      }
  else
    if (info.value>9.9E19) 
      {
        if (info.value2 == -999)
          {
            sprintf( buffer, "\nData:   Masked");
          }
        else
          {
            sprintf( buffer, "\nData 1:   Masked");
          }
        info.color=-999;
      }
    else
      {
        if (info.value2 == -999)
          {
            sprintf( buffer, "\nData:   %g", info.value);
          }
        else
          {
            sprintf( buffer, "\nData 1:   %g", info.value);
          }
      }

  /* Fourth and Fith Line of Text */
  strcat(final, buffer);

  if (info.value2 != -999)
    if (info.value2>9.9E19) 
      {
        sprintf( buffer, "\nData 2:   Masked");
      }
    else
      {
        sprintf( buffer, "\nData 2:   %g", info.value2);
        if (info.value<9.9E19)
          {
            sprintf(buffer, "\nVector: %g",sqrt(info.value2*info.value2+info.value*info.value));
          }
        else
          {
            sprintf(buffer, "");
          }

      }
  else
    {
      sprintf( buffer, "");
    }
  
  /* Sixth Line of Text */
  strcat(final, buffer);
  if (info.color!=-999)
    {
      sprintf( buffer, "\nColor:   %d", info.color);
    }
  else
    {
      sprintf( buffer, "");
    }
  strcat(final, buffer);
  return QString(final);
}

MainWindow::MainWindow(QWidget * parent, Qt::WindowFlags flags):
    QMainWindow(parent, flags), qImage(NULL)
{
  this->setAutoFillBackground(false);
  this->setAttribute(Qt::WA_OpaquePaintEvent);
}

void MainWindow::actionTriggered(QAction *a) {
  Gpoint pointA;
  Gextent 			extent; 
  struct data_point   		info;
  struct  item_list   		*item=NULL;
  PyObject *canvas, *funcs, *func, *args, *kargs, *kval;
  int line;

  pointA.x = menupos.x();
  pointA.y = menupos.y();
  pointA.x = cnorm(this->vcs_legacy_obj, 0,(float)menupos.x());
  pointA.y = cnorm(this->vcs_legacy_obj, 1,(float)menupos.y());

  item = select_item(this->vcs_legacy_obj, pointA, NULL, NULL, pe_none);
  /* Initialize info struct */
  info.x=-999.;
  info.y=-999.;
  info.x_index=-999;
  info.y_index=-999;
  info.value=-999.;
  info.value2=-999.;
  info.color=-999;
  if ((item != NULL) && (item->type == pe_dsp)) {
    extent.ll.x = plnorm(0,item->data.pedsp->x1);
    extent.ll.y = plnorm(1,item->data.pedsp->y1);
    extent.lr.x = plnorm(0,item->data.pedsp->x2);
    extent.lr.y = plnorm(1,item->data.pedsp->y1);
    extent.ur.x = plnorm(0,item->data.pedsp->x2);
    extent.ur.y = plnorm(1,item->data.pedsp->y2);
    extent.ul.x = plnorm(0,item->data.pedsp->x1);
    extent.ul.y = plnorm(1,item->data.pedsp->y2);
    //           printextent(extent);
    if (within(pointA,extent)) {
      get_data_coords(this->vcs_legacy_obj, pointA, item, &info);
    }
  }
  PY_ENTER_THREADS
    PY_GRAB_THREAD
    canvas = getPyCanvas( this->vcs_legacy_obj->canvas_id );
  kargs = PyDict_New();
  if (info.x!=-999.)
    {
      kval = Py_BuildValue("d",info.x);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"datawc_x",kval);
  Py_DECREF(kval);
  
  if (info.y!=-999.)
    {
      kval = Py_BuildValue("d",info.y);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"datawc_y",kval);
  Py_DECREF(kval);
  
  if (info.value!=-999.)
    {
      kval = Py_BuildValue("d",info.value);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"value",kval);
  Py_DECREF(kval);
  if (info.value2!=-999.)
    {
      kval = Py_BuildValue("d",info.value2);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"value2",kval);
  Py_DECREF(kval);
  if (info.x_index!=-999)
    {
      kval = Py_BuildValue("i",info.x_index);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"index_x",kval);
  Py_DECREF(kval);
  if (info.y_index!=-999)
    {
      kval = Py_BuildValue("i",info.y_index);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"index_y",kval);
  Py_DECREF(kval);
  if (info.color!=-999.)
    {
      kval = Py_BuildValue("i",info.color);
    }
  else
    {
      Py_INCREF(Py_None);
      kval = Py_None;
    }
  PyDict_SetItemString(kargs,"color",kval);
  Py_DECREF(kval);
  
  kval = Py_BuildValue("i",menupos.x());
  PyDict_SetItemString(kargs,"XW_x",kval);
  Py_DECREF(kval);
  
  kval = Py_BuildValue("i",menupos.y());
  PyDict_SetItemString(kargs,"XW_y",kval);
  Py_DECREF(kval);
  
  PyDict_SetItemString(kargs,"canvas",canvas);
  
  funcs = PyObject_GetAttrString(canvas,"user_actions_names");
  if (PyList_Check(funcs)) 
    {
      for (line=0;line<PyObject_Length(funcs);line++) {
        if (strcmp(PyString_AsString(PyList_GetItem(funcs,line)),(const char *)a->text().toLatin1().data())==0) break;
      }
    }
  else line=0;
  Py_DECREF(funcs);      
  /* Set the line number as argument */
  args = Py_BuildValue("()",line);
  
  /* following is for direct call of func */
  funcs = PyObject_GetAttrString(canvas,"user_actions"); /* decref ? */
  if (PyList_Check(funcs))
    {
      func = PyList_GetItem(funcs,line);
      if (PyCallable_Check(func))
        {
          PY_RELEASE_THREAD
            PY_LEAVE_THREADS
            PY_ENTER_THREADS
            PY_GRAB_THREAD
            kval = PyEval_CallObjectWithKeywords(func,args,kargs);
          Py_DECREF(kargs);
          Py_DECREF(args);
          Py_XDECREF(kval);
          PY_RELEASE_THREAD
            PY_LEAVE_THREADS
            }
      else
        {
          PY_RELEASE_THREAD
            PY_LEAVE_THREADS
            return;
        }
    }
  else {
    PY_RELEASE_THREAD
      PY_LEAVE_THREADS
      return;
  }
}

bool MainWindow::event(QEvent *event)
{
  bool res = true;
  QEvent::Type etype;
  extern void event_handler(PyVCScanvas_Object *self,  QEvent *event);
  //fprintf(stderr,"in event received event: %i\n",event->type());

  /* events that are Qt specific */
  etype=event->type();
  //fprintf(stderr,"ok etpye is: %i\n",etype);
  if (event->type()==VCS_SHOW_EVENT) {
    this->raise();
    this->show();
  }
  // else if (event->type()==VCS_ANIMATION_CREATED_EVENT) {
  //   fprintf(stderr,"Ok we are done with anim on canvas: %i, %i\n",this->vcs_legacy_obj->canvas_id,this->vcs_legacy_obj->wkst_id);
  //   this->animationCreated(this->vcs_legacy_obj->canvas_id);
  // }
  else if (event->type()==VCS_PUT_IMAGE_EVENT) {
    this->image = ((QVCSEvent *)event)->data;
    this->qImage = NULL;
    this->repaint();
  }
  else if (event->type()==VCS_PUT_QIMAGE_EVENT) {
    this->image = NULL;
    this->qImage = (QImage*)((QVCSEvent *)event)->data;
    this->repaint();
  }
  else if (event->type()==VCS_REPAINT_EVENT) {
    this->repaint();
  }
  else if (event->type() == VCS_DIM_EVENT) {
    //     printf("getting geom %d\n", vcs_legacy_working);
    ((QVCSEvent *)event)->geom = this->geometry();
    //     printf("got geom\n");
  }
  else if (event->type() == VCS_MENU_EVENT) {
    QMenu *menu = new QMenu();
    QList<QString> strList;
    PyObject *canvas, *user_act_nms, *user_action_name;
    //char *astring;
    int i,nactions;

    connect(menu, SIGNAL(triggered(QAction*)), this, SLOT(actionTriggered(QAction*)));
    PY_ENTER_THREADS
      PY_GRAB_THREAD
      canvas  = getPyCanvas( this->vcs_legacy_obj->canvas_id );
    user_action_name = PyString_FromString("user_actions_names");
    user_act_nms = PyObject_GetAttr(canvas, user_action_name);
    Py_XDECREF(user_action_name);
    if PyList_Check(user_act_nms)
      {
        nactions=PyList_Size(user_act_nms);
        for (i=0;i<nactions;i+=1)
          {
            user_action_name = PyList_GetItem(user_act_nms,i);
            if PyString_Check(user_action_name) 
              {
                strList << QString(PyString_AsString(user_action_name));
              }
            else
              {
                strList << QString("Action %1").arg(i);
              }
          }
      }
    else
      {
        PyErr_SetString(PyExc_TypeError, "user_actions_names must be a list");
        return false;
      };
    Py_XDECREF(user_act_nms);
    PY_RELEASE_THREAD
      PY_LEAVE_THREADS

      for (int i=0; i<strList.size(); ++i)
        menu->addAction(strList[i]);
    menupos.setX(((QVCSEvent *)event)->point.x);
    menupos.setY(((QVCSEvent *)event)->point.y);
    menu->popup(QCursor::pos());
    menu->exec();
    disconnect(menu, SIGNAL(triggered(QAction*)), this, SLOT(actionTriggered(QAction*)));
    delete menu;
  }
  else if (event->type() == VCS_PROP_EVENT) {
    if (static_cast<QVCSEvent *>(event)->maxWidth!=-1)  this->setMaximumWidth(static_cast<QVCSEvent *>(event)->maxWidth);
    if (static_cast<QVCSEvent *>(event)->maxHeight!=-1) this->setMaximumHeight(static_cast<QVCSEvent *>(event)->maxHeight);
    if (static_cast<QVCSEvent *>(event)->minWidth!=-1)  this->setMinimumWidth(static_cast<QVCSEvent *>(event)->minWidth);
    if (static_cast<QVCSEvent *>(event)->minHeight!=-1) this->setMinimumHeight(static_cast<QVCSEvent *>(event)->minHeight);
    if (strcmp(static_cast<QVCSEvent *>(event)->icon,"")!=0) {
      QIcon ic(static_cast<QVCSEvent *>(event)->icon);
      this->setWindowIcon(ic);
    }
    if (strcmp(static_cast<QVCSEvent *>(event)->title,"")!=0) this->setWindowTitle(static_cast<QVCSEvent *>(event)->title);
  }
  else if (event->type()==VCS_HIDE_EVENT) {
    this->hide();
  }
  else if (event->type()==VCS_RESIZE_EVENT) {
    this->resize(static_cast<QVCSEvent *>(event)->geom.size());
    QVCSEvent* vces = static_cast<QVCSEvent *>(event);
    if (vces->geom.x()>=0 && vces->geom.y()>=0 && vces->geom.topLeft()!=this->pos()) {
      this->move(static_cast<QVCSEvent *>(event)->geom.topLeft());
    }
  }
  else if (event->type()==QEvent::Type(QEvent::Resize)) {
    //printf("here ? \n");
    vcs_legacy_acquire_update();
    this->setupCairo();
    vcs_legacy_release_update();

  }
  else if (event->type() == VCS_INFO_EVENT) {
    struct data_point info = static_cast<QVCSEvent *>(event)->info;
    QToolTip::showText(QCursor::pos(), prepare_info(info));
  }
  else if (event->type() == VCS_HIDE_INFO_EVENT) {
    QToolTip::hideText();
  }

  // int button_is_pressed = 0;
  // /* Events that are vcs_legacy mainloop */
  // switch (etype) {
  // case VCS_ButtonPress:
  //   button_is_pressed = 1;
  //   break;
  // case VCS_ButtonRelease:    
  //   button_is_pressed = 0;
  //   break;
  // default:
  //   break;
  // }
  switch (etype) {
  case VCS_ButtonPress:
  case VCS_ButtonRelease:
  case VCS_CirculateNotify:
  case VCS_ConfigureNotify:
  case VCS_CreateNotify:
  case VCS_ClientMessage:
  case VCS_DestroyNotify:
  case VCS_EnterNotify:
  case VCS_LeaveNotify:
  case VCS_FocusIn:
  case VCS_FocusOut:
  case VCS_Expose:
  case VCS_GraphicsExpose:
  case VCS_GravityNotify:
  case VCS_KeyPress:
  case VCS_KeyRelease:
  case VCS_MapNotify:
  case VCS_MotionNotify:
  case VCS_NoExpose:
  case VCS_ReparentNotify:
  case VCS_UnmapNotify:
  case VCS_RESIZE_EVENT:
  case QEvent::Type(QEvent::Resize):
    if (this->vcs_legacy_obj!=NULL) {
      if (this->vcs_legacy_obj->havexmainloop == 1) {
        acquire_global_lock();
        //         printf("sending event type %i to vcs_legacy, vs %i\n",event->type(),VCS_Expose);
        /* only do this if the xmainloop has been started by python */
        //         if (event->type() == VCS_ButtonPress) printf("vcs_legacy BP\n");
        //         if (event->type() == VCS_ButtonRelease) printf("vcs_legacy BR\n");
        event_handler(this->vcs_legacy_obj,event);
        release_global_lock();
        //         if (event->type() == VCS_ButtonPress) printf("back from vcs_legacy BP\n");
        //         if (event->type() == VCS_ButtonRelease) printf("back from vcs_legacy BR\n");
      }
    }
  break;
  default:
    break;
  }
  //   vcs_legacythreaddone.wakeAll();
  if (etype==VCS_ButtonPress || etype==VCS_ButtonRelease)
    res = true;
  else
    res = QMainWindow::event(event);

  //   if ( (etype==VCS_SHOW_EVENT) || (etype==VCS_DIM_EVENT) || (etype==VCS_DESKDIM_EVENT) || (etype==VCS_RESIZE_EVENT)|| (etype==VCS_PROP_EVENT) || (etype==VCS_INFO_EVENT) || (etype==VCS_HIDE_EVENT) || (etype==VCS_REPAINT_EVENT)) {
  //     ((MyThread *)((QVCSEvent *)event)->mythread)->done=true;
  //     vcs_legacy_working--;
  //   }
  if (dynamic_cast<QVCSBaseEvent*>(event)) {
    QVCSBaseEvent *ve = (QVCSBaseEvent *)event;
    if (ve->mutex && ve->cond) {
      ve->mutex->lock();
      ve->mutex->unlock();
      ve->cond->wakeAll();
    }
  }

  return res;
}


extern "C" CANVASINFO_LINK		head_canvas_info;
extern "C" void VCS2CAIRO_setrgb(cairo_t *cr,int color);

void MainWindow::setupCairo()
{
  CANVASINFO_LINK         cptr; /* connection ID pointers */
  //   while((updating>0)) {}; /* wait until vcs_legacy and Qt are done */
  vcs_legacy_acquire_update();
  QSize qs = this->size();
  /* first test if vcs_legacy is setup */
  if (this->vcs_legacy_obj==NULL) {
    vcs_legacy_release_update();
    return;
  }


  if (head_canvas_info!=NULL) {
    cptr = head_canvas_info;
    while ((cptr != NULL) && cptr->connect_id.cr != this->vcs_legacy_obj->connect_id.cr) {
      cptr = cptr->next;
    }
    if (cptr == NULL ) {
      vcs_legacy_release_update();
      return;
    }
  }
  else {
    vcs_legacy_release_update();
    return;
  }

  if (&(this->vcs_legacy_obj->connect_id) == NULL) {
    vcs_legacy_release_update();
    return;
  }

  if (this->vcs_legacy_obj->connect_id.cr!=NULL) {
    this->unsetupCairo();
  }
  /* only do this if it is not done yet */
  this->vcs_legacy_obj->connect_id.surface =   cairo_image_surface_create(CAIRO_FORMAT_ARGB32,qs.width(),qs.height());
  this->vcs_legacy_obj->connect_id.cr = cairo_create(this->vcs_legacy_obj->connect_id.surface);
  //fprintf(stderr,"ok surface and cairo setup as: %p, %p, on %i, size: %ix%i\n",this->vcs_legacy_obj->connect_id.surface,this->vcs_legacy_obj->connect_id.cr,this->vcs_legacy_obj->connect_id.wkst_id,qs.width(),qs.height());
#ifdef GENCAIRO
  fprintf(stderr,"surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32,%i,%i);\n",qs.width(),qs.height());
  fprintf(stderr,"cr = cairo_create(surface);\n");
#endif
  VCS2CAIRO_setrgb(this->vcs_legacy_obj->connect_id.cr,0);
  cairo_paint(this->vcs_legacy_obj->connect_id.cr);
#ifdef GENCAIRO
  fprintf(stderr,"cr = cairo_paint(cr);\n");
#endif

  if (head_canvas_info!=NULL) {
    cptr->connect_id.cr = this->vcs_legacy_obj->connect_id.cr;
  }
  connect_id=this->vcs_legacy_obj->connect_id;
  vcs_legacy_release_update();
  cairoIsSetup = 1;
}

void MainWindow::unsetupCairo()
{
  cairoIsSetup = 0;
  //   while((updating>0)) {}; /* wait until vcs_legacy and Qt are done */
  if (this->vcs_legacy_obj->connect_id.cr==NULL) {
    /* nothing to be done it is not here */
    vcs_legacy_release_update();
    return;
  }
  cairo_destroy(this->vcs_legacy_obj->connect_id.cr);
  cairo_surface_destroy(this->vcs_legacy_obj->connect_id.surface);
#ifdef GENCAIRO
  fprintf(stderr,"cairo_destroy(cr);\n");
  fprintf(stderr,"cairo_surface_destroy(surface);\n");
#endif
  //   printf("ok destroyed a cairo status is: %s\n",cairo_status_to_string(cairo_status(this->vcs_legacy_obj->connect_id.cr)));
  this->vcs_legacy_obj->connect_id.cr=NULL;
  this->vcs_legacy_obj->connect_id.surface=NULL;
}

void MainWindow::paintEvent(QPaintEvent *)
{
  QPainter painter;  
  QImage img2;
  vcs_legacy_acquire_update();
  painter.begin(this);
  if (this->image == NULL && this->qImage==NULL) {
    if (this->vcs_legacy_obj == NULL) {
      painter.end();
      vcs_legacy_release_update();
      return;
    };
    if (this->vcs_legacy_obj->connect_id.cr == NULL) {
      painter.end();
      vcs_legacy_release_update();
      return;
    };
    QSize qs = this->size();
    if (this->vcs_legacy_obj->connect_id.cr!=NULL) {
      
      /* Making sure we have the right dims */
      if (this->width()!=cairo_image_surface_get_width (this->vcs_legacy_obj->connect_id.surface)) {
        painter.end();
        vcs_legacy_release_update();
        return;
      }
      QImage cairo_generated_image( cairo_image_surface_get_data (this->vcs_legacy_obj->connect_id.surface),
                                    cairo_image_surface_get_width (this->vcs_legacy_obj->connect_id.surface) , 
                                    cairo_image_surface_get_height (this->vcs_legacy_obj->connect_id.surface),
                                    QImage::Format_ARGB32_Premultiplied);
      QPointF origin(0.,0.);
      painter.drawImage(origin,cairo_generated_image);
    }
  }
  else if (this->qImage==NULL) {
    painter.fillRect(0,0,this->geometry().width(),this->geometry().height(),Qt::white);
    QPointF origin(0.,0.);
    QImage img ((uchar *)this->image,this->geometry().width(),this->geometry().height(),QImage::Format_ARGB32_Premultiplied);
    painter.drawImage(origin,img);
    free(this->image);
    this->image = NULL;
  }
  else {
    QRectF viewArea(0,0,this->geometry().width(),this->geometry().height());
    painter.fillRect(viewArea, Qt::white);
    
    QSize sz = this->qImage->size();
    double R = this->geometry().width()/ this->geometry().height();
    if (R>1.) {
        img2 = this->qImage->scaledToHeight(this->geometry().height());
    } else {
        img2 = this->qImage->scaledToWidth(this->geometry().width());
    };

    //QSize sz2=img2.size();
    //QRectF viewArea2(0,0,sz2.width(),sz2.height());
    painter.drawImage(0,0,img2);

    //painter.drawImage(viewArea,*this->qImage);
    delete this->qImage;
    this->qImage = NULL;
  }
  painter.end();
  vcs_legacy_release_update();
}


// void MainWindow::animationCreated(int window) {
//   emit animationCreatedSignal(window);
// }
