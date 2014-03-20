#ifndef MAINWINDOW_H
#define MAINWINDOW_H
#include <QtCore/QCustomEvent>
#include <QtCore/QEvent>
#include <QtCore/QMutex>
#include <QtCore/QThread>
#include <QtCore/QWaitCondition>
#include <QtGui/QMainWindow>
#include <pyvcs_legacy.h>
#include "xgks.h"
#include "vcs_legacy_events.h"
#include "qti.h"

#define VCS_SHOW_EVENT QEvent::Type(QEvent::User+1)
#define VCS_HIDE_EVENT QEvent::Type(QEvent::User+2)
#define VCS_PROP_EVENT QEvent::Type(QEvent::User+3)
#define VCS_REPAINT_EVENT QEvent::Type(QEvent::User+4)
#define VCS_DIM_EVENT QEvent::Type(QEvent::User+5)
#define VCS_MENU_EVENT QEvent::Type(QEvent::User+6)
#define VCS_RESIZE_EVENT QEvent::Type(QEvent::User+7)
#define VCS_INFO_EVENT QEvent::Type(QEvent::User+8)
#define VCS_HIDE_INFO_EVENT QEvent::Type(QEvent::User+9)
#define VCS_PUT_IMAGE_EVENT QEvent::Type(QEvent::User+10)
#define VCS_PUT_QIMAGE_EVENT QEvent::Type(QEvent::User+11)
/* #define VCS_ANIMATION_CREATED_EVENT QEvent::Type(QEvent::User+12) */

#define VCS_UNDEFINED_EVENT_1 QEvent::Type(QEvent::User+101)
#define VCS_UNDEFINED_EVENT_2 QEvent::Type(QEvent::User+102)
#define VCS_UNDEFINED_EVENT_3 QEvent::Type(QEvent::User+103)
#define VCS_UNDEFINED_EVENT_4 QEvent::Type(QEvent::User+104)
#define VCS_UNDEFINED_EVENT_5 QEvent::Type(QEvent::User+105)
#define VCS_UNDEFINED_EVENT_6 QEvent::Type(QEvent::User+106)
#define VCS_UNDEFINED_EVENT_7 QEvent::Type(QEvent::User+107)
#define VCS_UNDEFINED_EVENT_8 QEvent::Type(QEvent::User+108)
#define VCS_UNDEFINED_EVENT_9 QEvent::Type(QEvent::User+109)
#define VCS_UNDEFINED_EVENT_10 QEvent::Type(QEvent::User+110)
#define VCS_UNDEFINED_EVENT_11 QEvent::Type(QEvent::User+111)
#define VCS_UNDEFINED_EVENT_12 QEvent::Type(QEvent::User+112)

class MainWindow : public QMainWindow
{
  Q_OBJECT
public:
  MainWindow(QWidget * parent=0, Qt::WindowFlags flags=0);
  
  void paintEvent(QPaintEvent *);
  virtual bool event(QEvent *);
  PyVCScanvas_Object *vcs_legacy_obj;
  void setupCairo();
  void unsetupCairo();
  void *image;
  QImage *qImage;
public slots:
  void actionTriggered(QAction*);
  /*   void animationCreated(int window); */
  /* signals: */
  /*   void animationCreatedSignal(int window); */
};

class QVCSEvent : public QVCSBaseEvent
{
public:
  QVCSEvent(QEvent::Type type, bool _isSpontaneous=false)
      : QVCSBaseEvent(type, _isSpontaneous) {}
  QRect geom;
  int minWidth;
  int minHeight;
  int maxWidth;
  int maxHeight;
  char icon[1024];
  char title[1024];
  struct data_point info;
  Gpoint point;
  void *data;
};

extern "C" void vcs_legacy_acquire_update();

extern "C" void vcs_legacy_release_update();


#endif
  
