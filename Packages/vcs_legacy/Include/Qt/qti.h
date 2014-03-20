#ifndef QTI_H
#define QTI_H
#include <QtCore/QEvent>
#include <QtCore/QMutex>
#include <QtCore/QThread>
#include <QtCore/QWaitCondition>
#include <QtGui/QApplication>
#ifdef __APPLE__
#include <Python.h>
#endif
#include <cstdio>
#include <pthread.h>


//======================== Events ========================
class QVCSBaseEvent : public QEvent {
public:
  QVCSBaseEvent(QEvent::Type type, bool _isSpontaneous=false): QEvent(type), isSpontaneous(_isSpontaneous) {}
  QMutex *mutex;
  QWaitCondition *cond;
  bool isSpontaneous;
};

#define VCS_DEFERRED_EXECUTION QEvent::Type(QEvent::User+0x828)
#define VCS_EVENT_LOOP_STARTED QEvent::Type(QEvent::User+0x829)
class QDeferredExecutionEvent : public QVCSBaseEvent {
public:
  QDeferredExecutionEvent(void *f=NULL): QVCSBaseEvent(VCS_DEFERRED_EXECUTION)
  {
    this->function = f;
  }
  void *function;
};

//======================== QPythonApp ========================
class QPythonApp : public QApplication {
public:
  
  QPythonApp(int &argc, char **argv): QApplication(argc, argv)
  {
    // We are using the desktop widget later on, in other threads,
    // thus, we want to pre-create the widget here.
    QApplication::desktop();
  }
  
  virtual bool event(QEvent *e) {
    if (e->type()==VCS_DEFERRED_EXECUTION) {
      QDeferredExecutionEvent *ve = (QDeferredExecutionEvent*)e;
      ((void(*)())(ve->function))();
      if (ve->mutex && ve->cond) {
        ve->mutex->lock();
        ve->mutex->unlock();
        ve->cond->wakeAll();
      }
      return true;
    }
    else if (e->type()==VCS_EVENT_LOOP_STARTED) {
      QVCSBaseEvent *ve = (QVCSBaseEvent*)e;
      if (ve->mutex && ve->cond) {
        ve->mutex->lock();
        ve->mutex->unlock();
        ve->cond->wakeAll();
      }
      return true;
    }
    else
      return QApplication::event(e);
  }

};

//======================== VCSSecondaryThread ========================
class VCSSecondaryThread {
public:
  VCSSecondaryThread(int argc, char **argv) {
    this->argc = argc;
    this->argv = argv;
#ifndef __APPLE__
    this->eventLoopReadyMutex = new QMutex();
    this->eventLoopReadyCond  = new QWaitCondition();
#endif
  }

  void start() {
#ifndef __APPLE__
    this->eventLoopReadyMutex->lock();
#endif
    pthread_t tid;
    pthread_create(&tid, NULL, run, this);
#ifndef __APPLE__
    this->eventLoopReadyCond->wait(this->eventLoopReadyMutex);
#endif
  }
  
  static void *run(void *data) {
    int i;
#ifdef __APPLE__
    VCSSecondaryThread *thrd = (VCSSecondaryThread*)data;
    char **largv = (char**)malloc(thrd->argc*sizeof(char*));
    for (i=0; i<thrd->argc; i++)
      largv[i] = thrd->argv[i];
    int r = Py_Main(thrd->argc, largv);
    free(largv);
    QApplication::exit(r);
#else
    VCSSecondaryThread *thrd = (VCSSecondaryThread*)data;
    QPythonApp *app = new QPythonApp(thrd->argc, thrd->argv);
    QVCSBaseEvent *e = new QVCSBaseEvent(VCS_EVENT_LOOP_STARTED);
    e->mutex = thrd->eventLoopReadyMutex;
    e->cond = thrd->eventLoopReadyCond;
    app->postEvent(app, e);
    app->exec();
#endif
    return NULL;
  }
  
  int argc;
  char **argv;
#ifndef __APPLE__
  QMutex         *eventLoopReadyMutex;
  QWaitCondition *eventLoopReadyCond;
#endif
};

//======================== VCSQtManager ========================
class MainWindow;

typedef void VoidFunction(void);

class VCSQtManager {
public:
  static void createCanvases();
  static MainWindow* window(int id);
  
  static QApplication* app();
  
  static bool owningApp();
  
  static void sendEvent(int index, QEvent::Type eventType);
  static void sendEvent(int index, QVCSBaseEvent *event);
  static void sendEvent(QObject *, QVCSBaseEvent *event);

  static void executeDeferred(VoidFunction *function);
  
  static bool isCreated();
};

extern MainWindow* pMainWindows[10];
inline MainWindow* VCSQtManager::window(int id) {
  return pMainWindows[id];
}

inline QApplication* VCSQtManager::app() {
  return static_cast<QApplication*>(QCoreApplication::instance());
}

inline bool VCSQtManager::owningApp() {
  return dynamic_cast<QPythonApp*>(VCSQtManager::app())!=NULL;
}

inline void VCSQtManager::executeDeferred(VoidFunction *function) {
  if (VCSQtManager::owningApp()) {
    VCSQtManager::sendEvent((QObject*)(VCSQtManager::app()), new QDeferredExecutionEvent((void*)function));
  } else {
    fprintf(stderr, "CDAT: Cannot perform deferred execution for QApplication created by modules other than vcs_legacy\n");
  }
}

#endif
