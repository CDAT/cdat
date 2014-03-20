#include "Qt/qti.h"
#include "Qt/mainwindow.h"

//======================== VCSQtManager ========================

MainWindow* pMainWindows[10] = {};

bool VCSQtManager::isCreated() {
  return pMainWindows[0]!=NULL;
}

static void createWindows() {
  for (size_t i=0; i<sizeof(pMainWindows)/sizeof(MainWindow*); i++) {
    pMainWindows[i] = new MainWindow();
    pMainWindows[i]->vcs_legacy_obj = NULL;
    pMainWindows[i]->image = NULL;
  }
}

void VCSQtManager::createCanvases() {
  if (VCSQtManager::owningApp()) {
    VCSQtManager::sendEvent(VCSQtManager::app(),
                            new QDeferredExecutionEvent((void*)createWindows));
  }
  else {
    // We are not owning the main app, so create the windows and move
    // them to the main thread
    createWindows();
    for (size_t i=0; i<sizeof(pMainWindows)/sizeof(MainWindow*); i++)
      pMainWindows[i]->moveToThread(VCSQtManager::app()->thread());
  }
}
  
void VCSQtManager::sendEvent(int index, QEvent::Type eventType) {
  QVCSEvent *event = new QVCSEvent(eventType);
  VCSQtManager::sendEvent(index, event);
}

void VCSQtManager::sendEvent(int index, QVCSBaseEvent *event) {
  event->mutex = NULL;
  event->cond = NULL;
  if (QThread::currentThread()==VCSQtManager::window(index)->thread()) {
    QCoreApplication::sendEvent(VCSQtManager::window(index),event);
  }
  else {
    if (event->isSpontaneous || VCSQtManager::owningApp()) {
      QMutex *mutex = new QMutex();
      QWaitCondition *cond = new QWaitCondition();
      event->mutex=mutex;
      event->cond=cond;
      mutex->lock();
      QCoreApplication::postEvent(VCSQtManager::window(index),event);
      cond->wait(mutex);
      delete mutex;
      delete cond;
    }
    else {
      QCoreApplication::postEvent(VCSQtManager::window(index),event);
    }
  }
  //sendEvent(VCSQtManager::window(index), event);
}

void VCSQtManager::sendEvent(QObject *window, QVCSBaseEvent *event) {
  if (QThread::currentThread()==window->thread()) {
    event->mutex = NULL;
    event->cond = NULL;
    QCoreApplication::sendEvent(window, event);
  }
  else {
    QMutex *mutex = new QMutex();
    QWaitCondition *cond = new QWaitCondition();
    event->mutex = mutex;
    event->cond = cond;
    
    mutex->lock();
    QCoreApplication::postEvent(window, event);
    cond->wait(mutex);
    
    delete mutex;
    delete cond;
    event->mutex = NULL;
    event->cond = NULL;
  }
}
