%Module cdatguiwrap 0

%Import QtGui/QtGuimod.sip

class MainWindow : QMainWindow {
%TypeHeaderCode
#include <Qt/mainwindow.h>
%End

public:
  void paintEvent(QPaintEvent *);
  virtual bool event(QEvent *);
  void setupCairo();
  void unsetupCairo();

public slots:
  void actionTriggered(QAction*);
};

class VCSQtManager {

%TypeHeaderCode
#include <Qt/qti.h>
%End

public:
  static bool isCreated();
  static MainWindow* window(int id);
  static void sendEvent(int index, QEvent::Type eventType);

public slots:

signals:
  
};

