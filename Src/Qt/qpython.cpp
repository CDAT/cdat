#include <stdio.h>
#include <stdlib.h>
#include <python2.6/Python.h>
#include "qti.h"

int main(int argc, char **argv) {
#ifdef __APPLE__
  QPythonApp app(argc, argv);
  VCSSecondaryThread mythread(argc, argv);
  mythread.start();
  return app.exec();
#else
  return Py_Main(argc, argv);
#endif
}
