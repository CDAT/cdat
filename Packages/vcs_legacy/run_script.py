from PyQt4 import QtGui, QtCore
import os, sys, vcs

class ExecThread(QtCore.QThread):
    def run(self):
       sys.path.append(os.path.dirname(sys.argv[1]))
       execfile(sys.argv[1])
       QtCore.QCoreApplication.exit(0)

if __name__=="__main__":
    vcs.initQt()
    thr = ExecThread()
    thr.start()
    QtGui.QApplication.instance().exec_()
