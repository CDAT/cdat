import vcdatWindow,cdms2,os
from PyQt4 import QtCore

def main():
    # simple.py

    import sys
    from PyQt4 import QtGui

    app = QtGui.QApplication(sys.argv)
    ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'share','icons')
    icon = QtGui.QIcon(os.path.join(ICONPATH, "UV-CDAT_logo_sites.gif"))
    app.setWindowIcon(icon)


    cdat = vcdatWindow.QCDATWindow()

    if len(sys.argv)>1:
        fnm = sys.argv[1]
        cdat.tabView.widget(0).fileWidget.widget.fileNameEdit.setText(fnm)
        cdat.tabView.widget(0).fileWidget.widget.fileNameEdit.emit(QtCore.SIGNAL('returnPressed()'))
        if len(sys.argv)>2:
            var = sys.argv[2]
            index=-1
            for i in range(cdat.tabView.widget(0).fileWidget.widget.varCombo.count()):
                t = cdat.tabView.widget(0).fileWidget.widget.varCombo.itemText(i)
                if var == str(t).split()[0]:
                    index = i
                    break
            if index!=-1:
                cdat.tabView.widget(0).fileWidget.widget.varCombo.setCurrentIndex(index)

    sys.exit(app.exec_())
    

if __name__=="__main__":
    main()

