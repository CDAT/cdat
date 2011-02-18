import vcdatWindow,cdms2,os,argparse,sys
from PyQt4 import QtCore,QtGui
import customizeVCDAT

def main():
    # simple.py

    parser = argparse.ArgumentParser(description = "VCDAT")
    d = parser.add_argument_group("Data")
    d.add_argument("file",help="file to open at start time",nargs="?")
    d.add_argument("-f,--file",dest="file",help="file to open at start time")
    d.add_argument("var",help="variable to load at start time (you need a file arg as well)",nargs="?")
    d.add_argument("-v,--var",dest="var",help="variable to load at start time (you need a file arg as well)",nargs="?")
    g = parser.add_argument_group("GUI")
    g.add_argument("-t,--tab",dest="tab",help="tab to select at start time",choices=["var","gm","cmd","calc"])
    g.add_argument("-g,--graphicsmethod",dest="gm",help="graphics method type to select at start time",choices=customizeVCDAT.plotTypes)
    n = parser.parse_args(sys.argv[1:])
    app = QtGui.QApplication(sys.argv)
    ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'share','icons')
    icon = QtGui.QIcon(os.path.join(ICONPATH, "UV-CDAT_logo_sites.gif"))
    app.setWindowIcon(icon)


    cdat = vcdatWindow.QCDATWindow()

    if n.file is not None:
        cdat.tabView.widget(0).fileWidget.widget.fileNameEdit.setText(n.file)
        cdat.tabView.widget(0).fileWidget.widget.fileNameEdit.emit(QtCore.SIGNAL('returnPressed()'))
        if n.var is not None:
            index=-1
            for i in range(cdat.tabView.widget(0).fileWidget.widget.varCombo.count()):
                t = cdat.tabView.widget(0).fileWidget.widget.varCombo.itemText(i)
                if n.var == str(t).split()[0]:
                    index = i
                    break
            if index!=-1:
                cdat.tabView.widget(0).fileWidget.widget.varCombo.setCurrentIndex(index)
    if n.tab in ["gm",]:
        cdat.tabView.setCurrentIndex(1)
    elif n.tab in ["cmd",]:
        cdat.tabView.setCurrentIndex(2)
    elif n.tab in ["calc",]:
        cdat.tabView.setCurrentIndex(3)

    for i in range(cdat.tabView.widget(1).plotOptions.plotTypeCombo.count()):
        if n.gm == str(cdat.tabView.widget(1).plotOptions.plotTypeCombo.itemText(i)):
            cdat.tabView.widget(1).plotOptions.plotTypeCombo.setCurrentIndex(i)
            
    sys.exit(app.exec_())
    

if __name__=="__main__":
    main()

