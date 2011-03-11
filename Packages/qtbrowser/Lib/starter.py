import vcdatWindow,cdms2,os,argparse,sys
from PyQt4 import QtCore,QtGui
import customizeVCDAT

def main():
    # simple.py

    parser = argparse.ArgumentParser(description = "VCDAT")
    d = parser.add_argument_group("Data")
    d.add_argument("file",help="file to open at start time",nargs="?")
    d.add_argument("-f",dest="file2",help="file to open at start time",metavar="FILE")
    d.add_argument("--file",dest="file2",help="file to open at start time",metavar="FILE")
    d.add_argument("var",help="variable to select at start time (you need a file arg as well)",nargs="?")
    d.add_argument("-v",dest="var1",help="variable to select at start time (you need a file arg as well)",metavar="VAR")
    d.add_argument("-v2",dest="var2",help="variable to load at start time (you need a file arg as well)",metavar="VAR2")
    d.add_argument("--var",dest="var1",help="variable to select at start time (you need a file arg as well)",metavar="VAR")
    d.add_argument("--var2",dest="var2",help="variable to load at start time (you need a file arg as well)",metavar="VAR2")
    d.add_argument("-l",help="load variable(s) into memory (you need a variable)",dest="load_var",action="store_true",default=False)
    g = parser.add_argument_group("GUI")
    g.add_argument("-t,--tab",dest="tab",help="tab to select at start time",choices=["var","gm","cmd","calc"])
    g.add_argument("-g,--graphicsmethod",dest="gm",help="graphics method type to select at start time",choices=customizeVCDAT.plotTypes)
    n = parser.parse_args(sys.argv[1:])
    app = QtGui.QApplication(sys.argv)
    ICONPATH = os.path.join(cdms2.__path__[0], '..', '..', '..', '..', 'share','icons')
    icon = QtGui.QIcon(os.path.join(ICONPATH, "UV-CDAT_logo_sites.gif"))
    app.setWindowIcon(icon)


    cdat = vcdatWindow.QCDATWindow()


    ifile = None
    if n.file is not None:
        ifile = n.file
    if n.file2 is not None:
        ifile = n.file2
    
    if ifile is not None:
        cdat.tabView.widget(0).fileWidget.widget.fileNameEdit.setText(ifile)
        cdat.tabView.widget(0).fileWidget.widget.fileNameEdit.emit(QtCore.SIGNAL('returnPressed()'))
        if n.var is not None or n.var1 is not None:
            for vrnm in [n.var1,n.var2]:
                index=-1
                for i in range(cdat.tabView.widget(0).fileWidget.widget.varCombo.count()):
                    t = cdat.tabView.widget(0).fileWidget.widget.varCombo.itemText(i)
                    if vrnm==n.var1:
                        vnms=[n.var,n.var1]
                    else:
                        vnms=[n.var2,]
                    if str(t).split()[0] in vnms:
                        index = i
                        break
                if index!=-1:
                    cdat.tabView.widget(0).fileWidget.widget.varCombo.setCurrentIndex(index)
                    if n.load_var:
                        cdat.tabView.widget(0).fileWidget.widget.defineVarButton.emit(QtCore.SIGNAL("clicked(bool)"),True)
    if n.tab in ["gm",]:
        cdat.tabView.setCurrentIndex(1)
    ## elif n.tab in ["cmd",]:
    ##     cdat.tabView.setCurrentIndex(2)
    elif n.tab in ["calc",]:
        cdat.tabView.setCurrentIndex(2)

    for i in range(cdat.tabView.widget(1).plotOptions.plotTypeCombo.count()):
        if n.gm == str(cdat.tabView.widget(1).plotOptions.plotTypeCombo.itemText(i)):
            cdat.tabView.widget(1).plotOptions.plotTypeCombo.setCurrentIndex(i)

    sys.exit(app.exec_())
    

if __name__=="__main__":
    main()

