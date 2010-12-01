import vcdatWindow

def main():
    # simple.py

    import sys
    from PyQt4 import QtGui

    app = QtGui.QApplication(sys.argv)


    cdat = vcdatWindow.QCDATWindow()
    cdat.show()
    
    sys.exit(app.exec_())
    

if __name__=="__main__":
    main()

