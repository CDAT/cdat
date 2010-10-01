import cdat_window

def main():
    # simple.py

    import sys
    from PyQt4 import QtGui

    app = QtGui.QApplication(sys.argv)

    widget = QtGui.QWidget()
    widget.resize(250, 150)
    widget.setWindowTitle('simple')
    widget.show()

    cdat = cdat_window.QCDATWindow()
    #cdat.resize(250, 150)
    cdat.show()
    
    sys.exit(app.exec_())
    

if __name__=="__main__":
    main()

