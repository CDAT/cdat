#!/usr/bin/env python


import sys

fnm = sys.prefix+"/Resources/Python.app/Contents/Info.plist"

f=open(fnm)
s=f.read()
pat="<key>CFBundleName</key>"
i=s.find(pat)#<string>Python</string>")
s2=s[:i+len(pat)]+s[i+len(pat):].replace("Python","UV-CDAT",1)
f=open(fnm,'w')
f.write(s2)
f.close()
