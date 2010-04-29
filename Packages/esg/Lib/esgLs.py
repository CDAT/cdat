#!/usr/bin/env python
"""
ESG file list markup.
"""

from esg import File
from esgParse import ESGParseException, esgErrorHandler
from xml.sax import make_parser, ContentHandler
import string

def loadls(path):
    """Create a list of ESG File nodes from an XML file path."""
    parser = make_parser()
    ch = esgLsContentHandler()
    eh = esgErrorHandler()
    parser.setContentHandler(ch)
    parser.setErrorHandler(eh)
    parser.parse(path)
    return ch.root

def _getatt(attrs, key, default=None):
    if attrs.has_key(key):
        result = attrs[key].encode()
    else:
        result = default
    return result

class esgLsContentHandler(ContentHandler):

    def __init__(self):
        self.root = []                  # Result list

    def setDocumentLocator(self, locator):
        self._locator = locator

    def startElement(self, name, attrs):
        ename = name.encode()
        if ename=='file':
            fobj = self.startFile(attrs)
            self.root.append(fobj)
        elif ename=='list':
            pass
        else:
            raise ESGParseException("Invalid tag: %s"%ename, None, self._locator)

    def startFile(self, attrs):
        path = _getatt(attrs, 'path')
        if path is None:
            raise ESGParseException("No path specified", None, self._locator)
        ssize = _getatt(attrs, 'size')
        if ssize is None:
            size=None
        else:
            size = string.atoi(ssize)
        fobj = File('<None>', name=path, size=size)
        return fobj

if __name__=='__main__':
    import sys

    filelist = loadls("../Test/filelist.xml")
    for f in filelist:
        print f.name, f.size
