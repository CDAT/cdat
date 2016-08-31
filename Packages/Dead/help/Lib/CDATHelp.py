import gzip
import os
import pickle
import string
import subprocess
import sys
import thread
import time

import HelpStdout
import ProgressBar
import SearchTable
from ProgressBar import ProgressBarCancelledException

class Version:
    def __init__(self,major,minor=0,rev=0):
        self.major = major
        self.minor = minor
        self.rev = rev
    def __cmp__(self,other):
        if self.major > other.major: return  1
        if self.major < other.major: return -1
        if self.minor > other.minor: return  1
        if self.minor < other.minor: return -1
        if self.rev   > other.rev:   return  1
        if self.rev   < other.rev:   return -1
        return 0
    
class CDATHelp:

    def __init__(self,progressBar):
        self.clear()
        self.__progressBar = progressBar
        self.__version = Version(4,3,1)
        self.__savedVersion = self.__version
        
    def clear(self):
        self.__depth = 0
        self.__indexList = []
        self.__searchTable = SearchTable.SearchTable()
        
    def build(self,depth):
        try:
            self.__progressBar.deiconify()
            self.__depth = depth
            self.__buildIndexList()
            self.__buildSearchTable()
            self.__progressBar.withdraw()
            self.__status('words %d ids %d'%(self.__searchTable.getWordCount(),self.__searchTable.getIdCount()))
        except ProgressBarCancelledException:
            self.__progressBar.withdraw()
            pass
            
    def getIndexList(self):
        return self.__indexList

    def getSearchTable(self):
        return self.__searchTable

    def getDepth(self):
        return self.__depth
    
    def load(self,fname):

        # Bail if file doesn't exist.
        if not os.path.exists(fname):
            return False

        # Bail if you can't open the file.
        try:
            source = open(fname,'rb')
        except:
            return False

        # Bail if the version is not current.
        try:
            self.__savedVersion = pickle.load(source)
        except:
            self.__savedVersion = Version(0)
        if self.__savedVersion != self.__version:
            self.__savedVersion = self.__version
            return False

        try:
            self.__progressBar.deiconify()
            self.__updateProgress('Loading index list', 0.25)
            self.__depth = pickle.load(source)
            self.__indexList = pickle.load(source)
            self.__updateProgress('Loading search table', 0.5)
            self.__searchTable = pickle.load(source)
            self.__updateProgress('Closing file %s'%fname, 0.75)
            source.close()
            self.__updateProgress('Done loading', 1.0)
            self.__progressBar.withdraw()
        except ProgressBarCancelledException:
            self.__progressBar.withdraw()
            pass
        
        return True
    
    def dump(self,fname):
        try:
            output = open(fname,'wb')
        except:
            return

        try:
            self.__progressBar.deiconify()
            pickle.dump(self.__savedVersion,output)
            self.__updateProgress('Saving index list', 0.25)
            pickle.dump(self.__depth,output)
            pickle.dump(self.__indexList,output)
            self.__updateProgress('Saving search table', 0.50)                           
            pickle.dump(self.__searchTable,output)
            self.__updateProgress('Closing file', 0.75)
            output.close()
            self.__updateProgress('Done saving', 1.0)
            self.__progressBar.withdraw()
        except ProgressBarCancelledException:
            self.__progressBar.withdraw()
            pass
        
    def __updateProgress(self,text,ratio):
        if self.__progressBar != None:
            self.__progressBar.update(text,ratio)
                    
    def __status(self,text):
        #print 'CDATHelp:', text
        pass
        
    def __buildIndexList(self):

        # Build a list of module names to participath in CDAT Help.
        ignores = [ 'help',  ]  # Names of modules to ignore.
        modules = []            # Result list of of collected modules.
        for ii in sys.path:
            if not os.path.isdir(ii):
                continue
            for jj in os.listdir(ii):
                if jj in modules:
                    continue
                if jj in ignores:
                    continue
                full = os.path.join(ii,jj)
                if not os.path.isdir(full):
                    continue
                modules.append(jj)
        modules.sort()

        self.__progressBar.deiconify()
        count = 0.0
        for ii in modules:
            count += 1
            ratio = count/len(modules)
            self.__updateProgress('Build step 1 of 2: Building index list for %s'%ii,ratio)
            self.__status('Building index list for %s'%ii)
            self.__buildIndex(ii)
        self.__indexList.sort(self.__compareLowercase)

    def __buildIndex(self,name):
        self.__status('%d name=%s'%(self.__depth,name))
        depth = name.count('.') + 1
        if depth > self.__depth:
            return

        try:
            first = string.split(name,'.')[0]
            exec 'import %s'%first
            exec 'dd = dir(%s)'%name
        except:
            return

        self.__indexList.append(name)
        for ii in dd:
            try:
                if ii[0] == '_':
                    continue
            except:
                # Skip unsubscriptable objects.
                continue
            
            if ii[:3] == 'im_':
                continue
            if ii[:5] == 'func_':
                continue
            sub = '%s.%s'%(name,ii)
            self.__buildIndex(sub)
        
    def __buildSearchTable(self):

        count = 0.0
        for name in self.__indexList:
            count += 1
                        
            depth = string.count(name,'.') + 1                        
            if(depth < 3):
                ratio = count/len(self.__indexList)
                self.__updateProgress('Build step 2 of 2: Building search table for %s'%name,ratio)
                #self.__status('Building search table for %s'%name)

            self.__searchTable.add(name)

        self.__status('Index size before fiter: %d'%len(self.__indexList))
        self.__searchTable.filter()
        self.__indexList = self.__searchTable.getAllNames()
        self.__status('Index size after fiter:  %d'%len(self.__indexList))
        self.__indexList.sort(self.__compareLowercase)
        
    def __compareLowercase(self, a, b):
        aa = string.lower(a)
        bb = string.lower(b)
        if aa > bb:
            return 1
        if aa== bb:
            return 0
        if aa < bb:
            return -1
