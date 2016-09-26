#####################################################################
#                                                                   #
#  File:  SearchTable.py                                            #
#  Date:  29-Nov-2007                                               #
#                                                                   #
#####################################################################

import re
import string
import sys
import HelpStdout

class SearchTable:

    def __init__(self):

        self.__idNameTable = {}
        self.__wordIdTable = {}

    def add(self, name):
        ''' Add the name to the search table.'''

        # Import the module.
        first = string.split(name,'.')[0]
        try:
            exec 'import %s'%first
        except:
            pass

        # Retrieve the object id.
        exec 'objId = id(%s)'%name

        # Add the name to the id/name table.
        hadId = True
        if not self.__idNameTable.has_key(objId):
            hadId = False
            self.__idNameTable[objId] = []
        self.__idNameTable[objId].append(name)

        # If the id already exists, it means that the same
        # object exists for another name and the word search
        # table had been already generated. Therefore, we
        # can bail.
        if hadId:
            #self.status('Id old %s'%name)
            return
        #self.status('Id new %s'%name)

        # Builtin function help() writes to stdout,
        # so let's redefine stdout to capture and retrieve text.
        old = sys.stdout
        helpStdout = HelpStdout.HelpStdout()
        sys.stdout = helpStdout
        
        result = None
        try:
            cmd = 'result = %s'%name
            exec cmd
            if result != None:
                cmd = 'help(%s)'%name
                exec cmd
        except:
            pass

        # Restore stdout and retrieve captured text.
        sys.stdout = old
        if result == None:
            return
        helpStdout.split()
        doc = helpStdout.get()


        # Parse the document and generate the word search table.
        for ii in range(len(doc)):

            # Strip off whitespace.
            line = doc[ii].strip()

            #self.status('line %s'%line)
            
            for word in re.split('\W',line):
                if not len(word):
                    continue
                #self.status('word %s'%word)
                
                # Search is case-insensitive.
                word = string.lower(word)

                # If it's a new word, create the list and append.
                if not self.__wordIdTable.has_key(word):
                    self.__wordIdTable[word] = []
                    self.__wordIdTable[word].append(objId)

                # If the word is not new, append only if the
                # object id is not already present.
                elif objId not in self.__wordIdTable[word]:
                    self.__wordIdTable[word].append(objId)
                    
    def getOne(self,word):
        '''Retrieves a sorted list of names for the given word.''' 

        if not self.__wordIdTable.has_key(word):
            return []        
        ids = self.__wordIdTable[word]
        result = []
        for ii in ids:
            names = self.__idNameTable[ii]
            result += names
        result.sort()
        return result

    def get(self,searchString):
        '''Returns a dictionary where:
        key = Result of search entry.
        val = List of matching search words.
        '''
        result2search = {}
        for word in string.split(searchString):
            self.status('Lookup %s'%word)
            found = self.getOne(word)
            for key in found:
                if key not in result2search.keys():
                    result2search[key] = []
                result2search[key].append(word)
        return result2search
    
    
    def filter(self):
        '''Filters redundant objects from the list.'''

        # Compress the list of names for each id down
        # to the shortest name.
        for key in self.__idNameTable.keys():
            vals = self.__idNameTable[key]
            shortest = ''
            for val in vals:
                if shortest == '':
                    shortest = val
                elif len(val) < len(shortest):
                    shortest = val
            self.__idNameTable[key] = []
            self.__idNameTable[key].append(shortest)
        #self.dumpWords('words.txt')
        
    def getAllNames(self):
        '''Retrieves a list of names in the table.'''
        result = []
        for list in self.__idNameTable.values():
            result.append(list[0])
        return result
    
    def getWordCount(self):
        '''Retrieves the number of words in the table.'''
        return len(self.__wordIdTable)

    def getIdCount(self):
        '''Retrieves the number of object ids in the table.'''
        return len(self.__idNameTable)

    def dumpWords(self,fname):
        f=open(fname,'w')
        for word in self.__wordIdTable.keys():
            f.write('%s\n'%word)
        f.close()
            
    def status(self,text):
        #print 'SearchTable: %s'%text
        pass
