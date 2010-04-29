#####################################################################
#####################################################################
#                                                                   #
#  File:  ResultsFrame.py                                           #
#  Date:  31-Oct-2007                                               #
#  Desc:  Frame to display results of user query for CDAT help.     #
#                                                                   #
#####################################################################
#####################################################################

import Tkinter
import Pmw
import string

class ResultsFrame (Pmw.ScrolledText):

    def __init__ (self, master):

        Pmw.ScrolledText.__init__(self,
                                  master,
                                  text_wrap='none',
                                  )
        self.component('text').configure(background='white')

    def status(self,text):
        print 'ResultsFrame: %s'%text
        pass
    
    def set(self,lines,searchWords=''):
        searchWords = string.lower(searchWords)
        self.clear()
        self.configure(text_state = 'normal')
        
        # Collect highlight tags.
        tagList=[]  
        for word in string.split(searchWords):
            #self.status('Collecting tags, word %s'%word)
            row = 0
            for line in lines:
                #self.status('Line %s'%line)
                row += 1
                cols = []
                found = 0
                lowerLine = string.lower(line)
                while found != -1:
                    found = string.find(lowerLine, word, found+1)
                    if found != -1:
                        cols.append(found)
                for col in cols:
                    tagList.append('%d.%d'%(row,col))
                    tagList.append('%d.%d'%(row,col+len(word)))

        # Add lines to self frame.
        for line in lines:
            self.insert('end','%s\n'%line)
        
        # Highlight the search text.
        if len(searchWords) > 0:
            self.tag_configure('highlight', background='yellow')
            apply (self.tag_add, ('highlight',) + tuple(tagList))

        self.configure(text_state = 'disabled')
        
