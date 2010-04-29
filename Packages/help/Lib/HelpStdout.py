import string

class HelpStdout:
    def __init__(self):
        self.rawData = []
        self.splitData = []
    def write(self,text):
        self.rawData.append(text)
    def split(self):
        for ii in self.rawData:
            temp = string.split(ii,'\n')
            for jj in temp:
                self.splitData.append(jj)
    def get(self):
        return self.splitData
    #def clear(self):
    #    self.rawData = []
    #    self.splitData = []
