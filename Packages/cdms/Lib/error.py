"Error object for cdms module"

class CDMSError (Exception):
    def __init__ (self, args="Unspecified error from package cdms"):
        self.args = (args,)
