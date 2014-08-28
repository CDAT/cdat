"Error object for vcs module, vcsError"
class vcsError (Exception):
    def __init__ (self, args=None):
        self.args = (args,)
    def __str__(self):
        return str(self.args[0])

