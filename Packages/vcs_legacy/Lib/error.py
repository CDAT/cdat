"Error object for vcs_legacy module, vcs_legacyError"
class vcs_legacyError (Exception):
    def __init__ (self, args=None):
        self.args = (args,)
    def __str__(self):
        return str(self.args[0])

