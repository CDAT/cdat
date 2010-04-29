from default_handler import default_handler

# CDMS handles requests starting with /cdms
class cdms_handler(default_handler):

    def __init__(self, filesystem):
        default_handler.__init__(self, filesystem)

    def match (self, request):
        return (request.uri[0:5] == '/cdms')
        
