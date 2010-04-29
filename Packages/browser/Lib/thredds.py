
class Thredds(object):
    
    def __init__(self):
        self.dataset_id = ''
        self.dataset_name = ''

        self.services = []
        self.catalog_refs = []
        self.files = []
        
    def setDatasetId(self, datasetId):
        self.dataset_id = datasetId
        
    def setDatasetName(self, datasetName):
        self.dataset_name = datasetName
    
        
    def addService(self, base, name, serviceType):
        service_obj = {'base' : base, 'name' : name, 'serviceType' : serviceType}
        self.services.append(service_obj)
        
    def addCatalogRef(self, href, title):
        catalog_ref_obj = {'href' : href, 'title' : title}
        self.catalog_refs.append(catalog_ref_obj)
        
    def addFile(self, name, size, url_suffix):
        file_obj = {'name' : name, 'size' : size, 'url_suffix' : url_suffix}
        self.files.append(file_obj)