import sys
import urllib2
import xml.dom.minidom
import thredds

def get(url):
    f = urllib2.urlopen(url)
    thredds_xml = ''
    for line in f.readlines():
        thredds_xml += line 

    return thredds_xml
    
def parse(thredds_xml):
    dom_obj = xml.dom.minidom.parseString(thredds_xml)
    thredds_obj = thredds.Thredds()

    dataset_element = dom_obj.getElementsByTagName('dataset')[0]
    dataset_id = dataset_element.getAttribute('ID')
    dataset_name = dataset_element.getAttribute('name')

    thredds_obj.setDatasetId(dataset_id)
    thredds_obj.setDatasetName(dataset_name)

    service_elements = dom_obj.getElementsByTagName('service')
    for cur_service in service_elements:
        if cur_service.hasAttribute('base'):
            base = cur_service.getAttribute('base')
        if cur_service.hasAttribute('name'):
            name = cur_service.getAttribute('name')
        if cur_service.hasAttribute('serviceType'):
            serviceType = cur_service.getAttribute('serviceType')
        thredds_obj.addService(base, name, serviceType)
    
    catalog_ref_elements = dom_obj.getElementsByTagName('catalogRef')
    for cur_catalog_ref in catalog_ref_elements:
        if cur_catalog_ref.hasAttribute('xlink:href'):
            href = cur_catalog_ref.getAttribute('xlink:href')
        else:
            href = cur_catalog_ref.getAttribute('href')
        
        if cur_catalog_ref.hasAttribute('xlink:title'):
            title = cur_catalog_ref.getAttribute('xlink:title')
        else:
            title = cur_catalog_ref.getAttribute('title')
            
        thredds_obj.addCatalogRef(href, title)
    
    dataset_elements = dom_obj.getElementsByTagName('dataset')
    dataSize_elements = dom_obj.getElementsByTagName('dataSize')
    property_elements = dom_obj.getElementsByTagName('property')
    access_elements = dom_obj.getElementsByTagName('access')

    if (len(dataset_elements) > 1 or len(dataSize_elements) > 0):
        for i in range(1, len(dataset_elements)):
            name = dataset_elements[i].getAttribute('name')
            if (len(dataSize_elements) > 0):
                size = dataSize_elements[i - 1].childNodes[0].data
            elif (len(property_elements) > 0):
                size = property_elements[i].getAttribute('value')
            else:
                size = 0
            
            if (len(access_elements) > 0):
                urlSuffix = access_elements[i - 1].getAttribute('urlPath')
            else:
                urlSuffix = dataset_elements[i].getAttribute('urlPath')
                
            thredds_obj.addFile(name, size, urlSuffix)
            
    dom_obj.unlink()
    
    return thredds_obj