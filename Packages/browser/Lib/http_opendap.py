import sys
import urllib2
import xml.dom.minidom
import re
import thredds

def get_dds(url, passwordMgr, parent):
    try:
        f = urllib2.urlopen(url + '.dds')
        dds = ''
        for line in f.readlines():
            dds += line
        return dds
    
    except urllib2.HTTPError, e:
        if e.code == 401:
            authline = e.headers.get('www-authenticate', '')
            
            scheme = authline[:authline.find(' ')]
            
            realm = authline[authline.find('"') + 1:-1]
            
            host = url.replace('http://', '')
            host = host[:host.find('/')]
            
            # function that will be called after prompting for username/password
            callback = get_dds
            callback_handler = parent.handle_dds
            
            parent.auth_prompt(realm, host, url, callback, callback_handler)
            
        else:
            raise e

            
def get_das(url, passwordMgr, parent):
    try:
        f = urllib2.urlopen(url + '.das')
        das = ''
        for line in f.readlines():
            das += line
        return das
    
    except urllib2.HTTPError, e:
        
        if e.code == 401:
            authline = e.headers.get('www-authenticate', '')
            
            scheme = authline[:authline.find(' ')]
            realm = authline[authline.find('"') + 1:-1]
            
            host = url.replace('http://', '')
            host = host[:host.find('/')]
            
            # functions that will be called after prompting for username/password
            callback = get_das
            callback_handler = parent.handle_das
            
            parent.auth_prompt(realm, host, url, callback, callback_handler)
            
        else:
            raise e
