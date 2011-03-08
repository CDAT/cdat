from distutils.core import setup
import os

setup (name = "qtbrowser",
       version='1.0',
       description = "Qt-based data browser",
       url = "http://cdat.sourceforge.net",
       packages = ['qtbrowser'],
       package_dir = {'qtbrowser': 'Lib'},
       scripts=['Scripts/vcdat',],
       data_files = [('share/icons',['icons/ESG_download.gif', 'icons/Open_folder.gif', 'icons/Save.gif', 'icons/Help.gif', 'icons/Print.gif', 'icons/Script.gif','icons/UV-CDAT_logo_sites.gif','icons/remove.gif','icons/on.gif','icons/off.gif']),]
      )
