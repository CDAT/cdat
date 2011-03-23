from distutils.core import setup
import os


icons = """
icons/ESG_download.gif
icons/off.gif
icons/symbol_help.ico
icons/on.gif
icons/printer.ico
icons/symbol_check.ico
icons/script_folder_smooth.ico
icons/remove.gif
icons/UV-CDAT_logo_sites.gif
icons/symbol_information.ico
icons/floppy_disk_blue.ico
icons/symbol_delete.ico
icons/folder_image_blue.ico
icons/pencil.ico
""".split()
setup (name = "qtbrowser",
       version='1.0',
       description = "Qt-based data browser",
       url = "http://cdat.sourceforge.net",
       packages = ['qtbrowser'],
       package_dir = {'qtbrowser': 'Lib'},
       scripts=['Scripts/vcdat',],
       data_files = [('share/icons',icons),]
      )
