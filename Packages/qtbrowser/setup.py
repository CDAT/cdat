from distutils.core import setup
import os

icons = """
icons/connected.ico
icons/off.gif
icons/on.gif
icons/printer.ico
icons/script_folder_smooth.ico
icons/remove.gif
icons/UV-CDAT_logo_sites.gif
icons/symbol_help.ico
icons/symbol_check.ico
icons/symbol_information.ico
icons/symbol_refresh.ico
icons/symbol_delete.ico
icons/floppy_disk_blue.ico
icons/folder_image_blue.ico
icons/pencil.ico
icons/run_copy.ico
icons/redo.ico
icons/blender-icon.png
icons/vistrails_icon.png
""".split()
setup (name = "qtbrowser",
       version='1.0',
       description = "Qt-based data browser",
       url = "http://cdat.sourceforge.net",
       packages = ['qtbrowser'],
       package_dir = {'qtbrowser': 'Lib'},
       scripts=['Scripts/uv-cdat','Scripts/vcdat'],
       data_files = [('share/icons',icons),]
      )
