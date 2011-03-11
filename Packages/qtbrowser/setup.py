from distutils.core import setup
import os


icons = """
icons/ESG_download.gif       icons/equal.gif              icons/off.gif
icons/Help.gif               icons/exp.gif                icons/on.gif
icons/Open_folder.gif        icons/fabs.gif               icons/power.gif
icons/Print.gif              icons/getmask.gif            icons/recycle.gif
icons/Save.gif               icons/greater.gif            icons/regrid.gif
icons/Script.gif             icons/grower.gif             icons/remove.gif
icons/UV-CDAT_logo_sites.gif icons/info.gif               icons/sin.gif
icons/add.gif                icons/inverse.gif            icons/sinh.gif
icons/base10.gif             icons/less.gif               icons/std.gif
icons/cos.gif                icons/log.gif                icons/subtract.gif
icons/cosh.gif               icons/mask.gif               icons/tan.gif
icons/divide.gif             icons/mlog.gif               icons/tanh.gif
icons/edit.gif               icons/mlog10.gif             icons/trashcan_empty.gif
icons/editdelete.gif         icons/multiply.gif
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
