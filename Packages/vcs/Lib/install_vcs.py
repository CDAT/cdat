#############################################################################
#                                                                           #
# Create the $HOME/.uvcdat directory if it is does not exist.        #
# Copy the appropriate files from the $PYTHONHOME/bin directory to the      #
# user's $HOME/.uvcdat directory.                                    #
#                                                                           #
#############################################################################
def _files():
   import sys, os, shutil,vcs
   dotdir, dotdirenv = vcs.getdotdirectory()
   #
   # Create .uvcdat directory if it does not exist
   try:
      fn = os.environ[dotdirenv]
   except:
      try:
         fn = '%s/%s' % (os.environ['HOME'],dotdir)
      except:
         print "Could not find the $HOME directory or the %s . Set your environment variable 'HOME' or '%s'" % (dotdir,dotdirenv)
         print "to your home directory. (e.g., 'setenv HOME /home/user')."
         sys.exit()
   if os.access(fn, os.X_OK) == 0:
      try:
         os.mkdir( fn )
      except:
         print "Do not have write permission for user's home directory. Must have write permissions."
         sys.exit()
   #
   # Copy the initial.attributes file to the user's $HOME/.uvcdat directory
   init_file_cp = os.path.join(vcs.__path__[0],'..','..','..','..', 'bin', 'initial.attributes')
   init_file = os.path.join( fn, 'initial.attributes')
   if (os.access(init_file_cp, os.F_OK) == 1)  and (os.path.isfile(init_file) == 0):
      shutil.copyfile(init_file_cp, init_file)
      
   # Copy the  icon file to the user's $HOME/.uvcdat directory
   icon_file_cp = os.path.join(vcs.__path__[0],'..','..','..','..', 'bin', 'vcs_icon.xbm')
   icon_file = os.path.join( fn, 'vcs_icon.xbm')
##    if (os.access(icon_file_cp, os.F_OK) == 1)  and (os.path.isfile(icon_file) == 0):
   if (os.access(icon_file_cp, os.F_OK) == 1):
      
      shutil.copyfile(icon_file_cp, icon_file)
   #
   # Copy the HARD_COPY file to the user's $HOME/.uvcdat directory
   hard_copy_cp = os.path.join(vcs.__path__[0],'..','..','..','..', 'bin', 'HARD_COPY')
   hard_copy = os.path.join(fn, 'HARD_COPY')
   if (os.access(hard_copy_cp, os.F_OK) == 1)  and (os.path.isfile(hard_copy) == 0):
      shutil.copyfile(hard_copy_cp, hard_copy)
      # Add the gplot commands to the HARD_COPY file
      add_gplot_commands()

   #
   # Copy the data_continent_states file to the user's $HOME/.uvcdat directory
   cont_states_cp = os.path.join( vcs.__path__[0],'..','..','..','..', 'bin', 'data_continent_states')
   cont_states = os.path.join( fn, 'data_continent_states')
   if (os.access(cont_states_cp, os.F_OK) == 1)  and (os.path.isfile(cont_states) == 0):
      shutil.copyfile(cont_states_cp, cont_states)
   #
   # Copy the data_continent_political file to the user's $HOME/.uvcdat directory
   cont_political_cp = os.path.join( vcs.__path__[0],'..','..','..','..', 'bin', 'data_continent_political' )
   cont_political = os.path.join( fn, 'data_continent_political' )
   if (os.access(cont_political_cp, os.F_OK) == 1)  and (os.path.isfile(cont_political) == 0):
      shutil.copyfile(cont_political_cp, cont_political)
   #
   # Copy the data_continent_river file to the user's $HOME/.uvcdat directory
   cont_river_cp = os.path.join( vcs.__path__[0],'..','..','..','..', 'bin', 'data_continent_river' )
   cont_river = os.path.join( fn, 'data_continent_river' )
   if (os.access(cont_river_cp, os.F_OK) == 1)  and (os.path.isfile(cont_river) == 0):
      shutil.copyfile(cont_river_cp, cont_river)
   #
   # Copy the data_continent_other7 file to the user's $HOME/.uvcdat directory
   cont_other7_cp = os.path.join( vcs.__path__[0],'..','..','..','..', 'bin', 'data_continent_other7' )
   cont_other7 = os.path.join( fn, 'data_continent_other7' )
   if (os.access(cont_other7_cp, os.F_OK) == 1)  and (os.path.isfile(cont_other7) == 0):
      shutil.copyfile(cont_other7_cp, cont_other7)

   # Copy font files to the user's $HOME/.uvcdat
   for font in ['Adelon_Regular', 'Arabic', 'Athens_Greek', 'AvantGarde-Book_Bold',
                'Chinese_Generic1', 'Clarendon', 'Courier', 'hebrew', 'HelvMono',
                'Russian', 'Times_CG_ATT', 'jsMath-wasy10', 'blex', 'blsy', 'jsMath-msam10']:
      fnm_cp = os.path.join( vcs.__path__[0],'..','..','..','..', 'bin', font+'.ttf' )
      fnm = os.path.join( fn, font+'.ttf')
      if (os.access(fnm_cp, os.F_OK) == 1)  and (os.path.isfile(fnm) == 0):
         shutil.copyfile(fnm_cp, fnm)

#############################################################################
#                                                                           #
# Set the user's XGKSFontDir environment variable               #
#                                                                           #
#############################################################################
def _XGKSFontDir( ):
   import sys, os, vcs
   xgks_env = os.path.join(vcs.__path__[0],'..','..','..','..', 'lib', 'xgksfonts')
   os.environ['XGKSFontDir'] = xgks_env

#############################################################################
#                                                                           #
# Return the list of available printers from the HARD_COPY file. The        #
# printer list will be located in the user's $HOME/.uvcdat/HARD_COPY #
# file.                                                                     #
#                                                                           #
#############################################################################
def list_printers( ):
   import os,vcs
   dotdir = vcs.getdotdirectory()[0]
   plist = []
   try:
      fn = '%s/%s/HARD_COPY' % (os.environ['HOME'],dotdir)
   except:
      return plist
   try:
      f=open( fn )
   except:
      return plist
   ln = f.readline()
   while ln:
      if ln[0] != '#':
         if (ln[0:9] == 'landscape' or ln[0:8] == 'portrait' or
            ln[0] == ' ' or ln[0] == '\n'):
            pass
         else:
            plist.append(ln[0:-1])
      ln = f.readline()
   f.close( )
   return plist

#############################################################################
#                                                                           #
# Add to the list of available printers in  the HARD_COPY file.  The        #
# printer list will be located in the user's $HOME/.uvcdat/HARD_COPY #
# file.                                                                     #
#                                                                           #
#############################################################################
def add_printer( printer_name ):
   import os
   plist = []
   dotdir = vcs.getdotdirectory()[0]
   try:
      fn = '%s/%s/HARD_COPY' % (os.environ['HOME'],dotdir)
   except:
      return plist
   try:
      f=open( fn, "a" )
   except:
      return
   f.write( "\n" )
   f.write( printer_name )
   f.write( "\n" )
   f.close( )

#############################################################################
#                                                                           #
# Remove name from the list of available printers in the HARD_COPY file. The#
# printer list will be located in the user's $HOME/.uvcdat/HARD_COPY #
# file.                                                                     #
#                                                                           #
#############################################################################
def remove_printer( printer_name ):
   import os, string,vcs
   plist = []
   dotdir = vcs.getdotdirectory()[0]
   try:
      fn = '%s/%s/HARD_COPY' % (os.environ['HOME'],dotdir)
   except:
      return plist
   try:
      f=open( fn, "r+" )
   except:
      return
   lp = f.tell()
   ln = f.readline()
   while string.strip(ln) != printer_name and ln != '':
      lp = f.tell()
      ln = f.readline()
   f.seek( lp-1 )
   f.write ( ' '*len(ln) )
   f.close( )

#############################################################################
#                                                                           #
# Add the gplot command to the HARD_COPY file. This command is necessary    #
# for converting cgm files to postscript, eps, and gif files.               #
#                                                                           #
#############################################################################
def add_gplot_commands( ):
   import sys, os, string, vcs
   dotdir = vcs.getdotdirectory()[0]
   try:
      fn = os.path.join(os.environ['HOME'], dotdir, 'HARD_COPY')
      fn2 = os.path.join(os.environ['HOME'], dotdir, 'HARD_COPY.tmp')
   except:
      return
   try:
      f=open( fn, "r+" )
      f2=open( fn2, "w" )
   except:
      return

   # Replace the Landscape command string in the file
   lp = f.tell()
   ln = f.readline()
   while (1):
      if ( (ln != '\n') and ((string.split( ln )[0] == "#landscape") or 
         (string.split( ln )[0] == "landscape")) ): break
      f2.write( ln ) 
      lp = f.tell()
      ln = f.readline()
   f.seek( lp )
   landscape = 'landscape = ' + os.path.join(vcs.__path__[0],'..','..','..','..') + '/bin/gplot -dPSC -r90 -x-1.75 -D -X12.5 -Y10\n\n'
   f2.write( landscape ) 

   # Replace the Portrait command string in the file
   lp = f.tell()
   ln = f.readline()
   ln = f.readline()
   ln = f.readline()
   while (1):
      if ( (ln != '\n') and ((string.split( ln )[0] == "#portrait") or 
         (string.split( ln )[0] == "portrait")) ): break
      f2.write( ln ) 
      lp = f.tell()
      ln = f.readline()
   f.seek( lp )
   portrait = 'portrait = ' + os.path.join(vcs.__path__[0],'..','..','..','..') + '/bin/gplot -dPSC -D -X10 -Y12.5\n\n'
   f2.write( portrait ) 

   ln = f.readline()
   ln = f.readline()
   ln = f.readlines()
   f2.writelines( ln )

   f.close( )
   f2.close( )

   # Move HARD_COPY.tmp to HARD_COPY
   command = 'mv -f %s %s' % (fn2, fn)
   os.system( command ) 

#---------------------------------------------------------------------
# End of File
#---------------------------------------------------------------------
