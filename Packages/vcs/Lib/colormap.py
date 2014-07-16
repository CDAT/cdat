"""
# Colormap (Cp) module
"""
#################################################################################
#                                                                               #
# Module:       colormap (Cp) module                                            #
#                                                                               #
# Copyright:    2000, Regents of the University of California                   #
#               This software may not be distributed to others without          #
#               permission of the author.                                       #
#                                                                               #
# Author:       PCMDI Software Team                                             #
#               Lawrence Livermore NationalLaboratory:                          #
#               support@pcmdi.llnl.gov                                          #
#                                                                               #
# Description:  Python command wrapper for VCS's colormap secondary object.     #
#                                                                               #
# Version:      4.0                                                             #
#                                                                               #
#################################################################################
#
#
#
#############################################################################
from UserDict import UserDict
import vcs
import copy

def process_src(nm,code):
  numbers = eval(code)
  d= {}
  for i in range(255):
    val = numbers[i*3:i*3+3]
    if val!=():
      d[i]=list(val)
  cp =  Cp(nm)
  cp.index.data.update(d)



class RGB_Table(UserDict):
    def __init__(self, name, dict=None):
      self.data = {0: [100, 100, 100], 1: [0, 0, 0], 2: [85, 85, 85], 3: [32, 32, 32], 4: [100, 100, 100], 
          5: [100, 100, 0], 6: [0, 3, 100], 7: [0, 6, 100], 8: [0, 9, 100], 9: [0, 12, 100], 
          10: [0, 15, 100], 11: [0, 18, 100], 12: [0, 21, 100], 13: [0, 24, 100], 14: [0, 27, 100], 
          15: [0, 30, 100], 16: [0, 33, 100], 17: [0, 36, 100], 18: [0, 39, 100], 19: [0, 42, 100], 
          20: [0, 45, 100], 21: [0, 48, 100], 22: [0, 51, 100], 23: [0, 55, 100], 24: [0, 57, 100], 
          25: [0, 60, 100], 26: [0, 64, 100], 27: [0, 67, 100], 28: [0, 69, 100], 29: [0, 73, 100], 
          30: [0, 76, 100], 31: [0, 78, 100], 32: [0, 82, 100], 33: [0, 85, 100], 34: [0, 88, 100], 
          35: [0, 91, 100], 36: [0, 94, 100], 37: [0, 97, 100], 38: [0, 100, 100], 39: [0, 100, 97],
          40: [0, 100, 94], 41: [0, 100, 91], 42: [0, 100, 88], 43: [0, 100, 85], 44: [0, 100, 82], 
          45: [0, 100, 78], 46: [0, 100, 76], 47: [0, 100, 73], 48: [0, 100, 69], 49: [0, 100, 67], 
          50: [0, 100, 64], 51: [0, 100, 60], 52: [0, 100, 57], 53: [0, 100, 55], 54: [0, 100, 51], 
          55: [0, 100, 48], 56: [0, 100, 45], 57: [0, 100, 42], 58: [0, 100, 39], 59: [0, 100, 36],
          60: [0, 100, 33], 61: [0, 100, 30], 62: [0, 100, 27], 63: [0, 100, 24], 64: [0, 100, 21], 
          65: [0, 100, 18], 66: [0, 100, 15], 67: [0, 100, 12], 68: [0, 100, 9], 69: [0, 100, 6], 
          70: [0, 100, 3], 71: [0, 100, 0], 72: [3, 100, 0], 73: [6, 100, 0], 74: [9, 100, 0], 
          75: [12, 100, 0], 76: [15, 100, 0], 77: [18, 100, 0], 78: [21, 100, 0], 79: [24, 100, 0],
          80: [27, 100, 0], 81: [30, 100, 0], 82: [33, 100, 0], 83: [36, 100, 0], 84: [39, 100, 0], 
          85: [42, 100, 0], 86: [45, 100, 0], 87: [48, 100, 0], 88: [51, 100, 0], 89: [55, 100, 0], 
          90: [57, 100, 0], 91: [60, 100, 0], 92: [64, 100, 0], 93: [67, 100, 0], 94: [69, 100, 0], 
          95: [73, 100, 0], 96: [76, 100, 0], 97: [78, 100, 0], 98: [82, 100, 0], 99: [85, 100, 0],
          100: [88, 100, 0], 101: [91, 100, 0], 102: [94, 100, 0], 103: [97, 100, 0], 104: [100, 100, 0], 
          105: [100, 98, 0], 106: [100, 96, 0], 107: [100, 94, 0], 108: [100, 92, 0], 109: [100, 90, 0], 
          110: [100, 88, 0], 111: [100, 85, 0], 112: [100, 84, 0], 113: [100, 82, 0], 114: [100, 80, 0], 
          115: [100, 78, 0], 116: [100, 76, 0], 117: [100, 74, 0], 118: [100, 71, 0], 119: [100, 69, 0], 
          120: [100, 67, 0], 121: [100, 65, 0], 122: [100, 64, 0], 123: [100, 62, 0], 124: [100, 59, 0], 
          125: [100, 57, 0], 126: [100, 55, 0], 127: [100, 53, 0], 128: [100, 51, 0], 129: [100, 49, 0], 
          130: [100, 47, 0], 131: [100, 45, 0], 132: [100, 43, 0], 133: [100, 41, 0], 134: [100, 39, 0], 
          135: [100, 37, 0], 136: [100, 35, 0], 137: [100, 33, 0], 138: [100, 32, 0], 139: [100, 31, 0], 
          140: [100, 30, 0], 141: [100, 29, 0], 142: [100, 28, 0], 143: [100, 27, 0], 144: [100, 26, 0], 
          145: [100, 25, 0], 146: [100, 24, 0], 147: [100, 23, 0], 148: [100, 22, 0], 149: [100, 21, 0], 
          150: [100, 20, 0], 151: [100, 19, 0], 152: [100, 18, 0], 153: [100, 17, 0], 154: [100, 16, 0], 
          155: [100, 15, 0], 156: [100, 14, 0], 157: [100, 13, 0], 158: [100, 12, 0], 159: [100, 11, 0], 
          160: [100, 10, 0], 161: [100, 9, 0], 162: [100, 8, 0], 163: [100, 7, 0], 164: [100, 6, 0], 
          165: [100, 5, 0], 166: [100, 4, 0], 167: [100, 3, 0], 168: [100, 2, 0], 169: [100, 1, 0], 
          170: [100, 0, 0], 171: [98, 0, 0], 172: [96, 0, 0], 173: [94, 0, 0], 174: [92, 0, 0], 
          175: [90, 0, 0], 176: [89, 0, 0], 177: [87, 0, 0], 178: [85, 0, 0], 179: [83, 0, 0], 
          180: [81, 0, 0], 181: [79, 0, 0], 182: [77, 0, 0], 183: [75, 0, 0], 184: [73, 0, 0], 
          185: [71, 0, 0], 186: [69, 0, 0], 187: [68, 0, 0], 188: [66, 0, 0], 189: [64, 0, 0], 
          190: [62, 0, 0], 191: [60, 0, 0], 192: [58, 0, 0], 193: [56, 0, 0], 194: [55, 0, 0], 
          195: [53, 0, 0], 196: [51, 0, 0], 197: [49, 0, 0], 198: [47, 0, 0], 199: [45, 0, 0], 
          200: [43, 0, 0], 201: [41, 0, 0], 202: [39, 0, 0], 203: [38, 0, 0], 204: [38, 0, 2], 
          205: [39, 0, 4], 206: [40, 0, 6], 207: [41, 0, 8], 208: [42, 0, 10], 209: [43, 0, 12], 
          210: [44, 0, 14], 211: [45, 0, 16], 212: [46, 0, 18], 213: [47, 0, 20], 214: [48, 0, 22], 
          215: [49, 0, 24], 216: [50, 0, 26], 217: [51, 0, 29], 218: [52, 0, 31], 219: [53, 0, 33], 
          220: [54, 0, 35], 221: [55, 0, 37], 222: [56, 0, 39], 223: [57, 0, 41], 224: [58, 0, 43], 
          225: [59, 0, 45], 226: [60, 0, 47], 227: [61, 0, 49], 228: [62, 0, 51], 229: [63, 0, 53], 
          230: [64, 0, 55], 231: [65, 0, 57], 232: [65, 0, 59], 233: [67, 0, 61], 234: [67, 0, 63], 
          235: [69, 0, 65], 236: [69, 0, 67], 237: [71, 0, 69], 238: [71, 0, 71], 239: [73, 0, 74], 
          240: [100, 100, 100], 241: [0, 0, 0], 242: [100, 0, 0], 243: [0, 100, 0], 244: [0, 0, 100], 
          245: [100, 100, 0], 246: [0, 100, 100], 247: [100, 0, 100], 248: [100, 50, 10], 249: [60, 30, 10], 
          250: [5, 10, 67], 251: [50, 50, 0], 252: [80, 80, 80], 253: [80, 100, 80], 254: [95, 75, 75], 
          255: [60, 80, 100]}

      self.name = name
      if dict is not None: self.update(dict)
    def __setitem__(self,key,value):
       if (self.name == 'default'):
           raise ValueError, 'You cannot modify the default colormap.'
       if (key not in range(0,240)):
           raise ValueError, 'Cell index must be in the range 0 to 239.'
       if isinstance(value,(list,tuple)):
           value = list(value)
           if len(value) != 3:
              raise ValueError, 'Must be a tuple or list of size 3.'
           for i in range(len(value)):
              if value[i] not in range(0,101):
                 raise ValueError, 'The R,G,B values must be in the range 0 to 100.'
       else:
           raise ValueError, 'Must be either a list object, tuple object, or integer value.'
       self.data[key] = value
    def __getitem__(self,key):
       if (key not in range(0,256)):
           raise ValueError, 'Cell index must be in the range 0 to 255.'
       return self.data[key]
#
#
#############################################################################
#                                                                           #
# Colormap (Cp) Class.                                                      #
#                                                                           #
#############################################################################
class Cp:
    """
 Class: Cp                              # Colormap

 Description of Cp Class:
    The Colormap object allows the manipulation of the colormap index R,G,B values.

    This class is used to define a colormap table entry used in VCS, or it
    can be used to change some or all of the colormap R,G,B attributes in an
    existing colormap table entry.

 Other Useful Functions:
             a=vcs.init()               # Constructor
             a.show('colormap')         # Show predefined colormap objects
             a.update()                 # Updates the VCS Canvas at user's request
             a.mode=1, or 0             # If 1, then automatic update, else if
                                          0, then use update function to
                                          update the VCS Canvas.

 Example of Use:
    a=vcs.init()
    To Create a new instance of colormap use:
     cp=a.createcolormap('new','quick') # Copies content of 'red' to 'new'
     cp=a.createcolormap('new')         # Copies content of 'default' to 'new'
        print "Now our index is :",self.index[242],self

    To Modify an existing colormap use:
     cp=a.getcolormap('quick')

    cp.list()                           # Will list all the colormap indices and 
					#      R,G,B attribute values
    cp.color=16,100,0,0                 # Index, R, G, B      
    cp.color=16,0,100,0                 # Index range from 0 to 255, but can only
					#      modify from 0 to 239
    cp.color=17,0,0,100                 # R, G, B values range from 0 to 100, where
 					#      0 is low intensity and 100 is highest intensity
"""
    __slots__ = ["s_name","name","_name","index","_index"]
    def getname(self):
      return self._name
    def setname(self,value):
      if value=="default":
        raise Exception,"you cannot modify default colormap"
      vcs.elements[value]=vcs.elements[self.name]
      del(vcs.elements[self.name])
      self._name=value
    name = property(getname,setname)
    
    def getindex(self):
      return self._index
    def setindex(self,value):
      raise Exception,"invalid"
    index = property(getindex,setindex)
    #############################################################################
    #                                                                           #
    # Initialize the colormap attributes.                                       #
    #                                                                           #
    #############################################################################
    def __init__(self, Cp_name, Cp_name_src='default'):
        #                                                             #
        ###############################################################
        # Initialize the colormap class and its members               #
        #                                                             #
        # The getCpmember function retrieves the values of the        #
        # colormap members in the C structure and passes back the     #
        # appropriate Python Object.                                  #
        ###############################################################
        #                                                             #
        self.s_name='Cp'
        self._name = Cp_name
        if Cp_name=="default":
          self._index=RGB_Table(self._name) # Create colormap dictionary
        else:
          if isinstance(Cp_name_src,Cp):
            Cp_name_src=Cp_name_src.name
          if not Cp_name_src in vcs.elements["colormap"]:
            raise Exception,"Error source colormap '%s' does not exists" % Cp_name_src
          src = vcs.elements["colormap"][Cp_name_src]
          self._index = copy.deepcopy(src.index)
          self._index.name = Cp_name
        vcs.elements["colormap"][Cp_name]=self


	# Note: See RGB_Table Class for "index" setting of the colormap entries

    #############################################################################
    #                                                                           #
    # List out colormap members (attributes).                                   #
    #                                                                           #
    #############################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
           raise ValueError, 'This instance has been removed from VCS.'
        print "","----------Colormap (Cp) member (attribute) listings ----------"
        print "secondary method =", self.s_name
        print "name =", self.name
        print "index =", self.index

    #############################################################################
    #                                                                           #
    # Script out secondary colormap method in VCS to a file.                    #
    #                                                                           #
    #############################################################################
    def script(self, script_filename=None, mode=None):
        '''
 Function:     script                           # Calls _vcs.scriptCp

 Description of Function:
       Saves out a colormap graphics method in VCS or Python script form to a
       designated file.

 Example of Use:
    script(scriptfile_name, mode)
              where: scriptfile_name is the output name of the script file.
                     mode is either "w" for replace or "a" for append.

              Note: If the the filename has a ".py" at the end, it will produce a
                    Python script. If the filename has a ".scr" at the end, it will
                    produce a VCS script. If neither extensions are give, then by
                    default a Python script will be produced.

    a=vcs.init()
    cp=a.createcolormap('temp')
    cp.script('filename.py')         # Append to a Python file "filename.py"
    cp.script('filename.scr')        # Append to a VCS file "filename.scr"
    cp.script('filename','w')        # Create or overwrite to a Python file "filename.py"
'''
        if (script_filename == None):
          raise ValueError, 'Error - Must provide an output script file name.'

        if (mode == None):
           mode = 'a'
        elif (mode not in ('w', 'a')):
          raise ValueError, 'Error - Mode can only be "w" for replace or "a" for append.'

        # By default, save file in python script mode
        scr_type = script_filename[len(script_filename)-4:len(script_filename)]
        if (scr_type == '.scr'):
           print _vcs.scriptCp(self.name,script_filename,mode)
        else:
           mode = mode + '+'
           py_type = script_filename[len(script_filename)-3:len(script_filename)]
           if (py_type != '.py'):
              script_filename = script_filename + '.py'

           # Write to file
           fp = open(script_filename,mode)
           if (fp.tell() == 0): # Must be a new file, so include below
              fp.write("#####################################\n")
              fp.write("#                                 #\n")
              fp.write("# Import and Initialize VCS     #\n")
              fp.write("#                             #\n")
              fp.write("#############################\n")
              fp.write("import vcs\n")
              fp.write("v=vcs.init()\n\n")

           unique_name = '__Cp__' + self.name
           fp.write("#----------Colormap (Cp) member (attribute) listings ----------\n")
           fp.write("tl_list=v.listelements('colormap')\n")
           fp.write("if ('%s' in tl_list):\n" % self.name)
           fp.write("   %s = v.getcolormap('%s')\n" % (unique_name, self.name))
           fp.write("else:\n")
           fp.write("   %s = v.createcolormap('%s')\n" % (unique_name, self.name))
           fp.write("%s.index = '%s'\n" % (unique_name, self.index))


#################################################################################
#        END OF FILE                                                            #
#################################################################################
