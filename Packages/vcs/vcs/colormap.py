"""
# Colormap (Cp) module
"""
##########################################################################
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
##########################################################################
#
#
#
#############################################################################
from UserDict import UserDict
import vcs
import copy
import xmldocs


def process_src(nm, code):
    numbers = eval(code)
    d = {}
    for i in range(255):
        val = numbers[i * 3:i * 3 + 3]
        if val != ():
            d[i] = list(val) + [100]
    cp = Cp(nm)
    cp.index.data.update(d)


class RGB_Table(UserDict):
    __slots__ = ["data", ]

    def __init__(self, name, dict=None):
        self.data = {0: [100, 100, 100, 100.], 1: [0, 0, 0, 100.], 2: [85, 85, 85, 100.],
                     3: [32, 32, 32, 100.], 4: [100, 100, 100, 100.],
                     5: [100, 100, 0, 100.], 6: [0, 3, 100, 100.], 7: [0, 6, 100, 100.],
                     8: [0, 9, 100, 100.], 9: [0, 12, 100, 100.],
                     10: [0, 15, 100, 100.], 11: [0, 18, 100, 100.], 12: [0, 21, 100, 100.],
                     13: [0, 24, 100, 100.], 14: [0, 27, 100, 100.],
                     15: [0, 30, 100, 100.], 16: [0, 33, 100, 100.], 17: [0, 36, 100, 100.],
                     18: [0, 39, 100, 100.], 19: [0, 42, 100, 100.],
                     20: [0, 45, 100, 100.], 21: [0, 48, 100, 100.], 22: [0, 51, 100, 100.],
                     23: [0, 55, 100, 100.], 24: [0, 57, 100, 100.],
                     25: [0, 60, 100, 100.], 26: [0, 64, 100, 100.], 27: [0, 67, 100, 100.],
                     28: [0, 69, 100, 100.], 29: [0, 73, 100, 100.],
                     30: [0, 76, 100, 100.], 31: [0, 78, 100, 100.], 32: [0, 82, 100, 100.],
                     33: [0, 85, 100, 100.], 34: [0, 88, 100, 100.],
                     35: [0, 91, 100, 100.], 36: [0, 94, 100, 100.], 37: [0, 97, 100, 100.],
                     38: [0, 100, 100, 100.], 39: [0, 100, 97, 100.],
                     40: [0, 100, 94, 100.], 41: [0, 100, 91, 100.], 42: [0, 100, 88, 100.],
                     43: [0, 100, 85, 100.], 44: [0, 100, 82, 100.],
                     45: [0, 100, 78, 100.], 46: [0, 100, 76, 100.], 47: [0, 100, 73, 100.],
                     48: [0, 100, 69, 100.], 49: [0, 100, 67, 100.],
                     50: [0, 100, 64, 100.], 51: [0, 100, 60, 100.], 52: [0, 100, 57, 100.],
                     53: [0, 100, 55, 100.], 54: [0, 100, 51, 100.],
                     55: [0, 100, 48, 100.], 56: [0, 100, 45, 100.], 57: [0, 100, 42, 100.],
                     58: [0, 100, 39, 100.], 59: [0, 100, 36, 100.],
                     60: [0, 100, 33, 100.], 61: [0, 100, 30, 100.], 62: [0, 100, 27, 100.],
                     63: [0, 100, 24, 100.], 64: [0, 100, 21, 100.],
                     65: [0, 100, 18, 100.], 66: [0, 100, 15, 100.], 67: [0, 100, 12, 100.],
                     68: [0, 100, 9, 100.], 69: [0, 100, 6, 100.],
                     70: [0, 100, 3, 100.], 71: [0, 100, 0, 100.], 72: [3, 100, 0, 100.],
                     73: [6, 100, 0, 100.], 74: [9, 100, 0, 100.],
                     75: [12, 100, 0, 100.], 76: [15, 100, 0, 100.], 77: [18, 100, 0, 100.],
                     78: [21, 100, 0, 100.], 79: [24, 100, 0, 100.],
                     80: [27, 100, 0, 100.], 81: [30, 100, 0, 100.], 82: [33, 100, 0, 100.],
                     83: [36, 100, 0, 100.], 84: [39, 100, 0, 100.],
                     85: [42, 100, 0, 100.], 86: [45, 100, 0, 100.], 87: [48, 100, 0, 100.],
                     88: [51, 100, 0, 100.], 89: [55, 100, 0, 100.],
                     90: [57, 100, 0, 100.], 91: [60, 100, 0, 100.], 92: [64, 100, 0, 100.],
                     93: [67, 100, 0, 100.], 94: [69, 100, 0, 100.],
                     95: [73, 100, 0, 100.], 96: [76, 100, 0, 100.], 97: [78, 100, 0, 100.],
                     98: [82, 100, 0, 100.], 99: [85, 100, 0, 100.],
                     100: [88, 100, 0, 100.], 101: [91, 100, 0, 100.], 102: [94, 100, 0, 100.],
                     103: [97, 100, 0, 100.], 104: [100, 100, 0, 100.],
                     105: [100, 98, 0, 100.], 106: [100, 96, 0, 100.], 107: [100, 94, 0, 100.],
                     108: [100, 92, 0, 100.], 109: [100, 90, 0, 100.],
                     110: [100, 88, 0, 100.], 111: [100, 85, 0, 100.], 112: [100, 84, 0, 100.],
                     113: [100, 82, 0, 100.], 114: [100, 80, 0, 100.],
                     115: [100, 78, 0, 100.], 116: [100, 76, 0, 100.], 117: [100, 74, 0, 100.],
                     118: [100, 71, 0, 100.], 119: [100, 69, 0, 100.],
                     120: [100, 67, 0, 100.], 121: [100, 65, 0, 100.], 122: [100, 64, 0, 100.],
                     123: [100, 62, 0, 100.], 124: [100, 59, 0, 100.],
                     125: [100, 57, 0, 100.], 126: [100, 55, 0, 100.], 127: [100, 53, 0, 100.],
                     128: [100, 51, 0, 100.], 129: [100, 49, 0, 100.],
                     130: [100, 47, 0, 100.], 131: [100, 45, 0, 100.], 132: [100, 43, 0, 100.],
                     133: [100, 41, 0, 100.], 134: [100, 39, 0, 100.],
                     135: [100, 37, 0, 100.], 136: [100, 35, 0, 100.], 137: [100, 33, 0, 100.],
                     138: [100, 32, 0, 100.], 139: [100, 31, 0, 100.],
                     140: [100, 30, 0, 100.], 141: [100, 29, 0, 100.], 142: [100, 28, 0, 100.],
                     143: [100, 27, 0, 100.], 144: [100, 26, 0, 100.],
                     145: [100, 25, 0, 100.], 146: [100, 24, 0, 100.], 147: [100, 23, 0, 100.],
                     148: [100, 22, 0, 100.], 149: [100, 21, 0, 100.],
                     150: [100, 20, 0, 100.], 151: [100, 19, 0, 100.], 152: [100, 18, 0, 100.],
                     153: [100, 17, 0, 100.], 154: [100, 16, 0, 100.],
                     155: [100, 15, 0, 100.], 156: [100, 14, 0, 100.], 157: [100, 13, 0, 100.],
                     158: [100, 12, 0, 100.], 159: [100, 11, 0, 100.],
                     160: [100, 10, 0, 100.], 161: [100, 9, 0, 100.], 162: [100, 8, 0, 100.],
                     163: [100, 7, 0, 100.], 164: [100, 6, 0, 100.],
                     165: [100, 5, 0, 100.], 166: [100, 4, 0, 100.], 167: [100, 3, 0, 100.],
                     168: [100, 2, 0, 100.], 169: [100, 1, 0, 100.],
                     170: [100, 0, 0, 100.], 171: [98, 0, 0, 100.], 172: [96, 0, 0, 100.],
                     173: [94, 0, 0, 100.], 174: [92, 0, 0, 100.],
                     175: [90, 0, 0, 100.], 176: [89, 0, 0, 100.], 177: [87, 0, 0, 100.],
                     178: [85, 0, 0, 100.], 179: [83, 0, 0, 100.],
                     180: [81, 0, 0, 100.], 181: [79, 0, 0, 100.], 182: [77, 0, 0, 100.],
                     183: [75, 0, 0, 100.], 184: [73, 0, 0, 100.],
                     185: [71, 0, 0, 100.], 186: [69, 0, 0, 100.], 187: [68, 0, 0, 100.],
                     188: [66, 0, 0, 100.], 189: [64, 0, 0, 100.],
                     190: [62, 0, 0, 100.], 191: [60, 0, 0, 100.], 192: [58, 0, 0, 100.],
                     193: [56, 0, 0, 100.], 194: [55, 0, 0, 100.],
                     195: [53, 0, 0, 100.], 196: [51, 0, 0, 100.], 197: [49, 0, 0, 100.],
                     198: [47, 0, 0, 100.], 199: [45, 0, 0, 100.],
                     200: [43, 0, 0, 100.], 201: [41, 0, 0, 100.], 202: [39, 0, 0, 100.],
                     203: [38, 0, 0, 100.], 204: [38, 0, 2, 100.],
                     205: [39, 0, 4, 100.], 206: [40, 0, 6, 100.], 207: [41, 0, 8, 100.],
                     208: [42, 0, 10, 100.], 209: [43, 0, 12, 100.],
                     210: [44, 0, 14, 100.], 211: [45, 0, 16, 100.], 212: [46, 0, 18, 100.],
                     213: [47, 0, 20, 100.], 214: [48, 0, 22, 100.],
                     215: [49, 0, 24, 100.], 216: [50, 0, 26, 100.], 217: [51, 0, 29, 100.],
                     218: [52, 0, 31, 100.], 219: [53, 0, 33, 100.],
                     220: [54, 0, 35, 100.], 221: [55, 0, 37, 100.], 222: [56, 0, 39, 100.],
                     223: [57, 0, 41, 100.], 224: [58, 0, 43, 100.],
                     225: [59, 0, 45, 100.], 226: [60, 0, 47, 100.], 227: [61, 0, 49, 100.],
                     228: [62, 0, 51, 100.], 229: [63, 0, 53, 100.],
                     230: [64, 0, 55, 100.], 231: [65, 0, 57, 100.], 232: [65, 0, 59, 100.],
                     233: [67, 0, 61, 100.], 234: [67, 0, 63, 100.],
                     235: [69, 0, 65, 100.], 236: [69, 0, 67, 100.], 237: [71, 0, 69, 100.],
                     238: [71, 0, 71, 100.], 239: [73, 0, 74, 100.],
                     240: [100, 100, 100, 100.], 241: [0, 0, 0, 100.], 242: [100, 0, 0, 100.],
                     243: [0, 100, 0, 100.], 244: [0, 0, 100, 100.],
                     245: [100, 100, 0, 100.], 246: [0, 100, 100, 100.], 247: [100, 0, 100, 100.],
                     248: [100, 50, 10, 100.], 249: [60, 30, 10, 100.],
                     250: [5, 10, 67, 100.], 251: [50, 50, 0, 100.], 252: [80, 80, 80, 100.],
                     253: [80, 100, 80, 100.], 254: [95, 75, 75, 100.],
                     255: [60, 80, 100, 100.]}

        self.name = name
        if dict is not None:
            self.update(dict)

    def __setitem__(self, key, value):
        if (self.name == 'default'):
            raise ValueError('You cannot modify the default colormap.')
        if (key not in range(0, 256)):
            raise ValueError('Cell index must be in the range 0 to 255.')
        if isinstance(value, (list, tuple)):
            value = list(value)
            if len(value) not in [3, 4]:
                raise ValueError('Must be a tuple or list of size 3 or 4')
            if len(value) == 3:
                value.append(100.)
            for i in range(len(value)):
                if not 0. <= value[i] <= 100.:
                    raise ValueError(
                        'The R,G,B,A values must be in the range 0 to 100. %s' % str(value))
        else:
            raise ValueError(
                'Must be either a list object, tuple object, or integer value.')
        self.data[key] = value

    def __getitem__(self, key):
        if (key not in range(0, 256)):
            raise ValueError('Cell index must be in the range 0 to 255.')
        return self.data[key]
#
#
#############################################################################
#                                                                           #
# Colormap (Cp) Class.                                                      #
#                                                                           #
#############################################################################


class Cp(object):

    """
    The Colormap object allows the manipulation of the colormap index R,G,B values.

    This class is used to define a colormap table entry used in VCS, or it
    can be used to change some or all of the colormap R,G,B attributes in an
    existing colormap table entry.

    .. describe:: Some Useful Functions:

        .. code-block:: python

            # Constructor
            a=vcs.init()
            # Show predefined colormap objects
            a.show('colormap')
            # Updates the VCS Canvas at user's request
            a.update()
            # If mode=1, automatic update
            a.mode=1
            #If mode=0, use update function to update the VCS Canvas.
            a.mode=0



    .. describe:: General use of a colormap:

        .. code-block:: python

            # Create a VCS Canvas object
            a=vcs.init()
            #To Create a new instance of colormap use:
            # Copies content of 'red' to 'new'
            cp=a.createcolormap('new','quick')
            # Copies content of 'default' to 'new'
            cp=a.createcolormap('new')

    .. describe:: Modifying an existing colormap:

        .. code-block:: python

            cp=a.getcolormap('quick')

    .. describe:: Overview of colormap attributes:

        * List all the colormap indices and R,G,B attribute values

            .. code-block:: python

                cp.list()

        * Setting colormap attribute values:

            .. code-block:: python

                # Index, R, G, B
                cp.color=16,100,0,0
                # Index range from 0 to 255, but can only modify from 0 to 239
                cp.color=16,0,100,0
                # R, G, B values range from 0 to 100, where 0 is low intensity and 100 is highest intensity
                cp.color=17,0,0,100

    """
    __slots__ = ["s_name", "name", "_name", "index", "_index"]

    def getname(self):
        return self._name

    def setname(self, value):
        if value == "default":
            raise Exception("you cannot modify default colormap")
        vcs.elements[value] = vcs.elements[self.name]
        del(vcs.elements[self.name])
        self._name = value
    name = property(getname, setname)

    def getindex(self):
        if len(self._index) == 3:
            return self._index + [100.]
        return self._index

    def setindex(self, value):
        # usually we cannot set index, but there is an exception for lading
        # from json files
        if not(isinstance(value, dict) and value.keys() == [u'data', ]):
            raise Exception("invalid")
        else:
            d2 = {}
            d = value[u'data']
            for k in d.keys():
                if len(d[k]) == 3:  # Old style only r,g,b no a
                    d[k] += [100.]
                d2[int(k)] = d[k]
            self.index.data.update(d2)
    index = property(getindex, setindex)
    ##########################################################################
    #                                                                           #
    # Initialize the colormap attributes.                                       #
    #                                                                           #
    ##########################################################################

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
        self.s_name = 'Cp'
        self._name = Cp_name
        if Cp_name == "default":
            self._index = RGB_Table(self._name)  # Create colormap dictionary
        else:
            if isinstance(Cp_name_src, Cp):
                Cp_name_src = Cp_name_src.name
            if Cp_name_src not in vcs.elements["colormap"]:
                raise Exception(
                    "Error source colormap '%s' does not exists" %
                    Cp_name_src)
            src = vcs.elements["colormap"][Cp_name_src]
            self._index = copy.deepcopy(src.index)
            self._index.name = Cp_name
        vcs.elements["colormap"][Cp_name] = self

        # Note: See RGB_Table Class for "index" setting of the colormap entries

    # Set a colorcell RGB
    def setcolorcell(self, index, red, green, blue, alpha=100.):
        """
        Sets the R,G,B,A values of a colorcell

        :Example:

::

        #Create a vcs Canvas
        a = vcs.init()
        #Create a colormap
        cmap = a.createcolormap('example', 'default')
        #Set RGBA values
        cmap.setcolorcell(1,255,255,255,1.0)

:param index: Integer from 0-255.
:type index: int

:param red: Integer from 0-255 representing the concentration of red in the colorcell.
:type red: int

:param green: Integer from 0-255 representing the concentration of green in the colorcell.
:type green: int

:param blue: Integer from 0-255 representing the concentration of blue in the colorcell.
:type blue: int

:param alpha: Float representing the percentage of opacity in the colorcell.
:type alpha: float

:returns:
:rtype:
"""
        self.index[index] = [red, green, blue, alpha]

    # get a colorcell RGB
    def getcolorcell(self, index):
        """
        Gets the R,G,B,A values of a colorcell.

        :Example:

    ::

        #Create a vcs Canvas
        a = vcs.init()
        #Create a colormap
        cmap = a.createcolormap('example', 'default')
        #Get RGBA values
        cmap.getcolorcell(1)

:param index: Index of a cell in the colormap. Must be an integer from 0-255.
:type index: int

:returns: A list containing the red, green, blue, and alpha values (in that order), of the colorcell at the given index.
:rtype: list
        """
        return self.index[index]

    ##########################################################################
    #                                                                           #
    # List out colormap members (attributes).                                   #
    #                                                                           #
    ##########################################################################
    def list(self):
        if (self.name == '__removed_from_VCS__'):
            raise ValueError('This instance has been removed from VCS.')
        print "", "----------Colormap (Cp) member (attribute) listings ----------"
        print "secondary method =", self.s_name
        print "name =", self.name
        print "index =", self.index

    ##########################################################################
    #                                                                           #
    # Script out secondary colormap method in VCS to a file.                    #
    #                                                                           #
    ##########################################################################
    def script(self, script_filename=None, mode=None):
        if (script_filename is None):
            raise ValueError(
                'Error - Must provide an output script file name.')

        if (mode is None):
            mode = 'a'
        elif (mode not in ('w', 'a')):
            raise ValueError(
                'Error - Mode can only be "w" for replace or "a" for append.')

        # By default, save file in json
        scr_type = script_filename.split(".")
        if len(scr_type) == 1 or len(scr_type[-1]) > 5:
            scr_type = "json"
            if script_filename != "initial.attributes":
                script_filename += ".json"
        else:
            scr_type = scr_type[-1]
        if scr_type == '.scr':
            raise DeprecationWarning("scr script are no longer generated")
        elif scr_type == ".py":
            mode = mode + '+'
            py_type = script_filename[
                len(script_filename) -
                3:len(script_filename)]
            if (py_type != '.py'):
                script_filename = script_filename + '.py'

            # Write to file
            fp = open(script_filename, mode)
            if (fp.tell() == 0):  # Must be a new file, so include below
                fp.write("#####################################\n")
                fp.write("#                                 #\n")
                fp.write("# Import and Initialize VCS     #\n")
                fp.write("#                             #\n")
                fp.write("#############################\n")
                fp.write("import vcs\n")
                fp.write("v=vcs.init()\n\n")

            unique_name = '__Cp__' + self.name
            fp.write(
                "#----------Colormap (Cp) member (attribute) listings ----------\n")
            fp.write("tl_list=v.listelements('colormap')\n")
            fp.write("if ('%s' in tl_list):\n" % self.name)
            fp.write(
                "   %s = v.getcolormap('%s')\n" %
                (unique_name, self.name))
            fp.write("else:\n")
            fp.write(
                "   %s = v.createcolormap('%s')\n" %
                (unique_name, self.name))
            fp.write("%s.index = '%s'\n" % (unique_name, self.index))
        else:
            # Json type
            mode += "+"
            f = open(script_filename, mode)
            vcs.utils.dumpToJson(self, f)
            f.close()

    script.__doc__ = xmldocs.colormap_script
##########################################################################
#        END OF FILE                                                            #
##########################################################################
