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
        self.data = {
                0: [26, 0, 32, 100], 1: [26, 0, 33, 100], 2: [26, 1, 34, 100],
                3: [27, 1, 34, 100], 4: [27, 2, 35, 100], 5: [27, 3, 35, 100],
                6: [27, 3, 36, 100], 7: [27, 4, 37, 100], 8: [27, 5, 37, 100],
                9: [27, 5, 38, 100], 10: [27, 6, 38, 100], 11: [27, 6, 39, 100],
                12: [28, 7, 39, 100], 13: [28, 7, 40, 100], 14: [28, 8, 40, 100],
                15: [28, 8, 41, 100], 16: [28, 9, 41, 100], 17: [28, 10, 42, 100],
                18: [28, 10, 42, 100], 19: [28, 11, 43, 100], 20: [28, 11, 43, 100],
                21: [28, 12, 44, 100], 22: [28, 12, 44, 100], 23: [28, 13, 44, 100],
                24: [28, 13, 45, 100], 25: [28, 14, 45, 100], 26: [28, 14, 46, 100],
                27: [28, 15, 46, 100], 28: [28, 15, 46, 100], 29: [28, 16, 47, 100],
                30: [28, 16, 47, 100], 31: [27, 17, 47, 100], 32: [27, 17, 48, 100],
                33: [27, 18, 48, 100], 34: [27, 18, 48, 100], 35: [27, 19, 49, 100],
                36: [27, 19, 49, 100], 37: [27, 19, 49, 100], 38: [27, 20, 50, 100],
                39: [27, 20, 50, 100], 40: [27, 21, 50, 100], 41: [26, 21, 50, 100],
                42: [26, 22, 51, 100], 43: [26, 22, 51, 100], 44: [26, 23, 51, 100],
                45: [26, 23, 51, 100], 46: [26, 24, 52, 100], 47: [26, 24, 52, 100],
                48: [25, 25, 52, 100], 49: [25, 25, 52, 100], 50: [25, 26, 52, 100],
                51: [25, 26, 52, 100], 52: [25, 26, 53, 100], 53: [25, 27, 53, 100],
                54: [24, 27, 53, 100], 55: [24, 28, 53, 100], 56: [24, 28, 53, 100],
                57: [24, 29, 53, 100], 58: [24, 29, 53, 100], 59: [23, 30, 54, 100],
                60: [23, 30, 54, 100], 61: [23, 30, 54, 100], 62: [23, 31, 54, 100],
                63: [23, 31, 54, 100], 64: [22, 32, 54, 100], 65: [22, 32, 54, 100],
                66: [22, 33, 54, 100], 67: [22, 33, 54, 100], 68: [22, 33, 54, 100],
                69: [22, 34, 54, 100], 70: [21, 34, 55, 100], 71: [21, 35, 55, 100],
                72: [21, 35, 55, 100], 73: [21, 35, 55, 100], 74: [21, 36, 55, 100],
                75: [20, 36, 55, 100], 76: [20, 37, 55, 100], 77: [20, 37, 55, 100],
                78: [20, 37, 55, 100], 79: [20, 38, 55, 100], 80: [19, 38, 55, 100],
                81: [19, 39, 55, 100], 82: [19, 39, 55, 100], 83: [19, 39, 55, 100],
                84: [19, 40, 55, 100], 85: [19, 40, 55, 100], 86: [18, 41, 55, 100],
                87: [18, 41, 55, 100], 88: [18, 41, 55, 100], 89: [18, 42, 55, 100],
                90: [18, 42, 55, 100], 91: [18, 42, 55, 100], 92: [17, 43, 55, 100],
                93: [17, 43, 55, 100], 94: [17, 44, 55, 100], 95: [17, 44, 55, 100],
                96: [17, 44, 55, 100], 97: [17, 45, 55, 100], 98: [16, 45, 55, 100],
                99: [16, 45, 55, 100], 100: [16, 46, 55, 100], 101: [16, 46, 55, 100],
                102: [16, 47, 55, 100], 103: [16, 47, 55, 100], 104: [16, 47, 55, 100],
                105: [15, 48, 55, 100], 106: [15, 48, 55, 100], 107: [15, 48, 55, 100],
                108: [15, 49, 55, 100], 109: [15, 49, 55, 100], 110: [15, 50, 55, 100],
                111: [15, 50, 55, 100], 112: [14, 50, 55, 100], 113: [14, 51, 55, 100],
                114: [14, 51, 55, 100], 115: [14, 51, 55, 100], 116: [14, 52, 55, 100],
                117: [14, 52, 55, 100], 118: [14, 53, 55, 100], 119: [13, 53, 55, 100],
                120: [13, 53, 55, 100], 121: [13, 54, 55, 100], 122: [13, 54, 55, 100],
                123: [13, 54, 55, 100], 124: [13, 55, 55, 100], 125: [13, 55, 55, 100],
                126: [12, 55, 55, 100], 127: [12, 56, 55, 100], 128: [12, 56, 55, 100],
                129: [12, 57, 54, 100], 130: [12, 57, 54, 100], 131: [12, 57, 54, 100],
                132: [12, 58, 54, 100], 133: [12, 58, 54, 100], 134: [12, 58, 54, 100],
                135: [12, 59, 54, 100], 136: [12, 59, 54, 100], 137: [12, 60, 54, 100],
                138: [11, 60, 54, 100], 139: [11, 60, 54, 100], 140: [11, 61, 53, 100],
                141: [11, 61, 53, 100], 142: [11, 61, 53, 100], 143: [12, 62, 53, 100],
                144: [12, 62, 53, 100], 145: [12, 62, 53, 100], 146: [12, 63, 53, 100],
                147: [12, 63, 52, 100], 148: [12, 64, 52, 100], 149: [12, 64, 52, 100],
                150: [12, 64, 52, 100], 151: [13, 65, 52, 100], 152: [13, 65, 51, 100],
                153: [13, 65, 51, 100], 154: [13, 66, 51, 100], 155: [14, 66, 51, 100],
                156: [14, 66, 51, 100], 157: [14, 67, 50, 100], 158: [15, 67, 50, 100],
                159: [15, 68, 50, 100], 160: [15, 68, 50, 100], 161: [16, 68, 49, 100],
                162: [16, 69, 49, 100], 163: [17, 69, 49, 100], 164: [17, 69, 49, 100],
                165: [18, 70, 48, 100], 166: [18, 70, 48, 100], 167: [19, 70, 48, 100],
                168: [19, 71, 47, 100], 169: [20, 71, 47, 100], 170: [20, 71, 47, 100],
                171: [21, 72, 46, 100], 172: [22, 72, 46, 100], 173: [22, 72, 46, 100],
                174: [23, 73, 45, 100], 175: [23, 73, 45, 100], 176: [24, 73, 45, 100],
                177: [25, 74, 44, 100], 178: [25, 74, 44, 100], 179: [26, 74, 44, 100],
                180: [27, 75, 43, 100], 181: [28, 75, 43, 100], 182: [28, 75, 42, 100],
                183: [29, 76, 42, 100], 184: [30, 76, 41, 100], 185: [31, 76, 41, 100],
                186: [31, 77, 41, 100], 187: [32, 77, 40, 100], 188: [33, 77, 40, 100],
                189: [34, 78, 39, 100], 190: [35, 78, 39, 100], 191: [36, 78, 38, 100],
                192: [36, 78, 38, 100], 193: [37, 79, 37, 100], 194: [38, 79, 37, 100],
                195: [39, 79, 36, 100], 196: [40, 80, 36, 100], 197: [41, 80, 35, 100],
                198: [42, 80, 35, 100], 199: [43, 80, 34, 100], 200: [44, 81, 34, 100],
                201: [44, 81, 33, 100], 202: [45, 81, 32, 100], 203: [46, 81, 32, 100],
                204: [47, 82, 31, 100], 205: [48, 82, 31, 100], 206: [49, 82, 30, 100],
                207: [50, 82, 30, 100], 208: [51, 83, 29, 100], 209: [52, 83, 28, 100],
                210: [53, 83, 28, 100], 211: [54, 83, 27, 100], 212: [55, 84, 26, 100],
                213: [56, 84, 26, 100], 214: [57, 84, 25, 100], 215: [58, 84, 24, 100],
                216: [59, 84, 24, 100], 217: [60, 85, 23, 100], 218: [61, 85, 23, 100],
                219: [62, 85, 22, 100], 220: [63, 85, 21, 100], 221: [64, 85, 20, 100],
                222: [65, 86, 20, 100], 223: [66, 86, 19, 100], 224: [67, 86, 18, 100],
                225: [68, 86, 18, 100], 226: [69, 86, 17, 100], 227: [70, 86, 16, 100],
                228: [72, 87, 16, 100], 229: [73, 87, 15, 100], 230: [74, 87, 14, 100],
                231: [75, 87, 14, 100], 232: [76, 87, 13, 100], 233: [77, 87, 13, 100],
                234: [78, 87, 12, 100], 235: [79, 88, 12, 100], 236: [80, 88, 11, 100],
                237: [81, 88, 11, 100], 238: [82, 88, 10, 100], 239: [83, 88, 10, 100],
                240: [84, 88, 9, 100], 241: [85, 88, 9, 100], 242: [86, 88, 9, 100],
                243: [87, 89, 9, 100], 244: [88, 89, 9, 100], 245: [89, 89, 9, 100],
                246: [90, 89, 9, 100], 247: [91, 89, 10, 100], 248: [92, 89, 10, 100],
                249: [93, 89, 10, 100], 250: [94, 89, 11, 100], 251: [95, 90, 11, 100],
                252: [96, 90, 12, 100], 253: [97, 90, 13, 100], 254: [98, 90, 13, 100],
                255: [99, 90, 14, 100]}

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
