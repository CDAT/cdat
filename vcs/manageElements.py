# This file aims at removing elets creation from dpeending on a Canvas, we will try to simply have
# b = vcs.createboxfill()
# rather than
# x=vcs.init()
# b=x.createboxfill()
import vcs
import boxfill
import meshfill
import isofill
import isoline
import unified1D
import template
import projection
import colormap
import fillarea
import marker
import line
import texttable
import textorientation
import textcombined
import vector
import xmldocs
import random
from error import vcsError
import warnings
import dv3d


def check_name_source(name, source, typ):
    """make sure it is a unique name for this type or generates a name for user"""
    elts = vcs.listelements(typ)
    if name is None:
        rnd = random.randint(0, 1000000000000000)
        name = '__%s_%i' % (typ, rnd)
        while name in elts:
            rnd = random.randint(0, 1000000000000000)
            name = '__%s_%i' % (typ, rnd)
    if isinstance(name, unicode):
        name = str(name)
    if not isinstance(name, str):
        raise vcsError(
            '%s object name must be a string or %s name' %
            (typ, typ))

    if not isinstance(source, str):
        exec("ok = vcs.is%s(source)" % (typ,))
    else:
        ok = 0
    if (not isinstance(source, str)) and ok == 0:
        raise vcsError(
            'Error %s object source must be a string or a %s object' %
            (typ, typ))
    elif ok:
        source = source.name

    if name in elts:
        raise vcsError("Error %s object named %s already exists" % (typ, name))
    if source not in elts and typ != "display":
        raise vcsError(
            "Error source %s object (%s) does not exist!" %
            (typ, source))
    return name, source


def createtemplate(name=None, source='default'):
    """
    Create a new template given the the name and the existing template to copy
    the attributes from. If no existing template name is given, then the default
    template will be used as the template to which the attributes will be copied
    from.

    If the name provided already exists, then an error will be returned. Template
    names must be unique.

    :Example:

        .. doctest:: manageElements_createtemplate

            >>> con=vcs.createtemplate('example1') # create 'example1' template from 'default' template
            >>> vcs.listelements('template') # Show all the existing templates
            [... example1, ...]
            >>> con=vcs.createtemplate('example2','quick') # create 'example2' from 'quick' template
            >>> vcs.listelements('template') # Show all the existing templates
            [..., example2, ...]

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a template or a string name of a template

    :returns: A template
    :rtype: vcs.template.P

    """
    name, source = check_name_source(name, source, 'template')

    return template.P(name, source)


def gettemplate(Pt_name_src='default'):
    """
    %s

    :param Pt_name_src: String name of an existing template VCS object
    :type Pt_name_src:

    :returns: A VCS template object
    :rtype: vcs.template.P
    """
    # Check to make sure the argument passed in is a STRING
    if not isinstance(Pt_name_src, str):
        raise vcsError('The argument must be a string.')

    if Pt_name_src not in vcs.elements["template"].keys():
        raise ValueError("template '%s' does not exists" % Pt_name_src)
    return vcs.elements["template"][Pt_name_src]
gettemplate.__doc__ = gettemplate.__doc__ % xmldocs.get_docs['template']


def createprojection(name=None, source='default'):
    """
    Create a new projection method given the the name and the existing
    projection method to copy the attributes from. If no existing
    projection method name is given, then the default projection
    method will be used as the projection method to which the attributes will
    be copied from.

    If the name provided already exists, then an error will be returned. Projection
    method names must be unique.

    :Example:

        .. doctest:: manageElements_createprojection

            >>> vcs.show('projection')
            *******************Projection Names List**********************
            ...
            *******************End Projection Names List**********************
            >>> p=vcs.createprojection('example1',)
            >>> vcs.show('projection')
            *******************Projection Names List**********************
            ...
            *******************End Projection Names List**********************
            >>> p=vcs.createprojection('example2','polar')
            >>> vcs.show('projection')
            *******************Projection Names List**********************
            ...
            *******************End Projection Names List**********************

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a projection or a string name of a projection

    :returns: A projection graphics method object
    :rtype: vcs.projection.Proj
    """

    name, source = check_name_source(name, source, 'projection')
    return projection.Proj(name, source)


def getprojection(Proj_name_src='default'):
    """
    %s

    :param Proj_name_src: String name of an existing VCS projection object
    :type Proj_name_src: str

    :returns: A VCS projection object
    :rtype: vcs.projection.Proj
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Proj_name_src, str):
        raise vcsError('The argument must be a string.')

    if Proj_name_src not in vcs.elements["projection"]:
        raise vcsError("No such projection '%s'" % Proj_name_src)
    return vcs.elements["projection"][Proj_name_src]
getprojection.__doc__ = getprojection.__doc__ % xmldocs.get_docs['projection']


def createboxfill(name=None, source='default'):
    """

    Create a new boxfill graphics method given the the name and the existing
    boxfill graphics method to copy the attributes from. If no existing
    boxfill graphics method name is given, then the default boxfill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_createboxfill

            >>> vcs.show('boxfill')
            *******************Boxfill Names List**********************
            ...
            *******************End Boxfill Names List**********************
            >>> box=vcs.createboxfill('example1')
            >>> vcs.show('boxfill')
            *******************Boxfill Names List**********************
            ...
            *******************End Boxfill Names List**********************
            >>> box=vcs.createboxfill('example2','polar')
            >>> vcs.show('boxfill')
            *******************Boxfill Names List**********************
            ...
            *******************End Boxfill Names List**********************

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a boxfill or a string name of a boxfill

    :return: A boxfill graphics method object
    :rtype: vcs.boxfill.Gfb

    %s
    %s
    %s
    %s
    %s
    """

    name, source = check_name_source(name, source, 'boxfill')
    return boxfill.Gfb(name, source)
createboxfill.__doc__ = createboxfill.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.create_GM_input, xmldocs.boxfill_output)


def getboxfill(Gfb_name_src='default'):
    """
    %s

    :param Gfb_name_src: String name of an existing boxfill VCS object
    :type Gfb_name_src: str

    :return: A pre-existing boxfill graphics method
    :rtype: vcs.boxfill.Gfb

    %s
    %s
    %s
    %s
    %s
    """
    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfb_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gfb_name_src not in vcs.elements["boxfill"].keys():
        raise "The boxfill method: '%s' does not seem to exist"
    return vcs.elements["boxfill"][Gfb_name_src]
getboxfill.__doc__ = getboxfill.__doc__ % (
    xmldocs.get_docs['boxfill'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.boxfill_output)


def createtaylordiagram(name=None, source='default'):
    """
    Create a new taylordiagram graphics method given the the name and the existing
    taylordiagram graphics method to copy the attributes from. If no existing
    taylordiagram graphics method name is given, then the default taylordiagram graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    If the name provided already exists, then an error will be returned. Graphics
    method names must be unique.

    :Example:

        .. doctest:: manageElements_createtaylordiagram

            >>> vcs.show('taylordiagram') # show all available taylordiagrams
            *******************Taylordiagram Names List**********************
            ...
            *******************End Taylordiagram Names List**********************
            >>> td=vcs.createtaylordiagram('example1') # Create taylordiagram 'example1' that inherits from 'default'
            >>> vcs.show('taylordiagram') # should now contain the 'example1' taylordiagram
            *******************Taylordiagram Names List**********************
            ...
            *******************End Taylordiagram Names List**********************

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a taylordiagram or a string name of a

    :returns: A taylordiagram graphics method object
    :rtype: vcs.taylor.Gtd
    """

    name, source = check_name_source(name, source, 'taylordiagram')
    if name in vcs.elements["taylordiagram"].keys():
        raise vcsError(
            'Error creating taylordiagram graphic method: ' +
            name +
            ' already exist')
    if source not in vcs.elements["taylordiagram"].keys():
        raise vcsError(
            'Error creating taylordiagram graphic method ' +
            source +
            ' does not exist')
    n = vcs.taylor.Gtd(name, source)
    return n
# createtaylordiagram.__doc__ = xmldocs.create_taylor_doc


def gettaylordiagram(Gtd_name_src='default'):
    """
    %s

    :param Gtd_name_src: String name of an existing taylordiagram VCS object
    :type Gtd_name_src: str

    :returns: A taylordiagram VCS object
    :rtype: vcs.taylor.Gtd
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gtd_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gtd_name_src not in vcs.elements["taylordiagram"].keys():
        raise vcsError(
            "The taylordiagram graphic method %s does not exists" %
            Gtd_name_src)
    else:
        return vcs.elements["taylordiagram"][Gtd_name_src]
gettaylordiagram.__doc__ = gettaylordiagram.__doc__ % xmldocs.get_docs['taylordiagram']

def createmeshfill(name=None, source='default'):
    """
    Create a new meshfill graphics method given the the name and the existing
    meshfill graphics method to copy the attributes from. If no existing
    meshfill graphics method name is given, then the default meshfill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_createmeshfill

            >>> vcs.show('meshfill') # list names of all meshfill objects
            *******************Meshfill Names List**********************
            ...
            *******************End Meshfill Names List**********************
            >>> mesh=vcs.createmeshfill('example1') # inherits from default meshfill
            >>> vcs.show('meshfill') # should no
            *******************Meshfill Names List**********************
            ...
            *******************End Meshfill Names List**********************
            >>> mesh=vcs.createmeshfill('example2','quick')
            >>> vcs.show('meshfill')
            *******************Meshfill Names List**********************
            ...
            *******************End Meshfill Names List**********************

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a meshfill or a string name of a meshfill

    :returns: A meshfill graphics method object
    :rtype: vcs.meshfill.Gfm
    """
    name, source = check_name_source(name, source, 'meshfill')
    return meshfill.Gfm(name, source)


def getmeshfill(Gfm_name_src='default'):
    """
    %s

    :param Gfm_name_src: String name of an existing meshfill VCS object
    :type Gfm_name_src: str

    :returns: A meshfill VCS object
    :rtype: vcs.meshfill.Gfm
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfm_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gfm_name_src not in vcs.elements["meshfill"]:
        raise ValueError("meshfill '%s' does not exists" % Gfm_name_src)

    return vcs.elements["meshfill"][Gfm_name_src]
getmeshfill.__doc__ = getmeshfill.__doc__ % xmldocs.get_docs['meshfill']


def createisofill(name=None, source='default'):
    """

    Create a new isofill graphics method given the the name and the existing
    isofill graphics method to copy the attributes from. If no existing
    isofill graphics method name is given, then the default isofill graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        vcs.show('isofill')
        iso=vcs.createisofill('example1')
        vcs.show('isofill')
        iso=vcs.createisofill('example2','quick')
        vcs.show('isofill')

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: an isofill object, or string name of an isofill object

    :returns: An isofill graphics method
    :rtype: vcs.isofill.Gfi

    %s
    %s
    %s
    %s
    %s
    """

    name, source = check_name_source(name, source, 'isofill')
    return isofill.Gfi(name, source)
createisofill.__doc__ = createisofill.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert, xmldocs.create_GM_input, xmldocs.isofill_output)


def getisofill(Gfi_name_src='default'):
    """
    %s

    :param Gfi_name_src: String name of an existing isofill VCS object
    :type Gfi_name_src: str

    :returns: The specified isofill VCS object
    :rtype: vcs.isofill.Gfi

    %s
    %s
    %s
    %s
    %s
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfi_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gfi_name_src not in vcs.elements["isofill"]:
        raise ValueError("The isofill '%s' does not exists" % Gfi_name_src)
    return vcs.elements["isofill"][Gfi_name_src]
getisofill.__doc__ = getisofill.__doc__ % (
    xmldocs.get_docs['isofill'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.isofill_output)


def createisoline(name=None, source='default'):
    """

    Create a new isoline graphics method given the the name and the existing
    isoline graphics method to copy the attributes from. If no existing
    isoline graphics method name is given, then the default isoline graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        vcs.show('isoline')
        iso=vcs.createisoline('example1')
        vcs.show('isoline')
        iso=vcs.createisoline('example2','quick')
        vcs.show('isoline')

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: an isoline object, or string name of an isoline object

    :returns: An isoline graphics method object
    :rtype: vcs.isoline.Gi

    %s
    %s
    %s
    %s
    %s
    """

    name, source = check_name_source(name, source, 'isoline')
    return isoline.Gi(name, source)
createisoline.__doc__ = createisoline.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert, xmldocs.create_GM_input, xmldocs.isoline_output)


def getisoline(Gi_name_src='default'):
    """
    %s

    :param Gi_name_src: String name of an existing isoline VCS object
    :type Gi_name_src: str

    :returns: The requested isoline VCS object
    :rtype: vcs.isoline.Gi

    %s
    %s
    %s
    %s
    %s
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gi_name_src, str):
        raise vcsError('The argument must be a string.')
    if Gi_name_src not in vcs.elements["isoline"]:
        raise ValueError("The isoline '%s' does not exists" % Gi_name_src)
    return vcs.elements["isoline"][Gi_name_src]
getisoline.__doc__ = getisoline.__doc__ % (
    xmldocs.get_docs['isoline'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.isoline_output)


def create1d(name=None, source='default'):
    name, source = check_name_source(name, source, '1d')
    return unified1D.G1d(name, source)


def get1d(name):
    # Check to make sure the argument passed in is a STRING
    if not isinstance(name, str):
        raise vcsError('The argument must be a string.')

    if name not in vcs.elements["1d"]:
        raise ValueError("The 1d '%s' graphics method does not exists" % name)
    return vcs.elements["1d"][name]


def createxyvsy(name=None, source='default'):
    """
    Create a new Xyvsy graphics method given the the name and the existing
    Xyvsy graphics method to copy the attributes from. If no existing
    Xyvsy graphics method name is given, then the default Xyvsy graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        a=vcs.init()
        vcs.show('xyvsy')
        xyy=vcs.createxyvsy('example1',)
        vcs.show('xyvsy')
        xyy=vcs.createxyvsy('example2','quick')
        vcs.show('xyvsy')


    :param name: The name of the created object
    :type name: str


    :param source: The object to inherit from
    :type source: a xyvsy or a string name of a xyvsy

    :returns: A XYvsY graphics method object
    :rtype: vcs.unified1D.G1d

    %s
    %s
    %s
    %s
    %s
    """
    try:
        gm = vcs.create1d(name, source)
    except vcsError as ve:
        if ve.message == "Error 1d object named %s already exists" % name:
            warning_message = "A 1D graphics method named %s already exists, creating yours as %s" % (name,
                                                                                                      name + "_xyvsy")
            warnings.warn(warning_message)
            gm = vcs.create1d(name + "_xyvsy", source)
        else:
            raise ve
    gm.flip = True
    return gm
createxyvsy.__doc__ = createxyvsy.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert, xmldocs.create_GM_input, xmldocs.xyvsy_output)


def getxyvsy(GXy_name_src='default'):
    """
    %s

    :param GXy_name_src: String name of an existing Xyvsy graphics method
    :type GXy_name_src: str

    :returns: An XYvsY graphics method object
    :rtype: vcs.unified1D.G1d

    %s
    %s
    %s
    %s
    %s
    """
    gm = vcs.get1d(GXy_name_src)
    if gm.g_type != "xyvsy":
        # Already existed when name_src was created, most likely
        return vcs.get1d(GXy_name_src + "_xyvsy")
    return gm
getxyvsy.__doc__ = getxyvsy.__doc__ % (
    xmldocs.get_docs['xyvsy'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.xyvsy_output)


def createyxvsx(name=None, source='default'):
    """
    Create a new Yxvsx graphics method given the the name and the existing
    Yxvsx graphics method to copy the attributes from. If no existing
    Yxvsx graphics method name is given, then the default Yxvsx graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        a=vcs.init()
        vcs.show('yxvsx')
        yxx=vcs.createyxvsx('example1',)
        vcs.show('yxvsx')
        yxx=vcs.createyxvsx('example2','quick')
        vcs.show('yxvsx')


    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a yxvsy or a string name of a yxvsy

    :returns: A YXvsX graphics method object
    :rtype: vcs.unified1D.G1d

    %s
    %s
    %s
    %s
    %s
    """
    try:
        gm = vcs.create1d(name, source)
    except vcsError as ve:
        if ve.message == "Error 1d object named %s already exists" % name:
            warning_message = "A 1D graphics method named %s already exists, creating yours as %s" % (name,
                                                                                                      name + "_yxvsx")
            warnings.warn(warning_message)
            gm = vcs.create1d(name + "_yxvsx", source)
        else:
            raise ve
    return gm
createyxvsx.__doc__ = createyxvsx.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert, xmldocs.create_GM_input, xmldocs.yxvsx_output)


def getyxvsx(GYx_name_src='default'):
    """
    %s

    :param GYx_name_src: String name of an existing Yxvsx graphics method
    :type GYx_name_src: str

    :return: A Yxvsx graphics method object
    :rtype: vcs.unified1D.G1d

    %s
    %s
    %s
    %s
    %s
    """
    gm = vcs.get1d(GYx_name_src)
    if gm.g_type != "yxvsx":
        return vcs.get1d(GYx_name_src + "_yxvsx")
    return gm
getyxvsx.__doc__ = getyxvsx.__doc__ % (
    xmldocs.get_docs['yxvsx'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.yxvsx_output)


def createxvsy(name=None, source='default'):
    """
    Create a new XvsY graphics method given the the name and the existing
    XvsY graphics method to copy the attributes from. If no existing
    XvsY graphics method name is given, then the default XvsY graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::

        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        a=vcs.init()
        vcs.show('xvsy')
        xy=vcs.createxvsy('example1',)
        vcs.show('xvsy')
        xy=vcs.createxvsy('example2','quick')
        vcs.show('xvsy')

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a xvsy or a string name of a xvsy

    :returns: A XvsY graphics method object
    :rtype: vcs.unified1D.G1d

    %s
    %s
    %s
    %s
    %s
    """
    try:
        gm = vcs.create1d(name, source)
    except vcsError as ve:
        if ve.message == "Error 1d object named %s already exists" % name:
            warning_message = "A 1D graphics method named %s already exists, creating yours as %s" % (name,
                                                                                                      name + "_xvsy")
            warnings.warn(warning_message)
            gm = vcs.create1d(name + "_xvsy", source)
        else:
            raise ve
    return gm
createxvsy.__doc__ = createxvsy.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert, xmldocs.create_GM_input, xmldocs.xvsy_output)


def getxvsy(GXY_name_src='default'):
    """
    %s

    :param GXY_name_src: String name of a 1d graphics method
    :type GXY_name_src: str

    :returns: A XvsY graphics method object
    :rtype: vcs.unified1D.G1d
    %s
    %s
    %s
    %s
    %s
    """
    gm = vcs.get1d(GXY_name_src)
    # Deliberately yxvsx here; xvsy is just an alias
    if gm.g_type != "yxvsx":
        return vcs.get1d(GXY_name_src + "_xvsy")
    return gm
getxvsy.__doc__ = getxvsy.__doc__ % (
    xmldocs.get_docs['xvsy'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.xvsy_output)


def createvector(name=None, source='default'):
    """
    Create a new vector graphics method given the the name and the existing
    vector graphics method to copy the attributes from. If no existing
    vector graphics method name is given, then the default vector graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::
        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        a=vcs.init()
        vcs.show('vector')
        vec=vcs.createvector('example1',)
        vcs.show('vector')
        vec=vcs.createvector('example2','quick')
        vcs.show('vector')


:param name: The name of the created object
:type name: str

:param source: The object to inherit from
:type source: a vector or a string name of a vector

:returns: A vector graphics method object
:rtype: vcs.vector.Gv

    """
    name, source = check_name_source(name, source, 'vector')
    return vector.Gv(name, source)


def getvector(Gv_name_src='default'):
    """
    %s

    :param Gv_name_src: String name of an existing vector VCS object
    :type Gv_name_src: str

    :returns: A vector graphics method object
    :rtype: vcs.vector.Gv
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gv_name_src, str):
        raise vcsError('The argument must be a string.')
    if Gv_name_src not in vcs.elements["vector"]:
        raise ValueError("The vector '%s' does not exist" % Gv_name_src)
    return vcs.elements["vector"][Gv_name_src]
getvector.__doc__ = getvector.__doc__ % xmldocs.get_docs['vector']


def createscatter(name=None, source='default'):
    """

    Create a new scatter graphics method given the the name and the existing
    scatter graphics method to copy the attributes from. If no existing
    scatter graphics method name is given, then the default scatter graphics
    method will be used as the graphics method to which the attributes will
    be copied from.


    .. note::
        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    vcs.show('scatter')
    sct=vcs.createscatter('example1',)
    vcs.show('scatter')
    sct=vcs.createscatter('example2','quick')
    vcs.show('scatter')

:param name: The name of the created object
:type name: str

:param source: The object to inherit from
:type source: a scatter or a string name of a scatter

:return: A scatter graphics method
:rtype: vcs.unified1D.G1d

%s
%s
%s
%s
%s
"""
    try:
        gm = vcs.create1d(name, source)
    except vcsError as ve:
        if ve.message == "Error 1d object named %s already exists" % name:
            warning_message = "A 1D graphics method named %s already exists, creating yours as %s" % (name,
                                                                                                      name + "_scatter")
            warnings.warn(warning_message)
            gm = vcs.create1d(name + "_scatter", source)
        else:
            raise ve
    gm.linewidth = 0
    return gm
createscatter.__doc__ = createscatter.__doc__ % (
    xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert, xmldocs.create_GM_input, xmldocs.scatter_output)


def getscatter(GSp_name_src='default'):
    """
    %s

    :param GSp_name_src: String name of an existing scatter VCS object.
    :type GSp_name_src: str

    :returns: A scatter graphics method object
    :rtype: vcs.unified1D.G1d

    %s
    %s
    %s
    %s
    %s
    """
    gm = vcs.get1d(GSp_name_src)
    if gm.g_type != "scatter":
        return vcs.get1d(GSp_name_src + "_scatter")
    return gm
getscatter.__doc__ = getscatter.__doc__ % (
    xmldocs.get_docs['scatter'], xmldocs.plot_keywords_doc, xmldocs.graphics_method_core, xmldocs.axesconvert,
    xmldocs.get_GM_input, xmldocs.scatter_output)


def createline(name=None, source='default', ltype=None,
               width=None, color=None, priority=None,
               viewport=None, worldcoordinate=None,
               x=None, y=None, projection=None):
    """
    Create a new line secondary method given the the name and the existing
    line secondary method to copy the attributes from. If no existing line
    secondary method name is given, then the default line secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    vcs.show('line')
    ln=vcs.createline('example1')
    vcs.show('line')
    ln=vcs.createline('example2','black')
    vcs.show('line')
    # Create instance of line object 'red'
    ln2=vcs.createline(name='new', name_src='red',ltype='dash', width=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )
    # Plot using specified line object
    vcs.line(ln2)

:param name: Name of created object
:type name: str

:param source: a line, or string name of a line
:type source: str

:param ltype: One of "dash", "dash-dot", "solid", "dot", or "long-dash".
:type ltype: str

:param width: Thickness of the line to be created
:type width: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the line will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:param projection: Specify a geographic projection used to convert x/y from spherical coordinates into 2D coordinates.
:type projection: str or projection object

:returns: A VCS line secondary method object
:rtype: vcs.line.Tl

"""

    name, source = check_name_source(name, source, 'line')

    ln = line.Tl(name, source)
    if (ltype is not None):
        ln.type = ltype
    if (width is not None):
        ln.width = width
    if (color is not None):
        ln.color = color
    if (priority is not None):
        ln.priority = priority
    if (viewport is not None):
        ln.viewport = viewport
    if (worldcoordinate is not None):
        ln.worldcoordinate = worldcoordinate
    if (x is not None):
        ln.x = x
    if (y is not None):
        ln.y = y
    if (projection is not None):
        ln.projection = projection
    return ln


def getline(name='default', ltype=None, width=None, color=None,
            priority=None, viewport=None,
            worldcoordinate=None,
            x=None, y=None):
    """
    %s

    :param name: Name of created object
    :type name: str

    :param ltype: One of "dash", "dash-dot", "solid", "dot", or "long-dash".
    :type ltype: str

    :param width: Thickness of the line to be created
    :type width: int

    :param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
                  or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
    :type color: str or int

    :param priority: The layer on which the marker will be drawn.
    :type priority: int

    :param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
    :type viewport: list of floats

    :param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
    :type worldcoordinate: list of floats

    :param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
    :type x: list of floats

    :param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
    :type y: list of floats

    :returns: A VCS line object
    :rtype: vcs.line.Tl
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name, str):
        raise vcsError('The argument must be a string.')

    if name not in vcs.elements["line"]:
        raise ValueError("The line '%s' does not exist" % name)
    ln = vcs.elements["line"][name]
    if ltype is not None and ln.name != 'default':
        ln.type = ltype
    if width is not None and ln.name != 'default':
        ln.width = width
    if color is not None and ln.name != 'default':
        ln.color = color
    if priority is not None and ln.name != 'default':
        ln.priority = priority
    if viewport is not None and ln.name != 'default':
        ln.viewport = viewport
    if worldcoordinate is not None and ln.name != 'default':
        ln.worldcooridnate = worldcoordinate
    if viewport is not None and ln.name != 'default':
        ln.viewport = viewport
    if x is not None and ln.name != 'default':
        ln.x = x
    if y is not None and ln.name != 'default':
        ln.y = y
    return ln
getline.__doc__ = getline.__doc__ % xmldocs.get_docs['line']

def createmarker(name=None, source='default', mtype=None,
                 size=None, color=None, priority=None,
                 viewport=None, worldcoordinate=None,
                 x=None, y=None, projection=None):
    """
    Create a new marker secondary method given the the name and the existing
    marker secondary method to copy the attributes from. If no existing marker
    secondary method name is given, then the default marker secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    vcs.show('marker')
    mrk=vcs.createmarker('example1',)
    vcs.show('marker')
    mrk=vcs.createmarker('example2','black')
    vcs.show('boxfill')
    # Create instance of marker object 'red'
    mrk2=vcs.createmarker(name='new', name_src='red',mtype='dot', size=2,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )
    # Plot using specified marker object
    vcs.marker(mrk2)


:param name: Name of created object
:type name: str

:param source: A marker, or string name of a marker
:type source: str

:param mtype: Specifies the type of marker, i.e. "dot", "circle"
:type mtype: str

:param size:
:type size: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the marker will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:returns: A secondary marker method
:rtype: vcs.marker.Tm

    """

    name, source = check_name_source(name, source, 'marker')

    mrk = marker.Tm(name, source)
    if (mtype is not None):
        mrk.type = mtype
    if (size is not None):
        mrk.size = size
    if (color is not None):
        mrk.color = color
    if (priority is not None):
        mrk.priority = priority
    if (viewport is not None):
        mrk.viewport = viewport
    if (worldcoordinate is not None):
        mrk.worldcoordinate = worldcoordinate
    if (x is not None):
        mrk.x = x
    if (y is not None):
        mrk.y = y
    if (projection is not None):
        mrk.projection = projection
    return mrk


def getmarker(name='default', mtype=None, size=None, color=None,
              priority=None, viewport=None,
              worldcoordinate=None,
              x=None, y=None):
    """
    %s

    :param name: Name of created object
    :type name: str

    :param source: A marker, or string name of a marker
    :type source: str

    :param mtype: Specifies the type of marker, i.e. "dot", "circle"
    :type mtype: str

    :param size: Size of the marker
    :type size: int

    :param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
                  or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
    :type color: str or int

    :param priority: The layer on which the marker will be drawn.
    :type priority: int

    :param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
    :type viewport: list of floats

    :param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
    :type worldcoordinate: list of floats

    :param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
    :type x: list of floats

    :param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
    :type y: list of floats

    :returns: A marker graphics method object
    :rtype: vcs.marker.Tm
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name, str):
        raise vcsError('The argument must be a string.')

    if name not in vcs.elements["marker"]:
        raise ValueError("The marker object '%s' does not exists")
    mrk = vcs.elements["marker"][name]
    if (mtype is not None) and (mrk.name != "default"):
        mrk.type = mtype
    if (size is not None) and (mrk.name != "default"):
        mrk.size = size
    if (color is not None) and (mrk.name != "default"):
        mrk.color = color
    if (priority is not None) and (mrk.name != "default"):
        mrk.priority = priority
    if (viewport is not None) and (mrk.name != "default"):
        mrk.viewport = viewport
    if (worldcoordinate is not None) and (mrk.name != "default"):
        mrk.worldcoordinate = worldcoordinate
    if (x is not None) and (mrk.name != "default"):
        mrk.x = x
    if (y is not None) and (mrk.name != "default"):
        mrk.y = y
    return mrk
getmarker.__doc__ = getmarker.__doc__ % xmldocs.get_docs['marker']


def createfillarea(name=None, source='default', style=None,
                   index=None, color=None, priority=None,
                   viewport=None, worldcoordinate=None,
                   x=None, y=None):
    """
    Create a new fillarea secondary method given the the name and the existing
    fillarea secondary method to copy the attributes from. If no existing fillarea
    secondary method name is given, then the default fillarea secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    :Example:

        .. doctest:: manageElements_

    vcs.show('fillarea')
    fa=vcs.createfillarea('example1',)
    vcs.show('fillarea')
    fa=vcs.createfillarea('example2','black')
    vcs.show('fillarea')
    # Create instance of fill area object 'red'
    fa2=vcs.createmarker(name='new', name_src='red',style=1, index=1,
                  color=242, priority=1, viewport=[0, 2.0, 0, 2.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )
    # Plot using specified fill area object
    vcs.fillarea(fa2)

:param name: Name of created object
:type name: str

:param source: a fillarea, or string name of a fillarea
:type source: str

:param style: One of "hatch", "solid", or "pattern".
:type style: str

:param index: Specifies which `pattern <http://uvcdat.llnl.gov/gallery/fullsize/pattern_chart.png>`_ to fill with.
Accepts ints from 1-20.

:type index: int

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))

:type color: str or int

:param priority: The layer on which the fillarea will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:returns: A fillarea object
:rtype: vcs.fillarea.Tf
"""

    name, source = check_name_source(name, source, 'fillarea')

    fa = fillarea.Tf(name, source)
    if (style is not None):
        fa.style = style
    if (index is not None):
        fa.index = index
    if (color is not None):
        fa.color = color
    if (priority is not None):
        fa.priority = priority
    if (viewport is not None):
        fa.viewport = viewport
    if (worldcoordinate is not None):
        fa.worldcoordinate = worldcoordinate
    if (x is not None):
        fa.x = x
    if (y is not None):
        fa.y = y
    return fa


def getfillarea(name='default', style=None,
                index=None, color=None,
                priority=None, viewport=None,
                worldcoordinate=None,
                x=None, y=None):
    """
    %s

    :param name: String name of an existing fillarea VCS object
    :type name: str

    :param style: One of "hatch", "solid", or "pattern".
    :type style: str

    :param index: Specifies which `pattern <http://uvcdat.llnl.gov/gallery/fullsize/pattern_chart.png>`_ to fill with.
                  Accepts ints from 1-20.
    :type index: int

    :param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
                  or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
    :type color: str or int

    :param priority: The layer on which the texttable will be drawn.
    :type priority: int

    :param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
    :type viewport: list of floats

    :param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
    :type worldcoordinate: list of floats

    :param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
    :type x: list of floats

    :param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
    :type y: list of floats

    :returns: A fillarea secondary object
    :rtype: vcs.fillarea.Tf

    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name, str):
        raise vcsError('The argument must be a string.')
    if name not in vcs.elements["fillarea"].keys():
        raise vcsError("Fillarea '%s' does not exist" % (name))

    fa = vcs.elements["fillarea"][name]
    if (style is not None) and (fa.name != "default"):
        fa.style = style
    if (index is not None) and (fa.name != "default"):
        fa.index = index
    if (color is not None) and (fa.name != "default"):
        fa.color = color
    if (priority is not None) and (fa.name != "default"):
        fa.priority = priority
    if (viewport is not None) and (fa.name != "default"):
        fa.viewport = viewport
    if (worldcoordinate is not None) and (fa.name != "default"):
        fa.worldcoordinate = worldcoordinate
    if (x is not None) and (fa.name != "default"):
        fa.x = x
    if (y is not None) and (fa.name != "default"):
        fa.y = y
    return fa
getfillarea.__doc__ = getfillarea.__doc__ % xmldocs.get_docs['fillarea']

def createtexttable(name=None, source='default', font=None,
                    spacing=None, expansion=None, color=None, priority=None,
                    viewport=None, worldcoordinate=None,
                    x=None, y=None):
    """
    Create a new texttable secondary method given the the name and the existing
    texttable secondary method to copy the attributes from. If no existing texttable
    secondary method name is given, then the default texttable secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    .. deprecated:: 1.0
        expansion parameter is no longer used

    :Example:

        .. doctest:: manageElements_

    # Show names of all available texttable objects
    vcs.show('texttable')
    tt=vcs.createtexttable('example1',)
    vcs.show('texttable')
    tt=vcs.createtexttable('example2','black')
    vcs.show('texttable')
    # Show available fonts
    vcs.show('font')
    # Create instance of texttable object 'new'
    tt=vcs.createtexttable(name='new', source='red',font=1,spacing=1, font='default',
                  color=242, priority=1, viewport=[0, 1.0, 0, 1.0],
                  worldcoordinate=[0,100, 0,50]
                  x=[0,20,40,60,80,100],
                  y=[0,10,20,30,40,50] )
    # Plot using specified texttable object
    vcs.texttable(tt)


:param name: Name of created object
:type name: str

:param source: a texttable, or string name of a texttable
:type source: str

:param font: Which font to use (index or name).
:type font: int or string

:param expansion: DEPRECATED
:type expansion: DEPRECATED

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the texttable will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:returns: A texttable graphics method object
:rtype: vcs.texttable.Tt

"""

    name, source = check_name_source(name, source, 'texttable')

    tt = texttable.Tt(name, source)
    try:
        if (font is not None):
            tt.font = font
        if (spacing is not None):
            tt.spacing = spacing
        if (expansion is not None):
            tt.expansion = expansion
        if (color is not None):
            tt.color = color
        if (priority is not None):
            tt.priority = priority
        if (viewport is not None):
            tt.viewport = viewport
        if (worldcoordinate is not None):
            tt.worldcoordinate = worldcoordinate
        if (x is not None):
            tt.x = x
        if (y is not None):
            tt.y = y
        return tt
    except:
        pass


def gettexttable(name='default', font=None,
                 spacing=None, expansion=None, color=None,
                 priority=None, viewport=None,
                 worldcoordinate=None,
                 x=None, y=None):
    """
    %s

    :param name: String name of an existing VCS texttable object
    :type name: str

    :param font: ???
    :type font: ???

    :param expansion: ???
    :type expansion: ???

    :param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
                  or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
    :type color: str or int

    :param priority: The layer on which the texttable will be drawn.
    :type priority: int

    :param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
    :type viewport: list of floats

    :param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
    :type worldcoordinate: list of floats

    :param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
    :type x: list of floats

    :param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
    :type y: list of floats

    :returns: A texttable graphics method object
    :rtype: vcs.texttable.Tt
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(name, str):
        raise vcsError('The argument must be a string.')

    if name not in vcs.elements["texttable"]:
        raise ValueError("The texttable '%s' does not exists" % name)
    return vcs.elements["texttable"][name]
gettexttable.__doc__ = gettexttable.__doc__ % xmldocs.get_docs['texttable']


def createtextorientation(name=None, source='default'):
    """
    Create a new textorientation secondary method given the the name and
    the existing textorientation secondary method to copy the attributes
    from. If no existing textorientation secondary method name is given,
    then the default textorientation secondary method will be used as the
    secondary method to which the attributes will be copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    :Example:

        .. doctest:: manageElements_

    vcs.show('textorientation')
    to=vcs.createtextorientation('example1',)
    vcs.show('textorientation')
    to=vcs.createtextorientation('example2','black')
    vcs.show('textorientation')


:param name: The name of the created object
:type name: str

:param source: The object to inherit from
:type source: a textorientation or a string name of a textorientation

:returns: A textorientation secondary method
:rtype: vcs.textorientation.To
"""

    name, source = check_name_source(name, source, 'textorientation')

    return textorientation.To(name, source)


def gettextorientation(To_name_src='default'):
    """
    %s

    :param To_name_src: String name of an existing textorientation VCS object
    :type To_name_src: str

    :returns: A textorientation VCS object
    :rtype: vcs.textorientation.To
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(To_name_src, str):
        raise vcsError('The argument must be a string.')

    if To_name_src not in vcs.elements["textorientation"]:
        raise ValueError(
            "The textorientation '%s' does not exists" %
            To_name_src)
    return vcs.elements["textorientation"][To_name_src]
gettextorientation.__doc__ = gettextorientation.__doc__ % xmldocs.get_docs['textorientation']

def createtextcombined(Tt_name=None, Tt_source='default', To_name=None, To_source='default',
                       font=None, spacing=None, expansion=None, color=None,
                       priority=None, viewport=None, worldcoordinate=None, x=None, y=None,
                       height=None, angle=None, path=None, halign=None, valign=None, projection=None):
    """
    Create a new textcombined secondary method given the the names and
    the existing texttable and textorientation secondary methods to copy
    the attributes from. If no existing texttable and textorientation
    secondary method names are given, then the default texttable and
    textorientation secondary methods will be used as the secondary method
    to which the attributes will be copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    :Example:

::
    # Show available texttable object
    vcs.show('texttable')
    # Show available textorientation object
    vcs.show('textorientation')
    # Show font options
    vcs.show('font')
    tc=vcs.createtextcombined('example1','std','example1','7left')
    vcs.show('texttable')
    vcs.show('textorientation')

:param Tt_name: Name of created object
:type Tt_name: str

:param Tt_source: Texttable object to inherit from. Can be a texttable, or a string name of a texttable.
:type Tt_source: str or vcs.texttable.Tt

:param To_name: Name of the textcombined's text orientation  (to be created)
:type To_name: str

:param To_source: Name of the textorientation to inherit. Can be a textorientation, or a string name of a textorientation.
:type To_source: str or vcs.textorientation.To

:param font: Which font to use (index or name).
:type font: int or str

:param spacing: DEPRECATED
:type spacing: DEPRECATED

:param expansion: DEPRECATED
:type expansion: DEPRECATED

:param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
              or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
:type color: str or int

:param priority: The layer on which the object will be drawn.
:type priority: int

:param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
:type viewport: list of floats

:param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
:type worldcoordinate: list of floats

:param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
:type x: list of floats

:param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
:type y: list of floats

:param height: Size of the font
:type height: int

:param angle: Angle of the text, in degrees
:type angle: int

:param path: DEPRECATED
:type path: DEPRECATED

:param halign: Horizontal alignment of the text. One of ["left", "center", "right"].
:type halign: str

:param valign: Vertical alignment of the text. One of ["top", "center", "botom"].
:type valign: str

:param projection: Specify a geographic projection used to convert x/y from spherical coordinates into 2D coordinates.
:type projection: str or projection object

:returns: A VCS text object
:rtype: vcs.textcombined.Tc

"""
    # Check if to is defined
    if To_name is None:
        To_name = Tt_name
    Tt_name, Tt_source = check_name_source(Tt_name, Tt_source, 'texttable')
    To_name, To_source = check_name_source(
        To_name, To_source, 'textorientation')

    tc = textcombined.Tc(Tt_name, Tt_source, To_name, To_source)
    if (font is not None):
        tc.font = font
    if (spacing is not None):
        tc.spacing = spacing
    if (expansion is not None):
        tc.expansion = expansion
    if (color is not None):
        tc.color = color
    if (priority is not None):
        tc.priority = priority
    if (viewport is not None):
        tc.viewport = viewport
    if (worldcoordinate is not None):
        tc.worldcoordinate = worldcoordinate
    if (x is not None):
        tc.x = x
    if (y is not None):
        tc.y = y
    if (height is not None):
        tc.height = height
    if (angle is not None):
        tc.angle = angle
    if (path is not None):
        tc.path = path
    if (halign is not None):
        tc.halign = halign
    if (valign is not None):
        tc.valign = valign
    if (projection is not None):
        tc.projection = projection
    return tc
#
# Set alias for the secondary createtextcombined.
createtext = createtextcombined


def gettextcombined(Tt_name_src='default', To_name_src=None, string=None, font=None, spacing=None,
                    expansion=None, color=None,
                    priority=None, viewport=None, worldcoordinate=None, x=None, y=None,
                    height=None, angle=None, path=None, halign=None, valign=None):
    """
    %s

    :param Tt_name_src: Name of created object
    :type Tt_name_src: str

    :param To_name_src: Name of parent textorientation object
    :type To_name_src: str

    :param string: Text to render
    :param string: list of str

    :param font: Which font to use (index or name)
    :type font: int or str

    :param spacing: DEPRECATED
    :type spacing: DEPRECATED

    :param expansion: DEPRECATED
    :type expansion: DEPRECATED

    :param color: A color name from the `X11 Color Names list <https://en.wikipedia.org/wiki/X11_color_names>`_,
                  or an integer value from 0-255, or an RGB/RGBA tuple/list (e.g. (0,100,0), (100,100,0,50))
    :type color: str or int

    :param priority: The layer on which the object will be drawn.
    :type priority: int

    :param viewport: 4 floats between 0 and 1. These specify the area that the X/Y values are mapped to inside of the canvas
    :type viewport: list of floats

    :param worldcoordinate: List of 4 floats (xmin, xmax, ymin, ymax)
    :type worldcoordinate: list of floats

    :param x: List of lists of x coordinates. Values must be between worldcoordinate[0] and worldcoordinate[1].
    :type x: list of floats

    :param y: List of lists of y coordinates. Values must be between worldcoordinate[2] and worldcoordinate[3].
    :type y: list of floats

    :param height: Size of the font
    :type height: int

    :param angle: Angle of the rendered text, in degrees
    :type angle: list of int

    :param path: DEPRECATED
    :type path: DEPRECATED

    :param halign: Horizontal alignment of the text. One of ["left", "center", "right"]
    :type halign: str

    :param valign: Vertical alignment of the text. One of ["top", "center", "bottom"]
    :type valign: str

    :returns: A textcombined object
    :rtype: vcs.textcombined.Tc
    """

    # Check to make sure the arguments passed in are a STRINGS
    if not isinstance(Tt_name_src, str):
        raise vcsError('The first argument must be a string.')
    if To_name_src is None:
        sp = Tt_name_src.split(":::")
        if len(sp) == 2:
            Tt_name_src = sp[0]
            To_name_src = sp[1]
    if not isinstance(To_name_src, str):
        raise vcsError('The second argument must be a string.')

    tc = vcs.elements["textcombined"].get(
        "%s:::%s" %
        (Tt_name_src, To_name_src), None)
    if tc is None:
        raise Exception(
            "No such text combined: %s:::%s" %
            (Tt_name_src, To_name_src))

    if (string is not None) and (tc.Tt_name != "default"):
        tc.string = string
    if (font is not None) and (tc.Tt_name != "default"):
        tc.font = font
    if (spacing is not None) and (tc.Tt_name != "default"):
        tc.spacing = spacing
    if (expansion is not None) and (tc.Tt_name != "default"):
        tc.expansion = expansion
    if (color is not None) and (tc.Tt_name != "default"):
        tc.color = color
    if (priority is not None) and (tc.Tt_name != "default"):
        tc.priority = priority
    if (viewport is not None) and (tc.Tt_name != "default"):
        tc.viewport = viewport
    if (worldcoordinate is not None) and (tc.Tt_name != "default"):
        tc.worldcoordinate = worldcoordinate
    if (x is not None) and (tc.To_name != "default"):
        tc.x = x
    if (y is not None) and (tc.To_name != "default"):
        tc.y = y
    if (height is not None) and (tc.To_name != "default"):
        tc.height = height
    if (angle is not None) and (tc.To_name != "default"):
        tc.angle = angle
    if (path is not None) and (tc.To_name != "default"):
        tc.path = path
    if (halign is not None) and (tc.To_name != "default"):
        tc.halign = halign
    if (valign is not None) and (tc.To_name != "default"):
        tc.valign = valign
    return tc
gettextcombined.__doc__ = gettextcombined.__doc__ % xmldocs.get_docs['textcombined']
#
# Set alias for the secondary gettextcombined.
gettext = gettextcombined


def get3d_scalar(Gfdv3d_name_src='default'):
    """
    %s

    :param Gfdv3d_name_src: String name of an existing 3d_scalar VCS object.
    :type Gfdv3d_name_src: str

    :returns: A pre-existing 3d_scalar VCS object
    :rtype: vcs.dv3d.Gf3Dscalar
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfdv3d_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gfdv3d_name_src not in vcs.elements["3d_scalar"]:
        raise ValueError("dv3d '%s' does not exists" % Gfdv3d_name_src)

    return vcs.elements["3d_scalar"][Gfdv3d_name_src]
get3d_scalar.__doc__ = get3d_scalar.__doc__ % xmldocs.get_docs['3d_scalar']

def create3d_scalar(name=None, source='default'):
    """
    Create a new dv3d graphics method given the the name and the existing
    dv3d graphics method to copy the attributes from. If no existing
    dv3d graphics method name is given, then the default dv3d graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::
        If the name provided already exists, then an error will be returned. Graphics method names must be unique.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    a.show('3d_scalar')
    plot=a.create3d_scalar()

:param name: The name of the created object
:type name: str

:param source: The object to inherit from
:type source: a 3d_scalar or a string name of a 3d_scalar

:returns: A 3d_scalar graphics method object
:rtype: vcs.dv3d.Gf3Dscalar
"""
    name, source = check_name_source(name, source, '3d_scalar')
    return dv3d.Gf3Dscalar(name, source)

def get3d_dual_scalar(Gfdv3d_name_src='default'):
    """
    %s

    :param Gfdv3d_name_src: String name of an existing 3d_dual_scalar VCS object
    :type Gfdv3d_name_src: str

    :returns: A pre-existing 3d_dual_scalar VCS object
    :rtype: vcs.dv3d.Gf3DDualScalar
    """
    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfdv3d_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gfdv3d_name_src not in vcs.elements["3d_dual_scalar"]:
        raise ValueError("dv3d '%s' does not exists" % Gfdv3d_name_src)

    return vcs.elements["3d_dual_scalar"][Gfdv3d_name_src]
get3d_dual_scalar.__doc__ = get3d_dual_scalar.__doc__ % xmldocs.get_docs['3d_dual_scalar']

def create3d_dual_scalar(name=None, source='default'):
    """
    Create a new dv3d graphics method given the the name and the existing
    dv3d graphics method to copy the attributes from. If no existing
    dv3d graphics method name is given, then the default dv3d graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::
        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

        a=vcs.init()
        a.show('3d_dual_scalar')
        plot=a.create3d_dual_scalar()

    :param name: The name of the created object
    :type name: str

    :param source: The object to inherit from
    :type source: a 3d_dual_scalar or a string name of a 3d_dual_scalar

    :returns: A 3d_dual_scalar graphics method object
    :rtype: vcs.dv3d.Gf3DDualScalar
    """

    name, source = check_name_source(name, source, '3d_dual_scalar')
    return dv3d.Gf3DDualScalar(name, source)


def get3d_vector(Gfdv3d_name_src='default'):
    """
    %s

    :param Gfdv3d_name_src: String name of an existing 3d_vector VCS object
    :type Gfdv3d_name_src: str

    :returns: A pre-existing 3d_vector VCS object
    :rtype: vcs.dv3d.Gf3Dvector
    """

    # Check to make sure the argument passed in is a STRING
    if not isinstance(Gfdv3d_name_src, str):
        raise vcsError('The argument must be a string.')

    if Gfdv3d_name_src not in vcs.elements["3d_vector"]:
        raise ValueError("dv3d '%s' does not exists" % Gfdv3d_name_src)

    return vcs.elements["3d_vector"][Gfdv3d_name_src]
get3d_vector.__doc__ = get3d_vector.__doc__ % xmldocs.get_docs['3d_vector']

def create3d_vector(name=None, source='default'):
    """
    Create a new dv3d graphics method given the the name and the existing
    dv3d graphics method to copy the attributes from. If no existing
    dv3d graphics method name is given, then the default dv3d graphics
    method will be used as the graphics method to which the attributes will
    be copied from.

    .. note::
        If the name provided already exists, then an error will be returned. Graphics
        method names must be unique.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    a.show('3d_vector')
    plot=a.create3d_vector()

:param name: The name of the created object
:type name: str

:param source: The object to inherit from
:type source: a 3d_vector or a string name of a 3d_vector

:returns: A 3d_vector graphics method object
:rtype: vcs.dv3d.Gf3Dvector

"""

    name, source = check_name_source(name, source, '3d_vector')
    return dv3d.Gf3Dvector(name, source)

#############################################################################
#                                                                           #
# Colormap functions for VCS.                                               #
#                                                                           #
#############################################################################


def createcolormap(Cp_name=None, Cp_name_src='default'):
    """
    Create a new colormap secondary method given the the name and the existing
    colormap secondary method to copy the attributes from. If no existing colormap
    secondary method name is given, then the default colormap secondary method
    will be used as the secondary method to which the attributes will be
    copied from.

    .. note::
        If the name provided already exists, then an error will be returned.
        Secondary method names must be unique.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    cp=a.createcolormap('example1',)
    a.show('colormap')
    cp=a.createcolormap('example2','AMIP')
    a.show('colormap')

:param Cp_name: The name of the created object
:type Cp_name: str

:param Cp_name_src: The object to inherit
:type Cp_name_src: a colormap or a string name of a colormap

:returns: A VCS colormap object
:rtype: vcs.colormap.Cp
"""

    Cp_name, Cp_name_src = check_name_source(Cp_name, Cp_name_src, 'colormap')
    return colormap.Cp(Cp_name, Cp_name_src)


def getcolormap(Cp_name_src='default'):
    """
    %s

    :param Cp_name_src: String name of an existing colormap VCS object
    :type Cp_name_src: str

    :returns: A pre-existing VCS colormap object
    :rtype: vcs.colormap.Cp
    """
    # Check to make sure the argument passed in is a STRING
    if not isinstance(Cp_name_src, str):
        raise ValueError('Error -  The argument must be a string.')

    return vcs.elements["colormap"][Cp_name_src]
getcolormap.__doc__ = getcolormap.__doc__ % xmldocs.get_docs['colormap']

# Function that deal with removing existing vcs elements


def removeG(obj, gtype="boxfill"):
    exec("res = vcs.is%s(obj)" % gtype)
    if isinstance(obj, str):
        name = obj
        if obj not in vcs.elements[gtype].keys():
            raise RuntimeError("Cannot remove inexisting %s %s" % (gtype, obj))
    else:
        name = obj.name
        if not res:  # noqa
            raise RuntimeError("You are trying to remove a VCS %s but %s is not one" % (gtype, repr(obj)))
    msg = "Removed %s object %s" % (gtype, name)
    del(vcs.elements[gtype][name])
    return msg


def removeGfb(obj):
    return removeG(obj, "boxfill")


def removeGfi(obj):
    return removeG(obj, "isofill")


def removeGi(obj):
    return removeG(obj, "isoline")


def removeGXy(obj):
    return removeG(obj, "xyvsx")


def removeGYx(obj):
    return removeG(obj, "yxvsx")


def removeGXY(obj):
    return removeG(obj, "xvsy")


def removeG1d(obj):
    return removeG(obj, "1d")


def removeGv(obj):
    return removeG(obj, "vector")


def removeGSp(obj):
    return removeG(obj, "scatter")


def removeGfm(obj):
    return removeG(obj, "meshfill")


def removeGtd(obj):
    return removeG(obj, "taylordiagram")


def removeTl(obj):
    return removeG(obj, "line")


def removeTm(obj):
    return removeG(obj, "marker")


def removeTf(obj):
    return removeG(obj, "fillarea")


def removeTt(obj):
    return removeG(obj, "texttable")


def removeTo(obj):
    return removeG(obj, "textorientation")


def removeTc(obj):
    if isinstance(obj, str):
        Tt, To = obj.split(":::")
    else:
        To = obj.To_name
        Tt = obj.Tt_name
    msg = removeTt(Tt)
    msg += removeTo(To)
    removeG(obj, "textcombined")
    return msg


def removeProj(obj):
    return removeG(obj, "projection")


def removeCp(obj):
    return removeG(obj, "colormap")


def removeP(obj):
    return removeG(obj, "template")


def removeobject(obj):
    """
    The user has the ability to create primary and secondary class
    objects. The function allows the user to remove these objects
    from the appropriate class list.

    Note, To remove the object completely from Python, remember to
    use the "del" function.

    Also note, The user is not allowed to remove a "default" class
    object.

    :Example:

        .. doctest:: manageElements_

    a=vcs.init()
    # To Modify an existing line object
    line=a.getline('red')
    # Create an instance of an isoline object
    iso=a.createisoline('dean')
    #...
    # Removes line object from VCS list
    a.remove(line)
    # Remove isoline object from VCS list
    a.remove(iso)

:param obj: Any VCS primary or secondary object
:type obj: VCS object

:returns: String indicating the specified object was removed
:rtype: str
    """

    if vcs.istemplate(obj):
        msg = vcs.removeP(obj.name)
    elif vcs.isgraphicsmethod(obj):
        if (obj.g_name == 'Gfb'):
            msg = vcs.removeGfb(obj.name)
        elif (obj.g_name == 'Gfi'):
            msg = vcs.removeGfi(obj.name)
        elif (obj.g_name == 'Gi'):
            msg = vcs.removeGi(obj.name)
        elif (obj.g_name == 'GXy'):
            msg = vcs.removeGXy(obj.name)
        elif (obj.g_name == 'GYx'):
            msg = vcs.removeGYx(obj.name)
        elif (obj.g_name == 'GXY'):
            msg = vcs.removeGXY(obj.name)
        elif (obj.g_name == 'Gv'):
            msg = vcs.removeGv(obj.name)
        elif (obj.g_name == 'GSp'):
            msg = vcs.removeGSp(obj.name)
        elif (obj.g_name == 'Gfm'):
            msg = vcs.removeGfm(obj.name)
        elif (obj.g_name == 'G1d'):
            msg = vcs.removeG1d(obj.name)
        elif (obj.g_name == 'Gtd'):
            msg = vcs.removeGtd(obj.name)
        else:
            msg = 'Could not find the correct graphics class object.'
    elif vcs.issecondaryobject(obj):
        if (obj.s_name == 'Tl'):
            msg = vcs.removeTl(obj.name)
        elif (obj.s_name == 'Tm'):
            msg = vcs.removeTm(obj.name)
        elif (obj.s_name == 'Tf'):
            msg = vcs.removeTf(obj.name)
        elif (obj.s_name == 'Tt'):
            msg = vcs.removeTt(obj.name)
        elif (obj.s_name == 'To'):
            msg = vcs.removeTo(obj.name)
        elif (obj.s_name == 'Tc'):
            msg = vcs.removeTc(obj.name)
        elif (obj.s_name == 'Proj'):
            msg = vcs.removeProj(obj.name)
        elif (obj.s_name == 'Cp'):
            msg = vcs.removeCp(obj.name)
        else:
            msg = 'Could not find the correct secondary class object.'
    else:
        msg = 'This is not a template, graphics method, or secondary method object.'
    return msg
