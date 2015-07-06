def checkString(self, name, value):
    if not isinstance(value, str):
        raise ValueError(name + ' must be a string')
    return value


def checkInStringsList(self, name, value, values):
    """ check if value is in values"""
    if not isinstance(value, str):
        raise ValueError(name + 'must be a string')
    elif not value.lower() in values:
        err = name + " must be in ('" + values[0]
        for v in values[1:-1]:
            err = err + ", '" + v + "'"
        err = err + " or '" + values[-1] + "')"
        raise ValueError(err)
    return value.lower()


def checkInStringsListInt(self, name, value, values):
    """ checks the line type"""
    val = []
    str1 = name + ' can either be ('
    str2 = ' or ('
    i = 0
    for v in values:
        if not v == '':  # skips the invalid/non-contiguous values
            str2 = str2 + str(i) + ', '
            if isinstance(v, list) or isinstance(v, tuple):
                str1 = str1 + "'" + v[0] + "', "
                for v2 in v:
                    val.append(v2)
            else:
                val.append(v)
                str1 = str1 + "'" + v + "', "
            i = i + 1
    err = str1[:-2] + ')' + str2[:-2] + ')'
    if isinstance(value, str):
        value = value.lower()
        if value not in val:
            raise ValueError(err)
        i = 0
        for v in values:
            if isinstance(v, list) or isinstance(v, tuple):
                if value in v:
                    return i
            elif value == v:
                return i
            i = i + 1
    elif isinstance(value, int) or \
            (isinstance(value, float) and int(value) == value):
        if value not in range(len(values)):
            raise ValueError(err)
        else:
            return int(value)
    else:
        raise ValueError(err)


def isNumber(value):
    if isinstance(value, int) or isinstance(
            value, long) or isinstance(value, float):
        return 1
    return 0


def checkPositiveInt(self, name, value):
    if not isNumber(value):
        raise ValueError(name + ' must be an integer')
    elif (not (isinstance(value, int) or isinstance(value, long))
            and (not int(value) == value)):
        raise ValueError(name + ' must be an integer')
    elif value < 0:
        raise ValueError(name + ' must be positve')
    return value


def checkPositiveNumber(self, name, value):
    if not (isinstance(value, int) or isinstance(
            value, long) or isinstance(value, float)):
        raise ValueError(name + ' must be an integer or a float')
    elif value < 0:
        raise ValueError(name + ' must be positve')
    return value


def checkNumber(self, name, value):
    if not isNumber(value):
        raise ValueError(name + ' must be an integer or a float')
    return value


def isListorTuple(value):
    if isinstance(value, list) or isinstance(value, tuple):
        return 1
    return 0


def checkListorTuple(self, value, name):
    if not isListorTuple(value):
        raise ValueError(name + ' must be a list or a tuple')
    return value


def checkRGB(self, name, value):
    value = checkListorTuple(self, value, name)
    if len(value) != 3:
        raise ValueError(name + ' must have exactly 3 arguments')
    else:
        for i in range(3):
            if not isinstance(value[i], int):
                if int(value[i]) == value[i]:
                    value[i] = int(value[i])
                else:
                    raise ValueError(name + ' values must be integer')
            if value[i] < 0 or value[i] > 255:
                raise ValueError(name + ' values must be between 0 and 255')
    return value


def checkOnOff(self, name, value):
    return checkInStringsList(self, name, value, ['on', 'off'])


def checkLinestyle(self, name, value):
    """set the line style """
    return checkInStringsListInt(self, name, value, [
        "none",
        "solid",
        "dot",
        "dash",
        "long-dash",
        ["dot-dash", "dash-dot", "dashdot", "dotdash"],
        ["dot-long-dash", "long-dash-dot"],
        "dot-dot-dash",
        "dot-dash-dash",
        "dot-long-dash"])


def checkFont(self, name, value):
    """set the font """
    return checkInStringsListInt(self, name, value, [
        ["times", "times-roman"],
        "times-italic",
        "times-bold",
        "times-bolditalic",
        "helvetica",
        "helvetica-oblique",
        "helvetica-bold",
        "helvetica-boldoblique",
        "courier",
        "courier-oblique",
        "courier-bold",
        "courier-boldoblique",
        "symbol",
        "zapfdingbats"])


def checkSide(self, name, value):
    """ check the side """
    return checkInStringsList(
        self, name, value, ['normal', 'both', 'opposite'])


def checkLoc(self, name, value):
    """ check the loc (auto) or a location """
    if not (
        (isinstance(value, str) and value.lower() == 'auto')
        or
        isListorTuple(value)
    ):
        raise ValueError(name + 'must be a "auto" or a tuple/list')
    elif isListorTuple(value):
        if len(value) != 2:
            raise ValueError(name + 'tuple length must be 2 x/y coordinate')
        else:
            for v in value:
                if not isNumber(v):
                    raise ValueError(name + ' must contain numbers only')
    if isinstance(value, str):
        return value.lower()
    else:
        return value


def checkLayout(self, name, value):
    """ check the layout para/perp """
    if not isinstance(value, str):
        raise ValueError(name + 'must be a string')
    elif not value.lower()[:4] in ['para', 'perp']:
        raise ValueError(name + 'must be either "para" or "perp"')
    return value.lower()[:4]


def checkTickType(self, name, value):
    """ check the type of special ticks """
    if not isinstance(value, str):
        raise ValueError(name + 'must be a string')
    elif not value.lower() in ['none', 'both', 'ticks']:
        raise ValueError(name + ' must be either "none", "both" or "ticks"')
    return value.lower()


def checkTicksType(self, name, value):
    """ check the type of special ticks """
    if not isinstance(value, str):
        raise ValueError(name + 'must be a string')
    elif not value.lower() in ['auto', 'spec', 'zmean']:
        raise ValueError(name + ' must be either "auto", "spec" or "zmean"')
    if value.lower() == 'zmean':
        self.spec.values = {-1.: '90S', -0.866: '60S', -0.5: '30S', 0.: 'Eq',
                            0.5: '30N', 0.866: '60N', 1.: '90N'}
    return value.lower()


def checkTickLocation(self, name, value):
    """check that the locations are ok"""
    if not (isinstance(value, list) or isinstance(
            value, tuple) or isinstance(value, dict)):
        raise ValueError(name + ' must be a list/tuple or dictionary')
    elif isListorTuple(value):
        val = value
    else:
        val = value.keys()
    # Now checks that the location are actually numbers
    for v in val:
        if not isNumber(v):
            raise ValueError(name + ' locations position must be numbers !')
    # Now we're ok let's set the attributes
    if isListorTuple(value):
        self._loc = value
        return
    else:
        loc = sorted(value.keys())
        val = []
        for l in loc:
            k = value[l]
            if not (isNumber(k) or isinstance(k, str)):
                raise ValueError(name + ' values must be string or number')
            else:
                val.append(str(k))
        self._loc = loc
        self._values = val


def checkTickValues(self, name, value):
    """check that the locations are ok"""
    if not (isinstance(value, list) or isinstance(
            value, tuple) or isinstance(value, dict)):
        raise ValueError(name + ' must be a list/tuple or dictionary')
    elif isListorTuple(value):
        val = []
    else:
        val = value.keys()
    # Now checks that the location are actually numbers
    for v in val:
        if not isNumber(v):
            raise ValueError(name + ' locations position must be numbers !')
    # Now we're ok let's set the attributes
    if isinstance(value, dict):
        self.loc = val
        vals = []
        for v in value.keys():
            vals.append(value[v])
        value = vals
    val = []
    for k in value:
        if not (isNumber(k) or isinstance(k, str)):
            raise ValueError(name + ' values must be string or number')
        else:
            val.append(str(k))
    self._values = val
    return


def checkFormat(self, name, value):
    """checks for the format"""
    return checkInStringsList(self, name, value, [
        'general',
        'decimal', 'exponential', 'power', 'scientific', 'engineering',
        'ddmmyy', 'mmddyy', 'yymmdd', 'mmyy', 'mmdd',
        'monthday', 'daymonth', 'months', 'monthsy', 'monthl',
        'dayofweekl', 'dayofweeks', 'dayofyear',
        'hms', 'mmddhms', 'mmddyyhms', 'yymmddhms',
        'degreeslon', 'degreesmmlon', 'degreesmmsslon', 'mmsslon',
        'degreeslat', 'degreesmmlat', 'degreesmmsslat', 'mmsslat', ])


def checkAuto(self, name, value):
    """ check for 'auto' or a value """
    if not (
        (isinstance(value, str) and value.lower() == 'auto')
        or
        isNumber(value)
    ):
        raise ValueError(name + 'must be a "auto" or a number')
    if isinstance(value, str):
        return 'auto'
    else:
        return value


def checkTrueFalse(self, name, value):
    """ check for 'true' or 'false' """
    return checkInStringsList(self, name, value, ['true', 'false'])


def checkInOut(self, name, value):
    """ check in or out """
    return checkInStringsList(self, name, value, ['in', 'out', 'both'])


def checkScale(self, name, value):
    """ check the scale """
    return checkInStringsList(
        self, name, value, ['normal', 'logarithmic', 'reciprocal', 'logit'])


def checkPattern(self, name, value):
    """ check the pattern """
    if not isinstance(value, int):
        raise ValueError(name + 'must be an integer')
    elif not 0 <= value < 32:
        raise ValueError(name + 'must be an integer between 0 and 31')
    return value


def checkLoctype(self, name, value):
    """ check the location type (view/world)"""
    return checkInStringsList(self, name, value, ['view', 'world', 'viewport'])


def checkFrameType(self, name, value):
    """set the frame style """
    return checkInStringsListInt(self, name, value, [
        'closed',
        ['half', 'halfopen', 'half open', 'half-open'],
        ['breaktop', 'break-top', 'break top'],
        ['breakbottom', 'break-bottom', 'break bottom'],
        ['breakleft', 'break-left', 'break left'],
        ['breakright', 'break-right', 'break right']])


def checkGraphType(self, name, value):
    """ check the graph type"""
    return checkInStringsList(
        self, name, value, ['xy', 'chart', 'polar', 'smith', 'fixed', 'pie'])


def checkList2(self, name, value):
    """ check for a list of 2 number"""
    if not isListorTuple(value):
        raise ValueError(name + ' must be a list/tuple')
    if not len(value) == 2:
        raise ValueError(name + ' list/tuple must be of length 2')
    for v in value:
        if not isNumber(v):
            raise ValueError(name + ' list/tuple elements must be numbers')
    return value


def checkList4(self, name, value):
    """ check for a list of 4 number"""
    if not isListorTuple(value):
        raise ValueError(name + ' must be a list/tuple')
    if not len(value) == 4:
        raise ValueError(name + ' list/tuple must be of length 4')
    for v in value:
        if not isNumber(v):
            raise ValueError(name + ' list/tuple elements must be numbers')
    return value


def checkPercent(self, name, value):
    """ check to see if a number is between 0 and 1"""
    if not isNumber(value):
        raise ValueError(name + ' must be number')
    if not 0. <= value <= 1.:
        raise ValueError(name + ' must be between 0 and 1')
    return value


def checkSymbol(self, name, value):
    """ check the symbol"""
    return checkInStringsListInt(self, name, value, [
        'none',
        'circle',
        'square',
        'diamond',
        ['triangle up', 'triangleup', 'triangle-up', 'triangle_up'],
        ['triangle left', 'triangleleft', 'triangle-left', 'triangle_left'],
        ['triangle down', 'triangledown', 'triangle-down', 'triangle_down'],
        ['triangle right', 'triangleright',
         'triangle-right', 'triangle_right'],
        ['plus', '+'],
        'x',
        ['star', '*'],
        ['char', 'character']])


def checkChar(self, name, value):
    """ checks the character for symbol"""
    if isinstance(value, str):
        if len(value.strip()) != 1:
            raise ValueError(name + " must be a single character")
        return ord(value.strip())
    elif isinstance(value, int):
        if value not in range(256):
            raise ValueError(name + ' not in range(256)')
        return value
    else:
        raise ValueError(
            name + ' must be a single character or an integer less than 256')


def checkLineType(self, name, value):
    """ checks the line type"""
    return checkInStringsListInt(self, name, value, [
        'none',
        'straight',
        'left',
        'right',
        ['segments', 'seg'],
        ['3seg', '3segments']])


def checkBaseLineType(self, name, value):
    """ checks the baseline type"""
    return checkInStringsListInt(self, name, value, [
        'zero',
        ['setmin', 'set min'],
        ['setmax', 'set max'],
        ['graphmin', 'graph min'],
        ['graphmax', 'graph max'],
        ['setaverage', 'set average',
         'setavg', 'setaverage', 'set-average', 'set-avg']])


def checkFillType(self, name, value):
    """ checks the fill type"""
    return checkInStringsListInt(self, name, value, [
        'none',
        ['polygon', 'as polygon', 'as-polygone',
            'aspoly', 'poly', 'aspolygon', 'as-poly'],
        ['baseline', 'base', 'tobase', 'to baseline',
         'tobaseline', 'to-base', 'to-baseline']])


def checkRule(self, name, value):
    """ checks the rule"""
    return checkInStringsListInt(
        self, name, value, ['winding', ['even-odd', 'evenodd']])


def checkOffset(self, name, value):
    """ check the offset """
    if not isListorTuple(value):
        raise ValueError(name + ' must be a list/tuple')
    if not len(value) == 2:
        raise ValueError(name + ' must be of length 2')
    self.xoffset = value[0]
    self.yoffset = value[1]
    return


def checkXY(self, name, value):
    """ check xy """
    if not isListorTuple(value):
        raise ValueError(name + ' must be a list/tuple')
    if not len(value) == 2:
        raise ValueError(name + ' must be of length 2')
    self.xoffset = value[0]
    self.yoffset = value[1]
    return


def checkStringXY(self, name, value):
    """ check xy """
    if not isListorTuple(value):
        raise ValueError(name + ' must be a list/tuple')
    if not len(value) == 2:
        raise ValueError(name + ' must be of length 2')
    self.x = value[0]
    self.y = value[1]
    return


def getXY(self, name):
    """ get the offset """
    return [self.x, self.y]


def getStringXY(self, name):
    """ get the offset """
    return [self.x, self.y]


def checkAngle(self, name, value):
    """ checks angles in degrees """
    if not isNumber(value):
        raise ValuerError(name + ' must be a number between 0. and 360.')
    if not 0 <= value <= 360.:
        raise ValuerError(name + ' must be a number between 0. and 360.')
    return value


def checkDSetType(self, name, value):
    """ checks the dataset type"""
    return checkInStringsList(self, name, value, [
        'xy', 'xydx', 'xydy', 'xydxdx', 'xydydy', 'xydxdy',
        'xydxdxdydy', 'bar', 'bardy', 'bardydy',
        'xyhilo', 'xyz', 'xyr', 'xysize', 'xycolor',
        'xycolpat', 'xyvmap', 'xyboxplot'])


def checkJustification(self, name, value):
    """ checks the justification"""
    if isNumber(value):
        if value == 3 or value == 7 or value == 11:
            raise ValueError(name + ' wrong value: ' + str(value))
        else:
            return value
    elif isinstance(value, str):
        if value == '':
            raise ValueError(name + ' cannot be an empty string')
# else:
    tmp = checkInStringsListInt(self, name, value, [
        ['left-justify', 'lj', 'left', 'leftjustify', 'left justify'],
        ['right-justify', 'rj', 'right', 'rightjustify', 'right justify'],
        ['center-justify', 'cj', 'center', 'centerjustify', 'center justify'],
        '',
        ['left-bottom', 'lb', 'left bottom', 'leftbottom'],
        ['right-bottom', 'rb', 'right bottom', 'rightbottom'],
        ['center-bottom', 'cb', 'center bottom', 'centerbottom'],
        '',
        ['left-top', 'lt', 'left top', 'lefttop'],
        ['right-top', 'rt', 'right top', 'righttop'],
        ['center-top', 'ct', 'center top', 'centertop'],
        '',
        ['left-half', 'lh', 'left half', 'lefthalf'],
        ['right-half', 'rh', 'right half', 'righthalf'],
        ['center-half', 'ch', 'center half', 'centerhalf']
    ])
    return tmp


def checkOnOffInt(self, name, value):
    """ checks for on/off and set to 1/0"""
    return checkInStringsListInt(self, name, value, ['off', 'on'])


def checkArrowType(self, name, value):
    """ checks for arrow type"""
    return checkInStringsListInt(self, name, value, [
        'line',
        ['filled', 'fill'],
        'opaque'
    ])


def checkArrowPosition(self, name, value):
    """ checks for arrow type"""
    return checkInStringsListInt(self, name, value, [
        'line',
        ['filled', 'fill'],
        'opaque'
    ])


def checkAvalueType(self, name, value):
    """ checks for arrow type"""
    return checkInStringsListInt(self, name, value, [
        'none',
        'x',
        'y',
        ['xy', 'x,y', 'x, y'],
        ['string', 's', 'str'],
        'z'
    ])


def getX1X2Y1Y2(self, name):
    """ returns x1 x2 y1 y2"""
    return [self.x1, self.x2, self.y1, self.y2]


def checkX1X2Y1Y2(self, name, value):
    """ returns x1 x2 y1 y2"""
    if not isListorTuple(value):
        raise ValueError(name +
                         ' must be a list or a tuple:' +
                         '[x1value,x2value,y1value,y2value]')
    elif len(value) != 4:
        raise ValueError(
            name +
            ' must be of length 4: [x1value,x2value,y1value,y2value]')
    else:
        for v in value:
            if not isNumber(v):
                raise ValueError(
                    name +
                    ' elements must be numbers: ' +
                    '[x1value,x2value,y1value,y2value]')
    self.x1 = value[0]
    self.x2 = value[1]
    self.y1 = value[2]
    self.y2 = value[3]
    return


def checkRegionXy(self, name, value):
    if value is None:  # is it None ?
        if self.type not in [
                'polyi', 'polyo']:  # if yes are we a poly type region ?
            return value  # No then that's ok to set it to None
        else:
            raise ValueError(
                name + ' type is poly therefore xy can not be None')
    # Are we of the right type (poly)
    elif self.type not in ['polyi', 'polyo']:
        raise ValueError('type is not poly you can not set xy')
    # now it's not None and it's the right type
    elif not isListorTuple(value):  # right type of data ?
        raise ValueError(name + ' must be a list or tuple')
    for v in value:
        if not isListorTuple(v):
            raise ValueError(name + ' must be a list or tuple of such')
        elif len(v) != 2:
            raise ValueError(name + ' tuples/list inside must be of length 2')
        for vv in v:
            if not isNumber(vv):
                raise ValueError(name + ' values must be numbers')
    return value


def checkRegionLine(self, name, value):
    if value is None:  # is it None ?
        if self.type in [
                'polyi', 'polyo']:  # if yes are we a poly type region ?
            return value  # Yes then that's ok to set it to None
        else:
            raise ValueError(
                name + ' type is not poly therefore line can not be None')
    elif self.type in ['polyi', 'polyo']:  # Are we of the right type (poly)
        raise ValueError('type is poly you can not set line')
    # now it's not None and it's the right type
    elif not isListorTuple(value):  # right type of data ?
        raise ValueError(name + ' must be a list or tuple')
    for v in value:
        if not isListorTuple(v):
            raise ValueError(name + ' must be a list or tuple of such')
        elif len(v) != 4:
            raise ValueError(name + ' tuples/list inside must be of length 4')
        for vv in v:
            if not isNumber(vv):
                raise ValueError(name + ' values must be numbers')
    return value


def checkRegionType(self, name, value):
    value = checkString(self, name, value).lower()
    if value not in ['polyi', 'polyo',
                     'above', 'below',
                     'left', 'right',
                     'horizi', 'horizo',
                     'verti', 'verto']:
        raise ValueError(name + " must be  'polyi','polyo'," +
                         "'above','below'," +
                         "'left','right'," +
                         "'horizi','horizo'," +
                         "'verti' or 'verto'")
    return value


def setAxesMinMax(self, name, value):
    setattr(self.graph, self._secretname + name, value)
    return value


def checkColor(self, name, value):
    if isinstance(value, str):
        for c in self.parent.Color:  # goes through the user defined colors
            if c.name == value:
                return c.id
        # if we reach that then the name wasn't a user defined name !
        # Let's check with the xmgrace defined names
        if not value.lower() in [
                'white', 'black', 'red', 'green', 'blue',
                'yellow', 'brown', 'grey',
                'violet', 'cyan', 'magenta', 'orange', 'indigo', 'maroon',
                'turquoise', 'green']:
            raise ValueError(name + ' color: ' + value +
                             ' is not user defined or xmgrace default name')
        else:
            return checkInStringsListInt(self, name, value, ['white', 'black',
                                                             'red', 'green',
                                                             'blue', 'yellow',
                                                             'brown', 'grey',
                                                             'violet', 'cyan',
                                                             'magenta',
                                                             'orange',
                                                             'indigo',
                                                             'maroon',
                                                             'turquoise',
                                                             'green'])

    else:
        return checkPositiveInt(self, name, value)


def changeGraph(self, name, value):
    value = checkInt(value)
    self.parent.Graph[value].Set.append(
        self.parent.Graph[self.graph].Set.pop(self.id))
    for g in self.parent.Graph:
        g.nset = len(g.Set)
    return value
