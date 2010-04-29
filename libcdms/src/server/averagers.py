import VCSRegion

def djf_average (v):
    ar, keyargs = seasonal_average(v, [11, 0, 1])
    keyargs['long_name'] = 'Dec-Jan-Feb Average'
    return ar, keyargs

def mam_average(v):
    ar, keyargs = seasonal_average(v, [2,3,4])
    keyargs['long_name'] = 'Mar-Apr-May Average'
    return ar, keyargs

def jja_average (v):
    ar, keyargs = seasonal_average(v, [5,6,7])
    keyargs['long_name'] = 'Jun-Jul-Aug'
    return ar, keyargs

def son_average (v):
    ar, keyargs = seasonal_average(v, [8,9,10])
    keyargs['long_name'] = 'Sep-Oct-Nov Average'
    return ar, keyargs

def seasonal_average(v, months):
    "Return the average of self.var over a given list of months numbers (0 based)"
    import cdms

    it = -1
    order = v.getOrder()
    for i in range(len(order)):
        if order[i] == 't':
            it = i
            break
    if it < 0:
        print "Array is not suitable for jja averaging."
        return None
    ar, keyargs = VCSRegion.getRegion(v, level=None, other=None)
    if it == 0:
        a = ar[months[0], ...]
        for k in months[1:]:
            a = a + ar[k, ...]
        a = a / (1.0 * len(months))
    return a, keyargs
