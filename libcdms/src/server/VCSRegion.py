def getSubAxis(s, x):
    """Get a subaxis of s specified by x, cdms notation"""
    if x == ':': return s
    
    k1, k2 = s.mapInterval(x)
    t = s.subaxis(k1,k2)
    return t

def getRegion(v,longitude=':', latitude=':', level=':', time=':', other=':'):
    """Get region and keyword information appropriate for vcs plotting."""
    kw = getVCSKeywords(v)
    order = v.getOrder()
    rank = len(order)
    vargs = [None]*rank
    for i in range(rank):
        c = order[i]
        s = v.getAxis(i)
        if c == 'x':
            if longitude is None:
                longitude = (s[0],s[0],'cc')
            if s[0] > s[-1]:
                kw['xrev'] = 1
            kw['xaxis'] = getSubAxis(s, longitude)
            vargs[i] = longitude
        elif c == 'y':
            if latitude is None:
                latitude = (s[0],s[0],'cc')
            if s[0] > s[-1]:
                kw['yrev'] = 1
            kw['yaxis'] = getSubAxis(s, latitude)
            vargs[i] = latitude
        elif c == 'z':
            if level is None:
                level = (s[0],s[0],'cc')
            kw['zaxis'] = getSubAxis(s, level)
            vargs[i] = level
        elif c == 't':
            if time is None:
                time = (s[0],s[0],'cc')
            kw['taxis'] = getSubAxis(s, time)
            vargs[i] = time
        else: # '-'
            if other is None:
                other = (s[0],s[0],'cc')
            kw['waxis'] = getSubAxis(s, other)
            vargs[i] = other
            
    a = apply(v.getRegion, tuple(vargs))
    return a, kw

def getVCSKeywords (v):
    kw = {}
    kw['bg'] = 1
    kw['missing_value'] = v.getMissing()
    kw['name'] = getattr(v, 'id', 'unnamed')
    kw['long_name'] = getattr(v,'long_name', kw['name'])
    kw['file_comment'] = getattr(getattr(v,'parent',None), 'comment', '')
    kw['units'] = getattr(v, 'units', '')
    kw['comment1'] = getattr(v, 'comment1', '')
    kw['comment2'] = getattr(v, 'comment2', '')
    kw['comment3'] = getattr(v, 'comment3', '')
    kw['hms'] = getattr(v, 'time', '')
    kw['ymd'] = getattr(v, 'date', '')
    return kw
