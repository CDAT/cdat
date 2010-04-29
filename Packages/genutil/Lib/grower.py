# Adapted for numpy/ma/cdms2 by convertcdms.py
# Adapted for numpy/ma/cdms2 by convertcdms.py
import string
import cdms2 as cdms

def grower(x, y, singleton=0):
    """
    Function: grower

    Description of function:
        This function takes 2 transient variables and grows them to
        match their axes.
    
    Usage:
        x, y = grower(x, y, singleton=singletonoption)

    Options:
        singletonoption 0 | 1
        Default = 0 If singletonoption is set to 1 then an error is
        raised if one of the dims is not a singleton dimension.    
    """
    # Parse the x axes
    xorder=x.getOrder(ids=1)
    xaxes=x.getAxisList()
    # Parse the y axes
    yorder=y.getOrder(ids=1)
    yaxes=y.getAxisList()

    # Now determine the shape of the final array (matching x and y dims,x first)
    forder=[]
    prev=0
    txt=''
    for o in xorder:
        if o=='(' :
            prev=1
        elif prev==1:
            if o!=')':
               txt=txt+o
            else:
                forder.append('(%s)' % txt)
                prev=0
                txt=''
        else:
            forder.append(o)

    prev=0
    txt=''
    xorder=forder[:]
    nyorder=[]
    for o in yorder:
        if o=='(' :
            prev=1
        elif prev==1:
            if o!=')':
               txt=txt+o
            else:
                nyorder.append('(%s)' % txt)
                if not '(%s)' % txt in forder:
                    forder.append('(%s)' % txt)
                prev=0
                txt=''
        else:
            nyorder.append(o)
            if not o in forder:
                forder.append(o)
    yorder=nyorder
    # Now grow x
    #print forder,xorder,yorder,nyorder
    for o in forder:
        if not o in xorder:
            for i in range(len(yorder)):
                if yorder[i]==o :
                    newaxes=x.getAxisList()
                    ax=yaxes[i]
                    if len(ax)>1 and singleton==1:
                        raise 'Error, dimension:'+ax.id+'is not a singleton dimension,(len is:'+ \
                              str(len(ax))+ \
                              ') you specified to grow only singleton dims, exiting'
                    xsh=list(x.shape)
                    xsh.insert(0,len(ax))
                    x=cdms.MV2.resize(x,xsh)
                    newaxes.insert(0,ax)
                    x.setAxisList(newaxes)
    xorder=x.getOrder(ids=1)
    sp=string.split(xorder,'(')
    xorder=[]
    for s in sp:
        if string.find(s,')')==-1:
            for t in s:
                xorder.append(t)
        else:
            sp2=string.split(s,')')
            xorder.append(sp2[0])
            for t in sp2[1]:
                xorder.append(t)
    xaxes=x.getAxisList()


    # Now grow y
    #print forder,yorder
    for o in forder:
        if not o in yorder:
            for i in range(len(xorder)):
                if o in ['(%s)' % xorder[i], xorder[i] ] :
                    newaxes=y.getAxisList()
                    ax=xaxes[i]
                    if len(ax)>1 and singleton==1:
                        raise 'Error, dimension:'+ax.id+'is not a singleton dimension,(len is:'+ \
                              str(len(ax))+\
                                  ') you specified to grow only singleton dims, exiting'
                    ysh=list(y.shape)
                    ysh.insert(0,len(ax))
                    y=cdms.MV2.resize(y,ysh)
                    newaxes.insert(0,ax)
                    y.setAxisList(newaxes)
    # Figure out the string to reorder x and y
    #print x.shape,y.shape
    return x(order=forder),y(order=forder)
