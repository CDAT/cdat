#!/usr/bin/env python
import vcs,os

version = '1.0'
general_description = """
    Reads in and converts Ferret (spk) colormap file to vcs colormap
    If method is set to 'blend':
      colors will be set using the ferret % value, and blending will be used in between
      0% in ferret corresponds to index_start
      100% in ferret corresponds to index_end
    If method is set to 'contiguous':
      colors will be set starting at index_start and assigned in order as found in the ferret (spk) file, no blending between colors
    """

def spk2vcs(file,cname=None,x=None,index_start=16,index_end=239,method='blend',verbose=False):
    """ %s
    Usage:
    cmap, ncolors = spk2vcs(file,cname=None,x=None)
    Input:
    file                     : Ferret (spk) colormap file
    cname                    : VCS output colormap name, if None, uses ferret file name
    x                        : vcs canvas, if None then a vcs canvas instance will be created
    index_start              : 0%% of ferret %% index, default is 16
    index_end                : 100%% of ferret %% index, defalut is 239
    method                   : 'blend' or 'adjacent', defalut is 'blend'
    Output:
    cmap                     : vcs colormap object, with conitguous color set from index_Start if method='contiguous'
                               or spread from index_start to index_end if method is 'blend'
    """ 

    f=open(file)
    ln=f.readlines()
    # Treat colormap name
    if cname is None:
        cname = '.'.join(os.path.split(op.file)[-1].split('.')[:-1])
        if verbose: print 'Colormap name:',cname

    if x is None:
        x=vcs.init()
    cmap=x.createcolormap(cname)
    x.setcolormap(cmap.name)
    ncolors = 0
    last_index = index_start
    if verbose: print 'Method:',method
    for l in ln:
        sp=l.split()
        if len(sp)!=4: # Is it a line with 4 values (p,r,g,b)?
            continue
        p,r,g,b=sp
        try: # Are the 4 values float?
            p=float(p)
            r=float(r)
            g=float(g)
            b=float(b)
        except:
            continue
        if method == 'contiguous':
            x.setcolorcell(index_start + ncolors, int(r), int(g), int(b))
            if verbose: print 'Setting cell %s to: %s, %s, %s' % (index_start + ncolors, int(r), int(g), int(b))
            cmap=x.getcolormap(cmap.name)
            ncolors+=1
        else:
            index = index_start + int(p*(index_end-index_start)/100.)
            x.setcolorcell( index, int(r), int(g), int(b))
            cmap=x.getcolormap(cmap.name)
            if verbose: print 'Setting cell %s to: %s, %s, %s' % (index, int(r), int(g), int(b))
            dr = cmap.index[index][0] - cmap.index[last_index][0]
            dg = cmap.index[index][1] - cmap.index[last_index][1]
            db = cmap.index[index][2] - cmap.index[last_index][2]
            for indx in range(last_index+1,index):
                p = float(indx-last_index)/float(index-last_index)
                r = cmap.index[last_index][0]+int(p*dr)
                g = cmap.index[last_index][1]+int(p*dg)
                b = cmap.index[last_index][2]+int(p*db)
                x.setcolorcell(indx , r, g, b)
                if verbose: print '\t Sub-setting cell %s to: %s, %s, %s' % (indx , r, g, b)
                cmap=x.getcolormap(cmap.name)
            last_index = index
    return cmap
setattr(spk2vcs,'__doc__',spk2vcs.__doc__ %  general_description)

if __name__=='__main__':
    import optparse
    op=optparse.OptionParser(usage="%%prog [options]\n%s" % general_description,version="%%prog %s" % version)
    op.add_option("--file",dest='file',help="Ferret (spk) colormap file to convert, [default: %default]",default="pal1.spk")
    op.add_option("--name",dest="name",help="Name of the returned vcs colormap, [default: uses ferret (spk) file name]",default='default')
    op.add_option("--out",dest="out",help="Name of the returned vcs script file, [default: file.scr]",default='default')
    op.add_option("--index_start",dest="index_start",type='int',help='start index for mapping of ferret colors into vcs colormap, [default: %default]',default=16)
    op.add_option("--index_end",dest="index_end",type='int',help='end index for mapping of ferret colors into vcs colormap, [default: %default]',default=239)
    op.add_option("--method",dest="method",help='method for mapping of ferret colors into vcs colormap (blend or contiguous), [default: %default]',default='blend')
    op.add_option("--blend",dest="blend",action='store_true',help='end index for mapping of ferret colors into vcs colormap, overrides --method option',default=True)
    op.add_option("--contiguous",dest="blend",action='store_false',help='end index for mapping of ferret colors into vcs colormap, overrides --method option',default=True)
    op.add_option("--verbose",dest="verbose",action='store_true',help='Enable verbose screen output while converting colorcells, [default: %default]',default=False)
    
    op,args = op.parse_args()

    if op.method in [ 'contiguous','blend']:
        method = op.method
    else:
        op.error("options method can ONLY be either blend or contiguous")

    if op.blend is True:
        method = 'blend'
    else:
        method = 'contiguous'

    if op.name == 'default':
        cname = None
        
    cmap  = spk2vcs(op.file,index_start=op.index_start,index_end=op.index_end,method=method,cname=cname,verbose=op.verbose)

    if op.out == 'default':
        oname = '.'.join(os.path.split(op.file)[-1].split('.')[:-1])+'.scr'
    cmap.script(oname)
    print 'Done, colormap converted to VCS using "%s" method from index %s to index %s\nStored in file: %s' % (method,op.index_start,op.index_end,oname)
    
      
