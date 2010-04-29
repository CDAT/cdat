import cdutil.continent_fill

def plot(parent,slab,template_name,g_type,g_name,bg,ratio):
    try:
        x=parent.vcs[ parent.vcs_id ]
        cf=cdutil.continent_fill.Gcf()
        g=x.generate_gm(g_type.lower(),g_name)
        lons=slab.getLongitude()
        lats=slab.getLatitude()

        if lons is None or lats is None:
            return
        if g.datawc_x1>9.9E19:
            cf.datawc_x1=lons[0]
        else:
            cf.datawc_x1=g.datawc_x1
        if g.datawc_x2>9.9E19:
            cf.datawc_x2=lons[-1]
        else:
            cf.datawc_x2=g.datawc_x2
        if g.datawc_y1>9.9E19:
            cf.datawc_y1=lats[0]
        else:
            cf.datawc_y1=g.datawc_y1
        if g.datawc_y2>9.9E19:
            cf.datawc_y2=lats[-1]
        else:
            cf.datawc_y2=g.datawc_y2
        try:
            t=x.gettemplate(template_name)
##             t.data.list()
            cf.plot(x=x,template=template_name,ratio=ratio)
        except Exception,err:
            print err
    except Exception,err:
        print err
