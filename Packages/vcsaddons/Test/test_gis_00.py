import vcsaddons._gis
sources = ['../Data/fe_2007_06_county.dbf','../Data/co1990p020.dbf']

for s in sources:
    D = vcsaddons._gis.readdbffile(s)
    print D.keys()

    try:
        print D['NAME']
    except:
        print D['COUNTY']
