import genutil,sys

## Test reading of ascii file organized in columns
print 'Testing genutil.ASCII.read_col'
try:
    vars = genutil.ASCII.read_col(sys.prefix+'/sample_data/test_col.asc',header=4,cskip=1,idrow=True,axis=True)
except Exception,err:
    print 'genutil.ASCII: Data reading failed!\nCheck genutil.ASCII\nError message: %s' % err
    sys.exit(1)
    
if len(vars)!=3:
    print 'genutil.ASCII: Error should have returned 3 variables, returned: %s\nCheck cskip option or axis=True option' % len(vars)
    sys.exit(1)

ids_ok = ['TEST','TEST2','TEST4']
for i in range(3):
    v=vars[i]
    ax = v.getAxis(0)
    if v.id!=ids_ok[i]:
        print 'genutil.ASCII: Error axis on variables should be named axis, looks like the axis setup failed!\nCheck idrow=True option'
        sys.exit(1)
    if ax.id!='axis':
        print 'genutil.ASCII: Error axis on variables should be named axis, looks like the axis setup failed!\nCheck axis=True option'
        sys.exit(1)
    if (ax[:]!=[1.,2.,2.13,4.]).any():
        print ax[:]
        print 'genutil.ASCII: Error axis on variables have wrong values, looks like the axis setup failed!\nCheck axis=True option'
        sys.exit(1)

print 'genutil.ASCII.read_col tested ok'

print 'Testing genutil.ASCII.readAscii'
try:
    vars = genutil.ASCII.readAscii(sys.prefix+'/sample_data/testASCII.asc',header=5)
except:
    print 'genutil.ASCII.readAscii error: could not read in data test, check headr keyword?'
    sys.exit(1)
    
if len(vars)!=4:
    print 'Error reading in data test, we should return 4 var'
    sys.exit(1)
lens = [3312,15520,46,72]
for i in range(4):
    if len(vars[i])!=lens[i]:
        print 'genutil.ASCII.readAscii error: variable %s returned with wrong length (%s) instead of %s\nCheck header keyword?' % (i,len(vars[i]),lens[i])
        sys.exit(1)
shapes = [(46,72),(1,2, 80, 97),(46,),(72,)]
ids = ['clt','u','latitude','longitude']
try:
    vars = genutil.ASCII.readAscii(sys.prefix+'/sample_data/testASCII.asc',header=5,shape=shapes,ids=ids)
except Exception,err:
    print 'genutil.ASCII.readAscii error: could not read in data test\nCheck shape or ids options\nError message was: %s' % err
    sys.exit(1)
if len(vars)!=4:
    print 'Error reading in data test, we should return 4 var'
    sys.exit(1)
for i in range(4):
    if vars[i].id!=ids[i]:
        print 'genutil.ASCII.readAscii error: variable %s returned with wrong name (%s) instead of %s\nCheck header keyword?' % (i,vars[i].id,ids[i])
        sys.exit(1)
    if vars[i].shape!=shapes[i]:
        print 'genutil.ASCII.readAscii error: variable %s returned with wrong shape (%s) instead of %s\nCheck header keyword?' % (i,vars[i].shape,shapes[i])
        sys.exit(1)

print 'genutil.ASCII.readAscii tested ok'

   
