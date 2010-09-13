import MV2
from markError import clearError,markError,reportError

clearError()
print 'Test 15: reshape and mask and average ...',
a=MV2.arange(100)
try:
    failed = False
    a.shape=(10,10)
except:
    failed = True
    a = MV2.reshape(a,(10,10))
if failed is False: markError('shape should not have worked (protected attribute)')
if len(a.getAxisList())!=2: markError('reshape did not produce 2 axes')
        
a=MV2.masked_greater(a,23)
b=MV2.average(a,axis=0)
c=a-b

reportError()
