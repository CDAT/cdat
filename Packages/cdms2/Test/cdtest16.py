import MV2
print 'Test 16: changing shape  ...',
a=MV2.ones((1,1,4,5))
assert a.shape == (1,1,4,5)
a=MV2.reshape(a,(4,5))
assert a.shape == (4,5)
print 'OK'
