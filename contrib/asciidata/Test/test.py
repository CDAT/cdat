import asciidata, numpy

d = asciidata.tab_delimited('example2', omit=['Observation'])
print 'example2'
for k, v in d.items():
    print k
    print v
print '----------------------------------------'

d = asciidata.tab_delimited('example2', 
                              typecodes=[numpy.int, numpy.float32, numpy.float, numpy.float])
print 'example2, with typecodes'
for k, v in d.items():
    print k
    print v
print '----------------------------------------'

d = asciidata.comma_delimited('example3')
print 'example3'
for k, v in d.items():
    print k
    print v
print '----------------------------------------'
