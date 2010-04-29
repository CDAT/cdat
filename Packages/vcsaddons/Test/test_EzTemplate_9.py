import EzTemplate,sys
## Test for request out of def column/row template

M=EzTemplate.Multi(rows=4,columns=3)

# Test a good one
tok = M.get(row=3,column=2)

# Test out of range column
try:
    tbad = M.get(row=2,column=3)
    failed = False
except Exception,err:
    failed = True
if failed:
    print 'Ok out of range column works with error message:',err
else:
    raise Exception,'Out of range column should have failed it did not!'

# Test out of range row
try:
    tbad = M.get(row=4,column=2)
    failed = False
except Exception,err:
    failed = True
if failed:
    print 'Ok out of range row works with error message:',err
else:
    raise Exception, 'Out of range row should have failed it did not!'

