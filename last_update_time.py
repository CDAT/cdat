import os

ln=os.popen('p4 changes -m 1 //depot/main/...').readlines()


for l in ln:
    sp=l.split()
    date='_'.join(sp[3].split('/'))
    date=sp[3]
    print 'Last change on:',date, 'for more info run: p4 changes -m 1 //depot/main/...'
    
    
