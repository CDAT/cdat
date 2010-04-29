def statusbar(status,total=1.,prev=-1,title='Done',tk=None):
    '''
    # Create the illusion of a progressing bar
    # Date: August 1st, 2000
    # Author: Charles Doutriaux
    # Version 1.0.1
    # email: doutriaux1@llnl.gov
    # Input:
    # status : percentage done, or number of element done
    # total (optional): total number of element to be done
    # prev (optional) just a value returned by the function
        use only if you want the little animation at the end
        of the bar, pass 0 the first time
    # tk : if not None then display a tk statusbar rather than command line
    # Note that if you set statusbar.tk__ to anything but None then tk is all the time
    # When using the tk option, you can pass a list for status and/or total
    # then multiple bar will be plotted
    # Note if you pass a list for status then tk is automatically used
    # Externals: sys,Pmw/Blt (for Tk)

    # Warnings:
    # No carriage return can be sent to
        the sys.stdout in between 2 calls

    # Example of use with Tk and text:
    prevall=0
    previ=0
    prevj=0
    prevtxt=0
    ni=100
    nj=20
    for i in range(ni):
    previ=statusbar(float(i)/ni,prev=previ,tk=1)
    for j in range(nj):
        prevj=statusbar(float(j)/nj,prev=prevj,tk=1)
        prevall=statusbar([float(i)/ni,float(j)/nj],prev=prevall,tk=1,title='Test')
        prevtxt=statusbar(float(j)/nj,prev=prevtxt,tk=None,title='Test')


    '''
    import sys,string
    chardic={
      0:'|',
      1:'/',
      2:'-',
      3:'|',
      4:'\\',
      5:'-',
      }
    if prev==0 or prev==-1 : prev=[0,0]
    ntot=40.  # Length of the bar on screen
    nend=0
    if 'tk__' in dir(statusbar) :
        tk=statusbar.tk__
    if type(status) in [type([]),type(())] :
        tk=1
    if tk is None: 
        status=float(status)
        while status>total : status=status/100. # Makes sure status is < total
        for n in xrange(int(ntot+15+len(title))) : sys.stdout.write('\b') # Remove the previous line
        sys.stdout.write(title[:10]+' : ')
        for n in range(int(ntot)):  # Plot a character
            if status/total > n/ntot :
                sys.stdout.write('#')  # Character for already accomplished
                nend=n+1
            else:
                if prev!=-1:
                    if n==prev[0]:
                        a=chardic[prev[1]]
                        prev[1]=(prev[1]+1)%len(chardic)
                        sys.stdout.write(a)  # Character for animation
                    else:
                        sys.stdout.write(' ')  # Character for 'to be accomplished'
                else:
                    sys.stdout.write(' ')  # Character for 'to be accomplished'
        # add some counter at the end of the line
        sys.stdout.write('| %2.2f%s' % (status/total*100.,'%'))
        if status==total : sys.stdout.write('\n') # no little bell at the end
        sys.stdout.flush()  # output to the screen
        if prev!=-1 : prev[0]=nend
        return prev
    else:
        # the tk part
        # Allows control over multiple task, just pass a list for status and total
        import Pmw,Tkinter
        if not type(status) in [type([]),type(())]:
            status=[status]
            total=[total]
        nbar=len(status)
        colorList = Pmw.Color.spectrum(nbar)
        sheigth=str(25*nbar+50)
        if not type(total) in [type([]),type(())]:
            total=[total]
            for i in range(nbar-1):
                total.append(total[0])
        hsave=20.
        wsave=10.
        if prev==[0,0] :
            master=Tkinter.Toplevel()
            #Pmw.Blt._checkForBlt(master)
            master.geometry('200x'+sheigth)
            master.resizable(0,0)
            master.update()
            canvas=Tkinter.Canvas(master,bg='white')
            g=master.geometry()
            w=string.split(g,'x')[0]
            h=string.split(string.split(g,'x')[1],'+')[0]
            canvas['width']=w
            canvas['height']=h
            bars=[]
            txts=[]
            for i in range(nbar):
                h=string.atof(sheigth)
                h2=h*(1.-hsave/100.)
                h2=h/float(nbar)
                w=string.atof(200)
                x0=w*(wsave/200.)
                X=w*(1.-wsave/100.)
                x1=x0+X*status[i]/total[i]
                y0=h*.1/float(nbar+1)*i+h2*i
                y1=y0+h2*(i+1)
                bars.append(canvas.create_rectangle(x0,y0,x1,y1,fill=colorList[i],outline=colorList[i]))
                txts.append(canvas.create_text(x0+X/2.,y0+h2/2.))
            canvas.pack()
            
        else:
            master,canvas,bars,txts=prev
        
        tit=''
        for i in range(nbar):
            g=master.geometry()
            w=string.split(g,'x')[0]
            h=string.split(string.split(g,'x')[1],'+')[0]
            canvas['width']=w
            canvas['height']=h
            tmptit='%3.0f ' %((status[i]/total[i])*100.)
            tit=tit+tmptit+'%, '
            canvas.itemconfigure(txts[i],text=tmptit+'%')
            h=string.atof(h)
            h2=h*(1.-hsave/100.)
            h2=h2/float(nbar)
            w=string.atof(w)
            x0=w*(wsave/200.)
            X=w*(1.-wsave/100.)
            x1=x0+X*status[i]/total[i]
            y0=h*(hsave/100.)/float(nbar+1)*(i+1)+h2*i
            y1=y0+h2
            canvas.coords(bars[i],x0,y0,x1,y1)
            
        tit=title+': '+tit[:-2]
        master.title(tit)
        master.update()
            
        return master,canvas,bars,txts
            








