import vcs_legacy,sys
x=vcs_legacy.init()
fonts=[]
if len(sys.argv)>1 and not '-extended' in sys.argv:
    for font_file in sys.argv[1:]:
        fonts.append(x.addfont(font_file))
## else:
##     fonts = x.addfont("/home/doutriaux1/Fonts","r")
##     fonts.sort()
    
l1d = '` 1 2 3 4 5 6 7 8 9 0 - ='
l1u = '~ ! @ # $ % ^ & * ( ) _ +'
l2d = 'q w e r t y u i o p [ ] \\'
l2u = 'Q W E R T Y U I O P { } |'
l3d = 'a s d f g h j k l ; \''
l3u = 'A S D F G H J K L : "'
l4d = 'z x c v b n m , . /'
l4u = 'Z X C V B N M < > ?'
linesu = [l1u,l2u,l3u,l4u]
linesd = [l1d,l2d,l3d,l4d]

center = x.createline()
center.x=[.2,.8]
center.y=[.5,.5]

font = 2
fname = x.createtext()
fname.x=[.025]
fname.y=[.95]
fname.halign='left'
bg=0
t=x.createtemplate()

def showfont(font,linesu,linesd,outnm=None,pause=False):
    fnm= x.getfont(font)
    fname.string=fnm
    x.plot(fname,t,bg=bg)
    for i in range(4):
        u=x.createtext()
        d=x.createtext()
        u.font=font
        d.font=font
        u.height=25
        d.height=25
        u.x=[.5]
        u.halign='center'
        u.valign='half'
        d.x=[.5]
        d.halign='center'
        d.valign='half'
        u.y = [.9 - float(i)/10.]
        d.y = [.4 - float(i)/10.]
        u.string=linesu[i].replace(" ",'')
        d.string=linesd[i].replace(" ",'')
        x.plot(u,t,bg=bg)
        x.plot(d,t,bg=bg)
    x.plot(center,t,bg=bg)
    fnm=fnm.replace(" ","_")
    if outnm is None:
        x.postscript('font_'+fnm)
    else:
        x.postscript(outnm,mode='a')
    if pause:
        raw_input("Press Enter to continue")
    x.clear()

counter=0
nfonts = len(fonts)
if nfonts==1:
    pause=True
else:
    pause=False
    
for f in fonts:
    if f is None:
        continue
    counter+=1
    font = x.getfont(f)
    print 'Drawing font %s , index is %i (drawing %i out of %i)' % (f,font,counter,nfonts)
    showfont(font,linesu,linesd,'all_fonts_big',pause=pause)
