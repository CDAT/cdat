# Import the modules needed for the tuturial
import vcs, sys

# Initial VCS:
v = vcs.init()

# Create and draw a "Text" on the VCS Canvas.
tex = v.createtext('nTtex','std','nToex','7left')
tex.height = 20
tex.x=[0.1, 0.1, 0.1]
tex.y=[0.97, 0.93, 0.89]
tex.string=['Example of drawing VCS low-level primitives.',
  'Also, an example of using the viewport to clip graphics.',
  'Here the viewport is the default [0,1,0,1], see markers below']
v.plot(tex)

mk=v.createmarker('new')
mk.x=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
mk.y=[0.84,0.84,0.84,0.84,0.84,0.84,0.84,0.84,0.84]
mk.color=244
mk.size=7
mk.type='dot'
v.plot(mk)

t1 = v.createtext('new1')
t1.x=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
t1.y=[0.82,0.82,0.82,0.82,0.82,0.82,0.82,0.82,0.82]
t1.string=['x=0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9']
t1.height = 15
v.plot(t1)

# Let the world coordinate values remain the default
# setting (i.e.,[0, 1, 0, 1]), but change the global viewport.
v.viewport=[0.2,0.8,0.1,0.7]

# Create the line, marker, fill area, and text objects
ln=v.createline('new')
fa=v.createfillarea('new')
t=v.createtext('newTt','std','newTo','7left')

# Draw a box around the entire VCS Canvas viewport. It will
# use the global viewport setting above and use the default
# world coordinate values:
# Remember the default values for vp and wc are:
#            viewport         = [0.0, 1.0, 0.0, 1.0]
#            worldcoordinate  = [0.0, 1.0, 0.0, 1.0]
ln.x=[0, 1, 1, 0, 0]            # x line positions
ln.y=[0, 0, 1, 1, 0]            # y line positions
ln.width=4                      # test width
ln.color = 242                  # test color
ln.type = 4                     # test line type
v.plot(ln)                      # plot lines

t3 = v.createtext('new3')
t3.x=[0.04]
t3.y=[0.91]
t3.color = 242
t3.string=['Here the vieport was changed to [0.2, 0.8, 0.1, 0.7]']
t3.height = 18
v.plot(t3)

mk1=v.createmarker('new1')
mk1.x=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
mk1.y=[0.84,0.84,0.84,0.84,0.84,0.84,0.84,0.84,0.84]
mk1.color=242
mk1.size=6
mk1.type='dot'
v.plot(mk1)

t2 = v.createtext('new2')
t2.x=[0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9]
t2.y=[0.81,0.81,0.81,0.81,0.81,0.81,0.81,0.81,0.81]
t2.string=['x=0.1','0.2','0.3','0.4','0.5','0.6','0.7','0.8','0.9']
t2.height = 13
t2.color = 242
v.plot(t2)

# Draw text on the screen. Use global vp and default wc.
t.x = [0.2, 0.5]                # x text positions
t.y = [0.7, 0.6]                # y text positions
t.string=['PCMDI', 'CDAT']      # text strings
t.color=243                     # text color
t.height = 50                   # text size
v.plot(t)                       # plot text

# Draw fill area on screen. Use global vp and default wc.
fa.x=[[0.0,0.5,0.5,0.0],[0.5,1.0,0.75] ]# x fill area positions
fa.y=[[0.0,0.0,0.5,0.5],[0.5,0.5,0.0] ] # x fill area positions

fa.color=[241,245]      # fill area color
fa.style='hatch'        # fill area style
fa.index=3              # fill area index
v.plot(fa)              # plot fill area

