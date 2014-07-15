import vcs
x=vcs.init()
fa=x.createfillarea()
fa.x=[
       [.2,.49,.49,.2],
       [.51,.8,.8,.51],
       ]
fa.y=[
      [.2,.2,.8,.8],
      [.2,.2,.8,.8],
      ]
fa.color= [242,244]
fa.worldcoordinate=[0,10,0,1]
fa.viewport=[0,1,0,.5]
fa.list()
x.plot(fa)
raw_input("Press enter")

