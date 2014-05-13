import vcs
x=vcs.init()
fa=x.createfillarea()
fa.x=[.2,.8,.8,.2]
fa.y=[.2,.2,.8,.8]
fa.color= 242
fa.list()
x.plot(fa)
raw_input("Press enter")

