import vcs

x=vcs.init()

l = x.createline()

l.x=[.2,.8]
l.y=[.5,.5]
l.color = 242
l.width = 5
#l.type='dot'

x.plot(l)

raw_input("Press enter")

