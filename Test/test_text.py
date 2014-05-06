import vcs
x=vcs.init()


txt = x.createtextcombined()

txt.x = [.5]
txt.y = [.5]
txt.string= ['SOME TEXT',]

txt.color = 242

x.plot(txt)

raw_input("press enter")

