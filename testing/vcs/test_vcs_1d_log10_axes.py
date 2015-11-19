import vcs

data = [1,10,100,1000,10000,100000]

x=vcs.init()
gm = x.create1d()
x.plot(data,gm)

raw_input("yep")
