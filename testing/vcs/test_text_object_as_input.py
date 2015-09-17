import vcs

x=vcs.init()

tt=x.createtexttable()
to=x.createtextorientation()

t=x.createtemplate()

t.title.texttable = tt
t.title.textorientation = to


t.title.list()
