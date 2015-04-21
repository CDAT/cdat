import vcs

x=vcs.init()
iso = x.getisofill("a_isofill")

assert(iso.levels!=[])
