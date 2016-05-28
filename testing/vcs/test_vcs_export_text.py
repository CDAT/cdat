import cdms2, vcs, tempfile

x = vcs.init(bg=1, geometry=(800, 600))
txt = x.createtext()
txt.x = [.0000005,.00000005,.5,.99999,.999999]
txt.y = [0.05,.9,.5,.9,0.05]
txt.string = ["SAMPLE TEXT A","SAMPLE TEXT B","SAMPLE TEXT C","SAMPLE TEXT D","SAMPLE TEXT E"]
txt.halign = "center"
txt.valign = "base"
txt.height = 10
x.plot(txt)

tmpfile = tempfile.NamedTemporaryFile(suffix='.ps', \
              prefix='textAsPathsFalse', delete=True)
x.postscript(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.ps', \
              prefix='textAsPathsTrue', delete=True)
x.postscript(tmpfile.name, textAsPaths=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.pdf', \
              prefix='textAsPathsFalse', delete=True)
x.pdf(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.pdf', \
              prefix='textAsPathsTrue', delete=True )
x.pdf(tmpfile.name, textAsPaths=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.svg', \
              prefix='textAsPathsFalse', delete=True)
x.pdf(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.svg', \
              prefix='textAsPathsTrue', delete=True)
x.pdf(tmpfile.name, textAsPaths=True)
tmpfile.close()