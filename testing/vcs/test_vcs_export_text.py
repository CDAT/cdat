import cdms2, vcs, tempfile

x = vcs.init(bg=1, geometry=(800, 600))
txt = x.createtext()
txt.x = [0.2, 0.2, 0.5, 0.8, 0.8]
txt.y = [0.2, 0.8, 0.5, 0.8, 0.2]
txt.string = ["SAMPLE TEXT A","SAMPLE TEXT B","SAMPLE TEXT C","SAMPLE TEXT D","SAMPLE TEXT E"]
txt.halign = "center"
txt.valign = "base"
txt.height = 10
x.plot(txt)

tmpfile = tempfile.NamedTemporaryFile(suffix='.ps', \
              prefix='textAsPathsFalse', delete=False)
x.postscript(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.ps', \
              prefix='textAsPathsTrue', delete=False)
x.postscript(tmpfile.name, textAsPaths=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.pdf', \
              prefix='textAsPathsFalse', delete=False)
x.pdf(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.pdf', \
              prefix='textAsPathsTrue', delete=False)
x.pdf(tmpfile.name, textAsPaths=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.svg', \
              prefix='textAsPathsFalse', delete=False)
x.svg(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.svg', \
              prefix='textAsPathsTrue', delete=False)
x.svg(tmpfile.name, textAsPaths=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.eps', \
              prefix='textAsPathsFalse', delete=False)
x.eps(tmpfile.name, textAsPaths=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.eps', \
              prefix='textAsPathsTrue', delete=False)
x.eps(tmpfile.name, textAsPaths=True)
tmpfile.close()