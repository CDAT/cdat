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
              prefix='tmpTextAsObjectFalse', delete=True)
x.postscript(tmpfile.name, textAsObject=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.ps', \
              prefix='tmpTextAsObjectTrue', delete=True)
x.postscript(tmpfile.name, textAsObject=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.pdf', \
              prefix='tmpTextAsObjectFalse', delete=True)
x.pdf(tmpfile.name, textAsObject=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.pdf', \
              prefix='tmpTextAsObjectTrue', delete=True )
x.pdf(tmpfile.name, textAsObject=True)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.svg', \
              prefix='tmpTextAsObjectFalse', delete=True)
x.pdf(tmpfile.name, textAsObject=False)
tmpfile.close()

tmpfile = tempfile.NamedTemporaryFile(suffix='.svg', \
              prefix='tmpTextAsObjectTrue', delete=True)
x.pdf(tmpfile.name, textAsObject=True)
tmpfile.close()