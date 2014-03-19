import vcs,support
if support.dogui:
    x=vcs.init()
    x.templateeditor(template_name='ASD')
else:
    print 'You need to run this one by hand (turn support.dogui to 1 first)'

