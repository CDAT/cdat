import vcs_legacy,support
if support.dogui:
    x=vcs_legacy.init()
    x.graphicsmethodgui(gm_type='boxfill', gm_name='ASD')
else:
    print 'You need to run this one by hand (turn support.dogui to 1 first)'
