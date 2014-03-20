import vcs_legacy
## testing color split

ncols = 100
def verify_cols(colors,ok):
    for i in range(len(colors)):
        if colors[i]!=ok[i]:
            raise Exception,'Error colors (%s) are not matching good colors (%s)' % (colors,ok)

values = [-5,-4,-3,-2,-1,0,1,2,3,4,5]
ok_cols = [0, 12, 25, 37, 49, 50, 62, 75, 87, 99]
cols = vcs_legacy.getcolors(values,colors=range(ncols))
verify_cols(cols,ok_cols)

values = [-5,-4,-3,-2,-1,0,1,2,3]
ok_cols = [0, 12, 25, 37, 49, 50, 75, 99]
cols = vcs_legacy.getcolors(values,colors=range(ncols))
verify_cols(cols,ok_cols)

values = [-5,-4,-3,-2,-1,1,2,3,4,5]
ok_cols = [0, 16, 33, 49, 240, 50, 66, 83, 99]
cols = vcs_legacy.getcolors(values,colors=range(ncols))
verify_cols(cols,ok_cols)

values = [-5,-4,-3,-2,-1,1,2,3]
ok_cols = [0, 16, 33, 49, 240, 50, 99]
cols = vcs_legacy.getcolors(values,colors=range(ncols))
verify_cols(cols,ok_cols)
