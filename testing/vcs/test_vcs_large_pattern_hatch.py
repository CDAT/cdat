import os, sys, vcs, vcs.testing.regression as regression

canvas = regression.init()
fillarea = vcs.createfillarea()
fillarea.x = [[0, .33, .33, 0], [.33, .67, .67, .33], [.67, 1, 1, .67]]
fillarea.y = [[0, 0, 1, 1]] * 3
fillarea.style = ["solid", "pattern", "hatch"]
fillarea.index = [1, 5, 5]
fillarea.color = [50, 50, 50]
canvas.plot(fillarea, bg=True)
fnm = os.path.abspath("test_vcs_large_pattern_hatch.png")
regression.run(canvas, fnm)
