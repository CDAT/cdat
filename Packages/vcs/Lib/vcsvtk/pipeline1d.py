from .pipeline import Pipeline

import numpy
import vcs


class Pipeline1D(Pipeline):
    """Implementation of the Pipeline interface for 1D VCS plots."""

    def __init__(self, context_):
        super(Pipeline1D, self).__init__(context_)

    def plot(self, data1, data2, tmpl, gm, grid, transform):
        """Overrides baseclass implementation."""
        Y = self._context.trimData1D(data1)
        if data2 is None:
            X = Y.getAxis(0)
        else:
            X = Y
            data1._yname = data2.id
            Y = self._context.trimData1D(data2)

        if gm.flip:
            tmp = Y
            Y = X
            X = tmp

        if gm.smooth is not None:
            Y = smooth(Y, gm.smooth)

        l = self._context.canvas.createline()
        Xs = X[:].tolist()
        Ys = Y[:].tolist()
        xs = []
        ys = []
        prev = None
        for i, v in enumerate(Ys):
            if v is not None and Xs[i] is not None:  # Valid data
                if prev is None:
                    prev = []
                    prev2 = []
                prev.append(Xs[i])
                prev2.append(v)
            else:
                if prev is not None:
                    xs.append(prev)
                    ys.append(prev2)
                    prev = None

        if prev is not None:
            xs.append(prev)
            ys.append(prev2)

        l._x = xs
        l._y = ys
        l.color = gm.linecolor
        if gm.linewidth > 0:
            l.width = gm.linewidth
        else:
            l.priority = 0
        l.type = gm.line
        l._viewport = [tmpl.data.x1, tmpl.data.x2,
                       tmpl.data.y1, tmpl.data.y2]

        # Also need to make sure it fills the whole space
        x1, x2, y1, y2 = vcs.utils.getworldcoordinates(gm, X, Y)
        if numpy.allclose(y1, y2):
            y1 -= .0001
            y2 += .0001
        if numpy.allclose(x1, x2):
            x1 -= .0001
            x2 += .0001
        l._worldcoordinate = [x1, x2, y1, y2]
        if gm.marker is not None:
            m = self._context.canvas.createmarker()
            m.type = gm.marker
            m.color = gm.markercolor
            if gm.markersize > 0:
                m.size = gm.markersize
            else:
                m.priority = 0
            m._x = l.x
            m._y = l.y
            m._viewport = l.viewport
            m._worldcoordinate = l.worldcoordinate

        if not (Y[:].min() > max(y1, y2) or Y[:].max() < min(y1, y2) or
                X[:].min() > max(x1, x2) or X[:].max() < min(x1, x2)):
            if l.priority > 0:
                self._context.canvas.plot(l, donotstoredisplay=True)
            if gm.marker is not None and m.priority > 0:
                self._context.canvas.plot(m, donotstoredisplay=True)

        ren2 = self._context.createRenderer()
        self._context.renWin.AddRenderer(ren2)
        tmpl.plot(self._context.canvas, data1, gm, bg=self._context.bg,
                  renderer=ren2, X=X, Y=Y)
        if hasattr(data1, "_yname"):
            del(data1._yname)
        del(vcs.elements["line"][l.name])
        if gm.marker is not None:
            del(vcs.elements["marker"][m.name])

        if tmpl.legend.priority > 0:
            legd = self._context.canvas.createline()
            legd.x = [tmpl.legend.x1, tmpl.legend.x2]
            legd.y = [tmpl.legend.y1, tmpl.legend.y1]  # [y1, y1] intentional.
            legd.color = l.color
            legd.width = l.width
            legd.type = l.type
            t = self._context.canvas.createtext(
                  To_source=tmpl.legend.textorientation,
                  Tt_source=tmpl.legend.texttable)
            t.x = tmpl.legend.x2
            t.y = tmpl.legend.y2
            t.string = data1.id
            self._context.canvas.plot(t, donotstoredisplay=True)
            sp = t.name.split(":::")
            del(vcs.elements["texttable"][sp[0]])
            del(vcs.elements["textorientation"][sp[1]])
            del(vcs.elements["textcombined"][t.name])
            self._context.canvas.plot(legd, donotstoredisplay=True)
            del(vcs.elements["line"][legd.name])
        return {}
