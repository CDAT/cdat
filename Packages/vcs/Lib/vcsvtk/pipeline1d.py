from .pipeline import Pipeline

import numpy
import vcs


def power10(x):
    return numpy.ma.power(10, x)


def degsin(x):
    return numpy.ma.sin(x / 180. * numpy.pi)


def degarcsin(x):
    return numpy.ma.arcsin(x) * 180. / numpy.pi


def smooth(x, beta, window_len=11):
    """ kaiser window smoothing """
    # extending the data at beginning and at the end
    # to apply the window at the borders
    s = numpy.r_[x[window_len - 1:0:-1], x, x[-1:-window_len:-1]]
    w = numpy.kaiser(window_len, beta)
    y = numpy.convolve(w / w.sum(), s, mode='valid')
    return y[(window_len / 2):-(window_len / 2)]


class Pipeline1D(Pipeline):

    """Implementation of the Pipeline interface for 1D VCS plots."""

    def __init__(self, gm, context_):
        super(Pipeline1D, self).__init__(gm, context_)

    def convertAxis(self, axis, data, method, revert_method):
        new = method(data[:])
        # Now that the easy part is done, we need to convert the gm
        # appropriately
        gm = vcs.create1d(source=self._gm)
        sc = vcs.mkscale(new[0], new[-1])
        lbls = vcs.mklabels(sc)
        new_lbls = {}
        for k in lbls.keys():
            v = revert_method(eval(lbls[k]))
            new_lbls[k] = v

        if axis == "x":
            if gm.datawc_x1 != "*" and not numpy.allclose(gm.datawc_x1, 1.e20):
                gm.datawc_x1 = method(gm.datawc_x1)
            else:
                autoscale = True
            if gm.datawc_x2 != "*" and not numpy.allclose(gm.datawc_x2, 1.e20):
                gm.datawc_x2 = method(gm.datawc_x2)
            else:
                autoscale = True
            if autoscale:
                gm.datawc_x1 = sc[0]
                gm.datawc_x2 = sc[-1]
        else:
            if gm.datawc_y1 != "*" and not numpy.allclose(gm.datawc_y1, 1.e20):
                gm.datawc_y1 = method(gm.datawc_y1)
            else:
                autoscale = True
            if gm.datawc_y2 != "*" and not numpy.allclose(gm.datawc_y2, 1.e20):
                gm.datawc_y2 = method(gm.datawc_y2)
            else:
                autoscale = True
            if autoscale:
                gm.datawc_y1 = sc[0]
                gm.datawc_y2 = sc[-1]

        x1 = gm.datawc_x1
        x2 = gm.datawc_x2
        y1 = gm.datawc_y1
        y2 = gm.datawc_y2

        vcs.setTicksandLabels(gm, None, x1, x2, y1, y2, "x", "y")

        for att in ["ticlabels", "mtics"]:
            for num in ["1", "2"]:
                val = getattr(gm, axis + att + num)
                print axis + att + num, val
                new_dict = {}
                if isinstance(val, dict):
                    for v in val.keys():
                        new_dict[method(v)] = val[v]
                else:  # auto
                    new_dict = new_lbls
                setattr(gm, axis + att + num, new_dict)

        self._gm = gm
        return new

    def plot(self, data1, data2, tmpl, grid, transform):
        """Overrides baseclass implementation."""
        Y = self._context().trimData1D(data1)
        if data2 is None:
            X = Y.getAxis(0)
        else:
            X = Y
            data1._yname = data2.id
            Y = self._context().trimData1D(data2)

        if self._gm.flip:
            tmp = Y
            Y = X
            X = tmp

        if self._gm.smooth is not None:
            Y = smooth(Y, self._gm.smooth)

        # Here we try to apply the axis convert options
        for ax, conv, vals in [['x', self._gm.xaxisconvert, X], ['y', self._gm.yaxisconvert, Y]]:
            if conv == "log10":
                new = self.convertAxis(ax, vals, numpy.ma.log10, power10)
            elif conv == "ln":
                new = self.convertAxis(ax, vals, numpy.ma.log, numpy.ma.exp)
            elif conv == "exp":
                new = self.convertAxis(ax, vals, numpy.ma.exp, numpy.ma.log)
            elif conv == "area_wt":
                new = self.convertAxis(ax, vals, degsin, degarcsin)
            else:
                new = vals

            if vals is X:
                X = new
            else:
                Y = new

        l = self._context().canvas.createline()
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
        l.color = [self._gm.linecolor, ]
        if self._gm.linewidth > 0:
            l.width = self._gm.linewidth
        else:
            l.priority = 0
        l.type = self._gm.line
        l._viewport = [tmpl.data.x1, tmpl.data.x2,
                       tmpl.data.y1, tmpl.data.y2]

        # Also need to make sure it fills the whole space
        x1, x2, y1, y2 = vcs.utils.getworldcoordinates(self._gm, X, Y)
        if numpy.allclose(y1, y2):
            y1 -= .0001
            y2 += .0001
        if numpy.allclose(x1, x2):
            x1 -= .0001
            x2 += .0001
        l._worldcoordinate = [x1, x2, y1, y2]
        if self._gm.marker is not None:
            m = self._context().canvas.createmarker()
            m.type = self._gm.marker
            m.color = [self._gm.markercolor, ]
            if self._gm.markersize > 0:
                m.size = self._gm.markersize
            else:
                m.priority = 0
            m._x = l.x
            m._y = l.y
            m._viewport = l.viewport
            m._worldcoordinate = l.worldcoordinate

        if not (Y[:].min() > max(y1, y2) or Y[:].max() < min(y1, y2) or
                X[:].min() > max(x1, x2) or X[:].max() < min(x1, x2)):
            if l.priority > 0:
                self._context().canvas.plot(l, donotstoredisplay=True)
            if self._gm.marker is not None and m.priority > 0:
                self._context().canvas.plot(m, donotstoredisplay=True)

        ren2 = self._context().createRenderer()
        self._context().renWin.AddRenderer(ren2)
        tmpl.plot(self._context().canvas, data1, self._gm, bg=self._context().bg,
                  renderer=ren2, X=X, Y=Y)
        if hasattr(data1, "_yname"):
            del(data1._yname)
        del(vcs.elements["line"][l.name])
        if self._gm.marker is not None:
            del(vcs.elements["marker"][m.name])

        if tmpl.legend.priority > 0:
            legd = self._context().canvas.createline()
            legd.x = [tmpl.legend.x1, tmpl.legend.x2]
            legd.y = [tmpl.legend.y1, tmpl.legend.y1]  # [y1, y1] intentional.
            legd.color = l.color
            legd.width = l.width
            legd.type = l.type
            t = self._context().canvas.createtext(
                To_source=tmpl.legend.textorientation,
                Tt_source=tmpl.legend.texttable)
            t.x = tmpl.legend.x2
            t.y = tmpl.legend.y2
            t.string = data1.id
            self._context().canvas.plot(t, donotstoredisplay=True)
            sp = t.name.split(":::")
            del(vcs.elements["texttable"][sp[0]])
            del(vcs.elements["textorientation"][sp[1]])
            del(vcs.elements["textcombined"][t.name])
            self._context().canvas.plot(legd, donotstoredisplay=True)
            del(vcs.elements["line"][legd.name])
        return {}
