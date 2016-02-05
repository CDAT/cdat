from core import VCSaddon
import cdms2
import MV2
import numpy
import vcs
import vcsaddons


class Ghg(VCSaddon):

    def __init__(self, name=None, source='default', x=None, template=None):
        self.g_name = 'Ghg'
        self.g_type = 'histogram'
        VCSaddon.__init__(self, name, source, x, template)
        if source == 'default':
            self.line = []
            self.linewidth = []
            self.linecolors = []
            self.fillareastyles = []
            self.fillareaindices = []
            self.fillareacolors = []
            self.bins = []
        else:
            gm = vcsaddons.gms[self.g_name][source]
            self.line = gm.line
            self.linewidth = gm.linewidth
            self.linecolors = gm.linecolors
            self.fillareastyles = gm.fillareastyles
            self.fillareaindices = gm.fillareaindices
            self.fillareacolors = gm.fillareacolors
            self.bins = gm.bins

    def list(self):
        print '---------- Histogram (Ghg) member (attribute) listings ----------'
        print 'Canvas Mode = ', self.x.mode
        VCSaddon.list(self)
        print 'fillareastyles = ', self.fillareastyles
        print 'fillareaindices = ', self.fillareaindices
        print 'fillareacolors = ', self.fillareacolors
        print 'line = ', self.line
        print 'linewidth = ', self.linewidth
        print 'linecolors = ', self.linecolors
        print 'bins = ', self.bins

    def plot(self, data, template=None, bg=0, x=None):
        if x is None:
            x = self.x
        if template is None:
            template = self.template
        elif isinstance(template, str):
            template = x.gettemplate(template)
        elif not vcs.istemplate(template):
            raise ValueError("Error did not know what to do with template: %s" % template)

        # We'll just flatten the data... if they want to be more precise, should pass in more precise data
        data = data.flatten().asma()

        # ok now we have a good x and a good data
        if not self.bins:
            self.bins = vcs.utils.mkscale(*vcs.minmax(data))

        data_bins = numpy.digitize(data, self.bins) - 1
        binned = [data[data_bins==i] for i in range(len(self.bins))]

        means = []
        stds = []

        max_possible_deviance = 0

        for ind, databin in enumerate(binned):
            means.append(databin.mean())
            stds.append(databin.std())
            if len(self.bins) > ind + 1:
                max_possible_deviance = max(means[ind] - self.bins[ind], self.bins[ind + 1] - means[ind], max_possible_deviance)
            else:
                max_possible_deviance = max(means[ind] - self.bins[ind], max_possible_deviance)

        color_values = [std / max_possible_deviance for std in stds]
        y_values, _ = numpy.histogram(data, self.bins)
        nbars = len(self.bins) - 1

        # create the primitive
        fill = x.createfillarea()
        line = x.createline()
        fill.viewport = [
            template.data.x1, template.data.x2, template.data.y1, template.data.y2]
        line.viewport = [
            template.data.x1, template.data.x2, template.data.y1, template.data.y2]

        xmn, xmx = vcs.minmax(self.bins)
        ymn, ymx = 0, len(data)

        xmn, xmx, ymn, ymx = self.prep_plot(xmn, xmx, ymn, ymx)

        fill.worldcoordinate = [xmn, xmx, ymn, ymx]
        line.worldcoordinate = [xmn, xmx, ymn, ymx]

        styles = []
        cols = []
        indices = []
        lt = []
        lw = []
        lc = []
        xs = []
        ys = []

        levels = [.1 * i for i in range(11)]

        # Extend fillarea and line attrs to levels
        if self.fillareastyles:
            while len(self.fillareastyles) < len(levels):
                self.fillareastyles.append(self.fillareastyles[-1])
        else:
            self.fillareastyles = ["solid"] * len(levels)

        if self.fillareacolors:
            while len(self.fillareacolors) < len(levels):
                self.fillareacolors.append(self.fillareacolors[-1])
        else:
            for lev in levels:
                self.fillareacolors.append(int((self.color_2 - self.color_1) * lev) + self.color_1)

        if self.fillareaindices:
            while len(self.fillareaindices) < len(levels):
                self.fillareaindices.append(self.fillareaindices[-1])
        else:
            self.fillareaindices = [1] * len(levels)

        if self.line:
            while len(self.line) < len(levels):
                self.line.append(self.line[-1])
        else:
            self.line = ["solid"] * len(levels)

        if self.linewidth:
            while len(self.linewidth) < len(levels):
                self.linewidth.append(self.linewidth[-1])
        else:
            self.linewidth = [1] * len(levels)

        if self.linecolors:
            while len(self.linecolors) < len(levels):
                self.linecolors.append(self.linecolors[-1])
        else:
            self.linecolors = ["black"] * len(levels)

        for i in range(nbars):
            # Calculate level for bar
            value = color_values[i]
            for lev_ind in range(len(levels)):
                if levels[lev_ind] > value:
                    if lev_ind > 0:
                        lev_ind -= 1
                        break
                    else:
                        # Shouldn't ever get here since level 0 is 0
                        assert False

            styles.append(self.fillareastyles[lev_ind])
            cols.append(self.fillareacolors[lev_ind])
            indices.append(self.fillareaindices[lev_ind])
            lt.append(self.line[lev_ind])
            lw.append(self.linewidth[lev_ind])
            lc.append(self.linecolors[lev_ind])

            xs.append([self.bins[i], self.bins[i], self.bins[i + 1], self.bins[i + 1]])
            ys.append([0, y_values[i], y_values[i], 0])

        fill.style = styles
        fill.x = xs
        fill.y = ys
        fill.style
        fill.index = indices
        fill.color = cols
        fill.colormap = self.colormap
        line.x = xs
        line.y = ys
        line.type = lt
        line.width = lw
        line.color = lc
        displays = []
        displays.append(x.plot(fill, bg=bg))
        displays.append(x.plot(line, bg=bg))

        x.worldcoordinate = fill.worldcoordinate

        x_axis = cdms2.createAxis(self.bins, id="x")
        y_axis = cdms2.createAxis(vcs.mkscale(0, len(data)), id="y")

        dsp = template.plot(x, MV2.masked_array(data), self, bg=bg, X=x_axis, Y=y_axis)
        for d in dsp:
            displays.append(d)

        self.restore()
        # Ugh, hack
        x.backend.renWin.Render()
        return displays
