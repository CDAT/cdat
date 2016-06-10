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
            if isinstance(source, (str, unicode)):
                gm = vcsaddons.gms[self.g_type][source]
            else:
                gm = source
            self.line = gm.line
            self.linewidth = gm.linewidth
            self.linecolors = gm.linecolors
            self.fillareastyles = gm.fillareastyles
            self.fillareaindices = gm.fillareaindices
            self.fillareacolors = gm.fillareacolors
            self.bins = gm.bins

    def list(self):
        print '---------- Histogram (Ghg) member (attribute) listings ----------'  # pragma: no cover
        print 'Canvas Mode = ', self.x.mode  # pragma: no cover
        VCSaddon.list(self)  # pragma: no cover
        print 'fillareastyles = ', self.fillareastyles  # pragma: no cover
        print 'fillareaindices = ', self.fillareaindices  # pragma: no cover
        print 'fillareacolors = ', self.fillareacolors  # pragma: no cover
        print 'line = ', self.line  # pragma: no cover
        print 'linewidth = ', self.linewidth  # pragma: no cover
        print 'linecolors = ', self.linecolors  # pragma: no cover
        print 'bins = ', self.bins  # pragma: no cover

    def plot(self, data, template=None, bg=0, x=None, **kwargs):
        if x is None:
            x = self.x
        if template is None:
            template = self.template
        elif isinstance(template, str):
            template = x.gettemplate(template)
        elif not vcs.istemplate(template):  # pragma: no cover
            raise ValueError("Error did not know what to do with template: %s" % template)  # pragma: no cover
        try:
            data_name = data.title
        except AttributeError:
            try:
                data_name = data.long_name
            except AttributeError:
                try:
                    data_name = data.id + data.units
                except AttributeError:
                    try:
                        data_name = data.id
                    except AttributeError:
                        data_name = "array"

        # We'll just flatten the data... if they want to be more precise, should pass in more precise data
        if isinstance(data, cdms2.avariable.AbstractVariable):
            data = data.asma()
        data = data.flatten()

        # ok now we have a good x and a good data
        if not self.bins:
            self.bins = vcs.utils.mkscale(*vcs.minmax(data))

        # Sort the bins
        self.bins.sort()

        # Prune duplicates
        pruned_bins = []
        for bin in self.bins:
            if pruned_bins and numpy.allclose(bin, pruned_bins[-1]):
                continue
            pruned_bins.append(bin)
        self.bins = pruned_bins
        data_bins = numpy.digitize(data, self.bins) - 1
        binned = [data[data_bins==i] for i in range(len(self.bins))]
        means = []
        stds = []

        max_possible_deviance = 0

        for ind, databin in enumerate(binned):
            if len(databin) > 0:
                means.append(databin.mean())
                stds.append(databin.std())
            else:
                means.append(0)
                stds.append(0)
            if len(self.bins) > ind + 1:
                max_possible_deviance = max(means[ind] - self.bins[ind], self.bins[ind + 1] - means[ind], max_possible_deviance)
            else:
                max_possible_deviance = max(means[ind] - self.bins[ind], max_possible_deviance)
        color_values = [std / max_possible_deviance for std in stds]
        y_values = [len(databin) for databin in binned]
        nbars = len(self.bins) - 1

        # create the primitive
        fill = x.createfillarea()
        line = x.createline()
        fill.viewport = [
            template.data.x1, template.data.x2, template.data.y1, template.data.y2]
        line.viewport = [
            template.data.x1, template.data.x2, template.data.y1, template.data.y2]

        vcs_min_max = vcs.minmax(self.bins)
        if numpy.allclose(self.datawc_x1, 1e20):
            xmn = vcs_min_max[0]
        else:
            xmn = self.datawc_x1

        if numpy.allclose(self.datawc_x2, 1e20):
            xmx = vcs_min_max[1]
        else:
            xmx = self.datawc_x2

        if numpy.allclose(self.datawc_y2, 1e20):
            # Make the y scale be slightly larger than the largest bar
            ymx = max(y_values) * 1.25
        else:
            ymx = self.datawc_y2

        if numpy.allclose(self.datawc_y1, 1e20):
            ymn = 0
        else:
            ymn = self.datawc_y1

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
            while len(self.fillareastyles) < (len(levels) - 1):
                self.fillareastyles.append(self.fillareastyles[-1])
        else:
            self.fillareastyles = ["solid"] * (len(levels) - 1)

        if self.fillareacolors:
            while len(self.fillareacolors) < (len(levels) - 1):
                self.fillareacolors.append(self.fillareacolors[-1])
        else:
            for lev in levels[:-1]:
                self.fillareacolors.append(int((self.color_2 - self.color_1) * lev) + self.color_1)

        if self.fillareaindices:
            while len(self.fillareaindices) < (len(levels) - 1):
                self.fillareaindices.append(self.fillareaindices[-1])
        else:
            self.fillareaindices = [1] * (len(levels) - 1)

        if self.line:
            while len(self.line) < (len(levels) - 1):
                self.line.append(self.line[-1])
        else:
            self.line = ["solid"] * (len(levels) - 1)

        if self.linewidth:
            while len(self.linewidth) < (len(levels) - 1):
                self.linewidth.append(self.linewidth[-1])
        else:
            self.linewidth = [1] * (len(levels) - 1)

        if self.linecolors:
            while len(self.linecolors) < (len(levels) - 1):
                self.linecolors.append(self.linecolors[-1])
        else:
            self.linecolors = ["black"] * (len(levels) - 1)

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
                        assert False  # pragma: no cover
            else:
                assert False  # pragma: no cover
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

        x_axis = cdms2.createAxis(self.bins, id=data_name)
        y_axis = cdms2.createAxis(vcs.mkscale(ymn, ymx), id="bin_size")

        displays.append(x.plot(fill, bg=bg, render=False))
        arr = MV2.masked_array(y_values)
        arr.setAxis(0, x_axis)
        dsp = template.plot(x, arr, self, bg=bg, X=x_axis, Y=y_axis)
        for d in dsp:
            if d is not None:
                displays.append(d)
        legend_labels = {0: "No Variance",
                         .1: "",
                         .2: "",
                         .3: "",
                         .4: "",
                         .5: "",
                         .6: "",
                         .7: "",
                         .8: "",
                         .9: "",
                         1: "High Variance"}
        template.drawColorBar(self.fillareacolors, levels,
                              legend=legend_labels, x=x,
                              style=self.fillareastyles,
                              index=self.fillareaindices)

        displays.append(x.plot(line, bg=bg))

        x.worldcoordinate = fill.worldcoordinate

        self.restore()
        return displays
