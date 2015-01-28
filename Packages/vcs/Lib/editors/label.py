import point
import vcs
import inspect
import text

class LabelEditor(point.PointEditor):
    def __init__(self, interactor, label, dp, configurator):
        self.label = label
        self.display = dp
        super(LabelEditor, self).__init__(interactor, label, configurator)

    def get_text(self):
        return get_label_text(self.label, self.display.array[0])

    def place(self):
        pass

    def in_bounds(self, x, y):
        t = self.get_text()
        swidth, sheight = self.interactor.GetRenderWindow().GetSize()
        return inside_label(self.label, t, x, y, swidth, sheight)


def get_label_text(label, array):
    s = label.member

    smn, smx = vcs.minmax(array)

    if s == 'min':
        t = 'Min %g' % (smn)
    elif s == 'max':
        t = 'Max %g' % smx
    elif s == 'mean':
        if not inspect.ismethod(getattr(array,'mean')):
            t = float(getattr(array,s))
        else:
            t = array.mean()

        t = "Mean %f" % t
    else:
        # General slab attributes
        try:
            t = getattr(array, s)
        except AttributeError:
            t = ''
    return t

def inside_label(label, t, x, y, screen_width, screen_height):
    tt = label.texttable
    to = label.textorientation

    tc = vcs.createtextcombined(Tt_source=tt, To_source=to)
    tc.string = [t]
    tc.x = [label.x]
    tc.y = [label.y]

    return text.inside_text(tc, x, y, screen_width, screen_height) is not None