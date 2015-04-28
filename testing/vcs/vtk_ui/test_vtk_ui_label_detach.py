"""
Test label goes away when detached
"""
import vcs.vtk_ui

from vtk_ui_test import vtk_ui_test


class test_vtk_ui_label_detach(vtk_ui_test):
    def do_test(self):
        self.win.SetSize(130, 40)

        l = vcs.vtk_ui.Label(self.inter, "This shouldn't be there.", fgcolor=(0, 0, 0))
        l.show()
        l.detach()

        for ren in vtkiter(self.win.GetRenderers()):
            if ren.HasViewProp(l.actor):
                print "Actor still in scene"
                return
        del l
        self.test_file = "test_vtk_ui_label_detach.png"


def vtkiter(vtkIterator):
    vtkIterator.InitTraversal()
    obj = vtkIterator.GetNextItem()
    while obj is not None:
        yield obj
        obj = vtkIterator.GetNextItem()


if __name__ == "__main__":
    test_vtk_ui_label_detach().test()
