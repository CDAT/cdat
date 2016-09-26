import os, sys, time, vtk, vcs.vtk_ui


def init():
    win = vtk.vtkRenderWindow()
    win.SetNumberOfLayers(3)
    win.SetSize(100, 250)
    win.SetMultiSamples(0)

    inter = vtk.vtkRenderWindowInteractor()
    inter.SetRenderWindow(win)

    ren = vtk.vtkRenderer()
    ren.SetBackground((1, 1, 1))
    win.AddRenderer(ren)
    ren.SetLayer(0)

    win.SetOffScreenRendering(1)

    manager = vcs.vtk_ui.manager.get_manager(inter)

    win.AddRenderer(manager.renderer)
    win.AddRenderer(manager.actor_renderer)
    manager.elevate()

    return win, ren


def generate_png(win, fnm):
    win.Render()
    out_filter = vtk.vtkWindowToImageFilter()
    out_filter.SetInput(win)

    png_writer = vtk.vtkPNGWriter()
    png_writer.SetFileName(fnm)
    png_writer.SetInputConnection(out_filter.GetOutputPort())
    png_writer.Write()


class vtk_ui_test(object):
    def __init__(self):
        self.win, self.renderer = init()
        self.inter = self.win.GetInteractor()
        self.test_file = None
        self.passed = 1
        self.args = sys.argv[1:]

    def hover(self, x, y, duration):
        self.win.Render()
        self.inter.SetEventInformation(x, y)
        self.inter.MouseMoveEvent()
        time.sleep(duration)
        self.inter.InvokeEvent("TimerEvent")
        self.win.Render()

    def mouse_down(self, x, y):
        self.inter.SetEventInformation(x, y)
        self.inter.LeftButtonPressEvent()

    def mouse_move(self, x, y):
        self.inter.SetEventInformation(x, y)
        self.inter.MouseMoveEvent()

    def mouse_up(self, x, y):
        self.inter.SetEventInformation(x, y)
        self.inter.LeftButtonReleaseEvent()

    def click_event(self, x, y):
        self.win.Render()
        self.mouse_move(x, y)
        self.mouse_down(x, y)
        self.mouse_up(x, y)

    def set_key(self, key, shift=False, alt=False, control=False):
        if len(key) > 1:
            # key is a symbol
            self.inter.SetEventInformation(0, 0, 1 if control else 0, 1 if shift else 0, '', 1, key)
        else:
            self.inter.SetEventInformation(0, 0, 1 if control else 0, 1 if shift else 0, key, 1, None)
        self.inter.SetAltKey(alt)

    def key_down(self):
        self.inter.InvokeEvent("KeyPressEvent")

    def key_up(self):
        self.inter.InvokeEvent("KeyReleaseEvent")

    def key_event(self, key, shift=False, alt=False, control=False):
        self.set_key(key, shift, alt, control)
        self.key_down()
        self.set_key(key, shift, alt, control)
        self.key_up()

    def do_test(self):
        raise NotImplementedError("Implement do_test to execute a test.")

    def check_image(self, compare_against):
        """
        Checks the current render window's output against the image specified in the argument,
        returns the result of regression.check_result_image
        """
        generate_png(self.win, self.test_file)
        pth = os.path.join(os.path.dirname(__file__), "../..")
        sys.path.append(pth)
        import vcs.testing.regression as regression
        return regression.check_result_image(self.test_file, compare_against)

    def test(self):
        self.do_test()

        if self.test_file:
            if self.win.GetOffScreenRendering() == 0:
                # There was a race condition where resizing might take longer
                # than rendering, so the image was coming out weird. This
                # should fix that.
                from time import sleep
                sleep(2)
            if self.args:
                src = self.args[0]
                self.passed = self.check_image(src)
            else:
                generate_png(self.win, self.test_file)

        self.win.Finalize()
        self.inter.TerminateApp()
	print sys.argv[0], "passed" if self.passed == 0 else "failed"
	sys.exit(self.passed)
