"""
Test toolbar placement inside toolbar
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_toolbar_in_toolbar_open(vtk_ui_test):
	def do_test(self):
		self.win.SetSize(200, 250)

		toolbar = vcs.vtk_ui.Toolbar(self.inter, "Test Bar")
		tb = toolbar.add_toolbar("Sub-bar")
		tb.add_button(["first"])
		tb.add_button(["second"])
		toolbar.add_button(["Test Button"])
		toolbar.add_button(["Other Test"])
		toolbar.show()

		# Open both toolbars
		toolbar.label.__advance__(1)
		tb.label.__advance__(1)

		self.test_file = "test_vtk_ui_toolbar_in_toolbar_open.png"

if __name__ == "__main__":
	test_vtk_ui_toolbar_in_toolbar_open().test()
