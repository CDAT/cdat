"""
Test toolbar placement and basic appearance
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_toolbar_label(vtk_ui_test):
	def do_test(self):
		self.win.SetSize(200, 100)

		toolbar = vcs.vtk_ui.Toolbar(self.inter, "Test Bar")
		# Should default to closed
		toolbar.add_button(["Test Button"])
		toolbar.add_button(["Other Test"])
		toolbar.show()

		self.test_file = "test_vtk_ui_toolbar_label.png"

if __name__ == "__main__":
	test_vtk_ui_toolbar_label().test()
