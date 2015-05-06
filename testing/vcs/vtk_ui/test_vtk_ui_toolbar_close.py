"""
Test toolbar placement and basic appearance
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_toolbar_close(vtk_ui_test):
	def do_test(self):
		self.win.SetSize(200, 100)

		toolbar = vcs.vtk_ui.Toolbar(self.inter, "Test Bar")
		toolbar.add_button(["Test Button"])
		toolbar.add_button(["Other Test"])
		toolbar.label.__advance__(1)
		toolbar.label.__advance__(0)
		assert toolbar.open == False
		toolbar.show()

		self.test_file = "test_vtk_ui_toolbar_close.png"

if __name__ == "__main__":
	test_vtk_ui_toolbar_close().test()
