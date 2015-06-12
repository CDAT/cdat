"""
Test toggle_button get text
"""
import vcs.vtk_ui


from vtk_ui_test import vtk_ui_test

class test_vtk_ui_toggle_button_get_text(vtk_ui_test):
	def do_test(self):
		b = vcs.vtk_ui.ToggleButton(self.inter, "Simple label")
		b.set_state(1)
		assert b.get_text() == "Simple label"
		b.set_state(0)
		assert b.get_text() == "Simple label"
		self.passed = 0