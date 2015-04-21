from text import Label, text_dimensions
from datetime import datetime, timedelta
from line import Line
import vtk

class Textbox(Label):
    def __init__(self, interactor, string, on_editing_end=None, highlight_color=None, highlight_opacity=None, **kwargs):

        super(Textbox, self).__init__(interactor, string, **kwargs)
        self.drag_interval = timedelta(0, .1)
        self.editing = False
        self.edit_indicator = None
        self.column = 0
        self.row = 0
        self.text = string
        self.on_editing_end = on_editing_end
        self.highlight_opacity = highlight_opacity
        self.highlight_color = highlight_color

        self.cursor = Line((0, 0), (1, 1), renderer=self.widget.GetCurrentRenderer(), width=2)
        # Blink the cursor if we're editing.
        self.blink_timer = self.interactor.CreateRepeatingTimer(600)
        self.blink_observer = self.interactor.AddObserver("TimerEvent", self.blink_cursor)
        # All timer events trigger all listeners, so we will only update when the time elapsed is the expected period.
        self.last_blink = datetime.now()
        # Use the third argument (priority) to intercept key events before anything else does
        self.keyboard_observer = self.interactor.AddObserver("KeyPressEvent", self.typed, 1.0)

    def blink_cursor(self, obj, event):
        if datetime.now() - self.last_blink < timedelta(0, 0, 0, 400):
            return

        self.last_blink = datetime.now()
        if self.editing:
            if self.cursor.showing:
                self.cursor.hide()
            else:
                self.cursor.show()
            self.manager.queue_render()

    def get_char(self):
        keycode = self.interactor.GetKeyCode()
        if len(keycode) == 0 or ord(keycode) > 126 or ord(keycode) < 33:
            keycode = self.interactor.GetKeySym()
        return keycode

    def add_character(self, character):
        rows = self.text.split("\n")

        row = rows[self.row]

        if self.column >= len(row):
            row += character
            rows[self.row] = row
            if character == "\n":
                self.column = 0
                self.row += 1
            else:
                self.column = len(row)
        else:
            row = row[:self.column] + character + row[self.column:]
            rows[self.row] = row
            if character != "\n":
                self.column += 1
            else:
                self.row += 1
                self.column = 0

        self.text = "\n".join(rows)

    def delete_character(self):
        rows = self.text.split("\n")

        if self.column == 0 and self.row > 0:
            row = rows[self.row - 1]
            self.column = len(row)
            row += rows[self.row]
            rows[self.row - 1] = row
            del rows[self.row]
            self.row -= 1
        elif self.column == 0:
            return
        else:
            row = rows[self.row]
            if self.column < len(row):
                rows[self.row] = row[:self.column - 1] + row[self.column:]

                self.column -= 1
            else:
                rows[self.row] = row[:-1]
                self.column = len(row)

        self.text = "\n".join(rows)

    def typed(self, obj, event):
        if self.editing:
            c = self.get_char()

            if c in "qQeEjJ3cCtTaAsS":
                # Prevents VTK from doing stuff with these keys
                self.interactor.SetKeyCode("`")

            if c == "Backspace":
                self.delete_character()
            elif c == "Return":
                self.add_character("\n")
            elif len(c) == 1:
                self.add_character(c)
            elif c == "space":
                self.add_character(" ")
            elif c == "Escape":
                self.stop_editing()
                return
            elif c[:5] == "Shift":
                pass
            elif c == "Tab":
                self.add_character("\t")

            if c in ("Left", "Right", "Up", "Down"):
                # Reset blink counter
                self.last_blink -= timedelta(0,0, 400)

                rows = self.text.split("\n")

                if c == "Left":
                    if self.column == 0 and self.row > 0:
                        self.row -= 1
                        self.column = len(rows[self.row])
                    else:
                        self.column = max(0, self.column - 1)
                elif c == "Right":
                    if self.column == len(rows[self.row]) and self.row < len(rows) - 1:
                        self.column = 0
                        self.row += 1
                    else:
                        self.column = min(self.column + 1, len(rows[self.row]))
                elif c == "Up":
                    self.row = max(0, self.row - 1)
                    self.column = min(len(rows[self.row]), self.column)
                elif c == "Down":
                    if self.row == len(rows) - 1:
                        self.column = len(rows[self.row])
                    else:
                        self.row += 1

            if self.text[-2:] == "QQ":
                self.stop_editing()
                self.text = self.text[:-2]

            self.update()

    def set_text(self, text):
        super(Textbox, self).set_text(text)
        self.text = text

    def update(self):
        if self.repr.GetText() != self.text:
            self.widget.On()
            self.set_text(self.text)
            self.widget.On()
        self.place_cursor()

    def place_cursor(self):
        # Find current position of the text actor
        x, y = self.repr.GetPosition()

        # Use to adjust all window-space numbers
        w, h = self.interactor.GetRenderWindow().GetSize()

        # Get the maximum line height for the current font
        prop = vtk.vtkTextProperty()
        prop.ShallowCopy(self.actor.GetTextProperty())
        # Store for later
        angle = prop.GetOrientation()
        # Reset so we get accurate dimensions
        prop.SetOrientation(0)

        test_line = "Hhqjy"
        _, line_height = text_dimensions(test_line, prop)

        # Find the y of the top of the current row
        y -= line_height * self.row

        # Find the x of the current column
        rows = self.text.split("\n")

        # Grab the total width of the text
        max_width, _ = text_dimensions(self.text, prop)

        # Now grab the width of the current row
        row_width, _ = text_dimensions(rows[self.row], prop)

        # Here's the amount we need to adjust by
        width_difference = max_width - row_width

        # Adjust for alignment
        align = prop.GetJustificationAsString()
        if align == "center":
            # If we're center aligned, we need to adjust by half
            x += width_difference / 2.
        elif align == "right":
            # If we're right aligned, we need to adjust the full difference
            x += width_difference

        # Now we need to get the offset for the character clicked
        up_to_col_width, _ = text_dimensions(rows[self.row][:self.column], prop)
        x += up_to_col_width

        # Rotate both points to the orientation as the text
        def rotate(point, angle):
            import math
            x, y = point
            # Rotate about the origin
            theta = math.radians(angle)
            xrot = x * math.cos(theta) - y * math.sin(theta)
            yrot = x * math.sin(theta) + y * math.cos(theta)
            return int(xrot), int(yrot)

        x1, y1 = rotate((up_to_col_width, 1 * line_height / 2.), angle)
        x2, y2 = rotate((up_to_col_width, -1 * line_height / 2.), angle)
        x1 += self.left
        x2 += self.left
        y1 += y
        y2 += y

        self.cursor.point_1 = (x1, y1)
        self.cursor.point_2 = (x2, y2)

    def row_col_at_point(self, x, y):
        rows = self.text.split("\n")
        prop = self.actor.GetTextProperty()

        text_width, text_height = text_dimensions(self.text, prop)

        # Viewport coordinates of widget
        sw, sh = self.interactor.GetRenderWindow().GetSize()

        x0, y0 = self.left, sh - self.top

        # Adjust click coords to widget's bounds
        x = abs(x - x0)
        y = text_height - abs(y - y0)


        # Calculate the bounds of each row
        row_bounds = []
        max_width = 0
        # We're iterating in reverse because y goes from 0 at the bottom to 1 at the top
        row_at_point = None
        for row in rows[::-1]:
            if row == '':
                row = ' '

            w, h = text_dimensions(row, prop)
            row_bounds.append((w, h))

            if w > max_width:
                # Use for x offset calculations
                max_width = w

            if h < y and row_at_point is None:
                y = y - h
            else:
                row_at_point = rows.index(row)

        if row_at_point is None:
            row_at_point = len(rows) - 1

        # List was assembled backwards
        row_bounds.reverse()

        # Now let's find the column clicked...
        row = row_bounds[row_at_point]
        text = rows[row_at_point]

        # If max_width == row[0], then offset is 0 and all of the calcs below still work
        x -= (max_width - row[0]) / 2.0

        row_left = 0
        row_right = row[0]

        if x < row_left:
            # Clicked to the left of the row
            return row_at_point, 0

        if x > row_right:
            # Clicked to the right of the row
            return row_at_point, len(rows[row_at_point])

        if row == '':
            # Clicked on the blank row (inserted a space when calculating width earlier, for height considerations)
            return row_at_point, 1

        # OK, no easy answer; have to calc the width of each letter in the row till we find the column
        # Start at left or right depending on which side the click is closer to
        if x - row_left > row_right - x:
            # Start from right
            for reverse_index, c in enumerate(text[::-1]):
                w,_ = text_dimensions(c, prop)
                if row_right - w < x:
                    return row_at_point, len(text) - reverse_index
                # "New" right side is one character back
                row_right -= w
        else:
            # Start from left
            for index, c in enumerate(text):
                w, _ = text_dimensions(c, prop)
                if row_left + w > x:
                    return row_at_point, index
                # "New" left side is one character forward
                row_left += w
        # Return the very end of the box if we can't figure it out.
        return len(rows) - 1, len(rows[len(rows) - 1])

    def show_highlight(self):
        prop = self.actor.GetTextProperty()
        prop.SetBackgroundColor(self.highlight_color)
        prop.SetBackgroundOpacity(self.highlight_opacity)

    def hide_highlight(self):
        prop = self.actor.GetTextProperty()
        prop.SetBackgroundOpacity(0)

    def start_editing(self, point=None):
        if point is not None:
            self.row, self.column = self.row_col_at_point(*point)
        else:
            rows = self.text.split("\n")
            self.row = len(rows) - 1
            self.column = len(rows[-1])

        self.place_cursor()
        self.editing = True

    def stop_editing(self):
        self.editing = False
        self.cursor.hide()

        self.on_editing_end(self)
        self.manager.queue_render()

    def place(self):
        super(Textbox, self).place()
        if self.editing:
            self.place_cursor()

    def detach(self):
        if self.cursor is not None:
            self.cursor.detach()
            self.cursor = None
        self.interactor.DestroyTimer(self.blink_timer)
        self.interactor.RemoveObserver(self.blink_observer)
        self.interactor.RemoveObserver(self.keyboard_observer)
        super(Textbox, self).detach()
