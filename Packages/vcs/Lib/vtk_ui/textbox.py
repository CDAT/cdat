from text import Label, text_dimensions
from datetime import datetime, timedelta
from line import Line
import vtk


def rotate(point, angle):
    import math
    x, y = point
    # Rotate about the origin
    theta = math.radians(angle)
    xrot = x * math.cos(theta) - y * math.sin(theta)
    yrot = x * math.sin(theta) + y * math.cos(theta)
    return int(xrot), int(yrot)


class Textbox(Label):

    def __init__(self, interactor, string, on_editing_end=None,
                 highlight_color=None, highlight_opacity=None, **kwargs):

        super(Textbox, self).__init__(interactor, string, **kwargs)
        self.drag_interval = timedelta(0, .1)
        self.editing = False
        self.blank = False
        self.edit_indicator = None
        self.column = 0
        self.row = 0
        self.text = string
        self.on_editing_end = on_editing_end
        self.highlight_opacity = highlight_opacity
        self.highlight_color = highlight_color
        self.cursor = Line(
            (0, 0), (1, 1), renderer=self.widget.GetCurrentRenderer(), width=2)
        # Blink the cursor if we're editing.
        self.blink_timer = self.interactor.CreateRepeatingTimer(600)
        self.blink_observer = self.interactor.AddObserver(
            "TimerEvent",
            self.blink_cursor)
        # All timer events trigger all listeners, so we will only update when
        # the time elapsed is the expected period.
        self.last_blink = datetime.now()
        # Use the third argument (priority) to intercept key events before
        # anything else does
        self.keyboard_observer = self.interactor.AddObserver(
            "KeyPressEvent",
            self.typed,
            1.0)

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

    def show_cursor(self):
        self.last_blink = datetime.now()
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
            if self.blank:
                self.blank = False
                row = character
            else:
                row += character
            rows[self.row] = row
            if character == "\n":
                self.column = 0
                self.row += 1
            else:
                self.column = len(row)
        else:
            if self.blank:
                self.blank = False
                row = character
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
        if self.text == "":
            self.blank = True
            self.text = " "

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
                rows = self.text.split("\n")

                if c == "Left":
                    if self.column == 0 and self.row > 0:
                        self.row -= 1
                        self.column = len(rows[self.row])
                    else:
                        self.column = max(0, self.column - 1)
                elif c == "Right":
                    if self.column == len(
                            rows[self.row]) and self.row < len(rows) - 1:
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
                self.show_cursor()

            if self.text[-2:] == "QQ":
                self.stop_editing()
                self.text = self.text[:-2]

            self.update()

    def update(self):
        if self.repr.GetText() != self.text:
            self.set_text(self.text)
        self.place_cursor()

    def place_cursor(self):
        # Find current position of the text actor
        x, y = self.left, self.top

        # Use to adjust all window-space numbers
        w, h = self.interactor.GetRenderWindow().GetSize()

        # Translate distance from top to distance from bottom
        y = h - y

        # Prep a text property for getting measurements
        prop = vtk.vtkTextProperty()
        prop.ShallowCopy(self.actor.GetTextProperty())

        # Store for rotating the cursor's coords
        angle = prop.GetOrientation()

        # Reset so we get accurate dimensions
        prop.SetOrientation(0)

        rows = self.text.split("\n")

        dpi = self.interactor.GetRenderWindow().GetDPI()
        width, height = text_dimensions(self.text, prop, dpi)
        line_height = float(height) / len(rows)

        column_adjustment, _ = text_dimensions(rows[self.row][:self.column],
                                               prop, dpi)

        x += column_adjustment

        row_width, _ = text_dimensions(rows[self.row], prop, dpi)

        # Adjust for blank space caused by justifications
        halign = prop.GetJustificationAsString()
        if halign == "Centered":
            x += (width - row_width) / 2.
        elif halign == "Right":
            x += (width - row_width) + 1  # Adjust for some margin issues
        elif halign == "Left":
            x -= 3  # Adjust for some margin issues

        # Manual adjustments for justification artefacts
        valign = prop.GetVerticalJustificationAsString()
        if valign == "Top":
            y += 2
        elif valign == "Centered":
            pass
        elif valign == "Bottom":
            y -= 2

        # Get to the current row
        y -= line_height * self.row

        # Rotate both points to the orientation as the text
        y1 = y
        y2 = y - line_height

        x1 = x
        x2 = x

        x1, x2 = x1 - self.x, x2 - self.x
        y1, y2 = y1 - self.y, y2 - self.y

        x1, y1 = rotate((x1, y1), angle)
        x2, y2 = rotate((x2, y2), angle)

        x1 += self.x
        x2 += self.x
        y1 += self.y
        y2 += self.y

        self.cursor.point_1 = (x1, y1)
        self.cursor.point_2 = (x2, y2)

    def row_col_at_point(self, x, y):
        rows = self.text.split("\n")
        prop = self.actor.GetTextProperty()

        dpi = self.interactor.GetRenderWindow().GetDPI()
        text_width, text_height = text_dimensions(self.text, prop, dpi)

        # Viewport coordinates of widget
        sw, sh = self.interactor.GetRenderWindow().GetSize()
        # Normalize to window space
        if x < 1 and y < 1:
            x = sw * x
            y = sh * y

        # Rotate the click out of box space
        x -= self.x
        y -= self.y

        x, y = rotate((x, y), -1 * prop.GetOrientation())

        x += self.x
        y += self.y

        x0, y0 = self.left, sh - self.top

        # Adjust click coords to widget's bounds
        x = abs(x - x0)
        y = text_height - abs(y - y0)

        # Calculate the bounds of each row
        row_bounds = []
        max_width = 0
        # We're iterating in reverse because y goes from 0 at the bottom to 1
        # at the top
        row_at_point = None

        for row in rows[::-1]:
            if row == '':
                dim_row = ' '
            else:
                dim_row = row

            w, h = text_dimensions(dim_row, prop, dpi)
            row_bounds.append((w, h))

            if w > max_width:
                # Use for x offset calculations
                max_width = w

            if h < y:
                y = y - h
            else:
                if row_at_point is None:
                    row_at_point = rows.index(row)

        if row_at_point is None:
            row_at_point = len(rows) - 1

        # List was assembled backwards
        row_bounds.reverse()

        # Now let's find the column clicked...
        row = row_bounds[row_at_point]
        text = rows[row_at_point]

        # If max_width == row[0], then offset is 0 and all of the calcs below
        # still work
        just = prop.GetJustificationAsString()

        if just == "Left":
            row_left = 0
        elif just == "Centered":
            row_left = (max_width - row[0]) / 2.
        elif just == "Right":
            row_left = max_width - row[0]

        row_right = row_left + row[0]

        if x < row_left:
            # Clicked to the left of the row
            return row_at_point, 0

        if x > row_right:
            # Clicked to the right of the row
            return row_at_point, len(rows[row_at_point])

        if row == '':
            # Clicked on the blank row (inserted a space when calculating width
            # earlier, for height considerations)
            return row_at_point, 1

        # OK, no easy answer; have to calc the width of each letter in the row till we find the column
        # Start from left
        w = 0
        ind = 1
        while row_left + w < x:
            w, _ = text_dimensions(text[:ind], prop, dpi)
            ind += 1

        return row_at_point, ind - 1

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
        if self.blank:
            self.text = ""
        if self.on_editing_end:
            self.on_editing_end(self)
        if self.blank:
            self.text = " "

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
