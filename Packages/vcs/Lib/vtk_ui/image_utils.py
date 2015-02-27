import math
from vtk import vtkImageData, VTK_UNSIGNED_CHAR
import text

#__bg_window__ = None
#__bg_renderer__ = None

def load_image(image):
    from vtk import vtkPNGReader, vtkJPEGReader, vtkBMPReader, vtkTIFFReader
    try:
        readers = {
             ".png": vtkPNGReader,
            ".jpeg": vtkJPEGReader,
             ".jpg": vtkJPEGReader,
             ".bmp": vtkBMPReader,
             ".tiff": vtkTIFFReader,
        }
        from os.path import splitext
        _, extension = splitext(image)
        reader_type = readers[extension]
        reader = reader_type()
        reader.SetFileName(image)
        reader.Update()
        image = reader.GetOutput()
    except (TypeError, AttributeError):
        # splitext will raise an AttributeError if it is used on something that isn't a string
        # Not a string; either a vtkImageData or the user did something dumb
        if type(image) != vtkImageData:
            raise TypeError("image should be an instance of vtk.vtkImageData, or a path to an image file.")
    except KeyError:
        raise ValueError("image should refer to an image of one of these types: %s; provided %s" % (readers.keys(), extension))
    return image

def rounded_rect(width, height, radius, color, antialiased=True):
    c = color_image_data(width, height, color)
    mask = rounded_rect_mask(width, height, radius)
    if antialiased:
        mask = antialias_mask(mask, passes=2)
    return mask_image(c, mask)

def triangle(width, height, color, antialiased=True):
    c = color_image_data(width, height, color)
    mask = triangle_mask(width, height)
    
    if antialiased:
        mask = antialias_mask(mask, passes=2, fade=.75)

    return mask_image(c, mask)

def mask_image(image, mask):
    for c_ind, col in enumerate(mask):
        for r_ind, row in enumerate(col):
            # Set alpha
            image.SetScalarComponentFromFloat(c_ind, r_ind, 0, 3, row * 255)
    return image

def transpose(image):
    """
    Rows to columns, columns to rows
    """
    w, h, _ = image.GetDimensions()
    components = image.GetNumberOfScalarComponents()
    data = img(h, w, components=components)

    for col in xrange(w):
        for row in xrange(h):
            for component in xrange(components):
                data.SetScalarComponentFromFloat(row, col, 0, component, image.GetScalarComponentAsFloat(col, row, 0, component))
    return data

def mirror(image, vertical=True):
    """
    Flip vertical/horizontal
    """
    w, h, _ = image.GetDimensions()
    components = image.GetNumberOfScalarComponents()
    data = img(w, h, components=components)

    for x in xrange(w):
        for y in xrange(h):
            if vertical:
                new_x = x
                new_y = h - y - 1
            else:
                new_x = w - x - 1
                new_y = y

            for c in xrange(components):
                data.SetScalarComponentFromFloat(new_x, new_y, 0, c, image.GetScalarComponentAsFloat(x, y, 0, c))
    return data

def combine_images(*images):
    """
    Layer images onto each other (centered) and return the output
    """
    width, height = 0, 0
    for i in images:
        new_w, new_h, _ = i.GetDimensions()
        if new_w > width:
            width = new_w
        if new_h > height:
            height = new_h

    from vtk import vtkImageBlend
    blender = vtkImageBlend()

    for index, image in enumerate(images):
        w, h, _ = image.GetDimensions()
        if w != width or h != height:
            image = pad_image(image, pad_width=width, pad_height=height)
        blender.AddInputData(image)

    blender.Update()
    return blender.GetOutput()

def antialias_mask(mask, fade = .85, passes = 3):
    # We'll use this mask to make the aliasing changes, so antialiased 
    # cells don't effect their neighbors
    
    for _ in xrange(passes):
        antialiased = [[] for col in mask]

        for col_ind, col in enumerate(mask):
            for row_ind, pixel in enumerate(col):
                
                if pixel == 0:
                    # Antialias this pixel
                    alphas = []
                    if col_ind > 0:
                        if mask[col_ind - 1][row_ind] > 0:
                            alphas.append(mask[col_ind - 1][row_ind])

                    if row_ind > 0:
                        if mask[col_ind][row_ind - 1] > 0:
                            alphas.append(mask[col_ind][row_ind - 1])

                    if col_ind < len(mask) - 1:
                        if mask[col_ind + 1][row_ind] > 0:
                            alphas.append(mask[col_ind + 1][row_ind])

                    if row_ind < len(col) - 1:
                        if mask[col_ind][row_ind + 1] > 0:
                            alphas.append(mask[col_ind][row_ind + 1])

                    # This is a wild guess.
                    if alphas:
                        pixel = sum(alphas) / float(len(alphas)) * fade

                antialiased[col_ind].append(pixel)

        mask = antialiased

    return mask

def triangle_mask(width, height):
    grid = [[0 for _ in xrange(height)] for _ in xrange(width)]

    for col_ind, col in enumerate(grid):
        if col_ind >= (width - 2) / 2.0 :
            break

        for row_ind, pix in enumerate(col):
            if row_ind == height - 2:
                continue
            if row_ind <= 2 * height / float(width) * col_ind:
                grid[col_ind + 2][row_ind] = 1
                # True for both sides
                grid[width - col_ind - 3][row_ind] = 1

    return grid

def rounded_rect_mask(width, height, radius):
    grid = [[1 for _ in xrange(height)] for _ in xrange(width)]

    for col_ind, col in enumerate(grid):
        if col_ind > radius and col_ind < width - radius:
            continue
        for row_ind, pix in enumerate(col):
            
            if row_ind > radius and row_ind < height - radius:
                continue
            
            if col_ind <= radius:
                x = radius
            else:
                # -1 is to adjust for 0 indexing
                x = width - radius - 1

            if row_ind <= radius:
                y = radius
            else:
                # -1 is to adjust for 0 indexing
                y = height - radius - 1
            
            length = distance((x, y), (col_ind, row_ind))
            if length > radius:
                grid[col_ind][row_ind] = 0

    return grid


def color_image_data(width, height, color):

    image_data = img(width, height)

    for i in range(width):
        for j in range(height):
            # Set opacity
            if len(color) == 3:
                image_data.SetScalarComponentFromFloat(i, j, 0, 3, 255)
            for component, value in enumerate(color):
                image_data.SetScalarComponentFromFloat(i, j, 0, component, math.floor(value * 255))

    return image_data


def print_cols(cols):
    """
    Used to visualize image masks
    """
    counter = 0
    max_counter = len(cols[0])
    while counter < max_counter:
        row = ""
        for col in cols:
            row = "%s %d" % (row, col[counter])
        print row
        counter += 1

def distance(point_1, point_2):
    """
    Calculate distance between two points
    """
    
    xdiff = (point_1[0] - point_2[0])
    ydiff = (point_1[1] - point_2[1])

    return math.sqrt(xdiff ** 2 + ydiff ** 2)

def render_text(string, padding=5, width=None, height=None, size=12, font="Arial", fgcolor=(0,0,0), bgcolor=None):
    """
    Render text to an image and return it
    
    Not specifying bgcolor will give a transparent image, but that will take a *lot* more work to build.
    Specifying a bgcolor, width, and height will heavily optimize things.
    """
    actor = text.text_actor(string, fgcolor, size, font)

    if bgcolor is None:
        mask = True
        # Set it to the opposite of fgcolor so we can mask using it
        bgcolor = (1 - fgcolor[0], 1 - fgcolor[1], 1 - fgcolor[1])
    else:
        mask = False

    lines = string.split("\n")

    if width is None:
        # EM is defined as the square of the line height, and is the guide for making fonts
        # We can use that as an upper bound (assuming font size is ~ line height)
        width = size * max([len(s) for s in lines])

    if height is None:
        height = size * len(lines)
    
    image = actor_to_image(actor, bgcolor, width, height)
    
    if mask:
        image = mask_color(image, bgcolor)
        image = crop_blank_space(image)
        width, height, _ = image.GetDimensions()

        return pad_image(image, pad_width = width + padding * 2, pad_height= height + padding * 2)
    else:
        return image

def pad_image(image, pad_width = None, pad_height = None):

    width, height, _ = image.GetDimensions()
    pwidth = pad_width if pad_width is not None else width
    pheight = pad_height if pad_height is not None else height

    # Have to adjust width and height because Extent uses coordinates, not lengths.
    padded = img(pwidth, pheight)
    from math import floor
    left_pad = int(floor((pwidth - width) / 2.0))
    right_pad = pwidth - width - left_pad
    # Pad the left
    for x in range(left_pad):
        for y in range(pheight):
            for i in range(4):
                padded.SetScalarComponentFromFloat(x, y, 0, i, 0)

    # Pad the right
    for x in range(width, width + right_pad):
        for y in range(pheight):
            for i in range(4):
                padded.SetScalarComponentFromFloat(x + left_pad, y, 0, i, 0)

    top_pad = int(floor((pheight - height) / 2.0))
    # bottom_pad = pheight - height - top_pad

    for x in range(width):
        for y in range(pheight):
            if y < top_pad or y >= height + top_pad:
                # Pad the top and bottom
                for i in range(4):
                    padded.SetScalarComponentFromFloat(x + left_pad, y, 0, i, 0)
            else:
                # Copy the image
                for i in range(4):
                    component = image.GetScalarComponentAsFloat(x, y - top_pad, 0, i)
                    padded.SetScalarComponentFromFloat(x + left_pad, y, 0, i, component)
    return padded

def crop_blank_space(image):
    """
    Extracts the visual data from an image, and returns a padded version of the image.
    """
    width, height, _ = image.GetDimensions()
    left, bottom = 0, 0
    
    right = 0
    # Scan columns leftward from the right edge
    x = width - 1
    while x > 0:
        populated = False
        for y in range(height - 1, 0, -1):
            if image.GetScalarComponentAsFloat(x, y, 0, 3) != 0:
                populated = True
                break

        if populated:
            right = x + 1
            break

        x = x - 1

    # Scan columns rightward from the left edge
    x = 0
    while x < width:
        populated = False
        for y in range(height - 1, 0, -1):
            if image.GetScalarComponentAsFloat(x, y, 0, 3) != 0:
                populated = True
                break

        if populated:
            left = x
            break
        x += 1

    # Scan rows downward from the top edge
    y = height - 1
    top = 0
    while y > 0:
        populated = False
        for x in range(width - 1, 0, -1):
            if image.GetScalarComponentAsFloat(x, y, 0, 3) != 0:
                populated = True
                break

        if populated:
            top = y + 1
            break
        y -= 1
    
    y = 0
    while y < height:
        populated = False
        for x in range(width - 1, 0, -1):
            if image.GetScalarComponentAsFloat(x, y, 0, 3) != 0:
                populated = True
                break

        if populated:
            bottom = y
            break
        y += 1

    data = vtkImageData()
    
    # Have to adjust height/width because right - left gives the width, not the coordinate of the endpoint
    data.SetExtent(0, right - left - 1, 0, top - bottom - 1, 0, 0)
    data.AllocateScalars(VTK_UNSIGNED_CHAR, 4)

    for x in range (right - left):
        for y in range(top - bottom):
            for i in range(4):
                component = image.GetScalarComponentAsFloat(left + x, bottom + y, 0, i)
                data.SetScalarComponentFromFloat(x, y, 0, i, component)

    return data

def mask_color(image, mask_color):
    """
    Turns a color transparent; assumes that everything else should be the inverse color of mask_color (1-r , 1-b, 1-g)
    """
    from math import floor
    
    mask_color = [ floor(255 * component) for component in mask_color]
    
    keep_color = [ 255 - component for component in mask_color]
    # Create an image with an alpha channel
    image_data = vtkImageData()
    
    width, height, _ = image.GetDimensions()
    image_data = img(width, height)
    # Consider tracking what the min and max opacities are, and mapping all opacities to that range
    # Probably hide that behind a KWARG opation; sounds like extra cycles for not a lot of gain.
    # Copy over the pixels from image
    for x in range(width):
        for y in range(height):
            for component in range(3):
                pix_component = image.GetScalarComponentAsFloat(x, y, 0, component)
                # Remap all colors to the keep color; we're using alpha to get rid of the mask color
                image_data.SetScalarComponentFromFloat(x, y, 0, component, keep_color[component])
            # Since the mask color is the inverse of the keep color, we only need one component to figure out the alpha
            alpha = floor(abs(mask_color[component] - pix_component) / abs(keep_color[component] - mask_color[component]) * 255)
            alpha = min(alpha, 255)
            if alpha < 10:
                # Colors aren't perfect; let's just chop everything off below here, this is basically invisible anyway
                alpha = 0
            if alpha > 245:
                alpha = 255
            image_data.SetScalarComponentFromFloat(x, y, 0, 3, alpha)

    return image_data


def actor_to_image(actor, bgcolor, width, height, bgrender=True):
    """
    Render an actor offscreen to an
    image with the given dimensions and BG
    """
    from vtk import vtkRenderer, vtkRenderWindow, vtkWindowToImageFilter
    
    # bg window and bg renderer should be cached, but it's causing corrupted images to be generated. Not going to worry about it right now.

    #global __bg_window__
    #if __bg_window__ is None:
    __bg_window__ = vtkRenderWindow()
    if bgrender:
        __bg_window__.OffScreenRenderingOn()
    else:
        __bg_window__.OffScreenRenderingOff()

#    global __bg_renderer__
 #   if __bg_renderer__ is None:
    __bg_renderer__ = vtkRenderer()
    __bg_renderer__.SetBackground(*bgcolor)
    __bg_window__.AddRenderer(__bg_renderer__)

    __bg_renderer__.AddActor(actor)
    
    __bg_window__.SetSize(width, height)

    __bg_window__.Render()

    imageFilter = vtkWindowToImageFilter()
    imageFilter.SetInput(__bg_window__)
    imageFilter.Update()

    image_data = imageFilter.GetOutput()

    imageFilter.SetInput(None)
    __bg_renderer__.RemoveActor(actor)
    __bg_renderer__.Clear()
    
    return image_data

def save_png(image, filename):
    from vtk import vtkPNGWriter
    writer = vtkPNGWriter()
    writer.SetFileName(filename)
    writer.SetInputData(image)
    writer.Update()
    writer.Write()

def img(width, height, components=4):
    """
    Builds a ready-to-go vtkImageData
    """
    image_data = vtkImageData()
    image_data.SetDimensions(width, height, 1)
    
    image_data.AllocateScalars(VTK_UNSIGNED_CHAR, components)
    
    return image_data