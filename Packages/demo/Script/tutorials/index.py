# Variable:    index
# Description: A python list of tuples, each tuple representing a
#              category of demos. The first element of the category tuple
#              is the name of the category, followed by a list of
#              tuples, each tuple representing an individual demo, as:
#              ( filename, title, description ).
#
# index = [ ( cat1, [ ( fname, title, descr ),
#                     ( fname, title, descr ), ...
#                   ]
#           ),
#           ( cat2, ...
#           ), ...
#         ]
#

index = [

#----------------------------------------------------
    ('CDMS Basics',
#----------------------------------------------------
     [('CDMS_Basics_1.py',
       'EARTH_SCI',
       'Opening Files and Reading Data',
       'CDMS is an easy to use interface to read in data \
from multiple binary formats, such as NetCDF, HDF, Grads/GRIB \
(with a control file), PP, DRS (special build need), ...\
CDMS is also an OpenDAP client, you can access OpenDAP served \
data by typing the URL instead of the file name. \
In this example we open a file contained in your \
cdat distribution under the sample_data directory and read \
the variable named \'clt\' contained in it.'),
        
      ('CDMS_Basics_2.py',
       'EARTH_SCI',
       'Querying Files and Variables',
       'In this example we show how to query files and variables.'),
        
      ('CDMS_Basics_3.py',
       'EARTH_SCI',
       'Creating Axes',
       'Axes are important in CDAT because they add information \
about the variable. A variable is basically an array with a lot \
of information associated with itself. Axes are very important \
as they describe the array\'s dimensions. \
There\'s basically four important things on an axis: \n\
its values \n\
its name or id \n\
its bounds \n\
its units \n\
Let\'s say in this example that we have a dataset representing the \
amount of rain in a day. Also, say that the data are only \
collected if it actually rained. Here are the mini dataset we use: \n\
Date ---- Value \n\
Jan 3        2 cm \n\
Jan 5       15 cm \n\
Jan 17      3.5 cm \n\
Jan 23      4 cm' ),
        ]
      ),

#----------------------------------------------------
    ('Plotting Basics',
#----------------------------------------------------
     [('open_and_plot.py',
       'EARTH_SCI VISUAL',
       'Open a File and Plot a Variable',
       'Learn how to simply open a netCDF data file, list and \
extract a variable, and plot a variable using the boxfill, \
isofill, and isoline graphics methods.'),
        
      ('extract_and_plot.py',
       'EARTH_SCI VISUAL',
       'Plot Extracted and Derived Data',
       'Guides you through some basic features of extracting and scripting derived data.'),

      ('orientation_and_output.py',
       'EARTH_SCI VISUAL',
       'Page Orientation and Output',
       'Setting the VCS Canvas page orientation and generating output files.'),
       
      ('simple_x_y_line_plot.py',
       'EARTH_SCI VISUAL',
       'Simple X-Y Line Plot',
       'Guides you through a simple X-Y line plot.'),
      
      ('plot_keyword_arguments.py',
       'EARTH_SCI VISUAL',
       'Plot Keword Arguments',
       'Guides you through setting keyword "Plot" arguments.'),
      
      ('animate.py',
       'EARTH_SCI VISUAL',
       'Animate a Plot',
       'Guides you through some basic animation features.'),

      ('spinning_earth.py',
       'EARTH_SCI VISUAL',
       'Spinning Earth',
       'This example shows how to generate a GIF animation of a spinning Earth.'),

      ('boxfill_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying a Boxfill Graphics Method',
       'Guides you through creating and setting boxfill graphics method attributes.'),
      
      ('isofill_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying a Isofill Graphics Method',
       'Guides you through creating and setting isofill graphics method attributes.'),

      ('isoline_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying a Isoline Graphics Method',
       'Guides you through creating and setting isoline graphics method attributes.'),
      
      ('overlay_file.py',
       'EARTH_SCI VISUAL',
       'Simple Overlay Plot',
       'Guides you through creating simple overlay plots.'),

      ('vector_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying a Vector Graphics Method',
       'Guides you through creating and setting vector graphics method attributes.'),
      
      ('scatter_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying a Scatter Graphics Method',
       'Guides you through creating and setting scatter graphics method attributes.'),
      
      ('td.py',
       'EARTH_SCI VISUAL',
       'Taylor Diagrams',
       'Describes how to plot simple Taylor diagrams.'),
      
      ('continents_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying a Continents Graphics Method',
       'Guides you through choosing defined continents.'),
      
      ('show_continents.py',
       'EARTH_SCI VISUAL',
       'Creating Continents',
       'Guides you through creating external continents.'),
      
      ('output_file.py',
       'EARTH_SCI VISUAL',
       'Save Plot to an Output File',
       'Guides you through saving a plot in various graphics file formats.'),
      
      ('primitive_file.py',
       'EARTH_SCI VISUAL',
       'Plotting Primitives',
       'Guides you through plotting using graphics primitives.'),
      
      ('template_file.py',
       'EARTH_SCI VISUAL',
       'Creating and Modifying Templates',
       'Guides you through creating and setting template attributes.'),
      
      ]
     ),
    
#----------------------------------------------------
    ('Loops',
#----------------------------------------------------
     [('Loops_1.py',
       'DATA_ANALYSIS',
       'Looping Through Files',
       'In this example we loop through files and display some of their information.'),

      ('Loops_2.py',
       'DATA_ANALYSIS',
       'Averaging Over Multiple Files',
       'In this example we loop over multiple files and print their computed average.'),

      ('Loops_3.py',
       'DATA_ANALYSIS',
       'Concatenating Data',
       'In this example we loop through files containing each \
a year of data. We then concatenate the data either along \
the time dimension (to compute the time average) or along a new \
dimension in order to compute statitistics between years (standard \
deviation, etc..)'),

      ('statistics_tutorial.py',
       'DATA_ANALYSIS',
       'Statistical Functions',
       'This tutorial demonstrates usage of some basic statistical functions in the \
genutil module.'),
      
      ]
     ),

#----------------------------------------------------
    ('Air Pollution',
#----------------------------------------------------
     [('adv_subset.py',
       'AIR_POLLUTION',
       'Advanced spatial subsets of a variable',
       'Shows how to have finer control over the subset.  This \
uses CDMS subsetting, so all the subsets should be done in the \
native coordinates.Loops_1.py'),

      ('average_var.py',
       'AIR_POLLUTION',
       'Average variable',
       'Reads in data from multiple files and gets a daily average.'),

      ('change_datatype.py',
       'AIR_POLLUTION',
       'Change the data type',
       'Shows how to convert data between different types.'),

      ('change_date.py',
       'AIR_POLLUTION',
       'Change the date',
       'Shows how to change the starting date of a variable.'),

      ('change_variable.py',
       'AIR_POLLUTION',
       'Change the metadata and the values of a variable.',
       'This example creates a NOx variable from NO and NO2, \
converts it into ppb from ppm, and changes some metadata.'),

      ('coord_conv.py',
       'AIR_POLLUTION',
       'Coordinate conversions',
       'Shows how to convert coordinates and test whether a \
coordinate or date is within the spatial or temporal domain.'),

      ('mask_variable.py',
       'AIR_POLLUTION',
       'Masking a variable',
       'Shows how to mask a variable and create a new iovar variable.'),

      ('scan_var.py',
       'AIR_POLLUTION',
       'Scanning variables',
       'Shows how to read in data from multiple files an write out a single variable.'),

      ('spatial_subset.py',
       'AIR_POLLUTION',
       'Spatial subset',
       'Shows how to subset a variable spatially.'),

      ('temporal_subset.py',
       'AIR_POLLUTION',
       'Temporal subset',
       'Shows how to subset a variable temporally.'),

      ('vertical_subset.py',
       'AIR_POLLUTION',
       'Vertical subset',
       'Shows how to subset a variable vertically.'),

      ('write_ioapi.py',
       'AIR_POLLUTION',
       'Writing IOAPI files',
       'Shows how to read in data from a file and write out IOAPI files.'),

      ('write_netCDF.py',
       'AIR_POLLUTION',
       'Writing netCDF files',
       'Shows how to read in data from a file and write out \
Climate and Forcast compliant netCDF files.'),
      
      ]
     ),

#----------------------------------------------------
    ('Files',
#----------------------------------------------------
     [('Files_1.py',
       'APP_DEVEL',
       'Opening a File and Looking at Its Content',
       'In this example we open a file with cdms and query its content.'),

      ('Files_2.py',
       'APP_DEVEL',
       'Opening and Reading an ASCII File',
       'In this example we open an ASCII file and show how to read and parse its content.'),

      ('Files_3.py',
       'APP_DEVEL',
       'Concatenating NetCDF Files',
       'In this example we concatenate data split between three files into one.'),

      ('Files_5.py',
       'APP_DEVEL',
       'Printing to an ASCII File',
       'In this example we write data to an ASCII file.')
      ]
     ),

#----------------------------------------------------
    ('Variables and Axes',
#----------------------------------------------------
     [('Variables_and_Axes_1.py',
       'APP_DEVEL',
       'Creating a Variable From Scratch - Part 1',
       'How to create a variable from some data and set some attributes on it.'),
      
      ('Variables_and_Axes_2.py',
       'APP_DEVEL',
       'Creating a Variable From Scratch - Part 2',
       'How to create axes to \"decorate\" a variable.'),
      ]    
     ),
    
#----------------------------------------------------
    ('Advanced CDAT Scripting',
#----------------------------------------------------
     [('global_anomalies.py',
       'EARTH_SCI',
       'Global Anomalies',
       'Shows how to extract several AMIP model data, generate \
global anomalies data and save it to a NetCDF file, and create \
a global anomalies plot.'),

      ('regrid_mask_anomaly.py',
       'EARTH_SCI',
       'Regridding, Masking, and Calculating Anomaly',
       'Shows how to regrid data from one grid onto another \
grid, create mask and mask the data, merge sea and land data, \
and calculate annual cycle anomaly.'),

      ('compare_datasets.py',
       'EARTH_SCI APP_DEVEL',
       'Comparing Two Different Datasets',
       'Shows how to compare two different datasets with \
different resolutions and how to use the statistical module.'),

      ('redecorate.py',
       'EARTH_SCI APP_DEVEL',
       'Saving File and Variable Attributes to Attach them to a New Variable and File',
       'Shows how to save the file\'s global attributes \
together with the variable attribute to use them when writing \
a derived variable to a new file.'),
      ]
     ),


#----------------------------------------------------
    ('Diagnostics',
#----------------------------------------------------
     [
      ('test_wk_01.py',
       'DIAGNOSTICS',
       'Spectral Wave Number and Frequencies',
       'Shows how to produce a diagnostics plot displaying spectral \
wave number and frequences of symmetric and asymmetric components.'),
      
      ('skew-t-basic.py',
       'DIAGNOSTICS',
       'Skew-T Basic Plot',
       'Shows how to produce a simple Skew-T plot.'),
       
       ('skew-t-more.py',
        'DIAGNOSTICS',
        'Skew-T Plot With Aspect Ratio',
        'Shows how to produce a Skew-T plot with some options and a different aspect ratio.'),
      
      ('skew-t-windbarbs.py',
       'DIAGNOSTICS',
       'Skew-T Plot With Windbarbs',
       'Shows how to produce a Skew-T plot with windbarbs.'),
      ]
     ),
     
#----------------------------------------------------
    ('Xmgrace',
#----------------------------------------------------
     [
    ('xmgrace_tutorial_Numeric.py',
       'XMGRACE',
       'Xmgrace Basic Plotting Example',
       'Creates a portrait page with two plots, first plot on top being composed of two curves.'),
      ]
     ),

#----------------------------------------------------
    ('Xmgrace',
#----------------------------------------------------
     [('eztemplate_simple.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Basic Example',
       'In this example we simply plot three columns and four rows.'),

      ('eztemplate_legend_side_each.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Local Legend',
       'In this example we plot a legend on the side of each plot.'),

      ('eztemplate_legend_horiz_per_row.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Horizontal Legend Per Row',
       'In this example we show how to draw one horizontal legend bar on each row.'),

      ('eztemplate_legend_vert_per_row.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Vertical Legend Per Row',
       'In this example we show how to draw one vertical legend bar on each row.'),

      ('eztemplate_margins_legend_thickness.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Margins And Legend Thickness',
       'In this example we show how to control the margins an legend thickness.'),

      ('eztemplate_margins_legend_direction.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Margins And Legend Direction',
       'In this example we show how to control the margins and legend direction.'),

      ('eztemplate_legend_mix_local_global.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Mixing Local And Global Legend',
       'In this example we show how to mix the local and global legend \
and how to alternate the direction of the local legend bars.'),

      ('eztemplate_reverse.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Reverse Order',
       'In this example we show how to display templates in reversed order.'),

      ('eztemplate_spacing.py',
       'EZTEMPLATE VISUAL',
       'EzTemplate Spacing',
       'In this example we show how to control the spacing parameter')

      ]
     ),
     
#----------------------------------------------------
    ]  # The end.
#----------------------------------------------------
      
