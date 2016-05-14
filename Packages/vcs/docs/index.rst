VCS: Visualization Control System
==================================

What is VCS?
---------------

The PCMDI Visualization Control System (VCS) is expressly designed to meet the needs of scientific community. VCS allows wide-ranging changes to be made to the data display, provides for hardcopy output, and includes a means for recovery of a previous display.

In the VCS model, the data display is defined by a trio of named object sets, designated the “primary objects” (or “primary elements”). These include:

* **Data Ingestion**: The data, which drives the visualization is ingested into the system via cdms2 or numeric modules such as numpy;.

* **Graphics Method**: The graphics method, which specifies the display technique.

* **Template**: The picture template, which determines the appearance of each segment of the display. Tables for manipulating these primary objects are stored in VCS for later recall and possible use.

In addition, detailed specification of the primary objects’ attributes is provided by eight “secondary objects” (or secondary elements”):

* **colormap**: Specification of combinations of 256 available colors
* **fill area**: Style, style index, and color index
* **format**: Specifications for converting numbers to display strings
* **line**: Line type, width and color index
* **list**: A sequence of pairs of numerical and character values
* **marker**: Marker type, size, and color index
* **text**: Text font type, character spacing, expansion and color index
* **text orientation**: Character height, angle, path, and horizontal/vertical alignment

By combining primary and secondary objects in various ways (either at the command line or in a program), the VCS user can comprehensively diagnose and intercompare climate model simulations. VCS provides capabilities to:

- View, select and modify attributes of data variables and of their dimensions
- Create and modify existing template attributes and graphics methods
- Save the state-of-the-system as a script to be run interactively or in a program
- Save a display as a Computer Graphics Metafile (CGM), GIF, Postscript, Sun Raster, or Encapsulated Postscript file
- Perform grid transformations and compute new data variables
- Create and modify color maps
- Zoom into a specified portion of a display
- Change the orientation (portrait vs. landscape) or size (partial vs. full-screen) of a display
- Animate a single data variable or more than one data variable simultaneously
- Display data in various geospatial projections

For an overview of the concepts present in VCS, we recommend checking out the :doc:`user-guide`.

VCS is published under the Apache 2.0 License. Its source code can be found at
https://github.com/UV-CDAT/uvcdat/Packages/vcs

Table of contents
-----------------
.. toctree::
   :maxdepth: 2

   user-guide
   developer-docs

API index
---------

* :ref:`genindex`
* :ref:`modindex`VCS