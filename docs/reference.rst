VCS Reference Guide
--------------------

init
^^^^
* Initialize, Construct a VCS Canvas Object

.. code-block:: python

    import vcs,cdms2

    file = cdms2.open('clt.nc')

    slab = file.getslab('clt')

    a = vcs.init()

    # This examples constructs 4 VCS Canvas a.plot(slab)
    # Plot slab using default settings
    b = vcs.init()

    # Construct VCS object
    template = b.gettemplate('AMIP')

    # Get 'example' template object
    b.plot(slab, template)

    # Plot slab using template 'AMIP'
    c = vcs.init()

    # Construct new VCS object
    isofill = c.getisofill('quick')

    # Get 'quick' isofill graphics method
    c.plot(slab,template,isofill)

    # Plot slab using template and isofill objects
    d = vcs.init()

    # Construct new VCS object
    isoline = c.getisoline('quick')

    # Get 'quick' isoline graphics method
    c.plot(isoline,slab,template)

    # Plot slab using isoline and template objects

help
^^^^
* Print out the object's doc string

.. code-block:: python

    import vcs
    a = vcs.init()
    ln = a.getline('red')

    # Get a VCS line object
    # This will print out information on how to use ln
    a.objecthelp(ln)

open
^^^^
* Open VCS Canvas object.
* This routine really just manages the VCS canvas. It will popup the VCS Canvas for viewing. It can be used to display the VCS Canvas.

.. code-block:: python

    import vcs
    a = vcs.init()
    a.open()

close
^^^^^
* Close the VCS Canvas. It will remove the VCS Canvas object from the screen, but not deallocate it.

.. code-block:: python

    import vcs
    a = vcs.init()
    a.plot(array, 'default', 'isofill', 'quick')
    a.close()

mode
^^^^
* ``Options <0 = manual, 1 = automatic>``
* Update the VCS Canvas.
* Updating of the graphical displays on the VCS Canvas can be deferred until a later time. This is helpful when generating templates or displaying numerous plots. If a series of commands are given to VCS and the Canvas Mode is set to manual (i.e., 0), then no updating of the VCS Canvas occurs until the 'update' function is executed.

.. note:: By default the VCS Canvas Mode is set to ``1``, which means VCS will update the VCS Canvas as necessary without prompting from the user.

.. code-block:: python

    import vcs
    a = vcs.init()
    a.mode = 0
    # Set updating to manual mode
    a.plot(array, 'default', 'boxfill', 'quick')
    box = x.getboxfill('quick')
    box.color_1 = 100
    box.xticlabels('lon30', 'lon30')
    box.xticlabels('','')
    box.datawc(1e20, 1e20, 1e20, 1e20)
    box.datawc(-45.0, 45.0, -90.0, 90.0)

    # Update the changes manually
    a.update()

update
^^^^^^
* Update the VCS Canvas manually when the ``mode`` is set to ``0`` (manual).

.. code-block:: python

    import vcs

    a = vcs.init()
    a.mode = 0

    # Go to manual mode a.plot(s,'default','boxfill','quick')
    box = x.getboxfill('quick')
    box.color_1 = 100
    box.xticlabels('lon30', 'lon30')
    box.xticlabels('','')
    box.datawc(1e20, 1e20, 1e20, 1e20)
    box.datawc(-45.0, 45.0, -90.0, 90.0)

    # Update the changes manually
    a.update()