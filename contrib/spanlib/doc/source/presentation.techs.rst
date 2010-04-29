.. techs:
*************************
Some technical details...
*************************

.. tech_pca:

...about PCA
============


CPU: space versus time
----------------------

PCA decomposition is performed on spatio-temporal datasets.
If the number of channels becomes important, PCA can use a lot of CPU since
the size of the diagonalised matrix if to the square of this number.
It is possible to partly avoid this problem when the time dimension is lower
than the spatial dimension, using a correlation matrix
in time instead of in space.
F90 subroutine :func:`sl_pca` of SpanLib
provides the ability
to choose which of theses approaches to use for PCA.


Weights
-------

In some case, not all channels have the same weight.
For instance, for gridded dataset, weight must be proportional
to the grid cell area.
Whereas common PCA analysis does not take these weights into account
it is possible to give optional weights to
:func:`sl_pca`.
Using the :ref:`python module <python>`,
it is easy to "attach" weights to a variable for use by
:meth:`pca`.


Mask
----

Similarly, it is not useful to analyse masked points
(for example, gridded points situated on land when using analyse oceanic data).
The F90 subroutine :func:`sl_pca`
makes the supposition that none of the channels are masked (all channels are analysed).
However, as well as for the weights, it is possible to associate
a spatial mask to a dataset in order to remove masked points when
using the python module.


Aggregation of variables
------------------------

One can be interested in analysing several variables ate the same time.
These variables may come from different regions, datasets and may be
even of a completely different nature.
The essential problem of units may be solved using simple
normalisations. The :mod:`python module<spanlib>` naturally integrates
this aggregation process.
[Raynaud_et_al_2006]_ describe an example of use where variables
such as sea surface temperature, wind stress modulus and air-sea CO2
fluxes are analysed at the same time:
the simultaneous variability of the variables is filtered and
the dominant oscillations are extracted for each of these variables.


Reconstructions of modes
------------------------

Reconstructions (F90::func`sl_pca_rec`, Python::meth:`spanlib.SpAn.pca_rec`)
are simply the multiplication of EOFs by their associated PC.
The reconstruction of all modes is strictly equal to the original field.
When PCA is used for a reduction of d-o-f (see :ref:`dof`),
orginal PCs are first filtered and then converted back to the original space
using saved EOFs.


.. _mssa:
...about MSSA
=============

.. _mssa_window:
The window parameter
--------------------

This is the only and essential parameter of SSA and MSSA
(F90::func:`sl_mssa`, Python::meth:`spanlib.SpAn.mssa`).
It defines the maximal value of the lags use when building
the covariance matrix.
It acts as a spectral parameter: the spectral resolution
is higher for periods lower than this period.
A standard value is one third of the time dimension.

.. _mssa_prepca:
Pre-PCA
-------

Since the covariance matrix may quickly huge depending on
parameters (number of channels and window size), is more appropriate
to perform a pre-PCA to reduce the number of channels.

.. _mssa_phases:
Phase composites
----------------

One of the most important interests of MSSA is to be able to
extract intermittent space-time oscillations from the signal.
At the first order, an oscillation is its "typical" cycle.
:func:`sl_phasecomp` (F90)
and
:func:`spanlib.get_phases` (Python)
perfom phases composites: it computes an averaged cycle and cut it an
homegeneous parts (as one can do for the annual cycle in 12 months).

.. _svd:
...about SVD
============

Pre-PCA
-------
Unlike PCA, is not possible to switch channel and time
dimension to computes EOFs.
Therefore, it is common to perform a pre-PCA, like with
MSSA, to reduce the number of channels.

