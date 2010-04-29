.. fundamentals:

************
Fundamentals
************

PCA
===

Principal Component Analysis is also know as Empirical Orthogonal Functions (EOFs) decomposition:
it decomposes a space-time signal in pairs of
spatial EOFs and temporal
Principal Components (PCs)
that are the eigen solutions of the covariance (or correlation) matrix of the initial signal.
The first EOFs represent the dominant, pure spatial patterns of variability,
and their associated PCs are the coefficients that regulate these patterns.

.. note::

	In this document, "space" refers to the more general notion
	of "channel",  in opposition to "time".
	In climate studies, the channel dimension generally coincides with space.

For more information, see for example [Preisendorfer_1988]_ and [Wilks_95]_.

MSSA
===

Single channel analysis (SSA)
-----------------------------

Singular Spectrum Analysis is mathematically very similar to PCA:
there is now only one channel as an input dataset, and eigenmodes are computed on
the lag-covariance matrix (instead of on the cross -between channels- covriance matrix).
The EOFs have only a temporal dimension.
Therefore, SSA is intended to provides information on purely temporal signal, like
a classical Fourier decomposition.
However, SSA has many advantages on the latter method:

- It removes <emphasis>incoherent noise</emphasis> (white noise): 
the noisy part of the signal takes the form of low order
modes, identified as a "background" that can be easily neglected.
- It naturally extracts <emphasis>regular oscillations</emphasis> (with a narrow spectral peak).
These oscillations are identified as pair of modes whose PCs and EOFs
  are in phase quadrature, that can be <emphasis>intermittent</emphasis>.
- Coherent nonlinear trends are identified as the lower frequency modes.
- Compared to others, this method is efficient on short signal.

The maximal lag (the only parameter of SSA) is known as the
:ref:`window <mssa_window>`.

For more information, see for example [Broomhead_and_King_1986]_ and [Vautard_and_Ghil_1989]_.


Multi-channel analysis (MSSA)
-----------------------------

Multi-channel Singular Spectrum Analysis
is a combination of PCA and SSA: it is an SSA on several channels.
The diagonalized is built on covariances between channels (cross) and time segments (lag).
Therefore, it has the advantage of PCA for extracting the dominant "spatial" patterns
of the variability, and has also the spectral filtering capabilities of SSA.
All identified modes have spatio-temporal properties.
For example, oscillations are not constrained on a fixed spatial pattern, but can also
have a *propagative signature* over their cycle.
This advanced spatial and spectral filtering is helpful to identify
the most coherent (and more especially oscillatory) spatio-temporal modes in a short
noisy signal. 

For more information, see for example [Plaut_and_Vautard_1994], 
[Raynaud_et_al_2005]_ and  [Raynaud_et_al_2006]_.


Monte-Carlo statistical test
----------------------------

There is no formula to evaluate the statistical significance of the MSSA modes.
Indeed, colored noise may exhibit high spectral power at low frequencies,
and there is no way to explicitly seperate such noise from the signature
of pure dynamical oscillations.
[Allen_and_Smith_96]_ implemented an algorithm to estimate an interval
of bad confidence for MSSA eigen values against colored noise. 
It consists in the following phases :

	1) Building an ensemble of noisy dataset that have similar
	   properties (i.e for example lag 0 and 1 corvariance for
	   red noise) to those of the original dataset.
	2) Projecting the MSSA EOFs onto the lagged-cross-covariance
	   matrix (computed in the same way as for MSSA) 
	   of each noisy dataset.
	3) Extracting the trace of the resulting projections to get
	   an ensemble of "fake" eigenvalues.
	4) Computing quantiles (let's say 5%-95%) of the ensemble 
	   and compare them to the MSSA eigenvalues.

If eigenvalues fall into the interval defined by the quantiles,
associated modes must be rejected.

SVD
===

Singular Value Decomposition
works in a similar way to PCA, except that two different datasets are
decomposed at the same time.
Therefore, the diagonalyzed matrix is a cross-covariance matrix and
may not be symetric.
Dominant EOFs and PCs can be used for example to build a statistical
model linking the second variable to the first one.

All these analysis methods act as a linear filter.
For each of them, it is possible to reconstruct part of the filtered signal.
A reconstructed mode is the "multiplication" of its EOF by its PC, and
it has the same dimension of the initial dataset.
Such operation is necessary to go back from the EOF space to the physical space.

.. _dof:
Finally, PCA may be used also to simply reduce the number of degree-of-freedom (d-o-f) of a dataset.
For example, you can keep the first PC that explain a 70% of the variance.
These PCs are then used as an input dataset for other analysis.
This methodology is useful for MSSA and SVD since the eigen problem solving
may be very time consuming: we are now able, for example, to potentially reduce the
number of channels from several hundred or thounsand, to less than 20.


