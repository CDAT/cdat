************
Introduction
************

Observed or simulated multi-channel timeseries generally include
a sum of different signals that can be hardly distinguished one another,
even if their respective origin is fundamentally different.
Analysis methods that are able to extract the most coherent modes
of variability generally help to identify signals of interests.

SpanLib currently focuses on the use of linear analysis methods
that rely on eign solutions of covariance or correlation matrices.

This package provides a :ref:`f90lib` (as a module)
containing a minimal collection of subroutines to perform
**Principal Componant Analysis** (PCA),
**Multi-channel Singular Spectrum Analysis** (MSSA),
**Singular Value Decomposition** (SVD),
reconstruction of components and phase composites.
The package also provide a :ref:`pymod`
that calls the F90 library and gives the user a set of useful functions
to perform analyses.


In its future version, SpanLib will also include others methods
(such as Principal Oscillation Pattern analysis) and languages
(C, Java).

