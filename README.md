cdat
======
[![build status](https://travis-ci.org/CDAT/cdat.svg?branch=master)](https://travis-ci.org/CDAT/cdat/builds)
[![stable version](http://img.shields.io/badge/stable%20version-8.1-brightgreen.svg)](https://github.com/CDAT/cdat/releases/tag/v8.1)
![platforms](http://img.shields.io/badge/platforms-linux%20|%20osx-lightgrey.svg)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2586088.svg)](https://doi.org/10.5281/zenodo.2586088)

[![Anaconda-Server Badge](https://anaconda.org/cdat/cdat/badges/installer/conda.svg)](https://conda.anaconda.org/cdat)
[![Anaconda-Server Badge](https://anaconda.org/cdat/cdat/badges/downloads.svg)](https://anaconda.org/cdat/cdat)

CDAT builds on the following key technologies:

  1. Python and its ecosystem (e.g. NumPy, Matplotlib);
  2. Jupyter Notebooks and iPython;
  3. A toolset developed at LLNL for the analysis, visualization, and management of large-scale distributed climate data;
  4. VTK, the Visualization Toolkit, which is open source software for manipulating and displaying scientific data.

These combined tools, along with others such as the R open-source statistical
analysis and plotting software and custom packages (e.g. DV3D), form CDAT
and provide a synergistic approach to climate modeling, allowing researchers to
advance scientific visualization of large-scale climate data sets. The CDAT
framework couples powerful software infrastructures through two primary means:

  1. Tightly coupled integration of the CDAT Core with the VTK infrastructure to provide high-performance, parallel-streaming data analysis and visualization of massive climate-data sets (other tighly coupled tools include
  VCS, DV3D, and ESMF/ESMP);
  2. Loosely coupled integration to provide the flexibility of using tools quickly
  in the infrastructure such as ViSUS or R for data analysis and
  visualization as well as to apply customized data analysis applications within
  an integrated environment.

Within both paradigms, CDAT will provide data-provenance capture and
mechanisms to support data analysis.

CDAT is licensed under the [BSD-3][bds3] license.

------
We'd love to get contributions from you! Please take a look at the
[Contribution Documents](CONTRIBUTING.md) to see how to get your changes merged
in.
