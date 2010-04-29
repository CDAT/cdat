*****************
Subroutines (API)
*****************

.. function:: sl_pca(var, nkeep, xeof, pc, ev, ev_sum, weights, useteof, bLargeMatrix)

	:param var: Input variable
	:type var: ``ns,nt``
	:param nkeep: Number of modes to keep
	:param xeof: Output EOFs with size ``(ns,nkeep)``
	:type xeof: float array
	:param pc: Output PCs  with size ``(nt,nkeep)``
	:type pc: float array
	:param ev: Output eigen values with size ``(nkeep)``
	:type ev: float array

	Principal Component Analysis
	
	