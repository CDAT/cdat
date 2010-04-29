#################################################################################
# File: spanlib_python.py
#
# This file is part of the SpanLib library.
# Copyright (C) 2006-2009  Stephane Raynaud, Charles Doutriaux
# Contact: stephane dot raynaud at gmail dot com
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 2.1 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
#################################################################################

import spanlib_fort,numpy as npy
try:
	import cdms2 ,MV2
	MV = MV2
	cdms = cdms2
	cdms2_isVariable = cdms2.isVariable
	has_cdat_support = True
except:
	cdms2_isVariable = lambda var: False
	has_cdat_support = False
import copy,gc
from warnings import warn

docs = dict(
	npca = """- *npca*: int | ``None``
				Number of PCA modes to keep in analysis (defaults to 10).""", 
	prepca = """- *prepca*: int | bool | ``None``
				Number of pre-PCA modes to keep before MSSA and SVD analyses (defaults to ``npca`` if ``True``).
				If the number of input channels is greater than 30, it is automatically switch to ``True``.""", 
	nmssa = """- *nmssa*: int | ``None``
				Number of MSSA modes to keep in analysis (defaults to 10).""", 
	window = """- *window*: int | ``None``
				Size of the MSSA window parameter (defaults to 1/3 the time length).""", 
	nsvd = """- *nsvd*: int | ``None``
				Number of SVD modes to keep in analysis (defaults to 10).""", 
	iset = """- *iset*: int | ``None``
				Set it to an integer to perform reconstruction on one dataset.
				If ``None``, all dataset are reconstructed.""", 
	modes = """- *modes*: int | list | tuple
				If ``None``, all modes are summed. 
				Example of other usages:
				
					- ``4`` or ``[4]`` or ``(4,)``: only mode 4
					- ``-4``: modes 1 to 4
					- ``(1,3,-5)``: modes 1, 3, 4 and 5 (``-`` means "until")""", 
	raw = """- *raw*: bool
				When pre-PCA is used and ``raw`` is ``True``, it prevents from
				going back to physical space (expansion to PCA EOFs space).""", 
	scale = """- *scale*: bool | float
				Apply a factor to EOFs or PCs. This is essentially useful to add
				a quantitative meaning. If ``True``, ``scale`` is chosen so that
				the standard deviation of the mode (EOF or PC) is the square of
				the associated eigen value.""", 
	relative = """- *relative*: bool
				Return percentage of variance instead of its absolute value.""", 
	sum = """- *sum*: bool
				Return the sum of ALL (not only the selected modes) eigen values (total variance).""", 
	cumsum = """- *cumsum*: bool
				Return the cumulated sum of eigen values.""", 
)

def _filldocs_(func):
	func.__doc__ = func.__doc__ % docs
	return func





class SpAn(object):
	
	_npca_default = 10
	_npca_max = 30
	_nmssa_default = _nsvd_default = 8
	_nmssa_max = _nsvd_max = 20
	_window_default = 1/3. # Relative to time length
	_pca_params = ['npca', 'prepca']
	_mssa_params = ['nmssa', 'window']
	_svd_params = ['nsvd']
	_params = dict(pca=_pca_params, mssa=_pca_params+_mssa_params, svd=_pca_params+_svd_params)
	_all_params = _pca_params+_mssa_params+_svd_params

	def __init__(self, datasets, serial=False, weights=None, norms=True, quiet=False, **kwargs):
		""" Prepare the Spectral Analysis Object

		Description:::
		  This function creates an object for future analyses.
		  It optionally initializes some parameters.
		:::

		Usage:::
		analysis_object = SpAn(datasets,weights=None,npca=None,window=None,nmssa=None)

		  data	:: List of data on which to run the PC Analysis
		    Last dimensions must represent the spatial dimensions.
		    Analysis will be run on the first dimension.
		  weights :: If you which to apply weights on some points,
		    set weights to "0" where you wish to mask.
		    The input data mask will be applied,
		    using the union of all none spacial dimension mask.
		    If the data are on a regular grid, area weights
		    will be generated, if the cdutil (CDAT) module is available.
		    default: 1. everywhere]
		  npca	:: Number of principal components to return [default: 10]
		  nmssa   :: Number of MSSA modes retained [default: 4]
		  nsvd	:: Number of SVD modes retained [default: 10]
		  window  :: MSSA window parameter [default: time_length/3.]
		  serial :: If we have a list (or tuple) of variables as "datasets", they are analysed independantly (serial analyses in opposition to parallel analyses) if serial is True [default: False]. If False, they are packed before being analysed.
		:::

		Output:::
		  analysis_object :: SpAn object created for further analysis
		:::
		"""

		self._quiet = quiet
		self.clean()
		
		# We organize datasets as list of list of variables
		self._map_(datasets, serial)

		# Weights and norms in the same form as datasets
		norms = kwargs.pop('norm', norms)
		weights = kwargs.pop('weight', weights)
		if norms is None:	norms = 1.
		norms = self._check_shape_(norms, True)
		for iset,d in enumerate(self._datasets):
			if len(d) == 1: norms[iset] = [1.]
		weights = self._check_shape_(weights, None)

		# We stack and pack data
		getinvalid = kwargs.pop('getinvalid', None)
		nvalid = kwargs.pop('nvalid', None)
		for iset,d in enumerate(self._datasets):
			self._stack_(d, weights[iset], norms[iset], getinvalid=getinvalid, nvalid=nvalid)
		self._ndataset = len(self._datasets)
		self._ndata= [len(dataset) for dataset in self._datasets]


		# Check and save parameters
		self._update_(None, **kwargs)
#		self._update_(npca=npca,window=window,nmssa=nmssa,nsvd=nsvd,prepca=prepca)


	#################################################################
	## Get datasets info
	#################################################################

	def _time_axis_(self,iset,idata=None):
		"""Get the time axis of data variable of a dataset"""
		if idata is None:
			return self._stack_info[iset]['taxes']
		return self._stack_info[iset]['taxes'][idata]

	def _space_axis_(self,iset,idata,iaxis=None):
		"""Get a sp_ace axis of data variable of a dataset"""
		if iaxis is None:
			return self._stack_info[iset]['saxes'][idata]
		else:
			return self._stack_info[iset]['saxes'][idata][iaxis]

	def _mode_axis_(self,analysis_type,isets=None):
		"""Get a mode axis according to the type of modes (pca, mssa, svd)"""
		if not self._mode_axes.has_key(analysis_type):
			self._mode_axes[analysis_type] = {}
		single = False
		if analysis_type == 'svd':
			isets = [0]
		elif isets is None: 
			isets = xrange(self._ndataset)
		elif not isinstance(isets,(list,tuple)):
			isets = [isets]
			single = True
		out = []
		for iset in isets:
			nn = getattr(self,'_n'+analysis_type)
			if isinstance(nn, list): nn = nn[iset]
			if not has_cdat_support:
					self._mode_axes[analysis_type][iset] = nn
			elif not self._mode_axes[analysis_type].has_key(iset) or \
				len(self._mode_axes[analysis_type][iset]) != nn:
				self._mode_axes[analysis_type][iset] = cdms2.createAxis(npy.arange(1,nn+1))
				self._mode_axes[analysis_type][iset].id = analysis_type+'_mode'
				self._mode_axes[analysis_type][iset].long_name = analysis_type.upper()+' modes in decreasing order'
				if analysis_type!='svd':
					self._check_dataset_tag_('_mode_axes',iset,analysis_type, svd=analysis_type=='svd')
			if analysis_type == 'svd': return self._mode_axes[analysis_type][iset]
			out.append(self._mode_axes[analysis_type][iset])
		if single: return out[0]
		return out

	def _channel_axis_(self,iset, name, **kwargs):
		"""Get the channel axis for one dataset (MSSA or SVD)"""
		if not self._prepca[iset]:
			nchan = self._ns[iset]
		else:
			nchan = self._prepca[iset]
		channel_axes = getattr(self,'_%s_channel_axes'%name)
		if not has_cdat_support:
			channel_axes[iset] = nchan
		if not channel_axes.has_key(iset) or len(channel_axes[iset]) != nchan:
			channel_axes[iset] = cdms2.createAxis(npy.arange(nchan))
			channel_axes[iset].id = '%s_channel' % name
			channel_axes[iset].long_name = '%s channels'% name.upper()
			self._check_dataset_tag_('_%s_channel_axes'% name, iset, **kwargs)
		return channel_axes[iset]

	def _svd_channel_axis_(self,iset):
		"""Get the SVD channel axis for one dataset"""
		return _channel_axis_(iset, 'svd', svd=True)

	def _mssa_channel_axis_(self,iset):
		"""Get the MSSA channel axis for one dataset"""
		return _channel_axis_(iset, 'mssa')

	def _mssa_window_axis_(self,iset, update=False):
		"""Get the MSSA window axis for one dataset"""
		if not has_cdat_support:
			self._mssa_window_axes[iset] = self._window[iset]
		elif not self._mssa_window_axes.has_key(iset) or \
			len(self._mssa_window_axes[iset]) != self._window[iset]:
			self._mssa_window_axes[iset] = cdms2.createAxis(npy.arange(self._window[iset]))
			self._mssa_window_axes[iset].id = 'mssa_window'
			self._mssa_window_axes[iset].long_name = 'MSSA window time'
			self._check_dataset_tag_('_mssa_window_axes',iset)
		return self._mssa_window_axes[iset]

	def _mssa_pctime_axis_(self,iset,idata=0):
		"""Get the MSSA PCs time axis for one dataset"""
		nt = self._nt[iset] - self._window[iset] + 1
		if not has_cdat_support:
			self._mssa_pctime_axes[iset] = nt
		elif not self._mssa_pctime_axes.has_key(iset) or \
			len(self._mssa_pctime_axes[iset]) != nt:
			self._mssa_pctime_axes[iset] = cdms2.createAxis(npy.arange(nt))
			self._mssa_pctime_axes[iset].id = 'mssa_pctime'
			self._mssa_pctime_axes[iset].long_name = 'MSSA PC time'
			self._check_dataset_tag_('_mssa_pctime_axes',iset)
			taxis = self._time_axis_(iset,idata)
			if hasattr(taxis,'units') and taxis.units.split()[0].lower() in \
				['seconds','minutes','hours','days','months','years']:
				self._mssa_pctime_axes[iset].units = taxis.units.split()[0].lower() + ' since 0001-01-01'
				self._mssa_pctime_axes[iset].designateTime()
		return self._mssa_pctime_axes[iset]
		
	def _check_dataset_tag_(self,name,iset,key=None,long_name=True,id=True, svd=False):
		"""Mark some attributes as specific to a dataset (only if there are more then one dataset)
			iset:: ID of the dataset
			key:: A dictionary key to select the dataset [default: None]
			long_name:: Mark the long name [defualt: True]
			id:: Mark the id [default: True]
			svd: Mark using 'left' or 'right'
		"""
		if self._ndataset > 1:
			targetset = getattr(self,name)
			if key is not None:
				targetset = targetset[key]
			target = targetset[iset]
			if not cdms2_isVariable(target): return
			if svd:
				svdtag = ['left', 'right'][iset]
			if id: 
				if svd:
					target.id  = '%s_%s'%(svdtag, target.id)
				else:
					target.id += '_set%i'%iset
			if long_name:
				if svd:
					target.long_name += ' for %s dataset'%svdtag
				else:
					target.long_name += ' for dataset #%i'%iset
					
	def _cdat_ev_(self, iset,  ev, analysis_type, relative, cumsum, id=None, long_name=None, standard_name=None,  atts=None):
		"""Format eigenvalues as CDAT variable"""
		if not has_cdat_support: return ev
		ev = cdms2.createVariable(ev)
		if id is None:
			id = analysis_type.lower()+'_ev'
		if long_name is None:
			long_name = []
			if cumsum:
				id += '_cumsum'
				long_name.append('cumulative')
			if relative: 
				id += '_rel'
				long_name.append('relative')
		ev.id = ev.name = id
		atts = self._stack_info[iset]['atts'][0]
		if isinstance(long_name, list):
			long_name.append(analysis_type.upper()+' eigen values')
			ev.long_name = ' '.join(long_name).title()
			if atts.has_key('long_name'):
				ev.long_name += ' of '+atts['long_name']
		else:
			ev.long_name = long_name
		ev.setAxisList([self._mode_axis_(analysis_type.lower(), iset)])
		if standard_name is not False:
			if standard_name is None:
				ev.standard_name = 'eigen_values_of_'+analysis_type.lower()
			else:
				ev.standard_name = standard_name
		if relative:
			ev.units = '% of total variance'
		return ev
					


	def _norm_(self,iset,idata):
		"""Get the normalization factor of one subdataset of one dataset"""
		return self._stack_info[iset]['norms'][idata]
		
	def norms(self, iset, idata=None):
		"""Get the normalization factor of a dataset"""
		if idata is None:
			return self._stack_info[iset]['norms']
		return self._stack_info[iset]['norms'][idata]
		
	def _type_(self,iset,idata):
		"""Get 'numpy', 'MA' or 'cdms' for one subdataset of one dataset"""
		return self._stack_info[iset]['types'][idata]
		
	def _cdat_inside_(self, iset, idata=None):
		"""Check if a data var or a dataset has CDAT support"""
		# Not at all
		if not has_cdat_support: return False
		# Single var
		if idata is not None:
			return self._type_(iset, idata) == 'MV2'
		# Full dataset == at least one var
		for vt in self._stack_info[iset]['types']:
			if vt == 'MV2': return True
		return False

#	def _changed_param_(self,old,param):
#		"""Check if a parameter is changed for all datasets.
#		Return a list of booleans.
#		@param old: Dictionary of name and value of parameters
#		@param param: Parameter name.
#		"""
#		if isinstance(old[param],(list,tuple)):
#			return [old[param][iset] != getattr(self,'_'+param)[iset] for iset in xrange(self._ndataset)]
#		else:
#			return old[param] != getattr(self,'_'+param)
		
	def _update_(self, analysis_type=None, **kwargs):
		"""Initialize, update and check statistical paremeters.
		A value of None is converted to an optimal value.
		Analyses are re-ran if needed by checking dependencies.
		"""
		# Filter parameter list according to analysis_type
		running = isinstance(analysis_type, str)
		if running:
			for param in kwargs.keys():
				if param not in self._params[analysis_type]:
					del kwargs[param]
				
		req_params = kwargs.keys()
			
		# Initialize old values and defaults changed to False
		old = {}
		changed = {}
		init_all = [None]*self._ndataset
		for param in self._all_params:
			#print 'check', param
#			if not req_params: continue
			# Get old values , defaults to None and set new value
#			if kwargs.has_key(param):
			if param == 'nsvd':  # Single value for all datasets
				changed[param] = False
				old[param] = getattr(self,'_'+param,None)
				setattr(self,'_'+param,SpAn._check_length_(kwargs.pop(param, old[param]),0,None))
			else:
				changed[param] = [False]*self._ndataset
				old[param] = getattr(self,'_'+param,list(init_all))
				setattr(self,'_'+param,SpAn._check_length_(kwargs.pop(param, old[param]),self._ndataset,None))
#				print 'cur', param, getattr(self, '_'+param), changed[param]
#		if not req_params: return changed
		if not running: return changed

		# Number of PCA modes		
#		if 'npca' in req_params:
		for iset in xrange(self._ndataset):
			if self._npca[iset] is None:
				# Guess a value
				if self._prepca[iset] is not None:
					self._npca[iset] = self._prepca[iset]
				elif iset:
					self._npca[iset] = self._npca[iset-1] # From last dataset
				else:
					self._npca[iset] = SpAn._npca_default # Default value
			if self._prepca[iset] is not None:
				self._npca[iset] = max(self._npca[iset], self._prepca[iset]) # Min
			self._npca[iset] = npy.clip(self._npca[iset],1,min(SpAn._npca_max,self._ns[iset],self._nt[iset])) # Max
			
		# Number of pre-PCA modes before MSSA and SVD
#		if 'prepca' in req_params:
		for iset in xrange(self._ndataset):
			if self._prepca[iset] is None: # Default: pre-PCA needed over max (for MSSA and SVD)
				self._prepca[iset] = min(self._ns[iset], self._ns[iset]) > SpAn._npca_max
				if not self._quiet and self._prepca[iset] and analysis_type in ['mssa', 'svd'] :
					print '[mssa/svd] The number of valid points of one of the datasets is greater than %i, so we perform a pre-PCA'%SpAn._npca_max
			if self._prepca[iset] is True: # Defaults to the number of PCA modes
				self._prepca[iset] = self._npca[iset]
			elif self._prepca[iset]: # Max number of prepca modes is number of points
				self._prepca[iset] = min(self._prepca[iset], self._ns[iset], self._nt[iset])
			if self._prepca[iset] == 0:
				self._prepca[iset] = False
			
		# Dependency rules between prepca and npca
		for iset in xrange(self._ndataset):
			if self._prepca[iset] and self._npca[iset] < self._prepca[iset]:
				if not self._quiet  and self._prepca[iset]:
						print 'The number of pre-PCA modes (%i) for dataset #%i is lower than the number of PCA modes (%i), so we adjust the latter.' % (self._prepca[iset],iset,self._npca[iset])
				self._npca[iset] = self._prepca[iset]
			
		# Window extension of MSSA
#		if 'window' in req_params:
		for iset in xrange(self._ndataset):
			if self._window[iset] is None: # Initialization
				self._window[iset] = int(self._nt[iset]*SpAn._window_default)
			self._window[iset] = npy.clip(self._window[iset],1,max(1,self._nt[iset]))
			
		# Number of MSSA modes
		for iset in xrange(self._ndataset):
#			if 'nmssa' not in req_params and not changed['prepca'][iset]: continue
#			if not changed['prepca'][iset]: continue
			if self._nmssa[iset] is None: # Initialization
				# Guess a value
				if iset:
					self._nmssa[iset] = self._nmssa[iset-1] # From last dataset
				else:
					self._nmssa[iset] = SpAn._nmssa_default # Default value
			if self._prepca[iset]:
				nchanmax = self._prepca[iset] # Input channels are from pre-PCA
			else:
				nchanmax = self._ns[iset] # Input channels are from real space
			self._nmssa[iset] = npy.clip(self._nmssa[iset],1,
				min(SpAn._nmssa_max,nchanmax*self._window[iset])) # Max
			
		# Number of SVD modes (special case)
		if self._nsvd is None: # Initialization
			self._nsvd = SpAn._nsvd_default # Default value
		for iset in xrange(self._ndataset): # Check values
#			if 'nsvd' not in req_params and not changed['prepca'][iset]: continue
			if not changed['prepca'][iset]: continue
			if self._prepca[iset]:
				nchanmax = self._prepca[iset] # Input channels are from pre-PCA
			else:
				nchanmax = self._ns[iset] # Input channels are from real space
			self._nsvd = npy.clip(self._nsvd,1, min(SpAn._nsvd_max,nchanmax)) # Max
			
#		# Check what changed
#		for param in self._all_params:
#			changed[param] = self._changed_param_(old,param)
			
		# Re-run analyses when needed
#		if not kwargs: return changed # Just initializations (dry run to prevent not ending loop)
		changed['nsvd'] = old['nsvd'] != self._nsvd
		runsvd = False
		for iset in xrange(self._ndataset):
			
			# Check what changed
			for param in self._all_params:
				if param !=  'nsvd':
					changed[param][iset] = old[param][iset] != getattr(self,'_'+param)[iset]
			
			# Analyses
			# - PCA
			if (analysis_type == 'pca' or self._prepca[iset]) and \
				(self._pca_raw_eof.has_key(iset) and changed['npca'][iset]):
				print 'Rerunning PCA'
				self.pca(iset=iset)
					
			# - MSSA
			if analysis_type == 'mssa' and \
				(self._mssa_raw_eof.has_key(iset) and
					(changed['nmssa'][iset] or changed['window'][iset] or 
					(self._prepca[iset] and changed['prepca'][iset]))):
				print 'Rerunning MSSA'
				self.mssa(iset=iset)
			
			# - SVD
			if not runsvd and analysis_type == 'svd' and (changed['nsvd'] or \
				(self._svd_raw_eof.has_key(iset) and
					(self._prepca[iset] and changed['prepca'][iset]))):
				runsvd = True
		if runsvd:
			#FIXME: MUST NOT RERUN SVD
#			print 'Rerunning SVD'
			self.svd()
				
		# Inform about which params have been modified for each dataset
		return changed

	def _check_isets_(self,iset):
		"""Check if an iset is a valid dataset.
		It can be a list, and it is returned as a list.
		if an iset is invalid, it is removed from the output list.
		"""
		if iset is None: return range(self._ndataset)
		if iset == 'left':
			iset = 0
		elif iset == 'right':
			iset = 1
		if iset < 0 or iset >= self._ndataset:
			warn('Invalid dataset id: %i. Valid id are < %i'%(iset,self._ndataset))
		else:
			return [iset]

	def _check_shape_(self, inputs, fillvalue):
		"""Return input as datasets (tree) *shape*"""
		imap = self._input_map
		if isinstance(imap, int):
			return [SpAn._check_length_(inputs, max(1, imap), fillvalue), ]
		inputs = SpAn._check_length_(inputs,len(imap),fillvalue)
		for iset, im in enumerate(imap):
			inputs[iset] = SpAn._check_length_(inputs[iset], max(1, im), fillvalue)
		return inputs


	#################################################################
	## PCA
	#################################################################
	@_filldocs_
	def pca(self,iset=None,**kwargs):
		""" 
		Principal Components Analysis (PCA)
		
		It is called everytime needed by :meth:`pca_eof`, :meth:`pca_pc`, :meth:`pca_ev` and :meth:`pca_rec`.
		Thus, since results are stored in cache, it not necessary call it explicitly.

		:Parameters:
			%(npca)s
			%(iset)s
		"""

		# Check on which dataset to operate
		isets = self._check_isets_(iset)

		# Update params
		self._update_('pca', **kwargs)

		# Loop on datasets
		for iset in isets:
			
			# Check if old results can be used when npca is lower
			if getattr(self,'_pca_raw_pc').has_key(iset) and \
				getattr(self,'_pca_raw_pc')[iset].shape[-1] > self._npca[iset]:
				continue
			
			# Remove old results
			for att in 'raw_eof','raw_pc','raw_ev','ev_sum':
				dic = getattr(self,'_pca_'+att)
				if dic.has_key(iset): del dic[iset]

			# Compute PCA
			pdata = self._pdata[iset]
			if pdata.ndim == 1: # One single channel, so result is itself
				raw_eof = npy.ones(1,dtype=pdata.dtype)
				raw_pc = pdata
				raw_ev = raw_pc.var()
				ev_sum = ev

			else: # Several channels
				weights = self._stack_info[iset]['weights']
				raw_eof,raw_pc,raw_ev,ev_sum = spanlib_fort.pca(pdata,self._npca[iset],weights,-1)	

			# Save results
			self._pca_raw_pc[iset] = raw_pc
			self._pca_raw_eof[iset] = raw_eof
			self._pca_raw_ev[iset] = raw_ev
			self._pca_ev_sum[iset] = ev_sum
			
			# Delete formmated variables
			for vtype in 'pc', 'eof':
				vfmt = getattr(self, '_pca_fmt_'+vtype)
				if vfmt.has_key(iset): del vfmt[iset]
			gc.collect()

		self._last_analysis_type = 'pca'


	@_filldocs_
	def pca_eof(self, iset=None, scale=False, raw=False, **kwargs):
		"""Get EOFs from PCA analysis

		:Parameters:
			%(scale)s
			%(raw)s
			%(iset)s
			
		:PCA parameters:
			%(npca)s
			
		:Returns:
			Arrays with shape ``(npca,...)``
		"""
	
		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		changed =  self._update_('pca', **kwargs)

		# Of, let's format the variables
		fmt_eof = {}
		for iset in isets:
			
			# Operate only on selected datasets
			if isets is not None and iset not in isets: continue
			
			# EOF already available 
			if self._pca_fmt_eof.has_key(iset):
				fmt_eof[iset] = self._pca_fmt_eof[iset]
				continue
				
			# First PCA analysis?
			if not self._pca_raw_eof.has_key(iset): self.pca(iset=iset)
				
			# Get raw data back to physical space
			#FIXME: add raw for fmt_eof
			self._pca_fmt_eof[iset] = \
				self._unstack_(iset, self._pca_raw_eof[iset][:, :self._npca[iset]],
					self._mode_axis_('pca',iset),  cpatts=False, remean=False)
			
			# Set attributes and scale
			for idata,eof in enumerate(self._pca_fmt_eof[iset]):
				
				# Attributes
				if cdms2_isVariable(eof):
					if not self._stack_info[iset]['ids'][idata].startswith('variable_'):
						eof.id = self._stack_info[iset]['ids'][idata]+'_pca_eof'
					else:
						eof.id = 'pca_eof'
					eof.name = eof.id
					eof.standard_name = 'empirical_orthogonal_functions_of_pca'
					eof.long_name = 'PCA empirical orthogonal functions'
					atts = self._stack_info[iset]['atts'][idata]
					if atts.has_key('long_name'):
						eof.long_name += ' of '+atts['long_name']
					print 'scale', scale
					if scale and atts.has_key('units'):
						eof.units = atts['units']
					
				# Scaling
				if scale:
					if scale is True: # Std dev of EOF is sqrt(ev)
						scale = npy.sqrt(self._pca_raw_ev[iset]*(self.ns(iset)-1))
						for imode in xrange(eof.shape[0]):
							eof[imode] *= scale[imode]
					else:
						eof *= scale
					
			fmt_eof[iset] = self._pca_fmt_eof[iset]

		return self._demap_(fmt_eof, grouped=raw)		


	@_filldocs_
	def pca_pc(self, iset=None, scale=False, **kwargs):
		"""Get PCs from current PCA decomposition
		
		:Parameters:
			%(iset)s

		:PCA parameters:
			%(npca)s
			
		:Returns:
			Arrays with the shape ``(npca,nt)``
		"""
		# Check on which dataset to operate
		isets = self._check_isets_(iset)

		# Update params
		self._update_('pca', **kwargs)
		
		# Of, let's format the variable
		fmt_pc = {}
		for iset in isets:
			
			# PC already available 
			if self._pca_fmt_pc.has_key(iset):
				fmt_pc[iset] = self._pca_fmt_pc[iset]
				continue
			
			# First PCA analysis?
			if not self._pca_raw_pc.has_key(iset): self.pca(iset=iset)

			# Format the variable
			pc = npy.asarray(self._pca_raw_pc[iset][:,:self._npca[iset]].transpose(),order='C')
			if self._cdat_inside_(iset):
				pc = cdms2.createVariable()
				pc.setAxis(0, self._mode_axis_('pca',iset))
				pc.setAxis(1, self._time_axis_(iset,0))
				pc.id = pc.name = 'pca_pc'
				pc.standard_name = 'principal_components_of_pca'
				pc.long_name = 'PCA principal components of '
				atts = self._stack_info[iset]['atts'][0]
				if self._ndata[iset] == 1 and  atts.has_key('long_name'): 
					pc.long_name += atts['long_name']
	#			else:
	#				pc.long_name += 'dataset %i'%iset
				if scale and (self._ndata[iset] == 1 or npy.allclose(self.norms(iset), 1.)) and atts.has_key('units'):
					pc.units = atts['units']
				
			
			fmt_pc[iset] = self._pca_fmt_pc[iset] = pc
			self._check_dataset_tag_('_pca_fmt_pc',iset)

		return self._demap_(fmt_pc, grouped=True)		

	def pca_ec(self, xeof=None, xpc=None, iset=None, scale=False, **kwargs):
		
		# Check on which dataset to operate
		isets = self._check_isets_(iset)
		
		need_pca = xeof is None or xpc is None
		if need_pca:
			self._update_('pca', **kwargs)
		
		# Remap
		if xeof is not None:
			xeof = self._remap_(xeof)
		if xpc is not None:
			xpc = self._remap_(xpc,  grouped=True)
			
		# Loop on datasets
		for 	iset in isets:
			
			# PC already available 
			if self._pca_fmt_pc.has_key(iset):
				fmt_pc[iset] = self._pca_fmt_pc[iset]
				continue
			
			# First PCA analysis?
			if need_pca and not self._pca_raw_pc.has_key(iset): self.pca(iset=iset)
			
			if xeof is None:
				raw_eof = self._pca_raw_eof.has_key(iset)
			else:
				stack = self._core_stack_(xeof, dnorms=self._stack_info[0]['norms'], 
					dmasks=self._stack_info[0]['masks'], dorders=self._stack_info[0]['orders'], 
					dweights=self._stack_info[0]['weights'])
				raw_eof = stack['pdata']

	@_filldocs_
	def pca_ev(self,iset=None,relative=False,sum=False,cumsum=False,**kwargs):
		"""Get eigen values from current PCA decomposition

		:Parameters:
		  %(relative)s
		  %(sum)s
		  %(cumsum)s
		  %(iset)s
		
			
		:PCA parameters:
			%(npca)s
			
		:Returns:
			Arrays with shape ``(npca,)`` or a float
		"""

		# Check on which dataset to operate
		isets = self._check_isets_(iset)

		# Update params
		self._update_('pca', **kwargs)

		# Loop on dataset
		res = {}
		for iset in isets:
			
			# First PCA analysis?
			if not self._pca_raw_eof.has_key(iset): self.pca(iset=iset)
				
			# We only want the sum
			if sum:
				res[iset] = self._pca_ev_sum[iset]
				continue
				
			# Data
			ev = self._pca_raw_ev[iset][:self._npca[iset]]
			if cumsum:
				ev = raw_ev.cumsum()
			if relative: 
				ev = 100.*ev/self._pca_ev_sum[iset]

			# Format the variable
			if self._cdat_inside_(iset):
				
				id = 'pca_ev'
				long_name = []
				if cumsum:
					id += '_cumsum'
					long_name.append('cumulative')
				if relative: 
					id += '_rel'
					long_name.append('relative')
				ev = cdms2.createVariable(ev)
				ev.id = ev.name = id
				long_name.append('PCA eigen values')
				ev.long_name = ' '.join(long_name).title()+' of '
				ev.setAxisList([self._mode_axis_('pca',iset)])
				ev.standard_name = 'eigen_values_of_pca'
				atts = self._stack_info[iset]['atts'][0]
				if self._ndata[iset] == 1 and atts.has_key('long_name'):
					ev.long_name += atts['long_name']
				else:
					ev.long_name += 'dataset %i'%iset
				if relative:
					ev.units = '% of total variance'
				elif (self._ndata[iset] == 1 or npy.allclose(self.norms(iset), 1.)) and atts.has_key('units'):
					ev.units = atts['units']
					for ss in ['^','**',' ']:
						if ev.units.find(ss) != -1:
							ev.units = '(%s)^2' % ev.units
							break
							
			res[iset] = ev

		return self._demap_(res, grouped=True)		

	@_filldocs_
	def pca_rec(self, iset=None, modes=None, raw=False, **kwargs):
		"""Reconstruct a set of modes from PCA decomposition

		:Parameters:
			%(modes)s
			%(raw)s
			%(iset)s
			
		:PCA parameters:
			%(npca)s
			
		:Returns:
			Arrays with the same shape as input arrays.
		"""
		
		# Check on which dataset to operate
		isets = self._check_isets_(iset)
			
		# Update params
		self._update_('pca', **kwargs)

		# Loop on datasets
		pca_fmt_rec = {}
		for iset in isets:
			
			# First PCA analysis?
			if not self._pca_raw_pc.has_key(iset): self.pca(iset=iset)

			# Get raw data back to physical space
			reof = self._pca_raw_eof[iset][:,:self._npca[iset]]
			rpc = self._pca_raw_pc[iset][:,:self._npca[iset]]
			raw_rec,smodes = self._project_(reof,rpc,iset,modes)
			#FIXME: add raw for pca_rec
			pca_fmt_rec[iset] = self._unstack_(iset,raw_rec,self._time_axis_(iset))
			del  raw_rec
			
			# Set attributes and scale
			for idata,rec in enumerate(pca_fmt_rec[iset]):
				if not cdms2_isVariable(rec): continue
#				rec[:] *= self._norm_(iset,idata) # Scale
				if not self._stack_info[iset]['ids'][idata].startswith('variable_'):
					rec.id = self._stack_info[iset]['ids'][idata]+'_pca_rec'
				else:
					rec.id = 'pca_rec'
				rec.name = rec.id
#				if modes is not None:
				rec.id += smodes
#				else:
#					rec.modes = '1-%i'%self._npca[iset]
				rec.modes = smodes
				rec.standard_name = 'recontruction_of_pca_modes'
				rec.long_name = 'Reconstruction of PCA modes: '+smodes
				atts = self._stack_info[iset]['atts'][idata]
				if atts.has_key('long_name'):
					rec.long_name += ' of '+atts['long_name']
				if atts.has_key('units'):
					rec.units = atts['units']
					
		return self._demap_(pca_fmt_rec, grouped=raw)	
	

	#################################################################
	# MSSA
	#################################################################

	@_filldocs_
	def mssa(self,iset=None, **kwargs):
		""" MultiChannel Singular Spectrum Analysis (MSSA)

		It is called everytime needed by :meth:`mssa_eof`, :meth:`mssa_pc`, :meth:`mssa_ev` and :meth:`mssa_rec`.
		Thus, since results are stored in cache, it not necessary call it explicitly.

		:Parameters:
			%(nmssa)s
			%(window)s
			%(prepca)s
			%(iset)s
		"""

		# Check on which dataset to operate
		isets = self._check_isets_(iset)

		# Parameters
		self._update_('mssa', **kwargs)

		# Loop on datasets
		for iset,pdata in enumerate(self._pdata):

			# Operate only on selected datasets
			if isets is not None and iset not in isets: continue
			
			# Check if old results can be used when nmssa is lower
			if getattr(self,'_mssa_raw_pc').has_key(iset) and \
				getattr(self,'_mssa_raw_pc')[iset].shape[-1] > self._nmssa[iset]:
				continue
			
			# Remove old results
			for att in 'raw_eof','raw_pc','raw_ev','ev_sum':
				dic = getattr(self,'_mssa_'+att)
				if dic.has_key(iset): del dic[iset]

			# Compute MSSA
			if self._prepca[iset]: # Pre-PCA case
			
				# PCA
				if not self._pca_raw_pc.has_key(iset): self.pca(iset=iset)
				
				# MSSA
				raw_eof, raw_pc, raw_ev, ev_sum = \
				  spanlib_fort.mssa(self._pca_raw_pc[iset][:, :self._prepca[iset]].transpose(), 
				  self._window[iset], self._nmssa[iset])
				  
			else: # Direct MSSA case
				raw_eof, raw_pc, raw_ev, ev_sum = \
				  spanlib_fort.mssa(pdata, self._window[iset], self._nmssa[iset])

			# Save results
			self._mssa_raw_pc[iset] = raw_pc
			self._mssa_raw_eof[iset] = raw_eof
			self._mssa_raw_ev[iset] = raw_ev
			self._mssa_ev_sum[iset] = ev_sum

			# Delete formmated variables
			for vtype in 'pc', 'eof':
				vfmt = getattr(self, '_mssa_fmt_'+vtype)
				if vfmt.has_key(iset): del vfmt[iset]
				
		self._last_analysis_type = 'mssa'
		gc.collect()

	@_filldocs_
	def mssa_eof(self, iset=None, scale=False, raw=False,**kwargs):
		"""Get EOFs from MSSA analysis

		:Parameters:
			%(scale)s
			%(raw)s
			%(iset)s
			
		:MSSA parameters:
			%(nmssa)s
			%(window)s
			%(prepca)s
			
		:Returns:
			Arrays with shape ``(nmssa,nt-window+1,...)``
		"""

		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		self._update_('mssa', **kwargs)

		# Of, let's format the variable
		fmt_eof = {}
		for iset in isets: # (window*nchan,nmssa)
		
			# EOF already available 
			if self._mssa_fmt_eof.has_key(iset):
				fmt_eof[iset] = self._mssa_fmt_eof[iset]
				continue
				
			# No analyses performed?
			if not self._mssa_raw_eof.has_key(iset): self.mssa(iset=iset)
			raw_eof = self._mssa_raw_eof[iset]
			nm = self._nmssa[iset] ; nw = self._window[iset]
			nl = self._mssa_raw_eof[iset].shape[0]/nw
			raw_eof = self._mssa_raw_eof[iset].reshape((nl, nw, nm))
			
			if raw: # Do not go back to physical space
				self._mssa_fmt_eof[iset] = [cdms2.createVariable(raw_eof.T)]
				self._mssa_fmt_eof[iset][0].setAxisList(
					[self._mode_axis_('mssa',iset),self._mssa_channel_axis_(iset)])
					
			else: # Get raw data back to physical space
#				raw_eof = raw_eof.swapaxes(1,2).reshape((nl, nm*nw),order='F')
				raw_eof = npy.ascontiguousarray(raw_eof)
				raw_eof.shape = (nl, nm*nw)
				
				if not self._prepca[iset]: # No pre-PCA performed
					self._mssa_fmt_eof[iset] = self._unstack_(iset,raw_eof, 
						(self._mode_axis_('mssa',iset),self._mssa_window_axis_(iset)), 
						cpatts=False, remean=False)
						
				else:
					proj_eof,smodes = self._project_(self._pca_raw_eof[iset],raw_eof.T,iset, nt=nm*nw)
	#					npy.swapaxes(raw_eof,0,1).reshape((nw*nc,nm),order='F'),iset, nt=nw*nm)
					self._mssa_fmt_eof[iset] = self._unstack_(iset,proj_eof,
						(self._mode_axis_('mssa',iset),self._mssa_window_axis_(iset)), 
						cpatts=False, remean=False)
					
			# Set attributes
			for idata,eof in enumerate(self._mssa_fmt_eof[iset]):
				
				# Attributes
				if cdms2_isVariable(eof):
					if not raw and not self._stack_info[iset]['ids'][idata].find('variable_'):
						eof.id = self._stack_info[iset]['ids'][idata]+'_mssa_eof'
					else:
						eof.id = 'mssa_eof'
					eof.name = eof.id
					eof.standard_name = 'empirical_orthogonal_functions_of_mssa'
					eof.long_name = 'MSSA empirical orthogonal functions'
					if not raw:
						atts = self._stack_info[iset]['atts'][idata]
						if atts.has_key('long_name'):
							eof.long_name += ' of '+atts['long_name']
						if scale and atts.has_key('units'):
							eof.units = atts['units']
					
				# Scaling
				if scale:
					if scale is True:
						scale = npy.sqrt(self._mssa_raw_ev[iset])*nl*nw
					eof[:] *= scale
					
			fmt_eof[iset] = self._mssa_fmt_eof[iset]
			
		gc.collect()
		return self._demap_(fmt_eof, grouped=raw)

	@_filldocs_
	def mssa_pc(self, iset=None, **kwargs):
		"""Get PCs from MSSA analysis
		
		:Parameters:
			%(iset)s

		:MSSA parameters:
			%(nmssa)s
			%(window)s
			%(prepca)s
			
		:Returns:
			Arrays with the shape ``(nmssa,nt)``
		"""

		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		self._update_('mssa', **kwargs)

		# Of, let's format the variable
		fmt_pc = {}
		for iset in isets:
			
			# PC already available 
			if self._mssa_fmt_pc.has_key(iset):
				fmt_pc[iset] = self._mssa_fmt_pc[iset]
				continue
				
			# No analyses performed?
			if not self._mssa_raw_pc.has_key(iset): self.mssa(iset=iset)
						
			# Format the variable
			pc = npy.asarray(self._mssa_raw_pc[iset][:,:self._nmssa[iset]].transpose(),order='C')
			if self._cdat_inside_(iset):
				
				pc = cdms2.createVariable(pc)
				pc.setAxis(0,self._mode_axis_('mssa',iset))
				pc.setAxis(1,self._mssa_pctime_axis_(iset))
				pc.id = pc.name = 'mssa_pc'
				pc.standard_name = 'principal_components_of_mssa of '
				pc.long_name = 'MSSA principal components'
				atts = self._stack_info[iset]['atts'][0]
				if self._ndata[iset] == 1 and atts.has_key('long_name'): 
					pc.long_name += atts['long_name']
	#			else:
	#				pc.long_name += 'of dataset %i'%iset
				#if (self._ndata[iset] == 1 or npy.allclose(self.norms(iset), 1.)) and atts.has_key('units'):
				#	pc.units = atts['units']
			fmt_pc[iset] = self._mssa_fmt_pc[iset] = pc
			self._check_dataset_tag_('_mssa_fmt_pc',iset)

		return self._demap_(fmt_pc, grouped=True)
			

	@_filldocs_
	def mssa_ev(self, iset=None, relative=False, sum=False, cumsum=False, mctest=False, mcnens=50, mcqt=90, **kwargs):
		"""Get eigen values from current MSSA decomposition

		:Options:
		
		  %(relative)s
		  %(sum)s
		  %(cumsum)s
		  %(iset)s
		
			
		:MSSA options:
		
			%(nmssa)s
			%(window)s
			%(prepca)s
			- *mctest*: bool
				If ``True``, perfom a Monte-Carlo test if significance
				eigenvalues against red noise, following [Allen_and_Smith_96]_.
			- *mcnens*: int
				Size of the ensemble fo compute quantiles.
			- *mcqt*: float|int
				Value of the higher quantile in %% (the lower is ``100-mcqt``).
			
		:Returns:
			
			- Arrays with shape ``(nmssa,)`` or a float
			- Optionally, ``(ev, evmin, evmax)`` if ``mctest is True``
			
		:Basic example:
		
		>>> span = SpAn(data)
		>>> ev = span.mssa_ev()
		>>> ev_sum = span.mssa_ev(sum=True)
		>>> ev_cumsum = span.mssa_ev(cumsum=True)
		>>> ev_rel = span.mssa_ev(relative=True)
		
		:Monte-Carlo example:
		
		>>> span = SpAn(data)
		>>> ev, mc_evmin, mc_evmax = span.mssa_ev(mctest=True, mcqt=95, mcnens=100)
		>>> print (ev<evmin)|(ev>evmax)
		"""

		# Check on which dataset to operate
		isets = self._check_isets_(iset)

		# Update params
		self._update_('mssa', **kwargs)

		# Loop on dataset
		res = {}
		for iset in isets:
			
			# No analyses performed?
			if not self._mssa_raw_eof.has_key(iset): self.mssa(iset=iset)

			# We only want the sum
			if sum:
				res[iset] = self._mssa_ev_sum[iset]
				continue
				
			# Data
			ev = self._mssa_raw_ev[iset][:self._nmssa[iset]]
			if cumsum:
				ev = ev.cumsum()
			if relative: 
				ev = 100.*ev/self._mssa_ev_sum[iset]
			ev = [ev, ]
			
			# Mont-Carlo test
			if mctest:
				
				# Get reference data
				if self._prepca[iset]:
					data = self._pca_raw_pc[iset]
				else:
					data = self._pdata[iset]
				data = data-data.mean(axis=0)

				# Inits
				rn = RedNoise(data) # red noise generator
				nmssa = self._nmssa[iset]
				mcev = npy.zeros((mcnens, nmssa))
				
				# Generate and ensemble of surrogate data
				for iens in xrange(mcnens):
					
					# Create a sample red noise (nt,nchan)
					red_noise = rn.sample()
					
					# Block-covariance matrix (nchan*nwindow,nchan*nwindow,)
					cov = spanlib_fort.stcov(red_noise.T, self.window(iset))
					del red_noise
					
					# Fake eigen values
					ce = npy.dot(cov, self._mssa_raw_eof[iset]) ; del cov
					evmat = npy.dot(self._mssa_raw_eof[iset].T, ce) ; del ce
					mcev[iens] = npy.diag(evmat)[:nmssa] ; del evmat
					
				mcev.sort(axis=0) # Sort by value inside ensemble
				
				# Confidence interval
				# - min
				imini, iminf = divmod((1-mcqt/100.)*(mcnens-1), 1)
				evmin = mcev[int(imini)]
				if int(imini) != mcnens-1:
					evmin += (1-iminf)*mcev[int(imini)] + iminf*mcev[int(imini)+1]
				# - max
				imaxi, imaxf = divmod(mcqt/100.*(mcnens-1), 1)
				evmax = mcev[int(imaxi)]
				if int(imaxi) != mcnens-1:
					evmax += (1-imaxf)*mcev[int(imaxi)] + imaxf*mcev[int(imaxi)+1]
				ev.extend([evmin, evmax])

			# Format the variables
			if self._cdat_inside_(iset):
				
				for i, e in enumerate(ev):
					ev[i] = self._cdat_ev_(iset, e,  'mssa', relative, cumsum)
				if mctest:
					ev[1].id += '_mclow'
					ev[1].long_name += ': lower bound of MC confidence interval (%g%%)'%mcqt 
					ev[2].id += '_mchigh'
					ev[2].long_name += ': upper bound of MC confidence interval (%g%%)'%(100-mcqt)
					
			res[iset] = ev[0] if not mctest else ev

		return self._demap_(res, grouped=True)		

	@_filldocs_
	def mssa_mctest(self, iset=None, nens=20, conf=95., **kwargs):
		"""Monte-Carlo test of significance of MSSA eigenvalues
		
		We have:
		
		.. math:: c_l = \frac{\alpha^2 \gamma^l}{1-\gamma^2}
		
		Therefore:
		
		.. math:: \gamma = c_1 / c_2
		
		and
		
		.. math:: \alpha = \sqrt{\c_0 (1-\gamma^2)}
		"""

		# Check on which dataset to operate
		isets = self._check_isets_(iset)

		# Update params
		self._update_('mssa', **kwargs)

		# Loop on dataset
		evs = {}
		for iset in isets:
			
			# No analyses performed?
			if not self._mssa_raw_eof.has_key(iset): self.mssa(iset=iset)
						
			return evmin, evmax
			


	@_filldocs_
	def mssa_rec(self, iset=None, modes=None, raw=False, phases=False, **kwargs):
		"""Reconstruction of MSSA modes
		
		:Parameters:
			%(iset)s
			%(modes)s
			%(raw)s
			*phases* : ``False`` | ``True`` | int
				Return phases composites of the reconstructed field.
				By default, 8 phases are computed. You can psecify
				the number of phases by passing an integer.
			
		:MSSA parameters:
			%(nmssa)s
			%(window)s
			%(prepca)s
			
		:Returns:
			Arrays with the same shape as input arrays.
		"""
		
		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		self._update_('mssa', **kwargs)
		if isinstance(phases, int):
			self._nphase = phases
		elif phases is not False:
			phases = self._nphase
		
		# Loop on datasets
		mssa_fmt_rec = {}
		for iset in isets:

			# No analyses performed?
			if not self._mssa_raw_eof.has_key(iset): self.mssa(iset=iset)
			
			# Projection
			raw_rec,smodes = self._project_(self._mssa_raw_eof[iset],
				self._mssa_raw_pc[iset], iset, modes, nw=self._window[iset])
				
			# Phases composites
			if phases:
				raw_rec_t = phase_composites(raw_rec.T, phases, force_cdat=True)
				nt = raw_rec.shape[0]
				if self._cdat_inside_(iset):
					raw_rec = MV2.transpose(raw_rec_t)
					taxis = raw_rec.getAxis(1)
				else:
					raw_rec = raw_rec_t.asma().T
					taxis = nt
				del raw_rec_t
			else:
				taxis = self._time_axis_(iset)
				nt = len(taxis)
				
			# Get raw data back to physical space (nchan,nt)
			if not self._prepca[iset]: # No pre-PCA performed
				mssa_fmt_rec[iset] = self._unstack_(iset, raw_rec, taxis)
				
			elif raw: # Force direct result from MSSA
				if _cdat_inside_(iset):
					mssa_fmt_rec[iset] = [cdms2.createVariable(raw_rec.T)]
					mssa_fmt_rec[iset][0].setAxisList(0,[taxis,self._mode_axis_('mssa',iset)])
				else:
					mssa_fmt_rec[iset] = raw_rec.T
					
			else: # With pre-pca
				proj_rec, spcamodes = self._project_(self._pca_raw_eof[iset], 
					raw_rec.T, iset, nt=nt)
				mssa_fmt_rec[iset] = self._unstack_(iset, proj_rec, taxis)
			del  raw_rec
			
			# Set attributes
			for idata,rec in enumerate(mssa_fmt_rec[iset]):
				if not cdms2_isVariable(rec): continue
				if not self._stack_info[iset]['ids'][idata].startswith('variable_'):
					rec.id = self._stack_info[iset]['ids'][idata]+'_mssa_rec'
				else:
					rec.id = 'mssa_rec'
#				if modes is not None:
				rec.id += smodes #FIXME: do we keep it?
				rec.modes = smodes
#				else:
#					rec.modes = '1-%i'%self._nmssa[iset]
				rec.standard_name = 'recontruction_of_mssa_modes'
				rec.long_name = 'Reconstruction of MSSA modes'
				atts = self._stack_info[iset]['atts'][idata]
				if atts.has_key('long_name'):
					rec.long_name += ' of '+atts['long_name']
					
		return self._demap_(mssa_fmt_rec, grouped=raw)
	
	def mssa_phases(self, pair, iset=0, nphase=8, **kwargs):
		"""Build phase composites using an MSSA pair
		
		.. note::
		
			This method is special call to :meth:`mssa_rec`
			where ``nphase`` is not zero and ``modes`` are set
			to ``pair``.
		
		:Parameters:
		
			- *pair*: A tuple designating the pair of mode to
			  reconstruct. If a integer is given, this mode and the
			  the following are used.
			  
		:Options:
		
			- *iset*: the dataset to work on.
			- *nphase*: The number of phase composites.
			- Other options are passed to :meth:`mssa_rec`
			
		"""
		if isinstance(pair, int):
			pair = (pair, pair+1)
		
		# Dataset selection
		isets = self._check_isets_(iset)
		return self.mssa_rec(iset=iset, modes=pair, phases=nphase)

	#################################################################
	## SVD
	#################################################################
	@_filldocs_
	def svd(self,usecorr=False, largematrix=False, **kwargs):
		""" Singular Value Decomposition (SVD)

		It is called everytime needed by :meth:`svd_eof`, :meth:`svd_pc`, :meth:`svd_ev` and :meth:`svd_rec`.
		Thus, since results are stored in cache, it not necessary call it explicitly.

		:Parameters:
			%(nsvd)s
			%(prepca)s
			- *usecorr*: bool
				Use correlations instead of covariances.
			%(iset)s
		"""

		if self._ndataset<2:
			raise SpanlibError('Error you need at least (most) 2 datasets to run svd, otherwise use pca and mssa',  'svd')
		
		# Parameters
		self._update_('svd', **kwargs)

		# Loop on datasets
		for iset,pdata in enumerate(self._pdata[:2]):

			# Check if old results can be used when nsvd is lower
			if getattr(self,'_svd_raw_pc').has_key(iset) and \
				getattr(self,'_svd_raw_pc')[iset].shape[-1] > self._nsvd:
				continue
			
			# Remove old results
			for att in 'raw_eof','raw_pc','raw_ev','ev_sum':
				dic = getattr(self,'_svd_'+att)
				if isinstance(dic, dict) and dic.has_key(iset): 
					del dic[iset]

			# Prepare input to SVD
			if self._prepca[iset]: # Pre-PCA case
			
				# Pre-PCA
				if not self._pca_raw_pc.has_key(iset): self.pca(iset=iset)
				
				# svd
				data = self._pca_raw_pc[iset][:, :self._prepca[iset]].transpose()
				if iset == 0:
					left = data
					lweights = data[:, 0]*0.+1
				else:
					right = data
					rweights = data[:, 0]*0.+1

			else: # Direct svd case
				weights = self._stack_info[iset]['weights']
				if iset == 0:
					left = pdata
					lweights = weights

				else:
					right = pdata
					rweights = weights

		# Compute SVD
		raw_eof_left, raw_eof_right, raw_pc_left, raw_pc_right, raw_ev, ev_sum, info = \
			spanlib_fort.svd(left, right, self._nsvd, lweights, rweights, usecorr, largematrix)
		if info != 0:
			raise SpanlibError('Error when running fortran SVD',  'svd')
			
		# Save results
		self._svd_raw_pc[0] = raw_pc_left
		self._svd_raw_eof[0] = raw_eof_left
		self._svd_raw_pc[1] = raw_pc_right
		self._svd_raw_eof[1] = raw_eof_right
		self._svd_raw_ev = raw_ev
		self._svd_ev_sum = ev_sum


		self._last_analysis_type = 'svd'
		gc.collect()

	@_filldocs_
	def svd_eof(self,iset=None,scale=False, raw=False,**kwargs):
		"""Get EOFs from SVD analysis

		If SVD was not performed, it is done with all parameters sent to :meth:`svd`

		:Parameters:
			%(scale)s
			%(raw)s
			%(iset)s
			
		:SVD parameters:
			%(nsvd)s
			%(prepca)s
			
		:Returns:
			Arrays with shape ``(nsvd,...)``
		"""

		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		self._update_('svd', **kwargs)

		# Of, let's format the variable
		fmt_eof = {}
		for iset in xrange(self._ndataset): # (window*nchan,nsvd)
		
			# Operate only on selected datasets
			if isets is not None and iset not in isets: continue
		
			# EOF already available 
			if self._svd_fmt_eof.has_key(iset):
				fmt_eof[iset] = self._svd_fmt_eof[iset]
				continue
				
			# No analyses performed?
			if not self._svd_raw_eof.has_key(iset): self.svd(iset=iset)
			raw_eof = self._svd_raw_eof[iset]
			nm = self._nsvd
			nl = self._svd_raw_eof[iset].shape[0]
			raw_eof = self._svd_raw_eof[iset].reshape((nl, nm))
			
			if raw: # Do not go back to physical space
				self._svd_fmt_eof[iset] = [cdms2.createVariable(raw_eof.transpose())]
				self._svd_fmt_eof[iset][0].setAxisList(
					[self._mode_axis_('svd',iset),self._svd_channel_axis_(iset)])
					
			else: # Get raw data back to physical space
				raw_eof = raw_eof.reshape((nl, nm),order='F')
				
				if not self._prepca[iset]: # No pre-PCA performed
					self._svd_fmt_eof[iset] = self._unstack_(iset, raw_eof,
						self._mode_axis_('svd',iset), cpatts=False, remean=False)
						
				else:
					proj_eof,smodes = self._project_(self._pca_raw_eof[iset],raw_eof.transpose(),iset, nt=nm)
					self._svd_fmt_eof[iset] = self._unstack_(iset, proj_eof,
						self._mode_axis_('svd',iset), cpatts=False, remean=False)
					
			# Set attributes
			for idata,eof in enumerate(self._svd_fmt_eof[iset]):
				
				# Attributes
				if cdms2_isVariable(eof):
					if not raw and not self._stack_info[iset]['ids'][idata].find('variable_'):
						eof.id = self._stack_info[iset]['ids'][idata]+'_svd_eof'
					else:
						eof.id = 'svd_eof'
					eof.name = eof.id
					eof.standard_name = 'empirical_orthogonal_functions_of_svd'
					eof.long_name = 'SVD empirical orthogonal functions'
					if not raw:
						atts = self._stack_info[iset]['atts'][idata]
						if atts.has_key('long_name'):
							eof.long_name += ' of '+atts['long_name']
						if scale is True and atts.has_key('units'):
							eof.units = atts['units']
					
				# Scaling
				if scale:
					if scale is True:
						scale = npy.sqrt(self._mssa_raw_ev[iset])*nl
					eof[:] *= scale
					
			fmt_eof[iset] = self._svd_fmt_eof[iset]
			
		gc.collect()
		return self._demap_(fmt_eof, grouped=raw)

	@_filldocs_
	def svd_pc(self,iset=None,**kwargs):
		"""Get PCs from SVD analysis

		If SVD was not performed, it is done with all parameters sent to :meth:`svd`
		
		:Parameters:
			%(iset)s
			
		:SVD parameters:
			%(nsvd)s
			%(prepca)s
			
		:Returns:
			Arrays with shape ``(nsvd,nt)``
		"""

		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		self._update_('svd', **kwargs)


		# Of, let's format the variable
		fmt_pc = {}
		for iset in xrange(self._ndataset):
			
			# PC already available 
			if self._svd_fmt_pc.has_key(iset):
				fmt_pc[iset] = self._svd_fmt_pc[iset]
				continue
				
			# No analyses performed?
#			if not self._pca_raw_eof.has_key(iset): self.pca(iset=iset)
			if not self._svd_raw_pc.has_key(iset): self.svd(iset=iset)
						
			# Format the variable
			idata = 0 # Reference is first data
			pc = cdms2.createVariable(npy.asarray(self._svd_raw_pc[iset][:,:self._nsvd].transpose(),order='C'))
			pc.setAxis(0,self._mode_axis_('svd',iset))
			pc.setAxis(1,self._time_axis_(iset, idata))
			pc.id = pc.name = 'svd_pc'
			pc.standard_name = 'principal_components_of_svd'
			pc.long_name = 'SVD principal components'
			atts = self._stack_info[iset]['atts'][idata]
			if atts.has_key('long_name'): pc.long_name += ' of '+atts['long_name']
#			if atts.has_key('units'):     pc.units = atts['units']

			fmt_pc[iset] = self._svd_fmt_pc[iset] = pc
			self._check_dataset_tag_('_svd_fmt_pc', iset, svd=True)

		return self._demap_(fmt_pc, grouped=True)		
			
	@_filldocs_
	def svd_ev(self,relative=False,sum=False,cumsum=False,**kwargs):
		"""Get eigen values from SVD analysis

		:Parameters:
		  %(relative)s
		  %(sum)s
		  %(cumsum)s
		  %(iset)s
		
			
		:SVD parameters:
			%(nsvd)s
			%(prepca)s
			
		:Returns:
			Array with shape ``(nsvd,)`` or a float
		"""

		# Update params
		self._update_('svd', **kwargs)

		# No analyses performed?
		if not self._svd_raw_eof.has_key(0): self.svd()

		# We only want the sum
		if sum: return self._svd_ev_sum

		# Format the variable
		id = 'svd_ev'
		long_name = []
		raw_ev = self._svd_raw_ev[:self._nsvd]
		if cumsum:
			raw_ev = raw_ev.cumsum()
			id += '_cumsum'
			long_name.append('cumulative')
		if relative: 
			raw_ev = 100.*raw_ev/self._svd_ev_sum
			id += '_rel'
			long_name.append('relative')
		ev = cdms2.createVariable(raw_ev)
		ev.id = ev.name = id
		long_name.append('SVD eigen values')
		ev.long_name = ' '.join(long_name).title()
		ev.setAxis(0, self._mode_axis_('svd'))
		ev.standard_name = 'eigen_values_of_svd'
#		atts = self._stack_info['atts'][0]
#		if atts.has_key('long_name'):
#			ev.long_name += ' of '+atts['long_name']
		if relative:
			ev.units = '% of total variance'
		return ev

	@_filldocs_
	def svd_rec(self,modes=None,iset=None,raw=False, **kwargs):
		"""Reconstruction of SVD modes
		
		:Parameters:
			%(iset)s
			%(modes)s
			%(raw)s
			
		:SVD parameters:
			%(nsvd)s
			%(prepca)s
			
		:Returns:
			Arrays with the same shape as input arrays.
		"""
		
		# Dataset selection
		isets = self._check_isets_(iset)

		# Update params
		self._update_('svd', **kwargs)
		
		# Loop on datasets
		svd_fmt_rec = {}
		for iset in isets:

			# No analyses performed?
			if not self._svd_raw_eof.has_key(iset): self.svd(iset=iset)
			
			# Projection
			raw_rec,smodes = self._project_(self._svd_raw_eof[iset],
				self._svd_raw_pc[iset],iset,modes)
				
			# Get raw data back to physical space (nchan,nt)
			taxis = self._time_axis_(iset)
			if not self._prepca[iset]: # No pre-PCA performed
				svd_fmt_rec[iset] = self._unstack_(iset,raw_rec,taxis)
				
			elif raw: # Force direct result from svd
				svd_fmt_rec[iset] = [cdms2.createVariable(raw_rec.transpose())]
				svd_fmt_rec[iset][0].setAxisList(0,[taxis,self._mode_axis_('svd',iset)])
					
			else: # With pre-pca
				proj_rec, spcamodes = self._project_(self._pca_raw_eof[iset],raw_rec.transpose(),iset,nt=len(taxis))
				svd_fmt_rec[iset] = self._unstack_(iset,proj_rec,taxis)
			del  raw_rec
			
			# Set attributes
			for idata,rec in enumerate(svd_fmt_rec[iset]):
				if not cdms2_isVariable(rec): continue
				if not self._stack_info[iset]['ids'][idata].startswith('variable_'):
					rec.id = self._stack_info[iset]['ids'][idata]+'_svd_rec'
				else:
					rec.id = 'svd_rec'
#				if modes is not None:
				rec.id += smodes #FIXME: do we keep it?
				rec.modes = smodes
#				else:
#					rec.modes = '1-%i'%self._nsvd[iset]
				rec.standard_name = 'recontruction_of_svd_modes'
				rec.long_name = 'Reconstruction of SVD modes'
				atts = self._stack_info[iset]['atts'][idata]
				if atts.has_key('long_name'):
					rec.long_name += ' of '+atts['long_name']
					
		return self._demap_(svd_fmt_rec, grouped=raw)	
	
	def rec(self,analysis_type=None,*args,**kwargs):
		if analysis_type is None:
			analysis_type = self._last_analysis_type
		else:
			valid = ['pca','mssa','svd']
			if analysis_type not in valid:
				raise SpanlibException('rec','analysis_type must be one of '+str(valid))
		if analysis_type is None:
			warnings.warn('Yet no statistics performed, so nothing to reconstruct!')
		else:
			return getattr(self,self._last_analysis_type+'_rec')(*args,**kwargs)
			
	def clean(self):
		"""(Re-)Initialization"""
		dicts = []
		for aa in 'pca','mssa','svd':
			dicts.append('_%s_ev_sum'%aa)
			for bb in 'raw','fmt':
				for cc in 'eof','pc','ev':
					dicts.append('_%s_%s_%s'%(aa,bb,cc))
		dicts.extend(['_mode_axes','_mssa_window_axes','_mssa_pctime_axes','_mssa_channel_axes','_svd_channel_axes'])
		lists = ['_mssa_pairs','_stack_info','_svd_l2r','_nt','_ns','_ndata','_pdata']
		self._nphase = 8
		for ll,func in (dicts,dict),(lists,list):
			for att in ll:
				if hasattr(self,att):
					obj = getattr(self,att)
					del obj
				setattr(self,att,func())
		self._ndataset = 0
		self._last_analysis_type = None
		gc.collect()


	def nmssa(self):
		"""
		Number of MSSA modes
		
		:Returns:
			integer or tuple
		"""
		return self._demap_(self._nmssa, grouped=True)
		
	def npca(self):
		"""
		Number of PCA modes
		
		:Returns:
			integer or tuple
		""" 
		return self._demap_(self._npca, grouped=True)

	def ns(self):
		"""
		Length of channel axis (unmasked input points)
		
		:Returns:
			integer or tuple
		"""
		return self._demap_(self._ns, grouped=True)
		
	def nt(self):
		"""
		Length of time axis
		
		:Returns:
			integer or tuple
		"""
		return self._demap_(self._nt, grouped=True)
		
	def window(self, absolute=False):
		"""
		MSSA window parameter
		
		:Options: 
		
			- *absolute*: if False, return the window relative to time length, 
			  else return the effective window (multiplied by nt)
		
		:Returns:
			integer or tuple
		"""
		win = self._window
		if absolute:
			win = [int(w*nt) for nt in zip(win, self._nt)]
		return self._demap_(win, grouped=True)

	def nsvd(self):
		"""
		Number of SVD modes
		
		:Returns:
			integer or tuple
		"""
		return self._nsvd



	#################################################################
	## Organize datasets
	#################################################################


	def _map_(self, datasets, serial):
		"""Make datasets in the right form (a list of a list of variables)"""
		# We start from a list
		if not isinstance(datasets,(list,tuple)): # A single variable (array)
			datasets = [datasets]
			self._input_map = 0
			
		else: # Several variables or group of variables
			if isinstance(datasets,tuple):
				datasets = list(datasets)
			self._input_map = len(datasets)

		# Check if we are forced to be in serial analysis mode
		for d in datasets:
			if isinstance(d,(list,tuple)):
				serial = True
				break
		if not serial: # Convert to serial like mode (single group of variables)
			datasets = [datasets]
			
		else: # Several groups of variables
			
			input_map = []
			for iset in xrange(len(datasets)):
				
				if isinstance(datasets[iset],(list,tuple)): # Several variables
					
					# Convert tuple to list
					if isinstance(datasets[iset],tuple): 
						datasets[iset] = list(datasets[iset])
					
					# Append the number fo variables
					input_map.append(len(datasets[iset]))
					
				else: # A single variable
					
					datasets[iset] = [datasets[iset]]
					input_map.append(0)
					
			self._input_map = input_map
		self._datasets = datasets
		
	def _demap_(self, fields, grouped=None):
		"""Return fields as input dataset (depth and shapes)"""
		
		# Convert list to dictionary
		if isinstance(fields, list):
			if grouped is None: grouped = not isinstance(fields[0], list)
			ff = {}
			for i, f in enumerate(fields):
				ff[i] = f
			fields = ff
		else:
			if grouped is None: grouped = False
			fields = copy.copy(fields)
		
		# Loop on datasets	
		ret = ()
		for iset in sorted(fields.keys()):
			
			# Get input map for this dataset
			im = self._input_map if isinstance(self._input_map, int) else self._input_map[iset]
			
			# Get field of current dataset
			f = fields[iset]
			
			# Single input variable (or grouped to single)?
			if im == 0 and not grouped:
				f = f[0]
				
			# Only one dataset
			if isinstance(self._input_map, int): 
				return f
			
			# Append
			ret += f, 
			
		return ret
		

	def _remap_(self, data,  where=None, grouped=False):
		"""Organize data as a list of list of variable in the same way input data was reorganized"""
		try:
			# Single variable
			if self._input_map == 0: 
				if not isinstance(data, npy.ndarray): raise
				if grouped: return [data]
				return [[data]]
			
			# Several variables in a single dataset
			if isinstance(self._input_map,  int):
				if len(data)!=self._input_map: raise
				if grouped: return [data]
				datasets = []
				for i in xrange(self._input_map):
					var = data[i]
					if not isinstance(var, npy.ndarray): raise
					datasets.append(var)
				return [datasets]
					
			# Several datasets
			if grouped: return data
			datasets = []
			for j, imap in enumerate(self._input_map):
				if imap == 0: # Single var
					datasets.append([data[j]])
				else: # Several variables
					if len(data[j])!=imap: raise
					dd = []
					for idata in xrange(imap):
						var = data[j][i]
						if not isinstance(var, npy.ndarray): raise
						dd.append(var)
					datasets.append(dd)
			return datasets
			
		except:
			raise SpanlibError('Your input data must be in the same form as input data (map: %s)'%(self._input_map, ), where)
	

	@classmethod
	def _core_stack_(cls, dataset, dweights=None, dnorms=None, dorders=None, dmasks=None, notime=False, getinvalid=False, nvalid=None,):
		""" Takes several data files, of same time and stacks them up together
	
		This fonction concatenates several dataset that have the
		same time axis. It is useful for analysing for example
		several variables at the same time.
		It takes into account weights, masks and axes.
	
		:Params:
		
			- *dataset*: A list of data objects.
				They must all have the same time length.
				
		:Options:
		
			- *dweights*: Associated weights.
	
		"""
	
		# Inits
		len_time=None
		taxes = []
		saxes = []
		atts = []
		shapes = []
		ids = []
		grids = []
		mvs = []
		norms = []
		means = []
		types = []
		ndims = []
		orders = []
		masks = []
		if getinvalid:
			invalids = []
		if dweights is None:
			dweights = [None]*len(dataset)
		if dnorms is None:
			dnorms = [None]*len(dataset)
		if dorders is None:
			dorders = [None]*len(dataset)
		if dmasks is None:
			dmasks = [None]*len(dataset)
		if notime: getinvalid=False
	
		# Loop on datasets
		for idata,data in enumerate(dataset):
	
			# Guess data type
			if not cdms2_isVariable(data):
				#data = cdms2.createVariable(data,copy=0)
				if npy.ma.isMA(data):
					types.append('npy.ma')
				else:
					types.append('npy')
			else:
				types.append('MV2')
	
			# Check time axis
			if not notime: 
				if dorders[idata] is not None: # We specifiy the right order
					data = MV2.asarray(data)
					if cdms2.order2index(data.getAxisList(), data.getOrder())!=dorders[idata]:
						data = data(order=dorders[idata])
				elif cdms2_isVariable(data) and data.getTime() is not None: 
					# If a proper time axis is found, bring it to front
					order = cdms2.order2index(data.getAxisList(), data.getOrder())
					data = data(order='t...')
					dorders[idata] = order
				if len_time is None:
					len_time = data.shape[0]
				elif len_time != data.shape[0]:
					raise 'Error all datasets must have the same time length!!!!'
			else:
				orders.append(None)
	
			# Append info
			# - time
			if notime:
				taxes.append(None)
			elif not cdms2_isVariable(data):
				taxes.append(data.shape[0])
			else:
				taxes.append(data.getAxis(0))
			# - others axes
			if cdms2_isVariable(data): # cdms -> ids
				saxes.append(data.getAxisList()[1-int(notime):])
				ids.append(data.id)
				atts.append({})
				for att in data.listattributes():
					atts[-1][att] = data.attributes[att]
				grids.append(data.getGrid())
			else: # numpy -> length
				saxes.append(data.shape[1-int(notime):])
				ids.append(None)
				atts.append(None)
				grids.append(None)
			# - missing value
			if npy.ma.isMA(data):
				mvs.append(data.missing_value)
			else:
				mvs.append(1.e20)
			# - shape
			shapes.append(data.shape)
			ndims.append(data.ndim)
			
			# Pack 
			packed = SpAn._pack_(data, dweights[idata], dnorms[idata], notime=notime, 
				getinvalid=getinvalid, nvalid=nvalid, mask=dmasks[idata])
	
			# Create or concatenate
			if not idata:
				stacked = packed['data']
				weights = packed['weights']
			else:
				stacked = npy.concatenate((stacked,packed['data']))
				weights = npy.concatenate((weights,packed['weights']))
			norms.append(packed['norm'])
			masks.append(packed['mask'])
			orders.append(dorders[idata])
			means.append(packed['mean'])
			if getinvalid: invalids.append(packed['invalid'])
			del packed
			gc.collect()
	
		# Store data and information
		if notime or stacked.ndim==1:
			ns = 1
		else:
			ns = stacked.shape[0]
		if taxes[0] is None:
			nt = 0
		else:
			nt = taxes[0]
		stack_info = dict(ids=ids,taxes=taxes,saxes=saxes,masks=masks,
			weights=weights,atts=atts,shapes=shapes,grids=grids,ndims=ndims, 
			mvs=mvs,types=types,norms=norms,means=means, orders=orders)
		if getinvalid: stack_info['invalids'] = invalids
		stack = dict(info = stack_info, 
			ndata = len(ids), 
			pdata = stacked, 
			nt = stacked.shape[1], 
			ns = ns, 
		)
		return stack


	def _stack_(self, dataset, dweights, dnorms, **kwargs):
		""" Takes several data files, of same time and stacks them up together

		This fonction concatenates several dataset that have the
		same time axis. It is useful for analysing for example
		several variables at the same time.
		It takes into account weights, masks and axes.

		Parameters:
		
			- dataset: A list of data objects. They must all have the same time length.
			- dweights: Associated weights.
		:::
		"""
		stack = SpAn._core_stack_(dataset, dweights, dnorms, **kwargs)
		
		# Store data and information
		self._stack_info.append(stack['info'])
		self._ndata.append(stack['ndata'])
		self._pdata.append(stack['pdata'])
		self._nt.append(stack['nt'])
		self._ns.append(stack['ns'])


	def _unstack_(self, iset, pdata ,firstaxes, cpatts=True, remean=True):
		"""Return a list of variables in the physical space, for ONE dataset.
		
		Parameters:
		
			iset: dataset index
			pdata: (~ns,nt) typically from fortran routines
			firstaxes: MUST be a tuple of all axes except spatial axes  
			cpatts: copy attributes of input variables to unstacked output variables
			remean: add temporal mean of input variables to unstacked output variables
		"""

		# Loop on stacked data
		istart = 0
		unstacked = []
		#if not isinstance(firstaxes,list):
			#firstaxes = [firstaxes]
		if firstaxes is not None and not isinstance(firstaxes, tuple):
			firstaxes = firstaxes,
		for idata in xrange(len(self._stack_info[iset]['ids'])):

			# Get needed stuff
			props = {}
			for att in self._stack_info[iset].keys():
				if att[-1] == 's':
					props[att[:-1]] = self._stack_info[iset][att][idata]

			# Axes and shape
			# - first axes
			axes = []
			if isinstance(firstaxes,  tuple):
				for fa in firstaxes:
					if isinstance(fa,(list,tuple)): # TIME
						axes.append(fa[idata])
					elif isinstance(fa,dict): # MODE
						axes.append(fa[iset])
					else:
						axes.append(fa) # WINDOW OR OTHERS
			# - spatial axes
			if props['type'] == 'MV2' and props['ndim'] > 1: 
				axes.extend(props['saxe'])
			# - shape
			requested_shape = ()
			for axis in axes:
				if axis is None: continue
				requested_shape += (axis, ) if isinstance(axis, int) else (len(axis), )
							
			# Unpack data
			unpacked, istart, mlen = self._unpack_(pdata, istart, props, requested_shape, remean)

			# Convert to right type
			if props['type'] != 'npy': # Masked
			
				# Masking
				final = eval(props['type']).masked_object(unpacked, props['mv']) ; del unpacked
				final.setMissing(props['mv'])
				
				# CDAT
				if props['type'] == 'MV2':
					
					# Grid
					final.setAxisList(axes)
					final.setGrid(props['grid'])
					
					# Attributes
					if cpatts:
						for attn, attv in props['att'].items():
							setattr(final, attn, attv)
							
					# Order of axes
					if props['order'] is not None:
						final = final(order=props['order'])
						
			else: # Numpy
				final = unpacked
				
			# Append to output
			unstacked.append(final)
			istart += mlen

		gc.collect()
		return unstacked
		
		
	@classmethod
	def _pack_(cls, data, weights=None, norm=None, mask=None, notime=False, nvalid=None, getinvalid=False, demean=True):
		""" Pack a dataset and its weights according to its mask
	
		:Description:
			This function packs a dataset in 2D space by removing
			all masked points and returning a space-time array.
			It performs this operation also on the weights.
			It is used for removing unnecessary points and
			simplifying the input format for analysis functions.
	
	
		:Parameters:
		
			- **data**: masked array
				Flatten in space an [x,y,t] array by 
				removingits masked point
		
		:Options:
		
			- **weights**: Weights to be flatten also
			- **norm**: Normalisation coefficients
			- **mask**: Integer mask where valid data = 1
	
		:Returns:
		
			- **packed_data**: : Space-time packed array
			- **packed_weights**: Packed weights that were guessed or used
			- **mask**: Mask that were guessed or used

		"""
		# Sizes
		packed = {}
		if notime:
			nt = 1
		else:
			nt = data.shape[0]
		if data.ndim == 1:
			nstot = 1
		else:
			nstot = npy.multiply.reduce(data.shape[1-notime:])
		sh=list(data.shape)
		
		# Weights ?
		if weights is None:
			if data.ndim == (3-notime) and cdms2_isVariable(data) and \
				'x' in data.getOrder() and 'y' in data.getOrder():
				import cdutil
				weights = cdutil.area_weights(data[0]).data # Geographic weights
			else:
				weights = npy.ones(nstot,dtype='f')
				weights = npy.ascontiguousarray(weights)
				weights.shape = sh[1-notime:]
		elif npy.ma.isMA(weights):
			weights = weights.filled(0.)
		else:
			weights = npy.asarray(weights,dtype='f')
	
		# Scalings
		if cdms2_isVariable(data): 
			data = data.asma()
		data = data.copy()
		# - mean
		if notime or not demean:
			packed['mean'] = 0.
		else:
			packed['mean'] = data.mean(axis=0)
			data[:] -= packed['mean']
		# - norm
		packed['norm'] = norm
		
		# Mask
		bmask = npy.ma.getmaskarray(data)
		maskfull = 1-bmask.astype('l')
		if mask is not None:
			maskfull = mask
			bmask = mask==1
	
		# Is it already packed? Check the mask...
		if not bmask.any() or bmask.all():
			del maskfull
			if weights is None:
				weights = npy.ones(nstot,dtype='f')
			else:
				weights = npy.asarray(weights,dtype='f')
			packed['weights'] = weights.ravel()
			if len(packed['weights']) != nstot:
				packed['weights'] = npy.resize(packed['weights'], (nstot,))
			if npy.ma.isMA(data): data = data.filled()
			data = npy.ascontiguousarray(data)
			data.shape = (nt,nstot)
			packed['data'] = data.T
			packed['mask'] = npy.ones(nstot)
			if getinvalid: packed['invalid'] = None
			SpAn._check_norm_(packed)
			return packed
	
		# Mask (1 means good)
		if mask is None:
			# - first from data (integrate) => 1D
			if not notime:
				mask = maskfull.sum(axis=0) 
			else:
				mask = maskfull
			del maskfull
			# - now remove channels where weight is zero
			mask[weights==0.] = 0
			# - check number of valid data along time
			if not notime:
				nvalid = npy.clip(int(nvalid), 1, nt) if nvalid is not None else nt
				mask[mask<nvalid] = 0
			# - save as 0/1
			mask[:] = npy.clip(mask, 0, 1)
		packed['mask'] = mask
	
		# Number of valid points in spatial dimension
		ns = long(mask.sum())
		
		# Fill data
		# - fill with mean where possible
		if not notime and nvalid!=nt: 
			invalid = data.mask&mask.astype('?')
			data[invalid] = 0.
			if getinvalid: 
				packed['invalid'] = invalid
			else: del invalid, error
		elif getinvalid:
			packed['invalid'] = None
		# - finally fill with missing values
		data_num = data.filled(1.e20)
	
		# Pack space
		# - pack numeric data array
		mask = npy.ascontiguousarray(mask)
		mask.shape = nstot,
		data_num = npy.ascontiguousarray(data_num)
		data_num.shape = (nt,nstot)
		packed['data'] = npy.asarray(spanlib_fort.chan_pack(data_num,mask,ns), dtype='f', order='F')
		del data_num
		# - pack weights
		weights = npy.ascontiguousarray(weights)
		weights.shape = (1,nstot)
		packed['weights'] = npy.asarray(spanlib_fort.chan_pack(weights,mask,ns)[:,0], dtype='f')
	
		SpAn._check_norm_(packed)
		return packed
		
	@classmethod
	def _unpack_(cls, pdata, istart, props, rshape, remean):
		"""Unpack data along space, reshape, unnormalize and remean.
		
		Input is sub_space:other, output is other:split_space.
		For MSSA: pdata(nchan,window,nmssa) (other = window:nmssa)
		"""
		# Get the block
		mlen = int(props['mask'].sum())
		iend = istart + mlen
		indata = pdata[istart:iend,:]
		
		# Unpack
		unpacked = spanlib_fort.chan_unpack(props['mask'], indata, props['mv'])
		good = unpacked != props['mv']
		
		# Unnormalize
		if props['norm'] != 1.:
			unpacked[good] *= props['norm']
			
		# Time-average
		if remean:
			ii = npy.arange(good.shape[1])
			igood = ii[props['mask']==1] ; del ii
			rmean = props['mean'].reshape(good.shape[1])
			unpacked[:, igood] += rmean[igood]
			del rmean, igood
			
		# Shape
		if rshape != unpacked.shape:
			unpacked.shape = rshape

		return unpacked, istart, mlen
			
		
	@classmethod
	def _get_imodes_(cls, imode, nmode):
		
		# Which modes
		if imode is None:
			imode = range(1,nmode+1)
		elif isinstance(imode,slice):
			imode = range(imode.start,imode.stop,imode.step)
		else:
			if isinstance(imode,int):
				#if imode < 0:
					#imode = range(-imode)
				#else:
				imode = [imode,]
					#imode = [imode-1,]
			#imode = [im+1 for im in imode]
	
		# Rearrange modes (imode=[1,3,4,5,9] -> [1,1],[3,5],[9,9])
		imode = [im for im in imode if im <= nmode]
		imode.sort(cmp=lambda x,y: cmp(abs(x),abs(y)))
		if imode[0]<0: imode.insert(0,1)
		imodes = []
		im = 0
		while im < len(imode):
			imode1 = imode2 = imode[im]
			for imt in xrange(im+1,len(imode)):
				if imode[imt] > 0  and (imode[imt]-imode2) > 1: # end of group
					im = imt-1
					break
				imode2 = abs(imode[imt]) # increase group
				continue
			else:
				if im < len(imode)-1: im = imt
			im += 1
			imodes.append((imode1,imode2))
		return imodes
	
	@classmethod
	def _check_length_(cls, input, mylen, fillvalue):
		# A single value
		if mylen == 0:
			if isinstance(input,(list,tuple)):
				if not input: return None
				return input[0]
			return input
		# Multiple values as a list (or tuple)
		if not isinstance(input,(list,tuple)):
			fillvalue = input
			input = [input]
		if isinstance(input,tuple):
			input = list(input)
		dlen = mylen-len(input)
		if dlen < 0:
			input = input[:mylen]
		elif dlen > 0:
	#		for iadd in xrange(dlen):
				input.extend([fillvalue]*dlen)
		return input
	
	@classmethod
	def _check_norm_(cls, packed):
		"""Setup the normalisation factor of a packed dataset"""
		if packed['norm'] is True or packed['norm'] is None:
			packed['norm'] = packed['data'].std() # Standard norm
			packed['data'] = packed['data']/packed['norm'] # Norm copy
		elif packed['norm'] is not False:
			if packed['norm'] <0: # Relative norm, else strict norm
				packed['norm'] = abs(packed['norm'])*packed['data'].std()
			packed['data'] = packed['data']/packed['norm'] # Norm copy
		else:
			packed['norm'] = 1.
	

	def _project_(self, reof, rpc, iset=0, imodes=None, ns=None, nt=None, nw=None):
		"""Generic raw construction of modes for pure PCA, MSSA or SVD, according to EOFs and PCs, for ONE DATASET
		
		reof: (nspace,nmode)
		rpc: (nt,nmode)
		"""

		# Get EOFs and PCs for one dataset
		if isinstance(reof, (list, tuple, dict)): reof = reof[iset]
		if isinstance(rpc, (list, tuple, dict)): rpc = rpc[iset]
		if ns is None: 
			ns = reof.shape[0]
			if nw: ns /= nw
		if nt is None: nt = self._nt[iset]

		# Which modes
		nmode = reof.shape[-1]
		imodes = SpAn._get_imodes_(imodes, nmode)

		# Function of reconstruction
		if nw is not None:
			function = spanlib_fort.mssa_rec # MSSA
		else:
			function = spanlib_fort.pca_rec  # PCA/SVD


		# Arguments
		args = [reof,rpc]
		kwargs = {}
		if nw is not None:
			# MSSA
			args.extend([ns,nt,nw])
			
		# Loop no modes
		smodes = []
		ffrec = 0.
		for j,ims in enumerate(imodes):
			if ims[0] > nmode: break
			if ims[1] > nmode: ims = (ims[0], nmode)
			targs = args+list(ims)
			ffrec += function(*targs,**kwargs) # (nc,nt)
			if ims[0] == ims[1]:
				smode = str(ims[0])
			else:
				smode = '%i-%i'%tuple(ims)
			smodes.append(smode)
		return ffrec,'+'.join(smodes)


class SVDModel(SpAn):

	def __init__(self,predictor, predictand,**kwargs):

		SpAn.__init__(self,(predictor,predictand) ,**kwargs)

		# Perform an SVD between the first two datasets
		self.learn(nsvd=None,pca=None)

		# Compute the scale factors between the two datasets
		self._l2r = self._svd_raw_pc[1].std(axis=0)/self._svd_raw_pc[0].std(axis=0)

	def l2r(self, left, method='pcs'):
		return self._l2r*left
		
	def learn(self, **kwargs):
		"""Learning phase"""
#		self.clean()
		self.svd(**kwargs)
		
	def run(self, predictor, method='pcs', modes=None):
		"""Run the SVD model 
		
		- *method*: Method of reconstruction [default: 'regre']. 'direct' assumes that left and normalized expansion coefficients are equal (Syu and Neelin 1995). 'regre' does not use right EOFs but regression coefficients (Harrisson et al 2002)
		
		:Example:
			
			# Init
			>>> model = SVDModel(predictor,predictand)
			# Run method
			>>> predicted1 = model.run(new_predictor1)
			# Call method
			>>> predicted2 = model(new_predictor2)
		"""
		# Predictor as SpAn datasets
		predictor = self._remap_([predictor])[0]
#		if isinstance(predictor, (list, tuple)):
#			if isinstance(predictor, tuple):
#				predictor = list(predictor)
#			input_map = len(predictor)
#		else:
#			predictor = [predictor]
#			input_map = 0
#		print '-'*70
#		# Check input map
#		if input_map != self._input_map[0]:
#			raise SpanlibError('svdmodel::run', 'Your input predictor is not in the same form as the predictor used in the learning phase')
			
		# Stack with some checks
		notime = predictor[0].ndim != self._stack_info[0]['ndims'][0]
		stack = _core_stack_(predictor, dnorms=self._stack_info[0]['norms'], notime=notime, 
			dmasks=self._stack_info[0]['masks'], dorders=self._stack_info[0]['orders'], 
			dweights=self._stack_info[0]['weights'])
		var = stack['pdata'] # (nt,ns)
		
		# Get left expansion coefficients
		# - pre pca
		if self._prepca[0]:
			toproj = self._getec_(var, 'pca') # (nt,npca)
		else:
			toproj = var
		# - svd
		lec = self._getec_(toproj.T, 'svd') # (nt,nsvd)
		
		# Convert to right expansion coefficients (nt,nsvd)
		rec = self.l2r(lec, method=method)

		# Projections
		raw_rec,smodes = self._project_(self._svd_raw_eof[1], rec, 1, modes)
			
		# Get raw data back to physical space (nchan,nt)
		taxis = stack['info']['taxes'][0]
		if not self._prepca[1]: # No pre-PCA performed
			projected = self._unstack_(1, raw_rec, taxis)
			
		else: # With pre-pca
			proj_rec, spcamodes = self._project_(self._pca_raw_eof[1], raw_rec.T, 1,
				nt=len(taxis) if taxis is not None else 1)
			projected = self._unstack_(1, proj_rec, taxis)
		del  raw_rec
		
		# Set attributes
		for idata,rec in enumerate(projected):
			if not cdms2_isVariable(rec): continue
			if not self._stack_info[1]['ids'][idata].startswith('variable_'):
				rec.id = self._stack_info[1]['ids'][idata]+'_sm'
			else:
				rec.id = 'sm_var%idata'%idata
			rec.modes = smodes
			rec.long_name = 'Results of SVD model'
			atts = self._stack_info[1]['atts'][idata]
			if atts.has_key('long_name'):
				rec.long_name += ' of '+atts['long_name']
				
		return self._demap_({1:projected})


	__call__ = run

	def _getec_(self, var, anatype):
		"""Special reimplementation of the fortran pca_getec
		
		var: (ns,nt)
		anatype: 'pca' or 'svd'
		"""
		
		# Input arrays
		# - eofs(ns,nk)
		eofs = getattr(self, '_%s_raw_eof'%anatype)[0]
		# - weights(ns)
		if (anatype=='pca' and self._prepca[0]) or (anatype=='svd' and not self._prepca[0]) :
			weights = self._stack_info[0]['weights']
		else:
			weights = npy.ones(eofs.shape[0])
			
		# Projection (ec(nt,nk))
		ec = spanlib_fort.pca_getec(var, eofs, weights)
		
		return ec
	
	def clean(self):
		SpAn.clean(self)
		self._regre_ = None
		gc.collect()



class SpanlibError(Exception):
	def __init__(self,what, where=None):
		Exception.__init__(self)
		self._where = where
		self._what = what
	def __str__(self):
		if self._where is None:
			return 'SpanlibError: %s' %  self._what
		return 'SpanlibError: [%s] %s' % (self._where, self._what)



def phase_composites(data,  nphase=8, minamp=.5, firstphase=0, index=None, force_cdat=False):
	""" Phase composites for oscillatory fields

	  This computes temporal phase composites of a spatio-temporal
	  dataset. The dataset is expected to be oscillatory in time.
	  It corresponds to a reoganisation of the time axis to
	  to represents the dataset over its cycle in a arbitrary
	  number of phases. It is useful, for example, to have a
	  synthetic view of an reconstructed MSSA oscillation.

	:Parameters:
	
		- *data*:A ``cdms2`` variable with a time axis over which
		  composites are computed.
		  
	:Options:
	
		- *nphase*: Number of phases (divisions of the cycle)
		- *minamp*: Minimal value of retained data, relative to standard deviation.
		- *firstphase*: Position of the first phase in the 360 degree cycle.
		- *index*: Index to identify phases. If ``None``, the first PC is used 
		   (see :meth:`SpAn.pca_pc`)
		- *force_mv2: If ``data`` is not a :class:`MV2.array` (CDAT) array,
		  and CDAT is supported, output is a class:`MV2.array` array.

	:Returns:
		A ``cdms2`` variable with phase axis (of length ``nphase``)
		instead of a time axis.
	"""
	
	# Get the first PC and its smoothed derivative
	if index is None:
		pc = SpAn(data).pca_pc(npca=1, quiet=True)[0]
		if npy.ma.isMA(pc): pc = pc.filled()
	else:
		pc = npy.array(index)
	pc /= pc.std() # normalization
	if len(pc) > 2: # 1,2,1 smooth
		pc[1:-1] = npy.convolve(pc, [.25, .5, .25], 'valid')
	dpc = npy.gradient(pc)
	if len(pc) > 2: # 1,2,1 smooth
		dpc[1:-1] = npy.convolve(dpc, [.25, .5, .25], 'valid')
	dpc[:] = dpc/dpc.std() # normalization
	
	# Get amplitude and phase indexes
	amplitudes = npy.hypot(pc, dpc)
	angles = npy.arctan2(dpc, pc)
	dphase = 2*npy.pi/nphase
	angles[:] = npy.where(angles >= 2*npy.pi-dphase*.5, angles-dphase*.5, angles)
	marks = dphase * (npy.arange(nphase+1) - .5) + firstphase*npy.pi/360.
	angles = (angles-marks[0])%(npy.pi*2)+marks[0]
	
	# Initialize output variable
	if cdms2_isVariable(data) or (has_cdat_support and force_cdat):
		if not cdms2_isVariable(data): data = MV2.asarray(data)
		order = data.getOrder()
		itaxis = npy.clip(order.find('t'), 0, data.ndim-1)
		phases = MV2.repeat(MV2.take(data, (0, ), itaxis), nphase, itaxis)
		phases[:] = MV2.masked
		paxis = cdms2.createAxis(npy.arange(nphase)*dphase, id='phases')
		paxis.long_name = 'Circular phases'
		paxis.units = 'degrees'
		axes = data.getAxisList()
		axes[itaxis] = paxis
		phases.setAxisList(axes)
	else:
		if not isinstance(data, npy.ndarray):
			data = npy.asarray(data)
		phases = npy.resize(data[0].copy(), (nphase, )+data.shape[1:])
	
	# Loop on circular bins to make composites
	slices = [slice(None), ]*phases.ndim
	idx = npy.arange(data.shape[itaxis])
	for iphase in xrange(len(marks)-1):
		slices[itaxis] = iphase
		inbin = amplitudes > minamp
		inbin &= angles >= marks[iphase]
		inbin &= angles < marks[iphase+1]
		phases[tuple(slices)] = data.compress(inbin, axis=itaxis).mean(axis=itaxis)
		
	if cdms2_isVariable(phases) and 't' in order and not order.startswith('t'):
		return phases(order=order)
	return phases


class Filler(object):
	"""Class to fill missing value with a MSSA filtered version of the data
	
	The initialization automatically call the :meth:`fill` method.
	"""
	
	def __init__(self, data, **kwargs):
		
		self._kwargs = kwargs
		self._kwargs['getinvalid'] = True
		self._kwargs.setdefault('nvalid', 1)
		self._kwargs.setdefault('quiet', True)
		self._data = data
		self.filled = None
		self.filtered = None
		
		self.fill(**kwargs)

	def fill(self, nitermax=50, cvmax=2, npca=20, nmssa=15, **kwargs):
		"""Run the filler with a convergence loop
		
		Results are accessible in the following attributes:
		
		.. attribute:: filtered
		
			Filtered data, result from the convergence loop.
			
		.. attribute:: filled
		
			Data filled with :attr:`filtered`
			
		:Parameters:
		
			- **nitermax**: Maximal number of iterations
			- **cvmax**: Convergence criterion (%)
			- **npca**: Number of PCA modes (see :class:`SpAn`)
			- **nmssa**: Number of MSSA modes (see :class:`SpAn`)
			- Other parameters are passed to :class:`SpAn`
			
		:Returns:
		
			- :attr:`filled`
		
		"""
	
		kwargs['getinvalid'] = True
		kwargs.setdefault('nvalid', 1)
		kwargs.setdefault('quiet', True)
		data = self._data
		for icv in xrange(nitermax):
			
			# Setup data
			# - run analysis
			if icv:
				kwargs['getinvalid'] = False
				kwargs['nvalid'] = None
			span = SpAn(data, serial=False, npca=npca, nmssa=nmssa, **kwargs)
			# - first run
			if icv == 0:
				# Check input
				if isinstance(span._input_map, list):
					raise TypeError('Input data must be a single variable or a list of variables')
				mdata = span._datasets
				# Keep original data safe
				for ivar in xrange(len(mdata[0])):
					var = mdata[0][ivar]
					if cdms2_isVariable(var): mdata[0][ivar] = var.clone()
					else: mdata[0][ivar] = var.copy()			
			
			# Check convergence
			if icv == 0: # reference
			
				self.invalids = invalids = span._stack_info[0]['invalids']
				for invalid in invalids:
					if invalid is not None and invalid.any():
						break
				else:
					print "Nothing to fill in"
					datarec = data
					break
				cv = 1.
				last_energy = 0.
					
			else: # next steps
			
				# new energy
				energy = 0.
				for i, invalid in enumerate(invalids):
					if invalid is None: continue
					basevar = span._datasets[0][i]
					if cdms2_isVariable(span._datasets[0][i]):
						basevar = basevar.asma()
					basevar = basevar.copy()
					basevar[:] -= span._stack_info[0]['means'][i] #FIXME: order
					basevar[:] *= basevar
					energy += basevar[invalid].sum()
					del basevar
					
				# check convergence
				cv = (energy-last_energy)/energy
				print ' cv', cv*100
				if cv < 0 or npy.abs(cv) < cvmax/100.:
					print 'Convergence reached: %.2f%% (%i iterations)'%(100*cv, icv)
					break
				last_energy = energy
			
			# Reconstruction
			datarec = span.mssa_rec()
			mdatarec = span._remap_(datarec)
			
			# Replace invalid data with reconstructed field
			for ivar in xrange(len(mdatarec[0])):
				if invalids[ivar] is not None:
					mdata[0][ivar][:] = eval(span._type_(0, ivar)).where(invalids[ivar], 
						mdatarec[0][ivar], mdata[0][ivar])
			data = span._demap_(mdata)
					
	
			# Check iteration limit
			if icv >= nitermax:
				print 'Convergence not reached %i%% (%i iterations)'%(100*cv, icv)
				break
				
		# Output
		self.span = span
		self.nmssa = span.nmssa()
		self.npca = span.npca()
		self.filled = data
		self.filled.id = self._data.id+'_filled'
		self.filtered = datarec
		self.filled.id = self._data.id+'_filtered'
		return data

def fill_missing(data, nitermax=50, cvmax=0.02, out='filled', **kwargs):
	
	kwargs['getinvalid'] = True
	kwargs.setdefault('nvalid', 1)
	kwargs.setdefault('quiet', True)
	for icv in xrange(nitermax):
		
		# Setup data
		if icv:
			kwargs['getinvalid'] = False
			kwargs['nvalid'] = None
		span = SpAn(data, serial=False, **kwargs)
		if icv == 0:
			if isinstance(span._input_map, list):
				raise TypeError('Input data must be a single variable or a list of variables')
			mdata = span._remap_(data)

		
		
		# Check convergence
		if icv == 0: # reference
		
			invalids = span._stack_info[0]['invalids']
			for invalid in invalids:
				if invalid is not None and invalid.any():
#					print invalid.sum()
					break
			else:
				print "Nothing to fill in"
				if out=='filled':return data
				elif out=='both': return data, data
				return data
			cv = 1.
			last_energy = 0.
				
		else: # next steps
		
			# new energy
			energy = 0.
			for i, invalid in enumerate(invalids):
				if invalid is None: continue
				basevar = span._datasets[0][i]
				if cdms2_isVariable(span._datasets[0][i]):
					basevar = basevar.asma()
				basevar = basevar.copy()
				basevar[:] -= span._stack_info[0]['means'][i] #FIXME: order
				basevar[:] *= basevar
				energy += basevar[invalid].sum()
				del basevar
				
			# check convergence
			cv = (energy-last_energy)/energy
			print ' cv', cv*100
			if cv < 0 or npy.abs(cv) < cvmax:
				print 'Convergence reached: %i%% (%i iterations)'%(100*cv, icv)
				if out=='rec': return datarec
				elif out=='both': return data, datarec
				return data
			last_energy = energy
		
		# Reconstruction
		datarec = span.mssa_rec()
		mdatarec = span._remap_(datarec)
		for ivar in xrange(len(mdatarec[0])):
			
			# Keep original data safe
			if icv==0: 
				var = mdata[0][ivar]
				if cdms2_isVariable(var): mdata[0][ivar] = var.clone()
				else: mdatar[0][ivar] = var.copy()
				
			# Replace invalid data with reconstructed data
			if invalids[ivar] is not None:
				mdata[0][ivar][:] = eval(span._type_(0, ivar)).where(invalids[ivar], 
					mdatarec[0][ivar], mdata[0][ivar])
		data = span._demap_(mdata)
		pass
#		if span._input_map == 0:
#			if icv==0:
#				if cdms2_isVariable(data): data = data.clone()
#				else: data = data.copy()
#			data[:] = eval(span._type_(0, 0)).where(invalids[0], datarec, data)
#		else:
#			data = list(data)
#			for i, d in enumerate(datarec):
#				if icv==0:
#					if cdms2_isVariable(d): d = data.clone()
#					else: d = d.copy()
#				if invalids[i] is not None:
#					data[i][:] = eval(span._type_(0, i)).where(invalids[i], d, data[i])
				

		# Check iteration limit
		if icv >= nitermax:
			print 'Convergence not reached %i%% (%i iterations)'%(100*cv, icv)
			if out=='rec':return datarec
			elif out=='both': return data, datarec
			return data
		

class RedNoise(object):
	"""Create a red noise generated based on lag-0 and lag-1 autocovariances of a variable
	
	:Algorithmm: The algorithm method follows [Allen_and_Smith_1996] use the following formula.
	
		- The red noise is an autoregressive process of order 1 (AR(1)):
		
		  .. math:: u_t = u_0 + \gamma(u_{t-1}-u_0) + \alpha z_t
		  
		  where :math:`z` is a white noise of unit variance.

		- Its Lag-l autocovariance is:
		
		  .. math:: c_l = \frac{\alpha^2\gamma^l}{1-\gamma^2}
		
		- Estimator of the lag-l autocovariance of a sample: 

		  .. math:: \hat{c}_l = \frac{1}{N-l} \sum_{i=1}^{N-l} (d_i-\bar{d})(d_{i+l}-\bar{d})

		- Bias of the variance of surrogates generated using :math:`\gamma` and :math:`\alpha`:
		
		  .. math:: 
		  
			\mu^2(\gamma) = \frac{1}{N} + \frac{1}{N^2} \left[ \frac{N-\gamma^N}{1-\gamma}-
			\frac{\gamma(1-\gamma^{N-1})}{(1-\gamma)^2} \right]
			
		- Corrected estimator of the lag-l covariance:
		
		  .. math:: \tilde{c}_l \equiv \hat{c}_l + \hat{c}_0 \gamma^2
		  
		- Corrected :math:`\tilde{\gamma}` is the solution of :
		
		  .. math:: 
		  
			\frac{\hat{c}_1}{\hat{c}_0}  = 
			\frac{\tilde{\gamma}-\mu^2(\tilde{\gamma})}{1-\mu^2(\tilde{\gamma})}
			
		  using a Newton-Raphson algorithm.
			
		- We now have : :math:`\tilde{c}_0 = \hat{c}_0/(1-\mu^2(\tilde{\gamma}))` .
		- :math:`\tilde{\alpha}` is estimated using the second equation.
		- The red noise is then generated using the first formula above.
	
	:Parameters: *data*: array with time as first dimension
	
	:Example:
	
	>>> noiser = RedNoise(data)
	>>> noise1 = noiser.sample()
	>>> noise2 = noiser.sample()
	"""
	
	# Bias
	mu2 = classmethod(lambda cls,  g, N: -1./N + 2./N**2 * ( (N-g**N)/(1-g) + (g**N-g)/(1-g)**2 ))
	
	# Derivative of the bias with respect to gamma
	dmu2dg = classmethod(lambda cls, g, N: 2.*( -(N+1)*g**N + (N-1)*g**(N+1) + (N+1)*g -N+1 ) / ( N**2*(g-1)**3 ))
	
	# Newtown iterations
	nitermax = 6
	
	# Unbiased gamma
	ubg = classmethod(lambda cls, g, N: (g-RedNoise.mu2(g, N))/(1-RedNoise.mu2(g, N)))
	
	# Derivative of the unbiased gamma with respect to gamma 
	dubgdg = classmethod(lambda cls, g, N: (1-RedNoise.mu2(g, N)-RedNoise.dmu2dg(g, N)*(1-g)) / (1-RedNoise.mu2(g, N))**2)
	
	def __init__(self, data):
		
		# Get biased statistics (nchan)
		data = data-data.mean(axis=0)
		nt = data.shape[0]
		c0 = data.var(axis=0, ddof=0)
		c1 = (data[:-1]*data[1:]).sum(axis=0)
		c1 /= (nt-1)
		self.shape = data.shape
		del data
		
		# Find the bias and gamma (Newton-Raphson algo)
		gamma0 = c1/c0
		self.gamma = c1/c0
		for i in xrange(0, self.nitermax):
			self.gamma -= (self.ubg(self.gamma, nt)-gamma0)/self.dubgdg(self.gamma, nt)
		self.bias = self.mu2(self.gamma, nt)
		
		# Corrections
		c0 /= (1-self.bias)

		# Setup other red noise coefficients
		alpha2 = c0
		alpha2 *= 1-self.gamma**2
		self.alpha = npy.sqrt(alpha2) ; del alpha2
		self.gg = npy.repeat(self.gamma[npy.newaxis, :], nt, axis=0) # (nt,nchan)
		self.gg = self.gg.cumprod(axis=0, out=self.gg)

	
	def sample(self):
		"""Get a red noise sample fitted to input data"""
		white_noise = npy.random.randn(*self.shape) # (nt,nchan)
		white_noise /= white_noise.std(axis=0, ddof=1)
		white_noise *= self.alpha
		white_noise *= self.gg[::-1]
		red_noise = white_noise.cumsum(axis=0, out=white_noise)
		red_noise /= self.gg[::-1]
		red_noise -= red_noise.mean(axis=0)
		return red_noise

		
