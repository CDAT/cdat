module cmor_users_functions
  interface 
     subroutine cmor_create_output_path_cff(var_id, path, slen)
       character(*), intent(out) ::  path
       integer var_id
       integer , intent(out) :: slen
     end subroutine cmor_create_output_path_cff
  end interface
  interface 
     function cmor_set_cur_dset_attribute_cff(nm, value) result (ierr)
       character(*) nm
       character (*) value
       integer ierr
     end function cmor_set_cur_dset_attribute_cff
  end interface
  interface 
     function cmor_get_cur_dset_attribute_cff(name, value) result (ierr)
       character(*) name
       character(*) value
       integer ierr
     end function cmor_get_cur_dset_attribute_cff
  end interface
  interface 
     function cmor_has_cur_dset_attribute_cff(name) result (ierr)
       character(*) name
       integer ierr
     end function cmor_has_cur_dset_attribute_cff
  end interface
  interface 
     function cmor_set_variable_attribute_cff(id, nm, value) result (ierr)
       character(*) nm
       character (*) value
       integer ierr,id
     end function cmor_set_variable_attribute_cff
  end interface
  interface 
     function cmor_get_variable_attribute_cff(id, name, value) result (ierr)
       character(*) name
       character(*) value
       integer ierr,id
     end function cmor_get_variable_attribute_cff
  end interface
  interface 
     function cmor_has_variable_attribute_cff(id, name) result (ierr)
       character(*) name
       integer ierr,id
     end function cmor_has_variable_attribute_cff
  end interface
  interface 
     subroutine cmor_get_original_shape_cff(var_id,shape_array)
       integer var_id,shape_array
     end subroutine cmor_get_original_shape_cff
  end interface
  interface 
     subroutine cmor_set_table_cff(table_id)
       integer table_id
     end subroutine cmor_set_table_cff
  end interface
  interface cmor_handle_error
     subroutine cmor_handle_error_cff(error_msg, level)
       character(*) error_msg
       integer level
     end subroutine cmor_handle_error_cff
  end interface

  interface
     subroutine cmor_load_table_cff(table, table_id)
       integer, intent(out) :: table_id
       character(*) table
     end subroutine cmor_load_table_cff
  end interface

  interface 
     function cmor_setup_cff_nolog(path,ncmode,verbosity,mode,crsub) result (j)
       integer ncmode,verbosity,mode, j, crsub
       character(*) path
     end function cmor_setup_cff_nolog
  end interface
  interface 
     function cmor_setup_cff(path,ncmode,verbosity,mode,logfile,crsub) result (j)
       integer ncmode,verbosity,mode, j, crsub
       character(*) path, logfile
     end function cmor_setup_cff
  end interface

  interface
     function  cmor_ftn_get_tlen_cff(var_id) result(ierr)
       integer var_id,ierr
     end function cmor_ftn_get_tlen_cff
  end interface

  interface cmor_dataset_cff
     function cmor_dataset_cff(outpath,experiment_id,institution,&
          source,calendar,realization,contact,history,comment,&
          references, leap_year,leap_month,month_lengths,model_id,&
          forcing,initialization_method,physics_version,institute_id,&
          parent_exp_id,branch_time,parent_experiment_rip) result (ierr)
       character(*) :: outpath,experiment_id,institution,source,calendar,contact
       character(*) :: history,comment,references,model_id,forcing,institute_id
       character(*) :: parent_exp_id,parent_experiment_rip
       integer :: realization,leap_year,leap_month,month_lengths
       integer :: ierr,initialization_method,physics_version
       double precision branch_time
     end function cmor_dataset_cff
     function cmor_dataset_cff_null(outpath,experiment_id,institution,&
          source,calendar,realization,contact,history,comment,&
          references, leap_year,leap_month,model_id,forcing,&
          initialization_method,physics_version,institute_id, &
          parent_exp_id,branch_time,parent_experiment_rip) result (ierr)
       character(*) :: outpath,experiment_id,institution,source,calendar,contact
       character(*) :: history,comment,references,model_id,forcing,institute_id
       character(*) :: parent_exp_id,parent_experiment_rip
       integer :: realization,leap_year,leap_month
       integer :: ierr,initialization_method,physics_version
       double precision branch_time
     end function cmor_dataset_cff_null
     function cmor_dataset_cff_nobrch(outpath,experiment_id,institution,&
          source,calendar,realization,contact,history,comment,&
          references, leap_year,leap_month,month_lengths,model_id,&
          forcing,initialization_method,physics_version,institute_id, &
          parent_exp_id,parent_experiment_rip) result (ierr)
       character(*) :: outpath,experiment_id,institution,source,calendar,contact
       character(*) :: history,comment,references,model_id,forcing,institute_id
       character(*) :: parent_exp_id,parent_experiment_rip
       integer :: realization,leap_year,leap_month,month_lengths
       integer :: ierr,initialization_method,physics_version
     end function cmor_dataset_cff_nobrch
     function cmor_dataset_cff_null_nobrch(outpath,experiment_id,institution,&
          source,calendar,realization,contact,history,comment,&
          references, leap_year,leap_month,model_id,forcing,&
          initialization_method,physics_version,institute_id, &
          parent_exp_id,parent_experiment_rip) result (ierr)
       character(*) :: outpath,experiment_id,institution,source,calendar,contact
       character(*) :: history,comment,references,model_id,forcing,institute_id
       character(*) :: parent_exp_id,parent_experiment_rip
       integer :: realization,leap_year,leap_month
       integer :: ierr,initialization_method,physics_version
     end function cmor_dataset_cff_null_nobrch
  end interface

  interface 
     function cmor_axis_cff_double(axis_id,table_entry,units,length,&
          coord_vals,cell_bounds,cell_bounds_ndim,interval) result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       double precision :: coord_vals,cell_bounds
       integer cell_bounds_ndim, length
       integer ierr
     end function cmor_axis_cff_double
  end interface

  interface 
     function cmor_axis_cff_real(axis_id,table_entry,units,length,&
          coord_vals,cell_bounds,cell_bounds_ndim,interval) result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       real :: coord_vals,cell_bounds
       integer cell_bounds_ndim, length
       integer ierr
     end function cmor_axis_cff_real
  end interface
  interface 
     function cmor_axis_cff_int(axis_id,table_entry,units,length,&
          coord_vals,cell_bounds,cell_bounds_ndim,interval) result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       integer :: coord_vals,cell_bounds
       integer cell_bounds_ndim, length
       integer ierr
     end function cmor_axis_cff_int
  end interface
!!$  interface 
!!$     function cmor_axis_cff_long(axis_id,table_entry,units,length,&
!!$          coord_vals,cell_bounds,cell_bounds_ndim,interval) result(ierr)
!!$       integer, intent(out):: axis_id
!!$       character(*) table_entry,units,interval
!!$       integer (kind=8):: coord_vals,cell_bounds
!!$       integer cell_bounds_ndim, length
!!$       integer ierr
!!$     end function cmor_axis_cff_long
!!$  end interface
  interface 
     function cmor_axis_cff_nocoords(axis_id,table_entry,units,length,&
          interval) result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       integer length
        character type
        integer ierr
    end function cmor_axis_cff_nocoords
  end interface
  interface 
     function cmor_axis_cff_nobnds_double(axis_id,table_entry,units,length,&
          coord_vals,interval)  result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       double precision :: coord_vals
       integer length
       integer ierr
     end function cmor_axis_cff_nobnds_double
  end interface
  interface 
     function cmor_axis_cff_nobnds_real(axis_id,table_entry,units,length,&
          coord_vals,interval)  result(ierr)
       integer, intent(out)::axis_id
       character(*) table_entry,units,interval
       real :: coord_vals
       integer length
       integer ierr
     end function cmor_axis_cff_nobnds_real
  end interface
  interface 
     function cmor_axis_cff_nobnds_int(axis_id,table_entry,units,length,&
          coord_vals,interval)  result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       integer :: coord_vals
       integer length
       integer ierr
     end function cmor_axis_cff_nobnds_int
  end interface
!!$  interface 
!!$     function cmor_axis_cff_nobnds_long(axis_id,table_entry,units,length,&
!!$          coord_vals,interval)  result(ierr)
!!$       integer, intent(out):: axis_id
!!$       character(*) table_entry,units,interval
!!$       integer (kind=8) :: coord_vals
!!$       integer  length
!!$       integer ierr
!!$     end function cmor_axis_cff_nobnds_long
!!$  end interface
  interface 
     function cmor_axis_cff_nobnds_char(axis_id,table_entry,units,length,&
          coord_vals,cell_bounds_ndim,interval)  result(ierr)
       integer, intent(out):: axis_id
       character(*) table_entry,units,interval
       character(*) :: coord_vals
       integer cell_bounds_ndim, length
       integer ierr
     end function cmor_axis_cff_nobnds_char
  end interface

  interface 
     function cmor_zfactor_cff_double(zvar_id,axis_id, name, units, ndims, &
          axes_ids, values, bounds) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
       double precision values,bounds
     end function cmor_zfactor_cff_double
  end interface
  interface 
     function cmor_zfactor_cff_double_nobnds(zvar_id,axis_id, name, units, ndims, &
          axes_ids, values) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
       double precision values
     end function cmor_zfactor_cff_double_nobnds
  end interface
  interface 
     function cmor_zfactor_cff_real(zvar_id,axis_id, name, units, ndims, &
          axes_ids, values, bounds) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
       real values,bounds
     end function cmor_zfactor_cff_real
  end interface
  interface 
     function cmor_zfactor_cff_real_nobnds(zvar_id,axis_id, name, units, ndims, &
          axes_ids, values) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
       real values
     end function cmor_zfactor_cff_real_nobnds
  end interface
  interface 
     function cmor_zfactor_cff_int(zvar_id,axis_id, name, units, ndims, &
          axes_ids, values, bounds) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
       integer values,bounds
     end function cmor_zfactor_cff_int
  end interface
  interface 
     function cmor_zfactor_cff_int_nobnds(zvar_id,axis_id, name, units, ndims, &
          axes_ids, values) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
       integer values
     end function cmor_zfactor_cff_int_nobnds
  end interface
!!$  interface 
!!$     function cmor_zfactor_cff_long(zvar_id,axis_id, name, units, ndims, &
!!$          axes_ids, values, bounds) result (ierr)
!!$       integer,intent(out) :: zvar_id
!!$       integer axis_id,ndims,ierr
!!$       character(*) name,units
!!$       integer axes_ids
!!$       integer (kind=8) :: values,bounds
!!$     end function cmor_zfactor_cff_long
!!$  end interface
!!$  interface 
!!$     function cmor_zfactor_cff_long_nobnds(zvar_id,axis_id, name, units, ndims, &
!!$          axes_ids, values) result (ierr)
!!$       integer,intent(out) :: zvar_id
!!$       integer axis_id,ndims,ierr
!!$       character(*) name,units
!!$       integer axes_ids
!!$       integer(kind=8) :: values
!!$     end function cmor_zfactor_cff_long_nobnds
!!$  end interface
  interface 
     function cmor_zfactor_cff_novals(zvar_id,axis_id, name, units, ndims, &
          axes_ids) result (ierr)
       integer,intent(out) :: zvar_id
       integer axis_id,ndims,ierr
       character(*) name,units
       integer axes_ids
     end function cmor_zfactor_cff_novals
  end interface

  interface 
     function cmor_variable_cff_double(var_id,name,units,ndims,&
       axes_ids,missing,tol,&
       pos,onm,hist,com) result (ierr)
     integer , intent(out) :: var_id
     character(*) :: name,units,pos,onm,hist,com
     double precision tol
     double precision missing
     integer ierr,axes_ids,ndims
   end function cmor_variable_cff_double
  end interface
  interface 
     function cmor_variable_cff_int(var_id,name,units,ndims,&
       axes_ids,missing,tol,&
       pos,onm,hist,com) result (ierr)
     integer , intent(out) :: var_id
     character(*) :: name,units,pos,onm,hist,com
     double precision tol
     integer missing
     integer ierr,axes_ids,ndims
   end function cmor_variable_cff_int
  end interface
!!$  interface 
!!$     function cmor_variable_cff_long(var_id,name,units,ndims,&
!!$       axes_ids,missing,tol,&
!!$       pos,onm,hist,com) result (ierr)
!!$     integer , intent(out) :: var_id
!!$     character(*) :: name,units,pos,onm,hist,com
!!$     double precision tol
!!$     integer (kind=8) missing
!!$     integer ierr,axes_ids,ndims
!!$   end function cmor_variable_cff_long
!!$  end interface
  interface 
     function cmor_variable_cff_real(var_id,name,units,ndims,axes_ids,&
       missing,tol,&
       pos,onm,hist,com) result (ierr)
     integer , intent(out) :: var_id
     character(*) :: name,units,pos,onm,hist,com
     double precision tol
     real missing
     integer ierr,axes_ids,ndims
   end function cmor_variable_cff_real
  end interface
  interface 
     function cmor_variable_cff_nomiss(var_id,name,units,&
       ndims,axes_ids,tol,&
       pos,onm,hist,com) result (ierr)
     integer , intent(out) :: var_id
     character(*) :: name,units,pos,onm,hist,com
     double precision tol
     integer ierr,axes_ids,ndims
   end function cmor_variable_cff_nomiss
  end interface

  interface 
     function cmor_write_cff_real(var_id,data,suffix,ntimes_passed, &
          time_vals,time_bounds,refvar) result(ierr)
       integer var_id
       real :: data
       character(*) suffix
       integer ntimes_passed
       double precision time_vals, time_bounds
       integer refvar,ierr
     end function cmor_write_cff_real
  end interface
  interface 
     function cmor_write_cff_real_nobnds(var_id,data,suffix,ntimes_passed, &
          time_vals,refvar) result(ierr)
       integer var_id
       real :: data
       character(*) suffix
       integer ntimes_passed
       double precision time_vals
       integer refvar,ierr
     end function cmor_write_cff_real_nobnds
  end interface
  interface 
     function cmor_write_cff_real_notime(var_id,data,suffix,ntimes_passed, &
          refvar) result(ierr)
       integer var_id
       real :: data
       character(*) suffix
       integer ntimes_passed
       integer refvar,ierr
     end function cmor_write_cff_real_notime
  end interface
  interface 
     function cmor_write_cff_double(var_id,data,suffix,ntimes_passed, &
          time_vals,time_bounds,refvar) result(ierr)
       integer var_id
       double precision :: data
       character(*) suffix
       integer ntimes_passed
       double precision time_vals, time_bounds
       integer refvar,ierr
     end function cmor_write_cff_double
  end interface
  interface 
     function cmor_write_cff_double_nobnds(var_id,data,suffix,ntimes_passed, &
          time_vals,refvar) result(ierr)
       integer var_id
       double precision:: data
       character(*) suffix
       integer ntimes_passed
       double precision time_vals
       integer refvar,ierr
     end function cmor_write_cff_double_nobnds
  end interface
  interface 
     function cmor_write_cff_double_notime(var_id,data,suffix,ntimes_passed, &
          refvar) result(ierr)
       integer var_id
       double precision :: data
       character(*) suffix
       integer ntimes_passed
       integer refvar,ierr
     end function cmor_write_cff_double_notime
  end interface
  interface 
     function cmor_write_cff_int(var_id,data,suffix,ntimes_passed, &
          time_vals,time_bounds,refvar) result(ierr)
       integer var_id
       integer :: data
       character(*) suffix
       integer ntimes_passed
       double precision time_vals, time_bounds
       integer refvar,ierr
     end function cmor_write_cff_int
  end interface
  interface 
     function cmor_write_cff_int_nobnds(var_id,data,suffix,ntimes_passed, &
          time_vals,refvar) result(ierr)
       integer var_id
       integer :: data
       character(*) suffix
       integer ntimes_passed
       double precision time_vals
       integer refvar,ierr
     end function cmor_write_cff_int_nobnds
  end interface
  interface 
     function cmor_write_cff_int_notime(var_id,data,suffix,ntimes_passed, &
          refvar) result(ierr)
       integer var_id
       integer :: data
       character(*) suffix
       integer ntimes_passed
       integer refvar,ierr
     end function cmor_write_cff_int_notime
  end interface
!!$  interface 
!!$     function cmor_write_cff_long(var_id,data,suffix,ntimes_passed, &
!!$          time_vals,time_bounds,refvar) result(ierr)
!!$       integer var_id
!!$       integer(kind=8) :: data
!!$       character(*) suffix
!!$       integer ntimes_passed
!!$       double precision time_vals, time_bounds
!!$       integer refvar,ierr
!!$     end function cmor_write_cff_long
!!$  end interface
!!$  interface 
!!$     function cmor_write_cff_long_nobnds(var_id,data,suffix,ntimes_passed, &
!!$          time_vals,refvar) result(ierr)
!!$       integer var_id
!!$       integer(kind=8) :: data
!!$       character(*) suffix
!!$       integer ntimes_passed
!!$       double precision time_vals
!!$       integer refvar,ierr
!!$     end function cmor_write_cff_long_nobnds
!!$  end interface
!!$  interface 
!!$     function cmor_write_cff_long_notime(var_id,data,suffix,ntimes_passed, &
!!$          refvar) result(ierr)
!!$       integer var_id
!!$       integer(kind=8) :: data
!!$       character(*) suffix
!!$       integer ntimes_passed
!!$       integer refvar,ierr
!!$     end function cmor_write_cff_long_notime
!!$  end interface
!!$
!!$
!!$  interface 
!!$     function cmor_grid_cff_long(grid_id,ndims,axes_ids,&
!!$       lat,lon,nvertices,blat,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,area,blat,blon
!!$     end function cmor_grid_cff_long
!!$  end interface
!!$  interface  
!!$     function cmor_grid_cff_noarea_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blat,blon) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,blat,blon
!!$     end function cmor_grid_cff_noarea_long
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblon_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blat,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,blat,area
!!$     end function cmor_grid_cff_noblon_long
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblat_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,area,blon
!!$     end function cmor_grid_cff_noblat_long
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_nobnds_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,area
!!$     end function cmor_grid_cff_nobnds_long
!!$  end interface
!!$  interface   
!!$     function cmor_grid_cff_noblaar_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blon) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,blon
!!$     end function cmor_grid_cff_noblaar_long
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_nobloar_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blat) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon,blat
!!$     end function cmor_grid_cff_nobloar_long
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_nothg_long(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER (KIND=8) :: lat,lon
!!$     end function cmor_grid_cff_nothg_long
!!$  end interface


!!$  interface 
!!$     function cmor_grid_cff_double(grid_id,ndims,axes_ids,&
!!$       lat,lon,nvertices,blat,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       DOUBLE PRECISION :: lat,lon,area,blat,blon
!!$     end function cmor_grid_cff_double
!!$  end interface
  interface  
     function cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blat,blon) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       DOUBLE PRECISION :: lat,lon,blat,blon
     end function cmor_grid_cff_noarea_double
  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blat,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       DOUBLE PRECISION :: lat,lon,blat,area
!!$     end function cmor_grid_cff_noblon_double
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       DOUBLE PRECISION :: lat,lon,area,blon
!!$     end function cmor_grid_cff_noblat_double
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       DOUBLE PRECISION :: lat,lon,area
!!$     end function cmor_grid_cff_nobnds_double
!!$  end interface
  interface   
     function cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blon) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       DOUBLE PRECISION :: lat,lon,blon
     end function cmor_grid_cff_noblaar_double
  end interface
  interface  
     function  cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blat) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       DOUBLE PRECISION :: lat,lon,blat
     end function cmor_grid_cff_nobloar_double
  end interface
  interface  
     function  cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids,&
          lat,lon,nvertices) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       DOUBLE PRECISION :: lat,lon
     end function cmor_grid_cff_nothg_double
  end interface



!!$  interface 
!!$     function cmor_grid_cff_int(grid_id,ndims,axes_ids,&
!!$       lat,lon,nvertices,blat,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER :: lat,lon,area,blat,blon
!!$     end function cmor_grid_cff_int
!!$  end interface
  interface  
     function cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blat,blon) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       INTEGER :: lat,lon,blat,blon
     end function cmor_grid_cff_noarea_int
  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blat,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER :: lat,lon,blat,area
!!$     end function cmor_grid_cff_noblon_int
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER :: lat,lon,area,blon
!!$     end function cmor_grid_cff_noblat_int
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       INTEGER :: lat,lon,area
!!$     end function cmor_grid_cff_nobnds_int
!!$  end interface
  interface   
     function cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blon) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       INTEGER :: lat,lon,blon
     end function cmor_grid_cff_noblaar_int
  end interface
  interface  
     function  cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blat) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       INTEGER :: lat,lon,blat
     end function cmor_grid_cff_nobloar_int
  end interface
  interface  
     function  cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids,&
          lat,lon,nvertices) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       INTEGER :: lat,lon
     end function cmor_grid_cff_nothg_int
  end interface
  interface
     function cmor_grid_cff_nocoords(grid_id,ndims,axes_ids,nvert) &
          result (ierr)
       integer, intent(out) :: grid_id
       integer ierr, ndims, axes_ids, nvert
     end function cmor_grid_cff_nocoords
  end interface

!!$  interface 
!!$     function cmor_grid_cff_real(grid_id,ndims,axes_ids,&
!!$       lat,lon,nvertices,blat,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       REAL :: lat,lon,area,blat,blon
!!$     end function cmor_grid_cff_real
!!$  end interface
  interface  
     function cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blat,blon) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       REAL :: lat,lon,blat,blon
     end function cmor_grid_cff_noarea_real
  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blat,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       REAL :: lat,lon,blat,area
!!$     end function cmor_grid_cff_noblon_real
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,blon,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       REAL :: lat,lon,area,blon
!!$     end function cmor_grid_cff_noblat_real
!!$  end interface
!!$  interface  
!!$     function  cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids,&
!!$          lat,lon,nvertices,area) result(ierr)
!!$       integer, intent(out) :: grid_id
!!$       integer ierr,nvertices,ndims
!!$       integer axes_ids
!!$       REAL :: lat,lon,area
!!$     end function cmor_grid_cff_nobnds_real
!!$  end interface
  interface   
     function cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blon) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       REAL :: lat,lon,blon
     end function cmor_grid_cff_noblaar_real
  end interface
  interface  
     function  cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids,&
          lat,lon,nvertices,blat) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       REAL :: lat,lon,blat
     end function cmor_grid_cff_nobloar_real
  end interface
  interface  
     function  cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids,&
          lat,lon,nvertices) result(ierr)
       integer, intent(out) :: grid_id
       integer ierr,nvertices,ndims
       integer axes_ids
       REAL :: lat,lon
     end function cmor_grid_cff_nothg_real
  end interface


  interface
     function cmor_grid_cff_tvc_d(coord_grid_id,grid_id,name,units,&
          missing) result (ierr)
       integer,intent(out):: coord_grid_id
       integer grid_id,ierr
       character(*) name,units
       double precision missing
     end function cmor_grid_cff_tvc_d
  end interface
  interface
     function cmor_grid_cff_tvc_f(coord_grid_id,grid_id,name,units,&
          missing) result (ierr)
       integer,intent(out):: coord_grid_id
       integer grid_id,ierr
       character(*) name,units
       REAL missing
     end function cmor_grid_cff_tvc_f
  end interface
  interface
     function cmor_grid_cff_tvc_i(coord_grid_id,grid_id,name,units,&
          missing) result (ierr)
       integer,intent(out):: coord_grid_id
       integer grid_id,ierr
       character(*) name,units
       integer missing
     end function cmor_grid_cff_tvc_i
  end interface
  interface
     function cmor_grid_cff_tvc_no(coord_grid_id,grid_id,name,units)&
          result (ierr)
       integer,intent(out):: coord_grid_id
       integer grid_id,ierr
       character(*) name,units
     end function cmor_grid_cff_tvc_no
  end interface

  interface
     function cmor_set_grd_map_cff(gid,nm,nparam,att_names,lparam,&
          values,units,lunits) result(ierr)
       integer gid, nparam,lparam,lunits
       character(*):: att_names,units
       character(*) :: nm
       double precision :: values
     end function cmor_set_grd_map_cff
  end interface

  interface 
     function cmor_close_var_nofnm_cff(varid) result (ierr)
       integer ierr,varid
     end function cmor_close_var_nofnm_cff
  end interface
  interface 
     function cmor_close_var_fnm_cff(varid, fnm) result (ierr)
       integer ierr,varid
       character(*) :: fnm
     end function cmor_close_var_fnm_cff
  end interface
  interface 
     function cmor_close_var_nofnm_preserve_cff(varid, preserve) &
          result (ierr)
       integer ierr,varid,preserve
     end function cmor_close_var_nofnm_preserve_cff
  end interface
  interface 
     function cmor_close_var_fnm_preserve_cff(varid, fnm, preserve) &
          result (ierr)
       integer ierr,varid,preserve
       character(*) :: fnm
     end function cmor_close_var_fnm_preserve_cff
  end interface
  interface 
     function cmor_close_cff() result (ierr)
       integer ierr
     end function cmor_close_cff
  end interface
  
  interface cmor_axis
     module procedure cmor_axis_double_1
     module procedure cmor_axis_double_2
     module procedure cmor_axis_real_1
     module procedure cmor_axis_real_2
     module procedure cmor_axis_int_1
     module procedure cmor_axis_int_2
!!$     module procedure cmor_axis_long_1
!!$     module procedure cmor_axis_long_2
     module procedure cmor_axis_char
  end interface

  interface cmor_setup
     module procedure cmor_setup_ints
     module procedure cmor_setup_nc_char
  end interface


  integer, parameter:: CMOR_MAX_STRING = 1024
  integer, parameter:: CMOR_DEF_ATTR_STR_LEN = 256
  integer, parameter:: CMOR_MAX_ELEMENTS = 500
  integer, parameter:: CMOR_MAX_AXES = CMOR_MAX_ELEMENTS*3
  integer, parameter:: CMOR_MAX_VARIABLES = CMOR_MAX_ELEMENTS
  integer, parameter:: CMOR_MAX_GRIDS = 100
  integer, parameter:: CMOR_MAX_DIMENSIONS = 7
  integer, parameter:: CMOR_MAX_ATTRIBUTES = 100
  integer, parameter:: CMOR_MAX_ERRORS = 10
  integer, parameter:: CMOR_MAX_TABLES = 10
  integer, parameter:: CMOR_MAX_GRID_ATTRIBUTES = 25

  integer, parameter:: CMOR_QUIET = 0

  integer, parameter:: CMOR_EXIT_ON_MAJOR = 0
  integer, parameter:: CMOR_EXIT_ON_WARNING = 2

  real,    parameter:: CMOR_VERSION = 2.7
  real,    parameter:: CMOR_CF_VERSION = 1.4

  integer, parameter:: CMOR_WARNING = 20
  integer, parameter:: CMOR_NORMAL = 21
  integer, parameter:: CMOR_CRITICAL = 22

  integer, parameter:: CMOR_N_VALID_CALS = 8

  integer, parameter:: CMOR_PRESERVE_4 = 10
  integer, parameter:: CMOR_APPEND_4 = 11
  integer, parameter:: CMOR_REPLACE_4 = 12
  integer, parameter:: CMOR_PRESERVE_3 = 13
  integer, parameter:: CMOR_APPEND_3 = 14
  integer, parameter:: CMOR_REPLACE_3 = 15
  integer, parameter:: CMOR_PRESERVE = CMOR_PRESERVE_3
  integer, parameter:: CMOR_APPEND = CMOR_APPEND_3
  integer, parameter:: CMOR_REPLACE = CMOR_REPLACE_3

  interface cmor_zfactor
     module procedure cmor_zfactor_double
     module procedure cmor_zfactor_double_0dvalues
     module procedure cmor_zfactor_double_2dvalues
     module procedure cmor_zfactor_double_3dvalues
     module procedure cmor_zfactor_double_4dvalues
     module procedure cmor_zfactor_double_5dvalues
     module procedure cmor_zfactor_double_6dvalues
     module procedure cmor_zfactor_real
     module procedure cmor_zfactor_real_0dvalues
     module procedure cmor_zfactor_real_2dvalues
     module procedure cmor_zfactor_real_3dvalues
     module procedure cmor_zfactor_real_4dvalues
     module procedure cmor_zfactor_real_5dvalues
     module procedure cmor_zfactor_real_6dvalues
     module procedure cmor_zfactor_int
     module procedure cmor_zfactor_int_0dvalues
     module procedure cmor_zfactor_int_2dvalues
     module procedure cmor_zfactor_int_3dvalues
     module procedure cmor_zfactor_int_4dvalues
     module procedure cmor_zfactor_int_5dvalues
     module procedure cmor_zfactor_int_6dvalues
!!$     module procedure cmor_zfactor_long
!!$     module procedure cmor_zfactor_long_0dvalues
     module procedure cmor_zfactor_novals
  end interface

  interface cmor_variable
     module procedure cmor_variable_double
     module procedure cmor_variable_real
     module procedure cmor_variable_int
!!$     module procedure cmor_variable_long
  end interface
  interface cmor_write
!!$     module procedure cmor_write_1d_l
     module procedure cmor_write_1d_i
     module procedure cmor_write_1d_r
     module procedure cmor_write_1d_d
     module procedure cmor_write_2d_i
!!$     module procedure cmor_write_2d_l
     module procedure cmor_write_2d_r
     module procedure cmor_write_2d_d
!!$     module procedure cmor_write_3d_l
     module procedure cmor_write_3d_i
     module procedure cmor_write_3d_r
     module procedure cmor_write_3d_d
     module procedure cmor_write_4d_i
!!$     module procedure cmor_write_4d_l
     module procedure cmor_write_4d_r
     module procedure cmor_write_4d_d
!!$     module procedure cmor_write_5d_l
     module procedure cmor_write_5d_i
     module procedure cmor_write_5d_r
     module procedure cmor_write_5d_d
     module procedure cmor_write_6d_i
!!$     module procedure cmor_write_6d_l
     module procedure cmor_write_6d_r
     module procedure cmor_write_6d_d
     module procedure cmor_write_7d_i
!!$     module procedure cmor_write_7d_l
     module procedure cmor_write_7d_r
     module procedure cmor_write_7d_d
  end interface


  interface cmor_time_varying_grid_coordinate
     module procedure cmor_grid_tvc_r
     module procedure cmor_grid_tvc_d
     module procedure cmor_grid_tvc_i
  end interface

  interface cmor_grid
!!$     module procedure cmor_grid_1d_l
!!$     module procedure cmor_grid_2d_l
!!$     module procedure cmor_grid_3d_l
!!$     module procedure cmor_grid_4d_l
!!$     module procedure cmor_grid_5d_l
!!$     module procedure cmor_grid_6d_l
     module procedure cmor_grid_0d
     module procedure cmor_grid_1d_i
     module procedure cmor_grid_2d_i
     module procedure cmor_grid_3d_i
     module procedure cmor_grid_4d_i
     module procedure cmor_grid_5d_i
     module procedure cmor_grid_6d_i
     module procedure cmor_grid_1d_r
     module procedure cmor_grid_2d_r
     module procedure cmor_grid_3d_r
     module procedure cmor_grid_4d_r
     module procedure cmor_grid_5d_r
     module procedure cmor_grid_6d_r
     module procedure cmor_grid_1d_d
     module procedure cmor_grid_2d_d
     module procedure cmor_grid_3d_d
     module procedure cmor_grid_4d_d
     module procedure cmor_grid_5d_d
     module procedure cmor_grid_6d_d
  end interface
  interface cmor_ftn_map_data
     module procedure cmor_ftn_map_data_1d_i
     module procedure cmor_ftn_map_data_2d_i
     module procedure cmor_ftn_map_data_3d_i
     module procedure cmor_ftn_map_data_4d_i
     module procedure cmor_ftn_map_data_5d_i
     module procedure cmor_ftn_map_data_6d_i
     module procedure cmor_ftn_map_data_7d_i
     module procedure cmor_ftn_map_data_1d_r
     module procedure cmor_ftn_map_data_2d_r
     module procedure cmor_ftn_map_data_3d_r
     module procedure cmor_ftn_map_data_4d_r
     module procedure cmor_ftn_map_data_5d_r
     module procedure cmor_ftn_map_data_6d_r
     module procedure cmor_ftn_map_data_7d_r
     module procedure cmor_ftn_map_data_1d_d
     module procedure cmor_ftn_map_data_2d_d
     module procedure cmor_ftn_map_data_3d_d
     module procedure cmor_ftn_map_data_4d_d
     module procedure cmor_ftn_map_data_5d_d
     module procedure cmor_ftn_map_data_6d_d
     module procedure cmor_ftn_map_data_7d_d
  end interface
contains    

  function cmor_ftn_map_data_1d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:)
    integer, pointer :: mdata(:)
    integer, pointer :: pdata(:)
    integer , dimension(1) :: dshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j
    integer out
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
!!$ Ok we need to make sure the 1D array is big enough

    j=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          j=j*ntimes_passed
       else
          j=j*vshape(i)
       endif
    enddo
    if (j.gt.dshape(1)) then
       j=CMOR_CRITICAL
       call cmor_handle_error('data passed to cmor_write to' &
            //'not contain enough data',j)
    endif
    pdata=>data
    out=0
  end function cmor_ftn_map_data_1d_i

  function cmor_ftn_map_data_2d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:,:)
    integer, pointer :: mdata(:,:)
    integer, pointer :: pdata(:,:)
    integer , dimension(2) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.2) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.2) then
       do i=3,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.2) then
       write(msg,*) 'In FORTRAN: you passed a 2d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,2
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 2d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2)))
       pdata=>mdata
       do j=1,tshape(2)
          do i=1,tshape(1)
             pdata(i,j) = data(i,j)
          end do
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_2d_i
  function cmor_ftn_map_data_3d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:,:,:)
    integer, pointer :: mdata(:,:,:)
    integer, pointer :: pdata(:,:,:)
    integer , dimension(3) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.3) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
     if (ndims.gt.3) then
       do i=4,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
   if (ndims.ne.3) then
       write(msg,*) 'In FORTRAN: you passed a 3d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,3
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 3d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3)))
       pdata=>mdata
       do k=1,tshape(3)
          do j=1,tshape(2)
             do i=1,tshape(1)
                pdata(i,j,k) = data(i,j,k)
             end do
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_3d_i
  function cmor_ftn_map_data_4d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:,:,:,:)
    integer, pointer :: mdata(:,:,:,:)
    integer, pointer :: pdata(:,:,:,:)
    integer , dimension(4) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.4) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.4) then
       do i=5,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif

    if (ndims.ne.4) then
       write(msg,*) 'In FORTRAN: you passed a 4d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,4
       if (tshape(i).gt.dshape(i)) then
           write(msg, *)  'In FORTRAN: you passed a 4d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4)))
       pdata=>mdata
       do l=1,tshape(4)
          do k=1,tshape(3)
             do j=1,tshape(2)
                do i=1,tshape(1)
                   pdata(i,j,k,l) = data(i,j,k,l)
                end do
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_4d_i
  function cmor_ftn_map_data_5d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:,:,:,:,:)
    integer, pointer :: mdata(:,:,:,:,:)
    integer, pointer :: pdata(:,:,:,:,:)
    integer , dimension(5) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.5) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.5) then
       do i=6,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif

    if (ndims.ne.5) then
       write(msg,*) 'In FORTRAN: you passed a 5d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,5
       if (tshape(i).gt.dshape(i)) then
           write(msg, *)  'In FORTRAN: you passed a 5d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5)))
       pdata=>mdata
       do m=1,tshape(5)
          do l=1,tshape(4)
             do k=1,tshape(3)
                do j=1,tshape(2)
                   do i=1,tshape(1)
                      pdata(i,j,k,l,m) = data(i,j,k,l,m)
                   end do
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_5d_i
  function cmor_ftn_map_data_6d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:,:,:,:,:,:)
    integer, pointer :: mdata(:,:,:,:,:,:)
    integer, pointer :: pdata(:,:,:,:,:,:)
    integer , dimension(6) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,n,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.6) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.6) then
       do i=7,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.6) then
       write(msg,*) 'In FORTRAN: you passed a 6d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,6
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 6d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5),tshape(6)))
       pdata=>mdata
       do n=1,tshape(6)
          do m=1,tshape(5)
             do l=1,tshape(4)
                do k=1,tshape(3)
                   do j=1,tshape(2)
                      do i=1,tshape(1)
                         pdata(i,j,k,l,m,n) = data(i,j,k,l,m,n)
                      end do
                   enddo
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_6d_i
  function cmor_ftn_map_data_7d_i(vshape,pdata,data,mdata,ntimes_passed) result(out)
    integer, target:: data(:,:,:,:,:,:,:)
    integer, pointer :: mdata(:,:,:,:,:,:,:)
    integer, pointer :: pdata(:,:,:,:,:,:,:)
    integer , dimension(7) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,n,o,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.ne.7) then
       write(msg,*) 'In FORTRAN: you passed a 7d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,7
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 7d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5),tshape(6),tshape(7)))
       pdata=>mdata
       do o=1,tshape(7)
          do n=1,tshape(6)
             do m=1,tshape(5)
                do l=1,tshape(4)
                   do k=1,tshape(3)
                      do j=1,tshape(2)
                         do i=1,tshape(1)
                            pdata(i,j,k,l,m,n,o) = data(i,j,k,l,m,n,o)
                         end do
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_7d_i
  function cmor_ftn_map_data_1d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:)
    real, pointer :: mdata(:)
    real, pointer :: pdata(:)
    integer , dimension(1) :: dshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j
    integer out
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
!!$ Ok we need to make sure the 1D array is big enough

    j=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          j=j*ntimes_passed
       else
          j=j*vshape(i)
       endif
    enddo
    if (j.gt.dshape(1)) then
       j=CMOR_CRITICAL
       call cmor_handle_error('data passed to cmor_write to' &
            //'not contain enough data',j)
    endif
    pdata=>data
    out=0
  end function cmor_ftn_map_data_1d_r

  function cmor_ftn_map_data_2d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:,:)
    real, pointer :: mdata(:,:)
    real, pointer :: pdata(:,:)
    integer , dimension(2) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.2) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.2) then
       do i=3,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.2) then
       write(msg,*) 'In FORTRAN: you passed a 2d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,2
       if (tshape(i).gt.dshape(i)) then
           write(msg, *)  'In FORTRAN: you passed a 2d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2)))
       pdata=>mdata
       do j=1,tshape(2)
          do i=1,tshape(1)
             pdata(i,j) = data(i,j)
          end do
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_2d_r
  function cmor_ftn_map_data_3d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:,:,:)
    real, pointer :: mdata(:,:,:)
    real, pointer :: pdata(:,:,:)
    integer , dimension(3) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.3) exit
       
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
       
    enddo

    if (ndims.gt.3) then
       do i=4,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.3) then
       write(msg,*) 'In FORTRAN: you passed a 3d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,3
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 3d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3)))
       pdata=>mdata
       do k=1,tshape(3)
          do j=1,tshape(2)
             do i=1,tshape(1)
                pdata(i,j,k) = data(i,j,k)
             end do
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_3d_r
  function cmor_ftn_map_data_4d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:,:,:,:)
    real, pointer :: mdata(:,:,:,:)
    real, pointer :: pdata(:,:,:,:)
    integer , dimension(4) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.4) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.4) then
       do i=5,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.4) then
       write(msg,*) 'In FORTRAN: you passed a 4d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,4
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 4d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4)))
       pdata=>mdata
       do l=1,tshape(4)
          do k=1,tshape(3)
             do j=1,tshape(2)
                do i=1,tshape(1)
                   pdata(i,j,k,l) = data(i,j,k,l)
                end do
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_4d_r
  function cmor_ftn_map_data_5d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:,:,:,:,:)
    real, pointer :: mdata(:,:,:,:,:)
    real, pointer :: pdata(:,:,:,:,:)
    integer , dimension(5) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.5) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.5) then
       do i=6,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.5) then
       write(msg,*) 'In FORTRAN: you passed a 5d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,5
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 5d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5)))
       pdata=>mdata
       do m=1,tshape(5)
          do l=1,tshape(4)
             do k=1,tshape(3)
                do j=1,tshape(2)
                   do i=1,tshape(1)
                      pdata(i,j,k,l,m) = data(i,j,k,l,m)
                   end do
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_5d_r
  function cmor_ftn_map_data_6d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:,:,:,:,:,:)
    real, pointer :: mdata(:,:,:,:,:,:)
    real, pointer :: pdata(:,:,:,:,:,:)
    integer , dimension(6) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,n,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.6) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.6) then
       do i=7,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.6) then
       write(msg,*) 'In FORTRAN: you passed a 6d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,6
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 6d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5),tshape(6)))
       pdata=>mdata
       do n=1,tshape(6)
          do m=1,tshape(5)
             do l=1,tshape(4)
                do k=1,tshape(3)
                   do j=1,tshape(2)
                      do i=1,tshape(1)
                         pdata(i,j,k,l,m,n) = data(i,j,k,l,m,n)
                      end do
                   enddo
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_6d_r
  function cmor_ftn_map_data_7d_r(vshape,pdata,data,mdata,ntimes_passed) result(out)
    real, target:: data(:,:,:,:,:,:,:)
    real, pointer :: mdata(:,:,:,:,:,:,:)
    real, pointer :: pdata(:,:,:,:,:,:,:)
    integer , dimension(7) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,n,o,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.ne.7) then
       write(msg,*) 'In FORTRAN: you passed a 7d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,7
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 7d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5),tshape(6),tshape(7)))
       pdata=>mdata
       do o=1,tshape(7)
          do n=1,tshape(6)
             do m=1,tshape(5)
                do l=1,tshape(4)
                   do k=1,tshape(3)
                      do j=1,tshape(2)
                         do i=1,tshape(1)
                            pdata(i,j,k,l,m,n,o) = data(i,j,k,l,m,n,o)
                         end do
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_7d_r

  function cmor_ftn_map_data_1d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:)
    double precision, pointer :: mdata(:)
    double precision, pointer :: pdata(:)
    integer , dimension(1) :: dshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j
    integer out
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
!!$ Ok we need to make sure the 1D array is big enough

    j=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          j=j*ntimes_passed
       else
          j=j*vshape(i)
       endif
    enddo
    if (j.gt.dshape(1)) then
       j=CMOR_CRITICAL
       call cmor_handle_error('data passed to cmor_write to' &
            //'not contain enough data',j)
    endif
    pdata=>data
    out=0
  end function cmor_ftn_map_data_1d_d

  function cmor_ftn_map_data_2d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:,:)
    double precision, pointer :: mdata(:,:)
    double precision, pointer :: pdata(:,:)
    integer , dimension(2) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.2) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.2) then
       do i=3,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.2) then
       write(msg,*) 'In FORTRAN: you passed a 2d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,2
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 2d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2)))
       pdata=>mdata
       do j=1,tshape(2)
          do i=1,tshape(1)
             pdata(i,j) = data(i,j)
          end do
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_2d_d
  function cmor_ftn_map_data_3d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:,:,:)
    double precision, pointer :: mdata(:,:,:)
    double precision, pointer :: pdata(:,:,:)
    integer , dimension(3) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.3) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.3) then
       do i=4,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.3) then
        write(msg,*) 'In FORTRAN: you passed a 3d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,3
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 3d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3)))
       pdata=>mdata
       do k=1,tshape(3)
          do j=1,tshape(2)
             do i=1,tshape(1)
                pdata(i,j,k) = data(i,j,k)
             end do
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_3d_d
  function cmor_ftn_map_data_4d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:,:,:,:)
    double precision, pointer :: mdata(:,:,:,:)
    double precision, pointer :: pdata(:,:,:,:)
    integer , dimension(4) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.4) exit 
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.4) then
       do i=5,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.4) then
       write(msg,*) 'In FORTRAN: you passed a 4d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i = 1, 4
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 4d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4)))
       pdata=>mdata
       do l=1,tshape(4)
          do k=1,tshape(3)
             do j=1,tshape(2)
                do i=1,tshape(1)
                   pdata(i,j,k,l) = data(i,j,k,l)
                end do
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out=0
    endif
  end function cmor_ftn_map_data_4d_d
  function cmor_ftn_map_data_5d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:,:,:,:,:)
    double precision, pointer :: mdata(:,:,:,:,:)
    double precision, pointer :: pdata(:,:,:,:,:)
    integer , dimension(5) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.5) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.5) then
       do i=6,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.5) then
       write(msg,*) 'In FORTRAN: you passed a 5d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,5
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 5d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5)))
       pdata=>mdata
       do m=1,tshape(5)
          do l=1,tshape(4)
             do k=1,tshape(3)
                do j=1,tshape(2)
                   do i=1,tshape(1)
                      pdata(i,j,k,l,m) = data(i,j,k,l,m)
                   end do
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_5d_d
  function cmor_ftn_map_data_6d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:,:,:,:,:,:)
    double precision, pointer :: mdata(:,:,:,:,:,:)
    double precision, pointer :: pdata(:,:,:,:,:,:)
    integer , dimension(6) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,n,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (k.gt.6) exit
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.gt.6) then
       do i=7,ndims
          if ((vshape(i).eq.1).or.((vshape(i).eq.0).and.(ntimes_passed.eq.1))) ndims=ndims-1
       enddo
    endif
    if (ndims.ne.6) then
       write(msg,*) 'In FORTRAN: you passed a 6d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,6
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 6d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5),tshape(6)))
       pdata=>mdata
       do n=1,tshape(6)
          do m=1,tshape(5)
             do l=1,tshape(4)
                do k=1,tshape(3)
                   do j=1,tshape(2)
                      do i=1,tshape(1)
                         pdata(i,j,k,l,m,n) = data(i,j,k,l,m,n)
                      end do
                   enddo
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_6d_d
  function cmor_ftn_map_data_7d_d(vshape,pdata,data,mdata,ntimes_passed) result(out)
    double precision, target:: data(:,:,:,:,:,:,:)
    double precision, pointer :: mdata(:,:,:,:,:,:,:)
    double precision, pointer :: pdata(:,:,:,:,:,:,:)
    integer , dimension(7) :: dshape,tshape
    integer , dimension(7) :: vshape
    integer ndims
    integer i,j,k,l,m,n,o,error_code
    integer out
    character(600) msg
    j=1
    dshape = shape(data)
    ndims=0
    do i = 1, 7
       if (vshape(i).ne.-1) ndims=ndims+1
    end do
    
    k=1
    do i=1,ndims
       if (vshape(i).eq.0) then
          tshape(k)=ntimes_passed
       else 
          tshape(k)=vshape(i)
       endif
       if ((tshape(k).eq.1).and.(dshape(k).ne.1)) then
          ndims=ndims-1
       else
          k=k+1
       endif
       if (tshape(k).ne.dshape(k)) then
          j=0
       endif
    enddo
    if (ndims.ne.7) then
       write(msg,*) 'In FORTRAN: you passed a 7d array of shape:',dshape,'this does not'&
            //'match var number of dims: ',ndims,' please recheck your declaration,'&
            //' the expected shape was: ',tshape,char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    end if
    do i=1,7
       if (tshape(i).gt.dshape(i)) then
          write(msg, *)  'In FORTRAN: you passed a 7d array that'&
               //' is not big enough to hold your var, your data shape is:',&
               dshape,'expected shape was:',tshape,char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
    enddo
    if (j.eq.0) then
       allocate(mdata(tshape(1),tshape(2),tshape(3),tshape(4),tshape(5),tshape(6),tshape(7)))
       pdata=>mdata
       do o=1,tshape(7)
          do n=1,tshape(6)
             do m=1,tshape(5)
                do l=1,tshape(4)
                   do k=1,tshape(3)
                      do j=1,tshape(2)
                         do i=1,tshape(1)
                            pdata(i,j,k,l,m,n,o) = data(i,j,k,l,m,n,o)
                         end do
                      enddo
                   enddo
                enddo
             enddo
          enddo
       enddo
       out = 1
    else
       pdata=>data
       out =0
    endif

  end function cmor_ftn_map_data_7d_d


  function cmor_close(var_id,file_name, preserve) result(ierr)
    integer, optional :: var_id,preserve
    character (*), optional :: file_name
    integer ierr
    if (present(var_id)) then
       if (present(preserve)) then
          if (present(file_name)) then
             ierr = cmor_close_var_fnm_preserve_cff(var_id,file_name,&
                  preserve)
          else
             ierr = cmor_close_var_nofnm_preserve_cff(var_id,preserve)
          endif
       else
          if (present(file_name)) then
             ierr = cmor_close_var_fnm_cff(var_id,file_name)
          else
             ierr = cmor_close_var_nofnm_cff(var_id)
          endif
       endif
    else
       ierr = cmor_close_cff()
    endif
  end function cmor_close

  function cmor_set_grid_mapping(grid_id,mapping_name,parameter_names,&
       parameter_values,parameter_units) result(ierr)
    implicit none
    integer :: ierr,grid_id
    character(*) :: mapping_name
    character(*) :: parameter_names(:),parameter_units(:)
    double precision :: parameter_values(:)
    integer i,nparam,lparam,lunits
    character(len=1024),allocatable ::  paranm(:),paraun(:)
    nparam = size(parameter_values)
    lparam = 1024
    lunits = 1024
    allocate(paranm(nparam))
    allocate(paraun(nparam))
    do i = 1,nparam
       paranm(i) = trim(parameter_names(i))//char(0)
       paraun(i) = trim(parameter_units(i))//char(0)
    enddo
    
    ierr = cmor_set_grd_map_cff(grid_id,trim(mapping_name)//char(0),nparam,&
                                paranm(1), lparam, &
                                parameter_values(1), &
                                paraun(1), lunits)
    deallocate(paranm)
    deallocate(paraun)
  end function cmor_set_grid_mapping


  function cmor_grid_tvc_r(grid_id,table_entry,units,missing) result (ierr)
    implicit none
    character (*) table_entry,units
    integer var_id,grid_id
    integer ierr
    REAL,optional:: missing

    if (present(missing)) then
       ierr = cmor_grid_cff_tvc_f(var_id,grid_id,trim(table_entry)//char(0),trim(units)//char(0),missing)
    else
       ierr = cmor_grid_cff_tvc_no(var_id,grid_id,trim(table_entry)//char(0),trim(units)//char(0))
    end if

    if (ierr==0) then
       ierr = var_id
    else
       ierr = -ierr
    end if
  end function cmor_grid_tvc_r
  function cmor_grid_tvc_d(grid_id,table_entry,units,missing) result (ierr)
    implicit none
    character (*) table_entry,units
    integer var_id,grid_id
    integer ierr
    double precision missing

    ierr = cmor_grid_cff_tvc_d(var_id,grid_id,trim(table_entry)//char(0),trim(units)//char(0),missing)

    if (ierr==0) then
       ierr = var_id
    else
       ierr = -ierr
    end if
  end function cmor_grid_tvc_d
  function cmor_grid_tvc_i(grid_id,table_entry,units,missing) result (ierr)
    implicit none
    character (*) table_entry,units
    integer var_id,grid_id
    integer ierr
    integer missing

    ierr = cmor_grid_cff_tvc_i(var_id,grid_id,trim(table_entry)//char(0),trim(units)//char(0),missing)

    if (ierr==0) then
       ierr = var_id
    else
       ierr = -ierr
    end if
  end function cmor_grid_tvc_i

  function cmor_grid_0d(axis_ids,nvertices) result (ierr)
    implicit none
    integer ierr,i,nvert
    integer axis_ids(:)
    integer ndims,grid_id
    integer, dimension(10)::  axes_ids
    integer, optional :: nvertices

    if (present(nvertices)) then
       nvert = nvertices
    else
       nvert = 0
    end if
    ndims = size(axis_ids,1)
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    ierr = cmor_grid_cff_nocoords(grid_id,ndims,axes_ids(1),nvert)
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_0d

  function cmor_grid_1d_r(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    REAL latitude(:),longitude(:)
    REAL,OPTIONAL :: latitude_vertices(:,:)
    REAL,OPTIONAL :: longitude_vertices(:,:)
!!$    REAL,OPTIONAL :: area(:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 1
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,latitude_vertices(1,1), &
!!$                  longitude_vertices(1,1), area(1))
!!$          else
             ierr = cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,latitude_vertices(1,1), &
                  longitude_vertices(1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,latitude_vertices(1,1), &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,latitude_vertices(1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,longitude_vertices(1,1), &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,longitude_vertices(1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert, &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_1d_r
  function cmor_grid_2d_r(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    REAL latitude(:,:),longitude(:,:)
    REAL,OPTIONAL :: latitude_vertices(:,:,:)
    REAL,OPTIONAL :: longitude_vertices(:,:,:)
!!$    REAL,OPTIONAL :: area(:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 2
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,latitude_vertices(1,1,1), &
!!$                  longitude_vertices(1,1,1), area(1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,latitude_vertices(1,1,1), &
                  longitude_vertices(1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,latitude_vertices(1,1,1), &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,latitude_vertices(1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,longitude_vertices(1,1,1), &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,longitude_vertices(1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert, &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_2d_r
  function cmor_grid_3d_r(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    REAL latitude(:,:,:),longitude(:,:,:)
    REAL,OPTIONAL :: latitude_vertices(:,:,:,:)
    REAL,OPTIONAL :: longitude_vertices(:,:,:,:)
!!$    REAL,OPTIONAL :: area(:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 3
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1), area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,latitude_vertices(1,1,1,1), &
                  longitude_vertices(1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1), &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,latitude_vertices(1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1), &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,longitude_vertices(1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_3d_r
  function cmor_grid_4d_r(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    REAL latitude(:,:,:,:),longitude(:,:,:,:)
    REAL,OPTIONAL :: latitude_vertices(:,:,:,:,:)
    REAL,OPTIONAL :: longitude_vertices(:,:,:,:,:)
!!$    REAL,OPTIONAL :: area(:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 4
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1), area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1), &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1), &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_4d_r
  function cmor_grid_5d_r(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    REAL latitude(:,:,:,:,:),longitude(:,:,:,:,:)
    REAL,OPTIONAL :: latitude_vertices(:,:,:,:,:,:)
    REAL,OPTIONAL :: longitude_vertices(:,:,:,:,:,:)
!!$    REAL,OPTIONAL :: area(:,:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 5
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1,1), area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_5d_r
  function cmor_grid_6d_r(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    REAL latitude(:,:,:,:,:,:),longitude(:,:,:,:,:,:)
    REAL,OPTIONAL :: latitude_vertices(:,:,:,:,:,:,:)
    REAL,OPTIONAL :: longitude_vertices(:,:,:,:,:,:,:)
!!$    REAL,OPTIONAL :: area(:,:,:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 6
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1,1,1), area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_real(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_real(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_6d_r


  function cmor_grid_1d_d(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    DOUBLE PRECISION latitude(:),longitude(:)
    DOUBLE PRECISION,OPTIONAL :: latitude_vertices(:,:)
    DOUBLE PRECISION,OPTIONAL :: longitude_vertices(:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 1
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,latitude_vertices(1,1), &
!!$                  longitude_vertices(1,1), area(1))
!!$          else
             ierr = cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,latitude_vertices(1,1), &
                  longitude_vertices(1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,latitude_vertices(1,1), &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,latitude_vertices(1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,longitude_vertices(1,1), &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,longitude_vertices(1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert, &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_1d_d
  function cmor_grid_2d_d(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    DOUBLE PRECISION latitude(:,:),longitude(:,:)
    DOUBLE PRECISION,OPTIONAL :: latitude_vertices(:,:,:)
    DOUBLE PRECISION,OPTIONAL :: longitude_vertices(:,:,:)
!!$    DOUBLE PRECISION,OPTIONAL :: area(:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 2
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,latitude_vertices(1,1,1), &
!!$                  longitude_vertices(1,1,1), area(1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,latitude_vertices(1,1,1), &
                  longitude_vertices(1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,latitude_vertices(1,1,1), &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,latitude_vertices(1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,longitude_vertices(1,1,1), &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,longitude_vertices(1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert, &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_2d_d
  function cmor_grid_3d_d(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    DOUBLE PRECISION latitude(:,:,:),longitude(:,:,:)
    DOUBLE PRECISION,OPTIONAL :: latitude_vertices(:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: longitude_vertices(:,:,:,:)
!!$    DOUBLE PRECISION,OPTIONAL :: area(:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 3
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1), area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,latitude_vertices(1,1,1,1), &
                  longitude_vertices(1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1), &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,latitude_vertices(1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1), &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,longitude_vertices(1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_3d_d
  function cmor_grid_4d_d(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    DOUBLE PRECISION latitude(:,:,:,:),longitude(:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: latitude_vertices(:,:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: longitude_vertices(:,:,:,:,:)
!!$    DOUBLE PRECISION,OPTIONAL :: area(:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 4
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1), area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1), &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1), &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_4d_d
  function cmor_grid_5d_d(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    DOUBLE PRECISION latitude(:,:,:,:,:),longitude(:,:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: latitude_vertices(:,:,:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: longitude_vertices(:,:,:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 5
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1,1), area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_5d_d
  function cmor_grid_6d_d(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    DOUBLE PRECISION latitude(:,:,:,:,:,:),longitude(:,:,:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: latitude_vertices(:,:,:,:,:,:,:)
    DOUBLE PRECISION,OPTIONAL :: longitude_vertices(:,:,:,:,:,:,:)
!!$    DOUBLE PRECISION,OPTIONAL :: area(:,:,:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 6
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1,1,1), area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_double(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_double(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_6d_d





  function cmor_grid_1d_i(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    INTEGER latitude(:),longitude(:)
    INTEGER,OPTIONAL :: latitude_vertices(:,:)
    INTEGER,OPTIONAL :: longitude_vertices(:,:)
!!$    INTEGER,OPTIONAL :: area(:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 1
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,latitude_vertices(1,1), &
!!$                  longitude_vertices(1,1), area(1))
!!$          else
             ierr = cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,latitude_vertices(1,1), &
                  longitude_vertices(1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,latitude_vertices(1,1), &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,latitude_vertices(1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert,longitude_vertices(1,1), &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert,longitude_vertices(1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1), longitude(1), &
!!$                  nvert, &
!!$                  area(1))
!!$          else
             ierr = cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids(1),&
                  latitude(1), longitude(1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_1d_i
  function cmor_grid_2d_i(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    INTEGER latitude(:,:),longitude(:,:)
    INTEGER,OPTIONAL :: latitude_vertices(:,:,:)
    INTEGER,OPTIONAL :: longitude_vertices(:,:,:)
!!$    INTEGER,OPTIONAL :: area(:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 2
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,latitude_vertices(1,1,1), &
!!$                  longitude_vertices(1,1,1), area(1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,latitude_vertices(1,1,1), &
                  longitude_vertices(1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,latitude_vertices(1,1,1), &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,latitude_vertices(1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert,longitude_vertices(1,1,1), &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert,longitude_vertices(1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1), longitude(1,1), &
!!$                  nvert, &
!!$                  area(1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1), longitude(1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_2d_i
  function cmor_grid_3d_i(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    INTEGER latitude(:,:,:),longitude(:,:,:)
    INTEGER,OPTIONAL :: latitude_vertices(:,:,:,:)
    INTEGER,OPTIONAL :: longitude_vertices(:,:,:,:)
!!$    INTEGER,OPTIONAL :: area(:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 3
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1), area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,latitude_vertices(1,1,1,1), &
                  longitude_vertices(1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1), &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,latitude_vertices(1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1), &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert,longitude_vertices(1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1), longitude(1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1), longitude(1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_3d_i
  function cmor_grid_4d_i(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    INTEGER latitude(:,:,:,:),longitude(:,:,:,:)
    INTEGER,OPTIONAL :: latitude_vertices(:,:,:,:,:)
    INTEGER,OPTIONAL :: longitude_vertices(:,:,:,:,:)
!!$    INTEGER,OPTIONAL :: area(:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 4
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1), area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1), &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1), &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1), longitude(1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1), longitude(1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_4d_i
  function cmor_grid_5d_i(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    INTEGER latitude(:,:,:,:,:),longitude(:,:,:,:,:)
    INTEGER,OPTIONAL :: latitude_vertices(:,:,:,:,:,:)
    INTEGER,OPTIONAL :: longitude_vertices(:,:,:,:,:,:)
!!$    INTEGER,OPTIONAL :: area(:,:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 5
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1,1), area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1), longitude(1,1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_5d_i
  function cmor_grid_6d_i(axis_ids,latitude,longitude,&
       latitude_vertices,longitude_vertices) result(ierr)
    implicit none
    integer axis_ids(:)
    INTEGER latitude(:,:,:,:,:,:),longitude(:,:,:,:,:,:)
    INTEGER,OPTIONAL :: latitude_vertices(:,:,:,:,:,:,:)
    INTEGER,OPTIONAL :: longitude_vertices(:,:,:,:,:,:,:)
!!$    INTEGER,OPTIONAL :: area(:,:,:,:,:,:)
    integer ierr,grid_id,ndims,nvert,error_code
    character(1024) msg
    integer i
    integer, dimension(10)::  axes_ids
    
    ndims = 6
    do i = 1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    if (present(latitude_vertices)) then
       nvert = size(latitude_vertices,1)
       if (present(longitude_vertices)) then
          if (size(longitude_vertices,1).ne.nvert) then
             msg = 'cmor_grid (fortran):: longitudes and latitudes'//&
                  'vertices do not have the same number of vertices'//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          endif
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
!!$                  longitude_vertices(1,1,1,1,1,1,1), area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noarea_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
                  longitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       else
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblon_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,latitude_vertices(1,1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nobloar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,latitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       endif
    else
       if (present(longitude_vertices)) then
          nvert = size(latitude_vertices,1)
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_noblat_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert,longitude_vertices(1,1,1,1,1,1,1), &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_noblaar_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert,longitude_vertices(1,1,1,1,1,1,1))
!!$          endif
       else
          nvert=0
!!$          if (present(area)) then
!!$             ierr = cmor_grid_cff_nobnds_int(grid_id,ndims,axes_ids(1),&
!!$                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
!!$                  nvert, &
!!$                  area(1,1,1,1,1,1))
!!$          else
             ierr = cmor_grid_cff_nothg_int(grid_id,ndims,axes_ids(1),&
                  latitude(1,1,1,1,1,1), longitude(1,1,1,1,1,1), &
                  nvert)
!!$          endif
       endif
    endif
    if (ierr.eq.0) then
       ierr = grid_id
    endif
  end function cmor_grid_6d_i


  FUNCTION cmor_write_1d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL, target :: data(:)
    REAL, pointer :: mdata(:)
    real, pointer :: pdata(:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
    
  END FUNCTION cmor_write_1d_r
  FUNCTION cmor_write_2d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL,target :: data(:,:)
    REAL,pointer :: mdata(:,:)
    REAL, pointer :: pdata(:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
    
  END FUNCTION cmor_write_2d_r
  FUNCTION cmor_write_3d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL, target :: data(:,:,:)
    REAL, pointer :: mdata(:,:,:)
    REAL, pointer :: pdata(:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf

    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
       nullify(pdata)
   
  END FUNCTION cmor_write_3d_r
  FUNCTION cmor_write_4d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL, target :: data(:,:,:,:)
    REAL, pointer :: mdata(:,:,:,:)
    REAL, pointer :: pdata(:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,vshape(7),did_malloc
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
       nullify(pdata)
  END FUNCTION cmor_write_4d_r
  FUNCTION cmor_write_5d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL, target :: data(:,:,:,:,:)
    REAL, pointer :: mdata(:,:,:,:,:)
    REAL, pointer :: pdata(:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
    
  END FUNCTION cmor_write_5d_r
  FUNCTION cmor_write_6d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL, target :: data(:,:,:,:,:,:)
    REAL, pointer :: mdata(:,:,:,:,:,:)
    REAL, pointer :: pdata(:,:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif

    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
   
  END FUNCTION cmor_write_6d_r
  FUNCTION cmor_write_7d_r(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    REAL, target :: data(:,:,:,:,:,:,:)
    REAL, pointer :: mdata(:,:,:,:,:,:,:)
    REAL, pointer :: pdata(:,:,:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_real(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_real_nobnds(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_real_notime(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(pdata)
    endif
    nullify(pdata)
  END FUNCTION cmor_write_7d_r

  FUNCTION cmor_write_1d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION,target :: data(:)
    DOUBLE PRECISION,pointer :: mdata(:)
    DOUBLE PRECISION, pointer:: pdata(:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,vshape(7),did_malloc
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,1)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,pdata(1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,pdata(1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,pdata(1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    end if
    nullify(pdata)
  END FUNCTION cmor_write_1d_d
  FUNCTION cmor_write_2d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION,target :: data(:,:)
    DOUBLE PRECISION,pointer :: mdata(:,:)
    DOUBLE PRECISION, pointer :: pdata(:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,data(1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,data(1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,data(1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
   
  END FUNCTION cmor_write_2d_d
  FUNCTION cmor_write_3d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION , target:: data(:,:,:)
    DOUBLE PRECISION , pointer:: mdata(:,:,:)
    DOUBLE PRECISION, pointer :: pdata(:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,1)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,pdata(1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,pdata(1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,pdata(1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
  END FUNCTION cmor_write_3d_d

  FUNCTION cmor_write_4d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION, INTENT(IN), target :: data(:,:,:,:)
    DOUBLE PRECISION, pointer :: mdata(:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp
    character(1024) suf
    integer   vshape(7),did_malloc
    double precision, pointer :: pdata(:,:,:,:)

    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,pdata(1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,pdata(1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,pdata(1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) deallocate(mdata)
    nullify(pdata)
  END FUNCTION cmor_write_4d_d
  FUNCTION cmor_write_5d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION, target :: data(:,:,:,:,:)
    DOUBLE PRECISION, pointer :: mdata(:,:,:,:,:)
    DOUBLE PRECISION, pointer :: pdata(:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,1)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,pdata(1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,pdata(1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,pdata(1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
  END FUNCTION cmor_write_5d_d
  FUNCTION cmor_write_6d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION,target :: data(:,:,:,:,:,:)
    DOUBLE PRECISION,pointer :: mdata(:,:,:,:,:,:)
    DOUBLE PRECISION, pointer :: pdata(:,:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(pdata)
    endif
    nullify(pdata)
    
  END FUNCTION cmor_write_6d_d
  FUNCTION cmor_write_7d_d(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    DOUBLE PRECISION, target :: data(:,:,:,:,:,:,:)
    DOUBLE PRECISION, pointer :: mdata(:,:,:,:,:,:,:)
    DOUBLE PRECISION, pointer :: pdata(:,:,:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_double(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_double_nobnds(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_double_notime(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
  END FUNCTION cmor_write_7d_d

  FUNCTION cmor_write_1d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER, target :: data(:)
    INTEGER, pointer :: mdata(:)
    INTEGER, pointer :: pdata(:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,pdata(1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,pdata(1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,pdata(1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
       
  END FUNCTION cmor_write_1d_i
  FUNCTION cmor_write_2d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER,target :: data(:,:)
    INTEGER,pointer :: mdata(:,:)
    INTEGER, pointer :: pdata(:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,data(1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,data(1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,data(1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
    
  END FUNCTION cmor_write_2d_i
  FUNCTION cmor_write_3d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER, target :: data(:,:,:)
    INTEGER, pointer :: mdata(:,:,:)
    INTEGER, pointer :: pdata(:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,pdata(1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,pdata(1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,pdata(1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
   
  END FUNCTION cmor_write_3d_i
  FUNCTION cmor_write_4d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER, target :: data(:,:,:,:)
    INTEGER, pointer :: mdata(:,:,:,:)
    INTEGER, pointer :: pdata(:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,pdata(1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,pdata(1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,pdata(1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
   
  END FUNCTION cmor_write_4d_i
  FUNCTION cmor_write_5d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER , target:: data(:,:,:,:,:)
    INTEGER , pointer:: mdata(:,:,:,:,:)
    INTEGER, pointer :: pdata(:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,pdata(1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,pdata(1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,pdata(1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)
  END FUNCTION cmor_write_5d_i
  FUNCTION cmor_write_6d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER,target :: data(:,:,:,:,:,:)
    INTEGER, pointer :: mdata(:,:,:,:,:,:)
    INTEGER, pointer :: pdata(:,:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,pdata(1,1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    end if
    nullify(pdata)
       
  END FUNCTION cmor_write_6d_i
  FUNCTION cmor_write_7d_i(var_id, data, file_suffix, ntimes_passed,  &
        time_vals,time_bnds,store_with) RESULT(ierr)
    implicit none
    INTEGER, target :: data(:,:,:,:,:,:,:)
    INTEGER, pointer :: mdata(:,:,:,:,:,:,:)
    INTEGER, pointer :: pdata(:,:,:,:,:,:,:)
    INTEGER, INTENT(in) ::var_id
    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
    INTEGER, INTENT(in), OPTIONAL :: store_with
    integer refvar,ierr,ntp,did_malloc,vshape(7)
    character(1024) suf
    call cmor_get_original_shape_cff(var_id,vshape(1))
    if (present(ntimes_passed)) then 
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ntimes_passed)
    else
       ierr = cmor_ftn_get_tlen_cff(var_id)
       did_malloc = cmor_ftn_map_data(vshape,pdata,data,mdata,ierr)
    endif
    if (present(file_suffix)) then
       suf = trim(file_suffix)//char(0)
    else
       suf = char(0)
    endif

    if (present(store_with)) then
       refvar = store_with
    else
       refvar = -1 ! means do not pass it
    end if
    if (present(ntimes_passed)) then
       ntp = ntimes_passed
    else
       ntp=0
    endif
    if (present(time_vals)) then
       if (ntp==0) ntp = size(time_vals)
       if (present(time_bnds)) then
          ierr = cmor_write_cff_int(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
               time_vals(1),time_bnds(1,1),refvar)
       else
          ierr = cmor_write_cff_int_nobnds(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
               time_vals(1),refvar)
       end if
    else
       ierr = cmor_write_cff_int_notime(var_id,pdata(1,1,1,1,1,1,1),suf,ntp,&
            refvar)
    endif
    if (did_malloc.eq.1) then
       deallocate(mdata)
    endif
    nullify(pdata)

  END FUNCTION cmor_write_7d_i

!!$  FUNCTION cmor_write_1d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_1d_l
!!$  FUNCTION cmor_write_2d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:,:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1,1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1,1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1,1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_2d_l
!!$  FUNCTION cmor_write_3d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:,:,:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1,1,1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1,1,1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1,1,1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_3d_l
!!$  FUNCTION cmor_write_4d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:,:,:,:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1,1,1,1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1,1,1,1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1,1,1,1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_4d_l
!!$  FUNCTION cmor_write_5d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:,:,:,:,:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1,1,1,1,1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1,1,1,1,1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1,1,1,1,1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_5d_l
!!$  FUNCTION cmor_write_6d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:,:,:,:,:,:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1,1,1,1,1,1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1,1,1,1,1,1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1,1,1,1,1,1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_6d_l
!!$  FUNCTION cmor_write_7d_l(var_id, data, file_suffix, ntimes_passed,  &
!!$        time_vals,time_bnds,store_with) RESULT(ierr)
!!$    implicit none
!!$    INTEGER (KIND=8), INTENT(IN) :: data(:,:,:,:,:,:,:)
!!$    INTEGER, INTENT(in) ::var_id
!!$    CHARACTER(len=*), INTENT(in), OPTIONAL :: file_suffix
!!$    INTEGER, INTENT(in), OPTIONAL :: ntimes_passed
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_vals(:)
!!$    DOUBLE PRECISION, INTENT(in), OPTIONAL :: time_bnds(:,:)
!!$    INTEGER, INTENT(in), OPTIONAL :: store_with
!!$    integer refvar,ierr,n(1),ntp
!!$    character(1024) suf
!!$    if (present(file_suffix)) then
!!$       suf = trim(file_suffix)//char(0)
!!$    else
!!$       suf = char(0)
!!$    endif
!!$
!!$    if (present(store_with)) then
!!$       refvar = store_with
!!$    else
!!$       refvar = -1 ! means do not pass it
!!$    end if
!!$    if (present(ntimes_passed)) then
!!$       ntp = ntimes_passed
!!$    else
!!$       ntp=0
!!$    endif
!!$    if (present(time_vals)) then
!!$       if (ntp==0) ntp = size(time_vals)
!!$       if (present(time_bnds)) then
!!$          ierr = cmor_write_cff_long(var_id,data(1,1,1,1,1,1,1),suf,ntp,&
!!$               time_vals(1),time_bnds(1,1),refvar)
!!$       else
!!$          ierr = cmor_write_cff_long_nobnds(var_id,data(1,1,1,1,1,1,1),suf,ntp,&
!!$               time_vals(1),refvar)
!!$       end if
!!$    else
!!$       ierr = cmor_write_cff_long_notime(var_id,data(1,1,1,1,1,1,1),suf,ntp,&
!!$            refvar)
!!$    endif
!!$  END FUNCTION cmor_write_7d_l

  function cmor_variable_double(table,table_entry,units,axis_ids,missing_value,&
       tolerance,positive,original_name,history,comment) result(ierr)
    implicit none
    character(*),optional ::table,positive,original_name,history,comment
    character(*) table_entry,units
    integer axis_ids(:),ierr,l,var_id
    double precision ::  missing_value, tol
    double precision, optional ::  tolerance
    character(1024) pos,onm,hist,com,tbl,unit
    integer i
    integer, dimension(10) :: axes_ids
    l = size(axis_ids)
    do i=1, l
       axes_ids(i) = axis_ids(l-i+1)
    enddo
    unit = trim(units)//char(0)
    tbl = trim(table_entry)//char(0)

    if (present(table)) then
       i = cmor_load_table(table)
    endif

    if (present(tolerance)) then
       tol = tolerance
    else
       tol = 1.e-4
    end if
    
    if (present(positive)) then
       pos = trim(positive)//char(0)
    else
       pos = char(0)
    end if
    if (present(original_name)) then
       onm = trim(original_name)//char(0)
    else
       onm = char(0)
    end if

    if (present(history)) then
       hist = trim(history)//char(0)
    else
       hist = char(0)
    end if
    if (present(comment)) then
       com = trim(comment)//char(0)
    else
       com = char(0)
    end if
    
    
    ierr = cmor_variable_cff_double(var_id,tbl,unit,l,axes_ids(1),missing_value,tol,pos,onm,hist,com)

    if (ierr.eq.0) then
       ierr = var_id
    else
       ierr = -ierr
    end if
  end function cmor_variable_double
  function cmor_variable_real(table,table_entry,units,axis_ids,missing_value,&
       tolerance,positive,original_name,history,comment) result(ierr)
    implicit none
    character(*),optional ::table,positive,original_name,history,comment
    character(*) table_entry,units
    integer axis_ids(:),ierr,l,var_id
    double precision ::  tol
    real, optional ::  missing_value
    double precision, optional ::  tolerance
    character(1024) pos,onm,hist,com,tbl,unit
    integer i
    integer, dimension(10) :: axes_ids

    l = size(axis_ids)
    do i=1, l
       axes_ids(i) = axis_ids(l-i+1)
    enddo

    unit = trim(units)//char(0)
    tbl = trim(table_entry)//char(0)

    if (present(table)) then
       i = cmor_load_table(table)
    endif

    if (present(tolerance)) then
       tol = tolerance
    else
       tol = 1.e-4
    end if
    
    if (present(positive)) then
       pos = trim(positive)//char(0)
    else
       pos = char(0)
    end if
    if (present(original_name)) then
       onm = trim(original_name)//char(0)
    else
       onm = char(0)
    end if

    if (present(history)) then
       hist = trim(history)//char(0)
    else
       hist = char(0)
    end if
    if (present(comment)) then
       com = trim(comment)//char(0)
    else
       com = char(0)
    end if
    
    l = size(axis_ids)
    
    if (present(missing_value)) then
       ierr = cmor_variable_cff_real(var_id,tbl,unit,l,axes_ids(1),missing_value,tol,pos,onm,hist,com)
    else 
       ierr = cmor_variable_cff_nomiss(var_id,tbl,unit,l,axes_ids(1),tol,pos,onm,hist,com)
    endif

    if (ierr.eq.0) then
       ierr = var_id
    else
       ierr = -ierr
    end if
  end function cmor_variable_real
  function cmor_variable_int(table,table_entry,units,axis_ids,missing_value,&
       tolerance,positive,original_name,history,comment) result(ierr)
    implicit none
    character(*),optional ::table,positive,original_name,history,comment
    character(*) table_entry,units
    integer axis_ids(:),ierr,l,var_id
    double precision ::  tol
    integer ::  missing_value
    double precision, optional ::  tolerance
    character(1024) pos,onm,hist,com,tbl,unit
    integer i
    integer, dimension(10) :: axes_ids
    l = size(axis_ids)
    do i=1, l
       axes_ids(i) = axis_ids(l-i+1)
    enddo

    unit = trim(units)//char(0)
    tbl = trim(table_entry)//char(0)

    if (present(table)) then
       i = cmor_load_table(table)
    endif

    if (present(tolerance)) then
       tol = tolerance
    else
       tol = 1.e-4
    end if
    
    if (present(positive)) then
       pos = trim(positive)//char(0)
    else
       pos = char(0)
    end if
    if (present(original_name)) then
       onm = trim(original_name)//char(0)
    else
       onm = char(0)
    end if

    if (present(history)) then
       hist = trim(history)//char(0)
    else
       hist = char(0)
    end if
    if (present(comment)) then
       com = trim(comment)//char(0)
    else
       com = char(0)
    end if
    
    l = size(axis_ids)
    
    ierr = cmor_variable_cff_int(var_id,tbl,unit,l,axes_ids(1),missing_value,tol,pos,onm,hist,com)

    if (ierr.eq.0) then
       ierr = var_id
    else
       ierr = -ierr
    end if
  end function cmor_variable_int
!!$  function cmor_variable_long(table,table_entry,units,axis_ids,missing_value,&
!!$       tolerance,positive,original_name,history,comment) result(ierr)
!!$    implicit none
!!$    character(*),optional ::table,positive,original_name,history,comment
!!$    character(*) table_entry,units
!!$    integer axis_ids(:),ierr,l,var_id
!!$    double precision ::  tol
!!$    integer (kind=8) ::  missing_value
!!$    double precision, optional ::  tolerance
!!$    character(1024) pos,onm,hist,com,tbl,unit
!!$    integer i
!!$    integer, dimension(10) :: axes_ids
!!$    l = size(axis_ids)
!!$    do i=1, l
!!$       axes_ids(i) = axis_ids(l-i+1)
!!$    enddo
!!$
!!$    unit = trim(units)//char(0)
!!$    tbl = trim(table_entry)//char(0)
!!$
!!$    if (present(table)) then
!!$       l = cmor_load_table(table)
!!$    endif
!!$
!!$    if (present(tolerance)) then
!!$       tol = tolerance
!!$    else
!!$       tol = 1.e-4
!!$    end if
!!$    
!!$    if (present(positive)) then
!!$       pos = trim(positive)//char(0)
!!$    else
!!$       pos = char(0)
!!$    end if
!!$    if (present(original_name)) then
!!$       onm = trim(original_name)//char(0)
!!$    else
!!$       onm = char(0)
!!$    end if
!!$
!!$    if (present(history)) then
!!$       hist = trim(history)//char(0)
!!$    else
!!$       hist = char(0)
!!$    end if
!!$    if (present(comment)) then
!!$       com = trim(comment)//char(0)
!!$    else
!!$       com = char(0)
!!$    end if
!!$    
!!$    l = size(axis_ids)
!!$    
!!$    ierr = cmor_variable_cff_long(var_id,tbl,unit,l,axes_ids(1),missing_value,tol,pos,onm,hist,com)
!!$
!!$    if (ierr.eq.0) then
!!$       ierr = var_id
!!$    else
!!$       ierr = -ierr
!!$    end if
!!$  end function cmor_variable_long

  function cmor_zfactor_novals(zaxis_id,zfactor_name,axis_ids,&
       units) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in) :: axis_ids(:)
    character(*) , optional :: units
    integer i,ierr,ndims,zfactor
    character(1024) unit,zfnm
    integer, dimension(10) :: axes_ids
    ndims = size(axis_ids)
    do i=1, ndims
       axes_ids(i) = axis_ids(ndims-i+1)
    enddo
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    ierr = cmor_zfactor_cff_novals(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1))
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_novals
  function cmor_zfactor_double(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values(:)
    double precision, optional :: zfactor_bounds(:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       if (size(zfactor_bounds).ne.size(zfactor_values)+1) then
          msg = 'cmor_zfactor(fortran) zfactor_bounds length is not zfactor_values + 1 for zfactor:'//zfactor_name//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1),zfactor_bounds(1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double
  function cmor_zfactor_double_2dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values(:,:)
    double precision, optional :: zfactor_bounds(:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1),zfactor_bounds(1,1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double_2dvalues
  function cmor_zfactor_double_3dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values(:,:,:)
    double precision, optional :: zfactor_bounds(:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1),zfactor_bounds(1,1,1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double_3dvalues
  function cmor_zfactor_double_4dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values(:,:,:,:)
    double precision, optional :: zfactor_bounds(:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1,1),zfactor_bounds(1,1,1,1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double_4dvalues
  function cmor_zfactor_double_5dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values(:,:,:,:,:)
    double precision, optional :: zfactor_bounds(:,:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1),zfactor_bounds(1,1,1,1,1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double_5dvalues
  function cmor_zfactor_double_6dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values(:,:,:,:,:,:)
    double precision, optional :: zfactor_bounds(:,:,:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1,1),zfactor_bounds(1,1,1,1,1,1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double_6dvalues
  function cmor_zfactor_double_0dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    double precision :: zfactor_values
    double precision :: zfactor_value(1)
    double precision, optional :: zfactor_bounds(2)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    zfactor_value(1)=zfactor_values
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_double(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_value(1),zfactor_bounds(1))
    else
       ierr = cmor_zfactor_cff_double_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_value(1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_double_0dvalues
  function cmor_zfactor_real(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values(:)
    real, optional :: zfactor_bounds(:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran) :: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       if (size(zfactor_bounds).ne.size(zfactor_values)+1) then
          msg = 'cmor_zfactor(fortran) zfactor_bounds length is not zfactor_values + 1 for zfactor:'//zfactor_name//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1),zfactor_bounds(1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1))
    endif
     if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
 end function cmor_zfactor_real
   function cmor_zfactor_real_2dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values(:,:)
    real, optional :: zfactor_bounds(:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1),zfactor_bounds(1,1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_real_2dvalues
  function cmor_zfactor_real_3dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values(:,:,:)
    real, optional :: zfactor_bounds(:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1),zfactor_bounds(1,1,1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_real_3dvalues
  function cmor_zfactor_real_4dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values(:,:,:,:)
    real, optional :: zfactor_bounds(:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1),zfactor_bounds(1,1,1,1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_real_4dvalues
  function cmor_zfactor_real_5dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values(:,:,:,:,:)
    real, optional :: zfactor_bounds(:,:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1),zfactor_bounds(1,1,1,1,1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_real_5dvalues
  function cmor_zfactor_real_6dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values(:,:,:,:,:,:)
    real, optional :: zfactor_bounds(:,:,:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1,1),zfactor_bounds(1,1,1,1,1,1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_real_6dvalues
 function cmor_zfactor_real_0dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    real :: zfactor_values
    real :: zfactor_value(1)
    real, optional :: zfactor_bounds(2)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    zfactor_value(1)=zfactor_values
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_real(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_value(1),zfactor_bounds(1))
    else
       ierr = cmor_zfactor_cff_real_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_value(1))
    endif
     if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
 end function cmor_zfactor_real_0dvalues
  function cmor_zfactor_int(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values(:)
    integer, optional :: zfactor_bounds(:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor (fortran) ::ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       if (size(zfactor_bounds).ne.size(zfactor_values)+1) then
          msg = 'cmor_zfactor(fortran) zfactor_bounds length is not zfactor_values + 1 for zfactor:'//zfactor_name//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1),zfactor_bounds(1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int
   function cmor_zfactor_int_2dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values(:,:)
    integer, optional :: zfactor_bounds(:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1),zfactor_bounds(1,1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int_2dvalues
  function cmor_zfactor_int_3dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values(:,:,:)
    integer, optional :: zfactor_bounds(:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1),zfactor_bounds(1,1,1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int_3dvalues
  function cmor_zfactor_int_4dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values(:,:,:,:)
    integer, optional :: zfactor_bounds(:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1,1),zfactor_bounds(1,1,1,1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int_4dvalues
  function cmor_zfactor_int_5dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values(:,:,:,:,:)
    integer, optional :: zfactor_bounds(:,:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1),zfactor_bounds(1,1,1,1,1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int_5dvalues
  function cmor_zfactor_int_6dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values(:,:,:,:,:,:)
    integer, optional :: zfactor_bounds(:,:,:,:,:,:)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor (fortran):: you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       if (size(zfactor_values).ne.1) then
          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1,1),zfactor_bounds(1,1,1,1,1,1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_values(1,1,1,1,1,1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int_6dvalues
  function cmor_zfactor_int_0dvalues(zaxis_id,zfactor_name,axis_ids,&
       units,zfactor_values,zfactor_bounds) result(ierr)
    implicit none
    integer zaxis_id
    character(*) zfactor_name
    integer, intent(in), optional :: axis_ids(:)
    character(*) , optional :: units
    integer :: zfactor_values
    integer :: zfactor_value(1)
    integer, optional :: zfactor_bounds(2)
    integer ierr,ndims,zfactor
    character(1024) msg,unit,zfnm
    integer axes_ids(1)
    integer error_code
    
    zfnm = trim(zfactor_name)//char(0)
    zfactor_value(1)=zfactor_values
    if (present(axis_ids)) then
       ndims = size(axis_ids)
       if (ndims.ne.1) then
          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       endif
       axes_ids(1)=axis_ids(1)
    else
       ndims = 0
       axes_ids(1)=0
    endif
    if (present(units)) then
       unit = trim(units)//char(0)
    else 
       unit=char(0)
    endif
    if (present(zfactor_bounds)) then
       ierr = cmor_zfactor_cff_int(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_value(1),zfactor_bounds(1))
    else
       ierr = cmor_zfactor_cff_int_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),&
            zfactor_value(1))
    endif
    if (ierr.eq.0) then
       ierr = zfactor
    else
       ierr = -ierr
    end if
  end function cmor_zfactor_int_0dvalues
!!$  function cmor_zfactor_long(zaxis_id,zfactor_name,axis_ids,&
!!$       units,zfactor_values,zfactor_bounds) result(ierr)
!!$    implicit none
!!$    integer zaxis_id
!!$    character(*) zfactor_name
!!$    integer, intent(in), optional :: axis_ids(:)
!!$    character(*) , optional :: units
!!$    integer(kind=8) :: zfactor_values(:)
!!$    integer(kind=8), optional :: zfactor_bounds(:)
!!$    integer i,ierr,ndims,zfactor
!!$    character(1024) msg,unit,zfnm
!!$    integer axes_ids(1)
!!$    integer error_code
!!$    
!!$    zfnm = trim(zfactor_name)//char(0)
!!$    if (present(axis_ids)) then
!!$       ndims = size(axis_ids)
!!$       if (ndims.ne.1) then
!!$          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
!!$               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
!!$          error_code = CMOR_CRITICAL
!!$          call cmor_handle_error(msg,error_code)
!!$       endif
!!$       axes_ids(1)=axis_ids(1)
!!$    else
!!$       ndims = 0
!!$       if (size(zfactor_values).ne.1) then
!!$          msg = 'cmor_zfactor(fortran):: ndims and zfactor_values length do not match'//char(0)
!!$          error_code = CMOR_CRITICAL
!!$          call cmor_handle_error(msg,error_code)
!!$       endif
!!$       axes_ids(1)=0
!!$    endif
!!$    if (present(units)) then
!!$       unit = trim(units)//char(0)
!!$    else 
!!$       unit=char(0)
!!$    endif
!!$    if (present(zfactor_bounds)) then
!!$       if (size(zfactor_bounds).ne.size(zfactor_values)+1) then
!!$          msg = 'cmor_zfactor(fortran) zfactor_bounds length is not zfactor_values + 1 for zfactor:'//zfactor_name//char(0)
!!$          error_code = CMOR_CRITICAL
!!$          call cmor_handle_error(msg,error_code)
!!$       endif
!!$       ierr = cmor_zfactor_cff_long(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1),zfactor_bounds(1))
!!$    else
!!$       ierr = cmor_zfactor_cff_long_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_values(1))
!!$    endif
!!$     if (ierr.eq.0) then
!!$       ierr = zfactor
!!$    else
!!$       ierr = -ierr
!!$    end if
!!$ end function cmor_zfactor_long
!!$  function cmor_zfactor_long_0dvalues(zaxis_id,zfactor_name,axis_ids,&
!!$       units,zfactor_values,zfactor_bounds) result(ierr)
!!$    implicit none
!!$    integer zaxis_id
!!$    character(*) zfactor_name
!!$    integer, intent(in), optional :: axis_ids(:)
!!$    character(*) , optional :: units
!!$    integer(kind=8) :: zfactor_values
!!$    integer(kind=8) :: zfactor_value(1)
!!$    integer(kind=8), optional :: zfactor_bounds(2)
!!$    integer i,ierr,ndims,zfactor
!!$    character(1024) msg,unit,zfnm
!!$    integer axes_ids(1)
!!$    integer error_code
!!$    
!!$    zfnm = trim(zfactor_name)//char(0)
!!$    zfactor_value(1)=zfactor_values
!!$    if (present(axis_ids)) then
!!$       ndims = size(axis_ids)
!!$       if (ndims.ne.1) then
!!$          msg = 'cmor_zfactor(fortran) you passed ndims greater than 1 and zfactor_values, this '&
!!$               //'is not acceptable, use cmor_write to write zfactor_values'//char(0)
!!$          error_code = CMOR_CRITICAL
!!$          call cmor_handle_error(msg,error_code)
!!$       endif
!!$       axes_ids(1)=axis_ids(1)
!!$    else
!!$       ndims = 0
!!$       axes_ids(1)=0
!!$    endif
!!$    if (present(units)) then
!!$       unit = trim(units)//char(0)
!!$    else 
!!$       unit=char(0)
!!$    endif
!!$    if (present(zfactor_bounds)) then
!!$       ierr = cmor_zfactor_cff_long(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_value(1),zfactor_bounds(1))
!!$    else
!!$       ierr = cmor_zfactor_cff_long_nobnds(zfactor,zaxis_id,zfnm,unit,ndims,axes_ids(1),zfactor_value(1))
!!$    endif
!!$    if (ierr.eq.0) then
!!$       ierr = zfactor
!!$    else
!!$       ierr = -ierr
!!$    end if
!!$  end function cmor_zfactor_long_0dvalues

  subroutine cmor_set_table(table_id)
    integer table_id
    call cmor_set_table_cff(table_id)
  end subroutine cmor_set_table

  function cmor_load_table(table) result(table_id)
    integer table_id
    character(*) table
    call cmor_load_table_cff(trim(table)//char(0),table_id)
  end function cmor_load_table

  function cmor_axis_char(table,table_entry,units,length,&
       coord_vals,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    character(*) :: coord_vals(:)
    integer, optional :: length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer l,cell_bounds_ndim,i
    integer ierr,error_code
    character(*), optional :: table
    integer axis_id

    if (present(table)) then
       l = cmor_load_table(table)
    endif
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
    
    
    if (.not.present(length)) then
       l = size(coord_vals)
    else
       l= length
       if (l.gt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is only",size(coord_vals),&
               "long"//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       else if (l.lt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is really",size(coord_vals),&
               "long, check you really don't want to use it in its integrality"//char(0)
          error_code = CMOR_WARNING
          call cmor_handle_error(msg,error_code)
       endif
    endif
    cell_bounds_ndim = 0
    do i=0,l
       if (len(coord_vals(i)).gt.cell_bounds_ndim) then 
          cell_bounds_ndim = len(coord_vals(i))
       endif
    enddo
    ierr = cmor_axis_cff_nobnds_char(axis_id,trim(table_entry)//char(0),&
         trim(units)//char(0),l,coord_vals(1),cell_bounds_ndim,interv)

    if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
  end function cmor_axis_char
  function cmor_axis_double_1(table,table_entry,units,length,coord_vals,&
       cell_bounds,cell_bounds_ndim,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    double precision, optional :: coord_vals(:),cell_bounds(:)
    integer, optional :: cell_bounds_ndim,length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer axis_id
    integer l,ierr
    character(*), optional :: table
    integer error_code
    if (present(table)) then
       l = cmor_load_table(table)
    endif
    ! ok first check we have length if no coords or bounds
!!$    if (.not.present(length).and. .not. present(coord_vals) .and. .not. present(cell_bounds)) then
!!$          msg = "cmor_axis (fortran):: you need to pass length to cmor_axis if no bounds nor coords"//char(0)
!!$          error_code = CMOR_CRITICAL
!!$          call cmor_handle_error(msg,error_code)
!!$    endif
    
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
    
    if (present(cell_bounds).and. .not. present(coord_vals)) then
       msg = "cmor_axis (fortran):: you need to pass coords to cmor_axis if you pass bounds"//char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    endif
    
    l=0
    if (.not.present(length)) then
          l = size(coord_vals)
    else
       l= length
       if (present(coord_vals)) then 
          if (l.gt.size(coord_vals)) then
             write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
                  "but the coord_vals array your are sending is only",size(coord_vals),&
                  "long"//char(0)
             error_code = CMOR_CRITICAL
             call cmor_handle_error(msg,error_code)
          else if (l.lt.size(coord_vals)) then
             write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
                  "but the coord_vals array your are sending is really",size(coord_vals),&
                  "long, check you really don't want to use it in its integrality"//char(0)
             error_code = CMOR_WARNING
             call cmor_handle_error(msg,error_code)
          endif
       endif
    endif

    if (present(cell_bounds)) then
       ierr = cmor_axis_cff_double(axis_id,trim(table_entry)//char(0),trim(units)//char(0),&
            l,coord_vals(1),cell_bounds(1),1,interv)
    else
       if (present(coord_vals)) then
          ierr = cmor_axis_cff_nobnds_double(axis_id,trim(table_entry)//char(0),trim(units)//char(0),l,&
               coord_vals(1),interv)
       else
          ierr = cmor_axis_cff_nocoords(axis_id,trim(table_entry)//char(0),trim(units)//char(0),0,interv)
       endif
    endif
    if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
  end function cmor_axis_double_1
  function cmor_axis_double_2(table,table_entry,units,length,coord_vals,cell_bounds,&
       cell_bounds_ndim,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    double precision :: cell_bounds(:,:)
    double precision :: coord_vals(:)
    double precision,allocatable :: bnds(:)
    integer, optional :: cell_bounds_ndim,length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer axis_id
    integer l,i,ierr,error_code
    character(*), optional :: table
    
    if (present(table)) then
       l = cmor_load_table(table)
    endif
    
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
    
    
    if (.not.present(length)) then
       l = size(coord_vals)
    else
       l= length
       if (l.gt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is only",size(coord_vals),&
               "long"//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       else if (l.lt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is really",size(coord_vals),&
               "long, check you really don't want to use it in its integrality"//char(0)
          error_code = CMOR_WARNING
          call cmor_handle_error(msg,error_code)
       endif
    endif

    if (size(cell_bounds,1).ne.2) then
       msg = "cmor_axis (fortran): bounds first dimension length must be 2"//char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    endif
    allocate(bnds(l*2))
    do i=1,l
       bnds(2*i-1) = cell_bounds(1,i)
       bnds(2*i) = cell_bounds(2,i)
    enddo
    ierr = cmor_axis_cff_double(axis_id,trim(table_entry)//char(0),trim(units)//char(0),l,&
         coord_vals(1),bnds(1),2,interv)
    deallocate(bnds)
    if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
  end function cmor_axis_double_2
  function cmor_axis_real_1(table,table_entry,units,length,coord_vals,cell_bounds,&
       cell_bounds_ndim,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    real, optional :: cell_bounds(:)
    real :: coord_vals(:)
    integer, optional :: cell_bounds_ndim,length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer axis_id
    integer l,ierr,error_code
    character(*), optional :: table
    if (present(table)) then
       l = cmor_load_table(table)
    endif
    
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
        
    if (.not.present(length)) then
       l = size(coord_vals)
    else
       l= length
       if (l.gt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is only",size(coord_vals),&
               "long"//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       else if (l.lt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is really",size(coord_vals),&
               "long, check you really don't want to use it in its integrality"//char(0)
          error_code = CMOR_WARNING
          call cmor_handle_error(msg,error_code)
       endif
    endif

    if (present(cell_bounds)) then
       ierr = cmor_axis_cff_real(axis_id,trim(table_entry)//char(0),trim(units)//char(0),l,&
            coord_vals(1),cell_bounds(1),1,interv)
    else
       ierr = cmor_axis_cff_nobnds_real(axis_id,trim(table_entry)//char(0),trim(units)//char(0),&
            l,coord_vals(1),interv)
    endif
     if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
 end function cmor_axis_real_1
  function cmor_axis_real_2(table,table_entry,units,length,coord_vals,cell_bounds,&
       cell_bounds_ndim,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    real :: cell_bounds(:,:)
    real :: coord_vals(:)
    real, allocatable :: bnds(:)
    integer, optional :: cell_bounds_ndim,length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer axis_id
    integer l,ierr,i,error_code
    character(*), optional :: table
    
   if (present(table)) then
       l = cmor_load_table(table)
    endif
    
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
    
    
    if (.not.present(length)) then
       l = size(coord_vals)
    else
       l= length
       if (l.gt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is only",size(coord_vals),&
               "long"//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       else if (l.lt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is really",size(coord_vals),&
               "long, check you really don't want to use it in its integrality"//char(0)
          error_code = CMOR_WARNING
          call cmor_handle_error(msg,error_code)
       endif
    endif

    if (size(cell_bounds,1).ne.2) then
       msg = "cmor_axis (fortran): bounds first dimension length must be 2"//char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
   endif
    allocate(bnds(l*2))
    do i=1,l
       bnds(2*i-1) = cell_bounds(1,i)
       bnds(2*i) = cell_bounds(2,i)
    enddo
    ierr = cmor_axis_cff_real(axis_id,trim(table_entry)//char(0),trim(units)//char(0),&
         l,coord_vals(1),bnds(1),2,interv)
    deallocate(bnds)
    if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
  end function cmor_axis_real_2
  function cmor_axis_int_1(table,table_entry,units,length,coord_vals,cell_bounds,&
       cell_bounds_ndim,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    integer, optional :: cell_bounds(:)
    integer :: coord_vals(:)
    integer, optional :: cell_bounds_ndim,length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer axis_id
    integer l,ierr,error_code
    character(*), optional :: table
    
    if (present(table)) then
       l = cmor_load_table(table)
    endif
    
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
    
    
    if (.not.present(length)) then
       l = size(coord_vals)
    else
       l= length
       if (l.gt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is only",size(coord_vals),&
               "long"//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       else if (l.lt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is really",size(coord_vals),&
               "long, check you really don't want to use it in its integrality"//char(0)
          error_code = CMOR_WARNING
          call cmor_handle_error(msg,error_code)
       endif
    endif

    if (present(cell_bounds)) then
       ierr = cmor_axis_cff_int(axis_id,table_entry//char(0),trim(units)//char(0),&
            l,coord_vals(1),cell_bounds(1),1,interv)
    else
          ierr = cmor_axis_cff_nobnds_int(axis_id,table_entry//char(0),trim(units)//char(0),&
               l,coord_vals(1),interv)
    endif
    if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
  end function cmor_axis_int_1
  function cmor_axis_int_2(table,table_entry,units,length,coord_vals,cell_bounds,&
       cell_bounds_ndim,interval) result(ierr)
    implicit none
    character(*) table_entry,units
    integer :: cell_bounds(:,:)
    integer :: coord_vals(:)
    integer,allocatable :: bnds(:)
    integer, optional :: cell_bounds_ndim,length
    character(*) , optional :: interval
    character(1024) interv,msg
    integer axis_id
    integer l,ierr,i,error_code
    character(*), optional :: table

    if (present(table)) then
       l = cmor_load_table(table)
    endif
    
    if (present(interval)) then
       interv = trim(interval)//char(0)
    else
       interv = char(0)
    endif
    
    
    if (.not.present(length)) then
       l = size(coord_vals)
    else
       l= length
       if (l.gt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is only",size(coord_vals),&
               "long"//char(0)
          error_code = CMOR_CRITICAL
          call cmor_handle_error(msg,error_code)
       else if (l.lt.size(coord_vals)) then
          write(msg,*) "cmor_axis (fortran):: you passed legnth=",length,&
               "but the coord_vals array your are sending is really",size(coord_vals),&
               "long, check you really don't want to use it in its integrality"//char(0)
          error_code = CMOR_WARNING
          call cmor_handle_error(msg,error_code)
       endif
    endif

    if (size(cell_bounds,1).ne.2) then
       msg = "cmor_axis (fortran): bounds first dimension lengthmust be 2"//char(0)
       error_code = CMOR_CRITICAL
       call cmor_handle_error(msg,error_code)
    endif
    allocate(bnds(l*2))
    do i=1,l
       bnds(2*i-1) = cell_bounds(1,i)
       bnds(2*i) = cell_bounds(2,i)
    enddo
    ierr = cmor_axis_cff_int(axis_id,trim(table_entry)//char(0),trim(units)//char(0),l,&
         coord_vals(1),bnds(1),2,interv)
    deallocate(bnds)
    if (ierr.eq.0) then 
       ierr = axis_id
    else 
       ierr = -ierr
    endif
 end function cmor_axis_int_2
!!$  function cmor_axis_long_1(table,table_entry,units,length,coord_vals,cell_bounds,&
!!$       cell_bounds_ndim,interval) result(ierr)
!!$    implicit none
!!$    character(*) table_entry,units
!!$    integer (kind=8), optional :: cell_bounds(:)
!!$    integer (kind=8) :: coord_vals(:)
!!$    integer, optional :: cell_bounds_ndim,length
!!$    character(*) , optional :: interval
!!$    character(1024) interv
!!$    integer axis_id
!!$    integer l,ierr
!!$    character(*), optional :: table
!!$    
!!$  if (present(table)) then
!!$       l = cmor_load_table(table)
!!$    endif
!!$    
!!$    if (present(interval)) then
!!$       interv = interval//char(0)
!!$    else
!!$       interv = char(0)
!!$    endif
!!$    
!!$    
!!$    if (.not.present(length)) then
!!$       l = size(coord_vals)
!!$    else
!!$       l= length
!!$    endif
!!$
!!$    if (present(cell_bounds)) then
!!$       ierr = cmor_axis_cff_long(axis_id,table_entry//char(0),units//char(0),&
!!$            l,coord_vals(1),cell_bounds(1),1,interv)
!!$    else
!!$          ierr = cmor_axis_cff_nobnds_long(axis_id=axis_id,&
!!$               table_entry=table_entry//char(0),&
!!$               units=units//char(0),&
!!$               length=l,coord_vals=coord_vals(1),interval=interv)
!!$    endif
!!$    if (ierr.eq.0) then 
!!$       ierr = axis_id
!!$    else 
!!$       ierr = -ierr
!!$    endif
!!$  end function cmor_axis_long_1
!!$  function cmor_axis_long_2(table,table_entry,units,length,coord_vals,cell_bounds,&
!!$       cell_bounds_ndim,interval) result(ierr)
!!$    implicit none
!!$    character(*) table_entry,units
!!$    integer(kind=8)  :: coord_vals(:),cell_bounds(:,:)
!!$    integer(kind=8),allocatable :: bnds(:)
!!$    integer , optional :: cell_bounds_ndim,length
!!$    character(*) , optional :: interval
!!$    character(1024) interv,msg
!!$    integer axis_id
!!$    integer l,ierr,i,error_code
!!$    character(*), optional :: table
!!$    character(10) type
!!$
!!$    if (present(table)) then
!!$       l = cmor_load_table(table)
!!$    endif
!!$    
!!$    if (present(interval)) then
!!$       interv = interval//char(0)
!!$    else
!!$       interv = char(0)
!!$    endif
!!$    
!!$    
!!$    if (.not.present(length)) then
!!$       l = size(coord_vals)
!!$    else
!!$       l= length
!!$    endif
!!$
!!$    if (size(cell_bounds,1).ne.2) then
!!$       msg = "cmor_axis (fortran): bounds first dimension lengthmust be 2"//char(0)
!!$       error_code = CMOR_CRITICAL
!!$       call cmor_handle_error(msg,error_code)
!!$    endif
!!$    allocate(bnds(l*2))
!!$    do i=1,l
!!$       bnds(2*i-1) = cell_bounds(1,i)
!!$       bnds(2*i) = cell_bounds(2,i)
!!$    enddo
!!$    ierr = cmor_axis_cff_long(axis_id=axis_id,&
!!$         table_entry=table_entry//char(0),units=units//char(0),length=l,&
!!$         coord_vals=coord_vals(1),cell_bounds=bnds(1),&
!!$         cell_bounds_ndim=2,interval=interv)
!!$    deallocate(bnds)
!!$    ierr = -ierr
!!$  end function cmor_axis_long_2
     

  subroutine cmor_create_output_path(var_id, path)
    implicit none
    character(*), intent(out) :: path
    integer var_id
    integer slen
    call cmor_create_output_path_cff(var_id,path,slen)
    path = path(1:slen)
  end subroutine cmor_create_output_path

  function cmor_has_cur_dataset_attribute(value) result (ierr)
    implicit none
    character (*), intent (in) :: value
    integer ierr
    ierr = cmor_has_cur_dset_attribute_cff(trim(value)//char(0))
  end function cmor_has_cur_dataset_attribute

  function cmor_get_cur_dataset_attribute(name, value) result (ierr)
    implicit none
    character (*), intent (in) :: name
    character (*), intent (out) :: value
    integer ierr
    ierr = cmor_get_cur_dset_attribute_cff(trim(name)//char(0), value)
  end function cmor_get_cur_dataset_attribute

  function cmor_set_cur_dataset_attribute(name, value) result (ierr)
    implicit none
    character (*), intent (in) :: name
    character (*), intent (in) :: value
    integer ierr
    ierr = cmor_set_cur_dset_attribute_cff(trim(name)//char(0), trim(value)//char(0))
  end function cmor_set_cur_dataset_attribute
  function cmor_has_variable_attribute(var_id, value) result (ierr)
    implicit none
    character (*), intent (in) :: value
    integer, intent (in) :: var_id
    integer ierr
    ierr = cmor_has_variable_attribute_cff(var_id, trim(value)//char(0))
  end function cmor_has_variable_attribute

  function cmor_get_variable_attribute(var_id, name, value) result (ierr)
    implicit none
    character (*), intent (in) :: name
    character (*), intent (out) :: value
    integer, intent (in) :: var_id
    integer ierr
    ierr = cmor_get_variable_attribute_cff(var_id,trim(name)//char(0), value)
  end function cmor_get_variable_attribute

  function cmor_set_variable_attribute(var_id, name, value) result (ierr)
    implicit none
    character (*), intent (in) :: name
    character (*), intent (in) :: value
    integer, intent (in) :: var_id
    integer ierr
    ierr = cmor_set_variable_attribute_cff(var_id,trim(name)//char(0), trim(value)//char(0))
  end function cmor_set_variable_attribute

  function cmor_setup_ints(inpath,netcdf_file_action, set_verbosity,&
       exit_control, logfile, create_subdirectories) result(ierr)
    implicit none
    integer ierr,nc,verb,mode,crsub
    integer , optional, intent(in) :: netcdf_file_action
    integer , optional, intent(in) :: set_verbosity
    integer , optional, intent(in) :: exit_control
    character(*) , optional, intent(in) :: inpath
    character(*) , optional, intent(in) :: logfile
    integer , optional, intent(in) :: create_subdirectories
    character(1024) path


    if (present(inpath)) then
       path = inpath
    else
       path = "."
    endif
    if (present(netcdf_file_action)) then
       nc = netcdf_file_action
    else
       nc = CMOR_PRESERVE
    endif
    if(present(set_verbosity)) then
       verb = set_verbosity
    else
       verb = CMOR_NORMAL
    endif
    if (present(exit_control)) then
       mode =exit_control
    else
       mode = CMOR_NORMAL
    endif
    ! correction code to comply with Karl's old calls
    if (verb.eq.0 ) then 
       verb = CMOR_QUIET
    else if (verb.eq.1) then 
       verb = CMOR_NORMAL
    endif
    if (mode.eq.0) then 
       mode = CMOR_EXIT_ON_MAJOR
    else if (mode.eq.1 ) then 
       mode = CMOR_NORMAL
    else if (mode.eq.2) then 
       mode = CMOR_EXIT_ON_WARNING
    endif
    if (present(create_subdirectories)) then
       crsub = create_subdirectories
    else
       crsub = 1
    endif
    if (present(logfile)) then 
       ierr = -cmor_setup_cff(trim(path)//char(0),nc,verb,mode,trim(logfile)//char(0),crsub)
    else
       ierr = -cmor_setup_cff_nolog(trim(path)//char(0),nc,verb,mode,crsub)
    endif
       
  end function cmor_setup_ints

  FUNCTION similar(str1, str2)

    ! compares two strings after removing leading and trailing blanks and 
    ! ignoring case.  

    IMPLICIT NONE

    LOGICAL :: similar
    CHARACTER(len=*), INTENT(IN) :: str1
    CHARACTER(len=*), INTENT(IN) :: str2

    CHARACTER(len=LEN(str1)) :: low1
    CHARACTER(len=LEN(str2)) :: low2
    INTEGER :: len1, i

    low1 = ADJUSTL(str1)
    low2 = ADJUSTL(str2)

    len1 = LEN_TRIM(low1)
    similar = .TRUE.
    IF (len1 /= LEN_TRIM(low2)) THEN
       similar = .FALSE.
    ELSE
       ! convert strings to lower case
       DO i=1,len1
          IF ( LGE(low1(i:i),'A') .AND. LLE(low1(i:i),'Z') ) &
               low1(i:i) = ACHAR( IACHAR(low1(i:i)) + 32 )
          IF ( LGE(low2(i:i),'A') .AND. LLE(low2(i:i),'Z') ) &
               low2(i:i) = ACHAR( IACHAR(low2(i:i)) + 32 )
       END DO

       IF (TRIM(low1) /= TRIM(low2)) similar=.FALSE.
    END IF

    RETURN
  END FUNCTION similar


  function cmor_setup_nc_char(inpath,netcdf_file_action, set_verbosity,&
       exit_control, logfile, create_subdirectories) result(ierr)
    implicit none
    integer ierr,nc,verb,mode,crsub
    integer , optional, intent(in) :: set_verbosity
    integer , optional, intent(in) :: exit_control
    character(*), optional, intent(in) ::  inpath
    character(*) , intent(in) :: netcdf_file_action
    character(*) , optional, intent(in) :: logfile
    integer , optional, intent(in) :: create_subdirectories
    character(1024) path

    if (present(inpath)) then
       path = trim(inpath)//char(0)
    else
       path = "."
    endif
    if (similar(netcdf_file_action,"preserve") ) then
       nc = CMOR_PRESERVE
    else if (similar(netcdf_file_action,"append") ) then
       nc = CMOR_APPEND
    else if (similar(netcdf_file_action,"replace") ) then
       nc = CMOR_REPLACE
    else if (similar(netcdf_file_action,"preserve_3") ) then
       nc = CMOR_PRESERVE_3
    else if (similar(netcdf_file_action,"append_3") ) then
       nc = CMOR_APPEND_3
    else if (similar(netcdf_file_action,"replace_3") ) then
       nc = CMOR_REPLACE_3
    else !dummy value to generate error from C
       nc =1000
    endif

    if (present(create_subdirectories)) then
       crsub = create_subdirectories
    else
       crsub = 1
    endif

    if(present(set_verbosity)) then
       verb = set_verbosity
    else
       verb = CMOR_NORMAL
    endif
    if (present(exit_control)) then
       mode =exit_control
    else
       mode = CMOR_NORMAL
    endif
    ! correction code to comply withh Karl's old calls
    if (verb.eq.0 ) then 
       verb = CMOR_QUIET
    else if (verb.eq.1) then 
       verb = CMOR_NORMAL
    endif
    if (mode.eq.0) then 
       mode =CMOR_EXIT_ON_MAJOR
    else if (mode.eq.1 ) then
       mode = CMOR_NORMAL
    else if (mode.eq.2) then
       mode = CMOR_EXIT_ON_WARNING
    endif
    if (present(logfile)) then 
       ierr = -cmor_setup_cff(trim(path)//char(0),nc,verb,mode,trim(logfile)//char(0),crsub)
    else
       ierr = -cmor_setup_cff_nolog(trim(path)//char(0),nc,verb,mode,crsub)
    endif
  end function cmor_setup_nc_char

  function cmor_dataset(outpath,experiment_id,institution,source,calendar,&
       realization,&
       contact,history,comment,references,&
       leap_year,leap_month,month_lengths,model_id,forcing, &
       initialization_method,physics_version,institute_id,parent_experiment_id,branch_time,parent_experiment_rip) result (ierr)
    implicit none
    character(*), INTENT(in) :: outpath,experiment_id,institution,source,calendar
    character(*), optional, intent(in) :: model_id,forcing
    character(*), optional, intent(in) :: contact,history,comment,references,institute_id
    character(*), optional, intent(in) :: parent_experiment_id,parent_experiment_rip
    integer, optional,intent(in) :: leap_year,leap_month,month_lengths(12)
    integer r,ly,lm,im,pv
    integer, optional, intent(in) :: realization,initialization_method,physics_version
    character(1024) cntct,hist,comt,ref,mnm,fnm,instid,peid,perip
    integer ierr
    double precision, optional, intent(in) :: branch_time

    if (present(realization)) then
       r = realization
    else
       r = 1
    endif
    if (present(initialization_method)) then
       im = initialization_method
    else
       im = 0
    endif
    if (present(physics_version)) then
       pv = physics_version
    else
       pv = 0
    endif
    if (present(leap_year)) then
       ly = leap_year
    else
       ly =0
    endif
    if (present(leap_month)) then
       lm = leap_month
    else
       lm =0
    endif
    if (present(contact)) then
       cntct = trim(contact)//char(0)
    else
       cntct = char(0)
    endif
    if (present(history)) then
       hist = trim(history)//char(0)
    else
       hist = char(0)
    endif
    if (present(comment)) then
       comt = trim(comment)//char(0)
    else
       comt = char(0)
    endif
    if (present(references)) then
       ref = trim(references)//char(0)
    else
       ref = char(0)
    endif
    if (present(model_id)) then
       mnm = trim(model_id)//char(0)
    else
       mnm  = char(0)
    endif
    if (present(forcing)) then
       fnm = trim(forcing)//char(0)
    else
       fnm  = char(0)
    endif
    if (present(institute_id)) then
       instid = trim(institute_id)//char(0)
    else
       instid= char(0)
    endif
    if (present(parent_experiment_id)) then
       peid = trim(parent_experiment_id)//char(0)
    else
       peid= char(0)
    endif
    if (present(parent_experiment_rip)) then
       perip = trim(parent_experiment_rip)//char(0)
    else
       perip= char(0)
    endif
    if (present(month_lengths)) then
       if (present(branch_time)) then
          ierr = cmor_dataset_cff(trim(outpath)//char(0),trim(experiment_id)//char(0),&
               trim(institution)//char(0),trim(source)//char(0),trim(calendar)//char(0),r,&
               cntct,hist,comt,ref,&
               ly,lm,month_lengths(1),mnm,fnm,im,pv,instid,peid,branch_time,perip)
       else
          ierr = cmor_dataset_cff_nobrch(trim(outpath)//char(0),trim(experiment_id)//char(0),&
               trim(institution)//char(0),trim(source)//char(0),trim(calendar)//char(0),r,&
               cntct,hist,comt,ref,&
               ly,lm,month_lengths(1),mnm,fnm,im,pv,instid,peid,perip)
       endif
    else
       if (present(branch_time)) then
          ierr = cmor_dataset_cff_null(trim(outpath)//char(0),trim(experiment_id)//char(0),&
               trim(institution)//char(0),trim(source)//char(0),trim(calendar)//char(0),r,&
               cntct,hist,comt,ref,&
               ly,lm,mnm,fnm,im,pv,instid,peid,branch_time,perip)
       else
          ierr = cmor_dataset_cff_null_nobrch(trim(outpath)//char(0),trim(experiment_id)//char(0),&
               trim(institution)//char(0),trim(source)//char(0),trim(calendar)//char(0),r,&
               cntct,hist,comt,ref,&
               ly,lm,mnm,fnm,im,pv,instid,peid,perip)
       endif
    endif
    ierr = -ierr
  end function cmor_dataset
end module cmor_users_functions
