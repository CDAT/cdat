program main

  USE cmor_users_functions
  implicit none

  integer ncid

  type dims
     integer n
     character(256) name
     character(256) units
     double precision, DIMENSION(:), pointer :: values
     double precision, DIMENSION(:,:), pointer :: bounds     
     type(dims), pointer :: next
  end type dims
  character(256) filein
  type(dims), pointer :: mydims,current
  integer ndim,i,j,ntot
!  integer, allocatable, dimension(:):: arrayin
  double precision, allocatable, dimension(:):: arrayin
  integer, dimension(7):: dimlength = (/ (1,i=1,7) /)
  integer, PARAMETER::verbosity = 2
  integer ierr
  integer, allocatable, dimension(:) :: myaxis
  integer myvar

  print*, 'hi enter test file case'
  filein='Test/tas.asc'
  read(5,'(A)') filein
  open(unit=23,file=filein,form='formatted') 
  call allocate_dims(23,mydims,ndim)
  allocate(myaxis(ndim))
  current=>mydims
  ntot=1
  do i =1,ndim
     ntot=ntot*current%n
     current=>current%next
  enddo
  call read_ascii(23,mydims, ndim,ntot,arrayin)
  
  
!!$!! Ok here is the part where we define or variable/axis,etc... 
!!$!! Assuming that Karl's code is ok...
!!$
  
  print*,'CMOR SETUP'
!!$  
  ierr = cmor_setup(inpath='Test',   &
       netcdf_file_action='replace',                                       &
       set_verbosity=1,                                                    &
       exit_control=1)
    
  print*,'CMOR DATASET'
  ierr = cmor_dataset(                                   &
       outpath='Test',         &
       experiment_id='2xCO2 equilibrium experiment',           &
       institution=                                            &
       'GICC (Generic International Climate Center, ' //       &
       ' Geneva, Switzerland)',                                &
       source='GICCM  2002(giccm_0_brnchT_itea_2, T63L32)',    &
       calendar='noleap',                                      &
       realization=1,                                          &
       history='Output from archive/giccm_03_std_2xCO2_2256.', &
       comment='Equilibrium reached after 30-year spin-up ' // &
       'after which data were output starting with nominal '// &
       'date of January 2030',                                 &
       references='Model described by Koder and Tolkien ' //   &
       '(J. Geophys. Res., 2001, 576-591).  Also ' //          &
       'see http://www.GICC.su/giccm/doc/index.html '  //      &
       ' 2XCO2 simulation described in Dorkey et al. '//       &
       '(Clim. Dyn., 2003, 323-357.)' )
  
  current=>mydims
  do i = 0,ndim-1
     print*,'CMOR AXIS',i,'AAAAAAA**************************'
     print*, 'Name:',trim(adjustl(current%name)),'--',trim(adjustl(mydims%name))
     print*, current%units
     print*, current%n,size(current%values)
     print*, current%values(1:min(4,size(current%values)))
     print*, current%bounds(1:2,1:min(4,size(current%values)))
     if (i==0) then
        myaxis(ndim-i)=cmor_axis('CMOR_SAMPLE_TABLE', &
             table_entry=current%name,&
             units=current%units,&
             length=current%n)
     else
        myaxis(ndim-i)=cmor_axis('CMOR_SAMPLE_TABLE', &
             table_entry=current%name,&
             units=current%units,&
             length=current%n,&
             coord_vals=current%values,&
             cell_bounds=current%bounds)
     endif
     current=>current%next
  enddo

  print*,'CMOR VAR'
  myvar=cmor_variable('CMOR_SAMPLE_TABLE',&
       'tas',&
       'K',&
       myaxis,&
       missing_value=1.e20)

!! figures out length of dimension other than time



  j=ntot/mydims%n
  print*, '&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
  do i=1,mydims%n !! write times one at a time
!!$     print*,'before:', arrayin(j*(i-1)+1),j*(i-1)+1,j,i,ntot
     print*, 'size of arrayin',size(arrayin(j*(i-1)+1:j*i))
     ierr = cmor_write( &
          var_id        = myvar,                        &
          data          = arrayin(j*(i-1)+1:j*i), &
          ntimes_passed = 1,                              &
          time_vals     = mydims%values(i:i),                         &
          time_bnds     = mydims%bounds(1:2,i:i) &
          )
  enddo

  ierr = cmor_close()  

 
contains
  subroutine allocate_dims(file_id,mydims,ndim)
    implicit none
    integer i,n,j,tmp,file_id
    integer, intent(inout)::ndim
    type(dims) , pointer :: tmpdims,mydims
    read(file_id,'(I4)') ndim
!!$    allocate(dimlength(ndim))
    n=1
    allocate(mydims)
    tmpdims=>mydims
    do i = 1, ndim
       read(file_id,'(I4)') tmp
       dimlength(8-i)=tmp
       allocate(tmpdims%values(tmp))
       allocate(tmpdims%bounds(2,tmp))
       tmpdims%n=tmp
       allocate(tmpdims%next)
       tmpdims=>tmpdims%next
       n=n*tmp
    enddo
    deallocate(tmpdims)
    allocate(arrayin(n))
  end subroutine allocate_dims
  
  subroutine read_ascii(file_unit,mydims,ndim,ntot,arrayin)
    implicit none
    type(dims), pointer::  mydims
!    integer, dimension(ntot),intent(inout) :: arrayin
    double precision, dimension(ntot),intent(inout) :: arrayin
    type(dims), pointer ::  current
    integer, intent(in)::ndim,file_unit
    integer n,ntot,i,j,k
    
    current=>mydims
    ntot=1
    do i =1,ndim
       n=current%n
       ntot=ntot*n
       read(file_unit,*) current%name
       print*, 'NAME is:',current%name,trim(adjustl(mydims%name)),n,ntot
       read(file_unit,'(A)') current%units
       read(file_unit,*) (current%values(j),j=1,n)
       read(file_unit,*) ((current%bounds(j,k),j=1,2),k=1,n)
       print*, current%values(1),current%values(n)
       print*, current%bounds(1,1),current%bounds(2,1)
       print*, current%bounds(1,n),current%bounds(2,n)
       current=>current%next
    enddo
    print *, 'ntot:',ntot
       read(file_unit,*) (arrayin(i),i=1,ntot)

print* ,trim(adjustl(mydims%name))
print*,'done reading'
  end subroutine read_ascii

end program main

