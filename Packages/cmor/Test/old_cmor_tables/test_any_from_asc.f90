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
  type(dims), pointer :: mydims,current,ptimes
  integer ndim,i,j,ntot
!  real, allocatable, dimension(:,:,:):: arrayin
  real, allocatable, dimension(:,:):: arrayin2d
  real, allocatable, dimension(:,:,:):: arrayin3d
  real, allocatable, dimension(:,:,:,:):: arrayin4d
  real, allocatable, dimension(:,:,:,:,:):: arrayin5d
  integer, dimension(7):: dimlength = (/ (1,i=1,7) /)
  integer, PARAMETER::verbosity = 2
  integer ierr
  integer, allocatable, dimension(:) :: myaxis
  integer myvar
  character(50) :: var,units

  print*, 'Test Code: Welcome to the general from ascii testing code'
  read(5,'(A)') filein
  open(unit=23,file=filein,form='formatted') 
  call allocate_dims(23,mydims,ndim,dimlength,var,units)
  allocate(myaxis(ndim))
  current=>mydims
  ntot=1
  do i =1,ndim
     ntot=ntot*current%n
     current=>current%next
  enddo
  if (ndim.eq.2) then
     allocate(arrayin2d(dimlength(1),dimlength(2)))
     call read_ascii2d(23,mydims, ndim,ntot,arrayin2d)
     print*,'Test Code: allocate data:',shape(arrayin2d)
  else if (ndim.eq.3) then
     allocate(arrayin3d(dimlength(1),dimlength(2),dimlength(3)))
     call read_ascii3d(23,mydims, ndim,ntot,arrayin3d)
     print*,'Test Code: allocate data:',shape(arrayin3d)
  else if (ndim.eq.4) then
     allocate(arrayin4d(dimlength(1),dimlength(2),dimlength(3),dimlength(4)))
     call read_ascii4d(23,mydims, ndim,ntot,arrayin4d)
     print*,'Test Code: allocate data:',shape(arrayin4d)
  else if (ndim.eq.5) then
     call read_ascii5d(23,mydims, ndim,ntot,arrayin5d)
     allocate(arrayin5d(dimlength(1),dimlength(2),dimlength(3),dimlength(4),dimlength(5)))
     print*,'Test Code: allocate data:',shape(arrayin5d)
  endif
  
  
!!$!! Ok here is the part where we define or variable/axis,etc... 
!!$!! Assuming that Karl's code is ok...
!!$
  
  print*,'Test Code: CMOR SETUP'
!!$  
  ierr = cmor_setup(inpath='Test',   &
       netcdf_file_action='replace',                                       &
       set_verbosity=1,                                                    &
       exit_control=1)
    
  print*,'Test Code: CMOR DATASET'
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
       '(Clim. Dyn., 2003, 323-357.)', &
       model_id='pcmdi-01a',forcing='atm' )
  
  current=>mydims
  do i = 0,ndim-1
     print*,'Test Code: CMOR AXIS',i
     print*, 'Test Code: Axis Name:',trim(adjustl(current%name))
     if (trim(adjustl(current%name)).eq.'time') then
        print*, 'Test Code: time found'
        ptimes => current
        myaxis(ndim-i)=cmor_axis('CMOR_SAMPLE_TABLE', &
             table_entry=current%name,&
             units=current%units,&
             length=current%n,&
!!$          coord_vals=current%values,&
!!$          cell_bounds=current%bounds, &
             interval='1 month')
     else
        myaxis(ndim-i)=cmor_axis('CMOR_SAMPLE_TABLE', &
             table_entry=current%name,&
             units=current%units,&
             length=current%n,&
             coord_vals=current%values,&
             cell_bounds=current%bounds)
        print*, 'Test Code: not time'
     endif
     current=>current%next
  enddo
  
  print*,'Test Code: CMOR VAR'
  myvar=cmor_variable('CMOR_SAMPLE_TABLE',&
       var,&
       units,&
       myaxis,&
       missing_value=1.e20)

  !! figures out length of dimension other than time

  print*, 'Test Code: time before:',ptimes%values(i:i)
  if (ndim.eq.2) then 
     if (ptimes%name.eq.mydims%name) then
        do i=1,ptimes%n
           print*, 'Test code: Write time',i,ptimes%values(i)
           ierr = cmor_write( &
                var_id        = myvar, &
                data          = arrayin2d(:,i:i), &
                ntimes_passed = 1,   &
                time_vals     = (/ptimes%values(i)/),  &
                time_bnds     = RESHAPE (ptimes%bounds(1:2,i), (/2,1/)) &
                )
        enddo
     else
        print*, 'Test code: Write all times at once'
        ierr = cmor_write( &
             var_id        = myvar, &
             data          = arrayin2d, &
             ntimes_passed = ptimes%n,   &
             time_vals     = ptimes%values,  &
             time_bnds     = ptimes%bounds &
             )
     endif
  else if (ndim.eq.3) then
     if (ptimes%name.eq.mydims%name) then
        do i=1,ptimes%n
           print*, 'Test code: Write time',i,ptimes%values(i)
           ierr = cmor_write( &
                var_id        = myvar, &
                data          = arrayin3d(:,:,i:i), &
                ntimes_passed = 1,   &
                time_vals     = (/ptimes%values(i)/),  &
                time_bnds     = RESHAPE (ptimes%bounds(1:2,i), (/2,1/)) &
                )
        enddo
     else
        print*, 'Test code: Write all times at once',ptimes%units,ptimes%values
        ierr = cmor_write( &
             var_id        = myvar, &
             data          = arrayin3d, &
             ntimes_passed = ptimes%n,   &
             time_vals     = ptimes%values,  &
             time_bnds     = ptimes%bounds &
             )
     endif
  else if (ndim.eq.4) then
     if (ptimes%name.eq.mydims%name) then
        do i=1,ptimes%n
           print*, 'Test code: Write time',i,ptimes%values(i)
           ierr = cmor_write( &
                var_id        = myvar, &
                data          = arrayin4d(:,:,:,i:i), &
                ntimes_passed = 1,   &
                time_vals     = (/ptimes%values(i)/),  &
                time_bnds     = RESHAPE (ptimes%bounds(1:2,i), (/2,1/)) &
                )
        enddo
     else
        print*, 'Test code: Write all times at once',ptimes%units
        ierr = cmor_write( &
             var_id        = myvar, &
             data          = arrayin4d, &
             ntimes_passed = ptimes%n,   &
             time_vals     = ptimes%values,  &
             time_bnds     = ptimes%bounds &
             )
     endif
  else
     if (ptimes%name.eq.mydims%name) then
        do i=1,ptimes%n
           print*, 'Test code: Write time',i,ptimes%values(i)
           ierr = cmor_write( &
                var_id        = myvar, &
                data          = arrayin5d(:,:,:,:,i:i), &
                ntimes_passed = 1,   &
                time_vals     = (/ptimes%values(i)/),  &
                time_bnds     = RESHAPE (ptimes%bounds(1:2,i), (/2,1/)) &
                )
        enddo
     else
        print*, 'Test code: Write all times at once'
        ierr = cmor_write( &
             var_id        = myvar, &
             data          = arrayin5d, &
             ntimes_passed = ptimes%n,   &
             time_vals     = ptimes%values,  &
             time_bnds     = ptimes%bounds &
             )
     endif
  endif
  ierr = cmor_close()

contains
  subroutine allocate_dims(file_id,mydims,ndim,dimlength,var,units)
    implicit none
    integer i,n,j,tmp,file_id
    integer, intent(inout)::ndim
    integer, intent(inout):: dimlength(7)
    type(dims) , pointer :: tmpdims,mydims
    character(50), intent(inout) :: var,units
    read(file_id,'(A)') var
    read(file_id,'(A)') units
    read(file_id,'(I4)') ndim
    n=1
    allocate(mydims)
    tmpdims=>mydims
    do i = 1, ndim
       read(file_id,'(I4)') tmp
       dimlength(4-i)=tmp
       allocate(tmpdims%values(tmp))
       allocate(tmpdims%bounds(2,tmp))
       tmpdims%n=tmp
       allocate(tmpdims%next)
       tmpdims=>tmpdims%next
       n=n*tmp
    enddo
    deallocate(tmpdims)
  end subroutine allocate_dims
  
  subroutine read_ascii2d(file_unit,mydims,ndim,ntot,arrayin)
    implicit none
    type(dims), pointer::  mydims
    real, dimension(:,:),intent(inout) :: arrayin
    type(dims), pointer ::  current
    integer, intent(in)::ndim,file_unit
    integer n,ntot,i,j,k,l
    
    current=>mydims
    ntot=1
    do i =1,ndim
       n=current%n
       ntot=ntot*n
       read(file_unit,'(A)') current%name
       print*, 'Test Code: NAME is:',current%name
       read(file_unit,'(A)') current%units
       print*, 'Test Code: UNITS is:',current%units
       read(file_unit,*) (current%values(j),j=1,n)
       read(file_unit,*) ((current%bounds(j,k),j=1,2),k=1,n)
       print*, 'Test Code: bounds',current%bounds(1,1),current%bounds(1,2)
       current=>current%next
    enddo
    read(file_unit,*) ((arrayin(j,k),j=1,size(arrayin,1)),k=1,size(arrayin,2))
  end subroutine read_ascii2d
  subroutine read_ascii3d(file_unit,mydims,ndim,ntot,arrayin)
    implicit none
    type(dims), pointer::  mydims
    real, dimension(:,:,:),intent(inout) :: arrayin
    type(dims), pointer ::  current
    integer, intent(in)::ndim,file_unit
    integer n,ntot,i,j,k,l
    
    current=>mydims
    ntot=1
    do i =1,ndim
       n=current%n
       ntot=ntot*n
       read(file_unit,'(A)') current%name
       print*, 'Test Code: NAME is:',current%name
       read(file_unit,'(A)') current%units
       print*, 'Test Code: UNITS is:',current%units
       read(file_unit,*) (current%values(j),j=1,n)
       read(file_unit,*) ((current%bounds(j,k),j=1,2),k=1,n)
       print*, 'Test Code: bounds',current%bounds(1,1),current%bounds(1,2)
       current=>current%next
    enddo
    read(file_unit,*) (((arrayin(j,k,l),j=1,size(arrayin,1)),k=1,size(arrayin,2)),l=1,size(arrayin,3))
  end subroutine read_ascii3d
  subroutine read_ascii4d(file_unit,mydims,ndim,ntot,arrayin)
    implicit none
    type(dims), pointer::  mydims
    real, dimension(:,:,:,:),intent(inout) :: arrayin
    type(dims), pointer ::  current
    integer, intent(in)::ndim,file_unit
    integer n,ntot,i,j,k,l,m
    
    current=>mydims
    ntot=1
    do i =1,ndim
       n=current%n
       ntot=ntot*n
       read(file_unit,'(A)') current%name
       print*, 'Test Code: NAME is:',current%name
       read(file_unit,'(A)') current%units
       print*, 'Test Code: UNITS is:',current%units
       read(file_unit,*) (current%values(j),j=1,n)
       read(file_unit,*) ((current%bounds(j,k),j=1,2),k=1,n)
       print*, 'Test Code: bounds',current%bounds(1,1),current%bounds(1,2)
       current=>current%next
    enddo
    read(file_unit,*) ((((arrayin(j,k,l,m),j=1,size(arrayin,1)),k=1,size(arrayin,2)),l=1,size(arrayin,3)),m=1,size(arrayin,4))
  end subroutine read_ascii4d
  subroutine read_ascii5d(file_unit,mydims,ndim,ntot,arrayin)
    implicit none
    type(dims), pointer::  mydims
    real, dimension(:,:,:,:,:),intent(inout) :: arrayin
    type(dims), pointer ::  current
    integer, intent(in)::ndim,file_unit
    integer n,ntot,i,j,k,l,m,o
    
    current=>mydims
    ntot=1
    do i =1,ndim
       n=current%n
       ntot=ntot*n
       read(file_unit,'(A)') current%name
       print*, 'Test Code: NAME is:',current%name
       read(file_unit,'(A)') current%units
       print*, 'Test Code: UNITS is:',current%units
       read(file_unit,*) (current%values(j),j=1,n)
       read(file_unit,*) ((current%bounds(j,k),j=1,2),k=1,n)
       print*, 'Test Code: bounds',current%bounds(1,1),current%bounds(1,2)
       current=>current%next
    enddo
    read(file_unit,*) (((((arrayin(j,k,l,m,o),j=1,size(arrayin,1)),&
         k=1,size(arrayin,2)),l=1,size(arrayin,3)),m=1,size(arrayin,4)),&
         o=1,size(arrayin,5))
  end subroutine read_ascii5d


end program main

