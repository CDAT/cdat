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
  integer ndim,i,j,ntot,k,l
  double precision, allocatable, dimension(:,:,:,:):: arrayin
!  real, allocatable, dimension(:,:,:,:):: arrayin
  double precision, allocatable :: bigarray(:,:,:,:)
  integer, dimension(7):: dimlength = (/ (1,i=1,7) /)
  integer, PARAMETER::verbosity = 2
  integer ierr
  integer, allocatable, dimension(:) :: myaxis
  integer myvar
  real amin,amax,mymiss
  double precision bt
  bt=0.
  print*, 'Test Code: hi'
  filein='Test/ta_4D_r.asc'
  open(unit=23,file=filein,form='formatted') 
  call allocate_dims(23,mydims,ndim,dimlength)
  allocate(myaxis(ndim))
  allocate(arrayin(dimlength(1),dimlength(2),dimlength(3),dimlength(4)))
  allocate(bigarray(dimlength(1)+10,dimlength(2)+10,dimlength(3)+10,dimlength(4)+10))
  print*,'Test Code: allocate data    :',shape(arrayin),'dims:',dimlength(1),dimlength(2),dimlength(3),dimlength(4)
  print*,'Test Code: allocate data big:',shape(bigarray)
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

  print*, 'Test Code: putting everything into the big array contiguous fortran order means faster moving is first element'
  bigarray=666. ! initialize bigarray at some bad value
  ierr=1
  do l = 1, dimlength(4)
     do k = 1, dimlength(3)
        do j = 1, dimlength(2)
           do i = 1, dimlength(1)
              bigarray(i,j,k,l)=arrayin(i,j,k,l)
              ierr=ierr+1
           enddo
        enddo
     enddo
  enddo

  print*,'Test Code: CMOR SETUP'
!!$  
  ierr = cmor_setup(inpath='Test',   &
       netcdf_file_action='replace',                                       &
       set_verbosity=1,                                                    &
       exit_control=1)
    
  print*,'Test Code: CMOR DATASET'
  ierr = cmor_dataset(                                   &
       outpath='Test',         &
       experiment_id='abrupt 4XCO2',           &
       institution=                                            &
       'GICC (Generic International Climate Center, ' //       &
       ' Geneva, Switzerland)',                                &
       source='GICCM1  2002(giccm_0_brnchT_itea_2, T63L32)',    &
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
       '(Clim. Dyn., 2003, 323-357.)',model_id='GICCM1', &
       forcing='TO',contact="Barry Bonds",institute_id="PCMDI",&
       parent_experiment_rip="N/A",parent_experiment_id="N/A",branch_time=bt)
  
  current=>mydims
  do i = 0,ndim-1
     print*,'Test Code: CMOR AXIS',i,'AAAAAAA*************************************************************************'
     print*, 'Test Code: Name:',trim(adjustl(current%name))
!!$     print*, 'Test Code: ',current%units
!!$     print*, 'Test Code: ',current%n,size(current%values)
!!$     print*, 'Test Code: ',current%values(1:min(4,size(current%values)))
!!$     print*, 'Test Code: ',current%bounds(1:2,1:min(4,size(current%values)))
     if (trim(adjustl(current%name)).eq.'time') then
        print*, 'Test Code: time found','with ',current%n,'times'
        myaxis(ndim-i)=cmor_axis('Tables/CMIP5_Amon', &
          table_entry=current%name,&
          units=current%units,&
          length=current%n,&
          coord_vals=current%values,&
          cell_bounds=current%bounds, &
          interval='31 days')
     else
     myaxis(ndim-i)=cmor_axis('Tables/CMIP5_Amon', &
          table_entry=current%name,&
          units=current%units,&
          length=current%n,&
          coord_vals=current%values,&
          cell_bounds=current%bounds)
        print*, 'Test Code: not time'
     endif
     current=>current%next
  enddo

  print*,'Test Code: CMOR VARCMOR VARCMOR VARCMOR'

  mymiss=1.e20
  myvar=cmor_variable('Tables/CMIP5_Amon',&
       'ta',&
       'K',&
       myaxis,&
       missing_value=mymiss)

!! figures out length of dimension other than time

  j=ntot/mydims%n
!!$  print*, 'Test Code: &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&'
!!$  print*,'Test Code: before:', shape(arrayin),mydims%n
!!$  print*,'Test Code: before:', shape(arrayin(:,i,:))
!!$  print*, 'Test Code: time before:',mydims%next%values(i:i)
  current=>mydims%next%next
print*, 'Test Code: values:',current%values
print*, 'Test Code: bounds:',current%bounds
print*, 'Test Code: N:',current%n
  ierr = cmor_write( &
       var_id        = myvar, &
       data          = bigarray &
       )
  ierr = cmor_close()

contains
  subroutine allocate_dims(file_id,mydims,ndim,dimlength)
    implicit none
    integer i,n,j,tmp,file_id
    integer, intent(inout)::ndim
    integer, intent(inout):: dimlength(7)
    type(dims) , pointer :: tmpdims,mydims
    read(file_id,'(i8)') ndim
!!$    allocate(dimlength(ndim))
    n=1
    allocate(mydims)
    tmpdims=>mydims
    do i = 1, ndim
       read(file_id,'(I8)') tmp
!!$print*,'Test Code: allocatedat:',tmp
       dimlength(5-i)=tmp
       allocate(tmpdims%values(tmp))
       allocate(tmpdims%bounds(2,tmp))
       tmpdims%n=tmp
       allocate(tmpdims%next)
       tmpdims=>tmpdims%next
       n=n*tmp
    enddo
    deallocate(tmpdims)
  end subroutine allocate_dims
  
  subroutine read_ascii(file_unit,mydims,ndim,ntot,arrayin)
    implicit none
    type(dims), pointer::  mydims
    double precision, dimension(:,:,:,:),intent(inout) :: arrayin
!    real, dimension(:,:,:,:),intent(inout) :: arrayin
    type(dims), pointer ::  current
    integer, intent(in)::ndim,file_unit
    integer n,ntot,i,j,k,l,m
    
    current=>mydims
    ntot=1
    do i =1,ndim
       n=current%n
       ntot=ntot*n
       read(file_unit,'(A)') current%name
       print*, 'Test Code: NAME is:',current%name,trim(adjustl(mydims%name))
       if (current%name.eq."pressure") current%name="plevs"
       read(file_unit,'(A)') current%units
       read(file_unit,*) (current%values(j),j=1,n)
       read(file_unit,*) ((current%bounds(j,k),j=1,2),k=1,n)
       print*, 'Test Code: ',current%bounds(1,1),current%bounds(1,2)
       current=>current%next
    enddo
print*, 'Test Code: arrayin shape:',shape(arrayin)
    read(file_unit,*) ((((arrayin(j,k,l,m),j=1,size(arrayin,1)),k=1,size(arrayin,2)),l=1,size(arrayin,3)),m=1,size(arrayin,4))
print*, 'Test Code: done!'
print* ,'Test Code: ',trim(adjustl(mydims%name))
  end subroutine read_ascii

end program main

