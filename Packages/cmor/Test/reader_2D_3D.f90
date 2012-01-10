  SUBROUTINE read_3d_input_files(it, varname, field)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:,:) :: field
    
    INTEGER :: i, j, k
    REAL :: factor, offset,min,max
    CHARACTER(len=LEN(varname)) :: tmp
    
    min= -1.e20
    max=1.e20
    tmp = TRIM(ADJUSTL(varname))
    SELECT CASE (tmp)
    CASE ('CLOUD')  
       factor = 0.02
       offset = -20.
    CASE ('U')  
       factor = .075
       offset = 45.
       max=21
       min=17
    CASE ('T')
       factor = 1.3
       offset = -27.5
       min=191.
       max=240.
    CASE ('tro3')
       factor = 1.
       offset = 0.
    END SELECT
    
    DO k=1,SIZE(field, 3)
       DO j=1,SIZE(field, 2)
          DO i=1,SIZE(field, 1)
             field(i,j,k) = ((k-1)*64 + (j-1)*16 + (i-1)*4 + it)*factor - offset
             if (field(i,j,k).gt.max) field(i,j,k)=max
             if (field(i,j,k).lt.min) field(i,j,k)=min
          END DO
       END DO
    END DO
    
  END SUBROUTINE read_3d_input_files
  
  SUBROUTINE read_2d_input_files(it, varname, field)

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:) :: field
    
    INTEGER :: i, j
    REAL :: factor, offset,min,max
    CHARACTER(len=LEN(varname)) :: tmp
    
    min= -1.e20
    max=1.e20
    tmp = TRIM(ADJUSTL(varname))
    SELECT CASE (tmp)
    CASE ('LATENT')  
       
       factor = 4.2
       offset = 0.
       min=-65
       max=65
    CASE ('TSURF')
       factor = 2.0
       offset = -230.
       max=285
       min=270
    CASE ('SOIL_WET')
       factor = 4.
       offset = 0.
       max = 128.
    CASE ('PSURF')
       factor = 100.
       offset = -9.4e4
    CASE ('PRECIP')
       factor = 1.2e-6
       offset = 0.
       max = 1.e-3
       min=0.
    CASE ('SFTLF')
       factor = 1.
       offset = -12.
       max = 100.
       min=0.
    END SELECT
    
    DO j=1,SIZE(field, 2)
       DO i=1,SIZE(field, 1)
          field(i,size(field,2)+1-j) = ((j-1)*16 + (i-1)*4 + it)*factor - offset
             if (field(i,size(field,2)+1-j).gt.max) field(i,size(field,2)+1-j)=max
             if (field(i,size(field,2)+1-j).lt.min) field(i,size(field,2)+1-j)=min
       END DO
    END DO

  END SUBROUTINE read_2d_input_files
  SUBROUTINE read_1d_input_files(it, varname, field) 

    IMPLICIT NONE
    
    INTEGER, INTENT(IN) :: it
    CHARACTER(len=*), INTENT(IN) :: varname
    REAL, INTENT(OUT), DIMENSION(:,:) :: field
    
    INTEGER :: i, j
    REAL :: factor, offset,min,max
    CHARACTER(len=LEN(varname)) :: tmp
    
    min=-1.e20
    max=1.e20
    tmp = TRIM(ADJUSTL(varname))
    SELECT CASE (tmp)
    CASE ('OFLUX')  
       factor = .5e14
       offset = 2.e13
       max = 1.492e14
       min = 1.46e14
    END SELECT
    
    DO j=1,SIZE(field, 2)
       DO i=1,SIZE(field, 1)
          field(i,j) = ((j-1)*16 + (i-1)*4 + it)*factor - offset
          if (field(i,j).gt.max) field(i,j)=max
          if (field(i,j).lt.min) field(i,j)=min
       END DO
    END DO

  END SUBROUTINE read_1d_input_files
