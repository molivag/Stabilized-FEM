module Parameters
  
  implicit none
  
  character(len=14), parameter  :: ElemType = 'Quadrilateral'!,'Triangle'
  integer, parameter :: DimPr     = 2     !Dimension del problema 
  integer, parameter :: Nelem     = 100   !Number of elements
  integer, parameter :: n_nodes   = 121   !Total number of velocity nodes
  integer, parameter :: n_pnodes  = 121   !Total number of preasure nodes MAXVAL(pnodes,2)
  integer, parameter :: nUne      = 4     !Number of velocity nodes in the element
  integer, parameter :: nPne      = 4     !Number of preasure nodes in the element
  integer, parameter :: Dof       = 3     !Degrees of fredoom: 2 for velocity + 1 one for preasure
  integer, parameter :: ngp       = 3     !Number of Gauss points for quadrature  
  integer, parameter :: totGp     = ngp*ngp

  double precision, allocatable, dimension(:,:) :: gauss_points, gauss_weights !Verificar si debe ser global---> Si, se usa en la funcion ComputeK
  integer, dimension(Nelem, nUne + 1)    :: elements
  integer, dimension(Nelem, nPne + 1)    :: pelements  
  real,    dimension(n_nodes, DimPr + 1) :: nodes
  integer, dimension(n_nodes, 2)         :: pnodes
  real                                   :: materials

  != = = = = =  About Isoparametric Mapping  = = = = =  

!  call ReadIntegerFile(10,"elements_linear.dat", Nelem, nUne + 1, elements)  
!  call ReadRealFile(20,"nodes_linear.dat", n_nodes,3, nodes) !Para dreducir el numero de subrutinas, usar la sentencia option para 
!  call ReadReal(30,"materials.dat", materials)    !Para dreducir el numero de subrutinas, usar la sentencia option para      
!  call ReadIntegerFile(40,"pnodes_linear.dat", n_nodes,2, pnodes)
!  call ReadIntegerFile(50,"elements_linear.dat", Nelem,nPne + 1, pelements)
   
!  subroutine ReadRealFile(UnitNum, FileName, NumRows, NumCols, Real_Array)
!      implicit none
!
!      integer :: i, j, status, UnitNum, NumRows, NumCols
!      character (len=*), intent (in) :: FileName
!      real, dimension (1:NumRows, 1:NumCols), intent (out) :: Real_Array
!
!      open (unit = UnitNum, file =FileName, status='old', action='read' , iostat = status)
!      
!      if(nUne .eq. 8)then
!        goto 5001
!      elseif(nUne .eq. 4)then
!        goto 5002
!      end if
!
      ! read in values
!      5001 read(UnitNum,*) ((Real_Array(i,j), j=1,NumCols), i=1,NumRows)
!      goto 5003
!      5002 read(UnitNum,*) ((Real_Array(i,j), j=1,NumCols), i=2-1,NumRows-1,2)
!      
!      5003 continue
!      
!      print *, "Status_Real_File ", status
!
!      close (UnitNum)
!
!    end subroutine
!
!    subroutine ReadIntegerFile(UnitNum, FileName, NumRows, NumCols, IntegerArray)
!
!      integer :: i, j, status
!      integer, intent(in)            :: UnitNum, NumRows, NumCols
!      character (len=*), intent (in) :: FileName
!      integer, dimension (1:NumRows, 1:NumCols), intent (out) :: IntegerArray
!
!
!      open (unit = UnitNum, file =FileName, status='old', action='read' , iostat = status)
!
!      ! read in values
!      read(UnitNum,*) ((IntegerArray(i,j), j=1,NumCols), i=1,NumRows)
!      print *, "Status_Int_File  ", status
!      print*, "Shape of ",FileName," is ", shape(IntegerArray)
!      close (UnitNum)
!
!    end subroutine ReadIntegerFile
!
!    subroutine ReadReal(UnitNum, FileName, value)
!
!      integer :: status, UnitNum
!      character (len=*), intent (in) :: FileName
!      real :: value
!
!
!      open (unit = UnitNum, file =FileName, status='old', action='read' , iostat = status)
!
!      ! read in values
!      read(UnitNum,*) value
!      print *, "Status_Single_Val", status
!
!      close (UnitNum)

!    end subroutine

  
  
end module Parameters

