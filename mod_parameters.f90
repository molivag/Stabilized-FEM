module Parameters
  
  implicit none
  
  character(len=14), parameter  :: ElemType = 'Quadrilateral'
  integer, parameter :: DimPr     = 2     !Dimension del problema 
  integer, parameter :: Nelem     = 4     !Number of elements
  integer, parameter :: n_nodes   = 9    !Total number of velocity nodes
  integer, parameter :: n_pnodes  = 9     !Total number of preasure nodes MAXVAL(pnodes,2)
  integer, parameter :: nUne      = 4     !Number of velocity nodes in the element
  integer, parameter :: nPne      = 4     !Number of preasure nodes in the element
  integer, parameter :: Dof       = 3     !Degrees of fredoom: 2 of velocity + 1 of preasure
  integer, parameter :: totGp     = 1    ! 1,4,9 for Q, 1,3,4,6 for P 



  character(len=20), parameter :: File_element  = 'elementsHandGid.dat'
  character(len=20), parameter :: File_nodes    = 'nodesHandGid.dat'
  character(len=13), parameter :: File_material = 'materials.dat'
  character(len=20), parameter :: File_pnodes   = 'pnodesHandGid.dat'
  character(len=20), parameter :: File_pelement = 'elementsHandGid.dat'
  character(len=20), parameter :: File_PostMsh  = 'HandGidbug.post.msh'
  character(len=20), parameter :: File_PostRes  = 'HandGidbug.post.res'


  double precision, allocatable, dimension(:,:) :: gauss_points, gauss_weights !Verificar si debe ser global---> Si, se usa en la funcion ComputeK
  integer, dimension(Nelem, nUne + 1)    :: elements
  integer, dimension(Nelem, nPne + 1)    :: pelements  
  real,    dimension(n_nodes, DimPr + 1) :: nodes
  integer, dimension(n_nodes, 2)         :: pnodes
  real                                   :: materials
  


end module Parameters

