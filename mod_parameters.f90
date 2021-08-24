module Parameters
  
  implicit none
  
  character(len=6), parameter  :: ElemType = 'Quad'
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


  
  
end module Parameters

