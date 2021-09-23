program main_Stokes
  use library
  use Parameters
  use Isoparametric

  implicit none
  ! - - - - - - - - - - * * * Variables que se usan aqui en main * * * * * * * - - - - - - - - - -
  real(8), allocatable, dimension(:,:) :: A_K, Sv, Solution, N, dN_dxi, dN_deta
  real(4), allocatable, dimension(:,:) :: Fbcsvp
  integer                              :: NoBV, NoBVcol
  double precision, dimension(n_nodes,6)  :: results
  character(len=20)                         :: FileName
 !=============== S O L V E R ===============              
  external                             :: mkl_dgetrfnp, dgetrf, dgetrs
  integer                              :: S_m, S_n, S_lda, S_ldb, S_infoLU, S_nrhs , S_infoSOL
  integer, allocatable, dimension(:)   :: S_ipiv
  character(len=1)                     :: S_trans
  ! - - - - - - - - - - - - - - - * * * Fin * * * * * * * - - - - - - - - - - - - - - - - 
  
  call GeneralInfo( )
  call ReadIntegerFile(10,"elementsQ1.dat", Nelem, nUne + 1, elements)  
  call ReadRealFile(20,"nodesQ1.dat", n_nodes,3, nodes) !Para dreducir el numero de subrutinas, usar la sentencia option para 
  call ReadReal(30,"materials.dat", materials)    !Para dreducir el numero de subrutinas, usar la sentencia option para      
  call ReadIntegerFile(40,"pnodesQ1.dat", n_nodes,2, pnodes)
  call ReadIntegerFile(50,"elementsQ1.dat", Nelem,nPne + 1, pelements)
  print*, ' '
  print*, '!=============== INFO DURING EXECUTION ===============!'
  
  call GetQuadGauss(ngp,ngp,gauss_points, gauss_weights)
  !call GetTriGauss(gauss_points, gauss_weights)
  call ShapeFunctions(gauss_points, nUne, N, dN_dxi, dN_deta)  
  
  allocate(A_K(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes))
  call SetBounCond( NoBV, NoBVcol) !Esta funcion crea el archivo bcsVP.dat
  allocate( Fbcsvp(NoBV, NoBVcol) ) !Designo la memoria para la matriz de nodos con valor en la frontera
  call ReadMixFile(60,"Fbcsvp.dat", NoBV, NoBVcol, Fbcsvp)!Llamo el archivo de valores en la frontera y lo guardo en Fbcsvp
  
  call GlobalK( A_K, dN_dxi, dN_deta)

  allocate(Sv(2*n_nodes+n_pnodes, 1), Solution(2*n_nodes+n_pnodes, 1))
  Sv = 0 !initializing source vector (Sv) 
  !========== Una vez calculada la matriz global y el vector de fuente (Sv), les aplicamos las condiciones
  ! de frontera esta subrutina anterior usa como input Sv y A_K y los entrega de nuevo con las BCS aplicadas 
  call ApplyBoundCond(NoBV, Fbcsvp, A_K, Sv )
  !Despues de este ultimo call, obtenemos la matriz y vector global con condiciones de frontera
  
  Solution = Sv !Solucion sera reescrito por la solucion de lapack asi no reescribo el vector global.
  DEALLOCATE( N)
  DEALLOCATE( dN_dxi)
  DEALLOCATE( dN_deta)
  DEALLOCATE( Fbcsvp)
  !call writeMatrix(A_K, 111, 'Stab_GlobalK.txt', Sv, 222, 'Stab_GlobalSv.txt') !write global matrix and vector
  DEALLOCATE( Sv )
  
  print*,' '
  print*,'!=============== SOLVER (LAPACK) ===============!'
  S_m   = size(A_K,1)
  S_n   = size(A_K,2)
  S_lda = max(1,size(A_K,1)) ! lda ≥ max(1, n).
  allocate( S_ipiv(max(1,min(S_m, S_n)) ) )
  S_trans = 'N'
  S_nrhs  = 1
  S_ldb   = max(1,size(Sv,1))
 
  print*,'  •INITIALIZING LU FACTORIZATION A = P*L*U'
  call dgetrf( S_m, S_n, A_K, S_lda, S_ipiv, S_infoLU )
  ! call mkl_dgetrfnp( S_m, S_n, A_K, S_lda, S_infoLU )
  call MKLfactoResult( S_infoLU )

  print*, ' '  
  print*,'  •SOLVING SYSTEM OF EQUATIONS '
  call dgetrs( S_trans, S_n, S_nrhs, A_K, S_lda, S_ipiv, Solution, S_ldb, S_infoSOL )
  ! call dgesv( n, nrhs, a, lda, ipiv, b, ldb, info ) !other option from page 765 lapack manual
  call MKLsolverResult( S_infoSOL )
  
  DEALLOCATE( S_ipiv)
  !call writeMatrix(A_K, 333, 'StabA3K_LU.txt', Solution, 444, 'SolQ1.txt')

  write(*,*) 'Name for posprocess file...'
  read(*,*) FileName 
  call PosProcess(Solution, FileName)

  DEALLOCATE( A_K)
  DEALLOCATE( Solution)
  print*,' '  
  print*, 'Files written succesfully on Res/ . . . . .'
  
  print*, ' ' 

end program main_Stokes
