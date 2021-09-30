program main_Stokes
  use library
  use Parameters
  use Isoparametric

  implicit none
  ! - - - - - - - - - - * * * Variables que se usan aqui en main * * * * * * * - - - - - - - - - -
  real(8), allocatable, dimension(:,:) :: A_K, Sv, AK_LU, Solution, N, dN_dxi, dN_deta
  real(4), allocatable, dimension(:,:) :: Fbcsvp
  integer                              :: NoBV, NoBVcol
  !character(len=20)                    :: FileName
 !=============== S O L V E R ===============              
  external                             :: mkl_dgetrfnp, dgetrf, dgetrs
  integer                              :: S_m, S_n, S_lda, S_ldb, S_infoLU, S_nrhs , S_infoSOL
  integer, allocatable, dimension(:)   :: S_ipiv
  character(len=1)                     :: S_trans
  ! - - - - - - - - - - - - - - - * * * Fin * * * * * * * - - - - - - - - - - - - - - - - 
  
  call GeneralInfo( )
  call ReadIntegerFile(10,"elementsP1P1.dat", Nelem, nUne + 1, elements)  
  call ReadRealFile(20,"nodesP1P1.dat", n_nodes,3, nodes) !Para dreducir el numero de subrutinas, usar la sentencia option para 
  call ReadReal(30,"materials.dat", materials)    !Para dreducir el numero de subrutinas, usar la sentencia option para      
  call ReadIntegerFile(40,"pnodesP1P1.dat", n_nodes,2, pnodes)
  call ReadIntegerFile(50,"elementsP1P1.dat", Nelem,nPne + 1, pelements)
  print*, ' '
  print*, '!=============== INFO DURING EXECUTION ===============!'
  
  call GaussQuadrature(gauss_points, gauss_weights)
  call ShapeFunctions(gauss_points, nUne, N, dN_dxi, dN_deta)  
  
  allocate(A_K(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes), AK_LU(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes) )
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
  AK_LU = A_K
  DEALLOCATE( N)
  DEALLOCATE( dN_dxi)
  DEALLOCATE( dN_deta)
  DEALLOCATE( Fbcsvp)
  DEALLOCATE( Sv )
  
  print*,' '
  print*,'!=============== SOLVER (LAPACK) ===============!'
  S_m   = size(AK_LU,1)
  S_n   = size(AK_LU,2)
  S_lda = max(1,size(AK_LU,1)) ! lda ≥ max(1, n).
  allocate( S_ipiv(max(1,min(S_m, S_n)) ) )
  S_trans = 'N'
  S_nrhs  = 1
  S_ldb   = max(1,size(Solution,1))
 
  print*,'  •INITIALIZING LU FACTORIZATION A = P*L*U'
  call dgetrf( S_m, S_n, AK_LU, S_lda, S_ipiv, S_infoLU )
  call MKLfactoResult( S_infoLU )

  print*, ' '  
  print*,'  •SOLVING SYSTEM OF EQUATIONS '
  call dgetrs( S_trans, S_n, S_nrhs, AK_LU, S_lda, S_ipiv, Solution, S_ldb, S_infoSOL )
  call MKLsolverResult( S_infoSOL )
  
  DEALLOCATE( S_ipiv)
  
  call writeMatrix(A_K, 111, 'GlobalK_Triangle.dat', Solution, 444, 'P1P1Sol.dat')
  !write(*,"(A)") 'Name for mesh file...'
  !read(*,"(A)") FileName 
  call PosProcess(Solution, 'StokesP1P1.post.msh', 'msh')
  !write(*,"(A)") 'Name for results file...'
  !read(*,"(A)") FileName 
  call PosProcess(Solution, 'StokesP1P1.post.res', 'res')

  DEALLOCATE( A_K)
  DEALLOCATE( AK_LU)
  DEALLOCATE( Solution)
  print*,' '  
  
  print*, 'Files .post written succesfully on Pos/ . . . . .'
  print*, 'Files .dat written succesfully on Res/ . . . . .'
  print*, ' ' 

end program main_Stokes
