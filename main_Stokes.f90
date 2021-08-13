program Stokes
  use library
  use Parameters
  use Isoparametric


  implicit none
  ! - - - - - - - - - - * * * Variables que se usan aqui en main * * * * * * * - - - - - - - - - -
  double precision, allocatable, dimension(:,:) :: A_K
  double precision, allocatable, dimension(:,:) :: N, dN_dxi, dN_deta
  double precision, allocatable, dimension(:,:) :: Sv
  real, allocatable, dimension(:,:)             :: Fbcsvp
  integer                                       :: NoBV, NoBVcol, i, j
  external                                      :: SLEEP
  !========== S O L V E R
  external                                      :: mkl_dgetrfnp, dgetrf, dgetrs
  integer                                       :: S_m, S_n, S_lda, S_ldb, S_infoLU, S_nrhs , S_infoSOL
  integer, allocatable, dimension(:,:)          :: S_ipiv
  character*1                                   :: S_trans
  ! - - - - - - - - - - - - - - - * * * Fin * * * * * * * - - - - - - - - - - - - - - - - 
  
    
  call ReadIntegerFile(20,"elements.dat", Nelem, nUne + 1, elements)  
  call ReadRealFile(10,"nodes.dat", n_nodes,3, nodes) !Para dreducir el numero de subrutinas, usar la sentencia option para 
  call ReadReal(30,"materials.dat", materials)    !Para dreducir el numero de subrutinas, usar la sentencia option para      
  call ReadIntegerFile(40,"pnodes.dat", n_nodes,2, pnodes)
  call ReadIntegerFile(50,"pelements.dat", Nelem,nPne + 1, pelements)
  
  call GetQuadGauss(ngp,ngp,gauss_points, gauss_weights)
  
  call Quad8Nodes(gauss_points, N, dN_dxi, dN_deta)

  allocate(A_K(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes))
  call SetBounCond( NoBV, NoBVcol) !Esta funcion crea el archivo bcsVP.dat
  allocate( Fbcsvp(NoBV, NoBVcol) ) !Designo la memoria para la matriz de nodos con valor en la frontera
  call ReadMixFile(60,"Fbcsvp.dat", NoBV, NoBVcol, Fbcsvp)!Llamo el archivo de valores en la frontera y lo guardo en Fbcsvp
  

  call GlobalK( A_K, dN_dxi, dN_deta)

  ! DEALLOCATE( N)
  ! DEALLOCATE( dN_dxi)
  ! DEALLOCATE( dN_deta)
  
  allocate(Sv(2*n_nodes+n_pnodes, 1))
  Sv = 0 !initializing source vector (Sv) 
  
  
  !========== Una vez calculada la matriz global y el vector de fuente (Sv), les aplicamos las condiciones
  ! de frontera esta subrutina anterior usa como input Sv y A_K y los entrega de nuevo con las BCS aplicadas 
  call ApplyBoundCond(NoBV, Fbcsvp, A_K, Sv )
  !Despues de este ultimo call, obtenemos la matriz y vector global con condiciones de frontera
  ! DEALLOCATE( Fbcsvp)


  open(unit=700, file='AK.dat', ACTION="write", STATUS="replace ")
  do i=1,2*n_nodes+n_pnodes 
    write(700, 100)( A_K(i,j) ,j=1,2*n_nodes+n_pnodes)
  end do
  close(700)

  open(unit=710, file='Source.dat', ACTION="write", STATUS="replace ")
  do i=1,2*n_nodes+n_pnodes 
    write(710, 100) Sv(i,1)
  end do
  close(710)
  
  print*,'!==================== S O L V E R (L A P A C K) ====================!'
  S_m   = size(A_K,1)
  S_n   = size(A_K,2)
  S_lda = max(1,size(A_K,1))
  allocate( S_ipiv( 1, max(1,min(S_m, S_n)) ) )
  S_trans = 'N'
  S_nrhs  = size(Sv,1)
  S_ldb = max(1,size(Sv,1))
  print*,''
  print*,'= = = = = Solver Parameters = = = = = = = = = ='
  print*,'= shape of S_ipiv  ',shape(S_ipiv)
  print*,'= Leading dimension of A_K     ',S_lda
  print*,'= Leading dimension of Sv      ',S_ldb
  print*,'= Number of right-hand sides   ',S_nrhs
  print*,'= = = = = = = = = = = = = = = = = = = = = = = ='
  print*,''
  
  print*,'!=== INITIALIZING LU FACTORIZATION A = P*L*U'
  ! call sleep(2)
  call dgetrf( S_m, S_n, A_K, S_lda, S_ipiv, S_infoLU )
  ! call mkl_dgetrfnp( S_m, S_n, A_K, S_lda, S_infoLU )
  if ( S_infoLU .eq. 0 ) then
    print*,'!===FACTORIZATION DONE WITH STATUS', S_infoLU, ', THE EXECUTION IS SUCCESSFUL.'
  elseif(S_infoLU .lt. 0 )then
    print*,'!===MATRIX FACTORIZED WITH STATUS', S_infoLU, 'THE',S_infoLU,'-TH PARAMETER HAD AN ILLEGAL VALUE.'
  elseif(S_infoLU .gt. 0 )then
    print*,'!===THE FACTORIZATION HAS BEEN COMPLETED, BUT U_',S_infoLU, 'IS EXACTLY SINGULAR.'
    print*, 'DIVISION BY 0 WILL OCCUR IF YOU USE THE FACTOR U FOR SOLVING A SYSTEM OF LINEAR EQUATIONS.'
  endif
  print*, '.'
  print*, '.'
  print*, '.'  
  print*,'!========== SOLVING SYSTEM OF EQUATIONS '
  ! call sleep(2)
  call dgetrs( S_trans, S_n, S_nrhs, A_K, S_lda, S_ipiv, Sv, S_ldb, S_infoSOL )

  if ( S_infoSOL .eq. 0 ) then
    print*,'!=== SYSTEM SOLVED WITH STATUS', S_infoSOL, ', THE EXECUTION IS SUCCESSFUL.'
  elseif(S_infoSOL .lt. 0 )then
    print*,'!=== SYSTEM SOLVED WITH STATUS', S_infoSOL, 'THE',S_infoSOL,'-TH PARAMETER HAD AN ILLEGAL VALUE.'
  endif
  ! call sleep(1)
  print*,' '
  
  ! ========== Escribir en archivo la matriz global
  100 format (900E20.12)
  open(unit=70, file='AK_LU_old.dat', ACTION="write", STATUS="replace ")
  do i=1,2*n_nodes+n_pnodes 
    write(70, 100)( A_K(i,j) ,j=1,2*n_nodes+n_pnodes)
  end do
  close(70)

  open(unit=71, file='Sol_old.dat', ACTION="write", STATUS="replace ")
  do i=1,2*n_nodes+n_pnodes 
    write(71, 100) Sv(i,1)
  end do
  close(71)


  ! DEALLOCATE( N)
  ! DEALLOCATE( dN_dxi)
  ! DEALLOCATE( dN_deta)
  ! DEALLOCATE( Fbcsvp)
  ! DEALLOCATE( S_ipiv)
  ! DEALLOCATE( A_K)
  ! DEALLOCATE( Sv )
  

end program Stokes
