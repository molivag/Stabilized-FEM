module Isoparametric
  use Parameters
 
  contains
    
    subroutine GaussQuadrature(gauss_points, gauss_weights)!, xi, eta)
      implicit none
       
      double precision, allocatable, dimension(:,:),intent(out) :: gauss_points, gauss_weights
      real :: a,b, ex1,et1,ez1,ex2,et2,ez2

      allocate(gauss_points(totGp,2))
      allocate(gauss_weights(totGp,1))
      
      ! allocate(xi(size(gauss_points,1),1),eta(size(gauss_points,1),1))
      ! totGp = size(gauss_points,1) ! y declarar totGp con SAVE para tener siempre el valor de la variable totGp 
      gauss_points = 0
      gauss_weights = 0

      Select case(ElemType) 
        case('Quadrilateral') !if(nnode.eq.4. or .nnode.eq.9) then
          if(totGp.eq.1)then 
            gauss_points(1,1) = 0.0
            gauss_points(1,2) = 0.0
            gauss_weights(1,1) = 4.0
          else if(totGp.eq.4) then
            gauss_points(1,1) = -sqrt(1.0/3.0)
            gauss_points(2,1) = sqrt(1.0/3.0)
            gauss_points(3,1) = -sqrt(1.0/3.0)
            gauss_points(4,1) = sqrt(1.0/3.0)
            gauss_points(1,2) = -sqrt(1.0/3.0)
            gauss_points(2,2) = -sqrt(1.0/3.0)
            gauss_points(3,2) = sqrt(1.0/3.0)
            gauss_points(4,2) = sqrt(1.0/3.0)
            gauss_weights(1,1)=1.0
            gauss_weights(2,1)=1.0
            gauss_weights(3,1)=1.0
            gauss_weights(4,1)=1.0
          else if(totGp.eq.9) then
            gauss_points(1,1)=-sqrt(3.0/5.0)
            gauss_points(2,1)= 0.0
            gauss_points(3,1)= sqrt(3.0/5.0)
            gauss_points(4,1)=-sqrt(3.0/5.0)
            gauss_points(5,1)= 0.0
            gauss_points(6,1)= sqrt(3.0/5.0)
            gauss_points(7,1)=-sqrt(3.0/5.0)
            gauss_points(8,1)= 0.0
            gauss_points(9,1)= sqrt(3.0/5.0)
            gauss_points(1,2)=-sqrt(3.0/5.0)
            gauss_points(2,2)=-sqrt(3.0/5.0)
            gauss_points(3,2)=-sqrt(3.0/5.0)
            gauss_points(4,2)= 0.0
            gauss_points(5,2)= 0.0
            gauss_points(6,2)= 0.0
            gauss_points(7,2)= sqrt(3.0/5.0)
            gauss_points(8,2)= sqrt(3.0/5.0)
            gauss_points(9,2)= sqrt(3.0/5.0)
            gauss_weights(1,1)= 25.0/81.0
            gauss_weights(2,1)= 40.0/81.0
            gauss_weights(3,1)= 25.0/81.0
            gauss_weights(4,1)= 40.0/81.0
            gauss_weights(5,1)= 64.0/81.0
            gauss_weights(6,1)= 40.0/81.0
            gauss_weights(7,1)= 25.0/81.0
            gauss_weights(8,1)= 40.0/81.0
            gauss_weights(9,1)= 25.0/81.0
          end if
        case('Triangle') !else if(nnode.eq.3. or .nnode.eq.6) then
          if(totGp.eq.1) then
            gauss_points(1,1) = 1.0/3.0
            gauss_points(1,2) = 1.0/3.0
            gauss_weights(1,1) = 0.5
          else if(totGp.eq.3) then
            gauss_points(1,1) = 0.0
            gauss_points(2,1) = 0.5
            gauss_points(3,1) = 0.5
            gauss_points(1,2) = 0.5
            gauss_points(2,2) = 0.0
            gauss_points(3,2) = 0.5
            gauss_weights(1,1) =1.0/6.0
            gauss_weights(2,1) =1.0/6.0
            gauss_weights(3,1) =1.0/6.0
          else if(totGp.eq.4) then
            gauss_points(1,1) =  1.0/3.0
            gauss_points(2,1) =  1.0/5.0
            gauss_points(3,1) =  1.0/5.0
            gauss_points(4,1) =  3.0/5.0
            gauss_points(1,2) =  1.0/3.0
            gauss_points(2,2) =  3.0/5.0
            gauss_points(3,2) =  1.0/5.0
            gauss_points(4,2) =  1.0/5.0
            gauss_weights(1,1) = -27.0/96.0
            gauss_weights(2,1) = 25.0/96.0
            gauss_weights(3,1) = 25.0/96.0
            gauss_weights(4,1) = 25.0/96.0
          else if(totGp.eq.6) then
            ex1 = 0.816847572980459 !0.81684 75729 80459 verificar si es un vector fila o un real de 18 digitos
            et1 = 0.091576213509771
            ez1 = 0.091576213509771
            ex2 = 0.108103018168070
            et2 = 0.445948490915965
            ez2 = 0.445948490915965
            gauss_points(1,1) = ex1
            gauss_points(2,1) = et1
            gauss_points(3,1) = ez1
            gauss_points(4,1) = ex2
            gauss_points(5,1) = et2
            gauss_points(6,1) = ez2
            gauss_points(1,2) = et1
            gauss_points(2,2) = ez1
            gauss_points(3,2) = ex1
            gauss_points(4,2) = et2
            gauss_points(5,2) = ez2
            gauss_points(6,2) = ex2
            a = 0.054975870996713638
            b = 0.1116907969117165    
            gauss_weights(1,1) = a
            gauss_weights(2,1) = a
            gauss_weights(3,1) = a
            gauss_weights(4,1) = b
            gauss_weights(5,1) = b
            gauss_weights(6,1) = b
          end if
        case DEFAULT
          write(*,*) 'Invalid type of element.'   
      end select
      
      
      
    end subroutine GaussQuadrature

    subroutine ShapeFunctions(gauss_points, Nne,  N, dN_dxi, dN_deta )  
      implicit None

      double precision, dimension(:,:), intent(in)               :: gauss_points
      double precision, allocatable, dimension(:,:), intent(out) :: N
      double precision, allocatable, dimension(:,:), intent(out), optional :: dN_dxi, dN_deta
      double precision, dimension(totGp) :: xi_vector, eta_vector
      integer, dimension(Nne,DimPr)                     :: master_nodes
      double precision                                  :: xi, eta, mn_xi, mn_eta
      integer                                           :: i, j, jj, k, Nne
      
      ! = = = = = = = = = = = = = = = = = = = = = = = = = = =
      100 format (3A, 1x, I1, 1x, A)
      
      allocate( N(Nne,totGp) )
      N = 0.0
      xi_vector  = gauss_points(:,1)     ! xi-coordinate of point j
      eta_vector = gauss_points(:,2)

      select case(ElemType)

        CASE ('Quadrilateral')
          select case(Nne)
          case  (8)
           ! Shape functions for square (quadrilaters) linear elements
           !
           !  |
           !  |
           !  | o- - o - -o
           !  Y |         |
           !  | o         o
           !  | |         |
           !  | o- - o - -o
           !  |
           !  +--------X-------->
            write(*,100) ' Element type: ', ElemType, 'whit', Nne, 'nodes per element'
            !coordinates of the nodes of the master element
            master_nodes = reshape([1, -1, -1, 1, 0, -1, 0, 1, 1, 1, -1, -1, 1, 0, -1, 0], [Nne,DimPr])
            !NOTA ** Para que el reshape funcione correctamente, o que produzca el par de valores deseado, primero se deben
            !colocar todos los valores en x, luego todos los de y y luego, si hubiera todos los de z para que al acomodarse salga el par
            !suponiendo un reshape de 3,2 debe acomodarse x1, x2, x3, y1, y2, y3 DUDA *Siempre es asi*
            ! dN(xi,eta)/dx = dN/dxi(dxi/dx) + dN/deta(deta/dx)
            ! dN(xi,eta)/dy = dN/dxi(dxi/dy) + dN/deta(deta/dy)
            ! Aqui se calculan as funciones de forma N y parte de las derivadas dN/dxi and dN_deta
            ! mas no las derivadas dN/dx and dN/dy completas

            if (present(dN_dxi) .and. present(dN_deta))then
              write(*,*) 'Using derivatives of shape functions -', Nne
              allocate(dN_dxi(Nne,totGp) )
              allocate(dN_deta(Nne,totGp))
              dN_dxi  = 0.0
              dN_deta = 0.0

              do j = 1, totGp
                xi  = xi_vector(j) !xi-cor of point j
                eta = eta_vector(j)!eta-cor of point j
              
                dN_dxi(5,j)=-xi*(1+eta)
                dN_dxi(6,j)=-1.0/2*(1-eta**2)
                dN_dxi(7,j)=-xi*(1-eta)
                dN_dxi(8,j)=1.0/2*(1-eta**2)
                dN_deta(5,j)=1.0/2*(1-xi**2)
                dN_deta(6,j)=(1-xi)*(-eta)
                dN_deta(7,j)=-1.0/2*(1-xi**2)
                dN_deta(8,j)=(1+xi)*(-eta)
              
                do i = 1, 4
                  mn_xi = master_nodes(i,1)
                  mn_eta= master_nodes(i,2)
                  if (i==1) then
                    jj=8
                  else
                    jj=i+3
                  end if
                  k=i+4
                  dN_dxi(i,j)= mn_xi*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(dN_dxi(jj,j)+dN_dxi(k,j))
                  dN_deta(i,j)= mn_eta*(1.0 + mn_xi*xi)/4.0 - 1.0/2*(dN_deta(jj,j)+dN_deta(k,j))
              
                end do
              
              end do
            else
              write(*,*) 'Using only shape functions'
              continue
            end if

            !Despues de evaluar si estan las derivadas como variable dummy en la llamada construye las
            !funciones de forma.
            do j = 1, totGp
              xi  = xi_vector(j) ! xi-cor of point j
              eta = eta_vector(j)! eta-cor of point j
              N(5,j)=1.0/2*(1-xi**2)*(1+eta)
              N(6,j)=1.0/2*(1-xi)*(1-eta**2)
              N(7,j)=1.0/2*(1-xi**2)*(1-eta)
              N(8,j)=1.0/2*(1+xi)*(1-eta**2)
              do i = 1, 4
                mn_xi = master_nodes(i,1)
                mn_eta= master_nodes(i,2)
                if (i==1) then
                  jj=8
                else
                  jj=i+3
                end if
                k=i+4
                N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(N(jj,j)+N(k,j))
              end do
            end do

          case  (4)
            ! Shape functions for square (quadrilaters) linear elements
            !
            !  |
            !  |
            !  | o- - - -o
            !  Y |       |
            !  | |       |
            !  | o- - - -o
            !  |
            !  +--------X-------->
            
            write(*,100) ' Element type: ', ElemType, 'whit', Nne, 'nodes per element'
            !coordinates of the nodes of the master element
            master_nodes = reshape([1, -1, -1, 1, 1, 1, -1, -1], [Nne,DimPr])

            ! dN(xi,eta)/dx = dN/dxi(dxi\dx) + dN/deta(deta/dx)
            ! dN(xi,eta)/dy = dN/dxi(dxi\dy) + dN/deta(deta/dy)
            ! Aqui se calculan as funciones de forma N y parte de las derivadas dN/dxi and dN_deta
            ! mas no las derivadas dN/dx and dN/dy completas
            !do loop: compute N, dN_dxi, dN_deta
            if (present(dN_dxi) .and. present(dN_deta))then
              write(*,*) 'Using derivatives of shape functions', Nne
              allocate(dN_dxi(Nne,totGp) )
              allocate(dN_deta(Nne,totGp) )
              dN_dxi  = 0.0
              dN_deta = 0.0
              do j=1,totGp                              ! columns for point 1,2 ...
                xi=xi_vector(j);                      ! xi-coordinate of point j 
                eta=eta_vector(j);                    ! eta-coordinate of point j 
                do i=1,4                              ! rows for N1, N2, ...
                  mn_xi = master_nodes(i,1)
                  mn_eta= master_nodes(i,2)
                  dN_dxi(i,j)= mn_xi*(1.0 + mn_eta*eta)/4.0             ! dNi/dxi(xi,eta)
                  dN_deta(i,j)= mn_eta*(1.0 + mn_xi*xi )/4.0            ! dNi/deta(xi,eta
                end do
              end do
            else
              write(*,*) 'Using only shape functions'
              continue
            endif
            
            do j=1,totGp                            ! columns for point 1,2 ...
              xi=xi_vector(j);                      ! xi-coordinate of point j 
              eta=eta_vector(j);                    ! eta-coordinate of point j 
              do i=1,4                              ! rows for N1, N2, ...
                mn_xi = master_nodes(i,1)
                mn_eta= master_nodes(i,2)
                N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0        ! Ni(xi,eta)
              end do
            end do
            
          case DEFAULT
            write(*,*) 'Invalid number of nodes in the element.'
          end select
          
        CASE ('Triangle')
          !  |
          !  |        o
          !  |       / \
          !  |      /   \
          !  Y     /     \
          !  |    /       \
          !  |   o---------o
          !  |
          !  +--------X-------->
          
          if(Nne .EQ. 3) then
            if (present(dN_dxi) .and. present(dN_deta))then
              write(*,*) 'Using derivatives of shape functions', Nne
              allocate(dN_dxi(Nne,totGp) )
              allocate(dN_deta(Nne,totGp) )
              dN_dxi  = 0.0
              dN_deta = 0.0
              do j=1,totGp
                xi=xi_vector(j);  ! xi-coordinate of point j 
                eta=eta_vector(j); 
                N(1,j)      = 1.0-xi-eta
                N(2,j)      = xi
                N(3,j)      = eta
                dN_dxi(1,j) = -1.0
                dN_dxi(2,j) = 1.0
                dN_dxi(3,j) = 0.0
                dN_deta(1,j)= -1.0
                dN_deta(2,j)= 0.0
                dN_deta(3,j)= 1.0
              end do
            else
              write(*,*) 'Using only shape functions'
              continue
            endif
            do j=1,totGp
              xi=xi_vector(j);    ! xi-coordinate of point j 
              eta=eta_vector(j); 
              N(1,j) = 1.0-xi-eta
              N(2,j) = xi
              N(3,j) = eta
            end do
          elseif(Nne .EQ. 6) then
            if (present(dN_dxi) .and. present(dN_deta))then
              allocate(dN_dxi(Nne,totGp) )
              allocate(dN_deta(Nne,totGp) )
              dN_dxi  = 0.0
              dN_deta = 0.0
              do j=1,totGp
                xi=xi_vector(j);  ! xi-coordinate of point j 
                eta=eta_vector(j); 
                N(1,j)=(2.0*(1-xi-eta)-1.0)*(1-xi-eta)
                N(2,j)=(2.0*xi-1.0)*xi
                N(3,j)=(2.0*eta-1.0)*eta
                N(4,j)= 4.0*(1-xi-eta)*xi
                N(5,j)= 4.0*xi*eta
                N(6,j)= 4.0*(1-xi-eta)*eta
                dN_dxi(1,j)=1.0-4.0*(1-xi-eta)
                dN_dxi(2,j)=4.0*xi-1.0
                dN_dxi(3,j)=0.0
                dN_dxi(4,j)=4.0*((1-xi-eta)-xi)
                dN_dxi(5,j)=4.0*eta
                dN_dxi(6,j)=-4.0*eta
                dN_deta(1,j)=1.0-4.0*(1-xi-eta)
                dN_deta(2,j)=0.0
                dN_deta(3,j)=4.0*eta-1.0
                dN_deta(4,j)=-4.0*xi
                dN_deta(5,j)=4.0*xi
                dN_deta(6,j)=4.0*((1-xi-eta)-eta)
                !dN_dxi(1,j) = -2*(1-xi-eta-0.5)-2*(1-xi-eta) 
                !dN_dxi(2,j) = 2*(xi-0.5) + 2*xi 
                !dN_dxi(3,j) = 0
                !dN_deta(1,j)= -2*(1-xi-eta-0.5)-2*(1-xi-eta) 
                !dN_deta(2,j)= 0
                !dN_deta(3,j)= 2*(eta-0.5) + 2*eta 
              end do
            else
              write(*,*) 'Using only shape functions'
              continue
            endif
            N(1,j)=(2.0*(1-xi-eta)-1.0)*(1-xi-eta)
            N(2,j)=(2.0*xi-1.0)*xi
            N(3,j)=(2.0*eta-1.0)*eta
            N(4,j)= 4.0*(1-xi-eta)*xi
            N(5,j)= 4.0*xi*eta
            N(6,j)= 4.0*(1-xi-eta)*eta
          else
            print*, 'Invalid number of node in element' 
          end if
         
        case DEFAULT
          write(*,*) 'Invalid type of element.'   
      end select

    end subroutine ShapeFunctions




    !Rutinas Obsoletas, para consulta y verifiacion

    ! = = = = = = = = = = = = = = = = = = = = =  = = = = = = 
!    subroutine Quad8Nodes(gauss_points,  N, dN_dxi, dN_deta)
!      implicit None
!
!      ! Shape functions for square (quadrilaters) linear elements
!			!
!			!  |
!			!  |
!			!  | o- - o - -o
!			!  Y |         |
!			!  | o         o
!			!  | |         |
!			!  | o- - o - -o
!			!  |
!			!  +--------X-------->
!      
!
!      integer, parameter  :: Nne = 8          !Esto se puede quitar poniendo un modulo con todos las variables globales incluyendo las de mod_library
!      integer, parameter  :: dim_prob = 2
!
!      ! integer,                          intent(in) :: totGp
!      double precision, dimension(:,:), intent(in) :: gauss_points
!      double precision, allocatable, dimension(:,:), intent(out) :: N, dN_dxi, dN_deta
!      double precision, dimension(size(gauss_points,1)) :: xi_vector, eta_vector
!      integer, dimension(Nne,dim_prob) :: master_nodes
!      double precision    :: xi, eta, mn_xi, mn_eta
!      integer             :: i, j, jj, k
!
!      ! !number of gauss points
!      ! totGp = size(gauss_points,1) ! esta puede quedar como variable global si se usa en alguna otra subrutina
!      !                           ! si solo se usa aqui, entonces variable como local-----> Si se usa en otra rutina, en compK
!      allocate( N(Nne,totGp),dN_dxi(Nne,totGp),dN_deta(Nne,totGp) )
!
!      N       = 0.0
!      dN_dxi  = 0.0
!      dN_deta = 0.0
!
!      xi_vector  = gauss_points(:,1)     ! xi-coordinate of point j
!      eta_vector = gauss_points(:,2)
!
!      !coordinates of the nodes of the master element
!      master_nodes = reshape([1, -1, -1, 1, 0, -1, 0, 1, 1, 1, -1, -1, 1, 0, -1, 0], [Nne,dim_prob])
!      !NOTA ** Para que el reshape funcione correctamente, o que produzca el par de valores deseado, primero se deben
!      !colocar todos los valores en x, luego todos los de y y luego, si hubiera todos los de z para que al acomodarse salga el par
!      !suponiendo un reshape de 3,2 debe acomodarse x1, x2, x3, y1, y2, y3 DUDA *Siempre es asi*
!
!      ! dN(xi,eta)/dx = dN/dxi(dxi\dx) + dN/deta(deta/dx)
!      ! dN(xi,eta)/dy = dN/dxi(dxi\dy) + dN/deta(deta/dy)
!      ! Aqui se calculan as funciones de forma N y parte de las derivadas dN/dxi and dN_deta
!      ! mas no las derivadas dN/dx and dN/dy completas
!
!      do j = 1, totGp
!        xi  = xi_vector(j)      ! xi-coordinate of point j
!        eta = eta_vector(j)     ! eta-coordinate of point j
!
!        N(5,j)=1.0/2*(1-xi**2)*(1+eta)
!        N(6,j)=1.0/2*(1-xi)*(1-eta**2)
!        N(7,j)=1.0/2*(1-xi**2)*(1-eta)
!        N(8,j)=1.0/2*(1+xi)*(1-eta**2)
!        dN_dxi(5,j)=-xi*(1+eta)
!        dN_dxi(6,j)=-1.0/2*(1-eta**2)
!        dN_dxi(7,j)=-xi*(1-eta)
!        dN_dxi(8,j)=1.0/2*(1-eta**2)
!        dN_deta(5,j)=1.0/2*(1-xi**2)
!        dN_deta(6,j)=(1-xi)*(-eta)
!        dN_deta(7,j)=-1.0/2*(1-xi**2)
!        dN_deta(8,j)=(1+xi)*(-eta)
!
!        do i = 1, 4
!          mn_xi = master_nodes(i,1)
!          mn_eta= master_nodes(i,2)
!          if (i==1) then
!            jj=8
!          else
!            jj=i+3
!          end if
!          k=i+4
!          N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(N(jj,j)+N(k,j))
!          dN_dxi(i,j)= mn_xi*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(dN_dxi(jj,j)+dN_dxi(k,j))
!          dN_deta(i,j)= mn_eta*(1.0 + mn_xi*xi)/4.0 - 1.0/2*(dN_deta(jj,j)+dN_deta(k,j))
!
!        end do
!
!      end do
!
!    end subroutine Quad8Nodes
!
!    subroutine Quad4Nodes (gauss_points,  N, dN_dxi, dN_deta)
!      implicit None
!
!      !CompNDNatPointsQuad8
!      ! Shape functions for square (quadrilaters) linear elements
!			!
!			!  |
!			!  |
!			!  | o- - - -o
!			!  Y |       |
!			!  | |       |
!			!  | o- - - -o
!			!  |
!			!  +--------X-------->
!      
!
!      integer, parameter  :: Nne = 4        !Esto se puede quitar poniendo un modulo con todos las variables globales incluyendo las de mod_library
!      integer, parameter  :: dim_prob = 2
!
!      ! integer,                          intent(in) :: totGp
!      double precision, dimension(:,:), intent(in) :: gauss_points
!      double precision, allocatable, dimension(:,:), intent(out) :: N
!      double precision, allocatable, dimension(:,:), intent(out), optional :: dN_dxi, dN_deta
!      double precision, dimension(size(gauss_points,1)) :: xi_vector, eta_vector
!      integer, dimension(Nne,dim_prob) :: master_nodes
!      double precision    :: xi, eta, mn_xi, mn_eta
!      integer             :: i, j
!
!      !si solo se usa aqui, entonces variable como local-----> Si se usa en otra rutina, en compK
!      !Al parecer este alojamiento de memoria evita el error despues de ejecutar LAPACK
!      
!
!
!      xi_vector  = gauss_points(:,1)     ! xi-coordinate of point j
!      eta_vector = gauss_points(:,2)
!
!      !coordinates of the nodes of the master element
!      master_nodes = reshape([1, -1, -1, 1, 1, 1, -1, -1], [Nne,dim_prob])
!      !NOTA ** Para que el reshape funcione correctamente, o que produzca el par de valores deseado, primero se deben
!      !colocar todos los valores en x, luego todos los de y y luego, si hubiera todos los de z para que al acomodarse salga el par
!      !suponiendo un reshape de 3,2 debe acomodarse x1, x2, x3, y1, y2, y3 DUDA *Siempre es asi*
!
!      ! dN(xi,eta)/dx = dN/dxi(dxi\dx) + dN/deta(deta/dx)
!      ! dN(xi,eta)/dy = dN/dxi(dxi\dy) + dN/deta(deta/dy)
!      ! Aqui se calculan as funciones de forma N y parte de las derivadas dN/dxi and dN_deta
!      ! mas no las derivadas dN/dx and dN/dy completas
!           
!      !do loop: compute N, dN_dxi, dN_deta
!      if (present(dN_dxi) .and. present(dN_deta))then
!        allocate(dN_dxi(Nne,totGp) )
!        allocate(dN_deta(Nne,totGp) )
!        dN_dxi  = 0.0
!        dN_deta = 0.0
!        
!        do j=1,totGp                              ! columns for point 1,2 ...
!          xi=xi_vector(j);                      ! xi-coordinate of point j 
!          eta=eta_vector(j);                    ! eta-coordinate of point j 
!          do i=1,4                              ! rows for N1, N2, ...
!            mn_xi = master_nodes(i,1)
!            mn_eta= master_nodes(i,2)
!            dN_dxi(i,j)= mn_xi*(1.0 + mn_eta*eta)/4.0             ! dNi/dxi(xi,eta)
!            dN_deta(i,j)= mn_eta*(1.0 + mn_xi*xi )/4.0            ! dNi/deta(xi,eta
!          end do
!        end do
!      else
!        allocate(N(Nne,totGp) )
!        N     = 0.0
!        do j=1,totGp                              ! columns for point 1,2 ...
!          xi=xi_vector(j);                      ! xi-coordinate of point j 
!          eta=eta_vector(j);                    ! eta-coordinate of point j 
!          do i=1,4                              ! rows for N1, N2, ...
!            mn_xi = master_nodes(i,1)
!            mn_eta= master_nodes(i,2)
!            N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0        ! Ni(xi,eta)
!          end do
!        end do
!
!      endif
!
!
!      
!
!    end subroutine Quad4Nodes
!
!  	! != = = = = = = = = = = = = = = = = = = = = = =
!    subroutine Triang3pElem( gauss_points, N, dN_dxi, dN_deta)
!      implicit none
!
!			!  
!			!  |
!			!  |        o
!			!  |       / \
!			!  |      /   \
!			!  Y     /     \
!			!  |    /       \
!			!  |   /         \
!			!  |  o-----------o
!			!  |
!			!  +--------X-------->
!
!
!
!      integer, parameter  :: Nne = 4
!      integer, parameter  :: dim_prob = 2   !Esto se puede quitar poniendo un modulo con todos las variables globales incluyendo las de mod_library
!
!      double precision, dimension(:,:), intent(in) :: gauss_points
!      double precision, allocatable, dimension(:,:), intent(out) :: N
!      double precision, allocatable, dimension(:,:), intent(out), optional :: dN_dxi, dN_deta
!      double precision, dimension(size(gauss_points,1)) :: xi_vector, eta_vector
!      double precision    :: xi, eta
!      integer             :: j
!
!      allocate(N(Nne,totGp) )
!      allocate(dN_dxi(Nne,totGp) )
!      allocate(dN_deta(Nne,totGp) )
!      N = 0.0
!      dN_dxi  = 0.0
!      dN_deta = 0.0
!
!      xi_vector  = gauss_points(:,1)     ! xi-coordinate of point j
!      eta_vector = gauss_points(:,2)
!
!      do j=1,totGp
!        xi=xi_vector(j);                      ! xi-coordinate of point j 
!        eta=eta_vector(j); 
!
!        N(1,j)      =  2*(1-xi-eta)*(1-xi-eta-0.5)
!        dN_dxi(1,j) = -2*(1-xi-eta-0.5)-2*(1-xi-eta) 
!        dN_deta(1,j)= -2*(1-xi-eta-0.5)-2*(1-xi-eta) 
!
!        N(2,j)      = 2*xi*(xi-0.5)
!        dN_dxi(2,j) = 2*(xi-0.5) + 2*xi 
!        dN_deta(2,j)= 0
!
!        N(3,j)      =  2*eta*(eta-0.5)
!        dN_dxi(3,j) = 0
!        dN_deta(3,j)= 2*(eta-0.5) + 2*eta 
!
!      end do
!      
!      
!
!    end subroutine Triang3pElem
!
!    ! = = = = = = = = = = = = = = = = = = = = =  = = = = = = 
!    subroutine Shape(gauss_points, Nne, totGp,  N, dN_dxi, dN_deta )
!      implicit None
!
!      integer, intent(in) :: Nne, totGp
!      double precision, dimension(:,:), intent(in)               :: gauss_points
!      double precision, allocatable, dimension(:,:), intent(out) :: N
!      double precision, allocatable, dimension(:,:), intent(out), optional :: dN_dxi, dN_deta
!      double precision, dimension(totGp) :: xi_vector, eta_vector
!      integer, dimension(Nne,DimPr)                     :: master_nodes
!      double precision                                  :: xi, eta, mn_xi, mn_eta
!      integer                                           :: i, j, jj, k
!      
!      ! = = = = = = = = = = = = = = = = = = = = = = = = = = =
!    
!
!      xi_vector  = gauss_points(:,1)     ! xi-coordinate of point j
!      eta_vector = gauss_points(:,2)
!
!      100 format (3A, 1x, I1, 1x, A)      
!
!      select case(ElemType)
!
!        CASE ('Quad')
!          select case(nne)
!          case  (8)
!            write(*,100) 'Element type: ', ElemType, 'whit', nne, 'nodes per element'
!           ! Shape functions for square (quadrilaters) linear elements
!           !
!           !  |
!           !  |
!           !  | o- - o - -o
!           !  Y |         |
!           !  | o         o
!           !  | |         |
!           !  | o- - o - -o
!           !  |
!           !  +--------X-------->
!
!            !coordinates of the nodes of the master element
!            master_nodes = reshape([1, -1, -1, 1, 0, -1, 0, 1, 1, 1, -1, -1, 1, 0, -1, 0], [Nne,DimPr])
!            !NOTA ** Para que el reshape funcione correctamente, o que produzca el par de valores deseado, primero se deben
!            !colocar todos los valores en x, luego todos los de y y luego, si hubiera todos los de z para que al acomodarse salga el par
!            !suponiendo un reshape de 3,2 debe acomodarse x1, x2, x3, y1, y2, y3 DUDA *Siempre es asi*
!            ! dN(xi,eta)/dx = dN/dxi(dxi\dx) + dN/deta(deta/dx)
!            ! dN(xi,eta)/dy = dN/dxi(dxi\dy) + dN/deta(deta/dy)
!            ! Aqui se calculan as funciones de forma N y parte de las derivadas dN/dxi and dN_deta
!            ! mas no las derivadas dN/dx and dN/dy completas
!
!            if (present(dN_dxi) .and. present(dN_deta))then
!              write(*,*) 'Escribo solo derivadas de funciones base'
!              allocate( N(Nne,totGp) )
!              allocate(dN_dxi(Nne,totGp) )
!              allocate(dN_deta(Nne,totGp))
!              N = 0.0
!              dN_dxi  = 0.0
!              dN_deta = 0.0
!              do j = 1, totGp
!                xi  = xi_vector(j)      ! xi-coordinate of point j
!                eta = eta_vector(j)     ! eta-coordinate of point j
!                N(5,j)=1.0/2*(1-xi**2)*(1+eta)
!                N(6,j)=1.0/2*(1-xi)*(1-eta**2)
!                N(7,j)=1.0/2*(1-xi**2)*(1-eta)
!                N(8,j)=1.0/2*(1+xi)*(1-eta**2)
!                dN_dxi(5,j)=-xi*(1+eta)
!                dN_dxi(6,j)=-1.0/2*(1-eta**2)
!                dN_dxi(7,j)=-xi*(1-eta)
!                dN_dxi(8,j)=1.0/2*(1-eta**2)
!                dN_deta(5,j)=1.0/2*(1-xi**2)
!                dN_deta(6,j)=(1-xi)*(-eta)
!                dN_deta(7,j)=-1.0/2*(1-xi**2)
!                dN_deta(8,j)=(1+xi)*(-eta)
!
!                do i = 1, 4
!                  mn_xi = master_nodes(i,1)
!                  mn_eta= master_nodes(i,2)
!                  if (i==1) then
!                    jj=8
!                  else
!                    jj=i+3
!                  end if
!                  k=i+4
!                  N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(N(jj,j)+N(k,j))
!                  dN_dxi(i,j)= mn_xi*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(dN_dxi(jj,j)+dN_dxi(k,j))
!                  dN_deta(i,j)= mn_eta*(1.0 + mn_xi*xi)/4.0 - 1.0/2*(dN_deta(jj,j)+dN_deta(k,j))
!                end do
!              end do
!            else
!              write(*,*) 'Escribo solo funciones base'
!              allocate( N(Nne,totGp) )
!              N = 0.0
!              do j = 1, totGp
!                xi  = xi_vector(j)      ! xi-coordinate of point j
!                eta = eta_vector(j)     ! eta-coordinate of point j
!                N(5,j)=1.0/2*(1-xi**2)*(1+eta)
!                N(6,j)=1.0/2*(1-xi)*(1-eta**2)
!                N(7,j)=1.0/2*(1-xi**2)*(1-eta)
!                N(8,j)=1.0/2*(1+xi)*(1-eta**2)
!                do i = 1, 4
!                  mn_xi = master_nodes(i,1)
!                  mn_eta= master_nodes(i,2)
!                  if (i==1) then
!                    jj=8
!                  else
!                    jj=i+3
!                  end if
!                  k=i+4
!                  N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0 - 1.0/2*(N(jj,j)+N(k,j))
!                end do
!              end do
!            end if
!          case  (4)
!           !  Bilinear shape functions 
!              
!           !  |
!           !  | o- - - -o
!           !  Y |       |
!           !  | |       |
!           !  | |       |
!           !  | o- - - -o
!           !  |
!           !  +- - - x - - ->
!
!            write(*,100) ' Element type: ', ElemType, 'whit', nne, 'nodes per element'
!            !do loop: compute N, dN_dxi, dN_deta
!            if (present(dN_dxi) .and. present(dN_deta))then
!              write(*,*) 'Escribo solo derivadas de funciones base de 4 nodos'
!              allocate(N(Nne,totGp) )
!              allocate(dN_dxi(Nne,totGp) )
!              allocate(dN_deta(Nne,totGp) )
!              N = 0.0
!              dN_dxi  = 0.0
!              dN_deta = 0.0
!
!              do j=1,totGp                            ! columns for point 1,2 ...
!                xi=xi_vector(j);                      ! xi-coordinate of point j 
!                eta=eta_vector(j);                    ! eta-coordinate of point j 
!                do i=1,4                              ! rows for N1, N2, ...
!                  mn_xi = master_nodes(i,1)
!                  mn_eta= master_nodes(i,2)
!                  N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0
!                  dN_dxi(i,j)= mn_xi*(1.0 + mn_eta*eta)/4.0             ! dNi/dxi(xi,eta)
!                  dN_deta(i,j)= mn_eta*(1.0 + mn_xi*xi )/4.0            ! dNi/deta(xi,eta
!                end do
!              end do
!            else
!              allocate(N(Nne,totGp) )
!              N     = 0.0
!              write(*,*) 'Escribo funciones base de 4 nodos'
!              do j=1,totGp                            ! columns for point 1,2 ...
!                xi=xi_vector(j);                      ! xi-coordinate of point j 
!                eta=eta_vector(j);                    ! eta-coordinate of point j 
!                do i=1,4                              ! rows for N1, N2, ...
!                  mn_xi = master_nodes(i,1)
!                  mn_eta= master_nodes(i,2)
!                  N(i,j)=(1.0 + mn_xi*xi)*(1.0 + mn_eta*eta)/4.0        ! Ni(xi,eta)
!                end do
!              end do
!            endif
!          case DEFAULT
!            write(*,*) 'Invalid number of nodes in the element.'
!          end select
!
!        CASE ('Trian')
!			    !  |
!			    !  |        o
!			    !  |       / \
!			    !  |      /   \
!			    !  Y     /     \
!			    !  |    /       \
!			    !  |   /         \
!			    !  |  o-----------o
!			    !  |
!			    !  +--------X-------->
!          allocate(dN_dxi(Nne,totGp) )
!          allocate(dN_deta(Nne,totGp) )
!
!          dN_dxi  = 0.0
!          dN_deta = 0.0
!          xi_vector  = gauss_points(:,1)     ! xi-coordinate of point j
!          eta_vector = gauss_points(:,2)
!          do j=1,totGp
!            xi=xi_vector(j);                      ! xi-coordinate of point j 
!            eta=eta_vector(j); 
!            N(1,j)      =  2*(1-xi-eta)*(1-xi-eta-0.5)
!            dN_dxi(1,j) = -2*(1-xi-eta-0.5)-2*(1-xi-eta) 
!            dN_deta(1,j)= -2*(1-xi-eta-0.5)-2*(1-xi-eta) 
!            N(2,j)      = 2*xi*(xi-0.5)
!            dN_dxi(2,j) = 2*(xi-0.5) + 2*xi 
!            dN_deta(2,j)= 0
!            N(3,j)      =  2*eta*(eta-0.5)
!            dN_dxi(3,j) = 0
!            dN_deta(3,j)= 2*(eta-0.5) + 2*eta 
!          end do
!        
!        case DEFAULT
!          write(*,*) 'Invalid type of element.'
!      end select
!    end subroutine Shape


    !  ==============================



  !end contains
    
end module Isoparametric


