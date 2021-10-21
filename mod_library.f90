module library
  use Parameters
  use Isoparametric
  
  
  contains
   
    subroutine GeneralInfo( ) 
      print*, ' '
      print*, '- - - - 2D Cavity Driven Flow Simulation - - - - '
      print*, ' '
      print*,'!==================== GENERAL INFO ===============!'
      ! write(*,*)'= = = = = = = = = = = = = = = = = = = = = ='
      write(*,"(A30,8X,I6,1X,A11)") ' 1.- Dimension of the problem:    ', DimPr, '  |'
      write(*,"(A31,8X,A13,3X,A1)") ' 2.- Element type:                ', ElemType,'|'
      write(*,"(A30,8X,I6,1X,A11)") ' 3.- Total number of elements:    ', Nelem,'   |'
      write(*,"(A30,8X,I6,1X,A11)") ' 4.- Total velocity nodes:        ', n_nodes, '|'
      write(*,"(A30,8X,I6,1X,A11)") ' 5.- Total preasure nodes:        ', n_pnodes,'|'
      write(*,"(A30,8X,I6,1X,A11)") ' 6.- DoF per element:             ', Dof, '    |'
      write(*,"(A30,8X,I6,1X,A11)") ' 7.- Velocity nodes per element:  ', nUne, '   |'
      write(*,"(A30,8X,I6,1X,A11)") ' 8.- Preasure nodes per element:  ', nPne, '   |'
      write(*,"(A30,8X,I6,1X,A11)") ' 9.- Total of Gauss points:       ', totGp,'   |'
      ! write(*,*)'= = = = = = = = = = = = = = = = = = = = = ='    
      write(*,*)' '
      print*,'!============== FILE READING STATUS ============!'
     
    endsubroutine GeneralInfo

    subroutine ReadRealFile(UnitNum, FileName, NumRows, NumCols, Real_Array)
      implicit none

      integer :: i, j, status, UnitNum, NumRows, NumCols
      character (len=*), intent (in) :: FileName
      character(len=:), allocatable :: fileplace
      real, dimension (1:NumRows, 1:NumCols), intent (out) :: Real_Array
                  ! /home/maoliva/Codes/StokesFlow_Aca/Geo 
      fileplace = "~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Geo/"
      
      open (unit = UnitNum, file =fileplace//FileName, status='old', action='read' , iostat = status)
      
      ! read in values
      read(UnitNum,*) ((Real_Array(i,j), j=1,NumCols), i=1,NumRows)
      if (status.ne.0) then
        print *, "Status_Real_File ", status
      else
        continue
      end if
      
      close (UnitNum)
      
    end subroutine
    
    subroutine ReadIntegerFile(UnitNum, FileName, NumRows, NumCols, IntegerArray)
    
      integer :: i, j, status
      integer, intent(in)            :: UnitNum, NumRows, NumCols
      character(len=*), parameter    :: fileplace = "~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Geo/"
      character (len=*), intent (in) :: FileName
      integer, dimension (1:NumRows, 1:NumCols), intent (out) :: IntegerArray
      
      
      open (unit = UnitNum, file =fileplace//FileName, status='old', action='read' , iostat = status)
      
      ! read in values
      read(UnitNum,*) ((IntegerArray(i,j), j=1,NumCols), i=1,NumRows)
      if (status.ne.0) then
        print *, "Status_Int_File  ", status
      else
        continue
      end if
      ! print*, "Shape of ",FileName," is ", shape(IntegerArray)
      close (UnitNum)
      
    end subroutine ReadIntegerFile
    
    subroutine ReadReal(UnitNum, FileName, value)
      
      integer :: status, UnitNum
      character(len=*), parameter    :: fileplace = "~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Geo/"
      character (len=*), intent (in) :: FileName
      real :: value
      
      
      open (unit = UnitNum, file =fileplace//FileName, status='old', action='read' , iostat = status)
      
      ! read in values
      read(UnitNum,*) value
      if (status.ne.0) then
        print *, "Status_Single_Val", status
      else
        continue
      end if
      
      close (UnitNum)
      
    end subroutine
    
    subroutine ReadMixFile(UnitNum, FileName, NumRows, NumCols, Real_Array)
      implicit none
      
      ! - - - - - - - - - - * * * * * * * * * * - - - - - - - - - -
      ! Rutina que lee un conjunto de datos en el formato indicado
      ! en la etiqueta 22
      !- - - - - - - - - - * * * * * * * * * * - - - - - - - - - -
      
      integer :: i, j, status, UnitNum, NumRows, NumCols
      character(len=*), parameter    :: fileplace = "~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Geo/"
      character (len=*), intent (in) :: FileName
      real, dimension (1:NumRows, 1:NumCols), intent (out) :: Real_Array
      
      open (unit = UnitNum, file =fileplace//FileName, status='old', action='read' , iostat = status)
      
      ! read in values
      read(UnitNum,22) ((Real_Array(i,j), j=1,NumCols), i=1,NumRows)
      if (status.ne.0) then
        print *, "Status_Mix_File  ", status
      else
        continue
      end if
      
      22 format(3F13.10)
      close (UnitNum)
      
    end subroutine
    
    subroutine SetElementNodes(elm_num, element_nodes, node_id_map)
      ! subroutine SetElementNodes(elm_num, elements, nodes, element_nodes, node_id_map)
      implicit none
      
      ! integer, dimension(100,9),  intent(in)::  elements
      ! real, dimension(341,3), intent(in)    ::  nodes
      integer,intent(in)                       :: elm_num ! number of element for each elemental integral in do of K global
      real, dimension(nUne,DimPr), intent(out) :: element_nodes
      integer, dimension(nUne,1), intent(out)  :: node_id_map
      integer                                  :: i,j, global_node_id


      element_nodes = 0.0
      node_id_map = 0.0

      do i = 1, nUne
        global_node_id = elements(elm_num,i+1)
        do j=1 ,DimPr
          element_nodes(i,j) = nodes(global_node_id,j+1)
        end do
        node_id_map(i,1) = global_node_id
      end do

    end subroutine SetElementNodes

    subroutine PreassureElemNods(elm_num, pelement_nodes, pnode_id_map)
      ! subroutine PreassureElemNods(elm_num, pelements, nodes, pelement_nodes, pnode_id_map)
      implicit none

      ! integer, dimension(100,5),  intent(in)   ::  pelements
      ! real, dimension(341,3), intent(in)       ::  nodes
      integer,intent(in)                       :: elm_num ! number of element for each elemental integral in do of K global
      real, dimension(nPne,DimPr), intent(out) :: pelement_nodes
      integer, dimension(nPne,1), intent(out)  :: pnode_id_map
      integer                                  :: i,j, global_node_id


      pelement_nodes = 0.0
      pnode_id_map = 0.0

      do i = 1, nPne
        global_node_id = pelements(elm_num,i+1)
        do j=1 ,DimPr
          pelement_nodes(i,j) = nodes(global_node_id,j+1)
        end do
        pnode_id_map(i,1) = global_node_id
      end do

    end subroutine PreassureElemNods

    function J2D( element_nodes, dN_dxi, dN_deta, Gp)
      implicit none

      integer, intent(in)                      :: Gp !esta variable se usara en el lazo principal con el numero de punto de gauss para evaluar las integrales elementales
      real, dimension(nUne,DimPr), intent(in)  :: element_nodes
      double precision, dimension(nUne,totGp), intent(in) :: dN_dxi, dN_deta
      double precision, dimension(DimPr,nUne)  :: Basis2D
      double precision, dimension(1,nUne)      :: Nxi, Neta
      double precision, dimension(DimPr,DimPr) :: J2D

      !con estas instrucciones extraigo la columna de Nx como renglon y lo guardo en Nxi, Gp se
      !ira moviendo conforme la funcion J2D sea llamada en el lazo principal para cada elemento lo mismo para Neta con dN_deta
      Nxi  = spread(dN_dxi(:,Gp),dim = 1, ncopies= 1)
      Neta = spread(dN_deta(:,Gp),dim = 1, ncopies= 1)

      !Las siguientes tres lineas realizan de forma implicita el calculo de las derivadas
      !espaciales es decir dN/dx and dN/dy (eq. 5.114 - 5.117). Las derivadas espaciales 
      !no se calcula explicitamente, en su lugar se usa:
        
      !            d/dy = (d/deta)J^-1      (ver eq. 5.77 y 5.109)

      ! Esta forma de 
      Basis2D(1,:) = Nxi(1,:)
      Basis2D(2,:) = Neta(1,:)
      J2D = matmul(Basis2D,element_nodes) !Aqui se usan directamente las derivadas (eqs 5.114-5.117) de las coordenadas fisicas
                                          !respecto de las coordenadas del master element (isoparametric domain) para llenar la matriz Jacobiano.
      ! De la subroutina Quad4Nodes or Quad8Nodes  ya se tienen las derivadas de las funciones de forma respecto a las coordenadas del master element
      ! es decir dN/dxi and dN/deta contenidas en Basis 2D. Luego, se multiplican por element_nodes para completar el Jacobiano.
      

      ! - - - * * * D U D A * * * - - -
        !Si el nombre de la funcion es el mismo que la variable donde se guarda, entonces no puedo declararla como
        ! variable global, ¿Como debo hacerlo?

        !Si lo dejo como

        !J = Matmul(Basis2D,element_nodes)

        !Me marca un warning
      ! - - - * * * D U D A * * * - - -

      return
    end function J2D

    function JP2D( element_nodes, dN_dxi, dN_deta, Gp)
          implicit none
    
          integer, intent(in)                      :: Gp !esta variable se usara en el lazo principal con el numero de punto de gauss para evaluar las integrales elementales
          real, dimension(nPne,DimPr), intent(in)  :: element_nodes
          double precision, dimension(nPne,totGp), intent(in) :: dN_dxi, dN_deta
          double precision, dimension(DimPr,nPne)  :: Basis2D
          double precision, dimension(1,nPne)      :: Nxi, Neta
          double precision, dimension(DimPr,DimPr) :: JP2D
    
          !con estas instrucciones extraigo la columna de Nx como renglon y lo guardo en Nxi, Gp se
          !ira moviendo conforme la funcion J2D sea llamada en el lazo principal para cada elemento lo mismo para Neta con dN_deta
          Nxi  = spread(dN_dxi(:,Gp),dim = 1, ncopies= 1)
          Neta = spread(dN_deta(:,Gp),dim = 1, ncopies= 1)
    
          !Las siguientes tres lineas realizan de forma implicita el calculo de las derivadas
          !espaciales es decir dN/dx and dN/dy (eq. 5.114 - 5.117). Las derivadas espaciales 
          !no se calcula explicitamente, en su lugar se usa:
            
          !            d/dy = (d/deta)J^-1      (ver eq. 5.77 y 5.109)
    
          ! Esta forma de 
          Basis2D(1,:) = Nxi(1,:)
          Basis2D(2,:) = Neta(1,:)
          JP2D = matmul(Basis2D,element_nodes) !Aqui se usan directamente las derivadas (eqs 5.114-5.117) de las coordenadas fisicas
                                              !respecto de las coordenadas del master element (isoparametric domain) para llenar la matriz Jacobiano.
          ! De la subroutina Quad4Nodes or Quad8Nodes  ya se tienen las derivadas de las funciones de forma respecto a las coordenadas del master element
          ! es decir dN/dxi and dN/deta contenidas en Basis 2D. Luego, se multiplican por element_nodes para completar el Jacobiano.
          
    
          ! - - - * * * D U D A * * * - - -
            !Si el nombre de la funcion es el mismo que la variable donde se guarda, entonces no puedo declararla como
            ! variable global, ¿Como debo hacerlo?
    
            !Si lo dejo como
    
            !J = Matmul(Basis2D,element_nodes)
    
            !Me marca un warning
          ! - - - * * * D U D A * * * - - -
    
          return
        end function JP2D


    function inv2x2(A)

      implicit none

      double precision, dimension(DimPr, DimPr), intent(in) :: A
      double precision, dimension(DimPr, DimPr)             :: inv2x2
      double precision, dimension(DimPr,DimPr)              :: cofactor
      double precision, parameter :: EPS = 1.0E-10
      double precision            :: det
      


      det =   A(1,1)*A(2,2) - A(1,2)*A(2,1)

      if (abs(det) .le. EPS) then
        inv2x2 = 0.0D0
        return
      end if

      cofactor(1,1) = +A(2,2)
      cofactor(1,2) = -A(2,1)
      cofactor(2,1) = -A(1,2)
      cofactor(2,2) = +A(1,1)

      inv2x2 = transpose(cofactor) / det

      return

    end function inv2x2

    function buildJb(A)
      !Funcion que construye una matriz de 4 x 4 en bloques de 2 para el caso 2D

      implicit none

      double precision, dimension(DimPr,DimPr), intent (in)   :: A
      double precision, dimension(2*DimPr, 2*DimPr)           :: buildJb

      buildJb(1:2,1:2) = A
      buildJb(3:4,3:4) = A

    end function

    function m22det(A)

      implicit none
      double precision :: m22det
      double precision, dimension(2,2), intent(in)  :: A



      m22det =   A(1,1)*A(2,2) - A(1,2)*A(2,1)

      return

    end function m22det

    function CompH()
      implicit None

      ! integer :: CompH
      ! integer, dimension(3,4) :: H
      integer, dimension(Dof ,2*DimPr) :: CompH
      CompH = 0
      CompH(1,1)=1;
      CompH(2,4)=1;
      CompH(3,2)=1;
      CompH(3,3)=1;

      ! CompH = H
      ! - - - * * * D U D A * * *
        !no puedo colocar direwctamente el nombre d ela funcion (la funcion misma) como variable global y debo pasarselo a otra variable y esa si ponerla como
        !vbariable global por eso hago el cambio de CompH = H y H esta como variable global. Es Asi?
      ! - - - * * * D U D A * * *

      return

    end function CompH

    function compBmat(dN_dxi, dN_deta, Gp)

      implicit none

      double precision, dimension(nUne,size(gauss_points)), intent(in) :: dN_dxi, dN_deta
      integer, intent(in) :: Gp

      double precision, dimension(2*DimPr, DimPr*nUne) :: compBmat
      double precision, dimension(1, nUne)             :: Nxi, Neta
      integer ::  i

      compBmat = 0.0
      Nxi  = spread(dN_dxi(:,Gp),dim = 1, ncopies= 1)
      Neta = spread(dN_deta(:,Gp),dim = 1, ncopies= 1)


      do i=1, nUne
        compBmat(1,2*i-1)= Nxi(1,i)
        compBmat(3,2*i)  = Nxi(1,i)
        compBmat(2,2*i-1)= Neta(1,i)
        compBmat(4,2*i)  = Neta(1,i)
      end do

      ! compBmat = B
      return
      ! - - - * * * D U D A * * * - - -
        !En matlab basta con  Nxi(i) aqui quneuq es posible indicar un vector solo con una dimension, no sirve para multiplicarlo.
        !Siempre se debe indicar matriz como un vector fila o vector columna?
      ! - - - * * * D U D A * * * - - -

    end function compBmat

    function BPmat(dNp_dxi, dNp_deta, Gp)                                               
      !Computation of the preassure Strain-Displacement Matrix
    
      implicit none                                                                                      
    
      integer, intent(in) :: Gp                                              
      double precision, dimension(nPne,size(gauss_points)), intent(in) :: dNp_dxi, dNp_deta
      double precision, dimension(1*DimPr, 1*nPne) :: Bpmat ! 1 grado de libertad por nodo para los elementos de presion
      double precision, dimension(1, nPne)         :: Npxi, Npeta
      integer :: i                                                                                                   
    
      Bpmat = 0.0                                                                                                                        
      Npxi  = spread(dNp_dxi(:,Gp),dim = 1, ncopies= 1)        
      Npeta = spread(dNp_deta(:,Gp),dim = 1, ncopies= 1)     
    
      !Npxi  = Nx(:,Gp)                                                                         
      !Npeta = Ny(:,Gp)                                                              
    
      do i=1, nPne                                                                                      
        Bpmat(1,i) = Npxi(1,i)                                  
        Bpmat(2,i) = Npeta(1,i)                                 
      end do                                                    
    
      return                                                    
    
    end function BPmat  

    subroutine AssembleK(K, ke, node_id_map, ndDOF)

      implicit none
      real(8), dimension(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes),intent(in out)  :: K !Global Stiffnes matrix debe 
      !                                                                               llevar inout por que entra como variable (IN) 
      !                                                                                pero en esta funcion se modifica (out)
      real(8), dimension(2*nUne, 2*nUne), intent(in)   :: ke
      integer, dimension(nUne,1), intent(in)           :: node_id_map
      integer, intent(in)                              :: ndDOF 
      integer :: i, j, row_node, row, col_node, col !nodal Degrees of Freedom
      
      do i = 1, nUne
        row_node = node_id_map(i,1)
        row = ndDOF*row_node - (ndDOF-1)
        
        do j = 1, nUne
          col_node = node_id_map(j,1)
          col = ndDOF*col_node - (ndDOF-1)
          K(row:row+ndDOF-1, col:col+ndDOF-1) =  K(row:row+ndDOF-1, col:col+ndDOF-1) + &
          ke((i-1)*ndDOF+1:i*ndDOF,(j-1)*ndDOF+1:j*ndDOF)
        enddo
        
      enddo
      
      return
      
    end subroutine AssembleK
    
    
    subroutine AssemblyStab(ke, node_id_map, K)
      
      implicit none
      double precision, dimension(n_pnodes,n_pnodes), intent(in out)  :: K !Global Stiffnes matrix debe 
      !                                                           llevar inout por que entra como variable (IN) 
      !                                                            pero en esta funcion se modifica (out)
      double precision, dimension(nPne, nPne), intent(in) :: ke
      integer, dimension(nPne,1), intent(in)              :: node_id_map
      integer :: i, j, row_node, row, col_node, col, pnode_id !nodal Degrees of Freedom
      
      !K 
      
      do i = 1, nPne
        row_node = node_id_map(i,1)
        pnode_id = pnodes(row_node,2)
        row = pnode_id !ndDOF*col_node - (ndDOF-1)
        
        do j = 1, nPne
          col_node = node_id_map(j,1)
          pnode_id =  pnodes(col_node,2)
          col = pnode_id !ndDOF*col_node - (ndDOF-1)
          
          K(row,col) =  K(row , col) + ke(i,j)
        enddo
        
      enddo
      
      
      return
      
    end subroutine AssemblyStab
    
    
    subroutine GlobalK( A_K, dN_dxi, dN_deta) !Al tener un solo parametro de salida puedo declararla como funcion
      
      implicit none
      
      double precision, dimension(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes), intent(out) :: A_K  !Global Stiffnes matrix
      double precision, dimension(nUne,TotGp), intent(in) :: dN_dxi, dN_deta
      double precision, allocatable, dimension(:,:)     :: dNp_dxi, dNp_deta
      double precision, allocatable, dimension(:,:)     :: Np
      double precision, dimension(2*nUne, 2*nUne)       :: ke
      double precision, dimension(2*nUne,nPne)          :: kep
      double precision, dimension(nPne, nPne)           :: Stab !Will be assembled into K22 
      double precision, dimension(DimPr, DimPr)         :: Jaco, Jinv, JinvP, JacoP
      double precision                                  :: detJ, detJP
      double precision, dimension(2*DimPr, 2*DimPr)     :: Jb ! aqui tmb es Dof no DimPr pero 2 para vel y dos para P
      double precision, dimension(2*DimPr, DimPr*nUne)  :: B  !no es DimPr es Dof del elemento en cuestion, en este caso deb
      double precision, dimension(DimPr*1, 1*nPne)      :: nabP !2 para la matriz elemental de velocidad y uno para la matriz
      double precision, dimension(DimPr*1, 1*nPne)      :: JnabP !2 para la matriz elemental de velocidad y uno para la matriz
      double precision, dimension(1*nPne, DimPr*1)      :: JP_T !2 para la matriz elemental de velocidad y uno para la matriz
      double precision, dimension(nPne, nPne)           :: nabTPnabP
      double precision, dimension(Dof,DimPr*DimPr)      :: HJ
      double precision, dimension(Dof,2*nUne)           :: HJB
      double precision, dimension(2*nUne,Dof)           :: HJB_T !Todos estos dos, hablan de los DoF de la velocidad 
      double precision, dimension(2*nUne,Dof)           :: part1 !Todos estos dos, hablan de los DoF de la velocidad
      double precision, dimension(2*nUne,2*nUne)        :: part2 !Todos estos dos, hablan de los DoF de la velocidad
      double precision, dimension(2*nUne,2*nUne)        :: part3 !Todos estos dos, hablan de los DoF de la velocidad
      double precision, dimension(nUne,DimPr)           :: part4
      double precision, dimension(DimPr,1)              :: part5
      double precision, dimension(nPne,1)               :: part6
      double precision, dimension(1,nPne)               :: part7
      double precision, dimension(2*nUne,nPne)          :: part8
      double precision, dimension(DimPr,1)              :: A
      double precision, dimension(2*nUne,1)             :: dn
      real, dimension(Dof,2*DimPr)                      :: H
      real, dimension(Dof,Dof)                          :: cc, C   !Derived from elasticity formulation as Matertial matrix of Hook's Law
      double precision, allocatable, dimension(:,:)     :: K12, K12_T, K22!Lo puse allocatable por que marca error en la memoria 
      ! Array 'k12' at (1) is larger than limit set by '-fmax-stack-var-size=', moved from stack to static storage. This makes the procedure unsafe when called recursively, 
      !or concurrently from multiple threads. Consider using '-frecursive', or increase the '-fmax-stack-var-size=' limit, or change the code to use an ALLOCATABLE array. [-Wsurprising]
      real, dimension(nUne,DimPr)   :: element_nodes
      real, dimension(nPne,DimPr)   :: pelement_nodes
      integer, dimension(nUne,1)    :: node_id_map
      integer, dimension(nPne,1)    :: pnode_id_map
      integer                       :: gp, e, i,j, row_node, row, rowstab, colstab!, kk,l, mrow, ncol,
      integer                       :: col_node, pnode_id, col, dimAK, symmetric!, mrow, ncol
      double precision              :: Tau, Tauu
      A_K  = 0.0
      cc = reshape([2, 0, 0, 0, 2, 0, 0, 0, 1],[Dof,Dof])
      C  = materials * cc
      H  = CompH()
      
      !Setup for K11 block or Kuu
      do e = 1, Nelem    !elements loop for K11 block Global K
        ke = 0
        Jb = 0
        call SetElementNodes(e, element_nodes, node_id_map)
        !do-loop: compute element (velocity-velocity) stiffness matrix ke
        do gp  = 1, TotGp
          Jaco = J2D(element_nodes, dN_dxi, dN_deta, gp)
          detJ = m22det(Jaco)
          Jinv = inv2x2(Jaco)
          Jb   = buildJb (Jinv)
          B    = compBmat( dN_dxi, dN_deta, gp)
          HJ   = matmul(H,Jb)
          HJB  = matmul(HJ,B)
         HJB_T = transpose(HJB)
         part1 = matmul(HJB_T,C)
         part2 = matmul(part1,HJB)
         part3 = part2 * detJ
         !aqui marcaba error por que gauss_weights es un vector columna y debe indicarse con dos indices
         ke = ke + part3 * gauss_weights(gp,1)  !
        end do
        
        call AssembleK(A_K, ke, node_id_map, 2) ! assemble global K
        
      end do
      
      !Setup for K12 block or KuP
      allocate (K12(n_nodes*2,n_pnodes),K12_T(n_pnodes,n_nodes*2))
      allocate (K22(n_pnodes,n_pnodes))
      K12  = 0.0
      K22  = 0.0
      
      Tau = (0.5**2 / 4.0 * materials)
      print"(A5,f10.5)",' 𝜏= ', Tau
      print*, ' '
      call ShapeFunctions(gauss_points, nPne, Np, dNp_dxi, dNp_deta)
      !for-loop: compute K12 block of K
      do e = 1, Nelem
        kep = 0.0
        Stab = 0.0
        call SetElementNodes(e, element_nodes,  node_id_map)
        call PreassureElemNods(e, pelement_nodes, pnode_id_map) !--Arreglar esto para que sea con p en todos los arguments
        ! for-loop: compute element stiffness matrix kup_e
        do gp   = 1, TotGp
          Jaco  = J2D(element_nodes, dN_dxi, dN_deta, gp)
          JacoP = JP2D(pelement_nodes, dNp_dxi, dNp_deta, gp)
          detJ  = m22det(Jaco)
          detJP = m22det(JacoP)
          Jinv  = inv2x2(Jaco)
          JinvP = inv2x2(JacoP)
          nabP  = Bpmat(dNp_dxi, dNp_deta, gp)  !∇P 
          JnabP = matmul(JinvP,nabP) !J^-1 * ∇P 
          JP_T  = transpose(JnabP)   !(J^-1 * ∇P)^T
          dn    = 0.0
          do j  = 1, nUne
            part4(j,:) = [ dN_dxi(j,gp), dN_deta(j,gp) ]  
            part5 = reshape([part4(j,:)],[2,1])           !--Revisar por que en la linea 514 y 515 si se puede hacer
            A =  matmul(Jinv,part5)           !Separe multiplicaciones para que funcione 
            dN(2*j-1:2*j ,1)= A(:,1)          !quiza dividri operacion de este matmul
          end do
          part6(:,1) = Np(:,gp)
          part7 = transpose(part6)
          part8 = matmul(dn,part7)
          nabTPnabP = matmul(JP_T,JnabP) !∇'δP · ∇P 
          kep  = kep + part8 * (detJ*gauss_weights(gp,1)) 
          Tauu = Tau
          Stab = Stab + Tau * nabTPnabP * detJP * gauss_weights(gp,1) ! ∫ (∇'δP : ∇P) dΩ  
        end do  
        
        ! for-loop: assemble ke into global KP (it mean K12)
        do i = 1, nUne
          row_node = node_id_map(i,1)
          row = 2*row_node - 1
          do j = 1, nPne
            col_node = pnode_id_map(j,1)
            pnode_id = pnodes(col_node,2)
            col = pnode_id !Aqui puedo sustituir directamente y evito esta misma linea
            K12(row:row+1, col) = K12(row:row+1, col) + kep(2*i-1:i*2, j)
          end do
        end do 
        
        ! for-loop: assemble stab into global K22
        !if(nUne .EQ. nPne)then
         call AssemblyStab(Stab, pnode_id_map, K22) ! assemble global K
        !else
         !continue
        !end if
        
      end do
      
      dimAK = size(A_K,1)
      symmetric = dimAK - n_pnodes

      do i = 1, 2*n_nodes
        do j = 2*n_nodes+1, (2*n_nodes+n_pnodes)
          A_K(i, j) = -K12(i,j-symmetric)
        end do
      end do
      !========== Lower
      K12_T = transpose(K12)
      do i = 2*n_nodes+1, (2*n_nodes+n_pnodes)
        do j = 1, 2*n_nodes 
          A_K(i, j) = -K12_T(i-symmetric,j)
        end do
      end do
      !========== Filling the stabilization global matrix into A_K ==========
      
      !if(nUne .EQ. nPne)then
        RowStab = dimAK - n_pnodes 
        ColStab = dimAK - n_pnodes 
        do i = RowStab, 2*n_nodes+n_pnodes -1
          do j = ColStab, 2*n_nodes+n_pnodes-1
            A_K(i, j) = -K22( (i+1)-RowStab,(j+1)-ColStab )
          end do
        end do
      !else
      !  continue
      !end if
      
      !      do i = RowStab, 2*n_nodes+n_pnodes
      !        do j = ColStab, 2*n_nodes+n_pnodes
      !          do kk = 1, n_pnodes
      !            do l =1, n_pnodes
      !              !A_K(i, j) = K22((i+1)-RowStab,(j+1)-ColStab)
      !              A_K(i, j) = K22(kk,l)
      !            end do
      !          end do
      !        end do
      !      end do
      
      DEALLOCATE(K12)
      DEALLOCATE(K12_T)
      DEALLOCATE(Np)
      DEALLOCATE(dNp_dxi)
      DEALLOCATE(dNp_deta)
      DEALLOCATE(K22)
    end subroutine GlobalK
    
    
    
    subroutine SetBounCond( NoBV, NoBVcol )
      !========================================================================
      !Esta subroutina revisa todos los nodos de la malla y define el tipo de
      !nodo en la frontera. Abre un archivo en donde comenzara a escribir, 
      ! en la primer columna: el numero de nodo. 
      ! La segunda columna tendra el tipo de nodo
      ! 1 = ux (componente x de la velocidad) 
      ! 2 = uy (componente y de la velocidad) 
      ! 3 = para la presion 
      !La tercera columna asigna el valor correspondiente de la condicion de forntera
      !=========================================================================
      implicit none
                                                     !"/home/maoliva/Codes/StokesFlow_Aca/Geo/"
      character(len=*), parameter :: fileplace ="~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Geo/"
      integer, intent(out) :: NoBV, NoBVcol
      integer :: ierror, a ,b, c, i
      real    :: x, y, xmin, xmax, ymin, ymax, xhalf
      
      ! call ReadRealFile(10,"nodes.dat", 341,3, nodes)
      ! inicializamos los contadores
      ! Los contadores son para que cada vez
      ! que un if se cumpla, se sume el numero
      ! equivalente a los renglones escritos en
      ! el archivo de texto que se esta creando
      ! y asi se tenga el numero total de nodos en
      ! las fronteras
      a = 0
      b = 0
      c = 0 
      
      xmin = minval(nodes(:,2)) !the smallest number in y column
      xmax = maxval(nodes(:,2)) !the smallest number in y column
      ymin = minval(nodes(:,3)) !the smallest number in y column
      ymax = maxval(nodes(:,3)) !the smallest number in y column
      xhalf = xmax/2.0


      print*, ' '
      print*, 'xmin= ', xmin
      print*, 'xmax= ', xmax
      print*, 'ymin= ', ymin
      print*, 'ymax= ', ymax
      print*, 'xhalf= ', xhalf
      print*, ' '


      open(unit=100, file=fileplace//'Fbcsvp.dat',Status= 'replace', action= 'write',iostat=ierror)
      NoBVcol = size(nodes,2)     
     
      do i =1, n_nodes
        x=nodes(i,2)
        y=nodes(i,3)
        if(y.eq.ymax) then !top edge: velocity boundary condition
          write(100,50) i, 1, real(1)
          write(100,50) i, 2, real(0)
          a=a+2
          if(x.eq.xhalf)then !center zero pressure
            write(100,50) i,3, real(0)
            b=b+1
          end if
          
        else if (x.eq.xmin .or. y.eq.ymin .or. x.eq.xmax)then !The other 3 edges
          write(100,50) i, 1, real(0) !x-velocity
          write(100,50) i, 2, real(0) !y-velocity
          c=c+2
        end if
        NoBV = a+b+c
      end do
      
      close(100)
      
      !50 format(3F20.10)
      50 format(2I6,f10.3)
      
      
    end subroutine SetBounCond  
    

    subroutine ApplyBoundCond( NoBV, Fbcsvp, A_K, Sv )
      ! - - - - - - - - - - * * * * * * * * * * - - - - - - - 
      ! Set velocity (u) and preasure (p) boundary condition by penalty method
      ! - - - - - - - - - - * * * * * * * * * * - - - - - - - - - -
      implicit none
                          !Dof
      integer , dimension(NoBV,Dof), intent(in) :: Fbcsvp
      double precision, dimension(2*n_nodes+n_pnodes, 2*n_nodes+n_pnodes),intent(in out) :: A_K  !Global Stiffnes matrix
      double precision, dimension(2*n_nodes+n_pnodes, 1), intent(in out) :: Sv
      double precision :: param, coeff
      integer          :: preasure_row, NoBV, i, component, node_id, pnode_id
      
      !Esencialmente la siguiente instruccion hace: A_K(1*2-1,:) = A_K(1,:) Es decir, obtene el valor maximo de
      !la primera fila de la matriz global K (A_K). No le veo el caso pero lo dejamos asi.
      param = maxval(A_K(int(Fbcsvp(1,1))*2-1,:))
      coeff = abs(param) * 1.0E7

      print*, 'param', param
      print*, 'coeff', coeff
      
      preasure_row = 2*n_nodes

      do i =1, NoBV
        !print*, 'iteration', i
        node_id   = Fbcsvp(i,1) !se pone este int() pq la 1a y 2a col de Fbcsvp esta leida como integer pero 
        !print*, 'node_id', node_id
        component = Fbcsvp(i,2)!la matriz completa esta declarada como real en esta subroutina y en el main.
        !print*, 'component', component
        !print*, shape(Fbcsvp)
        !print*, Fbcsvp(i,:)
        if ( component .le. 2 ) then
          !print*, 'component of Boundary value', component
          !print*,'La pausa', 2*node_id-2+component,' ', 2*node_id-2 +component
          !read(*,*)
          A_K(2*node_id-2+component, 2*node_id-2 +component) = coeff
          Sv( 2*node_id-2+component, 1) = Fbcsvp(i,3)*coeff 
        else                                                     
          pnode_id = pnodes(node_id,2)
          !print*, 'pnode_id', pnode_id 
          !print*, 'preasure_row', preasure_row
          A_K(preasure_row+pnode_id, preasure_row + pnode_id) = coeff
          !print*, preasure_row+pnode_id, preasure_row + pnode_id
          Sv(preasure_row+pnode_id,1) = Fbcsvp(i,3)*coeff !el tres es por que en la columna 3 esta el valor de la condicon de forntera
        end if
      end do

    end subroutine ApplyBoundCond
  
    subroutine MKLfactoResult( value )
      implicit none

      integer :: value, val
      character(len=34) :: text
      character(len=48) :: text2

      text  = '   *FACTORIZATION DONE WITH STATUS'
      text2 = '   *THE FACTORIZATION HAS BEEN COMPLETED, BUT U('
      if ( value .eq. 0 ) then
        write(*, 101) text, value, ', THE EXECUTION IS SUCCESSFUL.'
      elseif(value .lt. 0 )then
        val = abs(value)
        write(*, 102) '    THE',val,'-TH PARAMETER HAD AN ILLEGAL VALUE.'
      elseif(value .gt. 0 )then
        write(*, 103) text2, value,',',value,') IS EXACTLY SINGULAR.'
        print*,'   DIVISION BY 0 WILL OCCUR IF YOU USE THE FACTOR U FOR SOLVING A SYSTEM'
        print*,'   OF LINEAR EQUATIONS.'
      endif
      print*, ' '

      101 format (A, 1x, I1, A)
      102 format (A, I4, A)
      103 format (A, I3, A, I3, A)
    
    end subroutine MKLfactoResult
    
    subroutine MKLsolverResult( value )
      implicit none
      
      integer :: value, val
      character(len=30) :: text
      character(len=35) :: text2
      text =  '   *SYSTEM SOLVED WITH STATUS'
      text2 = '-TH PARAMETER HAD AN ILLEGAL VALUE.'
      
      if ( value .eq. 0 ) then
        write(*,101) text, value, ', THE EXECUTION IS SUCCESSFUL.'
      elseif(value .lt. 0 )then
        val = abs(value)
        write(*,102) '    THE',val, text2
      endif
      print*,' '
      
      101 format (A, 1x, I1, A)
      102 format (A, I3, A)
      
    end subroutine MKLsolverResult
    
    subroutine writeMatrix(Matrix, unit1, name1, Vector, unit2, name2)
      implicit none
                                                    !"/home/maoliva/Codes/StokesFlow_Aca/Res/" 
      character(len=*), parameter    :: fileplace = "~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Res/"
      character(*) :: name1, name2
      integer :: i, j, mrow, ncol, unit1, unit2
      double precision, dimension(2*n_nodes+n_pnodes ,2*n_nodes+n_pnodes ), intent(in) :: Matrix
      double precision, dimension(2*n_nodes+n_pnodes ,1), intent(in) :: Vector
      
      100 format (900E20.12)
      
      mrow = 2*n_nodes+n_pnodes 
      ncol = 2*n_nodes+n_pnodes
      open(unit=unit1, file= fileplace//name1, ACTION="write", STATUS="replace")

      do i=1,2*n_nodes+n_pnodes 
        write(unit1, 100)( Matrix(i,j) ,j=1,2*n_nodes+n_pnodes)
      end do
      close(unit1)
    
      open(unit=unit2, file= fileplace//name2, ACTION="write", STATUS="replace")
      do i=1,2*n_nodes+n_pnodes 
        write(unit2, 100) Vector(i,1)
      end do
      close(unit2)
    
    end subroutine writeMatrix
    
    subroutine PosProcess(solution, nameFile1, activity)
      
      implicit none
                                                      !"/home/maoliva/Codes/StokesFlow_Aca/Pos/"  
      character(len=*), parameter    :: fileplace = "~/Dropbox/1.Doctorado/1.Research/Computing/Fortran/StokesFlow/Pos/"
      real*8, dimension(2*n_nodes+n_pnodes, 1), intent(in) :: solution
      character(*), intent(in)                             :: nameFile1, activity
      double precision, dimension(1, 2*n_nodes+n_pnodes)   :: solution_T
      double precision, dimension(1,n_nodes) :: xcor, ycor
      integer      :: ipoin,  prow, pnode_id
      
      solution_T = transpose(solution)
      xcor  = spread(nodes(:,2),dim = 1, ncopies= 1)
      ycor  = spread(nodes(:,3),dim = 1, ncopies= 1)
      
      prow=2*n_nodes
      
      open(unit=555, file= fileplace//nameFile1, ACTION="write", STATUS="replace")
      
      if(activity == "msh")then !quitar este if y acomodar el numero de unidad
        
        write(555,902) 'MESH', '"Cavity"', 'dimension', DimPr, 'ElemType', ElemType, 'Nnode', nUne
        write(555,"(A)") '#2D Cavity Driven Flow Results' 
        write(555,900) '#Element tipe: ', ElemType,'/',ElemType 
        write(555,"(A)")'Coordinates'
        write(555,"(A)") '#   No        X           Y'
        do ipoin = 1, n_nodes
          write(555,906) ipoin, xcor(1,ipoin), ycor(1,ipoin)
        end do
        write(555,"(A)") 'End Coordinates'
        write(555,"(A)") 'Elements'
        do ipoin = 1, Nelem
          write(555,908) elements(ipoin,:) 
        end do
        write(555,"(A)") 'End Elements'
        close(555)
        
      elseif(activity == "res")then
        write(555,"(A)") 'GiD Post Results File 1.0'
        write(555,"(A)") '#2D Cavity Driven Flow Results' 
        write(555,900) '#Element tipe: ', ElemType,'/',ElemType 
        write(555,"(A)") 'Result "Velocity Components" "Velocity" 0 Vector OnNodes'
        write(555,"(A)") 'ComponentNames "U_X" "U_Y" "U_Z" "" '
        write(555,"(A)") 'Values'
        ! se escribe el res de las componentes de la velocidad
        write(555,910) 
        do ipoin = 1, n_nodes
          write(555,912) ipoin, solution_T(1, 2*ipoin-1), solution_T(1,2*ipoin)
        end do
        write(555,"(A)") 'End Values'
        write(555,"(A)") 'Result "Pressure" "Pressure" 0 Scalar OnNodes'
        write(555,"(A)") 'ComponentNames "" '
        write(555,"(A)") 'Values'
        ! se escribe el res de la presion 
        write(555,914)
        do ipoin = 1, n_nodes
          pnode_id = pnodes(ipoin,2)
          write(555,916) ipoin, solution(prow+pnode_id, 1)  
        end do
        write(555,"(A)") 'End Values'
        close(555)
      else
        write(*,"(A)") ' "Activity" must be "msh" or "res" '
        close(555)
        stop
      end if
      
     ! if (nUne .NE. nPne) then
     !   write(550,902) 'MESH', '"Cavity"', 'dimension', DimPr, 'ElemType', ElemType, 'Nnode', nPne
     !   write(550,"(A)") '#2D Cavity Driven Flow Results' 
     !   write(550,900) '#Element tipe: ', ElemType,'/',ElemType 
     !   write(550,"(A)")'Coordinates'
     !   write(550,"(A)") '#   No        X           Y'
     !   do e =1, Nelem
     !     xypnode = pnodes(e,2)
     !       if(xypnode .NE. 0)then
     !         write(550,906) e, nodes(e,:)
     !       else
     !         continue
     !       end if
     !   end do
     !   write(550,"(A)") 'End Coordinates'
     !         write(550,"(A)") 'Elements'
     !         do ipoin = 1, Nelem
     !           write(550,908) pelements(ipoin,:) 
     !         end do
     !         write(550,"(A)") 'End Elements'
     !        close(550)
     !   write(550,"(A)") 'Result "Preassure" "Preassure" 0 Scalar OnNodes'
     !   write(550,"(A)") 'ComponentNames "" '
     !   write(550,"(A)") 'Values'
     !   ! se escribe el res de la presion 
     !   write(550,914)
     !   do ipoin = 1, n_nodes
     !     pnode_id = pnodes(ipoin,2)
     !     write(550,916) ipoin, solution(prow+pnode_id, 1)  
     !   end do
     !   write(550,"(A)") 'End Values'
     !   close(550)
     ! else
     !   continue
     ! end if
      
      900 format(A15, A13, A1, A13)
      902 format(A4,1x,A8,1X,A9,1X,I1,1X,A8,1X,A13,A6,1X,I1)
      906 format(I7,2(3x,f9.4)) !format for msh           
      908 format(9(2x,I7) )
      910 format('#',3x,'No    ' 3x, ' Ux ', 8x, ' Uy')
      912 format(I7,2x,2f12.5) !format for res velocity
      914 format('#',3x,'No'     9x, 'P')
      916 format(I7,2x,f12.5)  !format for res preassure
      
    end subroutine PosProcess
    
   
    
    !Fin de contains






end module library
