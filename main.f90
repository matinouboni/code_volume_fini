program chaleur

    use mod_maillage
    use mod_sortie
    use mod_precision

    implicit none

    !--- entree
    character(len=30) :: fichier_maillage

    !--- sorties
    integer :: nb_mailles, nb_aretes
    real(pr), dimension(:), allocatable :: aire_maille, d_arete, l_arete
    real(pr), dimension(:,:), allocatable :: coord_noeud, milieu_arete
    integer, dimension(:,:), allocatable :: arete_maille, noeud_maille, maille_arete
    !-- format msh    character(len=50), dimension(:), allocatable, intent(out) :: cl_arete
    !-- format medit
    integer, dimension(:), allocatable :: cl_arete

    !cas test 1 : parametre du pb

    real(pr) :: T_max =1
    real(pr) :: D = 1
    !real(pr) :: cfl=1
    real(pr) :: T_G=100
    real(pr) :: T_D=300
    real(pr) :: T_init=100
    integer  :: Nbr_iter
    integer  :: n

    !allocation et initialisation des tableaux T et Tnp1
    real(pr), dimension(:), allocatable :: T, Tnp1


    !pas de temps 
    real(pr) :: dt
    !variable temporaire dans le calcul de dt
    real(pr) :: temp
    integer :: i
    integer :: j
    !variable Fe
    real(pr) :: Fe
    real(pr) erreur1d

    open ( unit=10, file = 'cas_1d.mesh' )

    fichier_maillage='cas_1d.mesh'
    

    call maillage(fichier_maillage, nb_mailles, nb_aretes                    &
        &         , coord_noeud, noeud_maille, aire_maille, l_arete, d_arete &
        &         , milieu_arete, arete_maille, maille_arete, cl_arete)

    ! Fermeture du fichier
        close(10)

    ! initialisation de T
        allocate(T(nb_mailles),Tnp1(nb_mailles))

    do i=1,nb_mailles
        T(i)=T_init
    end do
    

    !calcul du pas de temps delta_t
    dt=0
    do i=1,3
        dt=dt+(l_arete(arete_maille(1,i))*D)/d_arete(arete_maille(1,i))
    end do
    dt=aire_maille(1)/dt

    do i=2,nb_mailles
        temp=0
        do j=1,3
            temp=temp+(l_arete(arete_maille(i,j))*D)/d_arete(arete_maille(i,j))
        end do
        temp=aire_maille(i)/temp
        if(temp < dt) then
            dt=temp
        end if
    end do

    print*,'delta=',dt

    Nbr_iter= INT(T_max/dt)

    print*,'Nombre d_iteration en temps', Nbr_iter

 

    Tnp1=T
    ! construction du schema 

    do n=1,Nbr_iter
        do i=1,nb_aretes
            if(maille_arete(i,2) .NE. 0) then
                Fe = (T(maille_arete(i,1))-T(maille_arete(i,2)))/d_arete(i)
                Tnp1(maille_arete(i,1))=Tnp1(maille_arete(i,1))-dt*(l_arete(i)*D*Fe)/aire_maille(maille_arete(i,1))
                Tnp1(maille_arete(i,2))=Tnp1(maille_arete(i,2))+dt*(l_arete(i)*D*Fe)/aire_maille(maille_arete(i,2))
            elseif(cl_arete(i)==10) then 
             Fe=(T_G-T(maille_arete(i,1)))/d_arete(i)
                 Tnp1(maille_arete(i,1))=Tnp1(maille_arete(i,1))+dt*(l_arete(i)*D*Fe)/aire_maille(maille_arete(i,1))
            elseif(cl_arete(i)==11) then 
                Fe=(T_D-T(maille_arete(i,1)))/d_arete(i)
                 Tnp1(maille_arete(i,1))=Tnp1(maille_arete(i,1))+dt*(l_arete(i)*D*Fe)/aire_maille(maille_arete(i,1))
            elseif(cl_arete(i)==20) then 
                Fe=0
                Tnp1(maille_arete(i,1))=Tnp1(maille_arete(i,1))+dt*(l_arete(i)*D*Fe)/aire_maille(maille_arete(i,1))
            end if
        end do
        T=Tnp1
        if(MOD(n,10) == 0) then 
            call sortie(n,T, coord_noeud, noeud_maille)
        end if 
    end do



    erreur1d=0
    do i=1,nb_aretes
        if(maille_arete(i,2)==0)then 
            erreur1d=erreur1d+abs(T(maille_arete(i,1))-T_exacte(T_G,T_D,milieu_arete(i,1),Nbr_iter*dt,D))
        end if
        
    end do

    print*,"erreur1d=",dt*erreur1d





    print*,""


    contains

    function T_exacte(T_G, T_D, x,t, D) result(theta)
        real(pr), intent (in)::  T_G, T_D, D ,x,t
        real(Pr)::theta
         integer::n
         theta=x
         do n=1,100
            theta=theta+(2/(n*pi))*((-1)**n)*exp(-D*((n*pi)**2)*t)*sin(n*pi*x)
         end do
         theta=T_G+(T_D-T_G)*theta
    end function T_exacte


    


end program chaleur