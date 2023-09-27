program chaleur

    use mod_maillage

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
    real(pr) :: cfl=1
    real(pr) :: T_G=100
    real(pr) :: T_D=300
    real(pr) :: T_init=100

    !allocation et initialisation des tableaux T et Tnp1
    real(pr), dimension(:), allocatable :: T, Tnp1



    !pas de temps 
    real(pr) :: dt
    !variable temporaire dans le calcul de dt
    real(pr) :: temp
    integer :: i, j



    open ( unit=10, file = 'cas_1d.mesh' )

    fichier_maillage='cas_1d.mesh'
    

    call maillage(fichier_maillage, nb_mailles, nb_aretes                    &
        &         , coord_noeud, noeud_maille, aire_maille, l_arete, d_arete &
        &         , milieu_arete, arete_maille, maille_arete, cl_arete)

    ! Fermeture du fichier
        close(10)

    !calcul du pas de temps deltat
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
        endif
    end do

    print*,'delta=',dt


    
end program chaleur