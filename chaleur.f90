program chaleur
    implicit none

    integer :: n
    real :: u

    u=1.0

    do n=1,10
        u=u/2.0+1.0/u
    end do

    print*, 'approx. de sqrt(2) :',u
    print*,'utilisation de git'
    
end program chaleur