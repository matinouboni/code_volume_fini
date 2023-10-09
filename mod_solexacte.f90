module mod_solexacte
    
    use mod_precision
    implicit none

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
        