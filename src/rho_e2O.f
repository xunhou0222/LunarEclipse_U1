      subroutine solve_rho_e2O(r_sun, r_earth, rho_e2s, rho_e2O)
c
c   不动点迭代求解地球锥心矢量长度
c       input:
c            r_sun    太阳半径
c            r_earth  地球半径
c            rho_e2s  地球到太阳中心的距离
c        output:
c            rho_e2O  地球的锥心矢量长度
    
        implicit none
    
        real(8) r_sun, r_earth, rho_e2s, rho_e2O
        real(8) rho
    
        rho = r_earth/r_sun*rho_e2s
        rho_e2O = r_earth/r_sun*(rho + rho_e2s)
    
        do while(DABS(rho - rho_e2O) > 10.0D-13)
            rho = rho_e2O
            rho_e2O = r_earth/r_sun*(rho + rho_e2s)
        end do
    
      end subroutine solve_rho_e2O