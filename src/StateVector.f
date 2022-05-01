c         call PLEPH    (in libtesteph.a)
c         call utc2tbd    (in utc2tdb.f)


      subroutine statevector(switch, AU, clight, JD, Ntar, POS)

c*************************************************************************************
c
c      计算给定历元下的某目标天体(太阳、月球）相对于地球的位置矢量
c      Input:
c          (1)switch    
c                     logical        switch为true时，考虑光行时，否则不考虑。
c          (2)AU
c                     real(8)      天文单位AU，单位km
c          (3)clight
c                     real(8)      光速，单位km/s
c          (4)JD
c                     real(8)      历元儒略日（UTC）
c          (5)Ntar  
c                     integer      目标天体的代号
c         
c      Output:
c             POS
c                     real(8),dimension(3)    目标天体相对于中心天体的位置矢量
c
c**************************************************************************************          

          implicit none

          logical switch
          real(8):: AU, clight, JD, POS(3)
          integer Ntar
          integer,parameter:: Ncen = 3
          real(8) TDB, PRD(6), VOL(3), r, DTDB1, DTDB2
          real(8),parameter:: d2s = 86400.0D0

          call utc2tdb(JD, TDB) 

          call PLEPH(TDB, Ntar, Ncen, PRD)
          POS(1) = PRD(1)*AU
          POS(2) = PRD(2)*AU
          POS(3) = PRD(3)*AU
          VOL(1) = PRD(4)*AU/d2s
          VOL(2) = PRD(5)*AU/d2s
          VOL(3) = PRD(6)*AU/d2s
          
          if (switch) then
            r = DSQRT(POS(1)**2 + POS(2)**2 +POS(3)**2)
            DTDB1 = 0.0
            DTDB2 = r/clight/d2s

            do while (DABS(DTDB1 - DTDB2) .GT. (1.0/d2s/10.0**3))
                DTDB1 = DTDB2
                POS(1) = POS(1) - VOL(1)*DTDB1*d2s
                POS(2) = POS(2) - VOL(2)*DTDB1*d2s
                POS(3) = POS(3) - VOL(3)*DTDB1*d2s
                r = DSQRT(POS(1)**2 + POS(2)**2 +POS(3)**2)
                DTDB2 = r/clight/d2s
            end do

            call PLEPH(TDB - DTDB2, Ntar, Ncen, PRD)
            POS(1) = PRD(1)*AU
            POS(2) = PRD(2)*AU
            POS(3) = PRD(3)*AU
            VOL(1) = PRD(4)*AU/d2s
            VOL(2) = PRD(5)*AU/d2s
            VOL(3) = PRD(6)*AU/d2s

          end if

      end subroutine statevector