
c           call iau_JD2CAL()    SOFA包里的jd2cal.for文件，实现儒略日向格里历的转化    (in libsofa_20210512.a)
c           call SPLIT()    split.f里的subroutine    (in libtesteph.a)       



      subroutine jd2CST(DJ, CST)

        real(8) DJ, DJ_CST, DJ2(2)
        integer CST(6)
        integer IY, IM, ID, J
        real(8) FD

        DJ_CST = DJ + 8.0/24.0
        call SPLIT(DJ_CST, DJ2)
        call iau_JD2CAL ( DJ2(1), DJ2(2), IY, IM, ID, FD, J )

        if (J .EQ.  0) then
            CST(1) = IY
            CST(2) = IM
            CST(3) = ID
            CST(4) = INT(24.0*FD)
            CST(5) = INT((24.0*FD - CST(4))*60)
            CST(6) = NINT(((24.0*FD - CST(4))*60 - CST(5))*60)

            if (CST(6) .EQ. 60) then
              CST(6) = 0
              CST(5) = CST(5) + 1
            end if

            if (CST(5) .EQ. 60) then
              CST(5) = 0
              CST(4) = CST(4) + 1
            end if
            
        else 
            write(*,*) 'ERROR! Unacceptable date!'
        end if

      end subroutine jd2CST