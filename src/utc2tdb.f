c         call SPLIT    (in libtesteph.a)
c         call iau_UTCTAI    (in libsofa_20210512.a)
c         call iau_TAITT    (in libsofa_20210512.a)


      subroutine utc2tdb(JD, TDB)

c************************************************************************************
c
c       由于历表文件的时间系统是TDB，因此需要将UTC转化为TDB。
c   但由于sofa包下实现TT与TDB转化的“tttdb.for”文件的注释中提到，TT与TDB只差仅为毫秒量级，
c   因此本程序不予考虑两者的差值，即，最终实现的时UTC转化为TT。
c
c*************************************************************************************

          implicit none

          real(8) JD, TDB
          real(8) UTC(2), TAI(2), TT(2)
          integer J, K

          call SPLIT(JD, UTC)

          call iau_UTCTAI(UTC(1), UTC(2), TAI(1), TAI(2), J)
          if (J .EQ. 0) then
              call iau_TAITT(TAI(1), TAI(2), TT(1), TT(2), K)
              if (K .NE. 0) then
                write(*,*)'Errors occured while Converting TAI TO TT!'
                stop
              end if
          else
              write(*,*)'Errors occured while Converting UTC TO TAI!'
              stop
          end if

          TDB = TT(1) + TT(2)

      end subroutine utc2tdb