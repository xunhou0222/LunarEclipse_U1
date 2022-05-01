c           call PLEPH          eph.f文件中的subroutine，用于计算给定历元某天体相对中心天体的位置与速度 (in libtesteph.a)
c           call CONST          eph.f文件中的subroutine，用于获取历表文件中的常数    (in libtesteph.a)
c           call iau_cal2jd     实现格里历向儒略日的转化    (in libsofa_20210512.a)
c           call statevector    计算给定历元目标天体相对中心天体的位置矢量，可选择是否考虑光行时    (in StateVector.f)


      program main

c******************************************************************
c
c    本程序中，距离单位转化为km，速度单位转化为km/s
c
c******************************************************************

        implicit none

c     读取历表文件中的常数所需的参数
        integer N    !常数总个数
        character*6 NAM(300)    !数组，存储常数名称
        real(8) VAL(300)    !数组。存储常数数值
        real(8) SSS(3)    !数组，存储历表文件起始历元，以及每个数据块的时间间隔（32天）
        real(8) r_sun, r_earth, r_moon, AU, clight    !历表文件中给出的太阳、地球、月球半径与AU、光速的值

        real(8) rho_e2s    !地球到太阳中心的距离
        real(8) Re2O(3)   !地球相对于锥心的位置矢量
        real(8) rho_e2O    !地球到锥心的距离
        real(8) RO2s(3)   !锥心相对于太阳中心的位置矢量
        real(8) Rm2O(3)   !月球相对于锥心的位置矢量
        real(8) rho_m2O
        real(8) theta_E, theta_M, delta, delta1    !地球与月球相对于锥心的夹角等参数

        integer, parameter:: year=2022, month=1, day=1    !设置预测的初始历元为2021年1月1日
        real(8) JD0, JD    !初始历元的儒略日与后续预测历元的儒略日
        real(8),parameter:: predict = 366.0    !设置总预测的时间范围为一年（366天左右）
        real(8) step, precision    !设置循环预测时，历元推进的步长，以及精度 
        real(8) outJD(10)
        integer outCST(6,10)


c     存储iau_cal2jd的返回值        
        integer J    !iau_cal2jd的返回状态信息，J=0则返回成功
        real(8) MDJ(2)    !iau_cal2jd返回的儒略日

        integer:: Nsun = 11, Nmoon = 10    !太阳与月亮的代号
        real(8) POS(3)    !PLEPH子程序返回的某天体相对中心天体的位置与速度
        real(8) Rm2s(3)    !月球相对于太阳的位置矢量
        real(8) Re2s(3)    !地球相对于太阳的位置矢量

        integer I1, I2  !计数器
        real(8),parameter:: PI = DACOS(-1.0D0)

        logical:: switch  = .TRUE.    !考虑光行差修正

c***********************************************************************

        write(*,'(A21,X,I4,A,6X,A29)') 'Calculation of U1 for', year, 
     &            '.', 'By Tingwei Jiang, UCAS, SHAO.'
        write(*,*)
        write(*,*) 'Take it easy! Just a few moments......'
        
        call CONST(NAM, VAL, SSS, N)
        do I1 = 1, N
            if (NAM(I1) .EQ. 'AU') then 
                AU = VAL(I1)    !历表给出的常数以km为单位，而位置以AU为单位，需要转换。
            else if (NAM(I1) .EQ. 'CLIGHT') then
                clight = VAL(I1)
            else if (NAM(I1) .EQ. 'RE') then
                r_earth = VAL(I1)*1.01  
            else if (NAM(I1) .EQ. 'ASUN') then
                r_sun = VAL(I1)
            else if (NAM(I1) .EQ. 'AM') then
                r_moon = VAL(I1)
                exit   !因为AM是参数表的要寻找的三个参数的最后一个
            end if
        end do
        
        call iau_cal2jd(year, month, day, MDJ(1), MDJ(2), J)
        if (J .EQ. 0) then
            JD0 = MDJ(1) + MDJ(2)
        else 
            write(*,*) 'ERROR!'//
     &        'Calculation of JD for the initial epoch failed!'
            stop
        end if  

        if ((JD0 .LT. SSS(1)) .OR. (JD0 + predict .GE. SSS(2))) then 
            write(*,*) 'ERROR!'//
     &        'The time interval of prediction is beyond the limit!'
            stop
        end if 


        JD = JD0
        precision = 1.0/3600.0/100.0/180.0*PI    !角度精度为10毫角秒
        step = 27.0/2.0/PI
        I2 = 0
         
c      开始循环，按step推进计算各个预测历元下的情况，寻找月食初亏时刻
        do while(JD .LE. (JD0 + predict))
            call statevector(switch,AU,clight,JD+1.0D-8,Nsun,POS)
            Re2s(1)= -POS(1)
            Re2s(2)= -POS(2)
            Re2s(3)= -POS(3)
            call statevector(switch,AU,clight,JD+1.0D-8,Nmoon,POS)
            Rm2s(1)= POS(1) + Re2s(1)
            Rm2s(2)= POS(2) + Re2s(2)
            Rm2s(3)= POS(3) + Re2s(3)
            
            rho_e2s = DSQRT(Re2s(1)**2 + Re2s(2)**2 +Re2s(3)**2)
            call solve_rho_e2O(r_sun, r_earth, rho_e2s, rho_e2O)
            RO2s = (rho_e2s + rho_e2O)/rho_e2s*Re2s
            Rm2O = Rm2s - RO2s
            rho_m2O = DSQRT(Rm2O(1)**2 + Rm2O(2)**2 +Rm2O(3)**2)
            Re2O = Re2s - RO2s

            theta_E = DASIN(r_earth/rho_e2O)
            theta_M = DASIN(r_moon/rho_m2O)
            delta1 = DACOS((Re2O(1)*Rm2O(1) + 
     &               Re2O(2)*Rm2O(2) + 
     &               Re2O(3)*Rm2O(3))/rho_e2O/rho_m2O) 
     &                   - theta_M - theta_E


            call statevector(switch, AU, clight, JD, Nsun, POS)
            Re2s(1)= -POS(1)
            Re2s(2)= -POS(2)
            Re2s(3)= -POS(3)
            call statevector(switch, AU, clight, JD, Nmoon, POS)
            Rm2s(1)= POS(1) + Re2s(1)
            Rm2s(2)= POS(2) + Re2s(2)
            Rm2s(3)= POS(3) + Re2s(3)
     
            rho_e2s = DSQRT(Re2s(1)**2 + Re2s(2)**2 +Re2s(3)**2)
            call solve_rho_e2O(r_sun, r_earth, rho_e2s, rho_e2O)
            RO2s = (rho_e2s + rho_e2O)/rho_e2s*Re2s
            Rm2O = Rm2s - RO2s
            rho_m2O = DSQRT(Rm2O(1)**2 + Rm2O(2)**2 +Rm2O(3)**2)
            Re2O = Re2s - RO2s

            theta_E = DASIN(r_earth/rho_e2O)
            theta_M = DASIN(r_moon/rho_m2O)
            delta = DACOS((Re2O(1)*Rm2O(1) + 
     &               Re2O(2)*Rm2O(2) + 
     &               Re2O(3)*Rm2O(3))/rho_e2O/rho_m2O) 
     &                   - theta_M - theta_E

            if (rho_m2O .LT. rho_e2O .AND. 
     &                    DABS(delta) .GT. DABS(delta1)) then
                if (DABS(delta) .LT. precision) then
                    I2 = I2 + 1
                    outJD(I2) = JD
                    call jd2CST(JD, outCST(1, I2))
                    JD = JD + 150.0
                else if (DABS(delta) .GT. 0.001) then
                    JD = JD + delta*step
                else 
                    JD = JD + delta*step/10.0
                end if

            else    
                JD = JD + 5.0

            end if

        end do

        write(*, 137) 'NO','JD(UTC)','TIME(CST)'

137   format(/A7, A16, A30)

        do I1 = 1, I2
            write(*, 138) I1, outJD(I1),
     &          outCST(1,I1),outCST(2,I1),outCST(3,I1),
     &          outCST(4,I1),outCST(5,I1),outCST(6,I1) 
        end do

138   format(/I6, F20.5, 
     &         I16, '/', I2.2, '/', I2.2,' ',
     &         I2.2, ':', I2.2, ':', I2.2)

     
        read(*,*)
    
      end program main