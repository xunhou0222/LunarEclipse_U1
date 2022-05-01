      subroutine SPLIT(TT,FR)
        
          implicit none
            
          real(8) FR(2), TT  
        
          FR(1)=DINT(TT)
          FR(2)=TT-FR(1)
            
          if (TT.GE.0.D0 .OR. FR(2).EQ.0.D0) return
                    
          FR(1)=FR(1)-1.D0
          FR(2)=FR(2)+1.D0
        
      end subroutine SPLIT