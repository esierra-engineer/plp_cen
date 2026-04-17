
!************************************************************
      SUBROUTINE BorResPD (lp, NFilaInicial, NFilaFinal)
      USE OSI

      INTEGER(C_SIZE_T) lp
      INTEGER NFila
      INTEGER NFilaFinal
      INTEGER NFilaInicial

!     Access number of constraints in problem.
      NFila = osi_lp_getnumrows (lp)
      NFilaFinal = min0(NFilaFinal, NFila)
!
!     esta rutina debe llamarse solo si NFilaFinal >= NFilaInicial
      IF (NFilaFinal .LT. NFilaInicial) RETURN
!
!     Delete range of constraints.      

      CALL BorResPDi(lp, NFilaInicial, NFilaFinal)

      RETURN
      END

      SUBROUTINE BorResPDi (lp, NFilaInicial, NFilaFinal)
      USE OSI

      INTEGER(C_SIZE_T) lp
      INTEGER NFilaFinal
      INTEGER NFilaInicial
      
      INTEGER rows(NFilaFinal - NFilaInicial + 1)

      INTEGER idx, num

      num = NFilaFinal - NFilaInicial + 1

      IF (num .lt. 1) RETURN

      DO idx = 0, num - 1
         rows(idx + 1) = idx +  NFilaInicial - 1
      ENDDO

      CALL osi_lp_deleterows (lp, num, rows)
      
      RETURN
      END
