      SUBROUTINE ModifCoef(NCoef, MCoefColInd, MCoefFilInd, MCoefVal, lp)
      USE OSI

!     archivo comun a todas las rutinas:
      INTEGER, INTENT(IN) :: NCoef
      DOUBLE PRECISION, INTENT(IN):: MCoefVal (NCoef)
      INTEGER, INTENT(IN) :: MCoefColInd (NCoef)
      INTEGER, INTENT(IN) :: MCoefFilInd (NCoef)

!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     local
      INTEGER i
      INTEGER j
      INTEGER IC   
      
      DO IC = 1, NCoef
         j = MCoefColInd (IC) - 1
         i = MCoefFilInd (IC) - 1
         CALL osi_lp_setcoefficient(lp, i, j, MCoefVal (IC))
      ENDDO
      RETURN
      END
