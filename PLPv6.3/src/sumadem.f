!***********************
!     Subrutina Suma Demandas
!***********************
!     Suma las demandas por barra
      SUBROUTINE SumaDem(NBloque, NBarra, BloPot, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     comun a todas las rutinas:
      INTEGER IBar
      INTEGER IBlo
      INTEGER NBarra
      INTEGER NBloque
      INTEGER ULog
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
!     codigo:
      WRITE(ULog, '(A)') 'sumadem: Sistema equivalente uninodal.'
      DO IBlo = 1, NBloque
         DO IBar = 2, NBarra
            BloPot(1, IBlo) = BloPot(1, IBlo) + BloPot(IBar, IBlo)
         ENDDO
      ENDDO
      RETURN
      END
