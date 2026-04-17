!******************
!     Subrutina TrasDual
!******************
      SUBROUTINE TrasDual(ScaleObj,                                     &
     &     PDLDAcFilaInd, PDLDAcNFila,                                  &
     &     Z, PromedioPi,                                               &
     &     lp)
!     archivo comun a todas las rutinas
      USE OSI
!     variables:
      INTEGER(C_SIZE_T), INTENT(IN):: lp
      INTEGER, INTENT(IN):: PDLDAcNFila
      INTEGER, INTENT(IN):: PDLDAcFilaInd(PDLDAcNFila)
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
!     outs
      DOUBLE PRECISION, INTENT(OUT):: PromedioPi(PDLDAcNFila)
      DOUBLE PRECISION, INTENT(OUT):: Z
!     locals
      INTEGER NFila

!     codigo:

!     Access number of constraints in problem.
      NFila = osi_lp_getnumrows (lp)

      CALL TrasDuali(NFila, ScaleObj,                                   &
     &     PDLDAcFilaInd, PDLDAcNFila,                                  &
     &     Z, PromedioPi,                                               &
     &     lp)

      RETURN
      END

      SUBROUTINE TrasDuali(NFila, ScaleObj,                             &
     &     PDLDAcFilaInd, PDLDAcNFila,                                  &
     &     Z, PromedioPi,                                               &
     &     lp)

      USE PLP

!     variables:
      INTEGER, INTENT(IN):: NFila
      INTEGER(C_SIZE_T), INTENT(IN):: lp
      INTEGER, INTENT(IN):: PDLDAcNFila
      INTEGER, INTENT(IN):: PDLDAcFilaInd(PDLDAcNFila)
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
!     outs
      DOUBLE PRECISION, INTENT(OUT):: PromedioPi(PDLDAcNFila)
      DOUBLE PRECISION, INTENT(OUT):: Z
!     locals
      INTEGER IFila
      INTEGER IFilaAcop
      DOUBLE PRECISION Dual(NFila)
      
      CALL GetDual(NFila, ScaleObj, Z, Dual, lp)

      DO IFilaAcop = 1, PDLDAcNFila
         IFila = PDLDAcFilaInd(IFilaAcop)
         PromedioPi(IFilaAcop) = PromedioPi(IFilaAcop) + Dual(IFila)
      ENDDO

      RETURN
      END
