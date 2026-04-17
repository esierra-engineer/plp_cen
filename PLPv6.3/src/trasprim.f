!***************************
!     Traspasa la Solucion Primal
!***************************
      SUBROUTINE TrasPrimal(PDNCol, ScaleObj,                           &
     &     Phi, Z, Primal,                                              &
     &     lp)
!     archivo comun a todas las rutinas
      USE OSI
!     variables:
      INTEGER(C_SIZE_T), INTENT(IN) :: lp
      INTEGER, INTENT(IN) :: PDNCol
      DOUBLE PRECISION, INTENT(IN):: Z
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
!     outs
      DOUBLE PRECISION, INTENT(OUT):: Primal(PDNCol)
      DOUBLE PRECISION, INTENT(OUT):: Phi

!     locals
      INTEGER NCol
!     codigo:

      NCol = osi_lp_getnumcols (lp)

      CALL TrasPrimali(NCol, PDNCol, ScaleObj,                          &
     &     Phi, Z, Primal,                                              &
     &     lp)

      RETURN
      END


      SUBROUTINE TrasPrimali(NCol, PDNCol, ScaleObj,                    &
     &     Phi, Z, Primal,                                              &
     &     lp)
!     archivo comun a todas las rutinas
      USE OSI
!     variables:
      INTEGER(C_SIZE_T), INTENT(IN) :: lp
      INTEGER, INTENT(IN) :: PDNCol
      DOUBLE PRECISION, INTENT(IN):: Z
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
!     outs
      DOUBLE PRECISION, INTENT(OUT):: Primal(PDNCol)
      DOUBLE PRECISION, INTENT(OUT):: Phi

!     locals
      INTEGER ICol
      INTEGER NCol
      DOUBLE PRECISION VarPhiObj(NCol - PDNCol)
      DOUBLE PRECISION PrimalTmp(NCol)

!     Pruebas.
!     codigo:

      CALL GetPrimal(NCol, ScaleObj, Z, PrimalTmp, lp)
      Primal(1:PDNCol) = PrimalTmp(1:PDNCol)

      CALL osi_lp_getobjcoeff(lp, PDNCol, NCol - 1, VarPhiObj)
      Phi = 0.0d0
      DO ICol = PDNCol + 1, NCol
         Phi = Phi + PrimalTmp(ICol)*VarPhiObj(ICol - PDNCol)*ScaleObj
      ENDDO
      RETURN
      END

