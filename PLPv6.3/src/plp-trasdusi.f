!***********************
!     Subrutina TrasDualSimul
!***********************
      SUBROUTINE TrasDualSimul(PDNFila, ScaleObj,                       &
     &     SimDual, lp)
!     archivo comun a todas las rutinas
      USE PLP

!     variables:
      INTEGER(C_SIZE_T), INTENT(IN) :: lp
      INTEGER, INTENT(IN) :: PDNFila
      DOUBLE PRECISION, INTENT(IN) :: ScaleObj

!     outs
      DOUBLE PRECISION, INTENT(OUT) :: SimDual(PDNFila)
!     locals
      DOUBLE PRECISION Z

!     codigo:
      CALL GetDual(PDNFila, ScaleObj, Z, SimDual, lp)

      RETURN
      END
