!*******************
!     Subrutina getprimal
!*******************
      SUBROUTINE GetPrimal (NCol, ScaleObj, Z, PrimalTmp, lp)
      USE OSI
      INTEGER(C_SIZE_T) lp
      INTEGER NCol
      DOUBLE PRECISION PrimalTmp (NCol)
      DOUBLE PRECISION Z
      DOUBLE PRECISION ScaleObj
!
!
!     Access LP solution objective value
      Z = osi_lp_getobjvalue(lp)

      Z = Z*ScaleObj

!
!     Access optimal variable values
      CALL osi_lp_getcolsol (lp, 0, NCol - 1, PrimalTmp)
      RETURN
      END
