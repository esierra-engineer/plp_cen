!******************************************************************
      SUBROUTINE GetDual (NFila, ScaleObj, Z, Dual, lp)

      USE OSI
      INTEGER, INTENT(IN):: NFila
      INTEGER(C_SIZE_T), INTENT(IN):: lp
      DOUBLE PRECISION, INTENT(IN):: ScaleObj

!     outs
      DOUBLE PRECISION, INTENT(OUT):: Dual (NFila)
      DOUBLE PRECISION, INTENT(OUT):: Z


!
!
!     Access LP solution objective value
      Z = osi_lp_getobjvalue(lp)
      Z = Z*ScaleObj
!
!     Access constraint dual values
      CALL osi_lp_getrowprice (lp, 0, NFila - 1, Dual)

      Dual(1:NFila) = Dual(1:NFila)*ScaleObj
      
      RETURN
      END
