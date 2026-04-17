!**********************************************************
!     modifica el borde del problema de pl (obj, upp, low y rhs)
!**********************************************************
!**********************************************************
!     modifica el borde del problema de pl (rhs)
!**********************************************************
      SUBROUTINE ModifBrdRhs(ModifNLD, MFRhsInd, MFRhsVal, lp)
      USE OSI
!
!
      INTEGER, INTENT(IN) :: ModifNLD
      INTEGER, INTENT(IN) :: MFRhsInd (ModifNLD)
      DOUBLE PRECISION, INTENT(IN) :: MFRhsVal (ModifNLD)
!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     locals
      INTEGER IModifInd
      INTEGER index
!
      DO IModifInd = 1, ModifNLD
         index = MFRhsInd(IModifInd) - 1
         CALL osi_lp_setrowrhs(lp, index, MFRhsVal(IModifInd))
      ENDDO

      RETURN
      END


!**********************************************************
!     modifica el borde de las variables (low)
!**********************************************************

      SUBROUTINE ModifLow(ModifNLow, MCLowInd, MCLowVal, lp)
      USE OSI
!
!
      INTEGER, INTENT(IN) :: ModifNLow
      INTEGER, INTENT(IN) :: MCLowInd (ModifNLow)
      DOUBLE PRECISION, INTENT(IN) :: MCLowVal (ModifNLow)
!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     locals
      INTEGER IModifInd
      INTEGER index
!
      DO IModifInd = 1, ModifNLow
         index = MCLowInd(IModifInd) - 1
         CALL osi_lp_setcollower(lp, index, MCLowVal(IModifInd))
      ENDDO
      RETURN
      END

!**********************************************************
!     modifica el borde de las variables (upp)
!**********************************************************
      SUBROUTINE ModifUpp(ModifNUpp, MCUppInd, MCUppVal, lp)
      USE OSI
!
!
      INTEGER, INTENT(IN) :: ModifNUpp
      INTEGER, INTENT(IN) :: MCUppInd (ModifNUpp)
      DOUBLE PRECISION, INTENT(IN) :: MCUppVal (ModifNUpp)
!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     locals
      INTEGER IModifInd
      INTEGER index      
!
      DO IModifInd = 1, ModifNUpp
         index = MCUppInd(IModifInd) - 1
         CALL osi_lp_setcolupper(lp, index, MCUppVal(IModifInd))
      ENDDO
      RETURN
      END


      DOUBLE PRECISION FUNCTION GetUppBnd(index, lp)
      USE OSI

      INTEGER(C_SIZE_T), INTENT(IN) :: lp
      INTEGER index

      INTEGER idx
      DOUBLE PRECISION UppBnd(1)

      idx = index - 1
      CALL osi_lp_getcolupper(lp, idx, idx, UppBnd)

      GetUppBnd = UppBnd(1)

      RETURN
      END


      DOUBLE PRECISION FUNCTION GetLowBnd(index, lp)
      USE OSI

      INTEGER(C_SIZE_T), INTENT(IN) :: lp
      INTEGER index
      INTEGER idx

      DOUBLE PRECISION LowBnd(1)

      idx = index - 1
      CALL osi_lp_getcollower(lp, idx, idx, LowBnd)

      GetLowBnd = LowBnd(1)

      RETURN
      END
 
      


!**********************************************************
!     modifica el borde , funcion objectivo
!**********************************************************


      SUBROUTINE ModifObj(ModifNObj, MCObjInd, MCObjVal, lp)
      USE OSI
!
!
      INTEGER, INTENT(IN) :: ModifNObj
      INTEGER, INTENT(IN) :: MCObjInd (ModifNObj)
      DOUBLE PRECISION, INTENT(IN) :: MCObjVal (ModifNObj)
!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     locals
      INTEGER IModifInd
      INTEGER index
!
      DO IModifInd = 1, ModifNObj
         index = MCObjInd(IModifInd) - 1
         CALL osi_lp_setobjcoeff(lp, index, MCObjVal(IModifInd))
      ENDDO
      
      RETURN
      END
