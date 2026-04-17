!************************************
!     Matriz Invariante A Agotaniento
!************************************
      SUBROUTINE GenPDAgoA(IBloque, NBarra, NCenEmb, CenInd,            &
     &     NAfluFict,                                                   &
     &     bdur,                                                        &
     &     COffset, FOffset,                                            &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!***********************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*(DimLargo) CAfluFict
      CHARACTER*80 fconcat
      INTEGER CenInd(Dim%Cen)
      INTEGER NAfluFict
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER IBloque
      INTEGER COffset
      INTEGER FOffset
      TYPE(AMatrix) A
      DOUBLE PRECISION bdur
!     Variables Locales
!**********************
      INTEGER FOffseti
      INTEGER IAfluFict
      INTEGER ICen
      DO IAfluFict = 1, NAfluFict

         Nombre = 'qaf'
         CALL Num2Char(CenInd(IAfluFict), CAfluFict, No, DimLargo)
         Nombre = fconcat(Nombre, CAfluFict)

         Nombre = fconcat(Nombre, '_')
         CALL Num2Char(IBloque, CAfluFict, No, DimLargo)
         Nombre = fconcat(Nombre, CAfluFict)

         PDNombre(COffset + IAfluFict) = Nombre
         FOffseti = FOffset

!     Demanda
!************
         FOffseti = FOffseti + NBarra

!     Balance Caudal Embalse
!**************************
         DO ICen = 1, NCenEmb
            IF (ICen .EQ. IAfluFict) THEN
               CALL Am_set(A, COffset + IAfluFict, FOffseti + ICen, -bdur )
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenEmb

      ENDDO

!***************
!     Offsets Finales
!***************
      COffset = COffset + NAfluFict

      RETURN
      END


!     Matriz FO y Limites Agotaniento
!************************************
      SUBROUTINE GenPDAgoFO(NAfluFict,                                  &
     &     CCaudFalla,                                                  &
     &     BloDur, FPhi,                                                &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)
      USE OSI
!     Variables Globales
!***********************
      INTEGER PDNCol
      INTEGER NAfluFict
      INTEGER COffset
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION CCaudFalla
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!**********************
      INTEGER IAfluFict

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      DO IAfluFict = 1, NAfluFict
!     Funcion Objetivo
!*********************
         FO(COffset + IAfluFict) = CCaudFalla*                    &
     &        BloDur/FPhi
!     Restricciones de tipo 'x <= ' y 'x >= '
!**********************************************
         LowBnd(COffset + IAfluFict) = 0.0d0
         UppBnd(COffset + IAfluFict) = DINFTY
      ENDDO
      RETURN
      END
