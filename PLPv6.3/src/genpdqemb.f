!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDQEmbA(IBloque, NBarra, NCenEmb, CenInd,           &
     &     NVol, ScaleVol,                                              &
     &     bdur, bdursc,                                                &
     &     COffset, FOffset, VOffset,                                   &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!******************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*(DimLargo) CVol
      CHARACTER*80 fconcat
      INTEGER CenInd(Dim%Cen)
      INTEGER NVol
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER COffset
      INTEGER FOffset
      INTEGER VOffset
      INTEGER IBloque
      TYPE(AMatrix) A
      DOUBLE PRECISION ScaleVol(NVol)
      DOUBLE PRECISION bdur
      DOUBLE PRECISION bdursc
!     Variables Locales
!****************
      INTEGER FOffseti
      INTEGER VOffseti
      INTEGER ICen
      INTEGER IVol
      DO IVol = 1, NVol

         Nombre = 'qe'
         CALL Num2Char(CenInd(IVol), CVol, No, DimLargo)
         Nombre = fconcat(Nombre, CVol)

         Nombre = fconcat(Nombre, '_')
         CALL Num2Char(IBloque, CVol, No, DimLargo)
         Nombre = fconcat(Nombre, CVol)

         PDNombre(COffset + IVol) = Nombre

         FOffseti = FOffset

!**************************
!     Demanda
!**************************
         FOffseti = FOffseti + NBarra

!********************
!     Balance Hidro Embalse
!********************
         DO ICen = 1, NCenEmb
            IF (ICen .EQ. IVol) THEN
               CALL Am_set(A, COffset + IVol, FOffseti + ICen, bdursc)
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenEmb


!***************
!     Vol Embalse
!***************
         VOffseti = VOffset
         VOffseti = VOffseti + NCenEmb

         DO ICen = 1, NCenEmb
            IF (ICen .EQ. IVol) THEN
               CALL Am_set(A, COffset + IVol, VOffseti + ICen, -bdur/ScaleVol(ICen))
            ENDIF
         ENDDO
      ENDDO

!***************
!     Offsets Finales
!***************
      COffset = COffset + NVol

      RETURN
      END



!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDQEmbFO(NVol,                                      &
     &     EmbQeLow, EmbQeUpp,                                          &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)
!     Variables Globales
!******************
      INTEGER PDNCol
      INTEGER NVol
      INTEGER COffset
      DOUBLE PRECISION EmbQeLow(NVol)
      DOUBLE PRECISION EmbQeUpp(NVol)
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!*****************
      INTEGER IVol
      DO IVol = 1, NVol

!     Funcion Objetivo
!****************
         FO(COffset + IVol) = 0.d0

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
         UppBnd(COffset + IVol) = EmbQeUpp(IVol)
         LowBnd(COffset + IVol) = EmbQeLow(IVol)
      ENDDO
      RETURN
      END
