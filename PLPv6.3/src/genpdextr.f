      SUBROUTINE GenPDExtrA(IBloque, NBarra, NCenEmb, NCenSer, CenInd,  &
     &     ExtrNCen, ExtrCenInd, CenXHid,                               &
     &     bdursc,                                                      &
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
      CHARACTER*(DimLargo) CExtr
      CHARACTER*80 fconcat

      INTEGER CenInd(Dim%Cen)
      INTEGER IBloque
      INTEGER CenXHid(Dim%Cen)
      INTEGER COffset
      INTEGER FOffset
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER ExtrNCen
      INTEGER ExtrCenInd(Dim%Extr)

      TYPE(AMatrix) A
      DOUBLE PRECISION bdursc
!     Variables Locales
!**********************
      INTEGER FOffseti
      INTEGER ICen
      INTEGER ICentral
      INTEGER IExtrCen
!
      DO IExtrCen = 1, ExtrNCen
         ICentral = ExtrCenInd(IExtrCen)


         Nombre = 'qx'
         CALL Num2Char(CenInd(ICentral), CExtr, No, DimLargo)
         Nombre = fconcat(Nombre, CExtr)

         Nombre = fconcat(Nombre, '@')
         CALL Num2Char(IExtrCen, CExtr, No, DimLargo)
         Nombre = fconcat(Nombre, CExtr)

         Nombre = fconcat(Nombre, '_')
         CALL Num2Char(IBloque, CExtr, No, DimLargo)
         Nombre = fconcat(Nombre, CExtr)

         PDNombre(COffset + IExtrCen) = Nombre

         FOffseti = FOffset

!**************************
!     Demanda
!**************************
         FOffseti = FOffseti + NBarra
!**************************
!     Balance Hidro Embalse
!**************************
         DO ICen = 1, NCenEmb + NCenSer
            IF (ICen .EQ. ICentral) THEN
               CALL Am_set(A, COffset + IExtrCen, FOffseti + ICen, bdursc)
            ELSE
               IF (CenXHid(IExtrCen) .EQ. ICen) THEN
                  CALL Am_set(A, COffset + IExtrCen, FOffseti + ICen, -bdursc)
               ENDIF
            ENDIF
         ENDDO
      ENDDO

      COffset = COffset + ExtrNCen

      RETURN
      END


!**********************************
!     Matriz FO y Limites Centrales
!**********************************
      SUBROUTINE GenPDExtrFO(ExtrNCen, ExtrMax,                         &
     &     COffset,                                                     &
     &     PDNCol, FO, LowBnd, UppBnd)

!     Variables Globales
!***********************
      INTEGER PDNCol
      INTEGER ExtrNCen
      INTEGER COffset
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      DOUBLE PRECISION ExtrMax(ExtrNCen)
!     Variables Locales
!**********************
      INTEGER IExtrCen
      DO IExtrCen = 1, ExtrNCen
!     Funcion Objetivo
!*********************
         FO(COffset + IExtrCen) = 0.0d0
!     Restricciones de tipo 'x <= ' y 'x >= '
!********************************************
         LowBnd(COffset + IExtrCen) = 0.0d0
         UppBnd(COffset + IExtrCen) = ExtrMax(IExtrCen)
      ENDDO

      RETURN
      END
