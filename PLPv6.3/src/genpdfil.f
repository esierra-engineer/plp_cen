!**********************************
!>    Matriz Invariante A Centrales
!**********************************
      SUBROUTINE GenPDFilA(CenInd, FiltNCen, FiltEmbInd,                 &
     &     COffset, FOffset,                                             &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
!     Variables Globales
!***********************
      INTEGER PDNCol
      CHARACTER*(DimLargo) CCentral
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre(PDNCol)
      CHARACTER*80 fconcat
      INTEGER CenInd(Dim%Cen)
      INTEGER COffset
      INTEGER FOffset
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      TYPE(AMatrix) A
      INTEGER FOffseti 
!     Variables Locales
!**********************
      INTEGER ICentral
      INTEGER IFiltCen
!
      DO IFiltCen = 1, FiltNCen
         ICentral = FiltEmbInd(IFiltCen)

         Nombre = 'qf'
         CALL Num2Char(CenInd(ICentral), CCentral, No, DimLargo)
         Nombre = fconcat(Nombre, CCentral)
         PDNombre(COffset + IFiltCen) = Nombre

         FOffseti = FOffset

         CALL Am_set(A, COffset + IFiltCen, FOffseti + IFiltCen, 1.d0         )
      ENDDO     
      RETURN
      END


      SUBROUTINE GenPDFilAi(NBarra, NCenEmb, NCenSer,                   &
     &     FiltNCen, FiltEmbInd,                                        &
     &     CenFHid,                                                     &
     &     bdursc,                                                      &
     &     COffset, FOffset,                                            &
     &     A, Dim)
      USE PLP, ONLY : PAR_DIMS
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
!     Variables Globales
!***********************
      INTEGER CenFHid(Dim%EmbFilt)
      INTEGER COffset
      INTEGER FOffset
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      TYPE(AMatrix) A
      DOUBLE PRECISION bdursc
!     Variables Locales
!**********************
      INTEGER FOffseti
      INTEGER ICen
      INTEGER ICentral
      INTEGER IFiltCen
!
      DO IFiltCen = 1, FiltNCen
         ICentral = FiltEmbInd(IFiltCen)
         FOffseti = FOffset

!**************************
!     Demanda
!**************************
         FOffseti = FOffseti + NBarra
!**************************
!     Balance Hidro Embalse
!**************************
         DO ICen = 1, NCenEmb
            IF (ICen .EQ. ICentral) THEN
               CALL Am_set(A, COffset + IFiltCen, FOffseti + ICen, bdursc)
            ELSE
               IF (CenFHid(IFiltCen) .EQ. ICen) THEN
                  CALL Am_set(A, COffset + IFiltCen, FOffseti + ICen, -bdursc)
               ENDIF
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenEmb
!**************************
!     Balance Hidro Pasada
!*************************
         DO ICen = 1, NCenSer
            IF (CenFHid(IFiltCen) .EQ. ICen + NCenEmb) THEN
               CALL Am_set(A, COffset + IFiltCen, FOffseti + ICen, -bdursc)
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenSer
      ENDDO     
      RETURN
      END



!**********************************
!     Matriz FO y Limites Centrales
!**********************************
      SUBROUTINE GenPDFilFO(FiltNCen,                                   &
     &     COffset,                                                     &
     &     PDNCol, FO, LowBnd, UppBnd)
      USE OSI
!     Variables Globales
!***********************
      INTEGER PDNCol
      INTEGER FiltNCen
      INTEGER COffset
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!**********************
      INTEGER IFiltCen

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      DO IFiltCen = 1, FiltNCen
!     Funcion Objetivo
!*********************
         FO(COffset + IFiltCen) = 0.0d0
!     Restricciones de tipo 'x <= ' y 'x >= '
!********************************************
         LowBnd(COffset + IFiltCen) = 0.0d0
         UppBnd(COffset + IFiltCen) = DINFTY 
      ENDDO

      RETURN
      END
