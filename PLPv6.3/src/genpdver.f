!**************************************************
!     Matriz Invariante A Vertimiento Embalses y Pasadas
!**************************************************
      SUBROUTINE GenPDVerA(IBloque, NBarra, NCenEmb, NCenSer, CenInd,   &
     &     NVert, CenVHid, bdursc,                                      &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     CVolOffset, COffset, FOffset,                                &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INCLUDE 'machcons.fpp'
!     Variables Gloables
!******************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*(DimLargo) CVert
      CHARACTER*80 fconcat
      INTEGER NVert
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER CenInd(Dim%Cen)
      INTEGER CenVHid(Dim%Cen, 2)
      INTEGER COffset
      INTEGER FOffset
      INTEGER CVolOffset
      TYPE(AMatrix) A
      DOUBLE PRECISION bdursc
      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb
!     Variables Locales
!*****************
      INTEGER FOffseti
      INTEGER CVolOffseti
      INTEGER ICen
      INTEGER IBloque
      INTEGER IVert
      INTEGER IReb

      DO IVert = 1, NVert

         Nombre = 'qv'
         CALL Num2Char(CenInd(IVert), CVert, No, DimLargo)
         Nombre = fconcat(Nombre, CVert)

         Nombre = fconcat(Nombre, '_')
         CALL Num2Char(IBloque, CVert, No, DimLargo)
         Nombre = fconcat(Nombre, CVert)

         PDNombre(COffset + IVert) = Nombre
         FOffseti = FOffset

!*********
!     Demanda
!*********
         FOffseti = FOffseti + NBarra

!********************
!     Balance Caudal Embalse
!********************
         DO ICen = 1, NCenEmb
            IF (ICen .EQ. IVert) THEN
               CALL Am_set(A, COffset + IVert, FOffseti + ICen, bdursc )
            ELSE
               IF (                                                     &
     &              (CenVHid(IVert, 1) .EQ. ICen) .OR.                  &
     &              (CenVHid(IVert, 2) .EQ. ICen)                       &
     &              ) THEN
                  CALL Am_set(A, COffset + IVert, FOffseti + ICen, -bdursc )
               ENDIF
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenEmb

!********************
!     Balance Hidro Pasada
!********************
         DO ICen = 1, NCenSer
            IF(IVert .EQ. ICen + NCenEmb) THEN
               CALL Am_set(A, COffset + IVert, FOffseti + ICen, bdursc )
            ELSE
               IF(                                                      &
     &              (CenVHid(IVert, 1) .EQ. ICen + NCenEmb) .OR.        &
     &              (CenVHid(IVert, 2) .EQ. ICen + NCenEmb)             &
     &              ) THEN
                  CALL Am_set(A, COffset + IVert, FOffseti + ICen, -bdursc )
               ENDIF
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenSer

      ENDDO

!**************************
!     Rebalse de Embalse
!**************************
      IF (FVertReb) THEN
         CVolOffseti = CVolOffset
!        variables de volumen
         CVolOffseti = CVolOffseti + 2*NCenEmb
!        Volumen de vertimiento 
         DO IReb = 1, NEmbVReb
            IVert = EmbVRebInd(IReb)
            DO ICen = 1, NCenEmb + NCenSer           
               IF (ICen .NE. IVert) THEN
                  IF (                                                     &
     &                 (CenVHid(IVert, 1) .EQ. ICen) .OR.                  &
     &                 (CenVHid(IVert, 2) .EQ. ICen)                       &
     &                 ) THEN
                     CALL Am_set(A, CVolOffseti + IReb, FOffset + NBarra + ICen, -bdursc )
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDIF

      


!***************
!     Offsets Finales
!***************
      COffset = COffset + NVert

      RETURN
      END
!*******************************************
!     FO y Limites Vertimiento Embalses y Pasadas
!*******************************************
      SUBROUTINE GenPDVerFO(NVert,                                      &
     &     CVertimiento, CenVMin, CenVMax,                              &
     &     BloDur, FPhi, COffset,                                       &
     &     PDNCol, FO, LowBnd, UppBnd)

!     Variables Globales
!******************
      INTEGER PDNCol
      INTEGER NVert
      INTEGER COffset
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION CenVMax(NVert)
      DOUBLE PRECISION CenVMin(NVert)
      DOUBLE PRECISION CVertimiento
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!*****************
      INTEGER IVert
      DO IVert = 1, NVert
!     Funcion Objetivo
!****************
         FO(COffset + IVert) = CVertimiento*BloDur/FPhi
!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
         LowBnd(COffset + IVert) = CenVMin(IVert)
         UppBnd(COffset + IVert) = CenVMax(IVert)
      ENDDO
      RETURN
      END
