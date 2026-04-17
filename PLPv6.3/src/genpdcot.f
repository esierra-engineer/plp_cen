!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDVolA(NCenEmb, CenInd,                             &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH, EmbVMinHInd, EmbCMinH,                            &
     &     COffset, VOffset,                                            &
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
      LOGICAL FVertReb
      INTEGER NCenEmb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(NEmbVReb)
      INTEGER CenInd(Dim%Cen)      
      INTEGER NVol
      INTEGER COffset
      INTEGER VOffset
      TYPE(AMatrix) A

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbCMinH(Dim%Emb)
      INTEGER IMinH
      
!     Variables Locales
!*****************
      INTEGER VOffseti
      INTEGER ICen
      INTEGER IVol
      INTEGER EVol
      INTEGER IReb

      NVol = NCenEmb
      DO IVol = 1, NVol
         EVol = IVol + NVol
         CALL Num2Char(CenInd(IVol), CVol, No, DimLargo)
         Nombre = fconcat('vf', CVol)
         PDNombre(COffset + IVol) = Nombre
         Nombre = fconcat('ve', CVol)
         PDNombre(COffset + EVol) = Nombre

         VOffseti = VOffset

!********************
!     Balance Volumen Embalse
!********************

         DO ICen = 1, NCenEmb
            IF (ICen .EQ. IVol) THEN
               CALL Am_set(A, COffset + IVol, VOffseti + ICen, 1.d0)

               CALL Am_set(A, COffset + EVol, VOffseti + ICen, -1.d0)
            ENDIF
         ENDDO
         VOffseti = VOffseti + NCenEmb

!********************
!     Balance Caudales Volumen en cas 
!********************
         DO ICen = 1, NCenEmb
            IF (ICen .EQ. IVol) THEN
               CALL Am_set(A, COffset + EVol, VOffseti + ICen, 1.0d0)
            ENDIF
         ENDDO
         VOffseti = VOffseti + NCenEmb

!**************************
!     Rebalse de Embalse
!**************************
         IF (FVertReb) THEN
            DO IReb = 1, NEmbVReb
               ICen = EmbVRebInd(IReb)
               IF (ICen .EQ. IVol) THEN
                  CALL Am_set(A, COffset + EVol, VOffseti + IReb, 1.0d0)
               ENDIF
            ENDDO
            VOffseti = VOffseti + 2*NEmbVReb
         ENDIF

!**************************
!     Cotas minimas con holgura 
!**************************
         IF (NEmbVMinH .gt. 0) THEN
            DO IMinH = 1, NEmbVMinH
               ICen = EmbVMinHInd(IMinH)
               IF (EmbCMinH(ICen) .gt. 0) THEN                
                  IF (ICen .EQ. IVol) THEN
                     CALL Am_set(A, COffset + IVol, VOffseti + IMinH, -1.0d0)
                  ENDIF
               ENDIF
            ENDDO
            VOffseti = VOffseti + 2*NEmbVMinH
         ENDIF
         
      ENDDO
      RETURN
      END
!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDVolFO(NVol, ScaleVol,                             &
     &     EmbVMin, EmbVMax,                                            &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE OSI
!     Variables Globales
!******************
      INTEGER NVol
      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION ScaleVol(NVol)
      DOUBLE PRECISION EmbVMin(NVol)
      DOUBLE PRECISION EmbVMax(NVol)
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!*****************
      INTEGER IVol
      INTEGER EVol

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      DO IVol = 1, NVol
         EVol = IVol + NVol
!     Funcion Objetivo
!****************
         FO(COffset + IVol) = 0.d0
         FO(COffset + EVol) = 0.d0
!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
         UppBnd(COffset + IVol) = EmbVMax(IVol)/ScaleVol(IVol)
         LowBnd(COffset + IVol) = EmbVMin(IVol)/ScaleVol(IVol)
         UppBnd(COffset + EVol) = DINFTY
         LowBnd(COffset + EVol) = -DINFTY
      ENDDO
      RETURN
      END
