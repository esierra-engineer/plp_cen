!*********************************
!     Matriz Invariante A Rebalse de Embalses
!*********************************
      SUBROUTINE GenPDRebA(NEmbVReb, EmbVRebInd,                        &
     &     NCenEmb, CenInd, edur,                                       &
     &     ScaleVol, COffset, VOffset,                                  &
     &     A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
!     Variables Globales
!******************
      INTEGER PDNCol, PDNFila
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*1 Sentido(PDNFila)

      CHARACTER*(DimLargo) CVol
      CHARACTER*80 fconcat
      INTEGER COffset
      INTEGER VOffset
      INTEGER NCenEmb

      INTEGER CenInd(Dim%Cen)
      DOUBLE PRECISION edur
      DOUBLE PRECISION ScaleVol(NCenEmb)
      TYPE(AMatrix) A

      INTEGER NEmbVReb
      INTEGER EmbVRebInd(NEmbVReb)
!     Variables Locales
!*****************
      INTEGER VOffseti
      INTEGER IVol
      INTEGER IReb
      INTEGER IColV
      INTEGER IColP
      INTEGER IColN
      INTEGER NVol
      INTEGER IEmb

      NVol = NCenEmb
      IColV = 0
      DO IReb = 1, NEmbVReb
         IEmb = EmbVRebInd(IReb)
         IColV = IReb
         IColP = IColV + NEmbVReb
         IColN = IColP + NEmbVReb

         CALL Num2Char(CenInd(IEmb), CVol, No, DimLargo)
         Nombre = fconcat('qrb', CVol)
         PDNombre(COffset + IColv) = Nombre
         Nombre = fconcat('vrbp', CVol)
         PDNombre(COffset + IColp) = Nombre
         Nombre = fconcat('vrbn', CVol)
         PDNombre(COffset + IColn) = Nombre

         VOffseti = VOffset
!********************
!     Balance Volumen Embalse
!********************
         VOffseti = VOffseti 
         DO IVol = 1, NVol
            IF (IVol .EQ. IEmb) THEN
               CALL Am_set(A, COffset + IColV, VOffseti + IVol, edur/ScaleVol(IVol))
            ENDIF
         ENDDO

         VOffseti = VOffseti + 2*NVol

!********************
!     Balance Vertimientos de Rebalse Embalse
!********************         
        ! volumenes +/- de rebalse respecto de cota de rebalse
         DO IVol = 1, NVol
            IF (IVol .EQ. IEmb) THEN
               CALL Am_set(A, COffset + IColP, VOffseti + IReb, -1.0d0)

               CALL Am_set(A, COffset + IColN, VOffseti + IReb, 1.0d0)
            ENDIF
         ENDDO
         VOffseti = VOffseti + NEmbVReb
         ! volumen de vertimiento <= que el rebalse positivo
         DO IVol = 1, NVol
            IF (IVol .EQ. IEmb) THEN
               CALL Am_set(A, COffset + IColP, VOffseti + IReb, -1.0d0)

               CALL Am_set(A, COffset + IColV, VOffseti + IReb,  edur/ScaleVol(IVol))

               Sentido(VOffseti + IReb) = 'L'
            ENDIF
         ENDDO
      ENDDO
      RETURN
      END
!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDRebFO(NEmb, NEmbVReb, EmbVRebInd,                 &
     &     ScaleVol,                                                    &
     &     EmbCReb, edur, FPhi,                                         &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE OSI
!     Variables Globales
!******************
      INTEGER COffset
      INTEGER PDNCol
      INTEGER NEmb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(NEmbVReb)

      DOUBLE PRECISION EmbCReb(NEmb)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION edur
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      DOUBLE PRECISION ScaleVol(NEmb)
      DOUBLE PRECISION FCau
      DOUBLE PRECISION FVol
!     Variables Locales
!*****************
      INTEGER IReb
      INTEGER IEmb
      INTEGER IColV
      INTEGER IColP
      INTEGER IColN

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      FCau = edur / FPhi

      IColV = 0
      DO IReb = 1, NEmbVReb
         IEmb = EmbVRebInd(IReb)
         FVol = ScaleVol(IEmb) / FPhi
         IColV = IReb
         IColP = IColV + NEmbVReb
         IColN = IColP + NEmbVReb
!     Funcion Objetivo
!****************
         FO(COffset + IColV) = FCau * EmbCReb(IReb)
         FO(COffset + IColP) = 0.0d0
         FO(COffset + IColN) = 0.0d0

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
         UppBnd(COffset + IColV) = DINFTY
         LowBnd(COffset + IColV) = 0.0d0
         UppBnd(COffset + IColP) = DINFTY
         LowBnd(COffset + IColP) = 0.0d0
         UppBnd(COffset + IColN) = DINFTY
         Lowbnd(COffset + IColN) = 0.0d0
      ENDDO
      RETURN
      END
