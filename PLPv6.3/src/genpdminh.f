!*********************************
!     Matriz Invariante A MinHalse de Embalses
!*********************************
      SUBROUTINE GenPDMinHA(NEmbVMinH, EmbVMinHInd, EmbCMinH,           &
     &     CenInd,                                                      &
     &     COffset, VOffset,                                            &
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

      INTEGER CenInd(Dim%Cen)
      TYPE(AMatrix) A

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbCMinH(Dim%Emb)
!     Variables Locales
!*****************
      INTEGER VOffseti
      INTEGER IMinH
      INTEGER IEmb

      DO IMinH = 1, NEmbVMinH
         IEmb = EmbVMinHInd(IMinH)

         CALL Num2Char(CenInd(IEmb), CVol, No, DimLargo)
         Nombre = fconcat('vmh', CVol)
         PDNombre(COffset + IMinH) = Nombre

         VOffseti = VOffset

!********************
!     Balance Vertimientos de MinHalse Embalse
!********************         
         ! volumen de embalse <= que el vmin
         IF (EmbCMinH(IEmb) .gt. 0) THEN                
            CALL Am_set(A, COffset + IMinH, VOffseti + IMinH, -1.0d0)
         ENDIF

         Sentido(VOffseti + IMinH) = 'L'
      ENDDO
      RETURN
      END
!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDMinHFO(NEmb, NEmbVMinH, EmbVMinHInd, EmbCMinH,    &
     &     ScaleVol,                                                    &
     &     FPhi,                                                        &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE OSI

!     Variables Globales
!******************
      INTEGER NEmb
      INTEGER COffset
      INTEGER PDNCol
      INTEGER NEmbVMinH

      INTEGER EmbVMinHInd(NEmbVMinH)
      DOUBLE PRECISION EmbCMinH(NEmb)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      DOUBLE PRECISION ScaleVol(NEmb)
!     Variables Locales
!*****************
      INTEGER IEmb
      INTEGER IMinH

      DOUBLE PRECISION FVol
      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      DO IMinH = 1, NEmbVMinH
!     Funcion Objetivo
!****************
         IEMb = EmbVMinHInd(IMinH)
         FVol = ScaleVol(IEmb) / FPhi
         
         IF (EmbCMinH(IEmb) .gt. 0) then
            FO(COffset + IMinH) = EmbCMinH(IEmb) * FVol

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
            UppBnd(COffset + IMinH) = DINFTY
            LowBnd(COffset + IMinH) = 0.0d0
         ELSE
            UppBnd(COffset + IMinH) = 0.0d0
            LowBnd(COffset + IMinH) = 0.0d0
         ENDIF
      ENDDO
      RETURN
      END
