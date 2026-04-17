!***************************************
!     Matriz Invariante A Angulos Barras
!***************************************
      SUBROUTINE GenPDAngA(IBloque, NBarra, NLinea, NCenEmb, NCenSer,   &
     &     NAng, ScaleAng, LinFOpe, LinNBar, LinXImp, LinVNom, LinHVDC, &
     &     COffset, FOffset,                                            &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim


!     Variables Globales
!***********************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre(PDNCol)
      CHARACTER*(DimLargo) CTeta
      CHARACTER*80 fconcat
      INTEGER IBloque
      INTEGER NBarra
      INTEGER NAng
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NLinea
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER COffset
      INTEGER FOffset
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION LinXImp(NLinea)
      DOUBLE PRECISION LinVNom(NLinea)
      TYPE(AMatrix) A
      LOGICAL LinFOpe(NLinea)
      LOGICAL LinHVDC(NLinea)
!     Variables Locales
!**********************
      INTEGER FOffseti
      INTEGER ILin
      INTEGER IAng

      DOUBLE PRECISION Aij

      DO IAng = 1, NAng

         Nombre = 'th'
         CALL Num2Char(IAng, CTeta, No, DimLargo)
         Nombre = fconcat(Nombre, CTeta)

         Nombre = fconcat(Nombre, '_')
         CALL Num2Char(IBloque, CTeta, No, DimLargo)
         Nombre = fconcat(Nombre, CTeta)

         PDNombre(COffset + IAng) = Nombre
         FOffseti = FOffset

!     Demanda
!************
         FOffseti = FOffseti + NBarra

!     Balance Caudal Embalse
!**************************
         FOffseti = FOffseti + NCenEmb

!     Balance Hidro Pasada
!*************************
         FOffseti = FOffseti + NCenSer

!     Flujo DC
!*************
         DO ILin = 1, NLinea
            IF (LinFOpe(ILin)) THEN
               IF (LinHVDC(ILin)) THEN
                  IF (IAng .EQ. LinNBar(1, ILin)) THEN
                     Aij = 0.0d0
                     CALL Am_set(A, COffset + IAng, FOffseti + ILin, Aij)
                  ENDIF
                  IF (IAng .EQ. LinNBar(2, ILin)) THEN
                     Aij = 0.0d0
                     CALL Am_set(A, COffset + IAng, FOffseti + ILin, Aij)
                  ENDIF
               ELSE
                  IF (IAng .EQ. LinNBar(1, ILin)) THEN
                     Aij = - 1.d0/(LinXImp(ILin)/LinVNom(ILin)/          &
     &                    LinVNom(ILin))/ScaleAng
                     CALL Am_set(A, COffset + IAng, FOffseti + ILin, Aij)
                  ENDIF
                  IF (IAng .EQ. LinNBar(2, ILin)) THEN
                     Aij = + 1.d0/(LinXImp(ILin)/LinVNom(ILin)/          &
     &                    LinVNom(ILin))/ScaleAng
                     CALL Am_set(A, COffset + IAng, FOffseti + ILin, Aij)
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
         FOffseti = FOffseti + NLinea

      ENDDO

!***************
!     Offsets Finales
!***************
      COffset = COffset + NAng

      RETURN
      END
!     FO Y Limites A Angulos Barras
!**********************************
      SUBROUTINE GenPDAngFO(NAng, FAngZero,                             &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE OSI

!     Variables Globales
!***********************
      INTEGER PDNCol
      INTEGER NAng
      INTEGER COffset
      LOGICAL FAngZero
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

!     Variables Locales
!**********************
      INTEGER IAng
      DOUBLE PRECISION lb
      DOUBLE PRECISION ub

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      IF (FAngZero) THEN
         lb = -DINFTY
      ELSE
         lb = 0.0d0
      ENDIF
      ub = DINFTY

      DO IAng = 1, NAng
!     Funcion Objetivo
!*********************
         FO(COffset + IAng) = 0.d0

!     Restricciones de tipo 'x <= ' y 'x >= '
!********************************************
         LowBnd(COffset + IAng) = lb
         UppBnd(COffset + IAng) = ub
      ENDDO

      IF (FAngZero) THEN
         LowBnd(COffset + 1) = 0
         UppBnd(COffset + 1) = 0
      ENDIF
      RETURN
      END
