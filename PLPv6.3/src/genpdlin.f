!*************
!     Flujos Lineas
!*************
      SUBROUTINE GenPDLinA(IBloque, NBarra, NLinea, NCenEmb, NCenSer,   &
     &     NFlujo, LinFOpe, LinNBar, LinNFlu, LinHVDC,                  &
     &     LinTMax, LinRes, LinVNom,                                    &
     &     LinFPer, FPerdTram, PerdEms, PerdRec,                        &
     &     COffset, FOffset, A, PDNCol, PDNombre, Dim)
      
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!******************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*(DimLargo) CBase
      CHARACTER*(DimLargo) CLin
      CHARACTER*(DimLargo) CFlu
      CHARACTER*80 fconcat
      INTEGER IBloque
      INTEGER NBarra
      INTEGER NLinea
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NFlujo
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(NLinea)
      INTEGER COffset
      INTEGER FOffset
      TYPE(AMatrix) A
      DOUBLE PRECISION PerdEms
      DOUBLE PRECISION PerdRec
      DOUBLE PRECISION LinRes(NLinea)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin)
      DOUBLE PRECISION LinVNom(NLinea)
      DOUBLE PRECISION LinRImp

      LOGICAL FPerdTram
      LOGICAL LinFOpe(NLinea)
      LOGICAL LinFPer(NLinea)
      LOGICAL LinHVDC(NLinea)

!     Variables Locales
!*****************
      INTEGER FOffseti
      INTEGER IBar
      INTEGER IColN
      INTEGER IColP
      INTEGER IFlu
      INTEGER ILin
      INTEGER ILinAux

      DOUBLE PRECISION ASumaMas
      DOUBLE PRECISION ASumaMenos
      DOUBLE PRECISION ASumaP
      DOUBLE PRECISION ASumaN
      DOUBLE PRECISION ASumaFactorP1
      DOUBLE PRECISION ASumaFactorP2
      DOUBLE PRECISION ASumaFactorN1
      DOUBLE PRECISION ASumaFactorN2

      INTEGER NB
      DOUBLE PRECISION Aij
!      
      IColP = 0
      CALL Num2Char(IBloque, CBase, No, DimLargo)


      DO ILin = 1, NLinea

         CALL Num2Char(ILin, CLin, No, DimLargo)
         DO IFlu = 1, LinNFlu(ILin)
            IColP = IColP + 1
            IColN = IColP + LinNFlu(ILin)

            CALL Num2Char(IFlu, CFlu, No, DimLargo)

            Nombre = 'l'
            Nombre = fconcat(Nombre, CLin)
            Nombre = fconcat(Nombre, 'fp')
            Nombre = fconcat(Nombre, CFlu)
            Nombre = fconcat(Nombre, '_')
            Nombre = fconcat(Nombre, CBase)
            PDNombre(COffset + IColP) = Nombre

            Nombre = 'l'
            Nombre = fconcat(Nombre, CLin)
            Nombre = fconcat(Nombre, 'fn')
            Nombre = fconcat(Nombre, CFlu)
            Nombre = fconcat(Nombre, '_')
            Nombre = fconcat(Nombre, CBase)
            PDNombre(COffset + IColN) = Nombre

            FOffseti = FOffset
!*********
!     Demanda
!*********

            ASumaFactorP1 = 0.0d0
            ASumaFactorN1 = 0.0d0            
            IF (FPerdTram .AND. LinFPer(ILin)) THEN
               LinRImp = LinRes(ILin)*dble(2*IFlu - 1)
               ASumaFactorP1 =                                          &
     &              LinTMax(1, IFlu, ILin)*                             &
     &              LinRImp/LinVNom(ILin)/                              &
     &              LinVNom(ILin)
               ASumaFactorN1 =                                          &
     &              LinTMax(2, IFlu, ILin)*                             &
     &              LinRImp/LinVNom(ILin)/                              &
     &              LinVNom(ILin)
            ENDIF
            IF (LinFOpe(ILin)) THEN
               ASumaFactorP2 = 1.0d0
               ASumaFactorN2 = 1.0d0
               NB = NBarra
            ELSE
               ASumaFactorP2 = 0.0d0
               ASumaFactorN2 = 0.0d0
               NB = 0
            ENDIF

            DO IBar = 1, NB
!     Extremo Inyector
               ASumaP = 0d0
               ASumaN = 0d0
               IF (IBar .EQ. LinNBar(1, ILin)) THEN
                  ASumaP = -1.0d0
                  ASumaN = +1.0d0
               ENDIF
!     Extremo Demandador
               IF (IBar .EQ. LinNBar(2, ILin)) THEN
                  ASumaP = +1.0d0
                  ASumaN = -1.0d0
               ENDIF

               ASumaMas = MAX(ASumaP, 0.0d0)
               ASumaMenos = MAX( - ASumaP, 0.0d0)

               Aij =                                                    &
     &              (ASumaP -                                           &
     &              (PerdRec*ASumaMas + PerdEms*ASumaMenos)*            &
     &              ASumaFactorP1)*                                     &
     &              ASumaFactorP2

               IF (Aij .ne. 0.0d0) THEN
                  CALL Am_set(A, COffset + IColP, FOffseti + IBar, Aij)
               ENDIF

               ASumaMas = MAX(ASumaN, 0.0d0)
               ASumaMenos = MAX( - ASumaN, 0.0d0)
               
               Aij =                                                    &
     &              (ASumaN -                                           &
     &              (PerdRec*ASumaMas + PerdEms*ASumaMenos)*            &
     &              ASumaFactorN1)*                                     &
     &              ASumaFactorN2

               IF (Aij .ne. 0.0d0) THEN
                  CALL Am_set(A, COffset + IColN, FOffseti + IBar, Aij)
               ENDIF

            ENDDO

            FOffseti = FOffseti + NBarra

!********************
!     Balance Caudal Embalse
!********************
            FOffseti = FOffseti + NCenEmb

!********************
!     Balance Caudal Pasada
!********************
            FOffseti = FOffseti + NCenSer

!****************
!     Flujo DC, Lineas
!****************
            DO ILinAux = 1, NLinea
               IF ((ILinAux .EQ. ILin) .AND. LinFOpe(ILin)) THEN
                  IF (LinHVDC(ILin)) THEN
                     CALL Am_set(A, COffset + IColP, FOffseti + ILinAux, 0.0d0)
                     
                     CALL Am_set(A, COffset + IColN, FOffseti + ILinAux, 0.0d0)
                  ELSE
                     CALL Am_set(A, COffset + IColP, FOffseti + ILinAux, +1.d0)

                     CALL Am_set(A, COffset + IColN, FOffseti + ILinAux, -1.d0)
                  ENDIF
               ENDIF
            ENDDO
            FOffseti = FOffseti + NLinea

         ENDDO
         IColP = IColP + LinNFlu(ILin)
      ENDDO

!***************
!     Offsets Finales
!***************
      COffset = COffset + 2*NFlujo

      RETURN
      END

!***********************

      SUBROUTINE GenPDLinFO(NLinea,                                     &
     &     LinNFlu, LinFOpe,                                            &
     &     LinTMax, DLin, DFlu,                                         &
     &     CTrasmision,                                                 &
     &     FPhi, COffset,                                               &
     &     PDNCol, FO, LowBnd, UppBnd)
!
      INTEGER PDNCol
      INTEGER COffset
      INTEGER IColN
      INTEGER IColP
      INTEGER IFlu
      INTEGER ILin
      INTEGER NLinea
      INTEGER DLin, DFlu
      INTEGER LinNFlu(NLinea)
      LOGICAL LinFOpe(NLinea)
      DOUBLE PRECISION CTrasmision
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION LinTMax(2, DFlu, DLin)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      IColP = 0
      DO ILin = 1, NLinea
         DO IFlu = 1, LinNFlu(ILin)
            IColP = IColP + 1
            IColN = IColP + LinNFlu(ILin)
            IF (LinFOpe(ILin)) THEN
!***************
!     Linea Operativa
!***************
!****************
!     Funcion Objetivo
!****************
               FO(COffset + IColP) = CTrasmision/FPhi
               FO(COffset + IColN) = CTrasmision/FPhi
!***********************************
!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
               UppBnd(COffset + IColP) = LinTMax(1, IFlu, ILin)
               LowBnd(COffset + IColP) = 0.d0
               UppBnd(COffset + IColN) = LinTMax(2, IFlu, ILin)
               LowBnd(COffset + IColN) = 0.d0
            ELSE
!******************
!     Linea No Operativa
!******************
!****************
!     Funcion Objetivo
!****************
               FO(COffset + IColP) = 0.d0
               FO(COffset + IColN) = 0.d0
!***********************************
!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
               UppBnd(COffset + IColP) = 0.d0
               LowBnd(COffset + IColP) = 0.d0
               UppBnd(COffset + IColN) = 0.d0
               LowBnd(COffset + IColN) = 0.d0
            ENDIF
         ENDDO
         IColP = IColP + LinNFlu(ILin)
      ENDDO
      RETURN
      END
