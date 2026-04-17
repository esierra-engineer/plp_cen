!**********************************
!>    Matriz Invariante A Centrales
!**********************************
      SUBROUTINE GenPDCenA(IBloque, NBarra, NCenEmb, NCenSer, NCenPas,  &
     &     NCentral, CenInd, CenRen, CenGHid, CenGBar,                  &
     &     bdursc,                                                      &
     &     COffset, FOffset,                                            &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Variables Globales
!***********************
      INTEGER PDNCol
      CHARACTER*(DimLargo) CCentral
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre(PDNCol)
      CHARACTER*80 fconcat
      INTEGER CenInd(Dim%Cen)
      INTEGER CenGBar(Dim%Cen)
      INTEGER CenGHid(Dim%Cen, 2)
      INTEGER COffset
      INTEGER FOffset
      INTEGER IBloque
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NCenPas
      INTEGER NCentral
      TYPE(AMatrix) A
      DOUBLE PRECISION bdursc
      DOUBLE PRECISION CenRen(Dim%Cen)
!     Variables Locales
!**********************
      INTEGER FOffseti
      INTEGER IBar
      INTEGER ICen
      INTEGER ICentral
      INTEGER NCenHid
!
      NCenHid = NCenEmb + NCenSer + NCenPas
      DO ICentral = 1, NCentral

         IF (ICentral .LE. NCenHid) THEN
            Nombre = 'qg'
         ELSE
            Nombre = 'g'
         ENDIF

         CALL Num2Char(CenInd(ICentral), CCentral, No, DimLargo)
         Nombre = fconcat(Nombre, CCentral)

         Nombre = fconcat(Nombre, '_')
         CALL Num2Char(IBloque, CCentral, No, DimLargo)
         Nombre = fconcat(Nombre, CCentral)

         PDNombre(COffset + ICentral) = Nombre
         FOffseti = FOffset


!**************************
!     Demanda
!**************************
         DO IBar = 1, NBarra
            IF (IBar .EQ. CenGBar(ICentral)) THEN
               CALL Am_set(A, COffset + ICentral, FOffseti + IBar, CenRen(ICentral))
            ENDIF
         ENDDO
         FOffseti = FOffseti + NBarra
!**************************
!     Balance Hidro Embalse
!**************************
         DO ICen = 1, NCenEmb
            IF (ICen .EQ. ICentral) THEN
               CALL Am_set(A, COffset + ICentral, FOffseti + ICen, bdursc)
            ELSE
               IF (                                                     &
     &              (CenGHid(ICentral, 1) .EQ. ICen) .OR.               &
     &              (CenGHid(ICentral, 2) .EQ. ICen)                    &
     &              ) THEN
                  CALL Am_set(A, COffset + ICentral, FOffseti + ICen, -bdursc)
               ENDIF
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenEmb
!**************************
!     Balance Hidro Serie
!*************************
         DO ICen = 1, NCenSer
            IF (ICentral .EQ. ICen + NCenEmb) THEN
               CALL Am_set(A, COffset + ICentral, FOffseti + ICen, bdursc)
            ELSE
               IF (                                                     &
     &              (CenGHid(ICentral, 1) .EQ. ICen + NCenEmb) .OR.     &
     &              (CenGHid(ICentral, 2) .EQ. ICen + NCenEmb)          &
     &              ) THEN
                  CALL Am_set(A, COffset + ICentral, FOffseti + ICen,-bdursc)
               ENDIF
            ENDIF
         ENDDO
         FOffseti = FOffseti + NCenSer

      ENDDO

!***************
!     Offsets Finales
!***************
      COffset = COffset + NCentral

      RETURN
      END

!**********************************
!     Matriz FO y Limites Centrales
!**********************************
      SUBROUTINE GenPDCenFO(NCentral,                                   &
     &     CenCVar, CenRen, CenPMin, CenPMax,                           &
     &     BloDur, FPhi, COffset,                                       &
     &     PDNCol, FO, LowBnd, UppBnd)

!     Variables Globales
!***********************
      INTEGER PDNCol
      INTEGER NCentral
      INTEGER COffset
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION CenCVar(NCentral)
      DOUBLE PRECISION CenRen(NCentral)
      DOUBLE PRECISION CenPMin(NCentral)
      DOUBLE PRECISION CenPMax(NCentral)
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
!     Variables Locales
!**********************
      INTEGER ICentral
      DO ICentral = 1, NCentral
!     Funcion Objetivo
!*********************
         FO(COffset + ICentral) = CenCVar(ICentral)*CenRen(ICentral)  &
     &        *BloDur/FPhi
!     Restricciones de tipo 'x <= ' y 'x >= '
!********************************************
         LowBnd(COffset + ICentral) = CenPMin(ICentral)/CenRen(ICentral)
         UppBnd(COffset + ICentral) = CenPMax(ICentral)/CenRen(ICentral)
      ENDDO

      RETURN
      END
