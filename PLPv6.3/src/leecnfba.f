      SUBROUTINE LeeCnfBarDim(ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcBar

      INTEGER NBarra
      INTEGER ULog
      TYPE(PAR_DIMS) Dim


      INTEGER Abrir
      CHARACTER*12 AuxVar
      INTEGER URead

      URead = Abrir(NArcBar, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecnfba: Error, no existe archivo ',        &
     &        NArcBar, '.'
         WRITE(ULog, '(3A)') 'leecnfba: Error, no existe archivo ',     &
     &        NArcBar, '.'
         STOP 1
      ENDIF
!     Numero de Barras
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NBarra
      Dim%Bar = NBarra

      CALL Cerrar(URead)

      RETURN
      END

!********************
!     Subrutina Lee Barras
!********************
      SUBROUTINE LeeCnfBar(NBarra, BarNom, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcBar

      TYPE(PAR_DIMS), INTENT(IN) :: Dim


      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 BarNom(Dim%Bar)
      INTEGER IBar
      INTEGER NBar
      INTEGER NBarra
      INTEGER ULog
      INTEGER URead
!     codigo:
!************************
!     Lee datos pcpbar.dat
!************************
      URead = Abrir(NArcBar, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecnfba: Error, no existe archivo ',        &
     &        NArcBar, '.'
         WRITE(ULog, '(3A)') 'leecnfba: Error, no existe archivo ',     &
     &        NArcBar, '.'
         STOP 1
      ENDIF
!     Numero de Barras
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NBarra
!     Verifica Dimensiones
!********************
      IF (NBarra .GT. Dim%Bar) THEN
         WRITE(6, '(A, I4, A)') 'leecnfba: Numero de barras >',         &
     &        Dim%Bar, '.'
         WRITE(ULog, '(A, I4, A)') 'leecnfba: Numero de barras >',      &
     &        Dim%Bar, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      DO IBar = 1, NBarra
         READ(URead, *) NBar, BarNom(IBar)
      ENDDO
      CALL Cerrar(URead)
      RETURN
      END


