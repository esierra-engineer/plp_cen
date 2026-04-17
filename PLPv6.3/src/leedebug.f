!******************
!     Subrutina LeeDebug
!******************
      SUBROUTINE LeeDebug(FLog, PriProgDin,                             &
     &     PDSvFl, ErSvFl, PsFzFl, PMSvFl, FSvLaPs,                     &
     &     FDatChe, IndEta1Imp, IndEta2Imp, IndSimImp, IndIteImp, FBest,&
     &     FInterfaz, ULog)
      INCLUDE 'pxp.fpp'
!     parametros y variables locales:
      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead
      INTEGER ULog
      INTEGER IndEta1Imp
      INTEGER IndEta2Imp
      INTEGER IndIteImp
      INTEGER IndSimImp
      LOGICAL FBest
      LOGICAL FLog
      LOGICAL FWarning
      LOGICAL PDSvFl
      LOGICAL FSvLaPs
      LOGICAL ErSvFl
      LOGICAL PsFzFl
      LOGICAL FTSvFl
      LOGICAL PMSvFl
      LOGICAL PriProgDin
      LOGICAL FDatChe
      INTEGER FInterfaz
      
      EXTERNAL Abrir
      FWarning = .FALSE.
      URead = Abrir(NArcDeb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leedebug: Error, no existe archivo ',        &
     &        NArcDeb, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FLog
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PriProgDin, PDSvFl, PMSvFl, FDatChe,               &
     &     ErSvFl, PsFzFl, FTSvFl, FSvLaPs
!     Valor por defecto de FBest
      FBest = .FALSE.
      READ(URead, '(A1)') AuxVar
      READ(URead, *) IndSimImp,                                         &
     &     IndIteImp, FBest, IndEta1Imp, IndEta2Imp
      IF (FBest) THEN
!     FBest = true puede implicar fuertes variaciones de la
!     SPFO de una iteracion a otra.
         FBest = .FALSE.
         WRITE(6, '(A, L1, A)') 'leedeb: FBest := ',                    &
     &        FBest, '.'
         FWarning = .TRUE.
      ENDIF
      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leedeb: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN         
            WRITE(6, '(A)') 'leedeb: Hay warnings.'
            WRITE(6, '(A)') 'leedeb: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      CALL Cerrar(URead)
      RETURN
      END
