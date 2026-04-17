      SUBROUTINE LeeMatDim(ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcMat

      INTEGER ULog
      TYPE(PAR_DIMS) Dim


      INTEGER PDMaxIte

      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      CHARACTER*12 AuxVar

      URead = Abrir(NArcMat, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leemat: Error, no existe archivo ',          &
     &        NArcMat, '.'
         WRITE(ULog, '(3A)') 'leemat: Error, no existe archivo ',       &
     &        NArcMat, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PDMaxIte
      Dim%PDIter = PDMaxIte
      
      CALL Cerrar(URead)

      RETURN
      END


!****************
!     Subrutina LeeMat
!****************
      SUBROUTINE LeeMat(PDMaxIte, PDError, UmbIntConf,                  &
     &     PMError, Lambda, FVolFinEmb, FPreProc, CCaudFalla,           &
     &     CVertimiento, CTrasmision, FPrevia, FactTiempo,              &
     &     FSeparaFCF, FGrabaCSV, FGrabaRES,                            &
     &     FConvPGradx, FConvPVar, UmbGradX, UmbZSPF,                   &
     &     NumEtaCF, ABLMax, ABEpsilon,                                 &
     &     NEtapa, FInterfaz, ULog)
      USE PLP, ONLY : PAR_DIMS, NArcMat

!     comun a todas las rutinas:
!     parametros y variables locales:
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      INTEGER ABLMax
      INTEGER NEtapa
      INTEGER NumEtaCF
      INTEGER PDMaxIte
      INTEGER PMMaxIte
      INTEGER ULog
      INTEGER URead
      LOGICAL FConvPGradX
      LOGICAL FConvPVar
      LOGICAL FFixTrasm
      LOGICAL FGrabaCSV
      LOGICAL FGrabaRES
      LOGICAL FSeparaFCF
      LOGICAL FVolFinEmb
      LOGICAL FPreProc
      LOGICAL FPrevia
      LOGICAL FWarning
      INTEGER FInterfaz
      DOUBLE PRECISION UmbGradX
      DOUBLE PRECISION UmbZSPF
      DOUBLE PRECISION ABEpsilon
      DOUBLE PRECISION CCaudFalla
      DOUBLE PRECISION CInter
!     hay que borrar*CTasa*
      DOUBLE PRECISION CTasa
!
      DOUBLE PRECISION CTrasmision
      DOUBLE PRECISION CVertimiento
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION Lambda
      DOUBLE PRECISION PDError
      DOUBLE PRECISION PMError
      DOUBLE PRECISION UmbIntConf
!     codigo:
!************************
!     Lee datos plpmat.dat
!************************
      FWarning = .FALSE.
      URead = Abrir(NArcMat, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leemat: Error, no existe archivo ',          &
     &        NArcMat, '.'
         WRITE(ULog, '(3A)') 'leemat: Error, no existe archivo ',       &
     &        NArcMat, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PDMaxIte, PDError, UmbIntConf

!     MaxNPlanos = DimPDPlanos

      READ(URead, '(A1)') AuxVar
      READ(URead, *) PMMaxIte, PMError
      READ(URead, '(A1)') AuxVar
      READ(URead, *) Lambda, CTasa, CCaudFalla, CVertimiento,           &
     &     CInter, CTrasmision, FVolFinEmb, FPreProc, FPrevia
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FFixTrasm, FSeparaFCF, FGrabaCSV, FGrabaRES
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ABLMax, ABEpsilon, NumEtaCF
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FConvPGradx, FConvPVar, UmbGradX, UmbZSPF

!     Cambio de unidades
!******************      
      CCaudFalla = CCaudFalla*FactTiempo
      CVertimiento = CVertimiento*FactTiempo
      CTrasmision = CTrasmision*FactTiempo
      CALL Cerrar(URead)
!****************
!     chequeo de datos
!****************
      IF (                                                              &
     &     (FConvPGradx) .AND.                                          &
     &     (NumEtaCF .GE. NEtapa)                                      &
     &     ) THEN
         WRITE(6, '(A, 2(I3, A))') 'leemat: NumEtaCF = ', NumEtaCF,     &
     &        ' >= NEtapa = ',  NEtapa, '.'
         WRITE(6, '(A)') 'leemat: FConvPGradx :=  FALSE'
         WRITE(ULog, '(A, 2(I3, A))') 'leemat: NumEtaCF = ', NumEtaCF,  &
     &        ' >= NEtapa = ',  NEtapa, '.'
         WRITE(ULog, '(A)') 'leemat: FConvPGradx :=  FALSE'
         FConvPGradx =  .FALSE.
         FWarning = .TRUE.
      ENDIF
      IF(FWarning) THEN
         WRITE(ULog, '(A)') 'leemat: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
         WRITE(6, '(A)') 'leemat: Hay warnings.'
            WRITE(6, '(A)') 'leemat: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF
      PDMaxIte = max0(PDMaxIte, 0)
      PMMaxIte = max0(PMMaxIte, 0)
      Lambda = MAX(Lambda, 0.d0)
      CCaudFalla = MAX(CCaudFalla, 0.d0)
      CVertimiento = MAX(CVertimiento, 0.d0)
      CInter = MAX(CInter, 0.d0)
      CTrasmision = MAX(CTrasmision, 0.d0)
!     El codigo asociado a este flag todavia esta en estudio
      FFixTrasm = .FALSE.
!
      RETURN
      END
