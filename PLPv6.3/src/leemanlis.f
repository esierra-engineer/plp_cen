      SUBROUTINE LeeManLinSDim(Dim, ULog)

      USE PLP, ONLY : PAR_DIMS, NArcManLin3

      TYPE(PAR_DIMS) Dim
      INTEGER ULog

      CHARACTER*12 AuxVar
      INTEGER Abrir
      INTEGER URead
      CHARACTER*48 NomLin


      INTEGER BloIni
      INTEGER BloFin
      DOUBLE PRECISION VNomLin
      DOUBLE PRECISION ResLin
      DOUBLE PRECISION XImpLin
      DOUBLE PRECISION ManALin
      DOUBLE PRECISION ManBLin
      LOGICAL FOpeLin
      INTEGER ISim, CSimul

      INTEGER LinManSInd(Dim%Simul)

      LinManSInd(1:Dim%Simul) = 1

      Dim%LinManS = 1

      URead = Abrir(NArcManLin3, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         RETURN
      ENDIF

      READ(URead, *, END=1000) AuxVar
      CSimul = 1
      DO WHILE (.TRUE.)
         READ(URead, *, END=1000) NomLin, BloIni, BloFin,               &
     &        ManALin, ManBLin,                                         &
     &        VNomLin, ResLin, XImpLin, FOpeLin, ISim

         IF (LinManSInd(ISim) .EQ. 1) THEN
            CSimul = CSimul + 1
            LinManSInd(ISim) = CSimul
         ENDIF
      ENDDO

      Dim%LinManS = CSimul

 1000 CALL Cerrar(URead)


      END

!**********************************
!     Subrutina Lee Mantenimiento Lineas
!**********************************
      SUBROUTINE LeeManLinS(FDatChe, NBloques, NSimul, NLinea,          &
     &     LinNom, LinNFlu,                                             &
     &     LinFOpe, LinRes, LinTMax, LinVNom, LinXImp,                  &
     &     LinTMaxS, LinManSInd, FSeparaLP, Finterfaz, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcManLin3

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
      INCLUDE 'machcons.fpp'
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 LinNom(Dim%Lin)
      CHARACTER*48 NomLin
      INTEGER BloIni
      INTEGER BloFin
      INTEGER IFlu
      INTEGER ILin
      INTEGER ISim
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBloques
      INTEGER NLin
      INTEGER NLinea
      INTEGER NSimul
      INTEGER NumBlo
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FOpeLin
      LOGICAL FStop
      LOGICAL FWarning
      LOGICAL FSeparaLP
      INTEGER FInterfaz

      INTEGER LinManSInd(Dim%Simul)

      LOGICAL LinFOpe(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinXImp(Dim%Lin, Dim%Blo, Dim%LinManS)

      DOUBLE PRECISION LinTMaxS(2, Dim%Flu, Dim%Lin, Dim%Blo)

      DOUBLE PRECISION VNomLin
      DOUBLE PRECISION ResLin
      DOUBLE PRECISION XImpLin
      DOUBLE PRECISION ManALin
      DOUBLE PRECISION ManBLin

!     
      INTEGER IBlo
      INTEGER CSimul, IISim
      INTEGER NFlujo

      LinManSInd(1:NSimul) = 1

      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManLin3, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         DO ILin = 1, NLinea
            LinTMaxS(1:2, 1:LinNFlu(ILin), ILin, 1:NBloques)  = &
     &           LinTMax(1:2, 1:LinNFlu(ILin), ILin, 1:NBloques, 1)
         ENDDO

         RETURN
      ENDIF

      IF (.NOT. FSeparaLP) THEN
         FSeparaLP = .TRUE.
         WRITE(6, '(3A)') 'leemanlins: Info, existe archivo ',          &
     &        NArcManLin3, ', activando modo FSeparaLP.'
         WRITE(ULog, '(3A)') 'leemanlins: Info, existe archivo ',       &
     &        NArcManLin3, ', activando modo FSeparaLP.'
      ENDIF

      DO ISim = 2, Dim%LinManS
         LinFOpe(1:NLinea, 1:NBloques, ISim) = LinFOpe(1:NLinea, 1:NBloques, 1) 
         LinRes(1:NLinea, 1:NBloques, ISim)  = LinRes(1:NLinea, 1:NBloques, 1)
         LinVNom(1:NLinea, 1:NBloques, ISim) = LinVNom(1:NLinea, 1:NBloques, 1)
         LinXImp(1:NLinea, 1:NBloques, ISim) = LinXImp(1:NLinea, 1:NBloques, 1)

         DO ILin = 1, NLinea
            LinTMax(1:2, 1:LinNFlu(ILin), ILin, 1:NBloques, ISim)  =   &
     &           LinTMax(1:2, 1:LinNFlu(ILin), ILin, 1:NBloques, 1)
         ENDDO

      ENDDO

 
      READ(URead, *, END=100) AuxVar
      CSimul = 1
      DO WHILE (.TRUE.)
         NLin = 0
         READ(URead, *, END=100) NomLin, BloIni, BloFin,                &
     &        ManALin, ManBLin,                                         &
     &        VNomLin, ResLin, XImpLin, FOpeLin, ISim
         IF ((ISim .LT. 1) .OR. (ISim .GT. NSimul)) THEN
            IF (FDatChe) THEN
               WRITE(6, '(3A)')                                         &
     &              'leemanlin: Error en datos linea ', NomLin, '.'
               WRITE(6, '(2(A, I4), A)')                                &
     &              'leemanlin: Numero de simulacion fuera de ',        &
     &              'rango: 1<', ISim, '<', NSimul, '.'
               WRITE(ULog, '(3A)')                                      &
     &              'leemanlin: Error en datos linea ', NomLin, '.'
               WRITE(ULog, '(2(A, I4), A)')                             &
     &              'leemanlin: Numero de simulacion fuera de ',        &
     &              'rango: 1<', ISim, '<', NSimul, '.'
               FStop = .TRUE.
               EXIT
            ENDIF
         ENDIF

         IF (LinManSInd(ISim) .EQ. 1) THEN
            CSimul = CSimul + 1
            LinManSInd(ISim) = CSimul
         ENDIF
         
         ILin = 1
         DO WHILE ((NLin .EQ. 0) .AND. (ILin .LE. NLinea))
            IF (LinNom(ILin) .EQ. NomLin) THEN
               NLin = ILin
            ENDIF
            ILin = ILin + 1
         ENDDO
         IF (NLin .EQ. 0) THEN
            WRITE(6, '(3A)') 'leemanlin: Error, linea ', NomLin,        &
     &           ' no existe.'
            WRITE(ULog, '(3A)') 'leemanlin: Error, linea ', NomLin,     &
     &           ' no existe.'
            FWarning = .TRUE.
            CYCLE
         ENDIF

         BloIni = MAX(BloIni, 1)
         BloFin = MIN(BloFin, NBloques)
         DO NumBlo = BloIni, BloFin
            IF (                                                        &
     &           (XImpLin .LT. SEPSILON) .OR.                           &
     &           (VNomLin .LT. SEPSILON)                                &
     &           ) THEN
               WRITE(6, '(4A, I3)') 'leemanlin: Linea ',                &
     &              NomLin,                                             &
     &              ' tiene el voltaje o la reactancia en cero',        &
     &              ' en la etapa ', NumBlo
               WRITE(ULog, '(4A, I3)') 'leemanlin: Linea ',             &
     &              NomLin,                                             &
     &              ' tiene el voltaje o la reactancia en cero',        &
     &              ' en la etapa ', NumBlo
               FStop = .TRUE.
            ENDIF
            
            IISim = LinManSInd(ISim)
         
            LinVNom(NLin, NumBlo, IISim) = VNomLin
            LinXImp(NLin, NumBlo, IISim) = XImpLin
            LinFOpe(NLin, NumBlo, IISim) = FOpeLin
            LinRes(NLin, NumBlo, IISim) = ResLin
            IF (ManALin .LT.                                            &
     &           LinTMax(1, LinNFlu(NLin), NLin, NumBlo, IISim)) THEN
!     Mantenimiento hacia abajo.
               DO IFlu = 1, LinNFlu(NLin)
                  IF (ManALin .LT. LinTMax(1, IFlu, NLin, NumBlo, IISim))&
     &                 THEN
                     LinTMax(1, IFlu, NLin, NumBlo, IISim) = ManALin
                  ENDIF
                  ManALin =                                             &
     &                 ManALin - LinTMax(1, IFlu, NLin, NumBlo, IISim)
                  IF (ManALin .LT. 0.0) THEN
                     ManALin = 0.0
                  ENDIF
               ENDDO
            ELSE
!     Mantenimiento hacia arriba.
               DO IFlu = 1, LinNFlu(NLin)
                  LinTMax(1, IFlu, NLin, NumBlo, IISim) =                &
     &                 ManALin/LinNFlu(NLin)
               ENDDO
            ENDIF
            IF (ManBLin .LT.                                            &
     &           LinTMax(2, LinNFlu(NLin), NLin, NumBlo, IISim)) THEN
!     Mantenimiento hacia abajo.
               DO IFlu = 1, LinNFlu(NLin)
                  IF (ManBLin .LE. LinTMax(2, IFlu, NLin, NumBlo, IISim))&
     &                 THEN
                     LinTMax(2, IFlu, NLin, NumBlo, IISim) = ManBLin
                  ENDIF              
                  ManBLin = ManBLin -                                   &
     &                 LinTMax(2, IFlu, NLin, NumBlo, IISim)
                  IF (ManBLin .LT. 0.0) THEN
                     ManBLin = 0.0
                  ENDIF
               ENDDO
            ELSE
!     Mantenimiento hacia arriba.
               DO IFlu = 1, LinNFlu(NLin)
                  LinTMax(2, IFlu, NLin, NumBlo, IISim) =                &
     &                 ManBLin/LinNFlu(NLin)
               ENDDO
            ENDIF
         ENDDO
      ENDDO

 100  CALL Cerrar(URead)

      IF (FStop) THEN
         STOP 1
      ENDIF

      IF (FWarning) THEN
         WRITE(ULog, '(A)') 'leemanli: Hay warnings.'
         IF (FInterfaz .gt. 1) THEN
            WRITE(6, '(A)') 'leemanli: Hay warnings.'
            WRITE(6, '(A)') 'leemanli: Continuo?'
            READ(5, *)
            WRITE(6, '(A)') 'Continuando...'
         ENDIF
      ENDIF

      NFlujo = MAXVAL(LinNFlu(1:NLinea))
      LinTMaxS(1:2, 1:NFlujo, 1:NLinea, 1:NBloques)  = 0.0d0

      DO IBlo = 1, NBloques
         DO ILin = 1, NLinea
            DO IFlu = 1, LinNFlu(ILin)
               DO ISim = 1, Dim%LinManS
                  LinTMaxS(1, IFlu, ILin, IBlo) =                       &
     &                 MAX(LinTMaxS(1, IFlu, ILin, IBlo),               &
     &                 LinTMax(1, IFlu, ILin, IBlo, ISim))
                  LinTMaxS(2, IFlu, ILin, IBlo) =                       &
     &                 MAX(LinTMaxS(2, IFlu, ILin, IBlo),               &
     &                 LinTMax(2, IFlu, ILin, IBlo, ISim))
               ENDDO
            ENDDO
         ENDDO
      ENDDO

      RETURN
      END
