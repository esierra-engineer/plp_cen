!**********************************
!     Subrutina Lee Mantenimiento Lineas
!**********************************
      SUBROUTINE LeeManLin(FDatChe, NBloques, NLinea, LinNom, LinNFlu,  &
     &     LinFOpe, LinRes, LinTMax, LinVNom, LinXImp,                  &
     &     Finterfaz, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, NArcManLin, NArcManLin2

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      INCLUDE 'machcons.fpp'
      EXTERNAL Abrir
      INTEGER Abrir
      CHARACTER*12 AuxVar
      CHARACTER*48 LinNom(Dim%Lin)
      CHARACTER*48 NomLin
      INTEGER BloIni
      INTEGER BloFin
      INTEGER IBlo
      INTEGER IFlu
      INTEGER ILin
      INTEGER ILinMan
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBloques
      INTEGER NBloMan
      INTEGER NLin
      INTEGER NLinea
      INTEGER NLinMan
      INTEGER NumBlo
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FOpeLin
      LOGICAL FStop
      LOGICAL FWarning
      INTEGER FInterfaz
      LOGICAL LinFOpe(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION VNomLin
      DOUBLE PRECISION ResLin
      DOUBLE PRECISION XImpLin
      DOUBLE PRECISION ManALin
      DOUBLE PRECISION ManBLin
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinXImp(Dim%Lin, Dim%Blo)
!     
      FStop = .FALSE.
      FWarning = .FALSE.
      URead = Abrir(NArcManLin, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leemanli: Error, no existe archivo ',        &
     &        NArcManLin, '.'
         WRITE(ULog, '(3A)') 'leemanli: Error, no existe archivo ',     &
     &        NArcManLin, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NLinMan
      DO ILinMan = 1, NLinMan
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomLin
         NLin = 0
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
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NBloMan
         READ(URead, '(A1)') AuxVar
         DO IBlo = 1, NBloMan
            READ(URead, *) NumBlo, ManALin, ManBLin, FOpeLin
            IF ((NumBlo .LT. 1) .OR. (NumBlo .GT. NBloques)) THEN
               IF (FDatChe) THEN
                  WRITE(6, '(3A)')                                      &
     &                 'leemanlin: Error en datos linea ', NomLin, '.'
                  WRITE(6, '(2(A, I4), A)')                             &
     &                 'leemanlin: Numero de etapa fuera de ',          &
     &                 'rango: 1<', NumBlo, '<', NBloques, '.'
                  WRITE(ULog, '(3A)')                                   &
     &                 'leemanlin: Error en datos linea ', NomLin, '.'
                  WRITE(ULog, '(2(A, I4), A)')                          &
     &                 'leemanlin: Numero de etapa fuera de ',          &
     &                 'rango: 1<', NumBlo, '<', NBloques, '.'
                  FStop = .TRUE.
               ENDIF
            ELSE
               IF (NLin .GT. 0) THEN
                  LinFOpe(NLin, NumBlo) = FOpeLin
                  IF (ManALin .LT.                                      &
     &                 LinTMax(1, LinNFlu(NLin), NLin, NumBlo)) THEN
!     Mantenimiento hacia abajo.
                     DO IFlu = 1, LinNFlu(NLin)
                        IF (ManALin .LT. LinTMax(1, IFlu, NLin, NumBlo))&
     &                       THEN
                           LinTMax(1, IFlu, NLin, NumBlo) = ManALin
                        ENDIF
                        ManALin = ManALin - LinTMax(1, IFlu, NLin, NumBlo)
                        IF (ManALin .LT. 0.0d0) THEN
                           ManALin = 0.0d0
                        ENDIF
                     ENDDO
                  ELSE
!     Mantenimiento hacia arriba.
                     DO IFlu = 1, LinNFlu(NLin)
                        LinTMax(1, IFlu, NLin, NumBlo) =                &
     &                       ManALin/LinNFlu(NLin)
                     ENDDO
                  ENDIF
                  IF (ManBLin .LT.                                      &
     &                 LinTMax(2, LinNFlu(NLin), NLin, NumBlo)) THEN
!     Mantenimiento hacia abajo.
                     DO IFlu = 1, LinNFlu(NLin)
                        IF (ManBLin .LE. LinTMax(2, IFlu, NLin, NumBlo))&
     &                       THEN
                           LinTMax(2, IFlu, NLin, NumBlo) = ManBLin
                        ENDIF              
                        ManBLin = ManBLin -                             &
     &                       LinTMax(2, IFlu, NLin, NumBlo) 
                        IF (ManBLin .LT. 0.0d0) THEN
                           ManBLin = 0.0d0
                        ENDIF
                     ENDDO
                  ELSE
!     Mantenimiento hacia arriba.
                     DO IFlu = 1, LinNFlu(NLin)
                        LinTMax(2, IFlu, NLin, NumBlo) =                 &
     &                       ManBLin/LinNFlu(NLin)
                     ENDDO
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      CALL Cerrar(URead)
      URead = Abrir(NArcManLin2, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .GT. 0) THEN
         READ(URead, *, END = 1000) AuxVar
         DO WHILE (.TRUE.)
            READ(URead, *, END = 1000) NomLin, BloIni, BloFin,          &
     &           ManALin, ManBLin,                                      &
     &           VNomLin, ResLin, XImpLin, FOpeLin
            NLin = 0
            ILin = 1
            DO WHILE ((NLin .EQ. 0) .AND. (ILin .LE. NLinea))
               IF (LinNom(ILin) .EQ. NomLin) THEN
                  NLin = ILin
               ENDIF
               ILin = ILin + 1
            ENDDO
            IF (NLin .EQ. 0) THEN
               WRITE(6, '(3A)') 'leemanlin: Error, linea ', NomLin,     &
     &              ' no existe.'
               WRITE(ULog, '(3A)') 'leemanlin: Error, linea ', NomLin,  &
     &              ' no existe.'
               FWarning = .TRUE.
            ENDIF
            IF (NLin .GT. 0) THEN
               BloIni = MAX(BloIni, 1)
               BloFin = MIN(BloFin, NBloques)
               DO NumBlo = BloIni, BloFin
                  IF (                                                  &
     &                 (XImpLin .LT. SEPSILON) .OR.                     &
     &                 (VNomLin .LT. SEPSILON)                          &
     &                 ) THEN
                     WRITE(6, '(4A, I3)') 'leemanlin: Linea ',          &
     &                    NomLin,                                       &
     &                    ' tiene el voltaje o la reactancia en cero',  &
     &                    ' en la etapa ', NumBlo
                     WRITE(ULog, '(4A, I3)') 'leemanlin: Linea ',       &
     &                    NomLin,                                       &
     &                    ' tiene el voltaje o la reactancia en cero',  &
     &                    ' en la etapa ', NumBlo
                     FStop = .TRUE.
                  ENDIF
                  LinVNom(NLin, NumBlo) = VNomLin
                  LinXImp(NLin, NumBlo) = XImpLin
                  LinFOpe(NLin, NumBlo) = FOpeLin
                  LinRes(NLin, NumBlo) = ResLin
                  IF (ManALin .LT.                                      &
     &                 LinTMax(1, LinNFlu(NLin), NLin, NumBlo)) THEN
!     Mantenimiento hacia abajo.
                     DO IFlu = 1, LinNFlu(NLin)
                        IF (ManALin .LT. LinTMax(1, IFlu, NLin, NumBlo))&
     &                       THEN
                           LinTMax(1, IFlu, NLin, NumBlo) = ManALin
                        ENDIF
                        ManALin =                                       &
     &                       ManALin - LinTMax(1, IFlu, NLin, NumBlo)
                        IF (ManALin .LT. 0.0) THEN
                           ManALin = 0.0
                        ENDIF
                     ENDDO
                  ELSE
!     Mantenimiento hacia arriba.
                     DO IFlu = 1, LinNFlu(NLin)
                        LinTMax(1, IFlu, NLin, NumBlo) =                &
     &                       ManALin/LinNFlu(NLin)
                     ENDDO
                  ENDIF
                  IF (ManBLin .LT.                                      &
     &                 LinTMax(2, LinNFlu(NLin), NLin, NumBlo)) THEN
!     Mantenimiento hacia abajo.
                     DO IFlu = 1, LinNFlu(NLin)
                        IF (ManBLin .LE. LinTMax(2, IFlu, NLin, NumBlo))&
     &                       THEN
                           LinTMax(2, IFlu, NLin, NumBlo) = ManBLin
                        ENDIF              
                        ManBLin = ManBLin -                             &
     &                       LinTMax(2, IFlu, NLin, NumBlo)
                        IF (ManBLin .LT. 0.0) THEN
                           ManBLin = 0.0
                        ENDIF
                     ENDDO
                  ELSE
!     Mantenimiento hacia arriba.
                     DO IFlu = 1, LinNFlu(NLin)
                        LinTMax(2, IFlu, NLin, NumBlo) =                &
     &                       ManBLin/LinNFlu(NLin)
                     ENDDO
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
 1000 CALL Cerrar(URead)
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
      RETURN
      END
