      SUBROUTINE LeeAflCenDim(ULog, Dim)


      USE PLP, ONLY : PAR_DIMS, NArcAflCen

      INTEGER ULog
      TYPE(PAR_DIMS) Dim

      CHARACTER*12 AuxVar
      INTEGER URead
      INTEGER Abrir
      INTEGER EstocNVar2, NClase

      URead = Abrir(NArcAflCen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeaflcen: Error, no existe archivo ',       &
     &        NArcAflCen, '.'
         WRITE(ULog, '(3A)') 'leeaflcen: Error, no existe archivo ',    &
     &        NArcAflCen, '.'
         STOP 1
      ENDIF


      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) EstocNVar2, NClase

      Dim%Clase = NClase


      CALL Cerrar(URead)

      RETURN
      END


!***************************************************************
!     Subrutina que lee los representantes de cada clase de las que
!     se han utilizado para discretizar el histograma de los caudales
!     afluentes a una central para una bloque dada.
!***************************************************************
      SUBROUTINE LeeAflCen(FDatChe, NBloques, NCentral, NCenEmb, NCenSer, &
     &     NCenPas, CenNom, CenTipo,                                      &
     &     HidSPPQAfl, PasQAfl, NClase, ULog, Dim)


      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
!     Lee los caudales por bloque y por apertura
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*48 NomCen
      INTEGER Abrir
      INTEGER ICen
      INTEGER ICenPP
      INTEGER IBlo
      INTEGER NBloques
      INTEGER NClase
      INTEGER NCen
      INTEGER NDia
      INTEGER EstocNCol
      INTEGER EstocNFila
      INTEGER EstocNVar
      INTEGER EstocNVar2
      INTEGER IEstocCen
      INTEGER IClase
      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenPP
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NBloCau
      INTEGER NumBlo
      INTEGER ULog
      INTEGER URead
      LOGICAL FDatChe
      LOGICAL FStop
      DOUBLE PRECISION CauBlo(Dim%Clase)
      DOUBLE PRECISION HidSPPQAfl(Dim%HidSPP, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION PasQAfl(Dim%Pas, Dim%Blo, Dim%Clase)

      EstocNCol = NCenPas
      EstocNFila = NCenEmb + NCenSer
      EstocNVar = EstocNFila + EstocNCol
      FStop = .FALSE.
      URead = Abrir(NArcAflCen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leeaflcen: Error, no existe archivo ',       &
     &        NArcAflCen, '.'
         WRITE(ULog, '(3A)') 'leeaflcen: Error, no existe archivo ',    &
     &        NArcAflCen, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) EstocNVar2, NClase
      IF (NClase .GT. Dim%Clase) THEN
         WRITE(6, '(A, I3, A, I3, A)')                                  &
     &        'leeaflcen: Error, NClase = ',                            &
     &        NClase, ' > DimClase = ', Dim%Clase, '.'
         WRITE(ULog, '(A, I3, A, I3, A)')                               &
     &        'leeaflcen: Error, NClase = ',                            &
     &        NClase, ' > DimClase = ', Dim%Clase, '.'
         FStop = .TRUE.
      ENDIF
      IF (FStop) THEN
         STOP 1
      ENDIF
!
!     Inicializacion de los arreglos con los caudales de las centrales:
!     Por defecto, estos se suponen deterministicos.
      DO ICen = 1, NCentral
         IF ((CenTipo(ICen) .EQ. PCenTipEmb) .OR.                    &
     &        (CenTipo(ICen) .EQ. PCenTipEmbAux) .OR.                &
     &        (CenTipo(ICen) .EQ. PCenTipSer) .OR.                   &
     &        (CenTipo(ICen) .EQ. PCenTipRie)) THEN
!     
!     embalses y pasadas
            DO IClase = 2, NClase
               HidSPPQAfl(ICen, 1:NBloques, IClase) =                      &
     &              HidSPPQAfl(ICen, 1:NBloques, 1)
            ENDDO
         ELSE IF (CenTipo(ICen) .EQ. PCenTipPas) THEN
!
!     pasadas puras
!     Indice del comienzo de las pasadas puras
            ICenPP = ICen - NCenEmb - NCenSer
            DO IClase = 2, NClase
               PasQAfl(ICenPP, 1:NBloques, IClase) =                       &
     &              PasQAfl(ICenPP, 1:NBloques, 1)
            ENDDO
         ENDIF
      ENDDO
      IF (EstocNVar2 .NE. EstocNVar) THEN
         IF (FDatChe) THEN
            WRITE(6, '(A, I5, A, I5, A)')                               &
     &           'leeaflcen: Error, EstocNVar2 = ',                     &
     &           EstocNVar2, ' ~ = EstocNVar = ', EstocNVar, '.'
            WRITE(ULog, '(A, I5, A, I5, A)')                            &
     &           'leeaflcen: Error, EstocNVar2 = ',                     &
     &           EstocNVar2, ' ~ = EstocNVar = ', EstocNVar, '.'
            FStop = .TRUE.
         ENDIF
      ENDIF
      DO IEstocCen = 1, EstocNVar2
         READ(URead, '(A1)', END = 100) AuxVar
         READ(URead, *) NomCen
         NCen = 0
         ICen = 1
         DO WHILE ((NCen .EQ. 0) .AND. (ICen .LE. EstocNVar))
            IF (CenNom(ICen) .EQ. NomCen) THEN
               NCen = ICen
            ENDIF
            ICen = ICen + 1
         ENDDO
         IF (NCen .EQ. 0) THEN
            IF (FDatChe) THEN
               WRITE(6, '(3A)') 'leeaflcen: Error, serie ', NomCen,     &
     &              ' no existe.'
               WRITE(ULog, '(3A)') 'leeaflcen: Error, serie ', NomCen,  &
     &              ' no existe.'
               FStop = .FALSE.
            ENDIF
         ENDIF
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NBloCau
         READ(URead, '(A1)') AuxVar
         DO IBlo = 1, NBloCau
!
!     Lectura de caudales aleatorios.
            READ(URead, *) NDia, NumBlo, (CauBlo(IClase), IClase = 1,   &
     &           NClase)
            IF ((NumBlo .LT. 1) .OR. (NumBlo .GT. NBloques)) THEN
               IF (FDatChe) THEN
                  WRITE(6, '(3A)')                                      &
     &                 'leeaflcen: Error en datos serie ', NomCen, '.'
                  WRITE(ULog, '(3A)')                                   &
     &                 'leeaflcen: Error en datos serie ', NomCen, '.'
                  WRITE(6, '(2A, I4, A, I4, A)')                        &
     &                 'leeaflcen: Numero de ',                         &
     &                 'bloque fuera de rango: 1 < = ', NumBlo,         &
     &                 ' < = ', NBloques, '.'
                  WRITE(ULog, '(2A, I4, A, I4, A)')                     &
     &                 'leeaflcen: Numero de ',                         &
     &                 'bloque fuera de rango: 1 < = ', NumBlo,         &
     &                 ' < = ', NBloques, '.'
                  FStop = .TRUE.
               ENDIF
            ELSE
               IF (NCen .NE. 0) THEN
                  IF ((CenTipo(NCen) .EQ. PCenTipEmb) .OR.              &
     &                 (CenTipo(NCen) .EQ. PCenTipEmbAux) .OR.          &
     &                 (CenTipo(NCen) .EQ. PCenTipSer) .OR.             &
     &                 (CenTipo(NCen) .EQ. PCenTipRie)) THEN
!
!     se le da la cualidad de 'aleatorios' a los caudales que llegan a e
!     embalse o pasada
                     DO IClase = 1, NClase
                        HidSPPQAfl(NCen, NumBlo, IClase) =              &
     &                       CauBlo(IClase)
                     ENDDO
                  ELSE IF (CenTipo(NCen) .EQ. PCenTipPas) THEN
!
!$$$  Indice del comienzo de las pasadas puras
                     NCenPP = NCen - NCenEmb - NCenSer
!
!     se le da la cualidad de 'aleatorios' a los caudales que llegan a e
!     pasada pura
                     DO IClase = 1, NClase
                        PasQAfl(NCenPP, NumBlo, IClase) =               &
     &                       CauBlo(IClase)
                     ENDDO
                  ELSE
                     IF (FDatChe) THEN
                        WRITE(6, '(3A)') 'leeaflcen: Error, central ',  &
     &                       NomCen, ' es de tipo termico.'
                        WRITE(ULog, '(4A)') 'leeaflcen: Error, ',       &
     &                       'central ', NomCen, ' es de tipo termico.'
                        FStop = .TRUE.
                     ENDIF
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
  100 CALL Cerrar(URead)
      IF (FStop) THEN
         STOP 1
      ENDIF
      RETURN
      END
