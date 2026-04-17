      SUBROUTINE LeeFilEmbDim(FFiltVar, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcFilEmb

      TYPE(PAR_DIMS) Dim
      LOGICAL FFiltVar
      INTEGER ULog

      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER URead
      INTEGER FiltNCen

!************************
!     Lee datos plpfilemb.dat
!************************
      Dim%FiltParam = 0
      Dim%EmbFilt = 0
      Dim%FiltTramo = 0
      FFiltVar = .FALSE.

      URead = Abrir(NArcFilEmb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leefilemb: No existe archivo ',           &
     &        NArcFilEmb, '.'
         RETURN
      ENDIF

      FFiltVar = .TRUE.

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FiltNCen

      Dim%EmbFilt = FiltNCen
      Dim%FiltParam = 3
      Dim%FiltTramo = 5 + 1
      
      CALL Cerrar(URead)
      RETURN
      END


!**************************************
!     Subrutina Datos de filtraciones de embalses
!**************************************
      SUBROUTINE LeeFilEmb(NCentral, NCenEmb, CenNom,                   &
     &     CenFHid, FiltEmbInd,                                         &
     &     FiltNTramo, FiltParam, FiltNCen, FiltProm, EmbVIni, NEtapa,  &
     &     FFiltVar, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcFilEmb, &
     &           PFiltVol, PFiltPend, PFiltConst
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
!     Variables Globales
!******************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER CenFHid(Dim%EmbFilt)
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER FiltNTramo(Dim%EmbFilt)
      INTEGER NEtapa
      INTEGER ULog
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION GetInfty
!     Variables Locales
!******************
      CHARACTER*48 NomCen
      CHARACTER*48 NomEmb
      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER IEta
      INTEGER IFil
      INTEGER ITra
      INTEGER IPar
      INTEGER FiltNCen
      INTEGER Ind
      INTEGER NumCen
      INTEGER NumEmb
      INTEGER URead
      DOUBLE PRECISION FFiltraciones
      DOUBLE PRECISION Vol
      LOGICAL FFiltVar
!
      LOGICAL FStop
      CHARACTER*42 Objeto

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


!************************
!     Lee datos plpfilemb.dat
!************************
      URead = Abrir(NArcFilEmb, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leefilemb: No existe archivo ',           &
     &        NArcFilEmb, '.'
         
         FFiltVar = .FALSE.
         RETURN
      ENDIF

      FFiltVar = .TRUE.

      WRITE(ULog, '(3A)') 'leefilemb: archivo ',           &
     &     NArcFilEmb, ' encontrado.'


      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FiltNCen
      IF (FiltNCen .GT. Dim%EmbFilt) THEN
         WRITE(6, '(2A, I3, A, I3, A)')                                 &
     &        'leefilemb: Numero embalses filtraciones  ',              &
     &        ' = ', FiltNCen, ' > ', Dim%EmbFilt, '.'
         WRITE(ULog, '(2A, I3, A, I3, A)')                              &
     &        'leefilemb: Numero embalses filtraciones  ',              &
     &        ' = ', FiltNCen, ' > ', Dim%EmbFilt, '.'
         STOP 1
      ENDIF

      FiltEmbInd = 0
      CenFHid = 0
      DO IFil = 1, FiltNCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         READ(URead, '(A1)') AuxVar
!     Filtracion promedio de la etapa 1 (Se pone en la 2da componente).
         READ(URead, *) FiltProm(IFil, 2)
         IF (FiltProm(IFil, 2) .LT. 0d0) FiltProm(IFil, 2) = 0.0d0
!     Por ahora se copia a las demas etapas.
         DO IEta = 3, NEtapa + 1
            FiltProm(IFil, IEta) = FiltProm(IFil, 2)
         ENDDO
         Objeto = 'embalse'
         CALL NomCen2NumCen(NumEmb, FStop, NomEmb,                      &
     &        CenNom, NCenEmb, Objeto, ULog)
         IF (FStop) THEN
            STOP 1
         ENDIF
         FiltEmbInd(IFil) = NumEmb
         
         READ(URead, '(A1)') AuxVar
         READ(URead, *) FiltNTramo(IFil)
         READ(URead, '(A1)') AuxVar
         IF (FiltNTramo(IFil) .GT. Dim%FiltTramo - 1) THEN
            WRITE(6, '(3A, I3, A, I3, A)')                              &
     &           'leefilemb: Numero filtraciones muy grande   ',        &
     &           NomCen, ' = ', FiltNTramo(IFil), ' > ',                &
     &           Dim%FiltTramo - 1, '.'
            WRITE(ULog, '(3A, I3, A, I3, A)')                           &
     &           'leefilemb: Numero filtraciones muy grande   ',        &
     &           NomCen, ' = ', FiltNTramo(IFil), ' > ',                &
     &           Dim%FiltTramo - 1, '.'
            STOP 1
         ENDIF

         DO ITra = 1, FiltNTramo(IFil)
            READ(URead, *)  Ind,                                        &
     &           (FiltParam(ITra, IFil, IPar), IPar = 1, Dim%FiltParam)
            FiltParam(ITra, IFil, PFiltVol) =                           &
     &           FiltParam(ITra, IFil, PFiltVol)*1.0D3
            FiltParam(ITra, IFil, PFiltPend) =                          &
     &           FiltParam(ITra, IFil, PFiltPend)/1.0D3
         ENDDO
         FiltParam(FiltNTramo(IFil) + 1, IFil, PFiltConst) = &
     &        FiltParam(FiltNTramo(IFil), IFil, PFiltConst)
         FiltParam(FiltNTramo(IFil) + 1, IFil, PFiltPend) = &
     &        FiltParam(FiltNTramo(IFil), IFil, PFiltPend)
         FiltParam(FiltNTramo(IFil) + 1, IFil, PFiltVol) = GetInfty()

         FiltNTramo(IFil) = FiltNTramo(IFil) + 1         
!     filtracion inicial, que depende de la cota inicial y por lo
!     tanto es dato.
         Vol = EmbVIni(FiltEmbInd(IFil))
         FiltProm(IFil, 1) = FFiltraciones(FiltNTramo(IFil),            &
     &        FiltParam(1, IFil, PFiltVol),                             &
     &        FiltParam(1, IFil, PFiltPend),                            &
     &        FiltParam(1, IFil, PFiltConst), Vol)

         IF (FiltProm(IFil, 1) .LT. 0d0) FiltProm(IFil, 1) = 0.0d0
!        lee central donde caen las filtraciones
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCen
         Objeto = 'central'
         CALL NomCen2NumCen(NumCen, FStop, NomCen,                      &
     &        CenNom, NCentral, Objeto, ULog)
         IF (FStop) THEN
            STOP 1
         ENDIF
         CenFHid(IFil) = NumCen
      ENDDO

      CALL Cerrar(URead)
      RETURN
      END
