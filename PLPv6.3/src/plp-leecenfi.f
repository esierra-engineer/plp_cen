      SUBROUTINE LeeCenFilDim(ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCenFil

      TYPE(PAR_DIMS) Dim

      INTEGER ULog

      CHARACTER*80 AuxVar
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      INTEGER FiltNCen

!

      URead = Abrir(NArcCenFil, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecenfil: No existe archivo ',              &
     &        NArcCenFil, '.'
         WRITE(ULog, '(3A)') 'leecenfil: No existe archivo ',           &
     &        NArcCenFil, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FiltNCen

      Dim%FiltParam = 3
      Dim%EmbFilt = FiltNCen
      Dim%FiltTramo = 2 + 1

      CALL Cerrar(URead)

      RETURN
      END
      

!**************************************
!     Subrutina Datos Centrales Filtraciones
!**************************************
      SUBROUTINE LeeCenFil(NCentral, NCenEmb, CenNom,                   &
     &     FiltCenInd, FiltEmbInd,                                      &
     &     FiltNTramo, FiltParam, FiltNCen, FiltProm, EmbVIni, NEtapa,  &
     &     ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCenFil, &
     &           PFiltVol, PFiltPend, PFiltConst
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
!     Variables Globales
!******************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER FiltCenInd(Dim%EmbFilt)
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
      INTEGER ICen
      INTEGER IEmb
      INTEGER IEta
      INTEGER IFil
      INTEGER ITra
      INTEGER IPar
      INTEGER FiltNCen
      INTEGER Ind
      INTEGER NumCen
      INTEGER NumEmb
      INTEGER URead
      DOUBLE PRECISION VFiltPend
      DOUBLE PRECISION FEscala
      DOUBLE PRECISION FFiltraciones
      DOUBLE PRECISION Vol

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


!************************
!     Lee datos pcpparfil.dat
!************************
      URead = Abrir(NArcCenFil, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecenfil: No existe archivo ',              &
     &        NArcCenFil, '.'
         WRITE(ULog, '(3A)') 'leecenfil: No existe archivo ',           &
     &        NArcCenFil, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) FiltNCen
      IF (FiltNCen .GT. Dim%EmbFilt) THEN
         WRITE(6, '(2A, I3, A, I3, A)')                                 &
     &        'leecenfil: Numero embalses filtraciones  ',              &
     &        ' = ', FiltNCen, ' > ', Dim%EmbFilt, '.'
         WRITE(ULog, '(2A, I3, A, I3, A)')                              &
     &        'leecenfil: Numero embalses filtraciones  ',              &
     &        ' = ', FiltNCen, ' > ', Dim%EmbFilt, '.'
         STOP 1
      ENDIF

      FiltCenInd = 0
      FiltEmbInd = 0
      DO IFil = 1, FiltNCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCen
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
         NumCen = 0
         ICen = 1
         DO WHILE ((NumCen .EQ. 0) .AND. (ICen .LE. NCentral))
            IF (CenNom(ICen) .EQ. NomCen) THEN
               NumCen = ICen
            ENDIF
            ICen = ICen + 1
         ENDDO
         IF (NumCen .EQ. 0) THEN
            WRITE(6,  '(A, A, A)')                                      &
     &           'leecenfil: Error, central ', NomCen, ' no existe.'
            WRITE(ULog,  '(A, A, A)')                                   &
     &           'leecenfil: Error, central ', NomCen, ' no existe.'
            STOP 1
         ENDIF
         FiltCenInd(IFil) = NumCen
         NumEmb = 0
         IEmb = 1
         DO WHILE ((NumEmb .EQ. 0) .AND. (IEmb .LE. NCenEmb))
            IF (CenNom(IEmb) .EQ. NomEmb) THEN
               NumEmb = IEmb
            ENDIF
            IEmb = IEmb + 1
         ENDDO
         IF (NumEmb .EQ. 0) THEN
            WRITE(6, '(A, A, A)')                                       &
     &           'leecenfil: Error, embalse ', NomEmb, ' no existe.'
            WRITE(ULog, '(A, A, A)')                                    &
     &           'leecenfil: Error, embalse ', NomEmb, ' no existe.'
            STOP 1
         ENDIF
         FiltEmbInd(IFil) = NumEmb
         READ(URead, '(A1)') AuxVar
         READ(URead, *) FiltNTramo(IFil)
         READ(URead, '(A1)') AuxVar
         IF (FiltNTramo(IFil) .GT. Dim%FiltTramo - 1) THEN
            WRITE(6, '(3A, I3, A, I3, A)')                              &
     &           'leecenfil: Numero filtraciones muy grande   ',        &
     &           NomCen, ' = ', FiltNTramo(IFil), ' > ',                &
     &           Dim%FiltTramo - 1, '.'
            WRITE(ULog, '(3A, I3, A, I3, A)')                           &
     &           'leecenfil: Numero filtraciones muy grande   ',        &
     &           NomCen, ' = ', FiltNTramo(IFil), ' > ',                &
     &           Dim%FiltTramo - 1, '.'
            STOP 1
         ENDIF
         VFiltPend = -DINFTY
         DO ITra = 1, FiltNTramo(IFil)
            READ(URead, *)  Ind,                                        &
     &           (FiltParam(ITra, IFil, IPar), IPar = 1, Dim%FiltParam), &
     &           FEscala
            FiltParam(ITra, IFil, PFiltVol) =                           &
     &           FiltParam(ITra, IFil, PFiltVol)*                       &
     &           FEscala/1.0D3
            FiltParam(ITra, IFil, PFiltPend) =                          &
     &           FiltParam(ITra, IFil, PFiltPend)/1.0D3
            IF (VFiltPend .GT. FiltParam(ITra, IFil, PFiltPend)) THEN
               WRITE(6, '(4A)') 'leecenfil: Las pendientes deben ',     &
     &              'ir en orden creciente, embalse ', NomEmb, '.'
               WRITE(ULog, '(4A)')                                      &
     &              'leecenfil: Las pendientes deben ir en ',           &
     &              'orden creciente, embalse ', NomEmb, '.'
               STOP 1
            ENDIF
            VFiltPend = FiltParam(ITra, IFil, PFiltPend)
         ENDDO
         FiltParam(FiltNTramo(IFil) + 1, IFil, PFiltVol) = GetInfty()
!     filtracion inicial, que depende de la cota inicial y por lo
!     tanto es dato.
         Vol = EmbVIni(FiltEmbInd(IFil))
         FiltProm(IFil, 1) = FFiltraciones(FiltNTramo(IFil),            &
     &        FiltParam(1, IFil, PFiltVol),                             &
     &        FiltParam(1, IFil, PFiltPend),                            &
     &        FiltParam(1, IFil, PFiltConst), Vol)
         IF (FiltProm(IFil, 1) .LT. 0d0) FiltProm(IFil, 1) = 0.0d0
      ENDDO
      CALL Cerrar(URead)
      RETURN
      END
