      SUBROUTINE LeeCenRenDim(ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCenRen

      TYPE(PAR_DIMS) Dim

      INTEGER ULog

      CHARACTER*80 AuxVar
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      INTEGER RendNCen
!
      URead = Abrir(NArcCenRen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecenren: No existe archivo ',              &
     &        NArcCenRen, '.'
         WRITE(ULog, '(3A)') 'leecenren: No existe archivo ',           &
     &        NArcCenRen, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) RendNCen

      Dim%EmbRend = RendNCen
      Dim%RendParam = 3
      Dim%RendTramo = 4 + 1

      CALL Cerrar(URead)

      RETURN
      END

!*******************************************
!     Subrutina Datos Centrales Rendimientos
!*******************************************
      SUBROUTINE LeeCenRen(NCentral, NCenEmb, CenNom,                   &
     &     RendCenInd, RendEmbInd,                                      &
     &     RendNTramo, RendParam, RendNCen, RendProm, EmbVIni, NEtapa, &
     &     ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCenRen, &
     &      PRendVol, PRendPend, PRendConst
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!     Variables Globales
!***********************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER RendCenInd(Dim%EmbRend)
      INTEGER RendEmbInd(Dim%EmbRend)
      INTEGER RendNTramo(Dim%EmbRend)
      INTEGER NEtapa
      INTEGER ULog
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION RendProm(Dim%EmbRend, Dim%Eta + 1)
      DOUBLE PRECISION GetInfty
!     Variables Locales
!**********************
      CHARACTER*48 NomCen
      CHARACTER*48 NomEmb
      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER ICen
      INTEGER IEmb
      INTEGER IEta
      INTEGER IRen
      INTEGER ITra
      INTEGER IPar
      INTEGER RendNCen
      INTEGER Ind
      INTEGER NumCen
      INTEGER NumEmb
      INTEGER URead
      DOUBLE PRECISION VRendPend
      DOUBLE PRECISION FEscala
      DOUBLE PRECISION FRendimientos
      DOUBLE PRECISION Vol

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!****************************
!     Lee datos pcpparren.dat
!****************************
      URead = Abrir(NArcCenRen, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecenren: No existe archivo ',              &
     &        NArcCenRen, '.'
         WRITE(ULog, '(3A)') 'leecenren: No existe archivo ',           &
     &        NArcCenRen, '.'
         STOP 1
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) RendNCen
      IF (RendNCen .GT. Dim%EmbRend) THEN
         WRITE(6, '(2A, I3, A, I3, A)')                                 &
     &        'leecenren: Numero embalses rendimientos  ',              &
     &        ' = ', RendNCen, ' > ', Dim%EmbRend, '.'
         WRITE(ULog, '(2A, I3, A, I3, A)')                              &
     &        'leecenren: Numero embalses rendimientos  ',              &
     &        ' = ', RendNCen, ' > ', Dim%EmbRend, '.'
         STOP 1
      ENDIF
      DO IRen = 1, RendNCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         READ(URead, '(A1)') AuxVar
!     Rendimiento promedio de la etapa 1 (Se pone en la 2da componente).
         READ(URead, *) RendProm(IRen, 2)
         IF (RendProm(IRen, 2) .LT. 0d0) RendProm(IRen, 2) = 0.0d0
!     Por ahora se copia a las demas etapas.
         DO IEta = 3, NEtapa + 1
            RendProm(IRen, IEta) = RendProm(IRen, 2)
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
            WRITE(6, '(A, A, A)')                                       &
     &           'leecenren: Error, central ', NomCen, ' no existe.'
            WRITE(ULog, '(A, A, A)')                                    &
     &           'leecenren: Error, central ', NomCen, ' no existe.'
            STOP 1
         ENDIF
         RendCenInd(IRen) = NumCen
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
     &           'leecenren: Error, embalse ', NomEmb, ' no existe.'
            WRITE(ULog, '(A, A, A)')                                    &
     &           'leecenren: Error, embalse ', NomEmb, ' no existe.'
            STOP 1
         ENDIF
         RendEmbInd(IRen) = NumEmb
         READ(URead, '(A1)') AuxVar
         READ(URead, *) RendNTramo(IRen)
         READ(URead, '(A1)') AuxVar
         IF (RendNTramo(IRen) .GT. Dim%RendTramo - 1) THEN
            WRITE(6, '(3A, I3, A, I3, A)')                              &
     &           'leecenren: Numero rendimientos muy grande   ',        &
     &           NomCen, ' = ', RendNTramo(IRen), ' > ',                &
     &           Dim%RendTramo - 1, '.'
            WRITE(ULog, '(3A, I3, A, I3, A)')                           &
     &           'leecenren: Numero rendimientos muy grande   ',        &
     &           NomCen, ' = ', RendNTramo(IRen), ' > ',                &
     &           Dim%RendTramo - 1, '.'
            STOP 1
         ENDIF
         VRendPend = DINFTY
         DO ITra = 1, RendNTramo(IRen)
            READ(URead, *)  Ind,                                        &
     &           (RendParam(ITra, IRen, IPar), IPar = 1, Dim%RendParam), &
     &           FEscala
            RendParam(ITra, IRen, PRendVol) =                           &
     &           RendParam(ITra, IRen, PRendVol)*                       &
     &           FEscala/1.0D3
            RendParam(ITra, IRen, PRendPend) =                          &
     &           RendParam(ITra, IRen, PRendPend)/1.0D3
            IF (VRendPend .LT. RendParam(ITra, IRen, PRendPend)) THEN
               WRITE(6, '(4A)') 'leecenren: Las pendientes deben ',     &
     &              'ir en orden decreciente, embalse ', NomEmb, '.'
               WRITE(ULog, '(4A)')                                      &
     &              'leecenren: Las pendientes deben ir en ',           &
     &              'orden decreciente, embalse ', NomEmb, '.'
               STOP 1
            ENDIF
            VRendPend = RendParam(ITra, IRen, PRendPend)
         ENDDO
         RendParam(RendNTramo(IRen) + 1, IRen, PRendVol) = GetInfty()
!     rendimiento inicial, que depende de la cota inicial y por lo
!     tanto es dato.
         Vol = EmbVIni(RendEmbInd(IRen))
         RendProm(IRen, 1) = FRendimientos(RendNTramo(IRen),            &
     &        RendParam(1, IRen, PRendVol),                             &
     &        RendParam(1, IRen, PRendPend),                            &
     &        RendParam(1, IRen, PRendConst), Vol)
      ENDDO
      CALL Cerrar(URead)
      RETURN
      END
