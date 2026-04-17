!***********************************************************************
!     Subrutina que, dado el embalse y el volumen, calcula los rendimien
!***********************************************************************
      SUBROUTINE Rendim(IEtapa, NBloque, RendNCen, FRendProm,           &
     &     RendProm, RendColInd, RendFilaInd, RendEmbInd, RendCenInd,   &
     &     RendNTramo, RendParam, FRendBndrs,                           &
     &     PmaxNCen, PmaxColInd, PmaxEmbInd,                            &
     &     PmaxCenInd, PmaxNTramo, PmaxParam,                           &
     &     BloInd, CenRen, CenPMin, CenPMax, VolIni, CenReni,           &
     &     FRestReserva, ParRes,                                                      &
     &     lp, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      LOGICAL FRestReserva
      TYPE(PAR_RESERVA), INTENT(IN) :: ParRes

!
      DOUBLE PRECISION, INTENT(IN):: RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION, INTENT(IN):: RendProm(Dim%EmbRend, Dim%Eta + 1)
      DOUBLE PRECISION, INTENT(IN):: VolIni(Dim%Emb)
      INTEGER, INTENT(IN) :: RendCenInd(Dim%EmbRend)
      INTEGER, INTENT(IN) :: RendColInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: RendFilaInd(Dim%EmbRend, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: RendNCen
      INTEGER, INTENT(IN) :: RendNTramo(Dim%EmbRend)
      INTEGER, INTENT(IN) :: RendEmbInd(Dim%EmbRend)

      INTEGER, INTENT(IN):: PmaxCenInd(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxColInd(Dim%EmbPmax, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: PmaxNCen
      INTEGER, INTENT(IN):: PmaxNTramo(Dim%EmbPmax)
      INTEGER, INTENT(IN):: PmaxEmbInd(Dim%EmbPmax)
      DOUBLE PRECISION, INTENT(IN):: PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)


      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: NBloque

      LOGICAL, INTENT(IN) :: FRendBndrs
      LOGICAL, INTENT(IN) :: FRendProm

      INTEGER BloInd(Dim%IBlo)
      DOUBLE PRECISION, INTENT(IN):: CenPMin(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)


!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

      DOUBLE PRECISION, INTENT(OUT):: CenReni(Dim%Cen)

!     local
      DOUBLE PRECISION Vol
      DOUBLE PRECISION FRendimientos
      DOUBLE PRECISION FPmaxvol
      INTEGER IBlo
      INTEGER IBRend
      INTEGER IBRendC
      INTEGER IRend
      INTEGER IRendEmb

      INTEGER IBPmax
      INTEGER IPmax
      INTEGER IPmaxEmb
      DOUBLE PRECISION PmaxEmb

      DOUBLE PRECISION RendEmb
      DOUBLE PRECISION MCoefVal (RendNCen*NBloque + ParRes%NumRend*2*NBloque*ParRes%NumZonas + ParRes%NumRend*NBloque + ParRes%NumRend*NBloque)
      INTEGER MCoefFilInd (RendNCen*NBloque + ParRes%NumRend*2*NBloque*ParRes%NumZonas + ParRes%NumRend*NBloque+ ParRes%NumRend*NBloque)
      INTEGER MCoefColInd (MAX(RendNCen, PMaxNCen)*NBloque + ParRes%NumRend*2*NBloque*ParRes%NumZonas + ParRes%NumRend*NBloque+ ParRes%NumRend*NBloque)

      INTEGER IBInd
      INTEGER ICentral
      INTEGER MColInd (MAX(RendNCen, PMaxNCen)*NBloque)
      DOUBLE PRECISION MCLowVal(MAX(RendNCen, PMaxNCen)*NBloque)
      DOUBLE PRECISION MCUppVal(MAX(RendNCen, PMaxNCen)*NBloque)

      DOUBLE PRECISION CenPmaxi(Dim%Cen, Dim%Blo)
      INTEGER Idx
      INTEGER IZona
      LOGICAL SkipEta
!

      CenPMaxi(1:Dim%Cen, 1:Dim%Blo) = CenPMax(1:Dim%Cen, 1:Dim%Blo)
      IBPmax = 0
      DO IPmax = 1, PmaxNCen
         IPmaxEmb = PmaxEmbInd(IPmax)
         Vol = VolIni(IPmaxEmb)
         PmaxEmb = FPmaxvol(PmaxNTramo(IPmax),           &
     &        PmaxParam(1, IPmax, PPmaxVol),             &
     &        PmaxParam(1, IPmax, PPmaxPend),            &
     &        PmaxParam(1, IPmax, PPmaxConst),           &
     &        Vol)

         DO IBlo = 1, NBloque
            IBPmax = IBPmax + 1

            MCoefColInd (IBPmax) = PmaxColInd(IPmax, IBlo, IEtapa)

            IBInd = BloInd(IBlo)
            ICentral = PmaxCenInd(IPmax)
            CenPMaxi(ICentral, IBInd) = MIN(PMaxEmb, CenPMax(ICentral, IBInd))
            MCUppVal(IBPmax) = CenPMaxi(ICentral, IBInd)/CenRen(ICentral)
         ENDDO
      ENDDO
      IF (lp .gt. 0 .and. IBPmax .gt. 0) THEN
         CALL ModifUpp(IBPMax, MCoefColInd, MCUppVal, lp)
      ENDIF


      CenReni(1:Dim%Cen) = CenRen(1:Dim%Cen)
      IBRend = 0
      IBRendC = 0
      DO IRend = 1, RendNCen
         IF (FRendProm) THEN
!           Se entregan los rendimientos promedio. No se consideran los
!           rendimientos de la  etapa inicial calculadas al final de la
!           rutina _leecenren_.
            RendEmb =  RendProm(IRend, IEtapa + 1)
         ELSE
            IF (IEtapa .EQ. 1) THEN
!              Se entregan los rendimientos de la etapa inicial calculadas
!              al final de la rutina _leecenren_.
               RendEmb = RendProm(IRend, 1)
            ELSE
!              IEtapa > 1
!              Se entregan los rendimientos de acuerdo al volumen del embalse.
               IRendEmb = RendEmbInd(IRend)
               Vol = VolIni(IRendEmb)
               RendEmb = FRendimientos(RendNTramo(IRend),      &
     &              RendParam(1, IRend, PRendVol),             &
     &              RendParam(1, IRend, PRendPend),            &
     &              RendParam(1, IRend, PRendConst),           &
     &              Vol)
            ENDIF
         ENDIF

         DO IBlo = 1, NBloque
            IBRendC = IBRendC + 1
            MCoefColInd (IBRendC) = RendColInd(IRend, IBlo, IEtapa)
            MCoefFilInd (IBRendC) = RendFilaInd(IRend, IBlo, IEtapa)
            MCoefVal (IBRendC) = RendEmb
            ICentral = RendCenInd(IRend)
            IBInd = BloInd(IBlo)
            IF (FRendBndrs) THEN
               IBRend = IBRend + 1
               CenReni(ICentral) = RendEmb
               MCLowVal(IBRend) = CenPMin(ICentral, IBInd)/CenReni(ICentral)
               MCUppVal(IBRend) = CenPMaxi(ICentral, IBInd)/CenReni(ICentral)
               MColInd(IBRend) = RendColInd(IRend, IBlo, IEtapa)
            ENDIF

            IF (FRestReserva) THEN
               SkipEta = .False.
               IF (ParRes%EtaIni .gt. 0 &
     &              .and. IEtapa .lt. ParRes%EtaIni) THEN
                  SkipEta = .True.
               ENDIF
               IF (ParRes%EtaFin .gt. 0 &
     &              .and. IEtapa .gt. ParRes%EtaFin) THEN
                  SkipEta = .True.
               ENDIF

               DO Idx = 1, ParRes%NumRend
                  IF (SkipEta .or. &
     &                 ParRes%RendCen(Idx) .ne. ICentral) THEN
                     CYCLE
                  ENDIF
                  DO IZona=1, ParRes%NumZonas
                     IBRendC = IBRendC + 1
                     MCoefColInd (IBRendC) = ParRes%RendColInd(Idx, IBInd,IZona)
                     MCoefFilInd (IBRendC) = ParRes%RendFilpInd(Idx, IBInd, IZona)
                     MCoefVal (IBRendC) = -RendEmb

                     IBRendC = IBRendC + 1
                     MCoefColInd (IBRendC) = ParRes%RendColInd(Idx, IBInd,IZona)
                     MCoefFilInd (IBRendC) = ParRes%RendFilnInd(Idx, IBInd,IZona)
                     MCoefVal (IBRendC) = RendEmb
                  ENDDO
                  ! se agrega el índice de la central para la restricción RC_cP_c^b - \sum_{c,z,b} R_{-,c,z}^b >= 0
                  IBRendC = IBRendC + 1
                  MCoefColInd (IBRendC) = ParRes%RendColCBajInd(Idx, IBInd)
                  MCoefFilInd (IBRendC) = ParRes%RendFilCBajInd(Idx, IBInd)
                  MCoefVal (IBRendC) = RendEmb
                  ! Tengo mi duda si del array las entradas que no uso
                  ! me modifican el lp con basura.
                  IF (ICentral .LE. (Dim%Ser + Dim%Emb)) THEN 
                     IBRendC = IBRendC + 1
                     MCoefColInd (IBRendC) = ParRes%RendColSubInd(Idx, IBInd)
                     MCoefFilInd (IBRendC) = ParRes%RendFilSubInd(Idx, IBInd)
                     MCoefVal (IBRendC) = RendEmb
                  ENDIF
               ENDDO
            ENDIF
         ENDDO


      ENDDO

      IF (lp .gt. 0 .and. IBRend .gt. 0) THEN
         CALL ModifCoef(IBRendC, MCoefColInd, MCoefFilInd, MCoefVal, lp)
         IF (FRendBndrs) THEN
            CALL ModifLow(IBRend, MColInd, MCLowVal, lp)
            CALL ModifUpp(IBRend, MColInd, MCUppVal, lp)
         ENDIF
      ENDIF

      RETURN
      END
