!***********************************************************************
!     Subrutina que, dado el embalse y el volumen, calcula las filtracio
!***********************************************************************
      SUBROUTINE Filtracv(IEtapa, FiltNCen, FlagFilt,                   &
     &     FiltProm, FiltVarFilInd,                                     & 
     &     FiltEmbInd, FiltNTramo,                                      &
     &     FiltParam, VolIni, FiltVals, PDLDAcColInd, ScaleVol,         &
     &     lp, Dim)

      USE PLP, ONLY : PAR_DIMS, C_SIZE_T, FILT_PROM, FILT_LINE,         &
     &     PFiltVol, PFiltPend, PFiltConst
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
!
      DOUBLE PRECISION, INTENT(IN):: FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION, INTENT(IN):: FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION, INTENT(IN):: VolIni(Dim%Emb)

      INTEGER, INTENT(IN) :: FiltVarFilInd(Dim%EmbFilt, Dim%Eta)
      INTEGER, INTENT(IN) :: FiltNCen
      INTEGER, INTENT(IN) :: FiltNTramo(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: FiltEmbInd(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: IEtapa

      INTEGER(C_SIZE_T), INTENT(IN) :: lp
      INTEGER, INTENT(IN) :: FlagFilt
      INTEGER, INTENT(IN) :: PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: ScaleVol(Dim%Emb)

!     out
      DOUBLE PRECISION, INTENT(OUT):: FiltVals(Dim%EmbFilt)

!     local
      
      INTEGER IFTramo
      INTEGER IFiltEmb
      
      INTEGER idx
      INTEGER IFil
      INTEGER ICol

      DOUBLE PRECISION Vol
      DOUBLE PRECISION value
      DOUBLE PRECISION FFiltracionesi      
      DOUBLE PRECISION pend
      DOUBLE PRECISION coef
      DOUBLE PRECISION const

!

      IF (FlagFilt .EQ. FILT_PROM) THEN
!     Se entregan las filtraciones promedio. No se consideran las
!     filtraciones de la  etapa inicial calculadas al final de la
!     rutina _leecenfil_.
         DO idx = 1, FiltNCen
            FiltVals (idx) = FiltProm(idx, IEtapa + 1)
            IFil = FiltVarFilInd(idx, IEtapa) - 1
            value = FiltVals (idx)
            CALL osi_lp_setrowrhs(lp, IFil, value)
         ENDDO
      ELSE
!        Se entregan las filtraciones de acuerdo al volumen del embalse.
         DO idx = 1, FiltNCen
            IFiltEmb = FiltEmbInd(idx)
            Vol = VolIni(IFiltEmb)
            FiltVals (idx) =                                       &
     &           FFiltracionesi(FiltNTramo(idx),                   &
     &           FiltParam(1, idx, PFiltVol),                      &
     &           FiltParam(1, idx, PFiltPend),                     &
     &           FiltParam(1, idx, PFiltConst), Vol, IFTramo)

            ICol = PDLDAcColInd(IFiltEmb, IEtapa) - 1
            IFil = FiltVarFilInd(idx, IEtapa) - 1
            IF (FlagFilt .EQ. FILT_LINE) THEN
               IF (IFTramo .GT. 0) THEN
                  pend = FiltParam(IFTramo, idx, PFiltPend)
                  coef = -pend * ScaleVol(IFiltEmb)
                  const = FiltParam(IFTramo, idx, PFiltConst)
               ELSE
                  coef = 0.0d0
                  const = 0.0d0
               ENDIF
               value = const
               CALL osi_lp_setcoefficient(lp, IFIl, ICol, coef)
            ELSE
               value = FiltVals(idx)
            ENDIF

            CALL osi_lp_setrowrhs(lp, IFil, value)
         ENDDO
      ENDIF

      RETURN
      END
