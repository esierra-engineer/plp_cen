!***********************************************************************
!     Subrutina que, dado el embalse y el volumen, calcula las filtracio
!***********************************************************************
      SUBROUTINE Filtrac(IEtapa, NBloque, FiltNCen, FlagFiltProm,       &
     &     FiltProm, FiltColInd, FiltEmbInd, FiltNTramo,                &
     &     FiltParam, VolIni,                                           &
     &     lp, Dim)
      USE PLP, ONLY : PAR_DIMS, C_SIZE_T, PFiltVol, PFiltPend, PFiltConst


      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!
      DOUBLE PRECISION, INTENT(IN):: FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION, INTENT(IN):: FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION, INTENT(IN):: VolIni(Dim%Emb)
      INTEGER, INTENT(IN) :: FiltColInd(Dim%EmbFilt, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN) :: FiltNCen
      INTEGER, INTENT(IN) :: FiltNTramo(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: FiltEmbInd(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: NBloque

      LOGICAL, INTENT(IN) :: FlagFiltProm

! out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp


! local
      DOUBLE PRECISION Vol
      DOUBLE PRECISION FFiltraciones
      INTEGER IBFilt
      INTEGER IBlo
      INTEGER IFiltCen
      INTEGER IFiltEmb

      INTEGER MCUppInd (FiltNCen*NBloque)
      DOUBLE PRECISION MCUppVal (FiltNCen*NBloque)

!
      IBFilt = 0
      IF (FlagFiltProm) THEN
!     Se entregan las filtraciones promedio. No se consideran las
!     filtraciones de la  etapa inicial calculadas al final de la
!     rutina _leecenfil_.
         IBFilt = 0
         DO IFiltCen = 1, FiltNCen
            DO IBlo = 1, NBloque
               IBFilt = IBFilt + 1
               MCUppVal (IBFilt) = FiltProm(IFiltCen, IEtapa + 1)
               MCUppInd (IBFilt) = FiltColInd(IFiltCen, IBlo, IEtapa)
            ENDDO
         ENDDO
      ELSE
         IF (IEtapa .EQ. 1) THEN
!     Se entregan las filtraciones de la etapa inicial calculadas
!     al final de la rutina _leecenfil_.
            IBFilt = 0
            DO IFiltCen = 1, FiltNCen
               DO IBlo = 1, NBloque
                  IBFilt = IBFilt + 1
                  MCUppVal (IBFilt) = FiltProm(IFiltCen, 1)
                  MCUppInd (IBFilt) = FiltColInd(IFiltCen, IBlo, IEtapa)
               ENDDO
            ENDDO
         ELSE
!     IEtapa > 1
!     Se entregan las filtraciones de acuerdo al volumen del embalse.
            IBFilt = 0
            DO IFiltCen = 1, FiltNCen
               DO IBlo = 1, NBloque
                  IBFilt = IBFilt + 1
                  IFiltEmb = FiltEmbInd(IFiltCen)
                  Vol = VolIni(IFiltEmb)
                  MCUppVal (IBFilt) =                                   &
     &              FFiltraciones(FiltNTramo(IFiltCen),                 &
     &              FiltParam(1, IFiltCen, PFiltVol),                   &
     &              FiltParam(1, IFiltCen, PFiltPend),                  &
     &              FiltParam(1, IFiltCen, PFiltConst), Vol)
                  MCUppInd (IBFilt) = FiltColInd(IFiltCen, IBlo, IEtapa)
               ENDDO
            ENDDO
         ENDIF
      ENDIF
      CALL ModifUpp(IBFilt, MCUppInd,  MCUppVal, lp)
      CALL ModifLow(IBFilt, MCUppInd,  MCUppVal, lp)

      RETURN
      END
