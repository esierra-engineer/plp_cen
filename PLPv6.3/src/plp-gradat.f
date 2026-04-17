      SUBROUTINE GraDatS(NEtapa, NSimul,                                &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd,                              &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH,                                                   &
     &     FExtrac, ExtrNCen, ExtrCenInd,                               &
     &     NBloque, BloInd, BloDur, BloEta, FactTiempo,                 &
     &     FAfluFict, FScaleQs, ScaleVol,                               &
     &     TipoEtapa, FPhi,                                             &
     &     NBarra, NLinea, NCentral, NCenEmb, NCenSer,                  &
     &     NCenPas, FPerdTram, FPerdLin,                                &
     &     BloPot, CenNom, CenInd, CenTipo, BarNom,                     &
     &     LinNom, LinNBar, LinFPer, LinNFlu,                           &
     &     CenRen, CenGBar, CenCVar,                                    &
     &     CenPMin,CenPMax, CenManSInd,                                         &
     &     EmbVIni, EmbFEsc,                                            &
     &     LinManSInd,                                                  &
     &     LinRes, LinTMax, LinTMaxS, LinVNom,                          &
     &     NFlujo, FRendProm, RendProm, RendNCen,                       &
     &     RendCenInd, RendEmbInd, RendNTramo, RendParam,               &
     &     PmaxNCen, PmaxCenInd, PmaxEmbInd, PmaxNTramo, PmaxParam,     &
     &     Mes, Cau2Vol, LajaLPar, LajaRPar,                            &
     &     MauleIPar, MauleRPar,                                        &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     AIncid, FGrabaCSV, FGrabaRES, UPreSuf,                       &
     &     SimulInd,                                                    &
     &     HidSPPQAfl, Kit, PasQAfl, PDNCol, PDNFila,                   &
     &     FInterfaz, FWarningFalla, nthreads, ULog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
      INTEGER NEmbVMinH
      INTEGER FConvLaja
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_LAJAM) ParLajaM
      INTEGER FConvMaule
      TYPE(PAR_MAULE) ParMaule
      LOGICAL FRestRalco
      TYPE(PAR_RALCO) ParRalco
      LOGICAL FRestGnl
      TYPE(PAR_GNL) ParGnl
      LOGICAL FRestReserva
      TYPE(PAR_RESERVA) ParReserva
      LOGICAL FBaterias
      TYPE(PAR_BATERIAS) ParBaterias

      INTEGER Mes(Dim%Eta)
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauConMauEta(Dim%Eta)
      DOUBLE PRECISION CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      LOGICAL LajaLPar(DimLLaja)
      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      INTEGER MauleIPar(DimIMaule)


      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*1 FPerdLin
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER CenInd(Dim%Cen)
      CHARACTER*48 LinNom(Dim%Lin)
      CHARACTER*12 TipoEtapa(Dim%Eta)
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      INTEGER CenManSInd(Dim%Simul)

      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo, Dim%LinManS)
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER CenGBar(Dim%Cen)
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBarra
      INTEGER NEtapa
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER BloEta(Dim%Blo)
      DOUBLE PRECISION FactTiempo

      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER NSimul

      INTEGER nthreads
      INTEGER FInterfaz
      LOGICAL FAfluFict
      LOGICAL FScaleQs
      LOGICAL FWarningFalla
      DOUBLE PRECISION ScaleVol(Dim%Emb)

      LOGICAL FFiltVar
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)

      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER ExtrCenInd(Dim%Extr)

      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NLinea
      LOGICAL FRendProm
      DOUBLE PRECISION RendProm(Dim%EmbRend, Dim%Eta + 1)
      INTEGER RendCenInd(Dim%EmbRend)
      INTEGER RendEmbInd(Dim%EmbRend)
      INTEGER RendNCen
      INTEGER RendNTramo(Dim%EmbRend)
      DOUBLE PRECISION RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)

      INTEGER PmaxCenInd(Dim%EmbPmax)
      INTEGER PmaxEmbInd(Dim%EmbPmax)
      INTEGER PmaxNCen
      INTEGER PmaxNTramo(Dim%EmbPmax)
      DOUBLE PRECISION PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)


      INTEGER UPreSuf
      LOGICAL FGrabaCSV
      LOGICAL FGrabaRES
      LOGICAL FPerdTram
      LOGICAL LinFPer(Dim%Lin)

      DOUBLE PRECISION HidSPPQAfl(Dim%HidSPP, Dim%Blo, Dim%Clase)
      INTEGER Kit(Dim%Simul, Dim%Kit)
      DOUBLE PRECISION PasQAfl(Dim%Pas, Dim%Blo, Dim%Clase)
      INTEGER PDNFila(Dim%Eta)
      INTEGER PDNCol(Dim%Eta)
      INTEGER ULog
      INTEGER LinManSInd(Dim%Simul)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinTMaxS(2, Dim%Flu, Dim%Lin, Dim%Blo)

!     local
      INTEGER NBloques
      INTEGER NCols
      INTEGER NFilas

      INTEGER ISimul

      DOUBLE PRECISION EtaPrimal(SUM(PDNCol(1:NEtapa)))
      DOUBLE PRECISION EtaDual(SUM(PDNFila(1:NEtapa)))
      DOUBLE PRECISION PasAfl(Dim%Pas, Dim%Blo)
      DOUBLE PRECISION BarPer(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPer(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2(Dim%Lin, Dim%Blo)

      DOUBLE PRECISION EtaPrimalS(SUM(PDNCol(1:NEtapa)))
      DOUBLE PRECISION EtaDualS(SUM(PDNFila(1:NEtapa)))
      DOUBLE PRECISION PasAflS(Dim%Pas, Dim%Blo)
      DOUBLE PRECISION BarPerS(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION LinPerS(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2S(Dim%Lin, Dim%Blo)

      INTEGER HorPro
      INTEGER MinPro
      INTEGER SegPro
      INTEGER TmpFinG(DimTmp)
      INTEGER TmpIniG(DimTmp)
      CHARACTER*(DimLargo) CHorPro
      CHARACTER*(DimLargo) CMinPro
      CHARACTER*(DimLargo) CSegPro

!     mensaje progreso
      CHARACTER*49 MensajeProgreso
      INTEGER NProgreso
      INTEGER NProgreso0
      INTEGER IDeltaProgreso
!

      CALL LeeTmp(TmpIniG)

      NBloques = SUM(NBloque(1:NEtapa))
      NCols = SUM(PDNCol(1:NEtapa))
      NFilas = SUM(PDNFila(1:NEtapa))

      EtaPrimalS(1:NCols) = 0.0d0
      EtaDualS(1:NFilas) = 0.0d0

      PasAflS(1:NCenPas, 1:NBloques) = 0.0d0
      BarPerS(1:NBarra, 1:NBloques) = 0.d0
      LinPerS(1:NLinea, 1:NBloques) = 0.d0
      LinPer2S(1:NLinea, 1:NBloques) = 0.d0

      IF (FGrabaRES) THEN
         CALL GraDatBDCen0(UPreSuf)
         CALL GraDatBDSer0(UPreSuf)
         CALL GraDatBDEmb0(UPreSuf)
         CALL GraDatBDCMg0(UPreSuf)
         CALL GraDatBDFlu0(UPreSuf)
      ENDIF

!     Reportes por simulacion
      MensajeProgreso =                                                 &
     &     'plp-main: Generando los reportes por simulacion: '
      NProgreso0 = NSimul
      NProgreso = 0
      IF (FGrabaCSV) NProgreso = NProgreso + NProgreso0
      IF (FGrabaRES) NProgreso = NProgreso + NProgreso0

      IF (FInterfaz .gt. 0) THEN
         CALL ReporteProgreso(MensajeProgreso, 0, NProgreso, Si, No)
      ENDIF


      DO ISimul = 1, NSimul
         CALL ExtrSimu (SimulInd, ISimul, NEtapa, NBloque, BloInd,      &
     &        NCenPas, NSimul, Kit,                                     &
     &        NCols, NFilas, PDNCol, PDNFila,                           &
     &        EtaDual, EtaPrimal,                                       &
     &        PasQAfl, PasAfl, Dim, ULog)

!
!     llamado rutina graba datos
!*******************************
         CALL GraDat(ISimul, NEtapa, NSimul,                            &
     &        FConvLaja, ParLaja, ParLajaM,                             &
     &        FConvMaule, ParMaule,                                     &
     &        FRestRalco, ParRalco,                                     &
     &        FRestGnl, ParGnl,                                         &
     &        FRestReserva, ParReserva,                                 &
     &        FBaterias, ParBaterias,                                   &
     &        FFiltVar, FiltNCen, FiltEmbInd,                           &
     &        FVertReb, NEmbVReb, EmbVRebInd,                           &
     &        NEmbVMinH,                                                &
     &        FExtrac, ExtrNCen, ExtrCenInd,                            &
     &        NBloque, BloInd, BloDur, BloEta, FactTiempo,              &
     &        FAfluFict, FScaleQs, ScaleVol,                            &
     &        TipoEtapa, FPhi,                                          &
     &        NBarra, NLinea, NCentral, NCenEmb, NCenSer,               &
     &        NCenPas, FPerdTram, FPerdLin, BarPer, LinPer,             &
     &        LinPer2, BloPot, CenNom, CenInd, CenTipo, BarNom,         &
     &        LinNom, LinNBar, LinFPer, LinNFlu,                        &
     &        CenRen, CenGBar, CenCVar,                                 &
     &        CenPMax(1, 1, CenManSInd(ISimul)),                        &
     &        CenPMin(1, 1, CenManSInd(ISimul)),                        &
     &        EmbVIni, EmbFEsc,                                         &
     &        NCols, NFilas, PDNCol, PDNFila,                           &
     &        EtaPrimal, EtaDual, PasAfl,                               &
     &        LinRes(1, 1, LinManSInd(ISimul)),                         &
     &        LinTMax(1, 1, 1, 1, LinManSInd(ISimul)),                  &
     &        LinVNom(1, 1, LinManSInd(ISimul)),                        &
     &        NFlujo, FRendProm, RendProm, RendNCen,                    &
     &        RendCenInd, RendEmbInd, RendNTramo, RendParam,            &
     &        PmaxNCen, PmaxCenInd, PmaxEmbInd, PmaxNTramo, PmaxParam,  &
     &        Mes, Cau2Vol, LajaLPar, LajaRPar,                         &
     &        MauleIPar, MauleRPar,                                     &
     &        CauConMauEta, CauRes105Eta,                               &
     &        AIncid, FGrabaCSV, FGrabaRES,                             &
     &        HidSPPQAfl, SimulInd,                                     &
     &        FWarningFalla, nthreads, Dim, ULog)

         CALL ExtrPromi (                                               &
     &     NSimul, NBloques,                                            &
     &     NCenPas, NBarra, NLinea,                                     &
     &     NCols, NFilas,                                               &
     &     EtaDual, EtaPrimal, PasAfl, BarPer, LinPer, LinPer2,         &
     &     EtaDualS, EtaPrimalS, PasAflS, BarPerS, LinPerS, LinPer2S,   &
     &     Dim)

         IDeltaProgreso = 1
         IF (FInterfaz .gt. 0) THEN
            CALL ReporteProgreso(' ', IDeltaProgreso, 0, .FALSE., .FALSE.)
         ENDIF
      ENDDO

      IF (FInterfaz .gt. 0) THEN
         CALL ReporteProgreso(' ', 0, 0, No, Si)
      ENDIF

!     Reportes simulacion promedio
      MensajeProgreso =                                                 &
     &     'plp-main: Generando los reportes promedio      : '
      NProgreso0 = 1
      NProgreso = 0
      IF (FGrabaCSV) NProgreso = NProgreso + NProgreso0
      IF (FGrabaRES) NProgreso = NProgreso + NProgreso0

      IF (FInterfaz .gt. 0) THEN
         CALL ReporteProgreso(MensajeProgreso, 0, NProgreso, Si, No)
      ENDIF
!
!     llamado rutina graba datos
      CALL GraDat(0, NEtapa, NSimul,                                    &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd,                              &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH,                                                   &
     &     FExtrac, ExtrNCen, ExtrCenInd,                               &
     &     NBloque, BloInd, BloDur, BloEta, FactTiempo,                 &
     &     FAfluFict, FScaleQs, ScaleVol,                               &
     &     TipoEtapa, FPhi,                                             &
     &     NBarra, NLinea, NCentral, NCenEmb, NCenSer,                  &
     &     NCenPas, FPerdTram, FPerdLin, BarPerS, LinPerS,              &
     &     LinPer2S, BloPot, CenNom, CenInd, CenTipo, BarNom,           &
     &     LinNom, LinNBar, LinFPer, LinNFlu,                           &
     &     CenRen, CenGBar, CenCVar,                                    &
     &     CenPMax(1, 1, CenManSInd(1)),                                &
     &     CenPMin(1, 1, CenManSInd(1)),                        &
     &     EmbVIni, EmbFEsc,                                            &
     &     NCols, NFilas, PDNCol, PDNFila,                              &
     &     EtaPrimalS, EtaDualS, PasAflS,                               &
     &     LinRes, LinTMaxS, LinVNom,                                   &
     &     NFlujo, FRendProm, RendProm, RendNCen,                       &
     &     RendCenInd, RendEmbInd, RendNTramo, RendParam,               &
     &     PmaxNCen, PmaxCenInd, PmaxEmbInd, PmaxNTramo, PmaxParam,     &
     &     Mes, Cau2Vol, LajaLPar, LajaRPar,                            &
     &     MauleIPar, MauleRPar,                                        &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     AIncid, FGrabaCSV, FGrabaRES,                                &
     &     HidSPPQAfl, SimulInd,                                        &
     &     FWarningFalla, nthreads, Dim, ULog)

      IF (FInterfaz .gt. 0) THEN
         CALL ReporteProgreso(' ', 0, 0, No, Si)
      ENDIF

      CALL LeeTmp(TmpFinG)
      CALL DifTmp(TmpFinG, TmpIniG, HorPro, MinPro, SegPro)
      CALL Num2Char (HorPro, CHorPro, Si, 2)
      CALL Num2Char (MinPro, CMinPro, Si, 2)
      CALL Num2Char (SegPro, CSegPro, Si, 2)
      WRITE(6, 98) 'plp-main: Grabacion de datos', CHorPro, CMinPro, CSegPro
      WRITE(ULog, 98) 'plp-main: Grabacion de datos', CHorPro, CMinPro, CSegPro
 98   FORMAT(A, ': ', A2, ':', A2, ':', A2)

      RETURN
      END


!**************************
!     Subrutina Graba Datos
!**************************
      SUBROUTINE GraDat(ISimul, NEtapa, NSimul,                         &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd,                              &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH,                                                   &
     &     FExtrac, ExtrNCen, ExtrCenInd,                               &
     &     NBloque, BloInd, BloDur, BloEta, FactTiempo,                 &
     &     FAfluFict, FScaleQs, ScaleVol,                               &
     &     TipoEtapa, FPhi,                                             &
     &     NBarra, NLinea, NCentral, NCenEmb, NCenSer,                  &
     &     NCenPas, FPerdTram, FPerdLin, BarPer, LinPer,                &
     &     LinPer2, BloPot, CenNom, CenInd, CenTipo, BarNom,            &
     &     LinNom, LinNBar, LinFPer, LinNFlu,                           &
     &     CenRen, CenGBar, CenCVar,                                    &
     &     CenPMax,CenPMin,                                             &
     &     EmbVIni, EmbFEsc,                                            &
     &     NCols, NFilas, PDNCol, PDNFila,                              &
     &     EtaPrimal, EtaDual, PasAfl,                                  &
     &     LinRes, LinTMax, LinVNom,                                    &
     &     NFlujo, FRendProm, RendProm, RendNCen,                       &
     &     RendCenInd, RendEmbInd, RendNTramo, RendParam,               &
     &     PmaxNCen, PmaxCenInd, PmaxEmbInd, PmaxNTramo, PmaxParam,     &
     &     Mes, Cau2Vol, LajaLPar, LajaRPar,                            &
     &     MauleIPar, MauleRPar,                                        &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     AIncid, FGrabaCSV, FGrabaRES,                                &
     &     EstocRHSP, SimulInd,                                         &
     &     FWarningFalla, nthreads, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
!
      INTEGER  NEmbVMinH

      INTEGER FConvLaja
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_LAJAM) ParLajaM
      INTEGER FConvMaule
      TYPE(PAR_MAULE) ParMaule
      LOGICAL FRestRalco
      TYPE(PAR_RALCO) ParRalco
      LOGICAL FRestGnl
      TYPE(PAR_GNL) ParGnl
      LOGICAL FRestReserva
      TYPE(PAR_RESERVA) ParReserva
      LOGICAL FBaterias
      TYPE(PAR_BATERIAS) ParBaterias

      INTEGER Mes(Dim%Eta)
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauConMauEta(Dim%Eta)
      DOUBLE PRECISION CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      LOGICAL LajaLPar(DimLLaja)
      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      INTEGER MauleIPar(DimIMaule)

      INTEGER NCols, NFilas
      INTEGER PDNFila(Dim%Eta)
      INTEGER PDNCol(Dim%Eta)
      DOUBLE PRECISION EtaPrimal(NCols)
      DOUBLE PRECISION EtaDual(NFilas)

      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*1 FPerdLin
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER CenInd(Dim%Cen)
      CHARACTER*48 LinNom(Dim%Lin)
      CHARACTER*12 TipoEtapa(Dim%Eta)
      DOUBLE PRECISION BarPer(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION FRendimientos
      DOUBLE PRECISION FPmaxvol
      DOUBLE PRECISION LinPer(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPer2(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinPTra(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION PasDat(Dim%Pas, DimDatPas, Dim%Blo)
      DOUBLE PRECISION PasAfl(Dim%Pas, Dim%Blo)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION PmaxCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION RendParam(Dim%RendTramo, Dim%EmbRend, Dim%RendParam)
      DOUBLE PRECISION SerDat(Dim%Ser, DimDatSer, Dim%Blo)
      DOUBLE PRECISION ExtrDat(Dim%Extr, Dim%Blo)
      DOUBLE PRECISION Vol
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER CenGBar(Dim%Cen)
      INTEGER IBlo
      INTEGER IEta
      INTEGER ICen
      INTEGER IRend
      INTEGER IPmax
      INTEGER IEmb
      INTEGER InvCenRend(Dim%Cen)
      INTEGER InvCenPMax(Dim%Cen)
      INTEGER ISimul
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBarra
      INTEGER NEtapa
      INTEGER NBloques
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER BloEta(Dim%Blo)
      DOUBLE PRECISION FactTiempo

      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER NSimul

      INTEGER nthreads
      LOGICAL FAfluFict
      LOGICAL FScaleQs
      DOUBLE PRECISION ScaleVol(Dim%Emb)

      LOGICAL FFiltVar
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)


      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER ExtrCenInd(Dim%Extr)

      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER RendCenInd(Dim%EmbRend)
      INTEGER RendEmbInd(Dim%EmbRend)
      LOGICAL FRendProm
      DOUBLE PRECISION RendProm(Dim%EmbRend, Dim%Eta + 1)
      INTEGER RendNCen
      INTEGER RendNTramo(Dim%EmbRend)
      LOGICAL FGrabaCSV
      LOGICAL FGrabaRES
      LOGICAL FPerdTram
      LOGICAL LinFPer(Dim%Lin)
      LOGICAL FWarningFalla

      INTEGER PmaxCenInd(Dim%EmbPmax)
      INTEGER PmaxEmbInd(Dim%EmbPmax)
      INTEGER PmaxNCen
      INTEGER PmaxNTramo(Dim%EmbPmax)
      DOUBLE PRECISION PmaxParam(Dim%PmaxTramo, Dim%EmbPmax, Dim%PmaxParam)

      DOUBLE PRECISION RendEmb
      DOUBLE PRECISION PmaxEmb
!
      NBloques = SUM(NBloque(1:NEtapa))

      CALL ExtrOper(NCols, NFilas, PDNCol, PDNFila,                     &
     &     EtaPrimal, EtaDual,                                          &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd,                              &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH,                                                   &
     &     FExtrac, ExtrNCen,                                           &
     &     NEtapa, NBloque, BloInd, BloDur, FactTiempo,                 &
     &     FAfluFict, FScaleQs, ScaleVol,                               &
     &     NBarra, NCentral, NCenEmb, NLinea, NFlujo, NCenSer, NCenPas, &
     &     CMg, PasAfl, CenPGen, EmbDat, SerDat, ExtrDat,               &
     &     PasDat, LinPTra, LinNFlu, Dim)
      IF (FPerdTram) THEN
         IF (ISimul .GT. 0) THEN
            CALL CalPTra(NBloques, NLinea, NBarra, FPerdLin,            &
     &           LinFPer, LinNBar, LinRes,                              &
     &           LinVNom, LinTMax,                                      &
     &           LinNFlu, LinPTra,                                      &
     &           BarPer, LinPer,                                        &
     &           LinPer2, Dim)
         ENDIF
      ELSE
         LinPer = 0.0d0
         LinPer2 = 0.0d0
         BarPer = 0.0d0
      ENDIF

      InvCenRend(1:NCentral) = 0
      InvCenPmax(1:NCentral) = 0
      DO IRend = 1, RendNCen
         InvCenRend(RendCenInd(IRend)) = IRend
      ENDDO

      DO IPmax = 1, PmaxNCen
         InvCenPmax(PmaxCenInd(IPmax)) = IPmax
      ENDDO
      DO ICen = 1, NCentral
         DO IBlo = 1, NBloques
            IEta = BloEta(IBlo)
            IF (InvCenRend(ICen) .GT. 0) THEN
               IRend = InvCenRend(ICen)
               IF (FRendProm) THEN
                  RendEmb = RendProm(IRend, IEta + 1)
               ELSE
               IF (IEta .EQ. 1) THEN
                  RendEmb = RendProm(IRend, 1)
               ELSE
                  IEmb = RendEmbInd(IRend)
                  Vol = EmbDat(IEmb, PEmbDatVol, IEta - 1)
                  RendEmb = FRendimientos(RendNTramo(IRend),          &
     &                 RendParam(1, IRend, PRendVol),                 &
     &                 RendParam(1, IRend, PRendPend),                &
     &                 RendParam(1, IRend, PRendConst), Vol)
               ENDIF
               ENDIF
               RenCen(ICen, IBlo) = RendEmb
            ELSE
               RenCen(ICen, IBlo) = CenRen(ICen)
            ENDIF

            IF (InvCenPmax(ICen) .GT. 0) THEN
               IPmax = InvCenPmax(ICen)
               IEmb = PmaxEmbInd(IPmax)
               IF (IEta .EQ. 1) THEN
                  Vol = EmbVIni(IEmb)
               ELSE
                  Vol = EmbDat(IEmb, PEmbDatVol, IEta - 1)
               ENDIF
               PmaxEmb = FPmaxvol(PmaxNTramo(IPmax),               &
     &              PmaxParam(1, IPmax, PPmaxVol),                 &
     &              PmaxParam(1, IPmax, PPmaxPend),                &
     &              PmaxParam(1, IPmax, PPmaxConst), Vol)
               PmaxCen(ICen, IBlo) = MIN(PmaxEmb, CenPmax(ICen, Iblo))
            ELSE
               PmaxCen(ICen, IBlo) = CenPmax(ICen, Iblo)
            ENDIF

         ENDDO
      ENDDO

#ifndef _OPENMP
      IF (nthreads .GT. 1) THEN
         nthreads = 1
      ENDIF
#endif

      IF (FGrabaCSV) THEN
!$OMP PARALLEL default(shared) NUM_THREADS(nthreads)
!$OMP SECTIONS
!     Graba Generacion Central
!*****************************
!$OMP SECTION
         CALL GraDatBDCen(NArcBDCenGen, ISimul,                         &
     &        NBloques, BloEta, BloDur, TipoEtapa,                      &
     &        NCentral, CenNom, CenTipo,                                &
     &        CenGBar, CenPGen, NBarra, BarNom, CMg,                    &
     &        RenCen, PmaxCen, CenCVar, FWarningFalla, Dim, ULog)

!     Graba Costos Operacionales
!*****************************
!$OMP SECTION
         CALL GraDatBDCop(NArcBDCostop, ISimul,                         &
     &        NBloques, BloEta, BloDur,                                 &
     &        NCentral, CenPGen, CenCVar, FPhi, Dim, ULog)

!     Graba Generacion Serie
!***************************
!$OMP SECTION
         CALL GraDatBDSer(NArcBDSerGen, ISimul,                         &
     &        NBloques, BloEta, TipoEtapa,                              &
     &        NCentral, NCenEmb, CenNom, CenTipo,                       &
     &        CenGBar, CenPGen, EmbDat, SerDat, NBarra, BarNom, FPhi,   &
     &        EstocRHSP, SimulInd, NSimul,                              &
     &        RenCen, AIncid, FactTiempo, Dim, ULog)
!     Graba Resultados Embalse
!*****************************
!$OMP SECTION
         CALL GraDatBDEmb(NArcBDEmb, ISimul,                            &
     &        NBloques, BloEta, TipoEtapa,                              &
     &        NCentral, NCenEmb, CenNom, CenTipo,                       &
     &        CenGBar, CenPGen, EmbFEsc, EmbVIni, EmbDat, SerDat, FPhi, &
     &        EstocRHSP, SimulInd, NSimul,                              &
     &        RenCen, AIncid, FactTiempo, Dim, ULog)
!     Graba Resultados Embalse Extras
!*****************************
!$OMP SECTION
         CALL GraDatBDEmbE(NArcBDEmbE, ISimul,                          &
     &        NBloques, BloEta, TipoEtapa,                              &
     &        NCentral, CenNom, CenTipo,                                &
     &        EmbFEsc, EmbDat, Dim, ULog)
!     Graba CMg
!**************
!$OMP SECTION
         CALL GraDatBDCMg(NArcBDCMg, ISimul,                            &
     &        NBloques, BloEta, BloDur, TipoEtapa, FPhi, NBarra,        &
     &        BarNom, CMg, BloPot, BarPer, Dim, ULog)
!     Graba Flujo Total Linea
!****************************
!$OMP SECTION
         CALL GraDatBDFlu(NArcBDLin, ISimul,                            &
     &        NBloques, BloEta, BloDur, TipoEtapa,                      &
     &        FPerdTram, FPerdLin, NLinea, LinNom, LinNBar,             &
     &        LinNFlu, LinPTra, LinPer, LinPer2, LinTMAx, CMg, Dim, ULog)

!     Graba Extracciones
!****************************
!$OMP SECTION
         IF (ExtrNCen .GT. 0) THEN
            CALL GraDatBDExtrac(NArcBDExtrac, ISimul,                   &
     &           NBloques, BloEta, TipoEtapa,                           &
     &           CenInd, ExtrNCen, ExtrCenInd, ExtrDat, Dim, ULog)
         ENDIF

!     Graba Convenio Laja
!****************************
!$OMP SECTION
         IF (FConvLaja .EQ. 1) THEN
            CALL GraDatBDLajaN(ParLaja%NArcLajaO, ISimul,               &
     &           NBloques, BloDur, BloEta, TipoEtapa,                   &
     &           EmbFEsc, ParLaja, Dim, ULog)
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            CALL GraDatBDLajaMN(ParLajaM%NArcLajaO, ISimul,             &
     &           NBloques, BloDur, BloEta, TipoEtapa, Mes,              &
     &           EmbFEsc, ParLajaM, Dim, ULog)
         ENDIF

!     Graba Convenio Maule
!****************************
!$OMP SECTION
         IF (FConvMaule .NE. 0) THEN
            CALL GraDatBDMauleN(ParMaule%NArcMauleO, ISimul,            &
     &           NBloques, BloDur, BloEta, TipoEtapa, Mes,              &
     &           EmbFEsc, ParMaule, Dim, ULog)
         ENDIF

!     Graba Convenio Gnl
!****************************
!$OMP SECTION
         IF (FRestGnl) THEN
            CALL GraDatBDGnlN(ParGnl%NArcGnlO, ISimul, NSimul,          &
     &           NBloques, BloDur, BloEta, TipoEtapa,                   &
     &           FPhi, FactTiempo, CenPGen,                             &
     &           ParGnl, Dim, ULog)
         ENDIF

!     Graba reservas
!****************************
!     $OMP SECTION
         IF (FRestReserva) THEN
            CALL GraDatBDResN(ISimul,                                 &
     &           NBloques,                                            &
     &           CenNom, BloEta, FPhi,                                &
     &           ParReserva, Dim, ULog)
            CALL GraDatBDCPRes(ISimul, NBloques, BloEta,              &
     &           BloDur, ParReserva, FPhi, Dim, ULog)
         ENDIF
!     Graba Baterias
!****************************
!     $OMP SECTION
         IF (FBaterias) THEN
            CALL GraDatBDBatN(ISimul,NBloques,BloEta,CenPGen,PmaxCen,CenPMin,         &
     &            FPhi,ParBaterias,Dim,ULog)
         ENDIF

!     Graba Convenio del Laja Antiguo
!*****************************
!$OMP SECTION
         IF (ISimul .GT. 0) THEN
            CALL GraDatBDLaja(NArcLajaOut, ISimul,                      &
     &           NEtapa, TipoEtapa, Mes, Cau2Vol, LajaLPar, LajaRPar,   &
     &           Dim, ULog)
         ENDIF

!     Graba Convenio del Maule Antiguo
!*****************************
!$OMP SECTION
         IF (ISimul .GT. 0) THEN
            CALL GraDatBDMaul(NArcMauleOut, ISimul,                        &
     &           NEtapa, TipoEtapa, Mes, Cau2Vol,                          &
     &           CauConMauEta, CauRes105Eta,                               &
     &           CenPGen, EmbFEsc, EmbDat,                                 &
     &           MauleIPar, MauleRPar, Dim, ULog)
         ENDIF

!$OMP END SECTIONS
!$OMP END PARALLEL
      ENDIF


      IF (FGrabaRES) THEN

!$OMP PARALLEL default(shared) NUM_THREADS(nthreads)
!$OMP SECTIONS
!     Graba Generacion Central
!*****************************
!$OMP SECTION
         CALL GraDatBDCen2(NBloques, BloDur,                            &
     &        NCentral, CenTipo,                                        &
     &        CenGBar, CenPGen, CMg,                                    &
     &        RenCen, Dim, ULog)
!     Graba Generacion Serie
!***************************
!$OMP SECTION
         CALL GraDatBDSer2(NBloques, BloEta,                            &
     &        NCentral, NCenEmb, CenTipo,                               &
     &        CenGBar, CenPGen, EmbDat, SerDat, FPhi,                   &
     &        RenCen, AIncid, FactTiempo, Dim, ULog)
!     Graba Resultados Embalse
!*****************************
!$OMP SECTION
         CALL GraDatBDEmb2(NBloques, BloEta, ISimul,                    &
     &        NCentral, NCenEmb, CenTipo,                               &
     &        CenGBar, CenPGen, EmbFEsc, EmbVIni, EmbDat, SerDat, FPhi, &
     &        EstocRHSP, SimulInd, NSimul,                              &
     &        ExtrNCen, ExtrCenInd, ExtrDat,                            &
     &        RenCen, AIncid, FactTiempo, Dim, ULog)
!     Graba CMg
!**************
!$OMP SECTION
         CALL GraDatBDCMg2(                                             &
     &        NBloques, BloEta, BloDur, FPhi, NBarra,                   &
     &        CMg, BloPot, BarPer, Dim, ULog)
!     Graba Flujo Total Linea
!****************************
!$OMP SECTION
         CALL GraDatBDFlu2(NBloques, BloDur,                            &
     &        FPerdTram, FPerdLin, NLinea, LinNBar,                     &
     &        LinNFlu, LinPTra, LinPer, LinPer2, LinTMax, CMg,          &
     &        Dim, ULog)

!$OMP END SECTIONS
!$OMP END PARALLEL
      ENDIF
      RETURN
      END
