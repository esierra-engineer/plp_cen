!     Calcula los caudales maximos y minimos turbinables de la
!     central El Toro tales que respeten el convenio del Laja.
      SUBROUTINE Laja1(IEtapa, NBloque, BloInd, BloDur, CenRen,         &
     &     Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,             &
     &     Mes,                                                         &
     &     CauDemNuReEta, CauDemRegAbaEta, CauDemRegTucaEta,            &
     &     ISimul, SimulInd, EstocRHSP, VolFiltLaja, FFasePrimal,       &
     &     FlagFiltProm, FiltProm, FiltParam, FiltNTramo,               &
     &     lp, Dim)

      USE PLP
      USE OSI
      INCLUDE 'machcons.fpp'

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
      
      
!     Convenio del Laja.
      DOUBLE PRECISION, INTENT(IN):: Cau2Vol(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemRegAbaEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauDemRegTucaEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION, INTENT(IN):: FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      TYPE(PAR_LAJAC), INTENT(IN):: LajaCPar
      INTEGER, INTENT(IN) :: FiltNTramo(Dim%EmbFilt)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      INTEGER, INTENT(IN) :: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      INTEGER, INTENT(IN) :: ISimul
      INTEGER, INTENT(IN) :: LajaIPar(DimILaja)
      INTEGER, INTENT(IN) :: Mes(Dim%Eta)
      INTEGER, INTENT(IN) :: SimulInd(Dim%Simul, Dim%Eta)
      LOGICAL, INTENT(IN):: FFasePrimal
      LOGICAL, INTENT(IN):: FlagFiltProm
      LOGICAL, INTENT(IN):: LajaLPar(DimLLaja)

      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)

!     out

      DOUBLE PRECISION, INTENT(OUT):: LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     Locales
      DOUBLE PRECISION FFiltraciones
      INTEGER IIEtapa
      LOGICAL NuevaLinea

      DOUBLE PRECISION CauAflAba
      DOUBLE PRECISION CauAflAntuco
      DOUBLE PRECISION CauAflCaptAltoPolc
      DOUBLE PRECISION CauAflRucue
      DOUBLE PRECISION CauAflTucapel
      DOUBLE PRECISION CauDemNuReEtai
      DOUBLE PRECISION CauDemRegTucaEtai
      DOUBLE PRECISION CauExtrMax
      DOUBLE PRECISION CauExtrMin
      DOUBLE PRECISION CauFiltLaja
      DOUBLE PRECISION CauAflHoInAba
      DOUBLE PRECISION CauAflHoInTucapel
      DOUBLE PRECISION CauVac
      DOUBLE PRECISION EcoEND
      DOUBLE PRECISION EcoENDGlo
      DOUBLE PRECISION EcoENDLoc
      DOUBLE PRECISION EcoNuRe
      DOUBLE PRECISION GenMaxElToro
      DOUBLE PRECISION pp
      DOUBLE PRECISION VolAflCaptAltoPolc
      DOUBLE PRECISION VolAflLaja
      DOUBLE PRECISION VolDelta
      DOUBLE PRECISION VolDemNuReEta
      DOUBLE PRECISION VolDemReg
      DOUBLE PRECISION VolDemRegTucaEta
      DOUBLE PRECISION VolDerDiaENDEta
      DOUBLE PRECISION VolExtrAnuEND
      DOUBLE PRECISION VolExtrMax
      DOUBLE PRECISION VolExtrMenEND
      DOUBLE PRECISION VolExtrMenNuRe
      DOUBLE PRECISION VolExtrRemAnuEND
      DOUBLE PRECISION VolExtrRemMenEND
      DOUBLE PRECISION VolFiltLaja
      DOUBLE PRECISION VolFiltLajaRMes
      DOUBLE PRECISION VolFiltLajaRYear
      DOUBLE PRECISION VolRealMax
      DOUBLE PRECISION VolResAba
      DOUBLE PRECISION CauRieDefAban
      DOUBLE PRECISION CauRieDefTuca
      DOUBLE PRECISION CauRieCCCE
      DOUBLE PRECISION CauRieLaDi
      DOUBLE PRECISION CauRieOpcionalLaja
      DOUBLE PRECISION CauRieZaCo
      DOUBLE PRECISION VoluLaja
      DOUBLE PRECISION VolUtil
      DOUBLE PRECISION VolVac
      DOUBLE PRECISION CauRieDefAnRe
      DOUBLE PRECISION CauDerAnRe
      DOUBLE PRECISION CauExcAnRe
      DOUBLE PRECISION CauRieDefNuRe
      DOUBLE PRECISION GenMaxZaCo
      INTEGER IClase(Dim%EstocFila)
      INTEGER IComp
      INTEGER IFiltLaja
      INTEGER probnum
      INTEGER UWrite1
      INTEGER UWrite2
      LOGICAL Imprime
      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION GetUppBnd

      INTEGER UDebLog

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!
      UDebLog = LajaCPar%UDebLog

      IF (.NOT. LajaLPar(IUsoConvLaja)) THEN
         RETURN
      ENDIF
      UWrite1 = LajaIPar(IILajaWrite1)
      UWrite2 = LajaIPar(IILajaWrite2)
!$$$  IF (SimulInd(ISimul, IEtapa) .EQ.
!$$$  $     LajaIPar(IIndSimImpLaja)) THEN
!     IComp = LajaIPar(IIVolLaja)
      IComp = LajaCPar%IEtaInd(IIVolLaja, IEtapa)
      VolRealMax = GetUppBnd(IComp, lp)
      IF (FFasePrimal .AND.                                             &
     &     (ISimul .EQ. LajaIPar(IIndSimImpLaja))) THEN
         Imprime = Si
      ELSE
         Imprime = No
      ENDIF
      IF (FFasePrimal) THEN
         NuevaLinea = .FALSE.
!
!     Extraccion de datos de LajaRPar.
         IF (IEtapa .EQ. 1) THEN
!     Se leen los datos iniciales no perturbados.
            IIEtapa = 0
         ELSE
!     Se leen los datos iniciales de la etapa, eventualmente perturbados
            IIEtapa = IEtapa
         ENDIF
         EcoENDGlo = LajaRPar(IEcoENDGlo, ISimul, IIEtapa)
         EcoENDLoc = LajaRPar(IEcoENDLoc, ISimul, IIEtapa)
         EcoNuRe = LajaRPar(IEcoNuRe, ISimul, IIEtapa)
         VoluLaja = LajaRPar(IVoluLaja, ISimul, IIEtapa)
         VolExtrAnuEND = LajaRPar(IVolExtrAnuEND, ISimul, IIEtapa)
         VolExtrMenEND = LajaRPar(IVolExtrMenEND, ISimul, IIEtapa)
         VolExtrMenNuRe = LajaRPar(IVolExtrMenNuRe, ISimul, IIEtapa)
!
         IClase = SimulInd(ISimul, IEtapa)
!     VolAflLaja INCLUYE las captaciones de Alto Polcura.
!         VolAflLaja = EstocRHSP(LajaIPar(IIAflLaja), IClase, IEtapa)
         VolAflLaja = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflLaja), Dim) * Cau2Vol(IEtapa)

!        VolAflCaptAltoPolc =                                           &
!    &        EstocRHSP(LajaIPar(IIAflCaptAltoPolc), IClase, IEtapa)
         VolAflCaptAltoPolc = QAfluEta(IEtapa, NBloque, BloInd,           &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflCaptAltoPolc), Dim) * Cau2Vol(IEtapa)

!         CauAflCaptAltoPolc =                                           &
!    &        EstocRHSP(LajaIPar(IIAflCaptAltoPolc), IClase, IEtapa)/   &
!    &        Cau2Vol(IEtapa)
         CauAflCaptAltoPolc = QAfluEta(IEtapa, NBloque, BloInd,           &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflCaptAltoPolc), Dim) 

!         CauAflHoInTucapel =                                           &
!     &        EstocRHSP(LajaIPar(IIAflTucapel), IClase, IEtapa)/       &
!     &        Cau2Vol(IEtapa)

         CauAflHoInTucapel = QAfluEta(IEtapa, NBloque, BloInd,            &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflTucapel), Dim) 

!         CauAflHoInAba =                                               &
!     &        EstocRHSP(LajaIPar(IIAflAbanico), IClase, IEtapa)/       &
!     &        Cau2Vol(IEtapa)

         CauAflHoInAba = QAfluEta(IEtapa, NBloque, BloInd,                &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflAbanico), Dim) 


!     CauAflAntuco NO INCLUYE las captaciones de Alto Polcura.
!         CauAflAntuco =                                                 &
!     &        EstocRHSP(LajaIPar(IIAflAntuco), IClase, IEtapa)/         &
!     &        Cau2Vol(IEtapa)

         CauAflAntuco = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflAntuco), Dim) 

!        CauAflRucue =                                                  &
!     &       EstocRHSP(LajaIPar(IIAflRucue), IClase, IEtapa)/          &
!     &        Cau2Vol(IEtapa)

         CauAflRucue = QAfluEta(IEtapa, NBloque, BloInd,                  &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     LajaIPar(IIAflRucue), Dim) 
!
         EcoEND = EcoENDLoc + EcoENDGlo
         IFiltLaja = LajaIPar(IIFiltLaja)
         IF (FlagFiltProm) THEN
            CauFiltLaja = FiltProm(IFiltLaja, IEtapa + 1)
         ELSE
            IF (IEtapa .EQ. 1) THEN
               CauFiltLaja = FiltProm(IFiltLaja, 1)
            ELSE
               CauFiltLaja =                                            &
     &              FFiltraciones(FiltNTramo(IFiltLaja),                &
     &              FiltParam(1, IFiltLaja, PFiltVol),                  &
     &              FiltParam(1, IFiltLaja, PFiltPend),                 &
     &              FiltParam(1, IFiltLaja, PFiltConst), VoluLaja)
            ENDIF
         ENDIF

!     El volumen filtrado debe ser menor que el volumen total mas
!     el volumen afluente.
         VolFiltLaja = MIN(VoluLaja + VolAflLaja,                       &
     &        CauFiltLaja*Cau2Vol(IEtapa))
         VolFiltLajaRMes = CauFiltLaja*LajaRPar(ICau2VolRM, 1, IEtapa)
         VolFiltLajaRYear = CauFiltLaja*LajaRPar(ICau2VolRY, 1, IEtapa)
!     0 < VoluLaja + VolAflLaja - VolFiltLaja
         CauFiltLaja = VolFiltLaja/Cau2Vol(IEtapa)
         CauAflAba = CauAflHoInAba + CauFiltLaja
         CauAflTucapel = CauAflHoInAba + CauFiltLaja +                  &
     &        CauAflAntuco + CauAflHoInTucapel + CauAflRucue
!     r: EcoEND + EcoNuRe < VoluLaja
         VolUtil = VoluLaja - (EcoEND + EcoNuRe)
!     0 < VolUtil
         VolDemRegTucaEta = CauDemRegTucaEta(IEtapa)*Cau2Vol(IEtapa)
         VolDemNuReEta = pp(MIN(CauDemNuReEta(IEtapa)*Cau2Vol(IEtapa),  &
     &           VolExtrMenNuRe + EcoNuRe))
!     El siguiente IF recalcula los derechos de riego si
!     el nivel del embalse esta por debajo del colchon inferior.
         IF (VolUtil .LT. LajaCPar%VolColInf) THEN
            VolDemReg = VolDemRegTucaEta + VolDemNuReEta + DEPSILON
            VolDemRegTucaEta = CauDemRegAbaEta(IEtapa)*Cau2Vol(IEtapa)* &
     &           VolDemRegTucaEta/VolDemReg
            VolDemNuReEta = CauDemRegAbaEta(IEtapa)*Cau2Vol(IEtapa)*    &
     &           VolDemNuReEta/VolDemReg
            VolResAba = Cau2Vol(IEtapa)*                                &
     &           pp(LajaCPar%GastoAba - MAX(CauAflAba, CauDemRegAbaEta(IEtapa)))
            EcoENDLoc = EcoENDLoc + VolResAba
         ELSE
            EcoENDLoc = 0d0
         ENDIF
         CauDemRegTucaEtai = VolDemRegTucaEta/Cau2Vol(IEtapa)
         CauDemNuReEtai = VolDemNuReEta/Cau2Vol(IEtapa)
         IF (LajaLPar(ICaptAltoPolc)) THEN
            CauRieDefAban = pp(                                         &
     &           CauDemRegAbaEta(IEtapa) + CauAflCaptAltoPolc -         &
     &           CauAflAba)
         ELSE
            CauRieDefAban = pp(                                         &
     &           CauDemRegAbaEta(IEtapa) -                              &
     &           CauAflAba)
         ENDIF
         CauRieDefTuca = pp(CauDemRegTucaEtai - CauAflTucapel)
         CauRieDefAnRe = MIN(CauRieDefAban, CauRieDefTuca)
         CauDerAnRe = MIN(                                              &
     &        CauDemRegTucaEtai,                                        &
     &        CauRieDefAnRe + CauAflTucapel)
         CauExcAnRe = pp(CauRieDefAnRe + CauAflTucapel - CauDerAnRe)
         CauRieDefNuRe = pp(CauDemNuReEtai - CauExcAnRe)
         CauExtrMin =                                                   &
     &        CauRieDefAnRe + CauRieDefNuRe
         EcoEND = EcoENDGlo + EcoENDLoc
         VolExtrRemMenEND = pp(VolExtrMenEND - VolFiltLajaRMes +        &
     &        LajaRPar(IVolAflCaptAltoPolcRM, ISimul, IEtapa) -         &
     &        LajaRPar(IVolExtrMinRM, ISimul, IEtapa))
         VolExtrRemAnuEND = pp(VolExtrAnuEND - VolFiltLajaRYear +       &
     &        LajaRPar(IVolAflCaptAltoPolcRY, ISimul, IEtapa) -         &
     &        LajaRPar(IVolExtrMinRY, ISimul, IEtapa))
         VolExtrMenEND = pp(VolExtrMenEND - VolFiltLaja +               &
     &                      VolAflCaptAltoPolc)
         VolExtrAnuEND = pp(VolExtrAnuEND - VolFiltLaja +               &
     &                      VolAflCaptAltoPolc)
         VolDerDiaENDEta = LajaCPar%CauDerDiaEND*Cau2Vol(IEtapa) -      &
     &        VolFiltLaja + VolAflCaptAltoPolc
         VolExtrMax = DINFTY
         IF (LajaLPar(ISetCauMaxDia)) THEN
            VolExtrMax = MIN(VolExtrMax, VolDerDiaENDEta)
         ENDIF
         IF (LajaLPar(ISetCauMaxMen)) THEN
            VolExtrMax = MIN(VolExtrMax, VolExtrRemMenEND)
         ENDIF
         IF (LajaLPar(ISetCauMaxAnu)) THEN
            VolExtrMax = MIN(VolExtrMax, VolExtrRemAnuEND)
         ENDIF
         IF (VolExtrAnuEnd .LE. 0.0d0) THEN
            VolExtrMax = pp(VolExtrMax + EcoEND - VolFiltLaja)
         ELSE
            VolExtrMax = pp(VolExtrMax + EcoEND)
         ENDIF
         IF (Imprime) THEN
            WRITE(UDebLog, '(I4, '','', $)') IEtapa
            WRITE(UDebLog, '(F10.2, '','', $)')                         &
     &           LajaRPar(ICau2VolRM, 1, IEtapa)
            WRITE(UDebLog, '(F10.2, '','', $)')                         &
     &           LajaRPar(ICau2VolRY, 1, IEtapa)
            WRITE(UDebLog, '(F10.2, '','', $)')                         &
     &           LajaRPar(IVolExtrMinRM, ISimul, IEtapa)
            WRITE(UDebLog, '(F10.2, '','', $)')                         &
     &           LajaRPar(IVolExtrMinRY, ISimul, IEtapa)
            WRITE(UDebLog, '(F10.2, '','', $)')                         &
     &           LajaRPar(IVolAflCaptAltoPolcRM, ISimul, IEtapa)
            WRITE(UDebLog, '(F10.2, '','', $)')                         &
     &           LajaRPar(IVolAflCaptAltoPolcRY, ISimul, IEtapa)
            WRITE(UDebLog, '(F10.2, '','', $)') VolFiltLajaRMes
            WRITE(UDebLog, '(F10.2, '','', $)') VolFiltLajaRYear
            WRITE(UDebLog, '(F10.2, '','', $)') VolExtrRemMenEND
            WRITE(UDebLog, '(F10.2, '','', $)') VolExtrRemAnuEND
            WRITE(UDebLog, '(F10.2, '','', $)') VolAflCaptAltoPolc
            WRITE(UDebLog, '(F10.2, '','', $)') VolFiltLaja
            WRITE(UDebLog, '(F10.2, '','', $)') VolExtrMenEND
            WRITE(UDebLog, '(F10.2, '','', $)') VolExtrAnuEND
            WRITE(UDebLog, *)
         ENDIF
!     Si VolExtrMax = 0 (Se acabaron los derechos de agua de
!     ENDESA), pero el lago esta vertiendo, hay que permitirselo.
         VolExtrMax = VolExtrMax + pp(VoluLaja - VolRealMax)
!     r: 0 < VoluLaja + VolAflLaja - VolFiltLaja
         VolVac = VoluLaja + VolAflLaja - VolFiltLaja
!     0 < VolVac
         CauVac = VolVac/Cau2Vol(IEtapa)
         VolExtrMax = MIN(VolExtrMax, VolVac)
!     VolExtrMax < VoluLaja + VolAflLaja - VolFiltLaja
         IF (CauExtrMin .GT. CauVac) THEN
!     No se cumplen las restricciones de riego. Embalse agotado.
            IF (Imprime) THEN
               WRITE(UWrite1, '(A, $)') ' VolEmbAgo'
            ENDIF
            CauExtrMin = CauVac
         ENDIF
         CauExtrMax = VolExtrMax/Cau2Vol(IEtapa)
         CauExtrMax = MAX(CauExtrMax, CauExtrMin)
         IComp = LajaIPar(IIGenElToro)
         GenMaxElToro = LajaCPar%UppGenElToro(IEtapa, ISimul)/CenRen(IComp)
         CauExtrMax = MIN(CauExtrMax, GenMaxElToro)
         CauExtrMin = MIN(CauExtrMin, GenMaxElToro)
         IF (VoluLaja .LT. LajaCPar%VolColInf) THEN
            CauExtrMax = MAX(CauExtrMin,CauRieDefAban)
         ENDIF
!jap         DeltaM = (VolAflLaja - VolFiltLaja)/Cau2Vol(IEtapa)
!jap         m1 = DeltaM - CauExtrMax
!jap         m0 = MIN(0.0d0, DeltaM - CauExtrMin)
!$$$  v1 = VolUtil
!jap         v1 = VoluLaja
!jap         v0 = VolColInf
!jap         IF (m1 .LT. 0.0d0) THEN
!jap            DeltaT0 = MAX(0.0d0, MIN(Cau2Vol(IEtapa), (v0 - v1)/m1))
!jap            v2 = v1 + (m1*DeltaT0 + m0*(Cau2Vol(IEtapa) - DeltaT0))
!jap         ELSE
!jap            v2 = v1 + m1*Cau2Vol(IEtapa)
!jap         ENDIF
!jap         m2 = (v2 - v1)/Cau2Vol(IEtapa)
!jap         CauExtrMax = MIN(CauExtrMax, DeltaM - m2)
!jap         m1 = DeltaM - CauExtrMin
!jap         m0 = DeltaM - CauExtrMax
!$$$  v1 = VolUtil
!jap         v1 = VoluLaja
!jap         v0 = VolColInf
!jap         IF ((m1 .GT. 0) .AND.
!jap     $        (v0 - v1 .GT. 0)) THEN
!jap            DeltaT0 = MIN(Cau2Vol(IEtapa), (v0 - v1)/m1)
!jap            v2 = v1 + (m1*DeltaT0 + m0*(Cau2Vol(IEtapa) - DeltaT0))
!jap         ELSE
!jap            v2 = v1 + m1*Cau2Vol(IEtapa)
!jap         ENDIF
!jap         m2 = (v2 - v1)/Cau2Vol(IEtapa)
!jap         CauExtrMin = DeltaM - m2
         IF (Imprime) THEN
            IF (.NOT. NuevaLinea) WRITE(UWrite1, *)
         ENDIF
!
!     Insercion de datos de LajaRPar.
         LajaRPar(ICauAflCaptAltoPolc, ISimul, IEtapa) =                &
     &        CauAflCaptAltoPolc
         LajaRPar(ICauRieDefAban, ISimul, IEtapa) = CauRieDefAban
         LajaRPar(ICauRieDefTuca, ISimul, IEtapa) = CauRieDefTuca
         LajaRPar(ICauExtrMax, ISimul, IEtapa) = CauExtrMax
         LajaRPar(ICauExtrMin, ISimul, IEtapa) = CauExtrMin
         LajaRPar(IAPol_Cap, ISimul, IEtapa) = CauAflCaptAltoPolc
         LajaRPar(IFilt_Gen, ISimul, IEtapa) = CauFiltLaja
         LajaRPar(IAban_HI, ISimul, IEtapa) = CauAflHoInAba
         LajaRPar(IAntu_Pas, ISimul, IEtapa) = CauAflAntuco
         LajaRPar(ITuca_HI, ISimul, IEtapa) = CauAflHoInTucapel
         LajaRPar(IQ1rAban, ISimul, IEtapa) = CauDemRegAbaEta(IEtapa)
         LajaRPar(IQ1rTuca, ISimul, IEtapa) = CauDemRegTucaEta(IEtapa)
         LajaRPar(IQ2rTuca, ISimul, IEtapa) = CauDemNuReEta(IEtapa)
!     Calculo de las disminucion de la cuota mensual y/o economias de lo
!     nuevos regantes.
         VolDelta = MIN(VolExtrMenNuRe, VolDemNuReEta)
         VolExtrMenNuRe = VolExtrMenNuRe - VolDelta
         VolDelta = VolDemNuReEta - VolDelta
!     Necesariamente se tiene que EcoNuRe > VolDelta
         EcoNuRe = EcoNuRe - VolDelta
         LajaRPar(IEcoENDGlo, ISimul, IEtapa) = EcoENDGlo
         LajaRPar(IEcoENDLoc, ISimul, IEtapa) = EcoENDLoc
         LajaRPar(IEcoNuRe, ISimul, IEtapa) = EcoNuRe
         LajaRPar(IVoluLaja, ISimul, IEtapa) = VoluLaja
         LajaRPar(IVolExtrAnuEND, ISimul, IEtapa) = VolExtrAnuEND
         LajaRPar(IVolExtrMenEND, ISimul, IEtapa) = VolExtrMenEND
         LajaRPar(IVolExtrMenNuRe, ISimul, IEtapa) = VolExtrMenNuRe
         IComp = LajaIPar(IIRieZaCo)
         GenMaxZaCo = LajaCPar%UppGenZaCo(IEtapa, ISimul)/CenRen(IComp)
         CauRieZaCo = MIN(GenMaxZaCo, CauDerAnRe*LajaCPar%PCenZaCo)
!         CauRieCCCE = LajaCPar%CRieCantEcol
         CauRieCCCE = MIN(LajaCPar%CRieCantEcol, CauAflRucue)
         CauRieLaDi = CauDerAnRe + CauDemNuReEtai - CauRieZaCo
         IF (LajaLPar(IUsoRieOpcional)) THEN
            CauRieOpcionalLaja = LajaCPar%PCenOpcionalLaja*VolDemNuReEta/ &
     &           Cau2Vol(IEtapa)
            LajaRPar(IRieOpcionalLaja, ISimul, IEtapa) =                &
     &           CauRieOpcionalLaja
         ELSE
            LajaRPar(IRieOpcionalLaja, ISimul, IEtapa) = 0.0d0
         ENDIF
!     Ajuste por agotamiento del lago
         CauRieZaCo = MIN(CauRieZaCo,                                   &
     &        CauExtrMin + CauAflHoInAba + CauFiltLaja + CauAflAntuco)
         CauRieLaDi = MIN(CauRieLaDi,                                   &
     &        CauExtrMin + CauAflHoInAba + CauFiltLaja + CauAflAntuco + &
     &        CauAflRucue - CauRieZaCo)
         LajaRPar(IRieZaCo, ISimul, IEtapa) = CauRieZaCo
         LajaRPar(IRieCCCE, ISimul, IEtapa) = CauRieCCCE
         LajaRPar(IRieLaDi, ISimul, IEtapa) = CauRieLaDi
      ENDIF
      IF (Imprime) THEN
         WRITE(UWrite2, '(I4, '','', $)') IEtapa
         WRITE(UWrite2, '(I3, '','', $)') Mes(IEtapa)
         WRITE(UWrite2, '(F10.2, '','', $)') Cau2Vol(IEtapa)
         WRITE(UWrite2, '(F10.2, '','', $)') LajaCPar%CauDerDiaEND
         WRITE(UWrite2, '(F10.2, '','', $)') VolDerDiaENDEta
         WRITE(UWrite2, '(F10.2, '','', $)') VolExtrMenEND
         WRITE(UWrite2, '(F10.2, '','', $)') VolExtrAnuEND
         WRITE(UWrite2, '(F10.2, '','', $)') VolExtrRemMenEND
         WRITE(UWrite2, '(F10.2, '','', $)') VolExtrRemAnuEND
         WRITE(UWrite2, '(F10.2, '','', $)') VoluLaja
         WRITE(UWrite2, '(F10.2, '','', $)') VolRealMax
         WRITE(UWrite2, '(F10.2, '','', $)') VolVac
         WRITE(UWrite2, '(F10.2, '','', $)') VolExtrMax
         WRITE(UWrite2, '(F10.2, '','', $)') VolFiltLaja
         WRITE(UWrite2, '(F10.2, '','', $)') CauExtrMax
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflCaptAltoPolc
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflAba
         WRITE(UWrite2, '(F10.2, '','', $)') CauDemRegAbaEta(IEtapa)
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflTucapel
         WRITE(UWrite2, '(F10.2, '','', $)') CauExtrMin
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflHoInAba
         WRITE(UWrite2, '(F10.2, '','', $)') CauFiltLaja
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflAntuco
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflRucue
         WRITE(UWrite2, '(F10.2, '','', $)') CauRieLaDi
         WRITE(UWrite2, '(F10.2, '','', $)') CauRieZaCo
         WRITE(UWrite2, '(F10.2, '','', $)') CauVac
         WRITE(UWrite2, '(F10.2, '','', $)') GenMaxElToro
         WRITE(UWrite2, '(F10.2, '','', $)') VolUtil
         WRITE(UWrite2, '(F10.2, '','', $)') LajaCPar%VolColInf
         WRITE(UWrite2, '(F10.2, '','', $)') EcoEND
         WRITE(UWrite2, '(F10.2, '','', $)') EcoNuRe
         WRITE(UWrite2, '(F10.2, '','', $)') CauAflHoInTucapel
         WRITE(UWrite2, *)
      ENDIF

      probnum = IEtapa

      CALL Laja1Dual(IEtapa, NBloque,   &
     &     LajaCPar, LajaLPar, LajaRPar, &
     &     ISimul, 1D9,  &
     &     lp, Dim)

      RETURN
      END SUBROUTINE
!     
      SUBROUTINE Laja1Dual(IEtapa, NBloque,   &
     &     LajaCPar, LajaLPar, LajaRPar, &
     &     ISimul, AflRuc,  &
     &     lp, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     Archivo comun a todas las rutinas.
      INCLUDE 'machcons.fpp'
!     Convenio del Laja.

      TYPE(PAR_LAJAC) LajaCPar

      DOUBLE PRECISION, INTENT(IN) :: LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER NBloque(Dim%Eta)
      INTEGER, INTENT(IN) :: ISimul
      LOGICAL, INTENT(IN) :: LajaLPar(DimLLaja)
!
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!     
!     Locales
      INTEGER NMod

      INTEGER IBlo

      INTEGER MCUppInd (5)
      DOUBLE PRECISION MCUppVal (5)
      INTEGER MCLowInd (5)
      DOUBLE PRECISION MCLowVal (5)

      DOUBLE PRECISION AflRuc

      IF (.NOT. LajaLPar(IUsoConvLaja)) THEN
         RETURN
      ENDIF

      DO IBlo = 1, NBloque(IEtapa)
         MCLowInd(1) = LajaCPar%IBloInd(IIRieZaCo, IBlo, IEtapa)
         MCUppInd(1) = LajaCPar%IBloInd(IIRieZaCo, IBlo, IEtapa)
         MCLowInd(2) = LajaCPar%IBloInd(IIRieCCCE, IBlo, IEtapa)
         MCUppInd(2) = LajaCPar%IBloInd(IIRieCCCE, IBlo, IEtapa)
         MCLowInd(3) = LajaCPar%IBloInd(IIRieLaDi, IBlo, IEtapa)
         MCUppInd(3) = LajaCPar%IBloInd(IIRieLaDi, IBlo, IEtapa)
         MCLowInd(4) = LajaCPar%IBloInd(IIGenElToro, IBlo, IEtapa)
         MCUppInd(4) = LajaCPar%IBloInd(IIGenElToro, IBlo, IEtapa)
         MCLowVal(1) = LajaRPar(IRieZaCo, ISimul, IEtapa)
         MCUppVal(1) = LajaRPar(IRieZaCo, ISimul, IEtapa)
!         MCLowVal(2) = LajaRPar(IRieCCCE, ISimul, IEtapa)
!         MCUppVal(2) = LajaRPar(IRieCCCE, ISimul, IEtapa)
         MCLowVal(2) = MIN(LajaRPar(IRieCCCE, ISimul, IEtapa), AflRuc)
         MCUppVal(2) = MIN(LajaRPar(IRieCCCE, ISimul, IEtapa), AflRuc)          
         MCLowVal(3) = LajaRPar(IRieLaDi, ISimul, IEtapa)
         MCUppVal(3) = LajaRPar(IRieLaDi, ISimul, IEtapa)
         MCLowVal(4) = LajaRPar(ICauExtrMin, ISimul, IEtapa)
         MCUppVal(4) = LajaRPar(ICauExtrMax, ISimul, IEtapa)
         NMod = 4
         IF (LajaLPar(IUsoRieOpcional)) THEN
            NMod = NMod + 1
            MCLowInd(NMod) = LajaCPar%IBloInd(IIRieOpcionalLaja, IBlo, IEtapa)
            MCUppInd(NMod) = LajaCPar%IBloInd(IIRieOpcionalLaja, IBlo, IEtapa)
            MCLowVal(NMod) =                                       &
     &           LajaRPar(IRieOpcionalLaja, ISimul, IEtapa)
            MCUppVal(NMod) =                                       &
     &           LajaRPar(IRieOpcionalLaja, ISimul, IEtapa)
         ENDIF
         CALL ModifUpp(NMod, MCUppInd, MCUppVal, lp)
         CALL ModifLow(NMod, MCLowInd, MCLowVal, lp)
      ENDDO
      RETURN
      END
