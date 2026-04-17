      SUBROUTINE Maule1(IEtapa, NBloque, BloInd, BloDur, CenRen,        &
     &     Cau2Vol, MauleIPar, MauleCPar, MauleLPar, MauleRPar, Mes,    &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     ISimul, SimulInd, EstocRHSP, VolIni,                         &
     &     VolFiltInv, VolDefRie, FFasePrimal,                          &
     &     FlagFiltProm, FiltProm, FiltParam, FiltNTramo,               &
     &     lp, Dim)
!     Archivo comun a todas las rutinas.
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!
      DOUBLE PRECISION, INTENT(IN):: FiltParam(Dim%FiltTramo, Dim%EmbFilt, Dim%FiltParam)
      DOUBLE PRECISION, INTENT(IN):: FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      INTEGER, INTENT(IN) :: FiltNTramo(Dim%EmbFilt)
      LOGICAL, INTENT(IN):: FlagFiltProm
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      INTEGER, INTENT(IN) :: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: Cau2Vol(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauConMauEta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: VolIni(Dim%Emb)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul
      INTEGER, INTENT(IN) :: MauleIPar(DimIMaule)
      INTEGER, INTENT(IN) :: Mes(Dim%Eta)
      INTEGER, INTENT(IN) :: SimulInd(Dim%Simul, Dim%Eta)
      LOGICAL, INTENT(IN):: FFasePrimal
      TYPE(PAR_MAULEC), INTENT(IN):: MauleCPar

      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)

!
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

      DOUBLE PRECISION, INTENT(OUT):: VolFiltInv      
      DOUBLE PRECISION, INTENT(OUT):: VolDefRie
      DOUBLE PRECISION, INTENT(OUT):: MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      LOGICAL, INTENT(OUT):: MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)

!     locals
      INTEGER IFiltInve
      INTEGER IClase(Dim%EstocFila)
      INTEGER IComp
      INTEGER IIEtapa
      INTEGER IMes
      INTEGER UWrite1
      INTEGER UWrite2
      INTEGER UWrite3
      INTEGER UWrite4
      LOGICAL FPasoPorResOrd
      LOGICAL FPrevisionDeshielo
      LOGICAL FVieneDePorSup
      LOGICAL FVieneDeResOrd
      LOGICAL NuevaLinea
      DOUBLE PRECISION FFiltraciones
      DOUBLE PRECISION CauFiltInv
      DOUBLE PRECISION VolGenMaxCip
      DOUBLE PRECISION VolGenMaxMaule
      DOUBLE PRECISION VolGenMinCip
      DOUBLE PRECISION PCenCMNA
      DOUBLE PRECISION PCenCMNB
      DOUBLE PRECISION pp
      DOUBLE PRECISION VolAflArme
      DOUBLE PRECISION VolAflArmeDfnv
      DOUBLE PRECISION VolAflClap
      DOUBLE PRECISION VolAflHiis
      DOUBLE PRECISION VolAflHimi
      DOUBLE PRECISION VolAflHima
      DOUBLE PRECISION VolAflHipe
      DOUBLE PRECISION VolAflInv
      DOUBLE PRECISION VolAflMau
      DOUBLE PRECISION VolAflMau20PC
      DOUBLE PRECISION VolAflMau80PC
      DOUBLE PRECISION VolAflMela
      DOUBLE PRECISION VolColb
      DOUBLE PRECISION VolCompEND
      DOUBLE PRECISION VolConMau
      DOUBLE PRECISION VolDisResOrdEND
      DOUBLE PRECISION VolDisResOrdRie
      DOUBLE PRECISION VolEcoInv
      DOUBLE PRECISION VolExtMaxMauEND
      DOUBLE PRECISION VolExtMaxMaule
      DOUBLE PRECISION VolExtMaxMauRie
      DOUBLE PRECISION VolExtRie
      DOUBLE PRECISION VolResCuoExt
      DOUBLE PRECISION VolResMauEND
      DOUBLE PRECISION VolResMauRie
      DOUBLE PRECISION VolRiego
      DOUBLE PRECISION CauRiego
      DOUBLE PRECISION VolExtMinMauEND
      DOUBLE PRECISION VolInv
      DOUBLE PRECISION VolMau
      DOUBLE PRECISION VolRes105
      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION VolExtMinInve

!     ReqCMel Segun codigo CDEC
      DOUBLE PRECISION ReqCMel      

      IF (MauleIPar(IIUsoConvMaule) .EQ. 0) RETURN
!     Se considera que nunca hay una prevision de deshielo
!     que supere los 500Hm3 o que la direccion de aguas no
!     permite a ENDESA la cuota extraordinaria de 50Hm3.
      UWrite1 = MauleIPar(IIMauleWrite1)
      UWrite2 = MauleIPar(IIMauleWrite2)
      UWrite3 = MauleIPar(IIMauleWrite3)
      UWrite4 = MauleIPar(IIMauleWrite4)
      FPrevisionDeshielo = .FALSE.
!
      IF (FFasePrimal) THEN
         NuevaLinea = .FALSE.
!
!     Extraccion de datos de MauleRPar.
         IF (IEtapa .EQ. 1) THEN
!     Se leen los datos iniciales no perturbados.
            IIEtapa = 0
         ELSE
!     Se leen los datos iniciales de la etapa, eventualmente perturbados
            IIEtapa = IEtapa
         ENDIF
!
!     Extraccion de datos de MauleIPar.
         FPasoPorResOrd = MauleLPar(IFPasoPorResOrd, ISimul, IIEtapa)
         FVieneDePorSup = MauleLPar(IFVieneDePorSup, ISimul, IIEtapa)
         FVieneDeResOrd = MauleLPar(IFVieneDeResOrd, ISimul, IIEtapa)
!     Extraccion de datos de MauleRPar.
         VolCompEND = MauleRPar(IVolCompEND, ISimul, IIEtapa)
         VolDisResOrdEND = MauleRPar(IVolDisResOrdEND, ISimul, IIEtapa)
         VolDisResOrdRie = MauleRPar(IVolDisResOrdRie, ISimul, IIEtapa)
         VolEcoInv = MauleRPar(IVolEcoInv, ISimul, IIEtapa)
         VolColb = VolIni(MauleIPar(IICenColbun))
         VolInv = MauleRPar(IVolInv, ISimul, IIEtapa)
         VolMau = MauleRPar(IVolMau, ISimul, IIEtapa)
         VolResCuoExt = MauleRPar(IVolResCuoExt, ISimul, IIEtapa)
         VolResMauEND = MauleRPar(IVolResMauEND, ISimul, IIEtapa)
         VolResMauRie = MauleRPar(IVolResMauRie, ISimul, IIEtapa)
!
         IMes = Mes(IEtapa)
         VolRes105 = CauRes105Eta(IEtapa)*Cau2Vol(IEtapa)
!     Volumenes Afluentes.
         IClase = SimulInd(ISimul, IEtapa)
!         VolAflHiis =                                                  &
!     &        EstocRHSP(MauleIPar(IIAflIsla), IClase, IEtapa)
         IF (MauleIPar(IIAflMina) .ne. 0) THEN
              VolAflHimi = QAfluEta(IEtapa, NBloque, BloInd,            &
     &            BloDur, EstocRHSP, IClase,                            &
     &            MauleIPar(IIAflMina), Dim) * Cau2Vol(IEtapa)
         ELSE
              VolAflHimi = 0
         ENDIF

         VolAflHiis = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &        BloDur, EstocRHSP, IClase,                                &
     &        MauleIPar(IIAflIsla), Dim) * Cau2Vol(IEtapa) +            &
     &        VolAflHimi

!         VolAflHipe =                                                  &
!     &        EstocRHSP(MauleIPar(IIAflBocMaule), IClase, IEtapa) +    &
!     &        VolAflHiis
         VolAflHipe = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &        BloDur, EstocRHSP, IClase,                                &
     &        MauleIPar(IIAflBocMaule), Dim) * Cau2Vol(IEtapa) +        &
     &        VolAflHiis

!         VolAflMela =                                                   &
!     &        EstocRHSP(MauleIPar(IIAflPehuenche), IClase, IEtapa)
         VolAflMela = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &        BloDur, EstocRHSP, IClase,                                & 
     &        MauleIPar(IIAflPehuenche), Dim) * Cau2Vol(IEtapa)

!         VolAflClap =                                                   &
!     &        EstocRHSP(MauleIPar(IIAflArmerillo), IClase, IEtapa)
         VolAflClap = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &        BloDur, EstocRHSP, IClase,                                & 
     &        MauleIPar(IIAflArmerillo), Dim) * Cau2Vol(IEtapa)

!         VolAflHima =                                                   &
!     &        EstocRHSP(MauleIPar(IIAflColbun), IClase, IEtapa)
         VolAflHima = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &        BloDur, EstocRHSP, IClase,                                & 
     &        MauleIPar(IIAflColbun), Dim) * Cau2Vol(IEtapa)

!         VolAflInv =                                                   &
!     &        EstocRHSP(MauleIPar(IIAflInve), IClase, IEtapa)
         VolAflInv = QAfluEta(IEtapa, NBloque, BloInd,                  &
     &        BloDur, EstocRHSP, IClase,                                & 
     &        MauleIPar(IIAflInve), Dim) * Cau2Vol(IEtapa)

!         VolAflMau =                                                   &
!     &        EstocRHSP(MauleIPar(IIAflMaule), IClase, IEtapa)
         VolAflMau = QAfluEta(IEtapa, NBloque, BloInd,                  &
     &        BloDur, EstocRHSP, IClase,                                & 
     &        MauleIPar(IIAflMaule), Dim) * Cau2Vol(IEtapa)
!
         IFiltInve = MauleIPar(IIFiltInve)
         IF (FlagFiltProm) THEN
            CauFiltInv = FiltProm(IFiltInve, IEtapa + 1)
         ELSE
            IF (IEtapa .EQ. 1) THEN
               CauFiltInv = FiltProm(IFiltInve, 1)
            ELSE
               CauFiltInv =                                             &
     &              FFiltraciones(FiltNTramo(IFiltInve),                &
     &              FiltParam(1, IFiltInve, PFiltVol),                  &
     &              FiltParam(1, IFiltInve, PFiltPend),                 &
     &              FiltParam(1, IFiltInve, PFiltConst), VolInv)
            ENDIF
         ENDIF
!     El volumen filtrado debe ser menor que el volumen total mas
!     el volumen afluente.
         VolFiltInv = MIN(VolInv + VolAflInv,                           &
     &        CauFiltInv*Cau2Vol(IEtapa))

!         write(*,*) 'cf ', VolInv, VolAflInv, CauFiltInv

         CauFiltInv = VolFiltInv/Cau2Vol(IEtapa)
!
!     Volumen Afluente en Armerillo.
         VolAflArme = VolAflHipe + VolAflMela + VolAflClap +            &
     &        VolFiltInv
!$$$  IF (IEtapa .GE. 93) THEN
!$$$  PRINT *
!$$$  ENDIF
!
!     Derechos de Riego.
         VolConMau =                                                    &
     &        CauConMauEta(IEtapa)*Cau2Vol(IEtapa)
!
         IComp = MauleIPar(IIGenCip)
         VolGenMinCip = MauleCPar%LowGenCip(IEtapa, ISimul)/CenRen(IComp)*Cau2Vol(IEtapa)
         IF (VolConMau - VolAflArme .GT. 0.0D0) THEN
!     No se puede embalsar en la Invernada.
            VolGenMaxCip = MauleCPar%UppGenCip(IEtapa, ISimul)/CenRen(IComp)*Cau2Vol(IEtapa)
            VolExtMinInve =                                             &
     &           MAX(                                                   &
     &           VolGenMinCip,                                          &
     &           MIN(                                                   &
     &           pp(VolAflInv - VolFiltInv),                            &
     &           pp(VolConMau - VolAflArme),                            &
     &           VolGenMaxCip))
         ELSE
            VolExtMinInve = VolGenMinCip
         ENDIF
!     Deficit de Riego.
         VolDefRie = pp(VolConMau - VolAflArme - VolExtMinInve)
!
!     Determinar el volumen maximo de extraccion desde la
!     laguna del Maule para riego y para generacion electrica
!     por parte de ENDESA.
         IF (VolMau .GE. MauleCPar%VolResOrdMax + MauleCPar%VolResExtMax) THEN
!     Esta en la porcion superior
            VolExtMaxMauRie =                                           &
     &           MIN(VolDefRie,                                         &
     &           VolResMauRie)
            VolExtMaxMauEND =                                           &
     &           MIN(MauleCPar%GastoMedMenMax*Cau2Vol(IEtapa),          &
     &           VolCompEND + VolEcoInv + VolResMauEND)
         ELSE IF (MauleCPar%VolResExtMax .GE. VolMau) THEN
!     Esta en la reserva extraordinaria.
            VolExtMaxMauRie = 0.0D0
            VolExtMaxMauEND = 0.0D0
!$$$            VolExtMaxMauEND = VolEcoInv
         ELSE
!     Esta en la reserva ordinaria.
            VolAflMau80PC = 0.8D0*VolAflMau
            VolExtMaxMauRie =                                           &
     &           MIN(VolDefRie,                                         &
     &           VolResMauRie,                                          &
     &           VolDisResOrdRie + VolAflMau80PC)
            IF (FPasoPorResOrd) THEN
!     No puede ocupar el 20%.
               VolExtMaxMauEND =                                        &
     &              MIN(MauleCPar%GastoMedMenMax*Cau2Vol(IEtapa),                 &
     &              VolEcoInv)
            ELSE
!     Puede ocupar el 20%.
               VolAflMau20PC = 0.2D0*VolAflMau
               VolExtMaxMauEND =                                        &
     &              MIN(MauleCPar%GastoMedMenMax*Cau2Vol(IEtapa),                 &
     &              VolEcoInv + VolResMauEND,                           &
     &              VolEcoInv + VolDisResOrdEND + VolAflMau20PC)
               IF ((VolDisResOrdEND .LE. 0.0D0) .AND.                   &
     &              (IMes .GE. Junio) .AND.                             &
     &              (IMes .LE. Agosto) .AND.                            &
     &              FPrevisionDeshielo) THEN
!     Se acabo el 20%, estamos fuera de la temporada de riego y
!     la prevision de deshielo asegura mas de 500Hm3 afluentes
!     al Maule durante el deshielo. Por lo tanto, tiene derecho
!     a una cuota adicional.
                  VolExtMaxMauEND =                                     &
     &                 MIN(MauleCPar%GastoMedMenMax*Cau2Vol(IEtapa),    &
     &                 VolEcoInv + VolResMauEND,                        &
     &                 VolEcoInv + 0.5D0*pp(VolMau - MauleCPar%VolResExtMax),     &
     &                 VolEcoInv + VolResCuoExt)
                  VolResCuoExt = pp(VolResCuoExt - VolExtMaxMauEND)
               ENDIF
            ENDIF
         ENDIF
         IF (MauleIPar(IIPLPExtRie) .EQ. 0) THEN
!     El PLP NO decide riego
            VolExtMaxMauRie = MauleCPar%ExtPar(ILeeExtMauRie, IClase(1), IEtapa)
         ENDIF
!     Primera estimacion de la extraccion minima de ENDESA.
         VolExtMinMauEND = 0.0d0
         IF (                                                           &
     &        (MauleCPar%VolResExtMax .GE. VolMau) .AND.                          &
     &        (MauleIPar(IIPLPExtENDCI) .EQ. 0)                         &
     &        ) THEN
!     El PLP NO decide generacion. La laguna del Maule ESTA en reserva
!     extraordinaria y hay un archivo que tiene las extracciones.
            VolExtMaxMauEND =                                           &
     &           MauleCPar%ExtPar(ILeeExtMauENDCI, IClase(1), IEtapa)
!     Se actualiza la extraccion minima.
            VolExtMinMauEND = VolExtMaxMauEND
         ENDIF
         IF (                                                           &
     &        (MauleCPar%VolResExtMax .LT. VolMau) .AND.                &
     &        (MauleIPar(IIPLPExtEND) .EQ. 0)                           &
     &        ) THEN
!     El PLP NO decide generacion. La laguna del Maule NO ESTA en
!     reserva extraordinaria y hay un archivo que tiene las extracciones
            VolExtMaxMauEND =                                           &
     &           MauleCPar%ExtPar(ILeeExtMauEND, IClase(1), IEtapa)
!     Se actualiza la extraccion minima.
            VolExtMinMauEND = VolExtMaxMauEND
         ENDIF
!     Extraccion maxima.
         VolExtMaxMaule = VolExtMaxMauEND + VolExtMaxMauRie
!     Prorrateo si el volumen de extraccion excede al maximo de la lagun
!         IComp = MauleCPar%IBloInd(IIGenMaule, 1, IEtapa)
         IComp = MauleIPar(IIGenMaule)
         VolGenMaxMaule = MauleCPar%UppGenMaule(IEtapa, ISimul)/CenRen(IComp)*Cau2Vol(IEtapa)
         IF (VolExtMaxMaule .GT. VolGenMaxMaule) THEN
            VolExtMaxMauEND = VolExtMaxMauEND*                          &
     &           VolGenMaxMaule/VolExtMaxMaule
            VolExtMaxMauRie = VolExtMaxMauRie*                          &
     &           VolGenMaxMaule/VolExtMaxMaule
            VolExtMaxMaule = VolGenMaxMaule
         ENDIF
         VolAflArmeDfnv = VolAflArme + VolExtMinInve +                  &
     &        VolExtMaxMauRie
         VolRiego = MIN(VolAflArmeDfnv, VolRes105)
!     Insercion de datos de MauleLPar.
         MauleLPar(IFPasoPorResOrd, ISimul, IEtapa) = FPasoPorResOrd
         MauleLPar(IFVieneDePorSup, ISimul, IEtapa) = FVieneDePorSup
         MauleLPar(IFVieneDeResOrd, ISimul, IEtapa) = FVieneDeResOrd
!     Insercion de datos de MauleRPar.
         MauleRPar(IVolAflArme, ISimul, IEtapa) = VolAflArme
         MauleRPar(IVolCompEND, ISimul, IEtapa) = VolCompEND
         MauleRPar(IVolDisResOrdEND, ISimul, IEtapa) = VolDisResOrdEND
         MauleRPar(IVolDisResOrdRie, ISimul, IEtapa) = VolDisResOrdRie
         MauleRPar(IVolEcoInv, ISimul, IEtapa) = VolEcoInv
         MauleRPar(IVolInv, ISimul, IEtapa) = VolInv
         MauleRPar(IVolMau, ISimul, IEtapa) = VolMau
         MauleRPar(IVolResCuoExt, ISimul, IEtapa) = VolResCuoExt
         MauleRPar(IVolResMauEND, ISimul, IEtapa) = VolResMauEND
         MauleRPar(IVolResMauRie, ISimul, IEtapa) = VolResMauRie
         IF (VolColb .GT. MauleCPar%VolColbLim) THEN
            PCenCMNB = MauleCPar%PCenCMNB1
            PCenCMNA = MauleCPar%PCenCMNA1
         ELSE
            PCenCMNB = MauleCPar%PCenCMNB2
            PCenCMNA = MauleCPar%PCenCMNA2
         ENDIF
!$$$ TODO: Resolver si esta asignacion tiene que ser truncada o no
         CauRiego = DNINT(VolRiego/Cau2Vol(IEtapa))
!         CauRiego = VolRiego/Cau2Vol(IEtapa)
!     El riego para el canal melado no puede superar los afluentes.
!     ReqCMel segun codigo CDEC
         ReqCMel = MIN(MauleCPar%PCenCMel*CauRiego, MauleCPar%GastoMaxCMel)

         MauleRPar(IRieCMel, ISimul, IEtapa) =                          &
!     &        MIN(MauleCPar%PCenCMel*CauRiego, VolAflMela/Cau2Vol(IEtapa))
     &        MIN(ReqCMel, VolAflMela/Cau2Vol(IEtapa))         

         MauleRPar(IRieCMNA, ISimul, IEtapa) = PCenCMNA*CauRiego
         MauleRPar(IRieCMNB, ISimul, IEtapa) = PCenCMNB*CauRiego
         MauleRPar(IRieOReg, ISimul, IEtapa) = MauleCPar%PCenOReg*CauRiego
         MauleRPar(IRieS123, ISimul, IEtapa) = MauleCPar%PCenS123*CauRiego
         IF (MauleIPar(IIUsoRieOpcional) .NE. 0) THEN
            MauleRPar(IRieOpcionalMaule, ISimul, IEtapa) =              &
     &           MauleCPar%PCenOpcionalMaule*CauRiego
         ELSE
            MauleRPar(IRieOpcionalMaule, ISimul, IEtapa) = 0.0d0
         ENDIF
         VolExtRie = VolExtMinInve + VolExtMaxMauRie
         MauleRPar(IExtMaxMauEND, ISimul, IEtapa) =                     &
     &        VolExtMaxMauEND/Cau2Vol(IEtapa)
         MauleRPar(IExtMinInve, ISimul, IEtapa) =                       &
     &        VolExtMinInve/Cau2Vol(IEtapa)
         MauleRPar(IExtRie, ISimul, IEtapa) = VolExtRie/Cau2Vol(IEtapa)
         MauleRPar(IVolExtMaxMauRie, ISimul, IEtapa) = VolExtMaxMauRie
         MauleRPar(IVolExtMinMauEND, ISimul, IEtapa) = VolExtMinMauEND
         MauleRPar(IVolExtMaxMauEND, ISimul, IEtapa) = VolExtMaxMauEND
         MauleRPar(IVolExtRie, ISimul, IEtapa) = VolExtRie
         MauleRPar(IVolAflInv, ISimul, IEtapa) = VolAflInv
         MauleRPar(IVolFiltInv, ISimul, IEtapa) = VolFiltInv
!$$$  IF (FFasePrimal .AND.
!$$$  $        (SimulInd(ISimul, IEtapa) .EQ.
!$$$  $        (ISimul .EQ.
!$$$  $        MauleIPar(IIndSimImpMaule))) THEN
         IF (FFasePrimal .AND.                                          &
     &        (ISimul .EQ.                                              &
     &        MauleIPar(IIndSimImpMaule))) THEN
            WRITE(UWrite3, '(I4, $)') IEtapa
            WRITE(UWrite3, '('','', I4, $)') Mes(IEtapa)
            WRITE(UWrite3, '('','', F6.1, $)') Cau2Vol(IEtapa)
            WRITE(UWrite3, '('','', F6.1, $)') VolAflHiis/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflHipe/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflMela/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflClap/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflHima/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflInv/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflMau/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolFiltInv/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflArme/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolConMau/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolRes105/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolDefRie/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolAflArmeDfnv/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolRiego/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolExtMaxMaule/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolExtMaxMauEND/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolExtMaxMauRie/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolExtMinInve/1D3
            WRITE(UWrite3, '('','', F6.1, $)') VolExtRie/1D3
            IComp = MauleIPar(IIGenCip)
            VolGenMaxCip = MauleCPar%UppGenCip(IEtapa, ISimul)/CenRen(IComp)*Cau2Vol(IEtapa)
            WRITE(UWrite3, '('','', F6.1, $)') VolGenMaxCip/1D3
            WRITE(UWrite3, *)
            WRITE(UWrite4, '(I4, $)') IEtapa
            WRITE(UWrite4, '('','', I4, $)') Mes(IEtapa)
            WRITE(UWrite4, '('','', I4, $)') MauleIPar(IIUsoConvMaule)
            WRITE(UWrite4, '('','', I4, $)') MauleIPar(IIUsoRieOpcional)
            WRITE(UWrite4, '('','', I4, $)') MauleIPar(IIPLPExtEND)
            WRITE(UWrite4, '('','', I4, $)') MauleIPar(IIPLPExtENDCI)
            WRITE(UWrite4, '('','', I4, $)') MauleIPar(IIPLPExtRie)
            WRITE(UWrite4, '('','', F6.1, $)') Cau2Vol(IEtapa)
            WRITE(UWrite4, '('','', F6.1, $)') VolAflHima/1D3
            WRITE(UWrite4, '('','', F6.1, $)')                          &
     &           MauleRPar(IRieCMNB, ISimul, IEtapa)/1D3
            WRITE(UWrite4, '('','', F6.1, $)')                          &
     &           MauleRPar(IRieCMNA, ISimul, IEtapa)/1D3
            WRITE(UWrite4, '('','', F6.1, $)')                          &
     &           MauleRPar(IRieCMel, ISimul, IEtapa)/1D3
            WRITE(UWrite4, '('','', F6.1, $)')                          &
     &           VolRes105/Cau2Vol(IEtapa)
            WRITE(UWrite4, '('','', F6.1, $)')                          &
     &           VolAflArmeDfnv/Cau2Vol(IEtapa)
            WRITE(UWrite4, '('','', F6.1, $)')                          &
     &           VolConMau/Cau2Vol(IEtapa)
            WRITE(UWrite4, '('','', F6.1, $)') CauConMauEta(IEtapa)
            WRITE(UWrite4, '('','', F6.1, $)') PCenCMNB
            WRITE(UWrite4, '('','', F6.1, $)') PCenCMNA
            WRITE(UWrite4, *)
         ENDIF
      ENDIF
!     

      CALL Maule1Dual(IEtapa, NBloque,                                  & 
     &     Cau2Vol, MauleIPar, MauleCPar, MauleRPar,                    &
     &     ISimul, 1D9,                                                 &
     &     lp, Dim)

      RETURN
      END SUBROUTINE

      SUBROUTINE Maule1Dual(IEtapa, NBloque,                            &
     &     Cau2Vol, MauleIPar, MauleCPar, MauleRPar,                    &
     &     ISimul, AflMel,                                              &
     &     lp, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!     
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN) :: Cau2Vol(Dim%Eta)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul
      INTEGER, INTENT(IN) :: MauleIPar(DimIMaule)
      DOUBLE PRECISION, INTENT(IN) :: MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      TYPE(PAR_MAULEC), INTENT(IN) :: MauleCPar
!
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp
!


! locals
      INTEGER ILow
      INTEGER IUpp
      INTEGER IBlo

      INTEGER MCUppInd (10)
      DOUBLE PRECISION MCUppVal (10)
      INTEGER MCLowInd (10)
      DOUBLE PRECISION MCLowVal (10)
      
      DOUBLE PRECISION AflMel
      DOUBLE PRECISION ReqCMel      


      IF (MauleIPar(IIUsoConvMaule) .EQ. 0) RETURN

      DO IBlo = 1, NBloque(IEtapa)
         MCLowInd(1) = MauleCPar%IBloInd(IIRieCMNB, IBlo, IEtapa)
         MCUppInd(1) = MauleCPar%IBloInd(IIRieCMNB, IBlo, IEtapa)
         MCLowInd(2) = MauleCPar%IBloInd(IIRieCMel, IBlo, IEtapa)
         MCUppInd(2) = MauleCPar%IBloInd(IIRieCMel, IBlo, IEtapa)
         MCLowInd(3) = MauleCPar%IBloInd(IIRieS123, IBlo, IEtapa)
         MCUppInd(3) = MauleCPar%IBloInd(IIRieS123, IBlo, IEtapa)
         MCLowInd(4) = MauleCPar%IBloInd(IIRieCMNA, IBlo, IEtapa)
         MCUppInd(4) = MauleCPar%IBloInd(IIRieCMNA, IBlo, IEtapa)
         MCLowInd(5) = MauleCPar%IBloInd(IIRieOReg, IBlo, IEtapa)
         MCUppInd(5) = MauleCPar%IBloInd(IIRieOReg, IBlo, IEtapa)
         MCLowInd(6) = MauleCPar%IBloInd(IIExtMauEND, IBlo, IEtapa)
         MCUppInd(6) = MauleCPar%IBloInd(IIExtMauEND, IBlo, IEtapa)
         MCLowInd(7) = MauleCPar%IBloInd(IIControl, IBlo, IEtapa)
         MCUppInd(7) = MauleCPar%IBloInd(IIControl, IBlo, IEtapa)
         MCLowVal(1) = MauleRPar(IRieCMNB, ISimul, IEtapa)
         MCUppVal(1) = MauleRPar(IRieCMNB, ISimul, IEtapa)       
         ReqCMel = MIN(MauleCPar%GastoMaxCMel, MauleRPar(IRieCMel, ISimul, IEtapa))
         MCLowVal(2) = MIN(ReqCMel, AflMel)
         MCUppVal(2) = MIN(ReqCMel, AflMel)
         MCLowVal(3) = MauleRPar(IRieS123, ISimul, IEtapa)
         MCUppVal(3) = MauleRPar(IRieS123, ISimul, IEtapa)
         MCLowVal(4) = MauleRPar(IRieCMNA, ISimul, IEtapa)
         MCUppVal(4) = MauleRPar(IRieCMNA, ISimul, IEtapa)
         MCLowVal(5) = MauleRPar(IRieOReg, ISimul, IEtapa)
         MCUppVal(5) = MauleRPar(IRieOReg, ISimul, IEtapa)
         MCLowVal(6) = MauleRPar(IVolExtMinMauEND, ISimul, IEtapa)/    &
     &        Cau2Vol(IEtapa)
         MCUppVal(6) = MauleRPar(IVolExtMaxMauEND, ISimul, IEtapa)/    &
     &        Cau2Vol(IEtapa)
         MCLowVal(7) = MauleRPar(IExtRie, ISimul, IEtapa)
         MCUppVal(7) = MauleRPar(IExtRie, ISimul, IEtapa)
         ILow = 7
         IUpp = 7
         IF (MauleIPar(IICompRiegoInve) .NE. 0) THEN
            ILow = ILow + 1
            MCLowInd(ILow) =                                    &
     &           MauleCPar%IBloInd(IIGenCip, IBlo, IEtapa)
            MCLowVal(ILow) =                                    &
     &           MauleRPar(IExtMinInve, ISimul, IEtapa)
         ENDIF
         IF (MauleIPar(IIUsoRieOpcional) .NE. 0) THEN
            ILow = ILow + 1
            IUpp = IUpp + 1
            MCLowInd(ILow) =  &
     &           MauleCPar%IBloInd(IIRieOpcionalMaule, IBlo, IEtapa)
            MCUppInd(IUpp) =  &
     &           MauleCPar%IBloInd(IIRieOpcionalMaule, IBlo, IEtapa)
            MCLowVal(ILow) =                                       &
     &           MauleRPar(IRieOpcionalMaule, ISimul, IEtapa)
            MCUppVal(IUpp) =                                       &
     &           MauleRPar(IRieOpcionalMaule, ISimul, IEtapa)
         ENDIF

         CALL ModifUpp(IUpp, MCUppInd, MCUppVal, lp)
         CALL ModifLow(ILow, MCLowInd, MCLowVal, lp)
      ENDDO
      RETURN
      END

      SUBROUTINE Maule1a(FFixMaule, FUndo,                              &
     &     IEtapa, NBloque, ISimul,                                     &
     &     MauleIPar, MauleCPar, MauleRPar, CenRen,                     &
     &     Primal, PDNCol,                                              &
     &     lp, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!     Archivo comun a todas las rutinas.      
      INCLUDE 'machcons.fpp'
!
!
      INTEGER PDNCol
      DOUBLE PRECISION, INTENT(IN):: MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: Primal(PDNCol)
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul
      INTEGER, INTENT(IN) :: MauleIPar(DimIMaule)
      LOGICAL, INTENT(IN):: FUndo
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      TYPE(PAR_MAULEC), INTENT(IN) :: MauleCPar

      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)

!     out
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

      LOGICAL, INTENT(OUT) :: FFixMaule

!     locals
      DOUBLE PRECISION pp
      DOUBLE PRECISION UppVal
      INTEGER IUpp
      INTEGER IBlo
      INTEGER IIIExtMauRie
      INTEGER IIIExtInvEND

      INTEGER MCUppInd (1)
      DOUBLE PRECISION MCUppVal (1)
      INTEGER IComp

      FFixMaule = No

      IF  (  &
     &     (MauleIPar(IIUsoConvMaule) .LE. 0)                           &
     &     .OR.                                                         &
     &     (MauleIPar(IIVerifConvenio) .LE. 0)) THEN
         RETURN
      ENDIF


      IF (                                                              &
     &     FUndo                                                        &
     &     .OR.                                                         &
     &     (                                                            &
     &     (MauleIPar(IIUsoConvMaule) .GT. 0)                           &
     &     .AND.                                                        &
     &     (MauleIPar(IIVerifConvenio) .GT. 0)                          &
     &     )) THEN
         DO IBlo = 1, NBloque(IEtapa)
            IIIExtMauRie = MauleCPar%IBloInd(IIExtMauRie, IBlo, IEtapa)
            IIIExtInvEND = MauleCPar%IBloInd(IIExtInvEND, IBlo, IEtapa)
            IF((Primal(IIIExtMauRie)*Primal(IIIExtInvEND)) .GT. SEPSILON) &
     &           THEN 
               IF (FUndo) THEN
                  UppVal = MauleCPar%UppExtMauRie(IEtapa, ISimul)
               ELSE
                  FFixMaule = Si
                  IComp = MauleIPar(IIGenCip)
                  UppVal = pp(                                          &
     &                 MauleRPar(IExtRie, ISimul, IEtapa) -             &
     &                 MauleCPar%UppGenCip(IEtapa, ISimul)/CenRen(IComp))
               ENDIF
               IUpp = 1
               MCUppInd(IUpp) = IIIExtMauRie
               MCUppVal(IUpp) = UppVal
               CALL ModifUpp(IUpp, MCUppInd, MCUppVal, lp)
            ENDIF
         ENDDO
      ELSE
         FFixMaule = No
      ENDIF
      RETURN
      END
