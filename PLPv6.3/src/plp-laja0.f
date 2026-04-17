!     Lee los datos e inicializa los arreglos necesarios para la
!     modelacion del convenio del Laja.
      SUBROUTINE Laja0(Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar, &
     &     NBloque, BloInd, BloDur,                                     &
     &     Mes, MesBal, CauDemNuReEta, CauDemRegAbaEta,                 &
     &     CauDemRegTucaEta,                                            &
     &     CenNom, NEtapa, NCentral,                                    &
     &     NSimul, NFlujo, NCenEmb, NCenSer,                            &
     &     CenManSInd, CenPMax,                                         &
     &     EtaDur, EmbVIni, FactTiempo, Year,                           &
     &     EstocRHSP, FiltProm,                                         &
     &     SimulInd, Ulog, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) ::  Dim

!     Convenio del Laja.
!
      EXTERNAL Abrir
      INTEGER Abrir
!
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)

      TYPE(PAR_LAJAC) LajaCPar

      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)

      CHARACTER*48 CenNom(Dim%Cen)
      DOUBLE PRECISION EtaDur(Dim%Eta)
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION CauDemRegAbaEta(Dim%Eta)
      DOUBLE PRECISION CauDemRegTucaEta(Dim%Eta)
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FiltProm(Dim%EmbFilt, Dim%Eta + 1)
      DOUBLE PRECISION LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      INTEGER IClase(Dim%EstocFila)
      INTEGER LajaIPar(DimILaja)
      INTEGER Mes(Dim%Eta)
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NCenHidSPP
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NSimul
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER ULog
      INTEGER Year(Dim%Eta)
      LOGICAL MesBal(Dim%Eta)
!     Locales
!$$$  CHARACTER*12 AuxVar
      CHARACTER*72 NomAflAbanico
      CHARACTER*72 NomAflAntuco
      CHARACTER*72 NomAflCaptAltoPolc
      CHARACTER*72 NomAflLaja
      CHARACTER*72 NomAflRucue
      CHARACTER*72 NomAflTucapel
      CHARACTER*72 NomCenAbanico
      CHARACTER*72 NomCenAbCCCE
      CHARACTER*72 NomCenAbLaDi
      CHARACTER*72 NomCenAbZaCo
      CHARACTER*72 NomCenElToro
      CHARACTER*72 NomCenFiltLaja
      CHARACTER*72 NomCenLaja
      CHARACTER*72 NomCenRieOpcional
      CHARACTER*72 NomCenTucapel
      CHARACTER*72 NomGeneLaja
      CHARACTER*72 NomGenElToro
      CHARACTER*72 NomVolLaja
      DOUBLE PRECISION CauDemNuRe(12, Dim%Year)
      DOUBLE PRECISION CauDemNuReDefecto(12)
      DOUBLE PRECISION CauDemNuReProm
      DOUBLE PRECISION CauDemRegAba(12, Dim%Year)
      DOUBLE PRECISION CauDemRegAbaDefecto(12)
      DOUBLE PRECISION CauDemRegAbaProm
      DOUBLE PRECISION CauDemRegTuca(12, Dim%Year)
      DOUBLE PRECISION CauDemRegTucaDefecto(12)
      DOUBLE PRECISION CauDemRegTucaProm
      DOUBLE PRECISION EcoENDGlo
      DOUBLE PRECISION EcoENDLoc
      DOUBLE PRECISION EcoNuRe
      DOUBLE PRECISION pp
      DOUBLE PRECISION VolExtrAnuEND
      DOUBLE PRECISION VolExtrMenEND
      DOUBLE PRECISION VolExtrMenNuRe
      DOUBLE PRECISION VoluLaja
      DOUBLE PRECISION VolAflAba
      DOUBLE PRECISION VolAflAntuco
      DOUBLE PRECISION VolAflCaptAltoPolc
      DOUBLE PRECISION VolAflHoInAba
      DOUBLE PRECISION VolAflHoInTucapel
      DOUBLE PRECISION VolAflLaja
      DOUBLE PRECISION VolAflRucue
      DOUBLE PRECISION VolAflTucapel
      DOUBLE PRECISION VolDemNuReEta
      DOUBLE PRECISION VolDemRegTucaEta
      DOUBLE PRECISION VolDerAnRe
      DOUBLE PRECISION VolExcAnRe
      DOUBLE PRECISION VolExtrMin
      DOUBLE PRECISION VolRieDefAban
      DOUBLE PRECISION VolRieDefAnRe
      DOUBLE PRECISION VolRieDefNuRe
      DOUBLE PRECISION VolRieDefTuca
      INTEGER IEtapa
      INTEGER IIEtapa
      INTEGER IMes
      INTEGER IMes0
      INTEGER IndSimImpLaja
      INTEGER ISimul
      INTEGER IYear
      INTEGER NAfluFict
      INTEGER NMes
      INTEGER NumCen
      INTEGER NumFil
      INTEGER NVert
      INTEGER NYear
      INTEGER URead
      INTEGER UWrite1
      INTEGER UWrite2
      LOGICAL FSetCauMaxAnu
      LOGICAL FSetCauMaxDia
      LOGICAL FSetCauMaxMen
      LOGICAL FUsoConvLaja
      LOGICAL FCaptAltoPolc
      LOGICAL FUsoRieOpcional
      LOGICAL FStop
      LOGICAL LajaLPar(DimLLaja)
      DATA CauDemNuReDefecto /                                          &
     &     13.00d0,  0.00d0,  0.00d0,  0.00d0,  0.00d0, 19.50d0,        &
     &     42.25d0, 55.25d0, 65.00d0, 65.00d0, 52.00d0, 32.50d0         &
     &     /
      DATA CauDemRegTucaDefecto /                                       &
     &     18.00d0,  0.00d0,  0.00d0,  0.00d0,  0.00d0, 27.00d0,        &
     &     58.50d0, 76.50d0, 90.00d0, 90.00d0, 72.00d0, 45.00d0         &
     &     /
      DATA CauDemRegAbaDefecto /                                        &
     &     9.40d0,  0.00d0,  0.00d0,  0.00d0,  0.00d0, 14.10d0,         &
     &     30.55d0, 39.95d0, 47.00d0, 47.00d0, 37.60d0, 23.50d0         &
     &     /
      CHARACTER*42 Objeto
      CHARACTER*72 Clave
      INTEGER IOS
      LOGICAL SigueLectura

      INTEGER UDebLog
!

      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION GenMax
      INTEGER IVolLaja
      INTEGER IEta
      INTEGER IGen
      INTEGER IBlo

      UDebLog = LajaCPar%UDebLog

      FStop = .FALSE.
      URead = Abrir(NArcLaja, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'laja0 : Info, no existe archivo ',     &
     &        NArcLaja, '.'
         LajaLPar(IUsoConvLaja) = .FALSE.
         RETURN
      ENDIF
!
      NCenHidSPP = NCenEmb + NCenSer
      READ(URead, '(A)', IOSTAT = IOS) Clave
      DO WHILE (IOS .EQ. 0)
         SigueLectura = Si
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CauDerAnuEND')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%CauDerAnuEND
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CauDerDiaEND')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%CauDerDiaEND
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CauDerMenEND')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%CauDerMenEND
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CRieCantEcol')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%CRieCantEcol
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'EcoENDGlo')) THEN
            READ(URead, *, IOSTAT = IOS) EcoENDGlo
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'EcoENDLoc')) THEN
            READ(URead, *, IOSTAT = IOS) EcoENDLoc
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'EcoNuRe')) THEN
            READ(URead, *, IOSTAT = IOS) EcoNuRe
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'FSetCauMaxAnu')) THEN
            READ(URead, *, IOSTAT = IOS) FSetCauMaxAnu
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'FSetCauMaxDia')) THEN
            READ(URead, *, IOSTAT = IOS) FSetCauMaxDia
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'FSetCauMaxMen')) THEN
            READ(URead, *, IOSTAT = IOS) FSetCauMaxMen
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'FUsoConvLaja')) THEN
            READ(URead, *, IOSTAT = IOS) FUsoConvLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'FCaptAltoPolc')) THEN
            READ(URead, *, IOSTAT = IOS) FCaptAltoPolc
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'FUsoRieOpcional')) THEN
            READ(URead, *, IOSTAT = IOS) FUsoRieOpcional
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'Gasto50cm')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%Gasto50cm
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'GastoAba')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%GastoAba
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'GastoToro')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%GastoToro
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'GastoTuc')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%GastoTuc
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'IndSimImpLaja')) THEN
            READ(URead, *, IOSTAT = IOS) IndSimImpLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomAflAbanico')) THEN
            READ(URead, *, IOSTAT = IOS) NomAflAbanico
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomAflCaptAltoPolc')) THEN
            READ(URead, *, IOSTAT = IOS) NomAflCaptAltoPolc
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomAflAntuco')) THEN
            READ(URead, *, IOSTAT = IOS) NomAflAntuco
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomAflLaja')) THEN
            READ(URead, *, IOSTAT = IOS) NomAflLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomAflRucue')) THEN
            READ(URead, *, IOSTAT = IOS) NomAflRucue
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomAflTucapel')) THEN
            READ(URead, *, IOSTAT = IOS) NomAflTucapel
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenAbanico')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenAbanico
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenAbCCCE')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenAbCCCE
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenAbLaDi')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenAbLaDi
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenAbZaCo')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenAbZaCo
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenElToro')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenElToro
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenFiltLaja')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenFiltLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenLaja')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenRieOpcional')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieOpcional
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomCenTucapel')) THEN
            READ(URead, *, IOSTAT = IOS) NomCenTucapel
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomGeneLaja')) THEN
            READ(URead, *, IOSTAT = IOS) NomGeneLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomGenElToro')) THEN
            READ(URead, *, IOSTAT = IOS) NomGenElToro
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NomVolLaja')) THEN
            READ(URead, *, IOSTAT = IOS) NomVolLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'NumAnos')) THEN
            READ(URead, *, IOSTAT = IOS) NYear
            NYear = MIN(NYear, Dim%Year)
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'PCenLimSupCol')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%PCenLimSupCol
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'PCenOpcionalLaja')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%PCenOpcionalLaja
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'Vol50cm')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%Vol50cm
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolColInf')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%VolColInf
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolColSup')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%VolColSup
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolDerAnuEND')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%VolDerAnuEND
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolDerMenEND')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%VolDerMenEND
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolExtrAnuEND')) THEN
            READ(URead, *, IOSTAT = IOS) VolExtrAnuEND
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolExtrMenEND')) THEN
            READ(URead, *, IOSTAT = IOS) VolExtrMenEND
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'VolExtrMenNuRe')) THEN
            READ(URead, *, IOSTAT = IOS) VolExtrMenNuRe
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'PCenZaCo')) THEN
            READ(URead, *, IOSTAT = IOS) LajaCPar%PCenZaCo
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CauDemNuRe')) THEN
            DO IYear = 1, NYear
               DO IMes = Abril, Marzo
                  READ(URead, *, IOSTAT = IOS)                          &
     &                 CauDemNuRe(IMes, IYear)
               ENDDO
            ENDDO
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CauDemRegTuca')) THEN
            DO IYear = 1, NYear
               DO IMes = Abril, Marzo
                  READ(URead, *, IOSTAT = IOS)                          &
     &                 CauDemRegTuca(IMes, IYear)
               ENDDO
            ENDDO
            SigueLectura = No
         ENDIF
         IF ((SigueLectura) .AND.                                       &
     &        (Clave .EQ. 'CauDemRegAba')) THEN
            DO IYear = 1, NYear
               DO IMes = Abril, Marzo
                  READ(URead, *, IOSTAT = IOS)                          &
     &                 CauDemRegAba(IMes, IYear)
               ENDDO
            ENDDO
            SigueLectura = No
         ENDIF
         IF (IOS .EQ. 0) THEN
            READ(URead, *, IOSTAT = IOS) Clave
         ELSE
            PRINT *, 'maule0: Error en el archivo de datos. Clave = ',  &
     &           Clave
         ENDIF
      ENDDO
      CALL Cerrar(URead)
      LajaLPar(IUsoConvLaja) = FUsoConvLaja
      IF (.NOT. LajaLPar(IUsoConvLaja)) THEN
         RETURN
      ENDIF
      LajaIPar(IIndSimImpLaja) = IndSimImpLaja
      LajaLPar(ISetCauMaxAnu) = FSetCauMaxAnu
      LajaLPar(ISetCauMaxDia) = FSetCauMaxDia
      LajaLPar(ISetCauMaxMen) = FSetCauMaxMen
      LajaLPar(IUsoConvLaja) = FUsoConvLaja
      LajaLPar(IUsoRieOpcional) = FUsoRieOpcional
      LajaLPar(ICaptAltoPolc) = FCaptAltoPolc
      Objeto = 'central serie clave NomCenAbZaCo, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenAbZaCo,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPAR(IIRieZaCo) = NumCen
      Objeto = 'central serie clave NomCenAbLaDi, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenAbLaDi,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPAR(IIRieLaDi) = NumCen
      Objeto = 'central serie clave NomCenAbCCCE, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenAbCCCE,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPAR(IIRieCCCE) = NumCen
      LajaIPAR(IIRieOpcionalLaja) = 0
      IF (LajaLPar(IUsoRieOpcional)) THEN
         Objeto = 'central serie clave NomCenRieOpcional, '
         CALL NomCen2NumCen(NumCen, FStop, NomCenRieOpcional,           &
     &        CenNom, NCenHidSPP, Objeto,                               &
     &        ULog)
         LajaIPAR(IIRieOpcionalLaja) = NumCen
      ENDIF
      Objeto = 'central serie clave NomCenAbanico, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenAbanico,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IICenAbanico) = NumCen
      Objeto = 'central serie clave NomCenElToro, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenElToro,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IICenElToro) = NumCen
      Objeto = 'embalse clave NomCenLaja, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenLaja,                     &
     &     CenNom, NCenEmb, Objeto,                                     &
     &     ULog)
      LajaIPar(IICenLaja) = NumCen
      Objeto = 'central serie clave NomCenTucapel, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenTucapel,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IICenTucapel) = NumCen
      Objeto = 'central serie clave NomAflAbanico, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflAbanico,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIAflAbanico) = NumCen
      Objeto = 'central serie clave NomAflCaptAltoPolc, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflCaptAltoPolc,             &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIAflCaptAltoPolc) = NumCen
      CALL NomCen2NumCen(NumCen, FStop, NomAflLaja,                     &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIAflLaja) = NumCen
      Objeto = 'central serie clave NomAflAntuco, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflAntuco,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIAflAntuco) = NumCen
      Objeto = 'central serie clave NomAflRucue, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflRucue,                    &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIAflRucue) = NumCen
      Objeto = 'central serie clave NomAflTucapel, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflTucapel,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIAflTucapel) = NumCen
      Objeto = 'embalse clave NomCenFiltLaja, '
      Call NomCen2NumFil(NArcCenFil, NumFil, FStop, NomCenFiltLaja,     &
     &     Objeto, ULog)
      LajaIPar(IIFiltLaja) = NumFil
      Objeto = 'central serie clave NomGeneLaja, '
      CALL NomCen2NumCen(NumCen, FStop, NomGeneLaja,                    &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIGeneLaja) = NumCen
      Objeto = 'central serie clave NomGenElToro, '
      CALL NomCen2NumCen(NumCen, FStop, NomGenElToro,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      LajaIPar(IIGenElToro) = NumCen
      Objeto = 'embalse clave NomVolLaja, '
      CALL NomCen2NumCen(NumCen, FStop, NomVolLaja,                     &
     &     CenNom, NCenEmb, Objeto,                                     &
     &     ULog)
      IVolLaja = NCenEmb
      NVert = NCenEmb + NCenSer
      NAfluFict = NCenEmb
      LajaIPar(IIVolLaja) = NCentral + 2*NFlujo +                       &
     &     NVert + NAfluFict + NumCen
      LajaIPar(IIVertLaja) = NCentral + 2*NFlujo + NumCen
      IF (FStop) THEN
         WRITE(6, '(A)') 'laja0: Debe reparar la base de datos.'
         WRITE(ULog, '(A)') 'laja0: Debe reparar la base de datos.'
         STOP 1
      ENDIF
      IF (IndSimImpLaja .GT. 0) THEN
         UWrite1 = Abrir(NArcLajaOut1, 'UNKNOWN', 'SEQUENTIAL', ULog)
         UWrite2 = Abrir(NArcLajaOut2, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite1, '(A, '','', $)') 'Eta'
         WRITE(UWrite1, '(A, '','', $)') 'Mes'
         WRITE(UWrite1, '(A, '','', $)') 'CauExt'
         WRITE(UWrite1, '(A, '','', $)') 'Cau2Vo'
         WRITE(UWrite1, '(A, '','', $)') 'CauFil'
         WRITE(UWrite1, '(A, '','', $)') 'VolExt'
         WRITE(UWrite1, '(A, '','', $)') 'VolFil'
         WRITE(UWrite1, '(A, '','', $)') 'VolAfL'
         WRITE(UWrite1, '(A, '','', $)') 'VolExA'
         WRITE(UWrite1, '(A, '','', $)') 'VolExM'
         WRITE(UWrite1, '(A, '','', $)') 'VolVer'
         WRITE(UWrite1, '(A, '','', $)') 'VolRea'
         WRITE(UWrite1, '(A, '','', $)') 'CauMin'
         WRITE(UWrite1, '(A, '','', $)') 'CauMax'
         WRITE(UWrite1, '(A, '','', $)') 'EcoNuRe'
         WRITE(UWrite1, '(A, '','', $)') 'VolAnu'
         WRITE(UWrite1, '(A, '','', $)') 'VolExA'
         WRITE(UWrite1, '(A, '','', $)') 'EENDLo'
         WRITE(UWrite1, '(A, '','', $)') 'EENDGl'
         WRITE(UWrite1, '(A, '','', $)') 'EcoEND'
         WRITE(UWrite1, *)
         WRITE(UWrite2, '(A, '','', $)') 'Eta'
         WRITE(UWrite2, '(A, '','', $)') 'Mes'
         WRITE(UWrite2, '(A, '','', $)') 'Cau2Vol'
         WRITE(UWrite2, '(A, '','', $)') 'CauDerDiaEND'
         WRITE(UWrite2, '(A, '','', $)') 'VolDerDiaENDEta'
         WRITE(UWrite2, '(A, '','', $)') 'VolExtrMenEND'
         WRITE(UWrite2, '(A, '','', $)') 'VolExtrAnuEND'
         WRITE(UWrite2, '(A, '','', $)') 'VolExtrRemMenEND'
         WRITE(UWrite2, '(A, '','', $)') 'VolExtrRemAnuEND'
         WRITE(UWrite2, '(A, '','', $)') 'VoluLaja'
         WRITE(UWrite2, '(A, '','', $)') 'VolRealMax'
         WRITE(UWrite2, '(A, '','', $)') 'VolVac'
         WRITE(UWrite2, '(A, '','', $)') 'VolExtrMax'
         WRITE(UWrite2, '(A, '','', $)') 'VolFilt'
         WRITE(UWrite2, '(A, '','', $)') 'CauExtrMax'
         WRITE(UWrite2, '(A, '','', $)') 'CauCaptAltoPolc'
         WRITE(UWrite2, '(A, '','', $)') 'CauAba'
         WRITE(UWrite2, '(A, '','', $)') 'CauDemRegAba'
         WRITE(UWrite2, '(A, '','', $)') 'CauAflTucapel'
         WRITE(UWrite2, '(A, '','', $)') 'CauExtrMin'
         WRITE(UWrite2, '(A, '','', $)') 'CauAflHoInAba'
         WRITE(UWrite2, '(A, '','', $)') 'CauFiltLaja'
         WRITE(UWrite2, '(A, '','', $)') 'CauAflAntuco'
         WRITE(UWrite2, '(A, '','', $)') 'CauAflRucue'
         WRITE(UWrite2, '(A, '','', $)') 'CauRieLaDi'
         WRITE(UWrite2, '(A, '','', $)') 'CauRieZaCo'
         WRITE(UWrite2, '(A, '','', $)') 'CauVac'
         WRITE(UWrite2, '(A, '','', $)') 'GenMaxToro'
         WRITE(UWrite2, '(A, '','', $)') 'VolUtil'
         WRITE(UWrite2, '(A, '','', $)') 'VolColInf'
         WRITE(UWrite2, '(A, '','', $)') 'EcoEND'
         WRITE(UWrite2, '(A, '','', $)') 'EcoNuRe'
         WRITE(UWrite2, '(A, '','', $)') 'CauAflHoInTucapel'
         WRITE(UWrite2, *)
      ELSE
         UWrite1 = 0
         UWrite2 = 0
      ENDIF
      VoluLaja = EmbVIni(LajaIPar(IICenLaja))
      DO ISimul = 1, NSimul
!
!     Insercion de datos de LajaRPar.
         LajaRPar(IEcoENDGlo, ISimul, 0) = EcoENDGlo
         LajaRPar(IEcoENDLoc, ISimul, 0) = EcoENDLoc
         LajaRPar(IEcoNuRe, ISimul, 0) = EcoNuRe
         LajaRPar(IVoluLaja, ISimul, 0) = VoluLaja
         LajaRPar(IVolExtrAnuEND, ISimul, 0) = VolExtrAnuEND
         LajaRPar(IVolExtrMenEND, ISimul, 0) = VolExtrMenEND
         LajaRPar(IVolExtrMenNuRe, ISimul, 0) = VolExtrMenNuRe
!
      ENDDO
      LajaIPar(IILajaWrite1) = UWrite1
      LajaIPar(IILajaWrite2) = UWrite2
      DO IYear = NYear + 1, Dim%Year
         DO IMes = 1, 12
            CauDemNuRe(IMes, IYear) = CauDemNuReDefecto(IMes)
            CauDemRegTuca(IMes, IYear) = CauDemRegTucaDefecto(IMes)
            CauDemRegAba(IMes, IYear) = CauDemRegAbaDefecto(IMes)
         ENDDO
      ENDDO
      DO IEtapa = 1, NEtapa
         NMes =                                                         &
     &     MAX(1, NINT(EtaDur(IEtapa)*FactTiempo/(3.6d0*24d0*31d0))) - 1
         CauDemNuReProm = 0d0
         CauDemRegTucaProm = 0d0
         CauDemRegAbaProm = 0d0
         IYear = Year(IEtapa)
         DO IMes0 = 0, NMes
            IMes = MOD(Mes(IEtapa) + IMes0 - 1, 12) + 1
            CauDemNuReProm = CauDemNuReProm + CauDemNuRe(IMes, IYear)
            CauDemRegTucaProm = CauDemRegTucaProm +                     &
     &           CauDemRegTuca(IMes, IYear)
            CauDemRegAbaProm = CauDemRegAbaProm +                       &
     &           CauDemRegAba(IMes, IYear)
         ENDDO
         CauDemNuReProm = CauDemNuReProm/DBLE(NMes + 1)
         CauDemRegTucaProm = CauDemRegTucaProm/DBLE(NMes + 1)
         CauDemRegAbaProm = CauDemRegAbaProm/DBLE(NMes + 1)
         CauDemNuReEta(IEtapa) = CauDemNuReProm
         CauDemRegTucaEta(IEtapa) = CauDemRegTucaProm
         CauDemRegAbaEta(IEtapa) = CauDemRegAbaProm
!$$$  CauDemRegAba(IEtapa) = GastoAba*CauDemNuReProm
!$$$  CauDemRegAba(IEtapa) = GastoAba
      ENDDO

      MesBal(1) = No

      DO IEtapa = 2, NEtapa
!     Cubos a millones de metros cubicos.
         Cau2Vol(IEtapa) = FactTiempo*EtaDur(IEtapa)
!     1 < = Mes(IEtapa) < = 12
         IF (IEtapa .GT. 1) THEN
            IF ((Mes(IEtapa - 1) .LE. Diciembre) .AND.                  &
     &           (Mes(IEtapa) .GE. Enero)) THEN
!     Fecha del balance de las economias: 1 de enero.
               MesBal(IEtapa) = Si
            ELSE
               MesBal(IEtapa) = No
            ENDIF
         ELSE
            MesBal(IEtapa) = No
         ENDIF
      ENDDO
      LajaRPar(ICau2VolRM, 1, NEtapa) = Cau2Vol(NEtapa)
      LajaRPar(ICau2VolRY, 1, NEtapa) = Cau2Vol(NEtapa)
      DO IEtapa = NEtapa - 1, 1, -1
         IF (Mes(IEtapa + 1) .NE. Mes(IEtapa)) THEN
            LajaRPar(ICau2VolRM, 1, IEtapa) = Cau2Vol(IEtapa)
            IF (Mes(IEtapa + 1) .EQ. Enero) THEN
               LajaRPar(ICau2VolRY, 1, IEtapa) = Cau2Vol(IEtapa)
            ELSE
               LajaRPar(ICau2VolRY, 1, IEtapa) =                        &
     &              LajaRPar(ICau2VolRY, 1, IEtapa + 1) +               &
     &              Cau2Vol(IEtapa)
            ENDIF
         ELSE
            LajaRPar(ICau2VolRY, 1, IEtapa) =                           &
     &           LajaRPar(ICau2VolRY, 1, IEtapa + 1) + Cau2Vol(IEtapa)
            LajaRPar(ICau2VolRM, 1, IEtapa) =                           &
     &           LajaRPar(ICau2VolRM, 1, IEtapa + 1) + Cau2Vol(IEtapa)
         ENDIF
      ENDDO
      DO ISimul = 1, NSimul
         LajaRPar(IVolExtrMinRY, ISimul, NEtapa + 1) = 0d0
         LajaRPar(IVolExtrMinRM, ISimul, NEtapa + 1) = 0d0
         LajaRPar(IVolAflCaptAltoPolcRM, ISimul, NEtapa + 1) = 0d0
         LajaRPar(IVolAflCaptAltoPolcRY, ISimul, NEtapa + 1) = 0d0
!$$$ TODO: verificar si el inicio de este ciclo es correcto
         DO IEtapa = NEtapa, 1, -1
            IClase(1:Dim%EstocFila) = SimulInd(ISimul, IEtapa)

!           VolAflLaja = EstocRHSP(LajaIPar(IIAflLaja), IClase, IEtapa)
            VolAflLaja = QAfluEta(IEtapa, NBloque, BloInd,              &
     &           BloDur, EstocRHSP, IClase,                             & 
     &           LajaIPar(IIAflLaja), Dim) * Cau2Vol(IEtapa)

!           VolAflCaptAltoPolc =                                       &
!     &           EstocRHSP(LajaIPar(IIAflCaptAltoPolc), IClase, IEtapa)
            VolAflCaptAltoPolc = QAfluEta(IEtapa, NBloque, BloInd,      &
     &           BloDur, EstocRHSP, IClase,                             & 
     &           LajaIPar(IIAflCaptAltoPolc), Dim) * Cau2Vol(IEtapa)

!           VolAflHoInTucapel =                                        &
!     &           EstocRHSP(LajaIPar(IIAflTucapel), IClase, IEtapa)
            VolAflHoInTucapel = QAfluEta(IEtapa, NBloque, BloInd,       &
     &           BloDur, EstocRHSP, IClase,                             & 
     &           LajaIPar(IIAflTucapel), Dim) * Cau2Vol(IEtapa) 

!           VolAflHoInAba =                                            &
!     &           EstocRHSP(LajaIPar(IIAflAbanico), IClase, IEtapa)
            VolAflHoInAba = QAfluEta(IEtapa, NBloque, BloInd,           &
     &           BloDur, EstocRHSP, IClase,                             & 
     &           LajaIPar(IIAflAbanico), Dim) * Cau2Vol(IEtapa) 

!     VolAflAntuco NO INCLUYE las captaciones de Alto Polcura.
!           VolAflAntuco =                                              &
!     &           EstocRHSP(LajaIPar(IIAflAntuco), IClase, IEtapa)
            VolAflAntuco = QAfluEta(IEtapa, NBloque, BloInd,            &
     &           BloDur, EstocRHSP, IClase,                             & 
     &           LajaIPar(IIAflAntuco), Dim) * Cau2Vol(IEtapa) 

!           VolAflRucue =                                               &
!     &           EstocRHSP(LajaIPar(IIAflRucue), IClase, IEtapa)
            VolAflRucue = QAfluEta(IEtapa, NBloque, BloInd,             &
     &           BloDur, EstocRHSP, IClase,                             & 
     &           LajaIPar(IIAflRucue), Dim) * Cau2Vol(IEtapa)

            VolAflAba = VolAflHoInAba +                                 &
     &           FiltProm(LajaIPar(IIFiltLaja), IEtapa + 1)*            &
     &           Cau2Vol(IEtapa)
            VolAflTucapel = VolAflHoInAba +                             &
     &           FiltProm(LajaIPar(IIFiltLaja), IEtapa + 1)*            &
     &           Cau2Vol(IEtapa) +                                      &
     &           VolAflAntuco + VolAflHoInTucapel + VolAflRucue
            VolDemRegTucaEta = CauDemRegTucaEta(IEtapa)*Cau2Vol(IEtapa)
            VolDemNuReEta =                                             &
     &           CauDemNuReEta(IEtapa)*Cau2Vol(IEtapa)
            IF (LajaLPar(ICaptAltoPolc)) THEN
               VolRieDefAban = pp(                                      &
     &              CauDemRegAbaEta(IEtapa)*Cau2Vol(IEtapa) +           &
     &              VolAflCaptAltoPolc - VolAflAba)
            ELSE
               VolRieDefAban = pp(                                      &
     &              CauDemRegAbaEta(IEtapa)*Cau2Vol(IEtapa) -           &
     &              VolAflAba)
            ENDIF
            VolRieDefTuca = pp(VolDemRegTucaEta - VolAflTucapel)
            VolRieDefAnRe = MIN(VolRieDefAban, VolRieDefTuca)
            VolDerAnRe = MIN(                                           &
     &           VolDemRegTucaEta,                                      &
     &           VolRieDefAnRe + VolAflTucapel)
            VolExcAnRe = pp(VolRieDefAnRe + VolAflTucapel - VolDerAnRe)
            VolRieDefNuRe = pp(VolDemNuReEta - VolExcAnRe)
            VolExtrMin =                                                &
     &           VolRieDefAnRe + VolRieDefNuRe
            IIEtapa = MIN(NEtapa, IEtapa + 1)
            IF (                                                        &
     &           (IEtapa .EQ. NEtapa) .OR.                             &
     &           (Mes(IIEtapa) .NE. Mes(IEtapa))                        &
     &           ) THEN
               LajaRPar(IVolExtrMinRM, ISimul, IEtapa) = VolExtrMin
               LajaRPar(IVolAflCaptAltoPolcRM, ISimul, IEtapa) =        &
     &              VolAflCaptAltoPolc
               IF (Mes(IIEtapa) .EQ. Enero) THEN
                  LajaRPar(IVolExtrMinRY, ISimul, IEtapa) = VolExtrMin
                  LajaRPar(IVolAflCaptAltoPolcRY, ISimul, IEtapa) =     &
     &                 VolAflCaptAltoPolc
               ELSE
                  LajaRPar(IVolExtrMinRY, ISimul, IEtapa) =             &
     &                 LajaRPar(IVolExtrMinRY, ISimul, IEtapa + 1) +    &
     &                 VolExtrMin
                  LajaRPar(IVolAflCaptAltoPolcRY, ISimul, IEtapa) =     &
     &                 LajaRPar(IVolAflCaptAltoPolcRY, ISimul,          &
     &                 IEtapa + 1) + VolAflCaptAltoPolc
               ENDIF
            ELSE
               LajaRPar(IVolExtrMinRY, ISimul, IEtapa) =                &
     &              LajaRPar(IVolExtrMinRY, ISimul, IEtapa + 1) +       &
     &              VolExtrMin
               LajaRPar(IVolExtrMinRM, ISimul, IEtapa) =                &
     &              LajaRPar(IVolExtrMinRM, ISimul, IEtapa + 1) +       &
     &              VolExtrMin
               LajaRPar(IVolAflCaptAltoPolcRY, ISimul, IEtapa) =        &
     &              LajaRPar(IVolAflCaptAltoPolcRY, ISimul,             &
     &              IEtapa + 1) + VolAflCaptAltoPolc
               LajaRPar(IVolAflCaptAltoPolcRM, ISimul, IEtapa) =        &
     &              LajaRPar(IVolAflCaptAltoPolcRM, ISimul,             &
     &              IEtapa + 1) + VolAflCaptAltoPolc
            ENDIF
            IF (ISimul .EQ. LajaIPar(IIndSimImpLaja)) THEN
               WRITE(UDebLog, '(I4, '','', $)') IEtapa
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflLaja
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflCaptAltoPolc
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflCaptAltoPolc
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflHoInTucapel
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflHoInAba
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflAntuco
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflRucue
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflAba
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolAflTucapel
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolDemRegTucaEta
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolDemNuReEta
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolRieDefAban
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolRieDefTuca
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolRieDefAnRe
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolDerAnRe
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolExcAnRe
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolRieDefNuRe
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              VolExtrMin
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              LajaRPar(IVolExtrMinRM, ISimul, IEtapa)
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              LajaRPar(IVolExtrMinRY, ISimul, IEtapa)
               WRITE(UDebLog, '(F10.2, '','', $)')                      &
     &              LajaRPar(IVolAflCaptAltoPolcRM, ISimul, IEtapa)
               WRITE(UDebLog, '(F10.2)')                                &
     &              LajaRPar(IVolAflCaptAltoPolcRY, ISimul, IEtapa)
            ENDIF
         ENDDO
      ENDDO


      ALLOCATE(LajaCPar%IBloInd(LajaBloQVarBeg:LajaBloVVarEnd, Dim%IBlo, Dim%Eta))
      ALLOCATE(LajaCPar%IEtaInd(LajaEtaVVarBeg:LajaEtaVVarEnd, Dim%Eta))
 
      ALLOCATE(LajaCPar%UppGenElToro(Dim%Eta, Dim%Simul))
      ALLOCATE(LajaCPar%UppGenZaCo(Dim%Eta, Dim%Simul))


      IGen = LajaIPar(IIGenElToro)
      DO ISimul = 1, NSimul
         DO IEta = 1, NEtapa
            GenMax = 0.0d0
            DO IBlo = 1, NBloque(IEta)
               GenMax = MAX(GenMax, &
     &              CenPMax(IGen, BloInd(IBlo, IEta), CenManSInd(ISimul)))
            ENDDO
            LajaCPar%UppGenElToro(IEta, ISimul) = GenMax
         ENDDO
      ENDDO

      IGen = LajaIPar(IIRieZaCo)
      DO ISimul = 1, NSimul
         DO IEta = 1, NEtapa
            GenMax = 0.0d0
            DO IBlo = 1, NBloque(IEta)
               GenMax = MAX(GenMax, &
     &              CenPMax(IGen, BloInd(IBlo, IEta), CenManSInd(ISimul)))
            ENDDO
            LajaCPar%UppGenZaCo(IEta, ISimul) = GenMax
         ENDDO
      ENDDO


      RETURN
      END
!
      DOUBLE PRECISION FUNCTION pp(x)
      DOUBLE PRECISION x
      pp = MAX(x, 0.0d0)
      RETURN
      END
