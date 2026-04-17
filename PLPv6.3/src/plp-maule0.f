!     Lee los datos e inicializa los arreglos necesarios para la
!     modelacion del convenio del Maule.
      SUBROUTINE Maule0(Cau2Vol,                                        &
     &     MauleIPar, MauleCPar, MauleLPar, MauleRPar,                  &
     &     Mes, CauConMauEta, CauRes105Eta,                             &
     &     CenNom, CenGHid, CenVHid, CenInd,                            &
     &     NBloque, BloInd,                                             &
     &     CenManSInd, CenPMin, CenPMax, CenVMin, CenVMax,              &
     &     NEtapa, NCentral,                                            &
     &     NSimul, NFlujo, NCenEmb, NCenSer, NCenPas,                   &
     &     EtaDur, EmbVIni, FactTiempo, Year, Ulog, Dim)
!      USE PLP, ONLY : PAR_DIMS, PAR_MAULEC, DimIMaule, DimRMaule, DimLMaule

      USE PLP
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!

      EXTERNAL Abrir
      INTEGER Abrir
!
      TYPE(PAR_MAULEC) MauleCPar

      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      INTEGER CenManSInd(Dim%Simul)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenVMin(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenVMax(Dim%Vert, Dim%Blo)

      LOGICAL FWarning
      INTEGER Ind1, Ind2
      INTEGER CenInd(Dim%Cen)
      INTEGER CenVAux(Dim%Cen, 2)
      INTEGER CenGAux(Dim%Cen, 2)
      INTEGER NControl
      CHARACTER*1 Connection
      LOGICAL FConnection
      CHARACTER*1 CnxNomGenCip
      CHARACTER*1 CnxNomGenMaule
      CHARACTER*1 CnxNomExtMauEND
      CHARACTER*1 CnxNomExtMauRie
      CHARACTER*1 CnxNomExtInvEND
      CHARACTER*1 CnxNomExtInvRie
      CHARACTER*1 CnxNomControl
      CHARACTER*1 CnxNomVertCip
      CHARACTER*1 CnxNomVertMaule
      CHARACTER*1 CnxNomBajoControl(Dim%Cen)
      CHARACTER*72 NomBajoControl(Dim%Cen)
!$$$  CHARACTER*12 AuxVar
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*72 NomCenRieCMNA
      CHARACTER*72 NomCenRieCMNB
      CHARACTER*72 NomCenRieCMel
      CHARACTER*72 NomCenRieS123
      CHARACTER*72 NomCenRieOpcional
      CHARACTER*72 NomCenRieOReg
      CHARACTER*72 NomCenFiltInve
      CHARACTER*72 NomCenInve
      CHARACTER*72 NomCenMaule
      CHARACTER*72 NomAflArmerillo
      CHARACTER*72 NomAflBocMaule
      CHARACTER*72 NomAflColbun
      CHARACTER*72 NomAflInve
      CHARACTER*72 NomAflIsla
      CHARACTER*72 NomAflMina
      CHARACTER*72 NomAflMaule
      CHARACTER*72 NomAflPehuenche
      CHARACTER*72 NomGenCip
      CHARACTER*72 NomGenMaule
      CHARACTER*72 NomExtMauEND
      CHARACTER*72 NomExtMauRie
      CHARACTER*72 NomExtInvEND
      CHARACTER*72 NomExtInvRie
      CHARACTER*72 NomControl
      CHARACTER*72 NomVertCip
      CHARACTER*72 NomVertMaule
      CHARACTER*72 NomVolColbun
      CHARACTER*48 NameControl
      CHARACTER*48 NameHydUnit
      DOUBLE PRECISION EtaDur(Dim%Eta)
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauConMau(12, Dim%Year)
      DOUBLE PRECISION CauConMauEta(Dim%Eta)
      DOUBLE PRECISION CauConMauDefecto(12)
      DOUBLE PRECISION CauConMauProm
      DOUBLE PRECISION CauRes105(12, Dim%Year)
      DOUBLE PRECISION CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION CauRes105Defecto(12)
      DOUBLE PRECISION CauRes105Prom
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION VolCompEND
      DOUBLE PRECISION VolDisResOrdEND
      DOUBLE PRECISION VolDisResOrdRie
      DOUBLE PRECISION VolEcoInv
      DOUBLE PRECISION VolInv
      DOUBLE PRECISION VolMau
      DOUBLE PRECISION VolResCuoExt
      DOUBLE PRECISION VolResMauEND
      DOUBLE PRECISION VolResMauRie
      INTEGER IHydUnit
      INTEGER NHydUnit
      INTEGER CenGHid(Dim%Cen, 2)
      INTEGER CenVHid(Dim%Cen, 2)
      INTEGER I
      INTEGER IEtapa
      INTEGER IMes
      INTEGER IMes0
      INTEGER IndSimImpMaule
      INTEGER IPLPExtEND
      INTEGER IPLPExtENDCI
      INTEGER IPLPExtRie
      INTEGER ISimul
      INTEGER IUsoRieOp
      INTEGER ICompRiegoInve
      INTEGER IUsoConvMaule
      INTEGER IVerifConvenio
      INTEGER IYear
      INTEGER MauleIPar(DimIMaule)
      INTEGER Mes(Dim%Eta)
      INTEGER NAfluFict
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NCenHid
      INTEGER NCenHidSPP
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NMes
      INTEGER NSimul
      INTEGER NumBajoControl
      INTEGER NumCen
      INTEGER NumFil
      INTEGER NVert
      INTEGER NYear
      INTEGER ULog
      INTEGER URead
      INTEGER UWrite1
      INTEGER UWrite2
      INTEGER UWrite3
      INTEGER UWrite4
      INTEGER Year(Dim%Eta)
      LOGICAL FPasoPorResOrd
      LOGICAL FVieneDePorSup
      LOGICAL FVieneDeResOrd
      LOGICAL FStop
      LOGICAL MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)
      DATA CauConMauDefecto /                                           &
     &     60.0D0,  20.0D0,   0.0D0,   0.0D0,   0.0D0,  40.0D0,         &
     &     100.0D0, 182.0D0, 200.0D0, 200.0D0, 160.0D0, 110.0D0         &
     &     /
      DATA CauRes105Defecto /                                           &
     &     80.0D0,  40.0D0,  40.0D0,  40.0D0,  40.0D0,  60.0D0,         &
     &     140.0D0, 180.0D0, 200.0D0, 200.0D0, 180.0D0, 120.0D0         &
     &     /
      CHARACTER*42 Objeto
      CHARACTER*72 Clave
      INTEGER IOS

      INTEGER IEta
      INTEGER IBlo
      DOUBLE PRECISION GenMax
      DOUBLE PRECISION GenMin

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!
      FStop = .FALSE.
      URead = Abrir(NArcMaule, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'maule0: Info, no existe archivo ',     &
     &        NArcMaule, '.'
         MauleIPar(IIUsoConvMaule) = 0
         RETURN
      ENDIF
      FWarning = .FALSE.
!
      IndSimImpMaule = 0
      IPLPExtEND = 1
      IPLPExtENDCI = 1
      IPLPExtRie = 1
      IUsoConvMaule = 0
      IVerifConvenio = 0
      IUsoRieOp = 0
      ICompRiegoInve = 1
      NCenHidSPP = NCenEmb + NCenSer
      NCenHid = NCenEmb + NCenSer + NCenPas
      READ(URead, '(A)', IOSTAT = IOS) Clave
      DO WHILE (IOS .EQ. 0)
         IF (Clave .EQ. 'CnxNomGenCip') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomGenCip
         ELSEIF (Clave .EQ. 'CnxNomGenMaule') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomGenMaule
         ELSEIF (Clave .EQ. 'CnxNomExtMauEND') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomExtMauEND
         ELSEIF (Clave .EQ. 'CnxNomExtMauRie') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomExtMauRie
         ELSEIF (Clave .EQ. 'CnxNomExtInvEND') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomExtInvEND
         ELSEIF (Clave .EQ. 'CnxNomExtInvRie') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomExtInvRie
         ELSEIF (Clave .EQ. 'CnxNomVertCip') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomVertCip
         ELSEIF (Clave .EQ. 'CnxNomVertMaule') THEN
            READ(URead, *, IOSTAT = IOS) CnxNomVertMaule
         ELSEIF (Clave .EQ. 'FPasoPorResOrd') THEN
            READ(URead, *, IOSTAT = IOS) FPasoPorResOrd
         ELSEIF (Clave .EQ. 'IVerifConvenio') THEN
            READ(URead, *, IOSTAT = IOS) IVerifConvenio
         ELSEIF (Clave .EQ. 'FVieneDePorSup') THEN
            READ(URead, *, IOSTAT = IOS) FVieneDePorSup
         ELSEIF (Clave .EQ. 'FVieneDeResOrd') THEN
            READ(URead, *, IOSTAT = IOS) FVieneDeResOrd
         ELSEIF (Clave .EQ. 'GastoMaxCMel') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%GastoMaxCMel
         ELSEIF (Clave .EQ. 'GastoMedMenMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%GastoMedMenMax
         ELSEIF (Clave .EQ. 'IndSimImpMaule') THEN
            READ(URead, *, IOSTAT = IOS) IndSimImpMaule
         ELSEIF (Clave .EQ. 'IPLPExtEND') THEN
            READ(URead, *, IOSTAT = IOS) IPLPExtEND
         ELSEIF (Clave .EQ. 'IPLPExtENDCI') THEN
            READ(URead, *, IOSTAT = IOS) IPLPExtENDCI
         ELSEIF (Clave .EQ. 'IPLPExtRie') THEN
            READ(URead, *, IOSTAT = IOS) IPLPExtRie
         ELSEIF (Clave .EQ. 'IUsoConvMaule') THEN
            READ(URead, *, IOSTAT = IOS) IUsoConvMaule
         ELSEIF (Clave .EQ. 'IVerifConvenio') THEN
            READ(URead, *, IOSTAT = IOS) IVerifConvenio
         ELSEIF (Clave .EQ. 'ICompRiegoInve') THEN
            READ(URead, *, IOSTAT = IOS) ICompRiegoInve
         ELSEIF (Clave .EQ. 'IUsoRieOpcional') THEN
            READ(URead, *, IOSTAT = IOS) IUsoRieOp
         ELSEIF (Clave .EQ. 'NomAflArmerillo') THEN
            READ(URead, *, IOSTAT = IOS) NomAflArmerillo
         ELSEIF (Clave .EQ. 'NomAflBocMaule') THEN
            READ(URead, *, IOSTAT = IOS) NomAflBocMaule
         ELSEIF (Clave .EQ. 'NomAflColbun') THEN
            READ(URead, *, IOSTAT = IOS) NomAflColbun
         ELSEIF (Clave .EQ. 'NomAflInve') THEN
            READ(URead, *, IOSTAT = IOS) NomAflInve
         ELSEIF (Clave .EQ. 'NomAflIsla') THEN
            READ(URead, *, IOSTAT = IOS) NomAflIsla, NomAflMina
         ELSEIF (Clave .EQ. 'NomAflMaule') THEN
            READ(URead, *, IOSTAT = IOS) NomAflMaule
         ELSEIF (Clave .EQ. 'NomAflPehuenche') THEN
            READ(URead, *, IOSTAT = IOS) NomAflPehuenche
         ELSEIF (Clave .EQ. 'NomCenFiltInve') THEN
            READ(URead, *, IOSTAT = IOS) NomCenFiltInve
         ELSEIF (Clave .EQ. 'NomCenInve') THEN
            READ(URead, *, IOSTAT = IOS) NomCenInve
         ELSEIF (Clave .EQ. 'NomCenMaule') THEN
            READ(URead, *, IOSTAT = IOS) NomCenMaule
         ELSEIF (Clave .EQ. 'NomCenRieCMel') THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieCMel
         ELSEIF (Clave .EQ. 'NomCenRieCMNA') THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieCMNA
         ELSEIF (Clave .EQ. 'NomCenRieCMNB') THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieCMNB
         ELSEIF (Clave .EQ. 'NomCenRieOpcional') THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieOpcional
         ELSEIF (Clave .EQ. 'NomCenRieOReg') THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieOReg
         ELSEIF (Clave .EQ. 'NomCenRieS123') THEN
            READ(URead, *, IOSTAT = IOS) NomCenRieS123
         ELSEIF (Clave .EQ. 'NomGenCip') THEN
            READ(URead, *, IOSTAT = IOS) NomGenCip
         ELSEIF (Clave .EQ. 'NomGenMaule') THEN
            READ(URead, *, IOSTAT = IOS) NomGenMaule
         ELSEIF (Clave .EQ. 'NomExtMauEND') THEN
            READ(URead, *, IOSTAT = IOS) NomExtMauEND
         ELSEIF (Clave .EQ. 'NomExtMauRie') THEN
            READ(URead, *, IOSTAT = IOS) NomExtMauRie
         ELSEIF (Clave .EQ. 'NomExtInvEND') THEN
            READ(URead, *, IOSTAT = IOS) NomExtInvEND
         ELSEIF (Clave .EQ. 'NomExtInvRie') THEN
            READ(URead, *, IOSTAT = IOS) NomExtInvRie
         ELSEIF (Clave .EQ. 'NomVertCip') THEN
            READ(URead, *, IOSTAT = IOS) NomVertCip
         ELSEIF (Clave .EQ. 'NomVertMaule') THEN
            READ(URead, *, IOSTAT = IOS) NomVertMaule
         ELSEIF (Clave .EQ. 'NomVolColbun') THEN
            READ(URead, *, IOSTAT = IOS) NomVolColbun
         ELSEIF (Clave .EQ. 'NumAnos') THEN
            READ(URead, *, IOSTAT = IOS) NYear
            NYear = MIN(NYear, Dim%Year)
         ELSEIF (Clave .EQ. 'PCenCMel') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenCMel
         ELSEIF (Clave .EQ. 'PCenCMNA1') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenCMNA1
         ELSEIF (Clave .EQ. 'PCenCMNA2') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenCMNA2
         ELSEIF (Clave .EQ. 'PCenCMNB1') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenCMNB1
         ELSEIF (Clave .EQ. 'PCenCMNB2') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenCMNB2
         ELSEIF (Clave .EQ. 'PCenOpcionalMaule') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenOpcionalMaule
         ELSEIF (Clave .EQ. 'PCenOReg') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenOReg
         ELSEIF (Clave .EQ. 'PCenS123') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%PCenS123
         ELSEIF (Clave .EQ. 'VolColbLim') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolColbLim
         ELSEIF (Clave .EQ. 'VolCompEND') THEN
            READ(URead, *, IOSTAT = IOS) VolCompEND
         ELSEIF (Clave .EQ. 'VolCompENDMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolCompENDMax
         ELSEIF (Clave .EQ. 'VolCuoExtMau') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolCuoExtMau
         ELSEIF (Clave .EQ. 'VolDisResOrdEND') THEN
            READ(URead, *, IOSTAT = IOS) VolDisResOrdEND
         ELSEIF (Clave .EQ. 'VolDisResOrdRie') THEN
            READ(URead, *, IOSTAT = IOS) VolDisResOrdRie
         ELSEIF (Clave .EQ. 'VolEcoInv') THEN
            READ(URead, *, IOSTAT = IOS) VolEcoInv
         ELSEIF (Clave .EQ. 'VolInvMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolInvMax
         ELSEIF (Clave .EQ. 'VolMauMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolMauMax
         ELSEIF (Clave .EQ. 'VolMaxEND') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolMaxEND
         ELSEIF (Clave .EQ. 'VolMaxRie') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolMaxRie
         ELSEIF (Clave .EQ. 'VolPorSupMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolPorSupMax
         ELSEIF (Clave .EQ. 'VolResCuoExt') THEN
            READ(URead, *, IOSTAT = IOS) VolResCuoExt
         ELSEIF (Clave .EQ. 'VolResExtMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolResExtMax
         ELSEIF (Clave .EQ. 'VolResMauEND') THEN
            READ(URead, *, IOSTAT = IOS) VolResMauEND
         ELSEIF (Clave .EQ. 'VolResMauRie') THEN
            READ(URead, *, IOSTAT = IOS) VolResMauRie
         ELSEIF (Clave .EQ. 'VolResOrdMax') THEN
            READ(URead, *, IOSTAT = IOS) MauleCPar%VolResOrdMax
         ELSEIF (Clave .EQ. 'Control') THEN
            READ(URead, *, IOSTAT = IOS) NomControl
            READ(URead, *, IOSTAT = IOS) CnxNomControl
         ELSEIF (Clave .EQ. 'BajoControl') THEN
            READ(URead, *, IOSTAT = IOS) NumBajoControl
            DO I = 1, NumBajoControl
               READ(URead, *, IOSTAT = IOS) NomBajoControl(I)
               READ(URead, *, IOSTAT = IOS) CnxNomBajoControl(I)
            ENDDO
         ELSEIF (Clave .EQ. 'CauConMau') THEN
            DO IYear = 1, NYear
               DO IMes = Abril, Marzo
                  READ(URead, *, IOSTAT = IOS)                          &
     &                 CauConMau(IMes, IYear)
               ENDDO
            ENDDO
         ELSEIF (Clave .EQ. 'CauRes105') THEN
            DO IYear = 1, NYear
               DO IMes = Abril, Marzo
                  READ(URead, *, IOSTAT = IOS)                          &
     &                 CauRes105(IMes, IYear)
               ENDDO
            ENDDO
         ENDIF
         IF (IOS .EQ. 0) THEN
            READ(URead, *, IOSTAT = IOS) Clave
         ELSE
            PRINT *, 'maule0: Error en el archivo de datos. Clave = ',  &
     &           Clave
         ENDIF
      ENDDO
      CALL Cerrar(URead)
      DO I = 1, DimIMaule
         MauleIPar(I) = 0
      ENDDO
      MauleIPar(IIUsoRieOpcional) = IUsoRieOp
      MauleIPar(IICompRiegoInve) = ICompRiegoInve
      MauleIPar(IIUsoConvMaule) = IUsoConvMaule
      MauleIPar(IIVerifConvenio) = IVerifConvenio
      MauleIPar(IIPLPExtEND) = IPLPExtEND
      MauleIPar(IIPLPExtENDCI) = IPLPExtENDCI
      MauleIPar(IIPLPExtRie) = IPLPExtRie
      IF (MauleIPar(IIUsoConvMaule) .EQ. 0) RETURN
      NVert = NCenEmb + NCenSer
      NAfluFict = NCenEmb
      Objeto = 'central serie clave NomCenRieCMNA, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenRieCMNA,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIRieCMNA) = NumCen
      Objeto = 'central serie clave NomCenRieCMNB, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenRieCMNB,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIRieCMNB) = NumCen
      Objeto = 'central serie clave NomCenRieCMel, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenRieCMel,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIRieCMel) = NumCen
      Objeto = 'central serie clave NomCenRieS123, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenRieS123,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIRieS123) = NumCen
      Objeto = 'central serie clave NomCenRieOReg, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenRieOReg,                  &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIRieOReg) = NumCen
      IF (MauleIPar(IIUsoRieOpcional) .NE. 0) THEN
         Objeto = 'central serie clave NomCenRieOpcional, '
         CALL NomCen2NumCen(NumCen, FStop, NomCenRieOpcional,           &
     &        CenNom, NCenHidSPP,                                       &
     &        Objeto,                                                   &
     &        ULog)
         MauleIPAR(IIRieOpcionalMaule) = NumCen
      ENDIF
      Objeto = 'embalse clave NomCenInve, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenInve,                     &
     &     CenNom, NCenEmb, Objeto,                                     &
     &     ULog)
      MauleIPAR(IICenInve) = NumCen
      MauleIPAR(IIVertInve) = NCentral + 2*NFlujo +                     &
     &     NumCen
      MauleIPAR(IIAflFictInve) = NCentral + 2*NFlujo +                  &
     &     NVert + NumCen
      MauleIPAR(IIVolInve) = NCentral + 2*NFlujo +                      &
     &     NVert + NAfluFict + NumCen
      Objeto = 'embalse clave NomCenMaule, '
      CALL NomCen2NumCen(NumCen, FStop, NomCenMaule,                    &
     &     CenNom, NCenEmb, Objeto,                                     &
     &     ULog)
      MauleIPAR(IICenMaule) = NumCen
      MauleIPAR(IIAflFictMaule) = NCentral + 2*NFlujo +                 &
     &     NVert + NumCen
      MauleIPAR(IIVolMaule) = NCentral + 2*NFlujo +                     &
     &     NVert + NAfluFict + NumCen
      Objeto = 'central serie clave NomAflIsla, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflIsla,                     &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIAflIsla) = NumCen
      IF (NomAflMina .NE. 'n/a') THEN
         Objeto = 'central serie clave NomAflMina, '
         CALL NomCen2NumCen(NumCen, FStop, NomAflMina,                  &
     &        CenNom, NCenHidSPP, Objeto,                               &
     &        ULog)
         MauleIPAR(IIAflMina) = NumCen
      ELSE
         MauleIPAR(IIAflMina) = 0
      ENDIF 
      Objeto = 'central serie clave NomAflArmerillo, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflArmerillo,                &
     &     CenNom, NCenHidSPP,                                          &
     &     Objeto,                                                      &
     &     ULog)
      MauleIPAR(IIAflArmerillo) = NumCen
      Objeto = 'central serie clave NomAflBocMaule, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflBocMaule,                 &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIAflBocMaule) = NumCen
      Objeto = 'central serie clave NomAflColbun, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflColbun,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIAflColbun) = NumCen
      Objeto = 'central serie clave NomAflInve, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflInve,                     &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIAflInve) = NumCen
      Objeto = 'central serie clave NomAflMaule, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflMaule,                    &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      MauleIPAR(IIAflMaule) = NumCen
      Objeto = 'central serie clave NomAflPehuenche, '
      CALL NomCen2NumCen(NumCen, FStop, NomAflPehuenche,                &
     &     CenNom, NCenHidSPP, Objeto, ULog)
      MauleIPAR(IIAflPehuenche) = NumCen
      Objeto = 'embalse clave NomCenFiltInve, '
      CALL NomCen2NumFil(NArcCenFil, NumFil, FStop, NomCenFiltInve,     &
     &     Objeto,                                                      &
     &     ULog)
      MauleIPAR(IIFiltInve) = NumFil
      Objeto = 'central serie clave NomGenCip, '
      CALL NomCen2NumCen(NumCen, FStop, NomGenCip,                      &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)

      ALLOCATE(MauleCPar%UppGenCip(Dim%Eta, Dim%Simul))
      ALLOCATE(MauleCPar%LowGenCip(Dim%Eta, Dim%Simul))
      ALLOCATE(MauleCPar%UppGenMaule(Dim%Eta, Dim%Simul))
      IF (CnxNomGenCip .EQ. 'V') THEN
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMax = 0.0d0
               DO IBlo = 1, NBloque(IEta)
                  GenMax = MAX(GenMax, & 
     &                 CenVMax(NumCen, BloInd(IBlo, IEta)))
               ENDDO
               MauleCPar%UppGenCip(IEta, ISimul) = GenMax
            ENDDO
         ENDDO
         
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMin = DINFTY
               DO IBlo = 1, NBloque(IEta)
                  GenMin = MIN(GenMin, &
     &                 CenVMin(NumCen, BloInd(IBlo, IEta)))
               ENDDO
               MauleCPar%LowGenCip(IEta, ISimul) = GenMin
            ENDDO
         ENDDO

         NumCen = NCentral + 2*NFlujo + NumCen
      ELSE
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMax = 0.0d0
               DO IBlo = 1, NBloque(IEta)
                  GenMax = MAX(GenMax, & 
     &                 CenPMax(NumCen, BloInd(IBlo, IEta), CenManSInd(ISimul)))
               ENDDO
               MauleCPar%UppGenCip(IEta, ISimul) = GenMax
            ENDDO
         ENDDO
         
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMin = DINFTY
               DO IBlo = 1, NBloque(IEta)
                  GenMin = MIN(GenMin, &
     &                 CenPMin(NumCen, BloInd(IBlo, IEta), CenManSInd(ISimul)))
               ENDDO
               MauleCPar%LowGenCip(IEta, ISimul) = GenMin
            ENDDO
         ENDDO
      ENDIF
      MauleIPAR(IIGenCip) = NumCen

      Objeto = 'central serie clave NomGenMaule, '
      CALL NomCen2NumCen(NumCen, FStop, NomGenMaule,                    &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomGenMaule .EQ. 'V') THEN
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMax = 0.0d0
               DO IBlo = 1, NBloque(IEta)
                  GenMax = MAX(GenMax, CenVMax(NumCen, BloInd(IBlo, IEta)))
               ENDDO
               MauleCPar%UppGenMaule(IEta, ISimul) = GenMax
            ENDDO
         ENDDO

         NumCen = NCentral + 2*NFlujo + NumCen
      ELSE
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMax = 0.0d0
               DO IBlo = 1, NBloque(IEta)
                  GenMax = MAX(GenMax, & 
     &                 CenPMax(NumCen, BloInd(IBlo, IEta), CenManSInd(ISimul)))
               ENDDO
               MauleCPar%UppGenMaule(IEta, ISimul) = GenMax
            ENDDO
         ENDDO
      ENDIF

      MauleIPAR(IIGenMaule) = NumCen
      Objeto = 'central serie clave NomExtMauEND, '
      CALL NomCen2NumCen(NumCen, FStop, NomExtMauEND,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomExtMauEND .EQ. 'V') THEN
         NumCen = NCentral + 2*NFlujo + NumCen
      ENDIF
      MauleIPAR(IIExtMauEND) = NumCen
      Objeto = 'central serie clave NomExtMauRie, '
      CALL NomCen2NumCen(NumCen, FStop, NomExtMauRie,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)

      ALLOCATE(MauleCPar%UppExtMauRie(Dim%Eta, Dim%Simul))

      IF (CnxNomExtMauRie .EQ. 'V') THEN
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMax = 0.0d0
               DO IBlo = 1, NBloque(IEta)
                  GenMax = MAX(GenMax, CenVMax(NumCen, BloInd(IBlo, IEta)))
               ENDDO
               MauleCPar%UppExtMauRie(IEta, ISimul) = GenMax
            ENDDO
         ENDDO

         NumCen = NCentral + 2*NFlujo + NumCen
      ELSE
         DO ISimul = 1, NSimul
            DO IEta = 1, NEtapa
               GenMax = 0.0d0
               DO IBlo = 1, NBloque(IEta)
                  GenMax = MAX(GenMax, & 
     &                 CenPMax(NumCen, BloInd(IBlo, IEta), CenManSInd(ISimul)))
               ENDDO
               MauleCPar%UppExtMauRie(IEta, ISimul) = GenMax
            ENDDO
         ENDDO
      ENDIF
      MauleIPAR(IIExtMauRie) = NumCen

      Objeto = 'central serie clave NomExtInvEND, '
      CALL NomCen2NumCen(NumCen, FStop, NomExtInvEND,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomExtInvEND .EQ. 'V') THEN
         NumCen = NCentral + 2*NFlujo + NumCen
      ENDIF
      MauleIPAR(IIExtInvEND) = NumCen
      Objeto = 'central serie clave NomExtInvRie, '
      CALL NomCen2NumCen(NumCen, FStop, NomExtInvRie,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomExtInvRie .EQ. 'V') THEN
         NumCen = NCentral + 2*NFlujo + NumCen
      ENDIF
      MauleIPAR(IIExtInvRie) = NumCen
      Objeto = 'central serie clave NomControl, '
      CALL NomCen2NumCen(NumCen, FStop, NomControl,                     &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomControl .EQ. 'V') THEN
         NumCen = NCentral + 2*NFlujo + NumCen
      ENDIF
      MauleIPAR(IIControl) = NumCen
      Objeto = 'central serie clave NomVertCip, '
      CALL NomCen2NumCen(NumCen, FStop, NomVertCip,                     &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomVertCip .EQ. 'V') THEN
         NumCen = NCentral + 2*NFlujo + NumCen
      ENDIF
      MauleIPAR(IIVertCip) = NumCen
      Objeto = 'central serie clave NomVertMaule, '
      CALL NomCen2NumCen(NumCen, FStop, NomVertMaule,                   &
     &     CenNom, NCenHidSPP, Objeto,                                  &
     &     ULog)
      IF (CnxNomVertMaule .EQ. 'V') THEN
         NumCen = NCentral + 2*NFlujo + NumCen
      ENDIF
      MauleIPAR(IIVertMaule) = NumCen
      Objeto = 'embalse clave NomVolColbun, '
      CALL NomCen2NumCen(NumCen, FStop, NomVolColbun,                   &
     &     CenNom, NCenEmb, Objeto,                                     &
     &     ULog)
      MauleIPAR(IICenColbun) = NumCen
      MauleIPAR(IIVertColbun) = NCentral + 2*NFlujo +                   &
     &     NumCen
      MauleIPAR(IIAflFictColbun) = NCentral + 2*NFlujo +                &
     &     NVert + NumCen
      MauleIPAR(IIVolColbun) = NCentral + 2*NFlujo +                    &
     &     NVert + NAfluFict + NumCen
      IF (FStop) THEN
         WRITE(6, '(A)') 'maule0: Debe reparar la base de datos.'
         WRITE(ULog, '(A)') 'maule0: Debe reparar la base de datos.'
         STOP 1
      ENDIF
      IF (IndSimImpMaule .GT. 0) THEN
         UWrite1 = Abrir(NArcMauleOut1, 'UNKNOWN', 'SEQUENTIAL', ULog)
         UWrite2 = Abrir(NArcMauleOut2, 'UNKNOWN', 'SEQUENTIAL', ULog)
         UWrite3 = Abrir(NArcMauleOut3, 'UNKNOWN', 'SEQUENTIAL', ULog)
         UWrite4 = Abrir(NArcMauleOut4, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite1, '(A, $)')                                       &
     &        'IEtapa, Mes(IEtapa), '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolInv, VolGenCip, VolFiltInv, VolVertInv, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolAflInv, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolMau, VolExtMau, VolVertMau, VolAflMau, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolEcoInv, VolCompEND, VolResMauEND, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolExtMauEND , VolResMauRie, VolExtMauRie, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolDisResOrdEND, VolDisResOrdRie, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolDefRie, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolExtMaxMauEND, VolExtMaxMauRie, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'VolResCuoExt, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'FVieneDePorSup, FVieneDeResOrd, '
         WRITE(UWrite1, '(A, $)')                                       &
     &        'FPasoPorResOrd, FPrevisionDeshielo'
         WRITE(UWrite1, *)
         WRITE(UWrite2, '(A, $)')                                       &
     &        'IEtapa, Mes(IEtapa), '
         WRITE(UWrite2, '(A, $)')                                       &
     &        'VolInv, VolGenCip, VolFiltInv, VolVertInv, '
         WRITE(UWrite2, '(A, $)')                                       &
     &        'VolAflInv, '
         WRITE(UWrite2, '(A, $)')                                       &
     &        'VolMau, VolExtMau, VolVertMau, VolAflMau, '
         WRITE(UWrite2, '(A, $)')                                       &
     &        'CauGenCip, CauVertInv, '
         WRITE(UWrite2, '(A, $)')                                       &
     &        'CauExtMau, CauVertMau'
         WRITE(UWrite2, *)
         WRITE(UWrite3, '(A, $)')                                       &
     &        'IEtapa, Mes(IEtapa), Cau2Vol(IEtapa), '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolAflHiis, VolAflHipe, VolAflMela, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolAflClap, VolAflHima, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolAflInv, VolAflMau, VolFiltInv, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolAflArme, VolConMau, VolRes105,  '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolDefRie, VolAflArmeDfnv, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolRiego, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolExtMaxMaule, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolExtMaxMauEND, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolExtMaxMauRie, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolExtMinInve, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'VolExtRie, '
         WRITE(UWrite3, '(A, $)')                                       &
     &        'CauGenMaxCip'
         WRITE(UWrite3, *)
         WRITE(UWrite4, '(A, $)')                                       &
     &        'IEtapa, Mes, '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'MauleIPar(IIUsoConvMaule), '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'MauleIPar(IIUsoRieOpcional), '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'MauleIPar(IIPLPExtEND), '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'MauleIPar(IIPLPExtENDCI), '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'MauleIPar(IIPLPExtRie), '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'Cau2Vol(IEtapa),  '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'VolAflHima/1D3, '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'IRieCMNB, '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'IRieCMNA, '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'IRieCMel, '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'CauRes105,  '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'CauAflArmeDfnv,  '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'CauConMau,  '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'CauConMauEta(IEtapa), '
         WRITE(UWrite4, '(A, $)')                                       &
     &        'PCenCMNB, PCenCMNA, '
         WRITE(UWrite4, *)
      ELSE
         UWrite1 = 0
         UWrite2 = 0
         UWrite3 = 0
         UWrite4 = 0
      ENDIF
      DO IYear = NYear + 1, Dim%Year
         DO IMes = 1, 12
            CauConMau(IMes, IYear) = CauConMauDefecto(IMes)
            CauRes105(IMes, IYear) = CauRes105Defecto(IMes)
         ENDDO
      ENDDO
      MauleIPar(IIndSimImpMaule) = IndSimImpMaule
      VolInv = EmbVIni(MauleIPar(IICenInve))
      VolMau = EmbVIni(MauleIPar(IICenMaule))
      DO ISimul = 1, NSimul
!
!     Insercion de datos de MauleLPar.
         MauleLPar(IFPasoPorResOrd, ISimul, 0) = FPasoPorResOrd
         MauleLPar(IFVieneDePorSup, ISimul, 0) = FVieneDePorSup
         MauleLPar(IFVieneDeResOrd, ISimul, 0) = FVieneDeResOrd
!
!     Insercion de datos de MauleRPar.
         MauleRPar(IVolCompEND, ISimul, 0) = VolCompEND
         MauleRPar(IVolDisResOrdEND, ISimul, 0) = VolDisResOrdEND
         MauleRPar(IVolDisResOrdRie, ISimul, 0) = VolDisResOrdRie
         MauleRPar(IVolEcoInv, ISimul, 0) = VolEcoInv
         MauleRPar(IVolInv, ISimul, 0) = VolInv
         MauleRPar(IVolMau, ISimul, 0) = VolMau
         MauleRPar(IVolResCuoExt, ISimul, 0) = VolResCuoExt
         MauleRPar(IVolResMauEND, ISimul, 0) = VolResMauEND
         MauleRPar(IVolResMauRie, ISimul, 0) = VolResMauRie
!
      ENDDO
      MauleIPar(IIMauleWrite1) = UWrite1
      MauleIPar(IIMauleWrite2) = UWrite2
      MauleIPar(IIMauleWrite3) = UWrite3
      MauleIPar(IIMauleWrite4) = UWrite4
      DO IEtapa = 1, NEtapa
         IYear = Year(IEtapa)
         NMes =                                                         &
     &     MAX(1, NINT(EtaDur(IEtapa)*FactTiempo/(3.6d0*24d0*31d0))) - 1
         CauRes105Prom = 0d0
         CauConMauProm = 0d0
         DO IMes0 = 0, NMes
            IMes = MOD(Mes(IEtapa) + IMes0 - 1, 12) + 1
            CauConMauProm = CauConMauProm + CauConMau(IMes, IYear)
            CauRes105Prom = CauRes105Prom + CauRes105(IMes, IYear)
         ENDDO
         CauRes105Eta(IEtapa) = CauRes105Prom/DBLE(NMes + 1)
         CauConMauEta(IEtapa) = CauConMauProm/DBLE(NMes + 1)
      ENDDO
      DO IEtapa = 1, NEtapa
!     Cubos a millones de metros cubicos.
         Cau2Vol(IEtapa) = FactTiempo*EtaDur(IEtapa)
      ENDDO
      DO Ind1 = 1, NCentral
         CenGHid(Ind1, 2) = 0
         CenVHid(Ind1, 2) = 0
      ENDDO
      NControl = MauleIPAR(IIControl)
      IF (NControl .EQ. 0) THEN
         WRITE(6, '(3A)') 'leecnfce: Error, controlador ',              &
     &        NameControl, ' no existe.'
         WRITE(ULog, '(3A)') 'leecnfce: Error, controlador ',           &
     &        NameControl, ' no existe.'
         FWarning = .TRUE.
      ENDIF
      DO I = 1, NumBajoControl
         Connection = CnxNomBajoControl(I)
         FConnection = .FALSE.
         IF (                                                           &
     &        (Connection .EQ. 'G') .OR.                                &
     &        (Connection .EQ. 'g') .OR.                                &
     &        (Connection .EQ. 'V') .OR.                                &
     &        (Connection .EQ. 'v')                                     &
     &        ) THEN
            FConnection = .TRUE.
         ELSE
            WRITE(6, '(2A)') 'leecnfce: Error, indicador de ',          &
     &           'connexion debe ser ''G'' o ''V'''
            WRITE(ULog, '(2A)') 'leecnfce: Error, indicador de ',       &
     &           'connexion debe ser ''G'' o ''V'''
            FWarning = .TRUE.
         ENDIF
         NameHydUnit = NomBajoControl(I)
         NHydUnit = 0
         IHydUnit = 1
         DO WHILE ((NHydUnit .EQ. 0) .AND. (IHydUnit .LE. NCenHid))
            IF (CenNom(IHydUnit) .EQ. NameHydUnit) THEN
               NHydUnit = IHydUnit
            ENDIF
            IHydUnit = IHydUnit + 1
         ENDDO
         IF (NHydUnit .EQ. 0) THEN
            WRITE(6, '(3A)') 'leecnfce: Error, unidad hidro ',          &
     &           NameHydUnit, ' no existe.'
            WRITE(ULog, '(3A)') 'leecnfce: Error, unidad hidro ',       &
     &           NameHydUnit, ' no existe.'
            FWarning = .TRUE.
         ENDIF
         IF (                                                           &
     &        (NControl .GT. 0) .AND.                                   &
     &        (FConnection) .AND.                                       &
     &        (NHydUnit .GT. 0)                                         &
     &        ) THEN
            IF (                                                        &
     &           (Connection .EQ. 'G') .OR.                             &
     &           (Connection .EQ. 'g')                                  &
     &           ) THEN
               CenGHid(NHydUnit, 2) = CenInd(NControl)
            ELSEIF (                                                    &
     &              (Connection .EQ. 'V') .OR.                          &
     &              (Connection .EQ. 'v')                               &
     &              ) THEN
               CenVHid(NHydUnit, 2) = CenInd(NControl)
            ENDIF
         ENDIF
      ENDDO
      DO Ind1 = 1, NCentral
         CenGAux(Ind1, 2) = 0
         CenVAux(Ind1, 2) = 0
         Ind2 = 0
         DO WHILE ((Ind2 .LT. NCentral) .AND.                           &
     &        (CenGAux(Ind1, 2) .EQ. 0))
            Ind2 = Ind2 + 1
            IF (CenInd(Ind2) .EQ. CenGHid(Ind1, 2)) THEN
               CenGAux(Ind1, 2) = Ind2
            ENDIF
         ENDDO
         Ind2 = 0
         DO WHILE ((Ind2 .LT. NCentral) .AND.                           &
     &        (CenVAux(Ind1, 2) .EQ. 0))
            Ind2 = Ind2 + 1
            IF (CenInd(Ind2) .EQ. CenVHid(Ind1, 2)) THEN
               CenVAux(Ind1, 2) = Ind2
            ENDIF
         ENDDO
      ENDDO
      DO Ind1 = 1, NCentral
         CenGHid(Ind1, 2) = CenGAux(Ind1, 2)
         CenVHid(Ind1, 2) = CenVAux(Ind1, 2)
      END DO


      ALLOCATE(MauleCPar%IBloInd(MauleBloQVarBeg:MauleBloVVarEnd, Dim%IBlo, Dim%Eta))
      ALLOCATE(MauleCPar%ExtPar(DimEMaule, Dim%Clase, 0:Dim%Eta))




      

      RETURN
      END
