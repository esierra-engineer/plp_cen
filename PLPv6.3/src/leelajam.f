!**********************************


      SUBROUTINE LeeLajaM(NArcLajaM,                                    &
     &     NCenEmb, NCenSer,                                            &  
     &     CenNom, NSimul, NEtapa, Mes,                                 &
     &     FiltNCen, FiltEmbInd,                                        &
     &     ScaleVol, ParLajam, ULog, Dim)

      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      INCLUDE 'machcons.fpp'

      CHARACTER*(*) NArcLajaM
      CHARACTER*48 CenNom(Dim%Cen)
      TYPE(PAR_LAJAM) ParLajam
      INTEGER NSimul
      INTEGER NEtapa
      INTEGER Mes(Dim%Eta)
      INTEGER ULog
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)

      DOUBLE PRECISION ScaleVol(Dim%Emb)

!     variables locales
      CHARACTER*12 AuxVar
      CHARACTER*48 NomCentral
      CHARACTER*48 NomInyecc
      CHARACTER*42 Objeto

      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning

      INTEGER NumCen
      INTEGER NumIny
      INTEGER IFilt
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER Idx
      INTEGER I
      INTEGER ICen
      
      EXTERNAL Abrir
      INTEGER Abrir

      INTEGER NCenHidSPP
      DOUBLE PRECISION FRiegoCost, FRiegoPrim, FRiegoNuev, FRiegoEmer, FRiegoSalt

      CHARACTER*(DimLargo) CVolB
      INTEGER ICol
      INTEGER IFil
      INTEGER IEta


      FStop = .FALSE.

      ALLOCATE(ParLajaM%VarBloNames(ParLajaM%DimColBlo))
      ALLOCATE(ParLajaM%VarEtaNames(ParLajaM%DimColEta))

      ParLajaM%VarBloNames(ParLajaM%IQDR) = 'qdr'
      ParLajaM%VarBloNames(ParLajaM%IQDE) = 'qde'
      ParLajaM%VarBloNames(ParLajaM%IQDM) = 'qdm'
      ParLajaM%VarBloNames(ParLajaM%IQGA) = 'qga'
      
      ParLajaM%VarEtaNames(ParLajaM%IVDRF)   = 'vdrf'
      ParLajaM%VarEtaNames(ParLajaM%IVDEF)   = 'vdef'
      ParLajaM%VarEtaNames(ParLajaM%IVDMF)   = 'vdmf'
      ParLajaM%VarEtaNames(ParLajaM%IVGAF)   = 'vgaf'
      ParLajaM%VarEtaNames(ParLajaM%IQDRH)   = 'qdrh'
      ParLajaM%VarEtaNames(ParLajaM%IQDEH)   = 'qdeh'
      ParLajaM%VarEtaNames(ParLajaM%IQDMH)   = 'qdmh'
      ParLajaM%VarEtaNames(ParLajaM%IQGAH)   = 'qgah'
      ParLajaM%VarEtaNames(ParLajaM%IQDEFM)  = 'qdefm'
      ParLajaM%VarEtaNames(ParLajaM%IQGTH)   = 'qgth'
      ParLajaM%VarEtaNames(ParLajaM%IQRS)    = 'qrs'
      ParLajaM%VarEtaNames(ParLajaM%IQHI)    = 'qhi'
      ParLajaM%VarEtaNames(ParLajaM%IQPR)    = 'qpr'
      ParLajaM%VarEtaNames(ParLajaM%IQNR)    = 'qnr'
      ParLajaM%VarEtaNames(ParLajaM%IQER)    = 'qer'
      ParLajaM%VarEtaNames(ParLajaM%IQSR)    = 'qsr'
      ParLajaM%VarEtaNames(ParLajaM%IQLAJA)  = 'qlaja'
      
      
      ALLOCATE(ParLajaM%ColIndEta(ParLajaM%DimColEta, Dim%Eta))
      ALLOCATE(ParLajaM%FilIndEta(ParLajaM%DimFilaEta, Dim%Eta))
      
      ! Apertura de archivo
      URead = Abrir(NArcLajaM, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leelajam: Error, no existe archivo ',           &
     &        NArcLajaM, '.'
         WRITE(ULog, '(3A)') 'leelajam: Error, no existe archivo ',        &
     &        NArcLajaM, '.'
         STOP 1
      ENDIF


      ! Comentario
      READ(URead, '(A1)') AuxVar
      ! Central El Toro
      Objeto ='embalse Laja'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,  &
     &     CenNom, NCenEmb, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF

      ParLajaM%IEmbLaja = NumCen
      ParLajaM%ScaleVol = ScaleVol(NumCen)

      ! buscamos indice de variable de filtracion
      ParLajaM%IFiltLaja = 0
      DO IFilt = 1, FiltNCen
         IF (FiltEmbInd(IFilt) .EQ. ParLajaM%IEmbLaja) THEN
            ParLajaM%IFiltLaja = IFilt
         ENDIF
      ENDDO

      NCenHidSPP = NCenEmb + NCenSer

      ! Definicion hoya intemedia
      Objeto ='afluente hoya intermedia Laja'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%NumAflHoyaInt      
      READ(URead, '(A1)') AuxVar

      ALLOCATE(ParLajaM%IAflHoyaInt(ParLajaM%NumAflHoyaInt))

      ICen = 0
      DO Idx=1, ParLajaM%NumAflHoyaInt
         READ(URead, *) NomCentral
         CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &        CenNom, NCenHidSPP, Objeto, ULog)
         IF (NumCen .GT. 0) THEN
            ICen = ICen + 1
            ParLajaM%IAflHoyaInt(ICen) = NumCen
         ENDIF
         IF (FWarning) THEN
            STOP 1
         ENDIF
      ENDDO
      ParLajaM%NumAflHoyaInt = ICen

      ! Volumen Maximo Laja
      READ(URead, '(A1)') AuxVar
      READ(URead, *)  ParLajaM%VolMaxLaja

      ! Definicion de colchones
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%NumColchon

      ALLOCATE(ParLajaM%VolColchon(ParLajaM%NumColchon))
      ALLOCATE(ParLajaM%FactDerRiegoColchon(ParLajaM%NumColchon))
      ALLOCATE(ParLajaM%FactDerElectColchon(ParLajaM%NumColchon))
      ALLOCATE(ParLajaM%FactDerMixtoColchon(ParLajaM%NumColchon))

      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%VolColchon(I), I=1, ParLajaM%NumColchon)

      DO I=1, ParLajaM%NumColchon
         ParLajaM%VolColchon(I) = ParLajaM%VolColchon(I)*1d3
      ENDDO

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%DerRiegoBase, &
     &     (ParLajaM%FactDerRiegoColchon(I), I=1, ParLajaM%NumColchon)
      ParLajaM%DerRiegoBase = ParLajaM%DerRiegoBase * 1d3

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%DerElectBase, &
     &     (ParLajaM%FactDerElectColchon(I), I=1, ParLajaM%NumColchon)
      ParLajaM%DerElectBase = ParLajaM%DerElectBase * 1d3

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%DerMixtoBase, &
     &     (ParLajaM%FactDerMixtoColchon(I), I=1, ParLajaM%NumColchon)
      ParLajaM%DerMixtoBase = ParLajaM%DerMixtoBase * 1d3

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%DerRiegoMax, ParLajaM%DerElectMax, &
     &     ParLajaM%DerMixtoMax, ParLajaM%GasAnticMax
      ParLajaM%DerRiegoMax = 1d3*ParLajaM%DerRiegoMax
      ParLajaM%DerElectMax = 1d3*ParLajaM%DerElectMax
      ParLajaM%DerMixtoMax = 1d3*ParLajaM%DerMixtoMax
      ParLajaM%GasAnticMax = 1d3*ParLajaM%GasAnticMax
      
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%MesIniTempRiego, ParLajaM%MesIniTempAntic

!     Parametros mensuales
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%CaudalMaxRiego, &
     &     ParLajaM%CaudalMaxElect, ParLajaM%CaudalMaxMixto, ParLajaM%CaudalMaxAntic

!     Costo riego no servido base, derecos de riego, electricos, mixto y gasto anticipado
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%CRiegoNS,   &
     &     ParLajaM%CQVar(ParLajaM%IQDR), &      
     &     ParLajaM%CQVar(ParLajaM%IQDE), &
     &     ParLajaM%CQVar(ParLajaM%IQDM), &
     &     ParLajaM%CQVar(ParLajaM%IQGA)

      ! factores mensuales de costos para costo riego no servido base, derecos de riego, electricos, mixto y gasto anticipado
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenCRiegoNS(I), I=1, 12)

      DO Idx = 1, ParLajaM%IQGA
         READ(URead, '(A1)') AuxVar
         READ(URead, *) (ParLajaM%FactMenCQVar(Idx, I), I=1, 12)
      ENDDO
      
      ALLOCATE(ParLajaM%CRiegoNSEta(NEtapa))
      ALLOCATE(ParLajaM%CQVarEta(ParLajaM%IQGA, NEtapa))
      
      DO IEta = 1, NEtapa
         ParLajaM%CRiegoNSEta(IEta) = ParLajaM%CRiegoNS &
     &        * ParLajaM%FactMenCRiegoNS(Mes(IEta))

         DO Idx = 1, ParLajaM%IQGA
            ParLajaM%CQVarEta(Idx, IEta) = &
     &           ParLajaM%CQVar(Idx) * ParLajaM%FactMenCQVar(Idx, Mes(IEta))
         ENDDO
      ENDDO
      
      
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenMaxRiego(I), I=1, 12)
      
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenMaxElect(I), I=1, 12)

      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenMaxMixto(I), I=1, 12)

      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenMaxAntic(I), I=1, 12)
      
!     Valores iniciales

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%DerRiegoIni, ParLajaM%DerElectIni, &
     &     ParLajaM%DerMixtoIni, ParLajaM%GasAnticIni

      ParLajaM%DerRiegoIni = 1d3 * ParLajaM%DerRiegoIni 
      ParLajaM%DerElectIni = 1d3 * ParLajaM%DerElectIni 
      ParLajaM%DerMixtoIni = 1d3 * ParLajaM%DerMixtoIni
      ParLajaM%GasAnticIni = 1d3 * ParLajaM%GasAnticIni

      ! Retiros de riego
      Objeto ='retiro de riego Laja'
      ParLajam%NumRetRiego = 0
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%NumRetRiego


      ALLOCATE(ParLajaM%ICenRetRiego(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%ICenInyRiego(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%FRiegoCost(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%FRiegoPrim(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%FRiegoNuev(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%FRiegoEmer(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%FRiegoSalt(ParLajaM%NumRetRiego))


      ICen = 0
      DO Idx=1, ParLajaM%NumRetRiego
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCentral
         CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &        CenNom, NCenHidSPP, Objeto, ULog)
         IF (FWarning) THEN
            STOP 1
         ENDIF
         
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomInyecc
         CALL NomCen2NumCen(NumIny, FWarning, NomInyecc,                &
     &        CenNom, NCenHidSPP, Objeto, 0)
         
         READ(URead, '(A1)') AuxVar         
         READ(URead, *) FRiegoCost, FRiegoPrim, FRiegoNuev, FRiegoEmer, FRiegoSalt

         ICen = ICen + 1
         ParLajaM%ICenRetRiego(ICen) = NumCen
         ParLajaM%ICenInyRiego(ICen) = NumIny
         ParLajaM%FRiegoCost(ICen) = FRiegoCost
         ParLajaM%FRiegoPrim(ICen) = FRiegoPrim
         ParLajaM%FRiegoNuev(ICen) = FRiegoNuev
         ParLajaM%FRiegoEmer(ICen) = FRiegoEmer
         ParLajaM%FRiegoSalt(ICen) = FRiegoSalt
      ENDDO
      ParLajaM%NumRetRiego = ICen

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%QFiltHist
      
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%CaudalDefPrim, ParLajaM%CaudalDefNuev, &
     &     ParLajaM%CaudalDefEmer, ParLajaM%CaudalDefSalt
      
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenRiegoPrim(I), I=1, 12)

      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenRiegoNuev(I), I=1, 12)

      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenRiegoEmer(I), I=1, 12)

      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLajaM%FactMenRiegoSalt(I), I=1, 12)

      
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%VolMuerto
      ParLajaM%VolMuerto = 1d3 * ParLajaM%VolMuerto
      
      ! Retiros manuales
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%NumRetiros

      ALLOCATE(ParLajaM%EtaRetiro(ParLajaM%NumRetiros))
      ALLOCATE(ParLajaM%RetPrimReg(ParLajaM%NumRetiros))
      ALLOCATE(ParLajaM%RetNuevReg(ParLajaM%NumRetiros))
      ALLOCATE(ParLajaM%RetEmerReg(ParLajaM%NumRetiros))
      ALLOCATE(ParLajaM%RetSaltReg(ParLajaM%NumRetiros))
      
      READ(URead, '(A1)') AuxVar      
      DO Idx=1, ParLajaM%NumRetiros
         READ(URead, *) &
     &        ParLajaM%EtaRetiro(Idx), &
     &        ParLajaM%RetPrimReg(Idx), & 
     &        ParLajaM%RetNuevReg(Idx),  &
     &        ParLajaM%RetEmerReg(Idx), & 
     &        ParLajaM%RetSaltReg(Idx)
      ENDDO
         

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLajaM%NumCaudalToro

      ALLOCATE(ParLajaM%EtaCaudalToro(ParLajaM%NumCaudalToro))
      ALLOCATE(ParLajaM%CaudalToro(ParLajaM%NumCaudalToro))
      
      READ(URead, '(A1)') AuxVar      
      DO Idx=1, ParLajaM%NumCaudalToro
         READ(URead, *) &
     &        ParLajaM%EtaCaudalToro(Idx), &
     &        ParLajaM%CaudalToro(Idx)
      ENDDO

!     finaliza las allocaciones
      
      ALLOCATE(ParLajaM%IQRI(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%IQRIHC(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%IQRDHC(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%IQRFHC(ParLajaM%NumRetRiego))

      ALLOCATE(ParLajaM%IQRIHF(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%IQRFHF(ParLajaM%NumRetRiego))
      ALLOCATE(ParLajaM%IQRDHF(ParLajaM%NumRetRiego))
      

      ALLOCATE(ParLajaM%DRExtra(Dim%Eta, ParLajaM%NumRetRiego))

      ParLajaM%DRExtra(1:Dim%Eta, 1:ParLajaM%NumRetRiego) = 0.0d0
      
      ICol = ParLajaM%NumColBlo
      DO Idx=1, ParLajaM%NumRetRiego
         CVolB = ' '
         CALL Num2Char(Idx, CVolB, No, DimLargo)

         ICol = ICol + 1
         ParLajaM%IQRI(Idx) = ICol
         ParLajaM%VarBloNames(ICol) = 'qri' // CVolB

         ParLajaM%NumColBlo = ICol
      ENDDO


      ICol = ParLajaM%NumColEta
      IFil = ParLajaM%NumFilEta
      DO Idx=1, ParLajaM%NumRetRiego
         CVolB = ' '
         CALL Num2Char(Idx, CVolB, No, DimLargo)

         ICol = ICol + 1
         ParLajaM%IQRIHC(Idx) = ICol
         ParLajaM%VarEtaNames(ICol) = 'qrih' // CVolB

         ICol = ICol + 1
         ParLajaM%IQRDHC(Idx) = ICol
         ParLajaM%VarEtaNames(ICol) = 'qrdh' // CVolB

         ICol = ICol + 1
         ParLajaM%IQRFHC(Idx) = ICol
         ParLajaM%VarEtaNames(ICol) = 'qrhr' // CVolB
         
         IFil = IFil + 1
         ParLajaM%IQRIHF(Idx) = IFil
         IFil = IFil + 1
         ParLajaM%IQRFHF(Idx) = IFil
         IFil = IFil + 1
         ParLajaM%IQRDHF(Idx) = IFil

         ParLajaM%NumColEta = ICol
         ParLajaM%NumFilEta = IFil
      ENDDO

      ALLOCATE(ParLajaM%TipoEtaGM(NEtapa))

      ParLajaM%TipoEtaGM(1:NEtapa) = 0
      DO IEta = 2, NEtapa
!        Suponemos que los datos de Meses y Agnos refieren a
!        calendario hidrologico, es decir, el mes uno corresponde a
!        abril y el agno calendario comienza en el mes 10.
!        
!        Tipo de Etapa para gasto medio
!        INICIOTEMP:  Etapa coincide con el inicio de la temporada de riego
!        INICIOANTIC:  Etapa coincide con el inicio de la temmporada de anticipos

         IF (Mes(IEta) .EQ. ParLajaM%MesIniTempRiego) THEN
            IF (Mes(IEta-1) .NE. Mes(IEta)) THEN
               ParLajaM%TipoEtaGM(IEta) = INICIOTEMP
            ENDIF
         ENDIF
         IF (Mes(IEta) .EQ. ParLajaM%MesIniTempAntic) THEN
            IF (Mes(IEta-1) .NE. Mes(IEta)) THEN
               ParLajaM%TipoEtaGM(IEta) = INICIOANTIC
            ENDIF
         ENDIF
      ENDDO
      
      ALLOCATE(ParLajaM%ColchonActivo(NSimul, NEtapa))
      ALLOCATE(ParLajaM%VarEtaPrev(ParLajaM%DimColEta, Dim%Simul, Dim%Eta))

      ! inicializaciones varias
      ALLOCATE(ParLajaM%VarEtaVol(ParLajaM%NumColEta))
      DO Idx = 1, ParLajaM%NumColEta
         ParLajaM%VarEtaVol(Idx) = (Idx .LE. ParLajaM%NumVarVol) 
      ENDDO

      ALLOCATE(ParLajaM%DataEta(ParLajaM%DimColEta, Dim%Eta))
      ALLOCATE(ParLajaM%DataBlo(ParLajaM%DimColBlo, Dim%Blo))
      
      RETURN 
      END

