      SUBROUTINE ExtrOper(NCols, NFilas, PDNCol, PDNFila,               &
     &     EtaPrimal, EtaDual,                                          &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParRes,                                        &
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
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!     parametros

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
      TYPE(PAR_RESERVA) ParRes
      LOGICAL FBaterias
      TYPE(PAR_BATERIAS) ParBaterias

      INTEGER NEmbVMinH
      INTEGER COffset
      INTEGER FOffset
      INTEGER IBar
      INTEGER ICen
      INTEGER IColN
      INTEGER IColP
      INTEGER IEmb
      INTEGER IEta
      INTEGER IFlu
      INTEGER IHid
      INTEGER ILin
      INTEGER IPas
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NAfluFict
      INTEGER NAng
      INTEGER NBarra
      INTEGER NEtapa
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION FactTiempo
      LOGICAL FAfluFict
      LOGICAL FScaleQs
      LOGICAL FFiltVar
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb
      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)

      LOGICAL FExtrac
      INTEGER ExtrNCen


      INTEGER IReb
      INTEGER NCenEmb
      INTEGER NCenPas
      INTEGER NCenSer
      INTEGER NCentral
      INTEGER NVol
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER NVert
      INTEGER IBInd
      INTEGER IBlo
      INTEGER NCols, NFilas
      INTEGER PDNFila(Dim%Eta)
      INTEGER PDNCol(Dim%Eta)
      DOUBLE PRECISION EtaDual(NFilas)
      DOUBLE PRECISION EtaPrimal(NCols)
      DOUBLE PRECISION PasAfl(Dim%Pas, Dim%Blo)

      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CMg(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION LinPTra(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION PasDat(Dim%Pas, DimDatPas, Dim%Blo)
      DOUBLE PRECISION SerDat(Dim%Ser, DimDatSer, Dim%Blo)
      DOUBLE PRECISION ExtrDat(Dim%Extr, Dim%Blo)

      DOUBLE PRECISION FLinN
      DOUBLE PRECISION FLinP

      DOUBLE PRECISION invbdur
      DOUBLE PRECISION scaleqs

      INTEGER CBloque, FBloque

      INTEGER IFiltCen, Idx
      INTEGER IExtrCen


      INTEGER IEtaCOff
      INTEGER IEtaFOff

      INTEGER I

      NVert = NCenEmb + NCenSer

      IF (FAfluFict) THEN
         NAfluFict = NCenEmb
      ELSE
         NAfluFict = 0
      ENDIF
      NVol = NCenEmb
      NAng = NBarra
      CBloque = NCentral + 2*NFlujo + NVert + NAfluFict + NVol + NAng
      FBloque = NBarra + NCenEmb + NCenSer + NLinea
      IF (FExtrac) THEN
!        agregamos las extracciones como variable del bloque
         CBloque = CBloque + ExtrNCen
      ENDIF
      IF (FConvLaja .EQ. 1) THEN
         CBloque = CBloque + ParLaja%NumColBlo
         FBloque = FBloque + ParLaja%NumFilBlo
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         CBloque = CBloque + ParLajaM%NumColBlo
         FBloque = FBloque + ParLajaM%NumFilBlo
      ENDIF
      IF (FConvMaule .NE. 0) THEN
         CBloque = CBloque + ParMaule%NumColBlo
         FBloque = FBloque + ParMaule%NumFilBlo
      ENDIF
      IF (FRestRalco) THEN
         CBloque = CBloque + ParRalco%NumColBlo
         FBloque = FBloque + ParRalco%NumFilBlo
      ENDIF
      IF (FRestGnl) THEN
         Do I = 1, ParGnl%NumTGNL
            CBloque = CBloque + ParGnl%TGNL(I)%NumColBlo
            FBloque = FBloque + ParGnl%TGNL(I)%NumFilBlo
         ENDDO
      ENDIF
      IF (FRestReserva) THEN
         CBloque = CBloque + ParRes%NumColBlo
         FBloque = FBloque + ParRes%NumFilBlo
      ENDIF
      
      IF (FBaterias) THEN
         CBloque = CBloque + ParBaterias%NumColBlo
         FBloque = FBloque + ParBaterias%NumFilBlo
      ENDIF
      IEtaCOff = 0
      IEtaFOff = 0
      DO IEta = 1, NEtapa
         DO IBlo = 1, NBloque(IEta)
            COffset = CBloque*(IBlo - 1)
            IBInd = BloInd(IBlo, IEta)

!           traspasa generacion
!
            DO ICen = 1, NCentral
               CenPGen(ICen, IBInd) = EtaPrimal(COffset + ICen + IEtaCOff)
            ENDDO
            COffset = COffset + NCentral

!           Traspasa Tranferencia Lineas
!
            IColP = 0
            DO ILin = 1, NLinea
               DO IFlu = 1, LinNFlu(ILin)
                  IColP = IColP + 1
                  IColN = IColP + LinNFlu(ILin)
                  FLinP = EtaPrimal(COffset + IColP + IEtaCOff)
                  FLinN = EtaPrimal(COffset + IColN + IEtaCOff)
                  IF (FLinP .GT. FLinN) THEN
                     LinPTra(1, IFlu, ILin, IBInd) = FLinP - FLinN
                     LinPTra(2, IFlu, ILin, IBInd) = 0.0d0
                  ELSE
                     LinPTra(1, IFlu, ILin, IBInd) = 0.0d0
                     LinPTra(2, IFlu, ILin, IBInd) = FLinN - FLinP
                  ENDIF
               ENDDO
               IColP = IColP + LinNFlu(ILin)
            ENDDO
            COffset = COffset + 2*NFlujo

!           Traspasa Vertimiento
!
            DO ICen = 1, NCenEmb
               EmbDat(ICen, PEmbDatVer, IBInd) =                        &
     &              EtaPrimal(COffset + ICen + IEtaCOff)
            ENDDO

            DO IPas = 1, NCenSer
               SerDat(IPas, PSerDatVer, IBInd) =                        &
     &              EtaPrimal(COffset + NCenEmb + IPas + IEtaCOff)
            ENDDO

            DO IPas = 1, NCenPas
               PasDat(IPas, PPasDatVer, IBInd) =                        &
     &              PasAfl(IPas, IBInd) -                               &
     &              CenPGen(NCenEmb + NCenSer + IPas, IBInd)
            ENDDO
            COffset = COffset + NVert

!           Traspasa Alfuente Ficticio
!
            IF (FAfluFict) THEN
               DO ICen = 1, NAfluFict
                  EmbDat(ICen, PEmbDatDef, IBInd) =                     &
     &                 EtaPrimal(COffset + ICen + IEtaCOff)
               ENDDO
               COffset = COffset + NAfluFict
            ELSE
               DO ICen = 1, NCenEmb
                  EmbDat(ICen, PEmbDatDef, IBInd) = 0.0d0
               ENDDO
            ENDIF

!           Saltamos caudal embalses y angulo barras
!
            COffset = COffset + NVol
            COffset = COffset + NAng

!           Traspasa las extracciones
            IF (FExtrac) THEN
               DO IExtrCen = 1, ExtrNCen
                  ExtrDat(IExtrCen, IBInd) =                             &
     &                 EtaPrimal(COffset + IExtrCen + IEtaCOff)
               ENDDO

               COffset = COffset + ExtrNCen
            ENDIF

!           valores de bloque de Laja
            IF (FConvLaja .EQ. 1) THEN
               DO Idx = 1, ParLaja%NumColBlo
                  ParLaja%DataBlo(Idx, IBInd) =                         &
     &                 EtaPrimal(COffset + Idx + IEtaCOff)
               ENDDO
               COffset = COffset + ParLaja%NumColBlo
            ENDIF
            IF (FConvLaja .EQ. 2) THEN
               DO Idx = 1, ParLajaM%NumColBlo
                  ParLajaM%DataBlo(Idx, IBInd) =                        &
     &                 EtaPrimal(COffset + Idx + IEtaCOff)
               ENDDO
               COffset = COffset + ParLajaM%NumColBlo
            ENDIF
!           valores de bloque de Maule
            IF (FConvMaule .NE. 0) THEN
               DO Idx = 1, ParMaule%NumColBlo
                  ParMaule%DataBlo(Idx, IBInd) =                         &
     &                 EtaPrimal(COffset + Idx + IEtaCOff)
               ENDDO
               COffset = COffset + ParMaule%NumColBlo
            ENDIF
            IF (FRestGnl) THEN
               Do I = 1, ParGnl%NumTGNL
                  COffset = COffset + ParGnl%TGNL(I)%NumColBlo
               ENDDO
            ENDIF
!           valores de reservas
            IF (FRestReserva) THEN
               DO Idx = 1, ParRes%NumColBlo
                  ParRes%DataBlo(Idx, IBInd) =                         &
     &                 EtaPrimal(COffset + Idx + IEtaCOff)
               ENDDO
               COffset = COffset + ParRes%NumColBlo
            ENDIF

            IF (FBaterias) THEN
               DO Idx=1, ParBaterias%NumColBlo
                  ParBaterias%DataBlo(Idx,IBInd)=                       &
     &                  EtaPrimal(COffset + Idx + IEtaCOff)
               ENDDO
               COffset = COffset + ParBaterias%NumColBlo
            ENDIF
         ENDDO
         COffset = CBloque*NBloque(IEta)

!        Traspasa datos de etapa
!
         DO IEmb = 1, NCenEmb
            EmbDat(IEmb, PEmbDatVol, IEta) =                            &
     &           EtaPrimal(COffset + IEmb + IEtaCOff)*ScaleVol(IEmb)
         ENDDO
         COffset = COffset + 2*NCenEmb

!        Traspasa Vols de Rebalses
!

         DO IEmb = 1, NCenEmb
            EmbDat(IEmb, PEmbDatReb, IEta) = 0.0d0
            EmbDat(IEmb, PEmbDatRebP, IEta) = 0.0d0
            EmbDat(IEmb, PEmbDatRebN, IEta) = 0.0d0
         ENDDO
         IF (FVertReb) THEN
            DO IReb = 1, NEmbVReb
               IEmb = EmbVRebInd(IReb)
               EmbDat(IEmb, PEmbDatReb, IEta) =                         &
     &              EtaPrimal(COffset + IReb + IEtaCOff)
            ENDDO
            COffset = COffset + NEmbVReb

            DO IReb = 1, NEmbVReb
               IEmb = EmbVRebInd(IReb)
               EmbDat(IEmb, PEmbDatRebP, IEta) = ScaleVol(IEmb)*        &
     &              EtaPrimal(COffset + IReb + IEtaCOff)
            ENDDO
            COffset = COffset + NEmbVReb

            DO IReb = 1, NEmbVReb
               IEmb = EmbVRebInd(IReb)
               EmbDat(IEmb, PEmbDatRebN, IEta) = ScaleVol(IEmb)*        &
     &              EtaPrimal(COffset + IReb + IEtaCOff)
            ENDDO
            COffset = COffset + NEmbVReb
         ENDIF

!        Cotas minimas con holgura Embalses
!

         IF (NEmbVMinH .gt. 0) THEN
            COffset = COffset + NEmbVMinH
         ENDIF


!        Traspasa Vars de Filtracion
!
         EmbDat(1:NCenEmb, PEmbDatFil, IEta) = 0.0d0
         IF (FFiltVar) THEN
            DO IFiltCen = 1, FiltNCen
               DO IEmb = 1, NCenEmb
                  IF (FiltEmbInd(IFiltCen) .EQ. IEmb) THEN
                     EmbDat(IEmb, PEmbDatFil, IEta) =                   &
     &                    EtaPrimal(COffset + IFiltCen + IEtaCOff)
                  ENDIF
               ENDDO
            ENDDO
            COffset = COffset + FiltNCen
         ENDIF
!        Traspasa datos de baterias
!        
         IF (FBaterias) THEN
            DO Idx=1, ParBaterias%NumColEta
               ParBaterias%DataEta(Idx, IEta)=                          &
     &            EtaPrimal(COffset + Idx + IEtaCOff)
            ENDDO
            COffset = COffset + ParBaterias%NumColEta
         ENDIF
!        Traspasa Datos de Laja
         IF (FConvLaja .EQ. 1) THEN
            DO Idx = 1, ParLaja%NumColEta
               ParLaja%DataEta(Idx, IEta) =                             &
     &              EtaPrimal(COffset + Idx + IEtaCOff)
               IF (ParLaja%VarEtaVol(Idx)) THEN
                  ParLaja%DataEta(Idx, IEta) =                          &
     &                 ScaleVol(ParLaja%IEmbLaja)                       &
     &                 * ParLaja%DataEta(Idx, IEta)
               ENDIF
            ENDDO
            COffset = COffset + ParLaja%NumColEta
         ENDIF

         IF (FConvLaja .EQ. 2) THEN
            DO Idx = 1, ParLajaM%NumColEta
               ParLajaM%DataEta(Idx, IEta) =                             &
     &              EtaPrimal(COffset + Idx + IEtaCOff)
               IF (ParLajaM%VarEtaVol(Idx)) THEN
                  ParLajaM%DataEta(Idx, IEta) =                          &
     &                 ScaleVol(ParLajaM%IEmbLaja)                       &
     &                 * ParLajaM%DataEta(Idx, IEta)
               ENDIF
            ENDDO
            COffset = COffset + ParLajaM%NumColEta
            IF (FRestReserva) THEN
               COffset = COffset + ParRes%NumColEta
            ENDIF
         ENDIF

!        Traspasa Datos de Maule
!
         IF (FConvMaule .NE. 0) THEN
            DO Idx = 1, ParMaule%NumColEta
               ParMaule%DataEta(Idx, IEta) =                             &
     &              EtaPrimal(COffset + Idx + IEtaCOff)
               IF (ParMaule%VarEtaVol(Idx)) THEN
                  ParMaule%DataEta(Idx, IEta) =                          &
     &                 ScaleVol(ParMaule%IEmbMaule)                      &
     &                 * ParMaule%DataEta(Idx, IEta)
               ENDIF
            ENDDO
            COffset = COffset + ParMaule%NumColEta
         ENDIF

!        Salta datos de Ralco
         IF (FRestRalco) THEN
            COffset = COffset + ParRalco%NumColEta
         ENDIF

!        Traspasa Datos de Gnl
!
         IF (FRestGnl) THEN
            Do I = 1, ParGnl%NumTGNL
               DO Idx = 1, ParGnl%TGNL(I)%NumColEta
                  ParGnl%TGNL(I)%DataEta(Idx, IEta) =                   &
     &                 EtaPrimal(COffset + Idx + IEtaCOff)
               ENDDO
               COffset = COffset + ParGnl%TGNL(I)%NumColEta
            ENDDO
         ENDIF


!        Traspasa Duales
!
         DO IBlo = 1, NBloque(IEta)
            IBInd = BloInd(IBlo, IEta)
            FOffset = FBloque*(IBlo - 1)

            ! Costos Marginales de las barras
            invbdur = 1.0d0/(BloDur(IBInd)*FactTiempo)
            DO IBar = 1, NBarra
               CMg(IBar, IBInd) =                                       &
     &              EtaDual(FOffset + IBar + IEtaFOff)                  &
     &              *invbdur*3.6d0
            ENDDO

            FOffset = FOffset + NBarra + NCenEmb

            IF (FScaleQs) THEN
               scaleqs = invbdur
            ELSE
               scaleqs = 1.0d0
            ENDIF
            ! Costos marginales de las centrales de servicio
            DO IHid = 1, NCenSer
               SerDat(IHid, PSerDatCMg, IBInd) =                         &
     &              - EtaDual(FOffset + IHid + IEtaFOff)*scaleqs
            ENDDO
            FOffset = FOffset + NCenSer  + NLinea

            IF (FConvLaja .EQ. 1) THEN
               FOffset = FOffset + ParLaja%NumFilBlo
            ENDIF
            IF (FConvLaja .EQ. 2) THEN
               FOffset = FOffset + ParLajaM%NumFilBlo
            ENDIF
            IF (FConvMaule .NE. 0) THEN
               FOffset = FOffset + ParMaule%NumFilBlo
            ENDIF
            IF (FRestRalco) THEN
               FOffset = FOffset + ParRalco%NumFilBlo
            ENDIF
            IF (FRestGnl) THEN
               Do I = 1, ParGnl%NumTGNL
                  FOffset = FOffset + ParGnl%TGNL(I)%NumFilBlo
               ENDDO
            ENDIF
            IF (FRestReserva) THEN
               invbdur = 1.0d0/(BloDur(IBInd)*FactTiempo)
               DO Idx = 1, ParRes%NumZonas*9
                  ParRes%DualBlo(Idx, IBInd) =                         &
     &                 EtaDual(FOffset + Idx + IEtaFOff)                   &
     &                 *invbdur*3.6d0
               ENDDO
               FOffset = FOffset + ParRes%NumFilBlo
            ENDIF
            IF (FBaterias) THEN
               invbdur = 1.0d0/FactTiempo
               DO Idx = 1, ParBaterias%NumFilBlo
                  ParBaterias%DualBlo(Idx, IBInd) =                         &
     &                 EtaDual(FOffset + Idx + IEtaFOff)                   &
     &                 *invbdur*3.6d0
               ENDDO
               FOffset= FOffset + ParBaterias%NumFilBlo
            ENDIF
         ENDDO
         FOffset = FBloque*NBloque(IEta)

         ! Duales de embalses
         DO IHid = 1, NCenEmb
            EmbDat(IHid, PEmbDatCMg, IEta) =                            &
     &           - EtaDual(FOffset + IHid + IEtaFOff)/ScaleVol(IHid)
         ENDDO
         FOffset = FOffset + 2*NVol

         ! Resto de duales
         IF (FVertReb) THEN
            FOffset = FOffset + 2*NEmbVReb
         ENDIF
         IF (FFiltVar) THEN
            FOffset = FOffset + FiltNCen
         ENDIF
         IF (FBaterias) THEN
            FOffset = FOffset + ParBaterias%NumFilEta
         ENDIF
         IF (FConvLaja .EQ. 1) THEN
            FOffset = FOffset + ParLaja%NumFilEta
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            FOffset = FOffset + ParLajaM%NumFilEta
            IF (FRestReserva) THEN
               FOffset = FOffset + ParRes%NumFilEta
            ENDIF
         ENDIF
         IF (FConvMaule .NE. 0) THEN
            FOffset = FOffset + ParMaule%NumFilEta
         ENDIF
         IF (FRestRalco) THEN
            FOffset = FOffset + ParRalco%NumFilEta
         ENDIF
         IF (FRestGnl) THEN
            Do I = 1, ParGnl%NumTGNL
               DO Idx = 1, ParGnl%TGNL(I)%NumFilEta
                  ParGnl%TGNL(I)%DualEta(Idx, IEta) =                              &
     &                 - EtaDual(FOffset + Idx + IEtaFOff)
            ENDDO
               FOffset = FOffset + ParGnl%TGNL(I)%NumFilEta
            ENDDO
         ENDIF

         IEtaCOff = IEtaCOff + PDNCol(IEta)
         IEtaFOff = IEtaFOff + PDNFila(IEta)
      ENDDO


      RETURN
      END
