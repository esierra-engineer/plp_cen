!******************************************
!>    Subrutina Define Matrices Invariantes P.D.
!******************************************
      SUBROUTINE PDMatInv(IEta, NBarra, NLinea, NCentral,               &
     &     NCenEmb, NCenSer, NCenPas, CenInd,                           &
     &     NVol, NVert, NFlujo, NAng, NAfluFict,                        &
     &     CenGBar, CenGHid, CenVHid,                                   &
     &     CenRen,                                                      &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd, FiltVarColInd, CenFHid,      &
     &     FExtrac, ExtrNCen, ExtrCenInd, CenXHid,                      &
     &     LinFOpe, LinFPer, LinHVDC, LinXImp, LinNBar, LinNFlu,        &
     &     LinTMax, LinRes, LinVNom,                                    &
     &     FPerdTram, FPerdLin,                                         &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH, EmbVMinHInd, EmbCMinH,                            &
     &     FScaleQs, ScaleAng, ScaleVol,                                &
     &     NBloque, BloInd, BloDur,                                     &
     &     FactTiempo,                                                  &
     &     A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
!     comun a todas las rutinas:
      USE PLP, ONLY : PAR_DIMS, PAR_LAJA, PAR_MAULE, PAR_RALCO, PAR_GNL,&
     &     PAR_LAJAM, PAR_RESERVA, PAR_BATERIAS
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     variables
!*********
      INTEGER PDNCol, PDNFila

      CHARACTER*1 FPerdLin
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*1 Sentido(PDNFila)

      INTEGER IEta
      INTEGER CenGBar(Dim%Cen)
      INTEGER CenGHid(Dim%Cen, 2)
      INTEGER CenVHid(Dim%Cen, 2)
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NAfluFict
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NCenPas
      INTEGER CenInd(Dim%Cen)
      INTEGER NCentral
      INTEGER NVol
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER NAng
      INTEGER NVert

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

      LOGICAL FFiltVar
      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER CenFHid(Dim%EmbFilt)

      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER CenXHid(Dim%Cen)
      INTEGER ExtrCenInd(Dim%Extr)

      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(NEmbVMinH)
      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)

      LOGICAL FScaleQs
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION ScaleVol(Dim%Emb)
      LOGICAL FPerdTram
      LOGICAL LinFOpe(Dim%Lin, Dim%Blo)
      LOGICAL LinFPer(Dim%Lin)
      LOGICAL LinHVDC(Dim%Lin)
      TYPE(AMatrix) A
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION LinXImp(Dim%Lin, Dim%Blo)
      DOUBLE PRECISION PerdEms
      DOUBLE PRECISION PerdRec

      INTEGER NBloque
      INTEGER BloInd(Dim%IBlo)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION bdur
      DOUBLE PRECISION bdursc
      DOUBLE PRECISION edur

!     Variables Locales
!*****************
      INTEGER COffset
      INTEGER FOffset
      INTEGER FOffset_ResLaja
      INTEGER FOffseti
      INTEGER FVolOffset
      INTEGER CVolOffset
      INTEGER QEmbOffset
      INTEGER IBlo
      INTEGER IBind
      INTEGER CBloque
      INTEGER FBloque
      INTEGER CenOffset
      INTEGER VerOffset
      INTEGER VolOffset
      INTEGER FBloOffset
      INTEGER FEmbOffset
      INTEGER BatCOffset
      INTEGER BatCOffsetPrevio
      INTEGER BarrasFOffset
      INTEGER I


!*******************************************
!>    El orden del tipos de las centrales es
!>    1 -  - >NCenEmb, 1 -  - >NCenSer, 1 -  - >NCenTermica
!>    llenado de matrices de coeficientes A y E
!*******************************************
!     Generacion Centrales
!********************
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
         CBloque = CBloque + ParReserva%NumColBlo
         FBloque = FBloque + ParReserva%NumFilBlo
      ENDIF
      IF (FBaterias) THEN
         CBloque = CBloque + ParBaterias%NumColBlo
         FBloque = FBloque + ParBaterias%NumFilBlo
      ENDIF

      PerdEms = 0.5d0
      PerdRec = 0.5d0
      IF (FPerdLin .EQ. 'E') THEN
         PerdEms = 1.0d0
         PerdRec = 0.0d0
      END IF
      IF (FPerdLin .EQ. 'R') THEN
         PerdEms = 0.0d0
         PerdRec = 1.0d0
      END IF

      edur = 0.0d0
      DO IBlo = 1, NBloque
         IBInd = BloInd(IBlo)
         bdur = BloDur(IBind)*FactTiempo
         edur = edur + bdur
      ENDDO


      COffset = 0
      FOffset = 0
      FVolOffset = NBloque*FBloque
      CVolOffset = NBloque*CBloque

      BatCOffset=0
      DO IBlo = 1, NBloque
         IBInd = BloInd(IBlo)
         bdur = BloDur(IBind)*FactTiempo
         IF (FScaleQs) THEN
            bdursc = 1.0d0
         ELSE
            bdursc = bdur
         ENDIF

!     centrales
!**********
         CenOffset = COffset
         BarrasFOffset= FOffset
         CALL GenPDCenA(IBlo, NBarra, NCenEmb, NCenSer, NcenPas,        &
     &        NCentral, CenInd, CenRen, CenGHid, CenGBar,               &
     &        bdursc,                                                   &
     &        COffset, FOffset,                                         &
     &        A, PDNCol, PDNombre, Dim)

!     lineas
!**********
         CALL GenPDLinA(IBlo, NBarra, NLinea, NCenEmb, NCenSer,         &
     &        NFlujo, LinFOpe(1, IBind), LinNBar, LinNFlu, LinHVDC,     &
     &        LinTMax(1, 1, 1, IBind), LinRes(1, IBind),                &
     &        LinVNom(1, IBind),                                        &
     &        LinFPer, FPerdTram, PerdEms, PerdRec,                     &
     &        COffset, FOffset,                                         &
     &        A, PDNCol, PDNombre, Dim)

!     Vertederos
!**********
         VerOffset = COffset
         CALL GenPDVerA(IBlo, NBarra, NCenEmb, NCenSer, CenInd,         &
     &        NVert, CenVHid, bdursc,                                   &
     &        FVertReb, NEmbVReb, EmbVRebInd,                           &
     &        CVolOffset, COffset, FOffset,                             &
     &        A, PDNCol, PDNombre, Dim)

!     Afluentes Agotamiento
!*********************
         CALL GenPDAgoA(IBlo, NBarra, NCenEmb, CenInd,                  &
     &        NAfluFict,                                                &
     &        bdursc,                                                   &
     &        COffset, FOffset,                                         &
     &        A, PDNCol, PDNombre, Dim)

!     Caudal Embalses
!**************
         QEmbOffset  = COffset
         CALL GenPDQEmbA(IBlo, NBarra, NCenEmb, CenInd,                 &
     &        NVol, ScaleVol,                                           &
     &        bdur, bdursc,                                             &
     &        COffset, FOffset, FVolOffset,                             &
     &        A, PDNCol, PDNombre, Dim)

!     Angulos Barras
!**************
         CALL GenPDAngA(IBlo, NBarra, NLinea, NCenEmb, NCenSer,         &
     &        NAng, ScaleAng, LinFOpe(1, IBind), LinNBar,               &
     &        LinXImp(1, IBind), LinVNom(1, IBind), LinHVDC,            &
     &        COffset, FOffset,                                         &
     &        A, PDNCol, PDNombre, Dim)

!     retiros
!**************
         IF (FExtrac) THEN
            CALL GenPDExtrA(IBlo, NBarra, NCenEmb, NCenSer, CenInd,     &
     &           ExtrNCen, ExtrCenInd, CenXHid,                         &
     &           bdursc,                                                &
     &           COffset, FOffset,                                      &
     &           A, PDNCol, PDNombre, Dim)
         ENDIF

!
! Modelos especiales
!**************
         FEmbOffset = FOffset + NBarra
         FBloOffset = FOffset + NBarra + NCenEmb + NCenSer + NLinea

!     Laja
!**************
         IF (FConvLaja .EQ. 1) THEN
            CALL GenPDLajaBloA(IEta, IBlo, bdur, edur,                  &
     &           ParLaja, CenOffset, FiltVarColInd,                     &
     &           FEmbOffset, COffset, FBloOffset,                       &
     &           A, PDNCol, PDNombre, Dim)
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            CALL GenPDLajaMBloA(IEta, IBlo, bdur, edur,                 &
     &           ParLajaM, CenOffset,                                   &
     &           FEmbOffset, COffset, FBloOffset,                       &
     &           A, PDNCol, PDNombre)
         ENDIF

!     Maule
!**************
         IF (FConvMaule .NE. 0) THEN
            CALL GenPDMauleBloA(IEta, IBlo, bdur, edur,                 &
     &           ParMaule, CenOffset, FiltVarColInd,                    &
     &           FEmbOffset, COffset, FBloOffset,                       &
     &           A, PDNCol, PDNombre, Dim)
         ENDIF
!     Ralco
!**************
         IF (FRestRalco) THEN
            CALL GenPDRalcoBloA(IEta, bdur, edur,                       &
     &           ParRalco,                                              &
     &           QEmbOffset, COffset, FBloOffset,                       &
     &           A)
         ENDIF
!     Gnl
!**************
         IF (FRestGnl) THEN
            Do I = 1, ParGnl%NumTGNL
               CALL GenPDTGnlBloA(IEta, bdur,                              &
     &              ParGnl%TGNL(I),                                        &
     &              CenOffset, COffset, FBloOffset,                        &
     &              A)
            ENDDO
         ENDIF

!     reserva
!**************
         IF (FRestReserva) THEN
            CALL GenPDResBloA(IEta, IBlo, IBind,                       &
     &           NCentral, CenInd, CenRen,                             &
     &           ParReserva,bdursc,bdur,CenGHid,Dim,   &
     &           CenOffset, COffset, FBloOffset,FEmbOffset,FConvLaja,ParLajaM, &
     &           A, PDNCol, PDNFila, PDNombre, Sentido)
         ENDIF

!     BATERIAS
!**************
         BatCOffsetPrevio= COffset
         IF (FBaterias) THEN
            CALL GenPDBatBloA(IEta,IBlo,Dim,                        &
     &      ParBaterias, COffset,FBloOffset,CenOffset,PDNCol,BloDur(IBind),        &
     &      PDNFila,BatCOffset,A,PDNombre,Sentido)

         ENDIF

         BatCOffset= BatCOffsetPrevio

!     Finaliza bloque
!**************
         FOffset = FOffset + FBloque
      ENDDO
!     Variables de Embalse
!********************
      FOffset = FVolOffset
!     Vols Embalses
!**************
      VolOffset = COffset
      CALL GenPDVolA(NCenEmb, CenInd,                                   &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     NEmbVMinH, EmbVMinHInd, EmbCMinH(1, IEta),                   &
     &     COffset, FVolOffset,                                         &
     &     A, PDNCol, PDNombre, Dim)
      COffset = COffset + 2*NVol
      FOffset = FOffset + 2*NVol

!     Rebalses Embalses
!**************
      IF (FVertReb) THEN
         CALL GenPDRebA(NEmbVReb, EmbVRebInd,                           &
     &        NCenEmb, CenInd, edur, ScaleVol,                          &
     &        COffset, FVolOffset,                                      &
     &        A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
         COffset = COffset + 3*NEmbVReb
         FOffset = FOffset + 2*NEmbVReb
      ENDIF

!     Cotas minimas con holgura Embalses
!**************

      IF (NEmbVMinH .gt. 0) THEN
         CALL GenPDMinHA(NEmbVMinH, EmbVMinHInd, EmbCMinH(1, IEta),     &
     &        CenInd,                                                   &
     &        COffset, FOffset,                                         &
     &        A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
         COffset = COffset + NEmbVMinH
         FOffset = FOffset + NEmbVMinH
      ENDIF


!     Filtraciones Embalses
!**************

      IF (FFiltVar) THEN
         CALL GenPDFilA(CenInd,                                         &
     &        FiltNCen, FiltEmbInd,                                     &
     &        COffset, FOffset,                                         &
     &        A, PDNCol, PDNombre, Dim)

         FOffseti = 0
         DO IBlo = 1, NBloque
            IBInd = BloInd(IBlo)
            bdur = BloDur(IBind)*FactTiempo
            IF (FScaleQs) THEN
               bdursc = 1.0d0
            ELSE
               bdursc = bdur
            ENDIF

            CALL GenPDFilAi(NBarra, NCenEmb, NCenSer,                   &
     &           FiltNCen, FiltEmbInd,                                  &
     &           CenFHid,                                               &
     &           bdursc,                                                &
     &           COffset, FOffseti,                                     &
     &           A, Dim)
            FOffseti = FOffseti + FBloque
         ENDDO

         COffset = COffset + FiltNCen
         FOffset = FOffset + FiltNCen
      ENDIF
      IF (FBaterias) THEN
         CALL GenPDBatEtaA(IEta,                        &
     &      ParBaterias, COffset,FOffset,PDNCol,        &
     &      A,PDNombre)
         FOffset = FOffset + ParBaterias%NumFilEta
         COffset = COffset + ParBaterias%NumColEta
      ENDIF
      IF (FConvLaja .EQ. 1) THEN
         CALL GenPDLajaEtaA(IEta, ParLaja, edur,                        &
     &        FiltVarColInd, VolOffset, COffset, FOffset,               &
     &        A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
         FOffset = FOffset + ParLaja%NumFilEta
         COffset = COffset + ParLaja%NumColEta
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         CALL GenPDLajaMEtaA(IEta, ParLajaM, edur,                      &
     &        FiltVarColInd, COffset, FOffset,                           &
     &        A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
         FOffset_ResLaja= FOffset
         FOffset = FOffset + ParLajaM%NumFilEta
         COffset = COffset + ParLajaM%NumColEta
         IF (FRestReserva) THEN 
            CALL GenPDResEtaA(IEta, ParLajaM, ParReserva,                    &
     &        COffset,PDNCol, FOffset, FOffset_ResLaja,                          &
     &        A, PDNombre)
            COffset = COffset + ParReserva%NumColEta
            FOffset = FOffset + ParReserva%NumFilEta
         ENDIF
      ENDIF

      IF (FConvMaule .NE. 0) THEN
         CALL GenPDMauleEtaA(IEta, ParMaule, edur,                      &
     &        FiltVarColInd, VolOffset, COffset, FOffset,               &
     &        A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
         FOffset = FOffset + ParMaule%NumFilEta
         COffset = COffset + ParMaule%NumColEta
      ENDIF

      IF (FRestRalco) THEN
         CALL GenPDRalcoEtaA(IEta, ParRalco,                            &
     &        VolOffset, COffset, FOffset,                              &
     &        A, PDNCol, PDNFila, PDNombre, Sentido)
         FOffset = FOffset + ParRalco%NumFilEta
         COffset = COffset + ParRalco%NumColEta
      ENDIF

      IF (FRestGnl) THEN
         Do I = 1, ParGnl%NumTGNL
            CALL GenPDTGnlEtaA(IEta,                                    &
     &        COffset, FOffset,                                         &
     &           A, PDNCol, PDNombre, ParGnl%TGNL(I))
            FOffset = FOffset + ParGnl%TGNL(I)%NumFilEta
            COffset = COffset + ParGnl%TGNL(I)%NumColEta
         ENDDO
      ENDIF

      RETURN
      END
