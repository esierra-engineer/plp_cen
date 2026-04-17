!*****************************
!     Define Matrices Variante P.D.
!*****************************
      SUBROUTINE PDMatVar(IEta, FFiltVar,                               &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbCReb,                     &
     &     NEmbVMinH, EmbVMinHInd, EmbCMinH,                            &
     &     FExtrac, ExtrNCen, ExtrMax,                                  &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FiltNCen, ScaleObj, ScaleVol,                                &
     &     NLinea, NCentral,                                            &
     &     NVol, NVert, NFlujo, NAng, NAfluFict,                        &
     &     CCaudFalla, CVertimiento, CTrasmision,                       &
     &     CenPMin, CenPMax, CenVMin, CenVMax,                          &
     &     LinTMax,                                                     &
     &     LinNFlu, LinFOpe, FAngZero,                                  &
     &     CenCVar, CenRen,                                             &
     &     EmbVMin, EmbVMax,                                            &
     &     EmbQeLow, EmbQeUpp,                                          &
     &     NBloque, BloInd, BloDur, FPhi,                               &
     &     PDNCol, FO, LowBnd, UppBnd, Dim)
!     comun a todas las rutinas:
      USE PLP, ONLY : PAR_DIMS, PAR_LAJA, PAR_MAULE, PAR_RALCO, PAR_GNL, &
     &     PAR_LAJAM, PAR_RESERVA, PAR_BATERIAS

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!
      INTEGER IEta
      INTEGER PDNCol
      INTEGER COffset
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NAfluFict
      INTEGER NCentral
      INTEGER NVol
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER NAng
      INTEGER NVert
      INTEGER FiltNCen
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

      LOGICAL FExtrac
      INTEGER ExtrNCen
      DOUBLE PRECISION ExtrMax(Dim%Extr)


      LOGICAL LinFOpe(Dim%Lin, Dim%Blo)
      LOGICAL FAngZero
      LOGICAL FVertReb
      INTEGER NEmbVReb
      INTEGER EmbVRebInd(Dim%EmbVReb)

      DOUBLE PRECISION CCaudFalla
      DOUBLE PRECISION CenCVar(Dim%Cen)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION CenVMax(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenVMin(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CTrasmision
      DOUBLE PRECISION CVertimiento
      DOUBLE PRECISION EmbCReb(Dim%EmbVReb)
      DOUBLE PRECISION EmbVMax(Dim%Emb)
      DOUBLE PRECISION EmbVMin(Dim%Emb)

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbCMinH(Dim%Emb)

      
      DOUBLE PRECISION EmbQeLow(Dim%Emb)
      DOUBLE PRECISION EmbQeUpp(Dim%Emb)
      DOUBLE PRECISION FPhi
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo)
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

      DOUBLE PRECISION ScaleVol(Dim%Emb)
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION SFPhi
      

      INTEGER NBloque
      INTEGER BloInd(Dim%IBlo)
      DOUBLE PRECISION BloDur(Dim%Blo)
      DOUBLE PRECISION bdur
      DOUBLE PRECISION edur
      DOUBLE PRECISION CQVert

      INTEGER IBlo
      INTEGER IBInd
      INTEGER CenOffset

      INTEGER I

!***********************************************
!     El orden del tipos de las centrales es
!     1 -  - > NCenEmb, 1 -  - > NCenSer, 1 -  - > NCenTermica
!     llenado de matrices de coeficientes A y E
!***********************************************
!     Generacion Centrales
!********************
      COffset = 0

      SFPhi = ScaleObj*FPhi

      edur = 0.0d0
      DO IBlo = 1, NBloque
         IBInd = BloInd(IBlo)
         bdur = BloDur(IBInd)
         edur = edur + bdur
      ENDDO
      DO IBlo = 1, NBloque
         IBInd = BloInd(IBlo)
         bdur = BloDur(IBInd)
         CenOffset = COffset
         CALL GenPDCenFO(NCentral,                                      &
     &        CenCVar, CenRen,                                          & 
     &        CenPMin(1, IBInd), CenPMax(1, IBInd),                     &
     &        bdur, SFPhi, COffset,                                     &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + NCentral

!        Flujos Lineas
!*************
         CALL GenPDLinFO(NLinea,                                        &
     &        LinNFlu, LinFOpe(1, IBind),                               &
     &        LinTMax(1, 1, 1, IBind), Dim%Lin, Dim%Flu,                &
     &        CTrasmision,                                              &
     &        SFPhi, COffset,                                           &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + 2*NFlujo

!        Vertederos
!**********
         CQVert = CVertimiento
         CALL GenPDVerFO(NVert,                                         &
     &        CQVert,                                                   &
     &        CenVMin(1, IBInd), CenVMax(1, IBInd),                     &
     &        bdur, SFPhi, COffset,                                     &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + NVert

!        Afluentes Agotamiento
!*********************
         CALL GenPDAgoFO(NAfluFict,                                     &
     &        CCaudFalla,                                               &
     &        bdur, SFPhi,                                              &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + NAfluFict

!        Caudal Embalses
!**************
         CALL GenPDQEmbFO(NVol,                                         &
     &        EmbQeLow, EmbQeUpp,                                       &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + NVol

!        Angulos Barras
!**************
         CALL GenPDAngFO(NAng, FAngZero,                                &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + NAng

!        retiros
!**************
         IF (FExtrac) THEN
            CALL GenPDExtrFO(ExtrNCen, ExtrMax,                         &
     &           COffset,                                               &
     &           PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ExtrNCen
         ENDIF
         
!        convenio laja
!**************
         IF (FConvLaja .EQ. 1) THEN
            CALL GenPDLajaBloFO(ParLaja,                                &
     &           bdur, SFPhi,                                           &
     &           COffset,                                               &
     &           PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParLaja%NumColBlo
         ENDIF
         IF (FConvLaja .EQ. 2) THEN
            CALL GenPDLajaMBloFO(IEta, ParLajaM,                        &
     &           bdur, SFPhi,                                           &
     &           COffset,                                               &
     &           PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParLajaM%NumColBlo
         ENDIF

!        convenio maule
!**************
         IF (FConvMaule .NE. 0) THEN
            CALL GenPDMauleBloFO(ParMaule,                              &
     &           bdur, SFPhi,                                           &
     &           COffset, CenOffset,                                    &
     &           PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParMaule%NumColBlo
         ENDIF

!        restriccion ralco
!**************
         IF (FRestRalco) THEN
            COffset = COffset + ParRalco%NumColBlo
         ENDIF

!        restriccion gnl
!**************
         IF (FRestGnl) THEN
            DO I=1, ParGNL%NumTGNL
               COffset = COffset + ParGnl%TGNL(I)%NumColBlo
            ENDDO
         ENDIF

!     restriccion reserva
!**************
         IF (FRestReserva) THEN
            CALL GenPDResFO(IEta, IBind,                            &
     &           ParReserva,ParLajaM,FConvLaja,                     &
     &           bdur, SFPhi,                                       &
     &           COffset,                                           &
     &           PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParReserva%NumColBlo
         ENDIF

         IF (FBaterias) THEN
            CALL GenPDBatBloFO(IBlo, NBloque, IBind,                  &
      &           ParBaterias, edur,                       &
      &           COffset,PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParBaterias%NumColBlo
         ENDIF
!     cierra lazo de bloques
      ENDDO

!     Vols Embalses
!**************
      CALL GenPDVolFO(NVol, ScaleVol,                                   &
     &     EmbVMin, EmbVMax,                                            &
     &     COffset,                                                     &
     &     PDNCol, FO, LowBnd, UppBnd)
      COffset = COffset + 2*NVol

      IF (FVertReb) THEN
         CALL GenPDRebFO(NVol, NEmbVReb, EmbVRebInd,                    &
     &        ScaleVol,                                                 &
     &        EmbCReb, edur, SFPhi,                                     &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + 3*NEmbVReb
      ENDIF            

      IF (NEmbVMinH .gt. 0) THEN
         CALL GenPDMinHFO(NVol, NEmbVMinH, EmbVMinHInd,                 &
     &        EmbCMinH,                                                 &
     &        ScaleVol,                                                 &
     &        SFPhi,                                                    &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + NEmbVMinH
      ENDIF            

      
      IF (FFiltVar) THEN
         CALL GenPDFilFO(FiltNCen,                                      &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + FiltNCen
      ENDIF
      IF (FBaterias) THEN
         CALL GenPDBatEtaFO(ParBaterias,COffset,edur,IEta,         &
     &          BloInd(1),PDNCol, FO, LowBnd, UppBnd)
         COffset= COffset + ParBaterias%NumColEta
      ENDIF
      IF (FConvLaja .EQ. 1) THEN
         CALL GenPDLajaEtaFO(ParLaja, edur,  SFPhi,                     &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + ParLaja%NumColEta
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         CALL GenPDLajaMEtaFO(IEta, ParLajaM, edur,  SFPhi,             &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + ParLajaM%NumColEta
         IF (FRestReserva) THEN 
            CALL GenPDResEtaFO(ParReserva,     &
      &     COffset, PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParReserva%NumColEta
         ENDIF
      ENDIF

      IF (FConvMaule .NE. 0) THEN
         CALL GenPDMauleEtaFO(ParMaule, edur, SFPhi,                    &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + ParMaule%NumColEta
      ENDIF

      IF (FRestRalco) THEN
         CALL GenPDRalcoEtaFO(ParRalco,                                 &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
         COffset = COffset + ParRalco%NumColEta
      ENDIF

      IF (FRestGnl) THEN
         DO I=1, ParGNL%NumTGNL
            CALL GenPDTGnlEtaFO(ParGnl%TGNL(I), edur, SFPhi,            &
     &        COffset,                                                  &
     &        PDNCol, FO, LowBnd, UppBnd)
            COffset = COffset + ParGnl%TGNL(I)%NumColEta
         ENDDO
      ENDIF

      RETURN
      END
