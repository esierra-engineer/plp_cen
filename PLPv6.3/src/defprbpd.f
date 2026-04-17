!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE DefPrbPD (NEtapa, NSimul, FOnePhi, FSeparaLP,          &
     &     FAngZero, FPerdTram, FPerdLin,                               &
     &     FAfluFict, FScaleQs,                                         &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVReb, EmbCReb,            &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH,                  &
     &     ScaleObj, ScalePhi, ScaleAng, ScaleVol, OptiEPS, OptiMLD,    &
     &     CCaudFalla, CVertimiento, CTrasmision,                       &
     &     NBarra, NLinea, NFlujo, NCentral,                            &
     &     NCenEmb, NCenSer, NCenPas, CenInd,                           &
     &     NBloque, BloInd, BloDur, BloPot,                             &
     &     CenManSInd, LinManSInd, EmbManSInd,                          &
     &     CenPMin, CenPMax, CenVMin, CenVMax,                          &
     &     CenCVar, CenRen, CenGBar, CenGHid, CenVHid,                  &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd, FiltVarColInd, CenFHid,      &
     &     FExtrac, ExtrNCen, ExtrCenInd, ExtrMax, CenXHid,             &
     &     EmbVIni, EmbVMin, EmbVMax, EmbQeLow, EmbQeUpp,               &
     &     LinVNom, LinRes, LinXImp, LinTMax,                           &
     &     LinNBar, LinNFlu, LinFPer, LinFOpe, LinHVDC,                 &
     &     FPhi, FactTiempo,                                            &
     &     PDNFila, PDNCol,                                             &
     &     PXNCol, PXNFila, PDLDAcNCol, PDLDAcColInd,                   &
     &     PlaCFRho, PlaCFBeta0, PlaCFIndCol,                           &
     &     NVarPhi, PlaneMaxIter,                                       &
     &     FSeparaFCF, FDepHidEta,                                      &
     &     AIncid, ULog, lp, envs, nthreads, Dim)
!     archivo comun a todas las rutinas:
#ifdef _OPENMP
      USE OMP_LIB
#endif
      USE PLP, ONLY : PAR_DIMS, C_SIZE_T, PAR_LAJA, PAR_MAULE, PAR_RALCO, &
     &      PAR_GNL, PAR_RESERVA, PAR_BATERIAS, DimTmp, DimLargo, No, PAR_LAJAM

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER nthreads
      INTEGER(C_SIZE_T) envs(Dim%Simul)
      INTEGER IEta
      INTEGER ISimul
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)
      INTEGER NEtapa
      INTEGER NSimul
      INTEGER NVarPhi
      INTEGER PXNCol(Dim%Eta)
      INTEGER PXNFila(Dim%Eta)
      INTEGER PlaneMaxIter

      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)

      INTEGER ULog
      LOGICAL FDepHidEta(Dim%Eta)
      LOGICAL FOnePhi
      LOGICAL FSeparaFCF

      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      CHARACTER*1 FPerdLin

      INTEGER CenGBar(Dim%Cen)
      INTEGER CenGHid(Dim%Cen, 2)
      INTEGER CenVHid(Dim%Cen, 2)
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NCenPas
      INTEGER CenInd(Dim%Cen)
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER PDNCol(Dim%Eta)
      INTEGER PDNFila(Dim%Eta)
      LOGICAL FAngZero
      LOGICAL FAfluFict
      LOGICAL FScaleQs
      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb

      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD

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
      INTEGER FiltNCen
      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER CenFHid(Dim%EmbFilt)

      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER CenXHid(Dim%Cen)
      INTEGER ExtrCenInd(Dim%Extr)
      DOUBLE PRECISION ExtrMax(Dim%Extr)

      INTEGER EmbManSInd(Dim%Simul)
      INTEGER CenManSInd(Dim%Simul)
      INTEGER LinManSInd(Dim%Simul)

      LOGICAL FPerdTram
      LOGICAL FSeparaLP
      LOGICAL LinFOpe(Dim%Lin, Dim%Blo, Dim%LinManS)
      LOGICAL LinFPer(Dim%Lin)
      LOGICAL LinHVDC(Dim%Lin)
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION CCaudFalla
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenVMax(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenVMin(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CTrasmision
      DOUBLE PRECISION CVertimiento
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta, Dim%EmbManS)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta, Dim%EmbManS)

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbVMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)

      DOUBLE PRECISION EmbQeLow(Dim%Emb)
      DOUBLE PRECISION EmbQeUpp(Dim%Emb)
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)
      DOUBLE PRECISION EmbCReb(Dim%EmbVReb)

      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinXImp(Dim%Lin, Dim%Blo, Dim%LinManS)

!

      INTEGER PDLDAcNCol
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      DOUBLE PRECISION PlaCFBeta0(Dim%XFila)
      DOUBLE PRECISION PlaCFRho(Dim%XCol, Dim%XFila)
      INTEGER PlaCFIndCol(Dim%XFila)
      INTEGER CountRNZ

!
!     locals
!
      INTEGER TmpFin(DimTmp)
      INTEGER TmpIni(DimTmp)
      INTEGER HorPro
      INTEGER MinPro
      INTEGER SegPro
      CHARACTER*(DimLargo) CHorPro
      CHARACTER*(DimLargo) CMinPro
      CHARACTER*(DimLargo) CSegPro
!
      CALL LeeTmp(TmpIni)

#ifndef _OPENMP
      IF (nthreads .GT. 1) THEN
         nthreads = 1
      ENDIF
#endif


      DO IEta = 1, NEtapa
         if (PXNFila(IEta) .GT. 0) THEN
            CountRNZ = COUNT(PlaCFRho(1:Dim%XCol, 1:PXNFila(IEta)) .NE. 0.0d0)
         ELSE
            CountRNZ = 0
         ENDIF

!$OMP PARALLEL NUM_THREADS(nthreads) &
!$OMP& default(shared) &
!$OMP& private(ISimul)
!$OMP DO SCHEDULE(RUNTIME)

         DO ISimul = 1, NSimul
            IF ((ISimul .GT. 1) .AND. .NOT. FSeparaLP) THEN
               lp(ISimul, IEta) = lp(1, IEta)
               CYCLE
            ENDIF

            CALL DefPrbPDi (IEta, ISimul,                                     &
     &           NEtapa, FOnePhi, FSeparaLP,                                  &
     &           FAngZero, FPerdTram, FPerdLin,                               &
     &           FAfluFict, FScaleQs,                                         &
     &           FVertReb, NEmbVReb, EmbVRebInd, EmbVReb, EmbCReb,            &
     &           NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH,                  &
     &           ScaleObj, ScalePhi, ScaleAng, ScaleVol, OptiEPS, OptiMLD,    &
     &           CCaudFalla, CVertimiento, CTrasmision,                       &
     &           NBarra, NLinea, NFlujo, NCentral,                            &
     &           NCenEmb, NCenSer, NCenPas, CenInd,                           &
     &           NBloque, BloInd, BloDur, BloPot,                             &
     &           CenManSInd, LinManSInd, EmbManSInd,                          &
     &           CenPMin, CenPMax, CenVMin, CenVMax,                          &
     &           CenCVar, CenRen, CenGBar, CenGHid, CenVHid,                  &
     &           FConvLaja, ParLaja, ParLajaM,                                &
     &           FConvMaule, ParMaule,                                        &
     &           FRestRalco, ParRalco,                                        &
     &           FRestGnl, ParGnl,                                            &
     &           FRestReserva, ParReserva,                                    &
     &           FBaterias, ParBaterias,                                      &
     &           FFiltVar, FiltNCen, FiltEmbInd, FiltVarColInd, CenFHid,      &
     &           FExtrac, ExtrNCen, ExtrCenInd, ExtrMax, CenXHid,             &
     &           EmbVIni, EmbVMin, EmbVMax, EmbQeLow, EmbQeUpp,               &
     &           LinVNom, LinRes, LinXImp, LinTMax,                           &
     &           LinNBar, LinNFlu, LinFPer, LinFOpe, LinHVDC,                 &
     &           FPhi, FactTiempo,                                            &
     &           PDNFila, PDNCol,                                             &
     &           PXNCol, PXNFila, PDLDAcNCol, PDLDAcColInd,                   &
     &           PlaCFRho, PlaCFBeta0, PlaCFIndCol, CountRNZ,                 &
     &           NVarPhi, PlaneMaxIter,                                       &
     &           FSeparaFCF, FDepHidEta,                                      &
     &           AIncid, lp(ISimul, IEta), envs(ISimul),                      &
     &           Dim)
         ENDDO

!$OMP END DO
!$OMP END PARALLEL


      ENDDO

      CALL LeeTmp(TmpFin)
      CALL DifTmp(TmpFin, TmpIni, HorPro, MinPro, SegPro)
      CALL Num2Char (HorPro, CHorPro, .TRUE., 2)
      CALL Num2Char (MinPro, CMinPro, .TRUE., 2)
      CALL Num2Char (SegPro, CSegPro, .TRUE., 2)
      WRITE(6, 98) 'Tiempo init. de LPs', CHorPro, CMinPro, CSegPro
      WRITE(ULog, 98) 'Tiempo init. de LPs', CHorPro, CMinPro, CSegPro

 98   FORMAT(A, ': ', A2, ':', A2, ':', A2)

      RETURN
      END


!
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
!

      SUBROUTINE DefPrbPDi (IEta, ISimul,                               &
     &     NEtapa, FOnePhi, FSeparaLP,                                  &
     &     FAngZero, FPerdTram, FPerdLin,                               &
     &     FAfluFict, FScaleQs,                                         &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVReb, EmbCReb,            &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH,                  &
     &     ScaleObj, ScalePhi, ScaleAng, ScaleVol, OptiEPS, OptiMLD,    &
     &     CCaudFalla, CVertimiento, CTrasmision,                       &
     &     NBarra, NLinea, NFlujo, NCentral,                            &
     &     NCenEmb, NCenSer, NCenPas, CenInd,                           &
     &     NBloque, BloInd, BloDur, BloPot,                             &
     &     CenManSInd, LinManSInd, EmbManSInd,                          &
     &     CenPMin, CenPMax, CenVMin, CenVMax,                          &
     &     CenCVar, CenRen, CenGBar, CenGHid, CenVHid,                  &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestRalco, ParRalco,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     FRestReserva, ParReserva,                                    &
     &     FBaterias, ParBaterias,                                      &
     &     FFiltVar, FiltNCen, FiltEmbInd, FiltVarColInd, CenFHid,      &
     &     FExtrac, ExtrNCen, ExtrCenInd, ExtrMax, CenXHid,             &
     &     EmbVIni, EmbVMin, EmbVMax, EmbQeLow, EmbQeUpp,               &
     &     LinVNom, LinRes, LinXImp, LinTMax,                           &
     &     LinNBar, LinNFlu, LinFPer, LinFOpe, LinHVDC,                 &
     &     FPhi, FactTiempo,                                            &
     &     PDNFila, PDNCol,                                             &
     &     PXNCol, PXNFila, PDLDAcNCol, PDLDAcColInd,                   &
     &     PlaCFRho, PlaCFBeta0, PlaCFIndCol, CountRNZ,                 &
     &     NVarPhi, PlaneMaxIter,                                       &
     &     FSeparaFCF, FDepHidEta,                                      &
     &     AIncid, lp, env, Dim)
!     archivo comun a todas las rutinas:
      USE OSI
      USE A_MATRIX
#ifdef _OPENMP
      USE OMP_LIB
#endif
      USE PLP, ONLY : PAR_DIMS, C_SIZE_T, PAR_LAJA, PAR_MAULE, &
     &     PAR_RALCO, PAR_GNL, PAR_LAJAM, PAR_RESERVA, PAR_BATERIAS

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER(C_SIZE_T) env
      INTEGER IEta
      INTEGER ISimul
      INTEGER(C_SIZE_T) lp
      INTEGER NEtapa
      INTEGER NVarPhi
      INTEGER PXNCol(Dim%Eta)
      INTEGER PXNFila(Dim%Eta)

      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)

      LOGICAL FDepHidEta(Dim%Eta)
      LOGICAL FOnePhi
      LOGICAL FSeparaFCF
      INTEGER PlaneMaxIter
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      CHARACTER*1 FPerdLin

      INTEGER CenGBar(Dim%Cen)
      INTEGER CenGHid(Dim%Cen, 2)
      INTEGER CenVHid(Dim%Cen, 2)
      INTEGER LinNBar(2, Dim%Lin)
      INTEGER LinNFlu(Dim%Lin)
      INTEGER NBarra
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NCenPas
      INTEGER CenInd(Dim%Cen)
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NLinea
      INTEGER PDNCol(Dim%Eta)
      INTEGER PDNFila(Dim%Eta)
      LOGICAL FAngZero
      LOGICAL FAfluFict
      LOGICAL FScaleQs
      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb

      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleAng
      DOUBLE PRECISION ScaleObj
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD

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
      INTEGER FiltNCen
      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)
      INTEGER FiltEmbInd(Dim%EmbFilt)
      INTEGER CenFHid(Dim%EmbFilt)

      LOGICAL FExtrac
      INTEGER ExtrNCen
      INTEGER CenXHid(Dim%Cen)
      INTEGER ExtrCenInd(Dim%Extr)
      DOUBLE PRECISION ExtrMax(Dim%Extr)


      INTEGER EmbManSInd(Dim%Simul)
      INTEGER CenManSInd(Dim%Simul)
      INTEGER LinManSInd(Dim%Simul)

      LOGICAL FPerdTram
      LOGICAL FSeparaLP
      LOGICAL LinFPer(Dim%Lin)
      DOUBLE PRECISION BloPot(Dim%Bar, Dim%Blo)
      DOUBLE PRECISION CCaudFalla
      DOUBLE PRECISION CenCVar(Dim%Cen, Dim%Eta)
      DOUBLE PRECISION CenPMax(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenPMin(Dim%Cen, Dim%Blo, Dim%CenManS)
      DOUBLE PRECISION CenVMax(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenVMin(Dim%Vert, Dim%Blo)
      DOUBLE PRECISION CenRen(Dim%Cen)
      DOUBLE PRECISION CTrasmision
      DOUBLE PRECISION CVertimiento
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta, Dim%EmbManS)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta, Dim%EmbManS)

      INTEGER NEmbVMinH
      INTEGER EmbVMinHInd(Dim%Emb)
      DOUBLE PRECISION EmbVMinH(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbCMinH(Dim%Emb, Dim%Eta)

      DOUBLE PRECISION EmbQeLow(Dim%Emb)
      DOUBLE PRECISION EmbQeUpp(Dim%Emb)
      DOUBLE PRECISION EmbVReb(Dim%EmbVReb)
      DOUBLE PRECISION EmbCReb(Dim%EmbVReb)

      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      LOGICAL LinFOpe(Dim%Lin, Dim%Blo, Dim%LinManS)
      LOGICAL LinHVDC(Dim%Lin)
      DOUBLE PRECISION LinRes(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinTMax(2, Dim%Flu, Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinVNom(Dim%Lin, Dim%Blo, Dim%LinManS)
      DOUBLE PRECISION LinXImp(Dim%Lin, Dim%Blo, Dim%LinManS)


!
      INTEGER PDLDAcNCol
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      DOUBLE PRECISION PlaCFBeta0(Dim%XFila)
      DOUBLE PRECISION PlaCFRho(Dim%XCol, Dim%XFila)
      INTEGER PlaCFIndCol(Dim%XFila)
      INTEGER CountRNZ

!
!     locals
!
      INTEGER NAfluFict
      INTEGER NAng
      INTEGER NVert
      INTEGER NVol

!
      CHARACTER*1 Sentido(PDNFila(IEta))
      CHARACTER*24 PDNombre(PDNCol(IEta))
      TYPE(AMatrix) A
      DOUBLE PRECISION FO(PDNCol(IEta))
      DOUBLE PRECISION LD(PDNFila(IEta))
      DOUBLE PRECISION LowBnd(PDNCol(IEta))
      DOUBLE PRECISION UppBnd(PDNCol(IEta))

      INTEGER CountANZ
      INTEGER DimMatnz, DimCols, DimRows
      CHARACTER*80 fconcat

      INTEGER I
!
      NVert = NCenEmb + NCenSer

      IF (FAfluFict) THEN
         NAfluFict = NCenEmb
      ELSE
         NAfluFict = 0
      ENDIF
      NVol = NCenEmb
      NAng = NBarra

      PDNombre = CHAR(0)


      CALL PDMatIni(A, PDNCol(IEta), PDNFila(IEta), PXNFila(IEta), &
     &     LD, FO, Sentido)

!     Define Matrices Invariantes
!******************************
      CALL PDMatInv(IEta,                                      &
     &     NBarra, NLinea, NCentral,                           &
     &     NCenEmb, NCenSer, NCenPas, CenInd,                  &
     &     NVol, NVert, NFlujo, NAng, NAfluFict,               &
     &     CenGBar, CenGHid, CenVHid,                          &
     &     CenRen,                                             &
     &     FConvLaja, ParLaja, ParLajaM,                       &
     &     FConvMaule, ParMaule,                               &
     &     FRestRalco, ParRalco,                               &
     &     FRestGnl, ParGnl,                                   &
     &     FRestReserva, ParReserva,                           &
     &     FBaterias,ParBaterias,                              &
     &     FFiltVar, FiltNCen,                                 &
     &     FiltEmbInd, FiltVarColInd, CenFHid,                 &
     &     FExtrac, ExtrNCen, ExtrCenInd, CenXHid,             &
     &     LinFOpe(1, 1, LinManSInd(ISimul)), LinFPer, LinHVDC,&
     &     LinXImp(1, 1, LinManSInd(ISimul)), LinNBar, LinNFlu,&
     &     LinTMax(1, 1, 1, 1, LinManSInd(ISimul)),            &
     &     LinRes(1, 1, LinManSInd(ISimul)),                   &
     &     LinVNom(1, 1, LinManSInd(ISimul)),                  &
     &     FPerdTram, FPerdLin,                                &
     &     FVertReb, NEmbVReb, EmbVRebInd,                     &
     &     NEmbVMinH, EmbVMinHInd, EmbCMinH,                   &
     &     FScaleQs, ScaleAng, ScaleVol,                       &
     &     NBloque(IEta), BloInd(1, IEta), BloDur,             &
     &     FactTiempo,                                         &
     &     A, PDNCol(IEta), PDNFila(IEta),                     &
     &     PDNombre, Sentido, Dim)

      DO I =1, PDNCol(IEta)
         PDNombre(I) = fconcat(PDNombre(I),'')
      ENDDO


      CountANZ = A%nz

!     Define Lado Derecho
!************************
      CALL PDMatLD(IEta, PDNFila(IEta),                        &
     &     NBarra, NLinea, NCenEmb, NCenSer,                   &
     &     NBloque(IEta), BloInd(1, IEta), BloPot,             &
     &     CenPMin(1, 1, CenManSInd(ISimul)),                  &
     &     CenPMax(1, 1, CenManSInd(ISimul)),                  &
     &     ISimul,                                             &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVReb,            &
     &     NEmbVMinH, EmbVMinHInd, EmbVMinH, EmbCMinH,         &
     &     FFIltVar, FiltNCen,                                 &
     &     FConvLaja, ParLaja, ParLajaM,                       &
     &     FConvMaule, ParMaule,                               &
     &     FRestRalco, ParRalco,                               &
     &     FRestGnl, ParGnl,                                   &
     &     FRestReserva, ParReserva,                           &
     &     FBaterias,ParBaterias,                              &
     &     EmbVIni, ScaleVol, LD,Sentido, Dim)

!     Define Matrices Variantes
!******************************
      CALL PDMatVar(IEta, FFiltVar,                            &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbCReb,            &
     &     NEmbVMinH, EmbVMinHInd,                             &
     &     EmbCMinH(1, IEta),                                  &
     &     FExtrac, ExtrNCen, ExtrMax,                         &
     &     FConvLaja, ParLaja, ParLajaM,                       &
     &     FConvMaule, ParMaule,                               &
     &     FRestRalco, ParRalco,                               &
     &     FRestGnl, ParGnl,                                   &
     &     FRestReserva, ParReserva,                           &
     &     FBaterias, ParBaterias,                             &
     &     FiltNCen, ScaleObj, ScaleVol,                       &
     &     NLinea, NCentral,                                   &
     &     NVol, NVert, NFlujo, NAng, NAfluFict,               &
     &     CCaudFalla, CVertimiento, CTrasmision,              &
     &     CenPMin(1, 1, CenManSInd(ISimul)),                  &
     &     CenPMax(1, 1, CenManSInd(ISimul)),                  &
     &     CenVMin, CenVMax,                                   &
     &     LinTMax(1, 1, 1, 1, LinManSInd(ISimul)), LinNFlu,   &
     &     LinFOpe(1, 1, LinManSInd(ISimul)), FAngZero,        &
     &     CenCVar(1, IEta), CenRen,                           &
     &     EmbVMin(1, IEta, EmbManSInd(ISimul)),               &
     &     EmbVMax(1, IEta, EmbManSInd(ISimul)),               &
     &     EmbQeLow, EmbQeUpp,                                 &
     &     NBloque(IEta), BloInd(1, IEta), BloDur,             &
     &     FPhi(IEta), PDNCol(IEta), FO,                       &
     &     LowBnd, UppBnd, Dim)


      DimMatnz = CountANZ
      DimCols = PDNCol(IEta)
      DimRows = PDNFila(IEta)

      IF (PXNCol(IEta)*PXNFila(IEta) .GT. 0) THEN
         DimMatnz = DimMatnz + CountRNZ
         DimMatnz = DimMatnz + PXNFila(IEta)
         DimCols = DimCols + PXNCol(IEta)
         DimRows = DimRows + PXNFila(IEta)
      ENDIF
      DimMatnz = DimMatnz + NVarPhi
      DimCols = DimCols + NVarPhi
      DimRows = DimRows + NVarPhi

      IF (FOnePhi) THEN
         CALL DefPrgOne (IEta, A, LD, FO,                      &
     &        LowBnd,  UppBnd,                                 &
     &        DimMatnz, DimCols, DimRows,                      &
     &        Sentido, PDNFila(IEta), PDNCol(IEta), PDNombre,  &
     &        PXNFila(IEta), PDLDAcNCol, PDLDAcColInd(1, IEta),&
     &        PlaCFRho, PlaCFBeta0, PlaCFIndCol,               &
     &        ISimul, PlaneMaxIter,                            &
     &        FSeparaFCF,                                      &
     &        ScaleObj, ScalePhi, ScaleVol, OptiEPS, OptiMLD,  &
     &        lp, env, Dim)
      ELSE
         CALL DefPrg (IEta, A, LD, FO,                         &
     &        LowBnd, UppBnd,                                  &
     &        DimMatnz, DimCols, DimRows,                      &
     &        Sentido, PDNFila(IEta), PDNCol(IEta), PDNombre,  &
     &        NVarPhi, PXNCol(IEta), PXNFila(IEta),            &
     &        PDLDAcNCol, PDLDAcColInd(1, IEta),               &
     &        PlaCFRho, PlaCFBeta0, PlaCFIndCol,               &
     &        ISimul,                                          &
     &        FSeparaLP, FSeparaFCF, FDepHidEta,               &
     &        ScaleObj, ScalePhi, ScaleVol, OptiEPS,           &
     &        lp, env, NEtapa, Dim)
      ENDIF

      IF (IEta .eq. 1 .and. ISimul .eq. 1) THEN
         CALL MatIncid(AIncid, Dim%HidSPP, A,  &
     &        NCentral, NCenEmb, NCenSer, NBarra, NFlujo)
      ENDIF

      CALL Am_close(A)

      RETURN
      END

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      SUBROUTINE DefPrg (IEta, A, LD, FO, LowBnd, UppBnd,               &
     &     DimMatnz, DimCols, DimRows,                                  &
     &     Sentido, PDNFila, PDNCol, PDNombre,                          &
     &     NVarPhi, PXNCol, PXNFila, PDLDAcNCol, PDLDAcColInd,          &
     &     PlaCFRho, PlaCFBeta0, PlaCFIndCol,                           &
     &     ISimul,                                                      &
     &     FSeparaLP, FSeparaFCF, FDepHidEta,                           &
     &     ScaleObj, ScalePhi, ScaleVol, OptiEPs,                       &
     &     lp, env, NEtapa, Dim)
      use iso_c_binding
      USE PLP
      USE OSI
      USE A_MATRIX
      INCLUDE 'machcons.fpp'


      TYPE(PAR_DIMS), INTENT(IN)::  Dim
      INTEGER PDNCol
      INTEGER PDNFila
      INTEGER PXNCol
      INTEGER PXNFila

      INTRINSIC loc
!     INTEGER loc
      CHARACTER*24 PDNombre(PDNCol)
      CHARACTER*24 XPDNombre(PXNCol)
      INTEGER ICol
      INTEGER IEta
      INTEGER IFila
      INTEGER IVarPhi
      INTEGER NVarPhiAux
      INTEGER(C_SIZE_T) env
      INTEGER(C_SIZE_T) lp
      INTEGER NEtapa
      INTEGER NCol
      INTEGER NFila
      INTEGER NFilaFinal
      INTEGER NFilaInicial
      INTEGER NNZero
      INTEGER NVarPhi
      LOGICAL DxAEQy
      LOGICAL FDepHidEta(NEtapa)
      LOGICAL FSeparaLP
      LOGICAL FSeparaFCF

      CHARACTER*1 Sentido(PDNFila)
      TYPE(AMatrix) A
      DOUBLE PRECISION LD(PDNFila)
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleObj

!     inicio arreglos de uso de CPLEX
      CHARACTER*80 probname
      CHARACTER*80 cname(NVarPhi)
      CHARACTER*12 CVarPhi
      CHARACTER*80 fconcat


      INTEGER PDLDAcNCol
      INTEGER PDLDAcColInd(Dim%PDLDAcCol)
      INTEGER PlaCFIndCol(Dim%XFila)
      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      DOUBLE PRECISION PlaCFRho(Dim%XCol, Dim%XFila)
      DOUBLE PRECISION PlaCFBeta0(Dim%XFila)
      INTEGER ISimul

      INTEGER IColx
      LOGICAL FSeparaX

      DOUBLE PRECISION coef
      DOUBLE PRECISION deps
      DOUBLE PRECISION OptiEPS


      INTEGER cols
      INTEGER objsen
      INTEGER rows

      INTEGER DimMatnz, DimCols, DimRows
      INTEGER(C_SIZE_T) colname(DimCols)
      INTEGER matbeg(DimCols + 1)
      DOUBLE PRECISION obj(DimCols)
      DOUBLE PRECISION ub(DimCols)
      DOUBLE PRECISION lb(DimCols)
      DOUBLE PRECISION rhs(DimRows)
      CHARACTER*1 sense(DimRows)
      INTEGER matind(DimMatnz)
      DOUBLE PRECISION matval(DimMatnz)

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

      NFila = PDNFila + PXNFila
      NCol = PDNCol + PXNCol
      cols = NCol
      rows = PDNFila
      objsen = 1

      DO IFila = 1, PDNFila
         rhs (IFila) = LD (IFila)
         sense (IFila) = Sentido (IFila)
      ENDDO
      DO ICol = 1, PDNCol
         ub (ICol) = UppBnd (ICol)
         lb (ICol) = LowBnd (ICol)
         colname (ICol) = loc (PDNombre (ICol))
         obj (ICol) = FO (ICol)
      ENDDO

      ! Xvarphi
      FSeparaX = .FALSE. .AND. FSeparaFCF .AND. FSeparaLP
      DO IFila = 1, PXNFila
         IF (FSeparaX .AND. (PlaCFIndCol(IFila) .NE. ISimul)) THEN
            rhs (PDNFila + IFila) = 0.0d0
            sense (PDNFila + IFila) = 'E'
         ELSE
            rhs (PDNFila + IFila) = PlaCFBeta0(IFila)/ScalePhi
            sense (PDNFila + IFila) = 'L'
         ENDIF
      ENDDO
      DO ICol = 1, PXNCol
         CVarPhi = ' '
         CALL Num2Char(ICol, CVarPhi, No, DimLargo)
         XPDNombre(ICol) = fconcat('xvarphi', CVarPhi)

         IF (FSeparaX .AND. (ICol .NE. ISimul)) THEN
            ub (PDNCol + ICol) = 0.0d0
            lb (PDNCol + ICol) = 0.0d0
            colname (PDNCol + ICol) = loc (XPDNombre (ICol))
            obj (PDNCol + ICol) = 0.0d0
         ELSE
            ub (PDNCol + ICol) = DINFTY
            lb (PDNCol + ICol) = -DINFTY
            colname (PDNCol + ICol) = loc (XPDNombre (ICol))
            IF (FSeparaX) THEN
               obj (PDNCol + ICol) = ScalePhi/ScaleObj
            ELSE
               obj (PDNCol + ICol) = (ScalePhi/ScaleObj)/PXNCol
            ENDIF
         ENDIF
      ENDDO


      DO ICol = 1, PDNCol
         !  Xvarphi
         DO IFila = 1, PXNFila
            IF (FSeparaX .AND. (PlaCFIndCol(IFila) .NE. ISimul)) THEN
               CYCLE
            ENDIF
            DO IColx = 1, PDLDAcNCol
               IF (PDLDAcColInd(IColx) .EQ. ICol) THEN
                  coef = PlaCFRho(IColx, IFila)                    &
     &                 *(ScaleVol(IColx)/ScalePhi)
                  deps = ABS(rhs (PDNFila + IFila))*OptiEPS
                  IF (.NOT. DxAEQy(coef, 0.0d0, deps)) THEN
                     CALL Am_set(A, ICol, PDNFila + IFila, coef)
                  ENDIF
               ENDIF
            ENDDO
         ENDDO
      ENDDO

      NNZero = 0
      CALL Am_flat(A, DEPSILON, matbeg, matind, matval, NNZero)


      !  Xvarphi
      DO ICol = PDNCol + 1, PDNCol + PXNCol
         IF ((FSeparaX .AND. (ICol .NE. ISimul + PDNCol))) THEN
            CYCLE
         ENDIF
         matbeg (ICol) = NNZero
         DO IFila = 1, PXNFila
            IF (PlaCFIndCol(IFila) .EQ. ICol - PDNCol) THEN
               NNZero = NNZero + 1
               matval (NNZero) = -1.0d0
               matind (NNZero) = PDNFila + IFila - 1
               rows = rows + 1
            ENDIF
         ENDDO
      ENDDO
      IF (IEta .LE. NEtapa) THEN
         NVarPhiAux = NVarPhi
         IF (IEta .EQ. NEtapa) THEN
            NVarPhiAux = 0
         ENDIF
         DO IVarPhi = 1, NVarPhiAux
            ub (NCol + IVarPhi) = DINFTY
            lb (NCol + IVarPhi) = -DINFTY

            CVarPhi = ' '
            CALL Num2Char(IVarPhi, CVarPhi, No, DimLargo)
            cname(IVarPhi) = fconcat('varphi', CVarPhi)
            colname (NCol + IVarPhi) = loc (cname(IVarPhi))
            obj (NCol + IVarPhi) = (ScalePhi/ScaleObj)/DBLE(NVarPhi)
            matbeg (NCol + IVarPhi) = NNZero
            NNZero = NNZero + 1
            matval (NNZero) = 1.0d0
            matind (NNZero) = NFila
            rhs (rows  + IVarPhi) = 0.0d0
            sense (rows + IVarPhi) = 'E'
         ENDDO
         cols = cols + NVarPhiAux
         rows = rows + NVarPhiAux
!
!     PELIGRO, WILL ROBINSON, PELIGRO!(1)
!     El unico criterio para decidir si la matriz de transicion es igual
!     la identidad o igual a 1/NVarphi consiste en revisar si la etapa
!     corresponde al invierno o al deshielo.
         IF ((FSeparaFCF) .OR.                                          &
     &        (FDepHidEta(IEta))) THEN
            DO IVarPhi = 1, NVarPhiAux
               obj (NCol + IVarPhi) = 0.0d0
            ENDDO
         ENDIF
      ENDIF

      IF (FSeparaLP) THEN
         CVarPhi = ' '
         CALL Num2Char(IEta, CVarPhi, No, DimLargo)
         probname = fconcat('LP_', CVarPhi)
         probname = fconcat(probname, '_')
         CVarPhi = ' '
         CALL Num2Char(ISimul, CVarPhi, No, DimLargo)
         probname = fconcat(probname, CVarPhi)
      ELSE
         CVarPhi = ' '
         CALL Num2Char(IEta, CVarPhi, No, DimLargo)
         probname = fconcat('LP_', CVarPhi)
      ENDIF

      matbeg(cols + 1) = NNZero

      lp = osi_lp_loadproblem(env, probname, cols, rows, objsen,     &
     &     matbeg, matind, matval, lb, ub, obj, sense, rhs, colname)


!     borra la ultima restriccion, la que corresponde a varphi
      NFilaInicial = PDNFila + PXNFila + 1
      NFilaFinal = IINFTY
      CALL BorResPD(lp, NFilaInicial, NFilaFinal)
      RETURN
      END

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE DefPrgOne(IEta, A, LD, FO, LowBnd, UppBnd,             &
     &     DimMatnz, DimCols, DimRows,                                  &
     &     Sentido, PDNFila, PDNCol, PDNombre,                          &
     &     PXNFila, PDLDAcNCol, PDLDAcColInd,                           &
     &     PlaCFRho, PlaCFBeta0, PlaCFIndCol,                           &
     &     ISimul, PlaneMaxIter,                                        &
     &     FSeparaFCF,                                                  &
     &     ScaleObj, ScalePhi, ScaleVol, OptiEPS, OptiMLD,              &
     &     lp, env, Dim)
      USE PLP
      USE OSI
      USE A_MATRIX


      INCLUDE 'machcons.fpp'

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
      INTEGER PDNCol
      INTEGER PDNFila

      INTRINSIC loc
!     INTEGER loc
      CHARACTER*24 PDNombre(PDNCol)
      CHARACTER*1 Sentido(PDNFila)
      INTEGER(C_SIZE_T) env
      INTEGER ICol
      INTEGER IEta
      INTEGER IFila
      INTEGER(C_SIZE_T) lp
      INTEGER NNZero
      INTEGER PXNFila
      LOGICAL FSeparaFCF
      INTEGER PlaneMaxIter

      TYPE(AMatrix) A
      DOUBLE PRECISION LD(PDNFila)
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION ScaleObj

      CHARACTER*80 probname

      INTEGER PDLDAcNCol
      INTEGER PDLDAcColInd(Dim%PDLDAcCol)
      INTEGER PlaCFIndCol(Dim%XFila)
      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      DOUBLE PRECISION PlaCFRho(Dim%XCol, Dim%XFila)
      DOUBLE PRECISION PlaCFBeta0(Dim%XFila)
      INTEGER ISimul

      CHARACTER*24 XPDNombre
      DOUBLE PRECISION vobj
      DOUBLE PRECISION vlb
      DOUBLE PRECISION vub
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol)
      DOUBLE PRECISION LDPhi

      CHARACTER*80 fconcat
      CHARACTER*12 CVarPhi

      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD

!     inicio arreglos de uso de CPLEX
      INTEGER DimMatnz, DimCols, DimRows

      INTEGER cols
      INTEGER objsen
      INTEGER rows
      INTEGER(C_SIZE_T) colname(DimCols)
      INTEGER matbeg(DimCols + 1)
      DOUBLE PRECISION obj(DimCols)
      DOUBLE PRECISION ub(DimCols)
      DOUBLE PRECISION lb(DimCols)
      DOUBLE PRECISION rhs(DimRows)
      CHARACTER*1 sense(DimRows)
      INTEGER matind(DimMatnz)
      DOUBLE PRECISION matval(DimMatnz)

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

      cols = PDNCol
      rows = PDNFila
      objsen = 1

      DO IFila = 1, PDNFila
         rhs (IFila) = LD (IFila)
         sense (IFila) = Sentido (IFila)
      ENDDO
      DO ICol = 1, PDNCol
         ub (ICol) = UppBnd (ICol)
         lb (ICol) = LowBnd (ICol)
         colname (ICol) = loc (PDNombre (ICol))
         obj (ICol) = FO (ICol)
      ENDDO

      NNZero = 0
      CALL Am_flat(A, DEPSILON, matbeg, matind, matval, NNZero)

      CVarPhi = ' '
      CALL Num2Char(IEta, CVarPhi, No, DimLargo)
      probname = fconcat('LP_', CVarPhi)
      probname = fconcat(probname, '_')
      CVarPhi = ' '
      CALL Num2Char(ISimul, CVarPhi, No, DimLargo)
      probname = fconcat(probname, CVarPhi)


      matbeg(cols + 1) = NNZero

      lp = osi_lp_loadproblem(env, probname, cols, rows, objsen,     &
     &     matbeg, matind, matval, lb, ub, obj, sense, rhs, colname)

      vobj = ScalePhi/ScaleObj
      IF (PXNFila .GT. 0 .or. PlaneMaxIter .gt. 0) THEN
         vlb = -DINFTY
         vub = DINFTY
      ELSE
         vlb = 0d0
         vub = 0d0
      ENDIF

      XPDNombre = 'varphi' // CHAR(0)
      CALL osi_lp_addcol(lp, vlb, vub, vobj, XPDNombre)

      cols = cols + 1

      DO IFila = 1, PXNFila
         IF (FSeparaFCF .AND. (PlaCFIndCol(IFila) .NE. ISimul)) THEN
            CYCLE
         ENDIF
         DO ICol = 1, PDLDAcNCol
            GradxPhi(ICol) = PlaCFRho(ICol, IFila)*ScaleVol(ICol)
         ENDDO
         LDPhi = -PlaCFBeta0(IFila)

         CALL AgrResPDi(0, isimul, ieta,                                &
     &        GradxPhi, LDPhi, cols,                                    &
     &        ScalePhi, PDLDAcNCol, PDLDAcColInd,                       &
     &        OptiEPS, OptiMLD, lp, 0)

      ENDDO

      RETURN
      END
