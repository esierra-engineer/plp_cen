!***********************
!     Subrutina agrrespd
!***********************
      SUBROUTINE AgrResPD(IEtapa, EmbVini, ScaleVol,                    &
     &     GradxPhi, LDPhiPrv,                                          &
     &     ScalePhi, PDNumIte, PDNCol, PXNCol,                          &
     &     NSimul, NEtapa, NCenEmb, PDLDAcNCol, PDLDAcColInd,           &
     &     FOnePhi, FSeparaFCF, FSeparaLP, FDepHidEta,                  &
     &     OptiEPS, OptiMLD, lp,                                        &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     Dim, UPlane)
      USE PLP
      USE OSI

      INCLUDE 'machcons.fpp'

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER UPlane
      INTEGER FConvLaja
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_LAJAM) ParLajaM

      INTEGER FConvMaule
      TYPE(PAR_MAULE) ParMaule

      LOGICAL FRestGnl
      TYPE(PAR_GNL) ParGnl

      EXTERNAL DxAEQy
      INTEGER IEtapa
      INTEGER ISimul
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)
      INTEGER NEtapa
      INTEGER NCenEmb
      INTEGER NCol
      INTEGER NSimul
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER PDLDAcNCol
      LOGICAL FSeparaFCF
      LOGICAL FSeparaLP
      LOGICAL FOnePhi
      LOGICAL FDepHidEta(Dim%Eta)
      LOGICAL DxAEQy
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION LDPhiPrv(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION EmbVIni(Dim%Emb, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD
      INTEGER(C_SIZE_T) lpi
      INTEGER IBeg, IEnd
      INTEGER IColx
      INTEGER PDNumIte

      LOGICAL FSeparaCortes
      LOGICAL FLPUnico

      DOUBLE PRECISION value
      INTEGER index
      DOUBLE PRECISION Infinity
      INTEGER IVarPhi
      INTEGER PXNCol(Dim%Eta)
      INTEGER PDNCol(Dim%Eta)

      INTEGER IEtapaOri
      INTEGER IEtapaDest

!
      IF (IEtapa .EQ. NEtapa) RETURN

      FLPUnico = .NOT. FSeparaLP
      FSeparaCortes = (FSeparaLP .AND. FSeparaFCF)                    &
     &     .OR. (FOnePhi .AND. FSeparaFCF)                            &
     &     .OR. (FOnePhi .AND. FDepHidEta(IEtapa))

      DO ISimul = 1, NSimul
         IF (FLPUnico .OR. FSeparaCortes) THEN 
            IBeg = ISimul
            IEnd = ISimul
         ELSE
            IBeg = 1
            IEnd = NSimul
         ENDIF

         lpi = lp(ISimul, IEtapa)
         NCol = osi_lp_getnumcols(lpi)
         IF (FOnePhi) THEN
            IColx = NCol
         ELSE
            IColx = NCol - NSimul + ISimul
         ENDIF

!     
!        Access number of variables in problem.
         IEtapaOri = IEtapa + 1
         IEtapaDest = IEtapa
         CALL AgrResPDv(PDNumIte, ISimul,                               &
     &        IEtapaOri, IEtapaDest,                                    &
     &        GradxPhi(1, ISimul, IEtapaOri),                           &
     &        LDPhiPrv(ISimul, IEtapaOri),                              &
     &        NCenEmb, EmbVIni(1, ISimul, IEtapaOri), ScaleVol,         &
     &        IColx, ScalePhi, PDLDAcNCol, PDLDAcColInd,                &
     &        OptiEPS, OptiMLD,                                         &
     &        lp, IBeg, IEnd,                                           &
     &        FConvLaja, ParLaja, ParLajaM,                             &
     &        FConvMaule, ParMaule,                                     &
     &        FRestGnl, ParGnl,                                         &
     &        Dim, UPlane)
         

         IF (PDNumIte .EQ. 0) THEN
            Infinity = osi_getinfty()
            NCol = PDNCol(IEtapa) + PXNCol(IEtapa)
            
            IF (FOnePhi) THEN
               IBeg = 1
               IEnd = 1
            ELSE IF ((.NOT. FSeparaLP) .OR. (FSeparaLP .AND. FSeparaFCF)) THEN
               IBeg = ISimul
               IEnd = ISimul
            ELSE
               IBeg = 1
               IEnd = NSimul
            ENDIF
            
            DO IVarPhi = IBeg, IEnd
               index = NCol + IVarPhi - 1
               value = Infinity
               CALL osi_lp_setcolupper(lpi, index, value)
               value = -Infinity               
               CALL osi_lp_setcollower(lpi, index, value)
            ENDDO
         ENDIF
      ENDDO
      RETURN
      END


      SUBROUTINE AgrResPDi(PDNumIte, ISimul, ieta,                      &
     &     GradxPhi, LD, IColx,                                         &
     &     ScalePhi, PDLDAcNCol, PDLDAcColInd,                             &
     &     OptiEPS, OptiMLD, lp, UPlane)

      USE OSI

      INCLUDE 'machcons.fpp'

      INTEGER UPlane, PDNumIte, ISimul, ieta
      EXTERNAL DxAEQy
      INTEGER IColx
      INTEGER ICol
      INTEGER IPDLDAcCol
      INTEGER(C_SIZE_T) lp
      INTEGER nzcnt
      INTEGER PDLDAcNCol
      INTEGER PDLDAcColInd(PDLDAcNCol)
      INTEGER rmatind(PDLDAcNCol + 1)
      LOGICAL DxAEQy
      DOUBLE PRECISION GradxPhi(PDLDAcNCol)
      DOUBLE PRECISION LD
      DOUBLE PRECISION rhs
      DOUBLE PRECISION rmatval(PDLDAcNCol + 1)
      DOUBLE PRECISION x
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD

      DOUBLE PRECISION max_rhs
      DOUBLE PRECISION rhso, deps

      DOUBLE PRECISION rowlb, rowub

      rhso = LD/ScalePhi

      deps = ABS(rhso)*OptiEPS + DEPSILON

      nzcnt = 0
      DO IPDLDAcCol = 1, PDLDAcNCol
         ICol = PDLDAcColInd(IPDLDAcCol)
         x = GradxPhi(IPDLDAcCol)/ScalePhi
         IF (.NOT. DxAEQy(x, 0.0d0, deps)) THEN
            nzcnt = nzcnt + 1
            rmatval(nzcnt) = -x
            rmatind(nzcnt) = ICol - 1
         ENDIF
      ENDDO
      nzcnt = nzcnt + 1
      rmatind(nzcnt) = IColx - 1
      rmatval(nzcnt) = 1.0d0
      rhs = LD/ScalePhi
!     
!     Add constraints.
      max_rhs = OptiMLD
      IF (ABS(rhs) .GT. max_rhs) THEN
         rmatval(1:nzcnt) = (max_rhs / ABS(rhs)) * rmatval(1:nzcnt) 
         rhs = (max_rhs / ABS(rhs)) * rhs
      ENDIF
      
      rowlb = rhs
      rowub = osi_getinfty()
      CALL AddPlane(UPlane, PDNumIte, ISimul, ieta, &
     &     lp, nzcnt, rmatind, rmatval, rowlb, rowub)

      RETURN
      END


!
!     Caso general para incluir convenios
!
      SUBROUTINE AgrResPDv(PDNumIte, ISimul,                        &
     &     IEtapaOri, IEtapaDest,                                       &
     &     GradxPhi, LD,                                                &
     &     NCenEmb, VolIni, ScaleVol,                                   &
     &     IColx, ScalePhi, PDLDAcNCol, PDLDAcColInd,                   &
     &     OptiEPS, OptiMLD,                                            &
     &     lp, IBeg, IEnd,                                              &
     &     FConvLaja, ParLaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     FRestGnl, ParGnl,                                            &
     &     Dim, UPlane)

      USE OSI
      USE PLP

      INCLUDE 'machcons.fpp'

      TYPE(PAR_DIMS) Dim

      INTEGER FConvLaja
      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_LAJAM) ParLajaM
      INTEGER FConvMaule
      TYPE(PAR_MAULE) ParMaule
      LOGICAL FRestGnl
      TYPE(PAR_GNL) ParGnl

      EXTERNAL DxAEQy
      INTEGER UPlane, PDNumIte, ISimul
      INTEGER IEtapaOri
      INTEGER IEtapaDest
      INTEGER NCenEmb
      INTEGER IColx
      INTEGER ICol
      INTEGER IPDLDAcCol
      INTEGER(C_SIZE_T) lp(Dim%Simul, Dim%Eta)
      INTEGER(C_SIZE_T) lpi
      INTEGER nzcnt
      INTEGER PDLDAcNCol
      INTEGER PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER rmatind(PDLDAcNCol + 1)
      LOGICAL DxAEQy
      DOUBLE PRECISION GradxPhi(PDLDAcNCol)
      DOUBLE PRECISION LD
      DOUBLE PRECISION VolIni(PDLDAcNCol)
      DOUBLE PRECISION ScaleVol(PDLDAcNCol)

      DOUBLE PRECISION rhs
      DOUBLE PRECISION rmatval(PDLDAcNCol + 1)
      DOUBLE PRECISION x
      DOUBLE PRECISION ScalePhi
      DOUBLE PRECISION OptiEPS
      DOUBLE PRECISION OptiMLD

      DOUBLE PRECISION max_rhs
      DOUBLE PRECISION deps

      DOUBLE PRECISION rowlb, rowub
      DOUBLE PRECISION vini
      
      INTEGER ISimul2
      INTEGER IBeg, IEnd
      
      INTEGER Idx
      INTEGER Ind
      
      LOGICAL IsVarCorteMaule
      LOGICAL IsVarCorteLaja
      LOGICAL IsVarCorteLajaM

      INTEGER I

      deps = ABS(LD/ScalePhi)*OptiEPS + DEPSILON

      nzcnt = 0

      LD = LD
      IPDLDAcCol = 0

      DO Idx = 1, NCenEmb
         IPDLDAcCol = IPDLDAcCol + 1
         ICol = PDLDAcColInd(IPDLDAcCol, IEtapaDest)
         x = GradxPhi(IPDLDAcCol)
         IF (.NOT. DxAEQy(x/ScalePhi, 0.0d0, deps)) THEN
            vini = VolIni(Idx)/ScaleVol(Idx)
            LD = LD - x*vini
            nzcnt = nzcnt + 1
            rmatval(nzcnt) = -x/ScalePhi
            rmatind(nzcnt) = ICol - 1
         ENDIF
      ENDDO

      IF (FConvLaja .eq. 1 .AND. ParLaja%UsaCorteOptim) THEN
         DO Ind = 1, ParLaja%PDLDAcNCol
            IPDLDAcCol = IPDLDAcCol + 1

            IF ( .NOT. IsVarCorteLaja(ISimul, IEtapaOri, ParLaja, Ind)) THEN
               CYCLE
            ENDIF

            ICol = PDLDAcColInd(IPDLDAcCol, IEtapaDest)
            x = GradxPhi(IPDLDAcCol)
            IF (.NOT. DxAEQy(x/ScalePhi, 0.0d0, deps)) THEN
               vini = ParLaja%VarEtaPrev(Ind, ISimul, IEtapaOri)/ParLaja%ScaleVol
               LD = LD - x*vini
               nzcnt = nzcnt + 1
               rmatval(nzcnt) = -x/ScalePhi
               rmatind(nzcnt) = ICol - 1
            ENDIF
         ENDDO
      ENDIF
 
      IF (FConvLaja .eq. 2) THEN
         DO Ind = 1, ParLajaM%PDLDAcNCol
            IPDLDAcCol = IPDLDAcCol + 1

            IF ( .NOT. IsVarCorteLajaM(IEtapaOri, ParLajaM, Ind)) THEN
               CYCLE
            ENDIF

            ICol = PDLDAcColInd(IPDLDAcCol, IEtapaDest)
            x = GradxPhi(IPDLDAcCol)
            IF (.NOT. DxAEQy(x/ScalePhi, 0.0d0, deps)) THEN
               vini = ParLajaM%VarEtaPrev(Ind, ISimul, IEtapaOri)/ParLajaM%ScaleVol
               LD = LD - x*vini
               nzcnt = nzcnt + 1
               rmatval(nzcnt) = -x/ScalePhi
               rmatind(nzcnt) = ICol - 1
            ENDIF
         ENDDO
      ENDIF
     
      IF (FConvMaule .NE. 0 .AND. ParMaule%UsaCorteOptim) THEN
         DO Ind = 1, ParMaule%PDLDAcNCol
            IPDLDAcCol = IPDLDAcCol + 1

            IF ( .NOT. IsVarCorteMaule(ISimul, IEtapaOri, ParMaule, Ind)) THEN
               CYCLE
            ENDIF

            ICol = PDLDAcColInd(IPDLDAcCol, IEtapaDest)
            x = GradxPhi(IPDLDAcCol)
            IF (.NOT. DxAEQy(x/ScalePhi, 0.0d0, deps)) THEN
               vini = ParMaule%VarEtaPrev(Ind, ISimul, IEtapaOri)/parMaule%ScaleVol
               LD = LD - x*vini
               nzcnt = nzcnt + 1
               rmatval(nzcnt) = -x/ScalePhi
               rmatind(nzcnt) = ICol - 1
            ENDIF
         ENDDO
      ENDIF


      Do I = 1, ParGnl%NumTGNL
         IF (FRestGnl .AND. ParGNL%TGNL(I)%UsaCorteOptim) THEN
            DO Ind = 1, ParGnl%TGNL(I)%PDLDAcNCol
               IPDLDAcCol = IPDLDAcCol + 1

               ICol = PDLDAcColInd(IPDLDAcCol, IEtapaDest)
               x = GradxPhi(IPDLDAcCol)
               IF (.NOT. DxAEQy(x/ScalePhi, 0.0d0, deps)) THEN
                  vini = ParGnl%TGNL(I)%VolEtaPrev(ISimul, IEtapaOri)
                  LD = LD - x*vini
                  nzcnt = nzcnt + 1
                  rmatval(nzcnt) = -x/ScalePhi
                  rmatind(nzcnt) = ICol - 1
               ENDIF
            ENDDO
         ENDIF
      ENDDO

      rhs = LD/ScalePhi

      nzcnt = nzcnt + 1
      rmatind(nzcnt) = IColx - 1
      rmatval(nzcnt) = 1.0d0


!     
!     Add constraints.
      max_rhs = OptiMLD
      IF (ABS(rhs) .GT. max_rhs) THEN
         rmatval(1:nzcnt) = (max_rhs / ABS(rhs)) * rmatval(1:nzcnt) 
         rhs = (max_rhs / ABS(rhs)) * rhs
      ENDIF
      
      rowlb = rhs
      rowub = osi_getinfty()
      DO ISimul2 = IBeg, IEnd
         lpi = lp(ISimul2, IEtapaDest)
         CALL AddPlane(UPlane, PDNumIte, ISimul2, IEtapaDest, &
     &        lpi, nzcnt, rmatind, rmatval, rowlb, rowub)
      ENDDO

      RETURN
      END




!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!     Agrega corte de factibilidad
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      SUBROUTINE AgrFact( &
     &     FactEPS, FactMLD, FactDBL, FSeparaCFA, FOneFeasRay,          &
     &     PDNumIte, NSimul, ISimul, IEtapaOri, IEtapaDest,             &  
     &     NCenEmb,                                                     &
     &     FConvLaja, Parlaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     PDLDAcFilaInd, PDLDAcColInd,                                 &
     &     lp, IStat, filename, ULogCF, Dim, UPlane)

      USE PLP
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN)::  Dim


      INTEGER, INTENT(IN):: UPlane, PDNumIte
      INTEGER, INTENT(IN):: FConvLaja
      TYPE(PAR_LAJA), INTENT(IN):: ParLaja
      TYPE(PAR_LAJAM), INTENT(IN):: ParLajaM
      INTEGER, INTENT(IN):: FConvMaule
      TYPE(PAR_MAULE), INTENT(IN):: ParMaule

      INTEGER, INTENT(IN):: NSimul
      INTEGER, INTENT(IN):: NCenEmb
      INTEGER, INTENT(IN):: ULogCF

      LOGICAL, INTENT(IN):: FSeparaCFA
      LOGICAL, INTENT(IN):: FOneFeasRay

      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: IEtapaOri
      INTEGER, INTENT(IN):: IEtapaDest
      INTEGER, INTENT(IN):: PDLDAcFilaInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER, INTENT(IN):: PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)

      DOUBLE PRECISION, INTENT(IN):: FactEPS
      DOUBLE PRECISION, INTENT(IN):: FactMLD
      INTEGER, INTENT(IN):: FactDBL

      CHARACTER*80, INTENT(IN):: filename

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp(Dim%Simul, Dim%Eta)
      INTEGER, INTENT(OUT):: IStat

!     locals
      INTEGER NCol
      INTEGER NFila
      INTEGER(C_SIZE_T) lpi


      lpi = lp(ISimul, IEtapaOri)

      NFila = osi_lp_getnumrows(lpi)
      NCol  = osi_lp_getnumcols(lpi)

      IF (Dim%FactMode .eq. FACT_NONE) THEN
         IStat = 1
         RETURN
      ELSE
         CALL AgrElastici(                                                 &
     &        NFila, NCol, NCenEmb,                                        &
     &        FactEPS, FactMLD, FactDBL, FSeparaCFA, FOneFeasRay,          &
     &        PDNumIte, NSimul, ISimul, IEtapaOri, IEtapaDest,             &  
     &        FConvLaja, Parlaja, ParLajaM,                                &
     &        FConvMaule, ParMaule,                                        &
     &        PDLDAcFilaInd, PDLDAcColInd,                                 &
     &        lp, IStat, ULogCF, filename, Dim, UPlane)         
      ENDIF

      RETURN
      END

!!!!
      SUBROUTINE AgrElastici(                                           &
     &     NFila, NCol, NCenEmb,                                        &
     &     FactEPS, FactMLD, FactDBL, FSeparaCFA, FOneFeasRay,          &
     &     PDNumIte, NSimul, ISimul, IEtapaOri, IEtapaDest,              &  
     &     FConvLaja, Parlaja, ParLajaM,                                &
     &     FConvMaule, ParMaule,                                        &
     &     PDLDAcFilaInd, PDLDAcColInd,                                 &
     &     lp, IStat, ULogCF, filename, Dim, UPlane)

      USE PLP
      USE OSI


      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: UPlane, PDNumIte
      INTEGER, INTENT(IN):: NCenEmb
      INTEGER, INTENT(IN):: NFila
      INTEGER, INTENT(IN):: NCol

      INTEGER, INTENT(IN):: FConvLaja
      TYPE(PAR_LAJA), INTENT(IN):: ParLaja
      TYPE(PAR_LAJAM), INTENT(IN):: ParLajaM
      INTEGER, INTENT(IN):: FConvMaule
      TYPE(PAR_MAULE), INTENT(IN):: ParMaule

      INTEGER, INTENT(IN):: NSimul

      LOGICAL, INTENT(IN):: FSeparaCFA, FOneFeasRay

      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: IEtapaOri
      INTEGER, INTENT(IN):: IEtapaDest
      INTEGER, INTENT(IN):: PDLDAcFilaInd(Dim%PDLDAcCol, Dim%Eta)
      INTEGER, INTENT(IN):: PDLDAcColInd(Dim%PDLDAcCol, Dim%Eta)

      INTEGER, INTENT(IN):: ULogCF
      DOUBLE PRECISION, INTENT(IN):: FactEPS
      DOUBLE PRECISION, INTENT(IN):: FactMLD
      INTEGER, INTENT(IN):: FactDBL
      CHARACTER*80, INTENT(IN):: filename


!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp(Dim%Simul, Dim%Eta)
      INTEGER, INTENT(OUT):: IStat

!     locals

      INTEGER(C_SIZE_T) fname, objs

      INTEGER nzcnt
      INTEGER rmatind(NCol + 1)
      DOUBLE PRECISION rmatval(NCol + 1)


      INTEGER ICol
      INTEGER IFila
      INTEGER Ind

      DOUBLE PRECISION max_rhs

      INTEGER IBeg
      INTEGER IEnd
      INTEGER II

      CHARACTER*12 name

      EXTERNAL DxAEQy
      LOGICAL DxAEQy

      INTEGER(C_SIZE_T) lpi
      INTEGER(C_SIZE_T) lpo

      DOUBLE PRECISION rowlb, rowub


      INTEGER res
      INTEGER IEmb
      INTEGER Idx

      INTEGER rows_ori(NFila)
      INTEGER cols_dest(NCol)
      INTEGER nrows_ori
      DOUBLE PRECISION ray(NFila)
      DOUBLE PRECISION rhsi(NFila + 1)
      DOUBLE PRECISION rhs

      DOUBLE PRECISION deps

      LOGICAL IsVarCorteMaule
      LOGICAL IsVarCorteLaja
      LOGICAL IsVarCorteLajaM
      LOGICAL OneRay

      OneRay = FOneFeasRay

!
      deps = FactEPS + DEPSILON

      lpi = lp(ISimul, IEtapaOri)
      lpo = lp(ISimul, IEtapaDest)

!
!     Filas especiales que contienen las restricciones y variables
!     dependientes de la etapa anterior. Se supone que las variables
!     se acoplan puramente a travez del lado derecho, por lo que sus
!     valores se restan directamente al nuevo rhs
!
      Idx = 0
      DO IEmb = 1, NCenEmb
         IFila = PDLDAcFilaInd(IEmb, IEtapaOri)
         ICol = PDLDAcColInd(IEmb, IEtapaDest)

         Idx = Idx + 1
         rows_ori(Idx) = IFila - 1
         cols_dest(Idx) = ICol - 1
      ENDDO
      
      IF (FConvLaja .EQ. 1) THEN
         DO Ind = 1, ParLaja%DimPDLDAcCol

            IF ( .NOT. IsVarCorteLaja(ISimul, IEtapaOri, ParLaja, Ind)) THEN
               CYCLE
            ENDIF

            IFila = ParLaja%FilIndEta(Ind, IEtapaOri)
            ICol = ParLaja%ColIndEta(Ind, IEtapaDest)

            Idx = Idx + 1
            rows_ori(Idx) = IFila - 1
            cols_dest(Idx) = ICol - 1
         ENDDO
      ENDIF
      IF (FConvLaja .EQ. 2) THEN
         DO Ind = 1, ParLajaM%DimPDLDAcCol

            IF ( .NOT. IsVarCorteLajaM(IEtapaOri, ParLajaM, Ind)) THEN
               CYCLE
            ENDIF

            IFila = ParLajaM%FilIndEta(Ind, IEtapaOri)
            ICol = ParLajaM%ColIndEta(Ind, IEtapaDest)
            Idx = Idx + 1
            rows_ori(Idx) = IFila - 1
            cols_dest(Idx) = ICol - 1
         ENDDO
      ENDIF


      IF (FConvMaule .NE. 0) THEN
         DO Ind = 1, ParMaule%DimPDLDAcCol

            IF ( .NOT. IsVarCorteMaule(ISimul, IEtapaOri, ParMaule, Ind)) THEN
               CYCLE
            ENDIF

            IFila = ParMaule%FilIndEta(Ind, IEtapaOri)
            ICol = ParMaule%ColIndEta(Ind, IEtapaDest)

            Idx = Idx + 1
            rows_ori(Idx) = IFila - 1
            cols_dest(Idx) = ICol - 1
         ENDDO
      ENDIF

      nrows_ori = Idx
      IF (FactDbl .GT. 1) THEN
         fname = loc(filename)
      ELSE
         fname = 0
      ENDIF
      objs = 0
      res = osi_lp_get_feasible_cut(lpi, lpo, nrows_ori, loc(rows_ori),  &
     &     loc(cols_dest), objs, fname, deps, FactDbl, loc(ray), loc(rhsi))

      IF (res .EQ. 0) THEN
!     No se puede encontrar un rayo extremo
         IStat = 1
         RETURN
      ENDIF

      rhs = rhsi(nrows_ori + 1)
      deps = 1.0d-3*FactEPS*ABS(rhs) + DEPSILON
      
      IF (OneRay) THEN
         
         IF (FactDBL .NE. 0) THEN
            WRITE(ULogCF,*) 'Comienza Corte:', ISimul, IEtapaOri
         ENDIF
         
         IStat = 0
         nzcnt = 0
         Idx = 0
         DO IEmb = 1, nrows_ori
            Idx = Idx + 1
            
            IF (DxAEQy(ray(Idx), 0.0d0, deps)) THEN
               CYCLE
            ENDIF
            
            ICol = cols_dest(Idx)
            
            IF (FactDBL .NE. 0) THEN
               name = ' '
               CALL osi_lp_getcolname(lpo, ICol, sizeof(name), loc(name))
               WRITE(ULogCF,*) 'cf: ', name, rows_ori(Idx) + 1, ray(Idx)
            ENDIF
            
            nzcnt = nzcnt + 1
            rmatval(nzcnt) = ray(Idx)
            rmatind(nzcnt) = ICol
         ENDDO
         
         IF (nzcnt .eq. 0) THEN
!     No se puede agregar un corte que mejore la factibilidad del problema
            IStat = -1
            RETURN
         ENDIF
         
         rhs = rhs + FactEPS * ABS(rhs)

         IF (FactDBL  .NE. 0) THEN
            WRITE(ULogCF,*) 'rhsi:', rhs
         ENDIF
         
         
!     Agrega el corte en el LP de la etapa de destino
         
         max_rhs = FactMLD
         IF (ABS(rhs) .GT. max_rhs) THEN
            rmatval(1:nzcnt) = (max_rhs / ABS(rhs)) * rmatval(1:nzcnt) 
            rhs = (max_rhs / ABS(rhs)) * rhs
         ENDIF
         
         
         IF (FSeparaCFA) THEN
            IBeg = ISimul
            IEnd = ISimul
         ELSE
            IBeg = 1
            IEnd = NSimul
         ENDIF
         
         rowlb = rhs
         rowub = osi_getinfty()
         DO II = IBeg, IEnd
            lpo = lp(II, IEtapaDest)
            CALL AddPlane(UPlane, PDNumIte, II, IEtapaDest, &
     &           lpo, nzcnt, rmatind, rmatval, rowlb, rowub)
         ENDDO
      ELSE
         IF (FactDBL .NE. 0) THEN
            WRITE(ULogCF,*) 'Comienzan Cortes:', ISimul, IEtapaOri
         ENDIF
         
         IStat = 0
         nzcnt = 0
         Idx = 0
         DO IEmb = 1, nrows_ori
            
            Idx = Idx + 1
            
            IF (DxAEQy(ray(Idx), 0.0d0, deps)) THEN
               CYCLE
            ENDIF
            
            ICol = cols_dest(Idx)
            
            IF (FactDBL .NE. 0) THEN
               name = ' '
               CALL osi_lp_getcolname(lpo, ICol, sizeof(name), loc(name))
               WRITE(ULogCF,*) 'cf: ', name, rows_ori(Idx) + 1, ray(Idx)
            ENDIF
            
            nzcnt = nzcnt + 1
            rmatval(1) = ray(Idx)
            rmatind(1) = ICol
            
            IF (FSeparaCFA) THEN
               IBeg = ISimul
               IEnd = ISimul
            ELSE
               IBeg = 1
               IEnd = NSimul
            ENDIF

            rhs = rhsi(Idx)

            rhs = rhs + FactEPS * ABS(rhs)
            
            IF (FactDBL  .NE. 0) THEN
               WRITE(ULogCF,*) 'rhsi:', rhs
            ENDIF
            
            rowlb = rhs
            rowub = osi_getinfty()
            DO II = IBeg, IEnd
               lpo = lp(II, IEtapaDest)
               CALL AddPlane(UPlane, PDNumIte, II, IEtapaDest, &
     &              lpo, 1, rmatind, rmatval, rowlb, rowub)
            ENDDO
            
         ENDDO
         
         IF (nzcnt .eq. 0) THEN
!     No se puede agregar un corte que mejore la factibilidad del problema
            IStat = -1
            RETURN
         ENDIF
      ENDIF
         
      RETURN
      END
