!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDLajaMBloA(IEta, IBloque, blodur, etadur,           &
     &     ParLajaM, CenOffset,                                          &
     &     FEmbOffset, COffset, FOffset,                                 &
     &     A, PDNCol, PDNombre)
      USE PLP, ONLY : PAR_DIMS, PAR_LAJAM, DimLargo, No
      USE A_MATRIX

!     Variables Globales
!******************
      INTEGER, INTENT(IN) :: PDNCol
      INTEGER, INTENT(IN) :: IEta
      INTEGER, INTENT(IN) :: CenOffset
      INTEGER, INTENT(IN) :: FEmbOffset
      INTEGER, INTENT(IN) :: IBloque
      TYPE(PAR_LAJAM), INTENT(IN) :: ParLajaM
      DOUBLE PRECISION, INTENT(IN) :: blodur
      DOUBLE PRECISION, INTENT(IN) :: etadur
!     out
      CHARACTER*24, INTENT(OUT) :: PDNombre (PDNCol)
      TYPE(AMatrix), INTENT(OUT) :: A
      INTEGER, INTENT(OUT) :: COffset
      INTEGER, INTENT(OUT) :: FOffset

      
!     Variables Locales
!****************
      CHARACTER*80 fconcat
      CHARACTER*12 Nombre
      CHARACTER*(DimLargo) CVolB
      INTEGER IEmbLaja
      INTEGER FOffseti
      INTEGER Idx
      INTEGER IVol
      INTEGER FCentRiego
      
      IVol = ParLajaM%IEmbLaja
      
      CALL Num2Char(IBloque, CVolB, No, DimLargo)
      
      DO Idx = 1, ParLajaM%NumColBlo
         Nombre = ParLajaM%VarBloNames(Idx)
         Nombre = fconcat('l_', Nombre)
         Nombre = fconcat(Nombre, '_')
         Nombre = fconcat(Nombre, CVolB)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      FOffseti = FOffset
      
!***************
!     Laja
!***************
      ! Particion de caudal de generacion en caudales
      ! de economias y turbinado a cuenta de gastos 
      !
      !    lqgt_j = l_qdr_j + l_qde_j + l_qdm_j + l_qga_j 
      !
      ! donde 
      !       'i' es el indice de la central El Toro
      !       'j' es el indice del bloque
      !
      !       lqg_j  Q Turbinado total
      !       lqdr_j Q Turbinado a cargo de los derechos de riego
      !       lqde_j Q Turbinado a cargo de los derechos de elect
      !       lqdm_j Q Turbinado a cargo de los derechos de mixto
      ! 
      IEmbLaja = CenOffset + ParLajaM%IEmbLaja
      CALL Am_set(A, IEmbLaja, FOffseti + 1, -1.0d0)

      CALL Am_set(A, COffset + ParLajaM%IQDR, FOffseti + 1, 1.0d0)
      CALL Am_set(A, COffset + ParLajaM%IQDE, FOffseti + 1, 1.0d0)
      CALL Am_set(A, COffset + ParLajaM%IQDM, FOffseti + 1, 1.0d0)
      CALL Am_set(A, COffset + ParLajaM%IQGA, FOffseti + 1, 1.0d0)

      FOffseti = FOffseti + 1

!     Agrega el gasto derecho riego al acumulado gasto horario 
      CALL Am_set(A, COffset + ParLajaM%IQDR, &
     &     ParLajaM%FilIndEta(ParLajaM%IQDRH_F, IEta), -blodur/etadur)

!     Agrega el gasto derecho electrico al acumulado gasto horario 
      CALL Am_set(A, COffset + ParLajaM%IQDE, &
     &     ParLajaM%FilIndEta(ParLajaM%IQDEH_F, IEta), -blodur/etadur)

!     Agrega el uso de economias al acumulado horario 
      CALL Am_set(A, COffset + ParLajaM%IQDM, &
     &     ParLajaM%FilIndEta(ParLajaM%IQDMH_F, IEta), -blodur/etadur)

!     Agrega el uso de economias al acumulado horario 
      CALL Am_set(A, COffset + ParLajaM%IQGA, &
     &     ParLajaM%FilIndEta(ParLajaM%IQGAH_F, IEta), -blodur/etadur)

!     Agrega el caudal turbinado al acumulado horario
      CALL Am_set(A, IEmbLaja, &
     &     ParLajaM%FilIndEta(ParLajaM%IQGTH_F, IEta), -blodur/etadur)


!     Agrega caudal de riego extraido a la ecuacion de la central de riego
      DO Idx = 1, ParLajaM%NumRetRiego
         FCentRiego = FEmbOffset + ParLajaM%ICenRetRiego(Idx)
         CALL Am_set(A, COffset + ParLajaM%IQRI(Idx), FCentRiego, 1.0d0)

         IF (ParLajaM%ICenInyRiego(Idx) .gt. 0) then
            FCentRiego = FEmbOffset + ParLajaM%ICenInyRiego(Idx)
            CALL Am_set(A, COffset + ParLajaM%IQRI(Idx), FCentRiego, -1.0d0)
         ENDIF
            

         CALL Am_set(A, COffset + ParLajaM%IQRI(Idx),          &
     &        ParLajaM%FilIndEta(ParLajaM%IQRIHF(Idx), IEta),  &
     &        -blodur/etadur)
      ENDDO

!***************
!     Offsets Finales
!***************
      COffset = COffset + ParLajaM%NumColBlo
      FOffset = FOffset + ParLajaM%NumFilBlo
      
      RETURN
      END


!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDLajaMBloFO(IEta, ParLajaM,                        &
     &     BloDur, FPhi,                                                &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE PLP, ONLY : PAR_DIMS, PAR_LAJAM

      USE OSI

!     Variables Globales
!******************
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION FPhi

      INTEGER PDNCol
      INTEGER COffset
      INTEGER IEta
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      TYPE(PAR_LAJAM) ParLajaM
      !
      INTEGER I

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + ParLajaM%NumColBlo) = 0

!     costos variables para caudales de generacion que permiten
!     ordenarlos segun preferencias de operacion
      DO I = 1, ParLajaM%IQGA
         FO(COffset + I) = ParLajaM%CQVarEta(I, IEta) * BloDur/FPhi
      ENDDO

 
!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + 1 : COffset + ParLajaM%NumColBlo) = DINFTY
      LowBnd(COffset + 1 : COffset + ParLajaM%NumColBlo) = 0
      
      RETURN
      END

!**********************************


      SUBROUTINE GenPDLajaMEtaA(IEta, ParLajaM, etadur,                 &
     &     FiltVarColInd, COffset, FOffset,                             &
     &     A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
      USE PLP
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!******************
      INTEGER PDNCol, PDNFila
      TYPE(PAR_LAJAM) ParLajaM
      CHARACTER*24 PDNombre (PDNCol)
      INTEGER IEta
      INTEGER COffset
      INTEGER FOffset
      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)
      CHARACTER*1 Sentido(PDNFila)
      
      DOUBLE PRECISION etadur
      TYPE(AMatrix) A

!     Variables Locales
!****************
      CHARACTER*12 Nombre
      CHARACTER*80 fconcat

      INTEGER Idx
      INTEGER IVol
      INTEGER FiltVarIndLajaM
      DOUBLE PRECISION etadursc

      IVol = ParLajaM%IEmbLaja
     
      DO Idx = 1, ParLajaM%NumColEta
         Nombre = ParLajaM%VarEtaNames(Idx)
         Nombre = fconcat('l_', Nombre)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      DO Idx = 1, ParLajaM%NumColEta
         ParLajaM%ColIndEta(Idx, IEta) = COffset + Idx
      ENDDO
      
      DO Idx = 1, ParLajaM%NumFilEta
         ParLajaM%FIlIndEta(Idx, IEta) = FOffset + Idx
      ENDDO

      etadursc = etadur/ParLajaM%ScaleVol
      ! Gasto anual derechos riego
      CALL Am_set(A, COffset + ParLajaM%IQDRH, &
     &     FOffset + ParLajaM%IVDRF_F, etadursc)
      CALL Am_set(A, COffset + ParLajaM%IVDRF, &
     &     FOffset + ParLajaM%IVDRF_F, 1.0d0)

      IF (IEta .GT. 1) THEN
         IF (ParLajaM%TipoEtaGM(IEta) .EQ. INICIOTEMP) THEN
            CALL Am_set(A, COffset + ParLajaM%IVGAF, &
     &           FOffset + ParLajaM%IVDRF_F, 1.0d0)
         ENDIF
      ENDIF

      ! Gasto anual derechos electrico
      CALL Am_set(A, COffset + ParLajaM%IQDEH, &
     &     FOffset + ParLajaM%IVDEF_F, etadursc)      
      CALL Am_set(A, COffset + ParLajaM%IVDEF, &
     &     FOffset + ParLajaM%IVDEF_F, 1.0d0)

      ! Gasto anual derechos mixto
      CALL Am_set(A, COffset + ParLajaM%IQDMH, &
     &     FOffset + ParLajaM%IVDMF_F, etadursc)
      CALL Am_set(A, COffset + ParLajaM%IVDMF, &
     &     FOffset + ParLajaM%IVDMF_F, 1.0d0)
      
      ! Gasto anual anticipado
      CALL Am_set(A, COffset + ParLajaM%IQGAH, &
     &     FOffset + ParLajaM%IVGAF_F, -etadursc)
      CALL Am_set(A, COffset + ParLajaM%IVGAF, &
     &     FOffset + ParLajaM%IVGAF_F, 1.0d0)

!     balance gasto derecho
      IF (.false.) then
         Sentido(FOffset + ParLajaM%IVGAF_F) = 'E'
      endif

      !  balance de riego
      CALL Am_set(A, COffset + ParLajaM%IQRS, FOffset + ParLajaM%IQRSB_F,  1d0)
      CALL Am_set(A, COffset + ParLajaM%IQPR, FOffset + ParLajaM%IQRSB_F, -1d0)
      CALL Am_set(A, COffset + ParLajaM%IQNR, FOffset + ParLajaM%IQRSB_F, -1d0)
      CALL Am_set(A, COffset + ParLajaM%IQER, FOffset + ParLajaM%IQRSB_F, -1d0)
      CALL Am_set(A, COffset + ParLajaM%IQSR, FOffset + ParLajaM%IQRSB_F, -1d0)

!     caudal total del laja
      CALL Am_set(A, COffset + ParLajaM%IQLAJA, FOffset + ParLajaM%IQLAJA_F, 1d0)
      CALL Am_set(A, COffset + ParLajaM%IQGTH, FOffset + ParLajaM%IQLAJA_F, -1d0)

      IF (ParLajaM%IFiltLaja .GT. 0) THEN
         FiltVarIndLajaM = FiltVarColInd(ParLajaM%IFiltLaja, IEta)
         IF (FiltVarIndLajaM .GT. 0) THEN
            CALL Am_set(A, FiltVarIndLajaM, FOffset + ParLajaM%IQLAJA_F, -1d0)
         ENDIF
      ENDIF         

      ! Gastos horarios
      CALL Am_set(A, COffset + ParLajaM%IQDRH, FOffset + ParLajaM%IQDRH_F, 1d0)
      CALL Am_set(A, COffset + ParLajaM%IQDEH, FOffset + ParLajaM%IQDEH_F, 1d0)
      CALL Am_set(A, COffset + ParLajaM%IQDMH, FOffset + ParLajaM%IQDMH_F, 1d0)
      CALL Am_set(A, COffset + ParLajaM%IQGAH, FOffset + ParLajaM%IQGAH_F, 1d0)
      CALL Am_set(A, COffset + ParLajaM%IQGTH, FOffset + ParLajaM%IQGTH_F, 1d0)

! retiro maximo de riego
      IF (IEta .GT. ParLajaM%NumCaudalToro) THEN      
         CALL Am_set(A, COffset + ParLajaM%IQDRH, FOffset + ParLajaM%IQDRT_F, 1d0)
         CALL Am_set(A, COffset + ParLajaM%IQDMH, FOffset + ParLajaM%IQDRT_F, 1d0)
         CALL Am_set(A, COffset + ParLajaM%IQGAH, FOffset + ParLajaM%IQDRT_F, 1d0)
         CALL Am_set(A, COffset + ParLajaM%IQDEFM, FOffset + ParLajaM%IQDRT_F, -1d0)
         Sentido(FOffset + ParLajaM%IQDRT_F) = 'L'
      ENDIF
      

      ! Agrega caudal de riego extraido 
      DO Idx = 1, ParLajaM%NumRetRiego
         CALL Am_set(A, COffset + ParLajaM%IQRIHC(Idx), FOffset + ParLajaM%IQRIHF(Idx), 1.0d0)
         CALL Am_set(A, COffset + ParLajaM%IQRIHC(Idx), FOffset + ParLajaM%IQRFHF(Idx), 1.0d0)
         CALL Am_set(A, COffset + ParLajaM%IQRFHC(Idx), FOffset + ParLajaM%IQRFHF(Idx), 1.0d0)
         CALL Am_set(A, COffset + ParLajaM%IQRDHC(Idx), FOffset + ParLajaM%IQRFHF(Idx), -1.0d0)
         CALL Am_set(A, COffset + ParLajaM%IQRDHC(Idx), FOffset + ParLajaM%IQRDHF(Idx), 1.0d0)
         CALL Am_set(A, COffset + ParLajaM%IQPR, FOffset + ParLajaM%IQRDHF(Idx), -ParLajaM%FRiegoPrim(Idx))
         CALL Am_set(A, COffset + ParLajaM%IQNR, FOffset + ParLajaM%IQRDHF(Idx), -ParLajaM%FRiegoNuev(Idx))
         CALL Am_set(A, COffset + ParLajaM%IQER, FOffset + ParLajaM%IQRDHF(Idx), -ParLajaM%FRiegoEmer(Idx))
         CALL Am_set(A, COffset + ParLajaM%IQSR, FOffset + ParLajaM%IQRDHF(Idx), -ParLajaM%FRiegoSalt(Idx))
      ENDDO

      RETURN
      END

!******************
      SUBROUTINE GenPDLajaMEtaFO(IEta, ParLajaM, edur, FPhi,     &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)
      USE PLP, ONLY : PAR_DIMS, PAR_LAJAM
      USE OSI

!     Variables Globales
!******************
      TYPE(PAR_LAJAM) ParLajaM

      DOUBLE PRECISION FPhi

      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION edur
      INTEGER IEta      
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

      INTEGER Idx
      INTEGER IVol

      DOUBLE PRECISION FVol 
      DOUBLE PRECISION FCau

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      IVol = ParLajaM%IEmbLaja

!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + ParLajaM%NumColEta) = 0

      FVol = ParLajaM%ScaleVol / FPhi
      FCau = edur / FPhi


      DO Idx=1, ParLajaM%NumRetRiego
         FO(COffset + ParLajaM%IQRFHC(Idx)) = ParLajaM%CRiegoNSEta(IEta) &
     &        *  FCau * ParLajaM%FRiegoCost(Idx)
      ENDDO

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + 1 : COffset + ParLajaM%NumColEta) = DINFTY
      LowBnd(COffset + 1 : COffset + ParLajaM%NumColEta) = 0

      RETURN
      END

!*********************

      DOUBLE PRECISION FUNCTION QAfluEtaM(IEta, NBloque, BloInd, BloDur, &
     &     EstocRHSP, IClaseFila, IAfl, Dim)     
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER IEta
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      INTEGER IAfl
!
      INTEGER IBind
      INTEGER IBlo
      DOUBLE PRECISION DSum

      QAfluEtaM = 0
      DSum = 0
      DO IBlo = 1, NBloque(IEta)
         IBInd = BloInd(IBlo, IEta)
         DSum = DSum + BloDur(IBInd) 
         QAfluEtaM = QAfluEtaM +                                          &
     &        BloDur(IBInd)*EstocRHSP(IAfl, IBInd, IClaseFila(IAfl))
      ENDDO
      QAfluEtaM = QAfluEtaM/DSum

      RETURN
      END

!*******************

      SUBROUTINE GetQsLajaM(IEta, NBloque, BloInd, BloDur, Mes,         &
     &     EstocRHSP, IClaseFila, ParLajaM,                             &
     &     QHoyaInter, QFiltLaja,                                       &
     &     QPRiego, QNRiego, QSRiego, QERiego,                          &      
     &     QDefAbanico, QDefTucapel, QTLajaMin,                         &
     &     Dim)

      USE PLP, ONLY : PAR_DIMS, PAR_LAJAM
      TYPE(PAR_DIMS), INTENT(IN)::  Dim


      INTEGER IEta
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER Mes(Dim%Eta)
      INTEGER IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      TYPE(PAR_LAJAM) ParLajaM

      DOUBLE PRECISION QHoyaInter
      DOUBLE PRECISION QFiltLaja
      DOUBLE PRECISION QDefAbanico
      DOUBLE PRECISION QDefTucapel
      DOUBLE PRECISION QTLajaMin
      DOUBLE PRECISION QPRiego
      DOUBLE PRECISION QNRiego
      DOUBLE PRECISION QSRiego
      DOUBLE PRECISION QERiego

!
      DOUBLE PRECISION QAfluEtaM
      DOUBLE PRECISION friego_mes
      INTEGER IAfl


      QHoyaInter = 0
      DO IAfl = 1, ParLajaM%NumAflHoyaInt         
         IF (ParLajaM%IAflHoyaInt(IAfl) .GT. 0) THEN
            QHoyaInter = QHoyaInter + QAfluEtaM(IEta, NBloque, BloInd,   &
     &           BloDur, EstocRHSP, IClaseFila,                          & 
     &           ParLajaM%IAflHoyaInt(IAfl), Dim)
         ENDIF
      ENDDO

      friego_mes = ParLajaM%FactMenRiegoPrim(Mes(IEta))
      QPRiego = friego_mes*ParLajaM%CaudalDefPrim

      friego_mes = ParLajaM%FactMenRiegoNuev(Mes(IEta))
      QNRiego = friego_mes*ParLajaM%CaudalDefNuev

      friego_mes = ParLajaM%FactMenRiegoSalt(Mes(IEta))
      QSRiego = friego_mes*ParLajaM%CaudalDefSalt

      friego_mes = ParLajaM%FactMenRiegoEmer(Mes(IEta))
      QERiego = friego_mes*ParLajaM%CaudalDefEmer
      
!
!     Deficits
!
      QDefAbanico = ParLajaM%QFiltHist - QFiltLaja
      QDefTucapel = QPRiego - QHoyaInter - QFiltLaja
      QTLajaMin = MIN(QDefTucapel, QDefAbanico)
      
      QPRiego = QTLajaMin + QHoyaInter + QFiltLaja
      
      QTLajaMin = QTLajaMin + QNRiego + QSRiego + QERiego

      
      RETURN
      END

!***************************************

      SUBROUTINE FijaLajaM(IEta,                                        &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, FactTiempo,            &
     &     EstocRHSP, IClaseFila,                                       &
     &     ParLajaM,ParRes, VolIni, NumEmb, FiltVals,                          &
     &     ISimul,FRestReserva,                                                      &
     &     lp, Dim)
      USE PLP
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INTEGER, INTENT(IN) :: IEta
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      INTEGER, INTENT(IN) :: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: FactTiempo
      INTEGER, INTENT(IN) :: Mes(Dim%Eta)

      TYPE(PAR_LAJAM), INTENT(INOUT) :: ParLajaM
      TYPE(PAR_RESERVA) ParRes
      LOGICAL FRestReserva
      INTEGER, INTENT(IN) :: NumEmb
      DOUBLE PRECISION, INTENT(IN):: VolIni(NumEmb)
      INTEGER, INTENT(IN) :: ISimul

      INTEGER, INTENT(IN) :: IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: EtaDur(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: FiltVals(Dim%EmbFilt)
      
!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp


!     locals
      DOUBLE PRECISION VolLajaM
      DOUBLE PRECISION VolUtil
      DOUBLE PRECISION VarLajaMPrev(ParLajaM%NumColEta)
      DOUBLE PRECISION LDResLajaM(ParLajaM%NumFilEta)
      DOUBLE PRECISION VarLajaM(ParLajaM%NumColEta)
      INTEGER Idx
      INTEGER IdxU
      INTEGER IdxL
      INTEGER IdxE
      DOUBLE PRECISION QHoyaInter
      DOUBLE PRECISION QDefAbanico
      DOUBLE PRECISION QDefTucapel
      DOUBLE PRECISION QTLajaMin
      DOUBLE PRECISION Caudal2Vol

      INTEGER IVol

      INTEGER MCUppInd (ParLajaM%NumColEta)
      DOUBLE PRECISION MCUppVal (ParLajaM%NumColEta)
      INTEGER MCLowInd (ParLajaM%NumColEta)
      DOUBLE PRECISION MCLowVal (ParLajaM%NumColEta)

      INTEGER I
      INTEGER ColchonActivo
      DOUBLE PRECISION VolIniColchon

      DOUBLE PRECISION DerRiego, DerElect, DerMixto
      DOUBLE PRECISION VolPrevio, VolColchon, VolIncrem      

      DOUBLE PRECISION QPRiego
      DOUBLE PRECISION QNRiego
      DOUBLE PRECISION QSRiego
      DOUBLE PRECISION QERiego
      
      DOUBLE PRECISION DINFTY
      DOUBLE PRECISION FMixto
      DOUBLE PRECISION QFiltLaja
      
      DINFTY = osi_getinfty()

!     LOGICAL Regla50cmActiva

      VarLajaM(1: ParLajaM%NumColEta) = 0
      LDResLajaM(1: ParLajaM%NumFilEta) = 0
!
!     Obtiene el valor de las variables previas del laja, que pueden
!     resultar de las condiciones iniciales o de la etapa previa 
!     
      CALL GetVarLajaMPrev(IEta, ISimul, ParLajaM, VarLajaMPrev)
!
!     Las restricciones de economias, gastos y derechos asumen en valor
!     de las variables de la etapa previa, que se consideran como valor
!     inicial
!
      LDResLajaM(ParLajaM%IVDRF_F) = VarLajaMPrev(ParLajaM%IVDRF)
      LDResLajaM(ParLajaM%IVDEF_F) = VarLajaMPrev(ParLajaM%IVDEF)
      LDResLajaM(ParLajaM%IVDMF_F) = VarLajaMPrev(ParLajaM%IVDMF)
      LDResLajaM(ParLajaM%IVGAF_F) = VarLajaMPrev(ParLajaM%IVGAF)

!
!     Volumen y filtraciones actuales y utiles del laja
!
      IVol = ParLajaM%IEmbLaja
      VolLajaM = VolIni(IVol)
      VolUtil = VolLajaM - ParLajaM%VolMuerto

      ColchonActivo = 0
      VolIniColchon = 0
      DO I = 1, ParLajaM%NumColchon
         IF (VolUtil .ge. VolIniColchon .and.                           &
     &        (VolUtil - VolIniColchon) .lt. ParLajaM%VolColchon(I)) THEN
            ColchonActivo = I
            EXIT
         ELSE
            VolIniColchon = VolIniColchon + ParLajaM%VolColchon(I)
         ENDIF
      ENDDO

      IF (ColchonActivo .eq. 0) THEN
         IF (VolUtil .lt. 0) THEN
            ColchonActivo = 1
         ELSE
            ColchonActivo = ParLajaM%NumColchon
         ENDIF
!         WRITE (ULog, *) 'lajam: colchon activo igual a 0', VolLajaM, ColchonActivo
      ENDIF
      
      ParLajaM%ColchonActivo(ISimul, IEta) = ColchonActivo
!
!     Calcula flujos relevantes 
!
      QFiltLaja = 0
      IF (ParLajaM%IFiltLaja .GT. 0) THEN
         QFiltLaja = FiltVals(ParLajaM%IFiltLaja)
      ENDIF
      
      Caudal2Vol = FactTiempo * EtaDur(IEta)
      CALL GetQsLajaM(IEta, NBloque, BloInd, BloDur, Mes,               &
     &     EstocRHSP, IClaseFila, ParLajaM,                             &
     &     QHoyaInter, QFiltLaja,                                       &
     &     QPRiego, QNRiego, QSRiego, QERiego,                          &      
     &     QDefAbanico, QDefTucapel, QTLajaMin,                         &
     &     Dim)

      
      DO Idx=1, ParLajaM%NumRetRiego
         LDResLajaM(ParLajaM%IQRDHF(Idx)) = ParLajaM%DRExtra(IEta, Idx)
      ENDDO     
     
!
!     Consideraciones para inicio de mes y de agno
!
      IF (IEta .GT. 1) THEN
         IF (ParLajaM%TipoEtaGM(IEta) .EQ. INICIOTEMP) THEN
!     Reiniciamos contador de los derechos de temporada
               
            DerRiego = ParLajaM%DerRiegoBase
            DerElect = ParLajaM%DerElectBase
            DerMixto = 0d0
            
            VolPrevio = 0d0
            DO I = 1, ColchonActivo
               VolColchon = ParLajaM%VolColchon(I)
               IF (I .eq. ColchonActivo) THEN
                  VolIncrem = VolUtil - VolPrevio
               ELSE
                  VolIncrem = VolColchon
               ENDIF
               DerRiego = DerRiego + &
     &              ParLajaM%FactDerRiegoColchon(I)*VolIncrem
               DerElect = DerElect + &
     &              ParLajaM%FactDerElectColchon(I)*VolIncrem
               VolPrevio = VolPrevio + VolColchon

            ENDDO
            FMixto = ParLajaM%FactDerMixtoColchon(ColchonActivo)
            DerMixto = DerMixto + ParLajaM%DerMixtoBase*FMixto
            DerRiego = DerRiego + ParLajaM%DerMixtoBase*(1d0 - FMixto)
            
            LDResLajaM(ParLajaM%IVDRF_F) = MIN(DerRiego, ParLajaM%DerRiegoMax)
            LDResLajaM(ParLajaM%IVDEF_F) = MIN(DerElect, ParLajaM%DerElectMax)
            LDResLajaM(ParLajaM%IVDMF_F) = MIN(DerMixto, ParLajaM%DerMixtoMax)
         ENDIF

         IF (ParLajaM%TipoEtaGM(IEta) .EQ. INICIOANTIC) THEN
!     Reiniciamos contador de gastos de anticipo de riego
               
            LDResLajaM(ParLajaM%IVGAF_F) = 0.0d0
         ENDIF
         
      ENDIF
      
      IF (IEta .LE. ParLajaM%NumRetiros) THEN
         Idx = IEta
         VarLajaM(ParLajaM%IQPR) = ParLajaM%RetPrimReg(Idx)
         VarLajaM(ParLajaM%IQNR) = ParLajaM%RetNuevReg(Idx)
         VarLajaM(ParLajaM%IQER) = ParLajaM%RetEmerReg(Idx)
         VarLajaM(ParLajaM%IQSR) = ParLajaM%RetSaltReg(Idx)
      ELSE
         VarLajaM(ParLajaM%IQPR) = QPRiego
         VarLajaM(ParLajaM%IQNR) = QNRiego
         VarLajaM(ParLajaM%IQER) = QERiego
         VarLajaM(ParLajaM%IQSR) = QSRiego
      ENDIF

 
!
!     Regla para definir el caudal minimo turbinado en LajaM para cumplir
!     con los convenios de riego
!
      VarLajaM(ParLajaM%IQDEFM) = MAX(QTLajaMin, 0.0d0)
      VarLajaM(ParLajaM%IQHI) = QHoyaInter
      
!
!     Upper boundaries simples
!
      IdxU = 0
      ! Gasto de derechos de riego
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQDRH, IEta)
      MCUppVal(IdxU) = ParLajaM%CaudalMaxRiego*ParLajaM%FactMenMaxRiego(Mes(IEta))

      ! Gasto de derechos electrico
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQDEH, IEta)
      MCUppVal(IdxU) = ParLajaM%CaudalMaxElect*ParLajaM%FactMenMaxElect(Mes(IEta))
      
      ! Gasto de derechos mixto
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQDMH, IEta)
      MCUppVal(IdxU) = ParLajaM%CaudalMaxMixto*ParLajaM%FactMenMaxMixto(Mes(IEta))
      
      ! Gasto de derechos mixto
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQGAH, IEta)
      MCUppVal(IdxU) = ParLajaM%CaudalMaxAntic*ParLajaM%FactMenMaxAntic(Mes(IEta))
      IF (FRestReserva) THEN ! Las restricciones son horarias, así que se multiplica por la duración de la etapa la cota
         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParRes%ColIndEta(IEta,ParRes%TQRPR)
         MCUppVal(IdxU) = ParLajaM%CaudalMaxRiego*ParLajaM%FactMenMaxRiego(Mes(IEta))*EtaDur(IEta)
         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParRes%ColIndEta(IEta,ParRes%TQRPE)
         MCUppVal(IdxU) = ParLajaM%CaudalMaxElect*ParLajaM%FactMenMaxElect(Mes(IEta))*EtaDur(IEta)

         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParRes%ColIndEta(IEta,ParRes%TQRPM)
         MCUppVal(IdxU) = ParLajaM%CaudalMaxMixto*ParLajaM%FactMenMaxMixto(Mes(IEta))*EtaDur(IEta)

         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParRes%ColIndEta(IEta,ParRes%TQRNR)
         MCUppVal(IdxU) = ParLajaM%CaudalMaxRiego*ParLajaM%FactMenMaxRiego(Mes(IEta))*EtaDur(IEta)

         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParRes%ColIndEta(IEta,ParRes%TQRNE)
         MCUppVal(IdxU) = ParLajaM%CaudalMaxElect*ParLajaM%FactMenMaxElect(Mes(IEta))*EtaDur(IEta)

         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParRes%ColIndEta(IEta,ParRes%TQRNM)
         MCUppVal(IdxU) = ParLajaM%CaudalMaxMixto*ParLajaM%FactMenMaxMixto(Mes(IEta))*EtaDur(IEta)
      ENDIF
!     
!     Asigna igualdades
!
      IdxE = IdxU

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQDEFM, IEta)
      MCUppVal(IdxU) = VarLajaM(ParLajaM%IQDEFM) 

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQHI, IEta)
      MCUppVal(IdxU) = VarLajaM(ParLajaM%IQHI) 

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQPR, IEta)
      MCUppVal(IdxU) = VarLajaM(ParLajaM%IQPR) 

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQNR, IEta)
      MCUppVal(IdxU) = VarLajaM(ParLajaM%IQNR) 

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQER, IEta)
      MCUppVal(IdxU) = VarLajaM(ParLajaM%IQER) 

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQSR, IEta)
      MCUppVal(IdxU) = VarLajaM(ParLajaM%IQSR) 

      IF (IEta .LE. ParLajaM%NumCaudalToro) THEN
         IdxU = IdxU + 1
         Idx = IEta
         MCUppInd(IdxU) = ParLajaM%ColIndEta(ParLajaM%IQGTH, IEta)
         MCUppVal(IdxU) = ParLajaM%CaudalToro(Idx)
      ENDIF
      
!     
!     Low boundaries simples
!
      IdxL = 0
      

!
!     Asigna igualdades
!
      DO Idx = 1, IdxU - IdxE
         IdxL = IdxL + 1
         MCLowInd(IdxL) = MCUppInd(IdxE + Idx)
         MCLowVal(IdxL) = MCUppVal(IdxE + Idx)
      ENDDO
         
!
!     Llama a cplex para setear los lados derechos de las restricciones

!     Escala las restricciones de volumenes
      DO Idx = 1, ParLajaM%NumResVol
         LDResLajaM(Idx) = LDResLajaM(Idx) / ParLajaM%ScaleVol
      ENDDO

      CALL ModifBrdRhs(ParLajaM%NumFilEta,                 &
     &     ParLajaM%FilIndEta(1, IEta), LDResLajaM,         &
     &     lp)

      CALL ModifUpp(IdxU, MCUppInd,  MCUppVal, lp)
      CALL ModifLow(IdxL, MCLowInd,  MCLowVal, lp)

      RETURN
      END

!***************************

      SUBROUTINE GetVarLajaMPrev(IEtapa, ISimul, ParLajaM, VarLajaMPrev)
      USE PLP

      TYPE(PAR_LAJAM), INTENT(IN) :: ParLajaM

      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul

      DOUBLE PRECISION, INTENT(OUT) :: VarLajaMPrev(ParLajaM%NumColEta)
!
      INTEGER IVol
!
      IVol = ParLajaM%IEmbLaja

      IF (IEtapa .EQ. 1) THEN
         VarLajaMPrev(1:ParLajaM%NumColEta) = 0
!        Usamos valores de archivo. Se supone que no hay que resolver si
!        estamos al principo de un mes o ano, esto viene implicito en el
!        valor inicial

         VarLajaMPrev(ParLajaM%IVDRF) = ParLajaM%DerRiegoIni
         VarLajaMPrev(ParLajaM%IVDEF) = ParLajaM%DerElectIni
         VarLajaMPrev(ParLajaM%IVDMF) = ParLajaM%DerMixtoIni
         VarLajaMPrev(ParLajaM%IVGAF) = ParLajaM%GasAnticIni
         
         
         RETURN
      ENDIF

      VarLajaMPrev(1:ParLajaM%NumColEta) =                                &
     &     ParLajaM%VarEtaPrev(1:ParLajaM%NumColEta, ISimul, IEtapa)
      
      RETURN
      END

!******************

!******************
      SUBROUTINE AgrFactLajaM(NFila, NCol,                              &
     &     IEtapaOri, IEtapaDest, deps,  ParLajaM,                      &  
     &     nzcnt, rmatval, rmatind, pi, LD, rhs,                        &
     &     lpi, FactDBL, ULogCF)

      USE PLP
      USE OSI
      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: NFila
      INTEGER, INTENT(IN):: NCol

      TYPE(PAR_LAJAM) ParLajaM

      INTEGER IEtapaOri
      INTEGER IEtapaDest

      INTEGER(C_SIZE_T) lpi

      EXTERNAL DxAEQy
      INTEGER nzcnt
      INTEGER rmatind(NCol)
      LOGICAL DxAEQy
      DOUBLE PRECISION rmatval(NCol)
      DOUBLE PRECISION LD(NFila)
      DOUBLE PRECISION pi(NFila)
      DOUBLE PRECISION rhs
      DOUBLE PRECISION deps

      INTEGER ICol
      INTEGER IFila
      INTEGER Ind

      INTEGER ULogCF
      INTEGER FactDBL

      CHARACTER*12 name
      LOGICAL IsVarCorteLajaM


      DO Ind = 1, ParLajaM%NumVarEst
         IF ( .NOT. IsVarCorteLajaM(IEtapaOri, ParLajaM, Ind)) THEN
            CYCLE
         ENDIF

         IFila = ParLajaM%FilIndEta(Ind, IEtapaOri)
         IF (DxAEQy(pi(IFila), 0.0d0, deps)) THEN
            CYCLE
         ENDIF

         rhs = rhs - pi(IFila)*LD(IFila)
            
         IF (FactDBL .NE. 0) THEN
            ICol = ParLajaM%ColIndEta(Ind, IEtapaOri)
            name = ' '
            CALL osi_lp_getcolname(lpi, ICol - 1, sizeof(name), loc(name))
            WRITE(ULogCF,*) 'cfl: ', name, IFila, pi(IFila), LD(IFila)
         ENDIF
         
         ICol = ParLajaM%ColIndEta(Ind, IEtapaDest)
         nzcnt = nzcnt + 1
         rmatval(nzcnt) = -pi(IFila)
         rmatind(nzcnt) = ICol  - 1
      ENDDO

      RETURN
      END

!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDLajaMN(NArcNom, ISimul,                         & 
     &     NBloque, BloDur, BloEta, TipoEtapa, Mes,                      &
     &     EmbFEsc, ParLajaM, Dim, ULog)
!
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim


!
      TYPE(PAR_LAJAM) ParLajaM
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      INTEGER Mes(Dim%Eta)      
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER Abrir
      INTEGER IEta
      INTEGER IBlo
      INTEGER ISimul
      INTEGER NBloque
      INTEGER BloEta(Dim%Blo)
      INTEGER UWrite
      INTEGER ICen
      DOUBLE PRECISION VolScale
      DOUBLE PRECISION Time
      INTEGER Idx
      INTEGER ULog
      INTEGER ColchonActivo
!
      
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A, '','', $)') 'Hidro'
         WRITE(UWrite, '(A, '','', $)') 'Bloque'         
         WRITE(UWrite, '(A, '','', $)') 'Etapa'
         WRITE(UWrite, '(A, '','', $)') 'Mes'
         WRITE(UWrite, '(A, '','', $)') 'TipoEtapa'
         WRITE(UWrite, '(A, '','', $)') 'TipoEtaGM'         
         WRITE(UWrite, '(A, '','', $)') 'HorasAcum'
         WRITE(UWrite, '(A, '','', $)') 'EmbFac'
         WRITE(UWrite, '(A, '','', $)') 'Colchon'
         DO Idx = 1, ParLajaM%NumColEta
            WRITE(UWrite, '(A, '', '', $)') ParLajaM%VarEtaNames(Idx)
         ENDDO
         DO Idx = 1, ParLajaM%NumColBlo
            WRITE(UWrite, '(A, '', '', $)') ParLajaM%VarBloNames(Idx)
         ENDDO
         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      ICen = ParLajaM%IEmbLaja
      VolScale = 1D3/EmbFEsc(ICen)
      VolScale = 1D3/1d6
      Time = 0
      DO IBlo = 1, NBloque
         IEta = BloEta(IBlo)         
         IF (ISimul .EQ. 0) THEN
            WRITE(UWrite, '(A, $)') 'MEDIA, '
            ColchonActivo = 0
         ELSE
            WRITE(UWrite, '(A, I3, '', '', $)') 'Sim', ISimul
            ColchonActivo = ParLajaM%ColchonActivo(ISimul, IEta)            
         ENDIF
         WRITE(UWrite, '(I4, '', '', $)') IBlo
         WRITE(UWrite, '(I4, '', '', $)') IEta
         WRITE(UWrite, '(I4, '', '', $)') Mes(IEta)
         WRITE(UWrite, '(A, '', '', $)')  TipoEtapa(IEta)
         WRITE(UWrite, '(I3, '', '', $)') ParLajaM%TipoEtaGM(IEta)         
         WRITE(UWrite, '(F7.1, '', '', $)') Time
         WRITE(UWrite, '(E9.2, '', '', $)') 1d3/VolScale
         WRITE(UWrite, '(I3, '', '', $)') ColchonActivo

         DO Idx = 1, ParLajaM%NumColEta
            IF (.NOT. ParLajaM%VarEtaVol(Idx)) THEN
               WRITE(UWrite, '(F9.3, '', '', $)')                       &
     &              ParLajaM%DataEta(Idx, IEta)
            ELSE
               WRITE(UWrite, '(F9.3, '', '', $)')                       &
     &              ParLajaM%DataEta(Idx, IEta)*VolScale
            ENDIF
         ENDDO
         DO Idx = 1, ParLajaM%NumColBlo
            WRITE(UWrite, '(F7.3, '', '', $)')                          &
     &           ParLajaM%DataBlo(Idx, IBlo)
         ENDDO

         WRITE(UWrite, *)
         Time = Time + BloDur(IBlo)
      ENDDO

      CALL Cerrar(UWrite)
      RETURN
      END


!
!
!

      LOGICAL FUNCTION IsVarCorteLajaM(IEtapaOri, ParLajaM, Ind)
      USE PLP

      INTEGER, INTENT(IN):: Ind
      INTEGER, INTENT(IN):: IEtapaOri
      TYPE(PAR_LAJAM), INTENT(IN):: ParLajaM

      IF (Ind .GT. ParLajaM%DimPDLDAcCol) THEN
         IsVarCorteLajaM = .False.
         RETURN
      ENDIF

      IsVarCorteLajaM = .TRUE.
      
! Gasto Medio Anual al inicio del mes no es variable de estado
      IF (ParLajaM%TipoEtaGM(IEtapaOri) .EQ. INICIOTEMP) THEN
         IF (Ind .EQ. ParLajaM%IVGAF) THEN
            IsVarCorteLajaM = .true.
         ELSE         
            IsVarCorteLajaM = .false.
         ENDIF
      ENDIF

      IF (ParLajaM%TipoEtaGM(IEtapaOri) .EQ. INICIOANTIC) THEN
         IF (Ind .EQ. ParLajaM%IVGAF) THEN
            IsVarCorteLajaM = .false.
         ELSE         
            IsVarCorteLajaM = .true.
         ENDIF
      ENDIF
      
      
      RETURN
      END
