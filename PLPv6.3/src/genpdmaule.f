!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDMauleBloA(IEta, IBloque, blodur, etadur,           &
     &     ParMaule, CenOffset, FiltVarColInd,                           &
     &     FEmbOffset, COffset, FOffset,                                 &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_MAULE, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim
!     Variables Globales
!******************
      INTEGER PDNCol
      CHARACTER*12 Nombre
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*(DimLargo) CVolB
      CHARACTER*80 fconcat
      INTEGER IEta
      INTEGER CenOffset
      INTEGER COffset
      INTEGER FOffset
      INTEGER FEmbOffset
      INTEGER IBloque
      TYPE(PAR_MAULE) ParMaule

      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)

      DOUBLE PRECISION blodur
      DOUBLE PRECISION etadur
      TYPE(AMatrix) A

!     Variables Locales
!****************      INTEGER FiltVarIndMaule
      INTEGER IQMaule
      INTEGER IQVerMaule
      INTEGER IQLagInvern
      INTEGER IQVerInvern
      INTEGER FiltVarIndInvern
      INTEGER FOffseti
      INTEGER Idx
      INTEGER IVol
      INTEGER FCentRiego

      IVol = ParMaule%IEmbMaule

      CALL Num2Char(IBloque, CVolB, No, DimLargo)

      DO Idx = 1, ParMaule%NumColBlo
         Nombre = ParMaule%VarBloNames(Idx)
         Nombre = fconcat('m_', Nombre)
         Nombre = fconcat(Nombre, '_')
         Nombre = fconcat(Nombre, CVolB)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      FOffseti = FOffset

      FiltVarIndInvern = 0
      IF (ParMaule%IFiltInvern .GT. 0) THEN
         FiltVarIndInvern = FiltVarColInd(ParMaule%IFiltInvern, IEta)
      ENDIF


!***************
!     Maule
!***************
      ! Particion de caudal de generacion en caudales
      ! de economias y turbinado a cuenta de gasto
      !
      !    qg_i_j = m_qgde_j + m_qgdr_j + ...
      !
      ! donde
      !       'i' es el indice de la central El Toro
      !       'j' es el indice del bloque
      !
      !
      IQMaule = CenOffset + ParMaule%IEmbMaule
      IQVerMaule = CenOffset + ParMaule%IQVerMaule
      CALL Am_set(A, IQMaule, FOffseti + ParMaule%IQMAULE_F, -1.0d0)

      CALL Am_set(A, IQVerMaule, FOffseti + ParMaule%IQMAULE_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMNE, FOffseti + ParMaule%IQMAULE_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMNR, FOffseti + ParMaule%IQMAULE_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMOE, FOffseti + ParMaule%IQMAULE_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMOR, FOffseti + ParMaule%IQMAULE_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMCE, FOffseti + ParMaule%IQMAULE_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMEI, FOffseti + ParMaule%IQMAULE_F, 1.0d0)


!     caudal laguna invernada a cuenta de derechos
      IQLagInvern = CenOffset + ParMaule%ILagInvern
      IQVerInvern = CenOffset + ParMaule%IQVerInvern
      CALL Am_set(A, IQLagInvern, FOffseti + ParMaule%IQLINVE_F, -1.0d0)

      CALL Am_set(A, IQVerInvern, FOffseti + ParMaule%IQLINVE_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQIDN, FOffseti + ParMaule%IQLINVE_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQISD, FOffseti + ParMaule%IQLINVE_F, 1.0d0)

!     caudal total entregado para riego
      CALL Am_set(A, COffset + ParMaule%IQMNR, FOffseti + ParMaule%IQTER_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMOR, FOffseti + ParMaule%IQTER_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQTER, FOffseti + ParMaule%IQTER_F, 1.0d0)

!     Calculos de promedios horarios
      CALL Am_set(A, COffset + ParMaule%IQMNE, ParMaule%FilIndEta(ParMaule%IQMNEH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQMNR, ParMaule%FilIndEta(ParMaule%IQMNRH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQMOE, ParMaule%FilIndEta(ParMaule%IQMOEH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQMOR, ParMaule%FilIndEta(ParMaule%IQMORH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQMCE, ParMaule%FilIndEta(ParMaule%IQMCEH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQMEI, ParMaule%FilIndEta(ParMaule%IQMEIH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQIDN, ParMaule%FilIndEta(ParMaule%IQIDNH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQISD, ParMaule%FilIndEta(ParMaule%IQISDH_F, IEta), -blodur/etadur)

      CALL Am_set(A, COffset + ParMaule%IQTER, ParMaule%FilIndEta(ParMaule%IQTERH_F, IEta), -blodur/etadur)

!     Caudal aportado Maule horario
      CALL Am_set(A, IQMaule, ParMaule%FilIndEta(ParMaule%IQMAUH_F, IEta), -blodur/etadur)

      CALL Am_set(A, IQVerMaule, ParMaule%FilIndEta(ParMaule%IQMAUH_F, IEta), -blodur/etadur)

!     Caudal aportado Invernada horario
      CALL Am_set(A, IQLagInvern, ParMaule%FilIndEta(ParMaule%IQINVH_F, IEta), -blodur/etadur)

      CALL Am_set(A, IQVerInvern, ParMaule%FilIndEta(ParMaule%IQINVH_F, IEta), -blodur/etadur)

!     Agrega caudal de riego extraido a la ecuacion de la central de riego
      DO Idx = 1, ParMaule%NumRetRiego
         FCentRiego = FEmbOffset + ParMaule%ICenRetRiego(Idx)
         CALL Am_set(A, ParMaule%ColIndEta(ParMaule%IQRIHC(Idx), IEta), FCentRiego, 1.0d0)
      ENDDO


!***************
!     Offsets Finales
!***************
      COffset = COffset + ParMaule%NumColBlo
      FOffset = FOffset + ParMaule%NumFilBlo

      RETURN
      END


!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDMauleBloFO(ParMaule,                              &
     &     BloDur, FPhi,                                                &
     &     COffset, CenOffset,                                          &
     &     PDNCol, FO, LowBnd, UppBnd)
      USE PLP, ONLY : PAR_DIMS, PAR_MAULE
      USE OSI

!     Variables Globales
!******************
      INTEGER PDNCol
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION FPhi
      INTEGER COffset
      INTEGER CenOffset
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      TYPE(PAR_MAULE) ParMaule

      INTEGER IQCanelon


      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + ParMaule%NumColBlo) = 0.0d0

      IF (ParMaule%IQCanelon .GT. 0) THEN
         IQCanelon = CenOffset + ParMaule%IQCanelon
         FO(IQCanelon) = ParMaule%CostoCanelon * BloDur/FPhi
      ENDIF


!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + 1 : COffset + ParMaule%NumColBlo) = DINFTY
      LowBnd(COffset + 1 : COffset + ParMaule%NumColBlo) = 0.0d0

      RETURN
      END


!**********************************

      SUBROUTINE GenPDMauleEtaA(IEta, ParMaule, etadur,                 &
     &     FiltVarColInd, VolOffSet, COffset, FOffset,                  &
     &     A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_MAULE
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!******************
      INTEGER PDNCol, PDNFila
      TYPE(PAR_MAULE) ParMaule
      CHARACTER*24 PDNombre (PDNCol)
      CHARACTER*1 Sentido(PDNFila)
      INTEGER IEta
      INTEGER COffset
      INTEGER FOffset
      INTEGER VolOffset
      INTEGER FiltVarColInd(Dim%EmbFilt, Dim%Eta)
      DOUBLE PRECISION etadur
      TYPE(AMatrix) A

!     Variables Locales
!****************
      CHARACTER*12 Nombre
      CHARACTER*80 fconcat

      INTEGER Idx
      INTEGER IVol
      DOUBLE PRECISION etadursc

      INTEGER FiltVarIndInvern

      INTEGER IFRiego

      IVol = ParMaule%IEmbMaule

      FiltVarIndInvern = 0
      IF (ParMaule%IFiltInvern .GT. 0) THEN
         FiltVarIndInvern = FiltVarColInd(ParMaule%IFiltInvern, IEta)
      ENDIF

      DO Idx = 1, ParMaule%NumColEta
         Nombre = ParMaule%VarEtaNames(Idx)
         Nombre = fconcat('m_', Nombre)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      DO Idx = 1, ParMaule%NumColEta
         ParMaule%ColIndEta(Idx, IEta) = COffset + Idx
      ENDDO

      DO Idx = 1, ParMaule%NumFilEta
         ParMaule%FIlIndEta(Idx, IEta) = FOffset + Idx
      ENDDO

      etadursc = etadur/ParMaule%ScaleVol
      ! Volumen mensual derechos electricos mensuales
      CALL Am_set(A, COffset + ParMaule%IQMNEH, FOffset + ParMaule%IVMGEMF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IQMOEH, FOffset + ParMaule%IVMGEMF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMGEMF, FOffset + ParMaule%IVMGEMF_F, 1.0d0)

      ! Volumen anual derechos electrecticos anuales
      CALL Am_set(A, COffset + ParMaule%IQMNEH, FOffset + ParMaule%IVMGEAF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IQMOEH, FOffset + ParMaule%IVMGEAF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMGEAF, FOffset + ParMaule%IVMGEAF_F, 1.0d0)

      ! Volumen  de gasto de reservas de riego de temporada
      CALL Am_set(A, COffset + ParMaule%IQMNRH, FOffset + ParMaule%IVMGRTF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IQMORH, FOffset + ParMaule%IVMGRTF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMGRTF, FOffset + ParMaule%IVMGRTF_F, 1.0d0)

      ! Volumen  de gasto de reservas electrica
      CALL Am_set(A, COffset + ParMaule%IQMOEH, FOffset + ParMaule%IVMGOEF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMGOEF, FOffset + ParMaule%IVMGOEF_F, 1.0d0)

      ! Volumen  de gasto de reservas de riego
      CALL Am_set(A, COffset + ParMaule%IQMORH, FOffset + ParMaule%IVMGORF_F, -etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMGORF, FOffset + ParMaule%IVMGORF_F, 1.0d0)

      ! Volumen  de derechos de reservas electrica
      CALL Am_set(A, COffset + ParMaule%IQMNIH, FOffset + ParMaule%IVMDOEF_F, - ParMaule%PorcQNIDE*etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMDOEF, FOffset + ParMaule%IVMDOEF_F, 1.0d0)

      ! Volumen  de derechos de reservas de riego
      CALL Am_set(A, COffset + ParMaule%IQMNIH, FOffset + ParMaule%IVMDORF_F, - ParMaule%PorcQNIDR*etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMDORF, FOffset + ParMaule%IVMDORF_F, 1.0d0)

      ! Volumen  de gasto de compensacion electrica
      CALL Am_set(A, COffset + ParMaule%IVMDCEN, FOffset + ParMaule%IVMDCEF_F,-1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMCEH, FOffset + ParMaule%IVMDCEF_F, etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMDCEF, FOffset + ParMaule%IVMDCEF_F, 1.0d0)

      ! Volumen  economias invernada
      CALL Am_set(A, COffset + ParMaule%IQMEIH, FOffset + ParMaule%IVMDEIF_F, etadursc)

      CALL Am_set(A, COffset + ParMaule%IVMDEIN, FOffset + ParMaule%IVMDEIF_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMDEIF, FOffset + ParMaule%IVMDEIF_F, 1.0d0)

      ! Volumen  util del maule
      CALL Am_set(A, VolOffset + ParMaule%IEmbMaule, FOffset + ParMaule%IVMUTIL_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMUTIL, FOffset + ParMaule%IVMUTIL_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMREB, FOffset + ParMaule%IVMREB_F, 1.0d0)

      IF (ParMaule%IQRebMaule .GT. 0) THEN
         CALL Am_set(A, VolOffset + ParMaule%IQRebMaule, FOffset + ParMaule%IVMREB_F, -etadursc)
      ENDIF

      ! Cotas de derechos y gasto
      CALL Am_set(A, COffset + ParMaule%IVMGEMF, FOffset + ParMaule%IVMDEMT_F,  1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMDEMT, FOffset + ParMaule%IVMDEMT_F, -1.0d0)

      Sentido(FOffset + ParMaule%IVMDEMT_F) = 'L'

      CALL Am_set(A, COffset + ParMaule%IVMGEAF, FOffset + ParMaule%IVMDEAT_F,  1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMDEAT, FOffset + ParMaule%IVMDEAT_F, -1.0d0)

      Sentido(FOffset + ParMaule%IVMDEAT_F) = 'L'

      CALL Am_set(A, COffset + ParMaule%IVMGRTF, FOffset + ParMaule%IVMDRTT_F,  1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMDRTT, FOffset + ParMaule%IVMDRTT_F, -1.0d0)

      Sentido(FOffset + ParMaule%IVMDRTT_F) = 'L'


      CALL Am_set(A, COffset + ParMaule%IVMGOEF, FOffset + ParMaule%IVDGRET_F,  1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMDOEF, FOffset + ParMaule%IVDGRET_F, -1.0d0)

      Sentido(FOffset + ParMaule%IVDGRET_F) = 'L'

      CALL Am_set(A, COffset + ParMaule%IVMGORF, FOffset + ParMaule%IVDGRRT_F,  1.0d0)

      CALL Am_set(A, COffset + ParMaule%IVMDORF, FOffset + ParMaule%IVDGRRT_F, -1.0d0)

      Sentido(FOffset + ParMaule%IVDGRRT_F) = 'L'


      ! Caudales horarios
      CALL Am_set(A, COffset + ParMaule%IQMNEH, FOffset + ParMaule%IQMNEH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMNRH, FOffset + ParMaule%IQMNRH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMOEH, FOffset + ParMaule%IQMOEH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMORH, FOffset + ParMaule%IQMORH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMCEH, FOffset + ParMaule%IQMCEH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMEIH, FOffset + ParMaule%IQMEIH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQIDNH, FOffset + ParMaule%IQIDNH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQISDH, FOffset + ParMaule%IQISDH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQTERH, FOffset + ParMaule%IQTERH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMAUH, FOffset + ParMaule%IQMAUH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQINVH, FOffset + ParMaule%IQINVH_F, 1.0d0)


      ! Agrega filtraciones y rebalse al caudal de la invernada
      IF (FiltVarIndInvern .GT. 0) THEN
         CALL Am_set(A, FiltVarIndInvern, FOffset + ParMaule%IQINVH_F, -1.0d0)
      ENDIF
      IF (ParMaule%IQRebInvern .GT. 0) THEN
         CALL Am_set(A, VolOffset + ParMaule%IQRebInvern, FOffset + ParMaule%IQINVH_F, -1.0d0)
      ENDIF
      ! Agrega rebalse al caudal del maule
      IF (ParMaule%IQRebMaule .GT. 0) THEN
         CALL Am_set(A, VolOffset + ParMaule%IQRebMaule, FOffset + ParMaule%IQMAUH_F, -1.0d0)
      ENDIF

      ! restricciones de entrega de caudal del maule
      CALL Am_set(A, COffset + ParMaule%IQDRAH, FOffset + ParMaule%IQDRMH_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQDRMH, FOffset + ParMaule%IQDRMH_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQINVH, FOffset + ParMaule%IQDRMH_F, -1.0d0)

      Sentido(FOffset + ParMaule%IQDRMH_F) = 'L'

      CALL Am_set(A, COffset + ParMaule%IQDRAH, FOffset + ParMaule%IQDRAH_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQTERH, FOffset + ParMaule%IQDRAH_F, 1.0d0)

      Sentido(FOffset + ParMaule%IQDRAH_F) = 'L'

      ! reconstruccion caudal en armerillo
      CALL Am_set(A, COffset + ParMaule%IQARMR, FOffset + ParMaule%IQARMR_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQHI, FOffset + ParMaule%IQARMR_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQINVH, FOffset + ParMaule%IQARMR_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQMAUH, FOffset + ParMaule%IQARMR_F, -1.0d0)

      IF (ParMaule%DescGastoElecArmr) THEN
         CALL Am_set(A, COffset + ParMaule%IQMNEH, FOffset + ParMaule%IQARMR_F, 1.0d0)

         CALL Am_set(A, COffset + ParMaule%IQMOEH, FOffset + ParMaule%IQARMR_F, 1.0d0)
      ENDIF

      ! restricciones de no embalse en la invernada
      IF (FiltVarIndInvern .GT. 0) THEN
         CALL Am_set(A, FiltVarIndInvern, FOffset + ParMaule%IQNINV_F, 1.0d0)
      ENDIF
      CALL Am_set(A, COffset + ParMaule%IQNINV, FOffset + ParMaule%IQNINV_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQIDNH, FOffset + ParMaule%IQRNIN_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQNINV, FOffset + ParMaule%IQRNIN_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQHINV, FOffset + ParMaule%IQRNIN_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQHNEIN,FOffset + ParMaule%IQRNIN_F, 1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQHEIN,FOffset + ParMaule%IQRNIN_F, 1.0d0)

      !Relajacion no embalsar
      IF (ParMaule%RelaxInvern) THEN
         Sentido(FOffset + ParMaule%IQNINV_F) = 'L'
      ENDIF

      Sentido(FOffset + ParMaule%IQRNIN_F) = 'E'

      ! restricciones de caudales de res 105
      CALL Am_set(A, COffset + ParMaule%IQARMR, FOffset + ParMaule%IQA105_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQA105, FOffset + ParMaule%IQA105_F, 1.0d0)

      Sentido(FOffset + ParMaule%IQA105_F) = 'L'

      CALL Am_set(A, COffset + ParMaule%IQR105, FOffset + ParMaule%IQR105_F, -1.0d0)

      CALL Am_set(A, COffset + ParMaule%IQA105, FOffset + ParMaule%IQR105_F, 1.0d0)

      Sentido(FOffset + ParMaule%IQR105_F) = 'L'


      ! entregas de riego
      DO Idx = 1, ParMaule%NumRetRiego
         IFRiego = FOffset + ParMaule%IQRIHF(Idx)
         CALL Am_set(A, COffset + ParMaule%IQRIHC(Idx), IFRiego, 1.0d0)

         IF (ParMaule%IQRHHC(Idx) .GT. 0) THEN
            CALL Am_set(A, COffset + ParMaule%IQRHHC(Idx), IFRiego, 1.0d0)
         ENDIF

         CALL Am_set(A, COffset + ParMaule%IQA105, IFRiego, -ParMaule%PRetRiego(Idx))
      ENDDO

      RETURN
      END

!******************
      SUBROUTINE GenPDMauleEtaFO(ParMaule, edur, FPhi,            &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)
      USE PLP, ONLY : PAR_DIMS, PAR_MAULE
      USE OSI
!     Variables Globales
!******************
      TYPE(PAR_MAULE) ParMaule

      DOUBLE PRECISION FPhi

      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION edur
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      DOUBLE PRECISION FCau
      DOUBLE PRECISION FVol

      INTEGER IVol
      INTEGER Idx

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

      IVol = ParMaule%IEmbMaule

      FCau = edur / FPhi
      FVol = ParMaule%ScaleVol / FPhi

!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + ParMaule%NumColEta) = 0.0d0

      FO(COffset + ParMaule%IQA105) = - FCau * ParMaule%ValorRiego105
      FO(COffset + ParMaule%IQTERH) = - FCau * ParMaule%ValorRiegoMaule
      FO(COffset + ParMaule%IQDRAH) = FCau * ParMaule%CostoRiegoNSMaule
      FO(COffset + ParMaule%IVMDEIF) = FVol * ParMaule%EconInvernCosto
      FO(COffset + ParMaule%IQHINV) = FCau * ParMaule%CostoEmbalsar
      FO(COffset + ParMaule%IQHNEIN) = FCau * ParMaule%CostoNoEmbalsar


      DO Idx = 1, ParMaule%NumRetRiego
         IF (ParMaule%IQRHHC(Idx) .GT. 0) THEN
            FO(COffset + ParMaule%IQRHHC(Idx)) = FCau * ParMaule%CostoRiegoNS105
         ENDIF
      ENDDO

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + 1 : COffset + ParMaule%NumColEta) = DINFTY
      LowBnd(COffset + 1 : COffset + ParMaule%NumColEta) = 0.0d0

      LowBnd(COffset + ParMaule%IQNINV) = -DINFTY

      RETURN
      END

!*******************

      SUBROUTINE GetQsMaule(IEta, NBloque, BloInd, BloDur, Mes, Year,   &
     &     ModQRiegoRes, VolDerRiegoResIni,                             &
     &     EstocRHSP, IClaseFila, ParMaule, QFiltInvern,                &
     &     QHoyaInter, QDefRiego, QAflMaule, QAflInvern, FInvernDesemb, &
     &     QRiego105, Dim)

      USE PLP, ONLY : PAR_DIMS, PAR_MAULE, FactTiempoH

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
      INTEGER IEta
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER Mes(Dim%Eta)
      INTEGER Year(Dim%Eta)
      INTEGER IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      TYPE(PAR_MAULE) ParMaule

      DOUBLE PRECISION QFiltInvern
      DOUBLE PRECISION QHoyaInter
      DOUBLE PRECISION QDefRiego
      DOUBLE PRECISION QAflMaule
      LOGICAL FInvernDesemb

!
      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION friego_mes
      INTEGER IAfl
      DOUBLE PRECISION QRiego
      DOUBLE PRECISION QMaule
      DOUBLE PRECISION QAflInvern
      DOUBLE PRECISION QRiego105

      LOGICAL ModQRiegoRes
      DOUBLE PRECISION VolDerRiegoResIni
      DOUBLE PRECISION TempRiegoDur
      DOUBLE PRECISION VolDerRiegoRes
      DOUBLE PRECISION friego_corr
      DOUBLE PRECISION CauFutRiego

      QHoyaInter = 0.0d0
      DO IAfl = 1, ParMaule%NumAflHoyaInt
         IF (ParMaule%IAflHoyaInt(IAfl) .GT. 0) THEN
            QHoyaInter = QHoyaInter + QAfluEta(IEta, NBloque, BloInd,   &
     &           BloDur, EstocRHSP, IClaseFila,                         &
     &           ParMaule%IAflHoyaInt(IAfl), Dim)
         ENDIF
      ENDDO

      QAflInvern = QAfluEta(IEta, NBloque, BloInd,                      &
     &     BloDur, EstocRHSP, IClaseFila,                               &
     &     ParMaule%IAflInvern, Dim)

      QAflMaule = QAfluEta(IEta, NBloque, BloInd,                       &
     &     BloDur, EstocRHSP, IClaseFila,                               &
     &     ParMaule%IAflMaule, Dim)

      friego_corr = 1.0d0
      IF (ModQRiegoRes) THEN
!        Estimamos cuanto dura aun la temporada de riego
         TempRiegoDur = ParMaule%DiasTempRiegoAcum(Mes(IEta))
!        si nos queda mas que un mes, hacemos la modulacion
         IF (TempRiegoDur .GT. 31) THEN
            TempRiegoDur = TempRiegoDur
            TempRiegoDur = 24 * FactTiempoH * TempRiegoDur
!           Estimamos cuanto volumen de riego tendremos disponible
            CauFutRiego = ParMaule%PorcQNIDR * QAflMaule                &
     &           + QHoyaInter + QFiltInvern
            VolDerRiegoRes = VolDerRiegoResIni                          &
     &           + TempRiegoDur * CauFutRiego * ParMaule%FactCauFutRiego
            friego_corr = MIN(1.0d0,                                    &
     &           VolDerRiegoRes/ParMaule%VolMaxDerRiegoAcum(Mes(IEta)))
         ENDIF
      ENDIF

      friego_mes = ParMaule%PRiegoMauleAnual(Mes(IEta), Year(IEta))
      QRiego = friego_corr * friego_mes * ParMaule%GastoRiegoMax
      QDefRiego = MAX(QRiego - QHoyaInter, 0.0d0)

      QRiego105 = ParMaule%QRiego105Anual(Mes(IEta), Year(IEta))
      IF (ParMaule%AutModRes105) THEN
         QRiego105 = friego_corr * QRiego105
      ENDIF

      FInvernDesemb = (QDefRiego .GT. QFiltInvern)

      IF (.NOT. FInvernDesemb) THEN
         QMaule = QHoyaInter + QAflInvern

         FInvernDesemb = QMaule .LT. friego_mes*ParMaule%GastoMauleMin
      ENDIF

      RETURN
      END

!***************************************

      SUBROUTINE FijaMaule(IEta,                                        &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, Year, FactTiempo,      &
     &     EstocRHSP, IClaseFila,                                       &
     &     ParMaule, VolIni, NumEmb, FiltVals,                          &
     &     ISimul,                                                      &
     &     lp, Dim)

      USE PLP
      USE OSI

!     USE PLP, ONLY : PAR_DIMS, PAR_MAULE, C_SIZE_T

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN) :: IEta
      INTEGER, INTENT(IN) :: NBloque(Dim%Eta)
      INTEGER, INTENT(IN) :: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: FactTiempo
      INTEGER, INTENT(IN) :: Mes(Dim%Eta)
      INTEGER, INTENT(IN) :: Year(Dim%Eta)

      TYPE(PAR_MAULE), INTENT(INOUT):: ParMaule
      INTEGER, INTENT(IN) :: NumEmb
      DOUBLE PRECISION, INTENT(IN):: VolIni(NumEmb)
      INTEGER, INTENT(IN) :: ISimul

      INTEGER, INTENT(IN) :: IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: EtaDur(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: FiltVals(Dim%EmbFilt)

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp

!
      DOUBLE PRECISION VarMaulePrev(ParMaule%NumColEta)
      DOUBLE PRECISION LDResMaule(ParMaule%NumFilEta)
      DOUBLE PRECISION VarMaule(ParMaule%NumColEta)
      DOUBLE PRECISION UppMaule(ParMaule%NumColEta)
      DOUBLE PRECISION LowMaule(ParMaule%NumColEta)

      INTEGER Idx
      INTEGER IdxU
      INTEGER IdxL
      INTEGER IdxE

      DOUBLE PRECISION VolMauleUtil
      DOUBLE PRECISION QHoyaInter
      DOUBLE PRECISION QDefRiego
      DOUBLE PRECISION QAflInvern
      DOUBLE PRECISION QAflMaule
      LOGICAL FInvernDesemb

      DOUBLE PRECISION QFiltInvern

      DOUBLE PRECISION VolColbunPrev

      LOGICAL RegimenNormal
      LOGICAL ReservaOrdinariaActiva

      LOGICAL EnRiego
      LOGICAL ModQRiegoRes


      DOUBLE PRECISION VCompElecN
      DOUBLE PRECISION VCompElecA
      DOUBLE PRECISION VReservasMaule
      DOUBLE PRECISION VolReserva

      DOUBLE PRECISION QRiego105
      DOUBLE PRECISION QLagInvern

      DOUBLE PRECISION VolDerRiegoResIni

      INTEGER IBlo

      INTEGER ICol, IFil

      INTEGER MCUppInd (Dim%IBlo + ParMaule%NumColEta)
      DOUBLE PRECISION MCUppVal (Dim%IBlo + ParMaule%NumColEta)
      INTEGER MCLowInd (Dim%IBlo + ParMaule%NumColEta)
      DOUBLE PRECISION MCLowVal (Dim%IBlo + ParMaule%NumColEta)

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!

      VarMaule(1: ParMaule%NumColEta) = 0.0d0
      LDResMaule(1: ParMaule%NumFilEta) = 0.0d0
!
!     Obtiene el valor de las variables previas del maule, que pueden
!     resultar de las condiciones iniciales o de la etapa previa
!
      CALL GetVarMaulePrev(IEta, ISimul,                                &
     &     ParMaule, VolIni, VarMaulePrev, VolColbunPrev, Dim)

!
!     Volumen util del maule
!
      VolMauleUtil = VarMaulePrev(ParMaule%IVMUTIL)
      QFiltInvern = FiltVals(ParMaule%IFiltInvern)

      VReservasMaule = ParMaule%VReservaOrdinaria + ParMaule%VReservaExtraord
      IF (VolMauleUtil .GE. VReservasMaule) THEN
         RegimenNormal = .TRUE.
         ReservaOrdinariaActiva = .FALSE.
      ELSE
         RegimenNormal = .FALSE.
         ReservaOrdinariaActiva = .TRUE.
      ENDIF
      ParMaule%EnRegimenNormal(ISimul, IEta) = RegimenNormal


      EnRiego = ParMaule%TipoEtaDR(IEta) .NE. 0

!
!     Las restricciones de economias, gasto y derechos asumen en valor
!     de las variables de la etapa previa, que se consideran como valor
!     inicial
!
      LDResMaule(ParMaule%IVMGEMF_F) = VarMaulePrev(ParMaule%IVMGEMF)
      LDResMaule(ParMaule%IVMGEAF_F) = VarMaulePrev(ParMaule%IVMGEAF)
      LDResMaule(ParMaule%IVMGRTF_F) = VarMaulePrev(ParMaule%IVMGRTF)
      LDResMaule(ParMaule%IVMGOEF_F) = VarMaulePrev(ParMaule%IVMGOEF)
      LDResMaule(ParMaule%IVMGORF_F) = VarMaulePrev(ParMaule%IVMGORF)
      LDResMaule(ParMaule%IVMDOEF_F) = VarMaulePrev(ParMaule%IVMDOEF)
      LDResMaule(ParMaule%IVMDORF_F) = VarMaulePrev(ParMaule%IVMDORF)
      LDResMaule(ParMaule%IVMDCEF_F) = VarMaulePrev(ParMaule%IVMDCEF)
      LDResMaule(ParMaule%IVMDEIF_F) = VarMaulePrev(ParMaule%IVMDEIF)

      IF (.NOT. EnRiego) THEN
         LDResMaule(ParMaule%IVMGRTF_F) = 0.0d0
         UppMaule(ParMaule%IQMNRH) = 0.0d0
         UppMaule(ParMaule%IQMORH) = 0.0d0
      ENDIF


      IF (RegimenNormal) THEN
!        Anulamos los volumenes acumulados de la reservas
         LDResMaule(ParMaule%IVMGOEF_F) = 0.0d0
         LDResMaule(ParMaule%IVMGORF_F) = 0.0d0
         LDResMaule(ParMaule%IVMDOEF_F) = 0.0d0
         LDResMaule(ParMaule%IVMDORF_F) = 0.0d0
      ELSE
         IF ((LDResMaule(ParMaule%IVMDOEF_F) .LE. 0.0d0) .AND.          &
     &        (LDResMaule(ParMaule%IVMDORF_F) .LE. 0.0d0)) THEN
!           Si no hay derechos asignados, estamos recien entrando a la
!           reserva ordinaria, asignamos los porcentajes
!           correspondientes del volumen total de la reserva
            VolReserva = VolMauleUtil - ParMaule%VReservaExtraord
            LDResMaule(ParMaule%IVMDOEF_F) = ParMaule%PorcQNIDE*VolReserva
            LDResMaule(ParMaule%IVMDORF_F) = ParMaule%PorcQNIDR*VolReserva
         ENDIF
      ENDIF

      ModQRiegoRes = .FALSE.
      VolDerRiegoResIni = 0.0d0
      IF (RegimenNormal) THEN
         UppMaule(ParMaule%IQMNEH) = ParMaule%GastoElecDiaMax
         UppMaule(ParMaule%IQMOEH) = 0.0d0
         UppMaule(ParMaule%IQMCEH) = DINFTY
         IF (EnRiego) THEN
            UppMaule(ParMaule%IQMNRH) = DINFTY
            UppMaule(ParMaule%IQMORH) = 0.0d0
         ENDIF
      ELSE
         UppMaule(ParMaule%IQMNEH) = 0.0d0
         UppMaule(ParMaule%IQMOEH) = ParMaule%GastoElecDiaMax           &
     &        * ParMaule%PGastoElecDiaMaxMenRes(Mes(IEta))
         UppMaule(ParMaule%IQMCEH) = 0.0d0

         IF (EnRiego) THEN
            UppMaule(ParMaule%IQMORH) = DINFTY
            UppMaule(ParMaule%IQMNRH) = 0.0d0
            IF (ParMaule%AnoModQRiegoRes .GT. 0) THEN
               IF (Year(IEta) .GE. ParMaule%AnoModQRiegoRes) THEN
                  ModQRiegoRes = .TRUE.
                  VolDerRiegoResIni = LDResMaule(ParMaule%IVMDORF_F) -  &
     &                 LDResMaule(ParMaule%IVMGORF_F)
               ENDIF
            ENDIF
         ENDIF
      ENDIF

      IF (ParMaule%EconInvernUsoEnReserva) THEN
         UppMaule(ParMaule%IQMEIH) = DINFTY
      ELSE
         IF (RegimenNormal) THEN
            UppMaule(ParMaule%IQMEIH) = DINFTY
         ELSE
            UppMaule(ParMaule%IQMEIH) = 0.0d0
         ENDIF
      ENDIF


!
!     Calcula flujos relevantes
!
      CALL GetQsMaule(IEta, NBloque, BloInd, BloDur, Mes, Year,         &
     &     ModQRiegoRes, VolDerRiegoResIni,                             &
     &     EstocRHSP, IClaseFila, ParMaule, QFiltInvern,                &
     &     QHoyaInter, QDefRiego,                                       &
     &     QAflMaule, QAflInvern, FInvernDesemb,                        &
     &     QRiego105, Dim)


      VarMaule(ParMaule%IQR105) = QRiego105

      IF (ReservaOrdinariaActiva) THEN
         VarMaule(ParMaule%IQMNIH) = QAflMaule
      ELSE
         VarMaule(ParMaule%IQMNIH) = 0.0d0
      ENDIF

      VarMaule(ParMaule%IQHI) = QHoyaInter

      LDResMaule(ParMaule%IQNINV_F) = QAflInvern

!     Por defecto puede embalsar la Invernada
      IFil = ParMaule%FilIndEta(ParMaule%IQRNIN_F, IEta) - 1
      ICol = ParMaule%ColIndEta(ParMaule%IQHEIN, IEta) - 1
      CALL osi_lp_setcoefficient(lp, IFIl, ICol, 1.0d0)


!     Laguna Invernada. Aplicamos la regla de que si no se puede
!     embalsar los retiros adicionales van a cuenta de economias en el
!     maule
      IF (FInvernDesemb) THEN
         ! No se permite embalsar
         IFil = ParMaule%FilIndEta(ParMaule%IQRNIN_F, IEta) - 1
         ICol = ParMaule%ColIndEta(ParMaule%IQNINV, IEta) - 1
         CALL osi_lp_setcoefficient(lp, IFIl, ICol, 1.0d0)

         IF (ParMaule%NoDesembInv) THEN
             IF (ReservaOrdinariaActiva) THEN
!               Si no se pueden acumular economias en colchon intermedio
!               Entonces Invernada mantiene cota
                IFil = ParMaule%FilIndEta(ParMaule%IQRNIN_F, IEta) - 1
                ICol = ParMaule%ColIndEta(ParMaule%IQHEIN, IEta) - 1
                CALL osi_lp_setcoefficient(lp, IFIl, ICol, 0.0d0)
             ENDIF
         ENDIF

         UppMaule(ParMaule%IQIDNH) = DINFTY
         UppMaule(ParMaule%IQISDH) = 0.0d0
      ELSE
         ! Permintido embalsar
         IFil = ParMaule%FilIndEta(ParMaule%IQRNIN_F, IEta) - 1
         ICol = ParMaule%ColIndEta(ParMaule%IQNINV, IEta) - 1
         CALL osi_lp_setcoefficient(lp, IFIl, ICol, 0.0d0)

         UppMaule(ParMaule%IQIDNH) = 0.0d0
         UppMaule(ParMaule%IQISDH) = DINFTY
      ENDIF

      VarMaule(ParMaule%IQDRMH) =  QDefRiego

!     Nuevas economias de Invernada
      IF (IEta .GT. 1) THEN
         QLagInvern = VarMaulePrev(ParMaule%IQIDNH)
!        Existe deficit de riego en la etapa anterior
         IF (VarMaulePrev(ParMaule%IQDRMH) .gt. 0) THEN
            VarMaule(ParMaule%IVMDEIN) = MAX(QLagInvern                       &
     &           - MAX(VarMaulePrev(ParMaule%IQNINV), 0.0d0), 0.0d0)          &
     &           * FactTiempo * EtaDur(IEta - 1)
         ENDIF
      ELSE
         VarMaule(ParMaule%IVMDEIN) =  0.0d0
      ENDIF

!     Fijamos el Volumen util minimo absoluto y el volumen util del
!     colchon inferior
      LDResMaule(ParMaule%IVMUTIL_F) = ParMaule%VEmbalseUtilMin
      LowMaule(ParMaule%IVMUTIL) = ParMaule%VReservaExtraord
!
!     Consideraciones para inicio de mes y de agno
!
      IF (IEta .GT. 1) THEN
!        revisamos condiciones de etapas de derechos electricas
         IF (ParMaule%TipoEtaDE(IEta) .NE. INTRAETA) THEN
!           Reiniciamos contador de gasto mensual
            LDResMaule(ParMaule%IVMGEMF_F) = 0.0d0
         ENDIF
         VarMaule(ParMaule%IVMDCEN) = 0.0d0
         IF (ParMaule%TipoEtaDE(IEta) .EQ. INICIOANO) THEN
!           Calulamos las nuevas compensaciones
            VCompElecN = MAX(ParMaule%VDerElecAnuMax                    &
     &           - VarMaulePrev(ParMaule%IVMGEAF), 0.0d0)
!           limitamos al maximo permitido
            VCompElecA = MIN(ParMaule%VCompElecMax,                     &
     &           VarMaulePrev(ParMaule%IVMDCEF) + VCompElecN)

!           Calculamos las compensaciones nuevas anuales
            VarMaule(ParMaule%IVMDCEN) = VCompElecA                     &
     &           - VarMaulePrev(ParMaule%IVMDCEF)

!           Reiniciamos contador de los gasto anuales electricos
            LDResMaule(ParMaule%IVMGEAF_F) = 0.0d0
            ParMaule%CompElecMaxed(ISimul, IEta) =                      &
     &           VCompElecA .GE. ParMaule%VCompElecMax
         ENDIF

!        restamos el rebalse a las compensaciones
         IF (VarMaulePrev(ParMaule%IVMREB) .GT. 0.0d0) THEN
            VCompElecN = VarMaule(ParMaule%IVMDCEN)  &
     &           - VarMaulePrev(ParMaule%IVMREB)
            ! limitamos que las compensaciones no se vuelvan negativas
            VCompElecN = MAX(VCompElecN, -VarMaulePrev(ParMaule%IVMDCEF))

            VarMaule(ParMaule%IVMDCEN) = VCompElecN
         ENDIF

!        revisamos condiciones de etapas de derechos riego
         IF (ParMaule%TipoEtaDR(IEta) .EQ. INICIOANO) THEN
!           Reiniciamos contador de los gasto de riego de temporada
            LDResMaule(ParMaule%IVMGRTF_F) = 0.0d0
         ENDIF

!        inicio de agno electrico/calendario
         IF (ParMaule%TipoEtaDE(IEta) .EQ. INICIOANO) THEN
            IF (ReservaOrdinariaActiva) THEN
!              asignamos los porcentages correspondientes del volumen
!              total de la reserva
               VolReserva = VolMauleUtil - ParMaule%VReservaExtraord
               LDResMaule(ParMaule%IVMDOEF_F) = ParMaule%PorcQNIDE*VolReserva
               LDResMaule(ParMaule%IVMGOEF_F) = 0.0d0

               LDResMaule(ParMaule%IVMDORF_F) = ParMaule%PorcQNIDR*VolReserva
               LDResMaule(ParMaule%IVMGORF_F) = 0.0d0
            ENDIF

            IF (.NOT. ParMaule%EconInvernAcumAnual) THEN
!              Si no se pueden acumular economias anuales, se pierden
               LDResMaule(ParMaule%IVMDEIF_F) = 0.0d0
            ENDIF
         ENDIF


      ENDIF

      VarMaule(ParMaule%IVMDEMT) = ParMaule%VDerElecMenMax(IEta)
      VarMaule(ParMaule%IVMDEAT) = ParMaule%VDerElecAnuMax
      VarMaule(ParMaule%IVMDRTT) = ParMaule%VDerRiegoTempMax

!
!     Reglas para calcular las nuevas economias de endesa y de reserva
!

!
!     Low boundaries simples
!
      IdxL = 0

      IdxL = IdxL + 1
      MCLowInd(IdxL) = ParMaule%ColIndEta(ParMaule%IVMUtIl, IEta)
      MCLowVal(IdxL) = LowMaule(ParMaule%IVMUTIL) / ParMaule%ScaleVol

!
!     Upper boundaries simples
!
      IdxU = 0

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMNEH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQMNEH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMOEH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQMOEH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMNRH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQMNRH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMORH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQMORH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMCEH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQMCEH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMEIH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQMEIH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQISDH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQISDH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQIDNH, IEta)
      MCUppVal(IdxU) = UppMaule(ParMaule%IQIDNH)

!     Cota 425 de colbun
      DO IBlo = 1, NBloque(IEta)
         Idx = ParMaule%Extr425ColInd(IBlo, IEta)
         IF (Idx .GT. 0) THEN
            IdxU = IdxU + 1
            MCUppInd(IdxU) = Idx
            IF (VolColbunPrev .GE. ParMaule%Vol425) THEN
               MCUppVal(IdxU) = ParMaule%ExtrMax425
            ELSE
               MCUppVal(IdxU) = 0.0d0
            ENDIF
         ENDIF
      ENDDO

!
!     Asigna igualdades
!
      IdxE = IdxU

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IVMDCEN, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IVMDCEN) / ParMaule%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IVMDEIN, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IVMDEIN) / ParMaule%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IVMDEMT, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IVMDEMT) / ParMaule%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IVMDEAT, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IVMDEAT) / ParMaule%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IVMDRTT, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IVMDRTT) / ParMaule%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQDRMH, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IQDRMH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQMNIH, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IQMNIH)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQHI, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IQHI)

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParMaule%ColIndEta(ParMaule%IQR105, IEta)
      MCUppVal(IdxU) = VarMaule(ParMaule%IQR105)

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
      DO Idx = 1, ParMaule%NumResVol
         LDResMaule(Idx) = LDResMaule(Idx) / ParMaule%ScaleVol
      ENDDO

      CALL ModifBrdRhs(ParMaule%NumFilEta,                  &
     &     ParMaule%FilIndEta(1, IEta), LDResMaule,         &
     &     lp)


      CALL ModifUpp(IdxU, MCUppInd,  MCUppVal, lp)
      CALL ModifLow(IdxL, MCLowInd,  MCLowVal, lp)

      RETURN
      END

!************************************

      SUBROUTINE GetVarMaulePrev(IEtapa, ISimul,                        &
     &     ParMaule, VolIni, VarMaulePrev, VolColbunPrev, Dim)
      USE PLP
      TYPE(PAR_DIMS), INTENT(IN)::  Dim

      TYPE(PAR_MAULE), INTENT(IN) :: ParMaule
      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul
      DOUBLE PRECISION, INTENT(IN) :: VolIni(Dim%Emb)
      DOUBLE PRECISION, INTENT(OUT) :: VarMaulePrev(ParMaule%NumColEta)
      DOUBLE PRECISION, INTENT(OUT) :: VolColbunPrev
!
      DOUBLE PRECISION VolMauleUtil
      INTEGER IVolMaule
      INTEGER IVolColbun
!

      IF (IEtapa .EQ. 1) THEN
!        VarMaulePrev(1:ParMaule%NumColEta) = 0.0d0
!        Usamos valores de archivo. Se supone que no hay que resolver si
!        estamos al principo de un mes o ano, esto viene implicito en el
!        valor inicial

         VarMaulePrev(ParMaule%IVMGEMF) = ParMaule%VGastoElecMenIni
         VarMaulePrev(ParMaule%IVMGEAF) = ParMaule%VGastoElecAnuIni
         VarMaulePrev(ParMaule%IVMGRTF) = ParMaule%VGastoRiegoIni
         VarMaulePrev(ParMaule%IVMGOEF) = ParMaule%VGastoRExtElecIni
         VarMaulePrev(ParMaule%IVMGORF) = ParMaule%VGastoRExtRiegoIni
         VarMaulePrev(ParMaule%IVMDOEF) = ParMaule%VDerRExtElecIni
         VarMaulePrev(ParMaule%IVMDORF) = ParMaule%VDerRExtRiegoIni
         VarMaulePrev(ParMaule%IVMDCEF) = ParMaule%VCompElecIni
         VarMaulePrev(ParMaule%IVMDEIF) = ParMaule%VEconInverIni

!        Definicion de volumen util neto
         IVolMaule = ParMaule%IEmbMaule
         VolMauleUtil = VolIni(IVolMaule) - ParMaule%VEmbalseUtilMin
         VarMaulePrev(ParMaule%IVMUTIL) = VolMauleUtil

!        Rescatamos el volmen de colbun
         IVolColbun = ParMaule%IEmbColbun
         VolColbunPrev = VolIni(IVolColbun)

         RETURN
      ENDIF

      VarMaulePrev(1:ParMaule%NumColEta) =                              &
     &     ParMaule%VarEtaPrev(1:ParMaule%NumColEta, ISimul, IEtapa)

!     Rescatamos el volmen de colbun
      IVolColbun = ParMaule%IEmbColbun
      VolColbunPrev = VolIni(IVolColbun)

      RETURN
      END


!******************
      SUBROUTINE AgrFactMaule(NFila, NCol, ISimul,                      &
     &     IEtapaOri, IEtapaDest, deps, ParMaule,                       &
     &     nzcnt, rmatval, rmatind, pi, LD, rhs,                        &
     &     lpi, FactDBL, ULogCF)

      USE PLP
      USE OSI
      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: NFila
      INTEGER, INTENT(IN):: NCol

      TYPE(PAR_MAULE) ParMaule

      INTEGER ISimul
      INTEGER IEtapaOri
      INTEGER IEtapaDest

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

      INTEGER(C_SIZE_T) lpi
      INTEGER ULogCF
      INTEGER FactDBL


      CHARACTER*12 name
      LOGICAL IsVarCorteMaule


      DO Ind = 1, ParMaule%NumVarEst
         IF ( .NOT. IsVarCorteMaule(ISimul, IEtapaOri, ParMaule, Ind)) THEN
            CYCLE
         ENDIF

         IFila = ParMaule%FilIndEta(Ind, IEtapaOri)
         IF (DxAEQy(pi(IFila), 0.0d0, deps)) THEN
            CYCLE
         ENDIF

         rhs = rhs - pi(IFila)*LD(IFila)

         IF (FactDBL .NE. 0) THEN
            ICol = ParMaule%ColIndEta(Ind, IEtapaOri)
            name = ' '
            CALL osi_lp_getcolname(lpi, ICol - 1, sizeof(name), loc(name))
            WRITE(ULogCF,*) 'cfm: ', name, IFila, pi(IFila), LD(IFila)
         ENDIF

         ICol = ParMaule%ColIndEta(Ind, IEtapaDest)
         nzcnt = nzcnt + 1
         rmatval(nzcnt) = -pi(IFila)
         rmatind(nzcnt) = ICol  - 1
      ENDDO

      RETURN
      END


!*********************************************
      SUBROUTINE InitParMaule(ParMaule, Dim)
      USE PLP, ONLY : PAR_MAULE, PAR_DIMS

      TYPE(PAR_MAULE) ParMaule
      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      ALLOCATE(ParMaule%VarBloNames(ParMaule%DimColBlo))

      ALLOCATE(ParMaule%VarEtaVol(ParMaule%DimColEta))
      ALLOCATE(ParMaule%VarEtaNames(ParMaule%DimColEta))

      ALLOCATE(ParMaule%ColIndEta(ParMaule%DimColEta, Dim%Eta))
      ALLOCATE(ParMaule%FilIndEta(ParMaule%DimFilaEta, Dim%Eta))

      ALLOCATE(ParMaule%TipoEtaDE(Dim%Eta))
      ALLOCATE(ParMaule%TipoEtaDR(Dim%Eta))

      ALLOCATE(ParMaule%DataEta(ParMaule%DimColEta, Dim%Eta))
      ALLOCATE(ParMaule%DataBlo(ParMaule%DimColBlo, Dim%Blo))

      ALLOCATE(ParMaule%VDerElecMenMax(Dim%Eta))
      ALLOCATE(ParMaule%PRiegoMauleAnual(12, Dim%Year))
      ALLOCATE(ParMaule%QRiego105Anual(12, Dim%Year))

      ALLOCATE(ParMaule%Extr425ColInd(Dim%IBlo, Dim%Eta))
      ALLOCATE(ParMaule%VColbunColInd(Dim%Eta))

      ALLOCATE(ParMaule%VarEtaPrev(ParMaule%DimColEta, Dim%Simul, Dim%Eta))

      ALLOCATE(ParMaule%EnRegimenNormal(Dim%Simul, Dim%Eta))
      ALLOCATE(ParMaule%CompElecMaxed(Dim%Simul, Dim%Eta))

      RETURN
      END

      SUBROUTINE LeeMaule(NArcMauleN, FConvMaule,                       &
     &     NCentral, NFlujo, NCenEmb,                                   &
     &     CenNom, CenInd, NEtapa, Mes,                                 &
     &     FiltNCen, FiltEmbInd, ScaleVol,                              &
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     ParMaule, ULog, Dim)
      USE PLP
      TYPE(PAR_DIMS), INTENT(IN) ::  Dim

      INCLUDE 'machcons.fpp'

      CHARACTER*(*) NArcMauleN
      INTEGER FConvMaule
      CHARACTER*48 CenNom(Dim%Cen)
      TYPE(PAR_MAULE) ParMaule
      INTEGER NEtapa
      INTEGER Mes(Dim%Eta)
      INTEGER ULog
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      DOUBLE PRECISION ScaleVol(Dim%Emb)
      INTEGER CenInd(Dim%Cen)

      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb


!     variables locales
      CHARACTER*12 AuxVar
      CHARACTER*12 CVolB
      CHARACTER*48 NomCentral
      CHARACTER*42 Objeto

      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning

      INTEGER NumCen
      INTEGER IFilt
      INTEGER IEta
      INTEGER NCentral
      INTEGER NFlujo
      INTEGER NCenEmb
      INTEGER Idx
      INTEGER I
      INTEGER ICen

      EXTERNAL Abrir
      INTEGER Abrir

      LOGICAL EnRiego
      INTEGER ICol
      INTEGER IFil

      DOUBLE PRECISION, ALLOCATABLE :: PRetRiego(:)
      LOGICAL, ALLOCATABLE :: HolgRiego(:)
      INTEGER, ALLOCATABLE :: ICenRetRiego(:)
      INTEGER NumRetRiego

      INTEGER NYear
      INTEGER IYear

      LOGICAL DxAEQy

      CALL InitParMaule(ParMaule, Dim)

      FStop = .FALSE.

      ParMaule%ColIndEta = 0
      ParMaule%FilIndEta = 0

      ParMaule%VarBloNames(ParMaule%IQMNE) = 'qmne'
      ParMaule%VarBloNames(ParMaule%IQMNR) = 'qmnr'
      ParMaule%VarBloNames(ParMaule%IQMOE) = 'qmoe'
      ParMaule%VarBloNames(ParMaule%IQMOR) = 'qmor'
      ParMaule%VarBloNames(ParMaule%IQMCE) = 'qmce'
      ParMaule%VarBloNames(ParMaule%IQMEI) = 'qmei'
      ParMaule%VarBloNames(ParMaule%IQIDN) = 'qidn'
      ParMaule%VarBloNames(ParMaule%IQISD) = 'qisd'
      ParMaule%VarBloNames(ParMaule%IQTER) = 'qter'

      ParMaule%VarEtaNames(ParMaule%IVMGEMF) = 'vmgemf'
      ParMaule%VarEtaNames(ParMaule%IVMGEAF) = 'vmgeaf'
      ParMaule%VarEtaNames(ParMaule%IVMGRTF) = 'vmgrtf'
      ParMaule%VarEtaNames(ParMaule%IVMGOEF) = 'vmgoef'
      ParMaule%VarEtaNames(ParMaule%IVMGORF) = 'vmgorf'
      ParMaule%VarEtaNames(ParMaule%IVMDOEF) = 'vmdoef'
      ParMaule%VarEtaNames(ParMaule%IVMDORF) = 'vmdorf'
      ParMaule%VarEtaNames(ParMaule%IVMDCEF) = 'vmdcef'
      ParMaule%VarEtaNames(ParMaule%IVMDEIF) = 'vmdeif'
      ParMaule%VarEtaNames(ParMaule%IVMDCEN) = 'vmdcen'
      ParMaule%VarEtaNames(ParMaule%IVMDEMT) = 'vmdemt'
      ParMaule%VarEtaNames(ParMaule%IVMDEAT) = 'vmdeat'
      ParMaule%VarEtaNames(ParMaule%IVMDRTT) = 'vmdrtt'
      ParMaule%VarEtaNames(ParMaule%IVMUTIL) = 'vmutil'
      ParMaule%VarEtaNames(ParMaule%IVMDEIN) = 'vmdein'
      ParMaule%VarEtaNames(ParMaule%IVMREB)  = 'vmreb'
      ParMaule%VarEtaNames(ParMaule%IQMNIH ) = 'qmnih'
      ParMaule%VarEtaNames(ParMaule%IQMNEH ) = 'qmneh'
      ParMaule%VarEtaNames(ParMaule%IQMNRH ) = 'qmnrh'
      ParMaule%VarEtaNames(ParMaule%IQMOEH ) = 'qmoeh'
      ParMaule%VarEtaNames(ParMaule%IQMORH ) = 'qmorh'
      ParMaule%VarEtaNames(ParMaule%IQMCEH ) = 'qmceh'
      ParMaule%VarEtaNames(ParMaule%IQMEIH ) = 'qmeih'
      ParMaule%VarEtaNames(ParMaule%IQIDNH ) = 'qidnh'
      ParMaule%VarEtaNames(ParMaule%IQISDH ) = 'qisdh'
      ParMaule%VarEtaNames(ParMaule%IQTERH ) = 'qterh'
      ParMaule%VarEtaNames(ParMaule%IQDRMH ) = 'qdrmh'
      ParMaule%VarEtaNames(ParMaule%IQDRAH ) = 'qdrah'
      ParMaule%VarEtaNames(ParMaule%IQHI )   = 'qhi'
      ParMaule%VarEtaNames(ParMaule%IQARMR ) = 'qarmr'
      ParMaule%VarEtaNames(ParMaule%IQMAUH ) = 'qmauleh'
      ParMaule%VarEtaNames(ParMaule%IQINVH ) = 'qinverh'
      ParMaule%VarEtaNames(ParMaule%IQR105 ) = 'qr105'
      ParMaule%VarEtaNames(ParMaule%IQA105 ) = 'qa105'
      ParMaule%VarEtaNames(ParMaule%IQNINV ) = 'qninv'
      ParMaule%VarEtaNames(ParMaule%IQHINV ) = 'qhinv'
      ParMaule%VarEtaNames(ParMaule%IQHNEIN) = 'qhnein'
      ParMaule%VarEtaNames(ParMaule%IQHEIN) = 'qhein'


!     Apertura de archivo
      URead = Abrir(NArcMauleN, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leemaule: Error, no existe archivo ',           &
     &        NArcMauleN, '.'
         WRITE(ULog, '(3A)') 'leemaule: Error, no existe archivo ',        &
     &        NArcMauleN, '.'
         STOP 1
      ENDIF

!     Comentario
      READ(URead, '(A1)') AuxVar

!     Central Maule
      Objeto = 'central Maule'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,                     &
     &     CenNom, NCenEmb, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF
      ParMaule%IEmbMaule = NumCen
      ParMaule%IAflMaule = NumCen
      ParMaule%IQVerMaule = NCentral + 2*NFlujo + NumCen
      ParMaule%IQRebMaule = 0
      IF (FVertReb) THEN
         DO Idx = 1, NEmbVReb
            IF (NumCen .EQ. EmbVRebInd(Idx)) THEN
               ParMaule%IQRebMaule = Idx + 2*NCenEmb
            ENDIF
         ENDDO
      ENDIF

      ParMaule%ScaleVol  = ScaleVol(NumCen)

!     Central Invernada
      Objeto = 'central Invernada'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,                    &
     &     CenNom, NCenEmb, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF
      ParMaule%ILagInvern = NumCen
      ParMaule%IAflInvern = NumCen
      ParMaule%IQVerInvern = NCentral + 2*NFlujo + NumCen
      ParMaule%IQRebInvern = 0
      IF (FVertReb) THEN
         DO Idx = 1, NEmbVReb
            IF (NumCen .EQ. EmbVRebInd(Idx)) THEN
               ParMaule%IQRebInvern = Idx + 2*NCenEmb
            ENDIF
         ENDDO
      ENDIF


!     buscamos indice de variable de filtracion
      ParMaule%IFiltInvern = 0
      DO IFilt = 1, FiltNCen
         IF (FiltEmbInd(IFilt) .EQ. ParMaule%ILagInvern) THEN
            ParMaule%IFiltInvern = IFilt
         ENDIF
      ENDDO

!     Central Melado
      Objeto = 'central Melado'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,                    &
     &     CenNom, NCentral, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF
      ParMaule%IEmbMelado = NumCen

!     Central Colbun
      Objeto = 'central Colbun'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,                    &
     &     CenNom, NCenEmb, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF
      ParMaule%IEmbColbun = NumCen
      ParMaule%ScaleVolColbun  = ScaleVol(NumCen)

!     buscamos indice de variable de filtracion
      ParMaule%IFiltColbun = 0
      DO IFilt = 1, FiltNCen
         IF (FiltEmbInd(IFilt) .EQ. ParMaule%IEmbColbun) THEN
            ParMaule%IFiltColbun = IFilt
         ENDIF
      ENDDO

!     Definicion de afluentes hoya intemedia
      Objeto ='afluente hoya intermedia'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%NumAflHoyaInt
      READ(URead, '(A1)') AuxVar

      ALLOCATE(ParMaule%IAflHoyaInt(ParMaule%NumAflHoyaInt))

      ICen = 0
      DO Idx=1, ParMaule%NumAflHoyaInt
         READ(URead, *) NomCentral
         CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &        CenNom, NCentral, Objeto, ULog)
         IF (NumCen .GT. 0) THEN
            ICen = ICen + 1
            ParMaule%IAflHoyaInt(ICen) = NumCen
         ENDIF
      ENDDO
      ParMaule%NumAflHoyaInt = ICen

!     VEmbalseUtilMin
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VEmbalseUtilMin

!     VReservaExtraord
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VReservaExtraord

!     VReservaOrdinaria
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VReservaOrdinaria

!     VDerRiegoTempMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VDerRiegoTempMax

!     VDerElectAnuMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VDerElecAnuMax

!     VCompElecMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VCompElecMax

!     VGastoElecMenIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VGastoElecMenIni

!     VGastoElecAnuIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VGastoElecAnuIni

!     VGastoRiegoIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VGastoRiegoIni

!     VGastoRExtElecIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VGastoRExtElecIni

!     VGastoRExtRiegoIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VGastoRExtRiegoIni

!     VDerRExtElecIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VDerRExtElecIni

      IF (ParMaule%VDerRExtElecIni .LT. ParMaule%VGastoRExtElecIni) THEN
         WRITE(6, '(2A)') 'leemaule: Error, ', &
     &        'VDerRExtElecIni .LT. VGastoRExtElecIni'
         WRITE(ULog, '(2A)') 'leemaule: Error, ', &
     &        'VDerRExtElecIni .LT. VGastoRExtElecIni'
         STOP 1
      ENDIF

!     VDerRExtRiegoIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VDerRExtRiegoIni

      IF (ParMaule%VDerRExtRiegoIni .LT. ParMaule%VGastoRExtRiegoIni) THEN
         WRITE(6, '(2A)') 'leemaule: Error, ', &
     &        'VDerRExtRiegoIni .LT. VGastoRExtRiegoIni'
         WRITE(ULog, '(2A)') 'leemaule: Error, ', &
     &        'VDerRExtRiegoIni .LT. VGastoRExtRiegoIni'
         STOP 1
      ENDIF

      ParMaule%UsaCorteOptim = FConvMaule .GT. 1

!     VCompElecIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VCompElecIni

!     VEconInverIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%VEconInverIni

!     GastoElecMenMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%GastoElecMenMax

!     GastoElecDiaMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%GastoElecDiaMax

!     Modulacion porcentual de GastoElecDiaMax mensual
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParMaule%PGastoElecDiaMaxMenRes(I), I=1, 12)
      ParMaule%PGastoElecDiaMaxMenRes(1:12)=                            &
     &     0.01d0*ParMaule%PGastoElecDiaMaxMenRes(1:12)

!     Descuenta el caudal de derechos electricos del balance de armerillo
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%DescGastoElecArmr


!     Gasto Riego Maximo [m3/seg]
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%GastoRiegoMax

      DO IEta = 1, NEtapa
         ParMaule%VDerElecMenMax(IEta) = ParMaule%GastoElecMenMax &
     &        *HorasMesHid(Mes(IEta))*FactTiempoH
      ENDDO

!     Caudal Minimo Embalsamiento [m3/seg],  Relajacion Embalsar,
!     No Embalsar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%GastoMauleMin, &
     &   ParMaule%RelaxInvern, ParMaule%NoDesembInv

!     Valor riego servido
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%ValorRiegoMaule, ParMaule%ValorRiego105

!     Costo riego no servido
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%CostoRiegoNSMaule, ParMaule%CostoRiegoNS105

!     Porcentajes de riego mensual Maule
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParMaule%PRiegoMaule(I), I=1, 12)
      ParMaule%PRiegoMaule(1:12) = 0.01d0*ParMaule%PRiegoMaule(1:12)

      DO IYear = 1, Dim%Year
         ParMaule%PRiegoMauleAnual(1:12, IYear) =                       &
     &        ParMaule%PRiegoMaule(1:12)
      ENDDO

!     Datos manuales
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NYear
      READ(URead, '(A1)') AuxVar
      DO IYear = 1, NYear
         READ(URead, *) (ParMaule%PRiegoMauleAnual(I, IYear), I=1, 12)
         ParMaule%PRiegoMauleAnual(1:12, IYear) = &
     &        0.01d0*ParMaule%PRiegoMauleAnual(1:12, IYear)
      ENDDO

!     Modulacion automatica
!     AnoModQRiegoRes, ano que comienza la modulacion, valor cero desactiva
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%AnoModQRiegoRes

!     FactCauFutRiego
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%FactCauFutRiego

!     Modulacion incluye a la Res 105
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%AutModRes105

!     VolMaxDerRiegoAcum
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParMaule%VolMaxDerRiegoAcum(I), I=1, 12)

!     VolMaxDerRiegoAcum
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParMaule%DiasTempRiegoAcum(I), I=1, 12)

!     Porcentaje Caudal de derechos entrantes electricos
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%PorcQNIDE
      ParMaule%PorcQNIDE = 0.01d0*ParMaule%PorcQNIDE

!     Porcentaje Caudal de derechos entrantes riego
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%PorcQNIDR
      ParMaule%PorcQNIDR = 0.01d0*ParMaule%PorcQNIDR

!     Q Riego mensual res 105
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParMaule%QRiego105(I), I=1, 12)

      DO IYear = 1, Dim%Year
         ParMaule%QRiego105Anual(1:12, IYear) = ParMaule%QRiego105(1:12)
      ENDDO

!     Datos manuales
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NYear
      READ(URead, '(A1)') AuxVar
      DO IYear = 1, NYear
         READ(URead, *) (ParMaule%QRiego105Anual(I, IYear), I=1, 12)
      ENDDO


!     Centrales de retiros de reigo
      Objeto ='central retiro riego'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NumRetRiego
      READ(URead, '(A1)') AuxVar

      ALLOCATE(ParMaule%ICenRetRiego(NumRetRiego))
      ALLOCATE(ParMaule%PRetRiego(NumRetRiego))
      ALLOCATE(ParMaule%HolgRiego(NumRetRiego))

      ALLOCATE(ParMaule%IQRIHC(NumRetRiego))
      ALLOCATE(ParMaule%IQRHHC(NumRetRiego))
      ALLOCATE(ParMaule%IQRIHF(NumRetRiego))

      ALLOCATE(PRetRiego(NumRetRiego))
      ALLOCATE(HolgRiego(NumRetRiego))
      ALLOCATE(ICenRetRiego(NumRetRiego))


      DO Idx=1, NumRetRiego
         READ(URead, *) NomCentral
         CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &        CenNom, NCentral, Objeto, ULog)
         ICenRetRiego(Idx) = NumCen
      ENDDO

!     Porc Ret Riego de Centrales
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (PRetRiego(I), I=1, NumRetRiego)
!     Retiro de riego incluye holgura o no
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (HolgRiego(I), I=1, NumRetRiego)

!     Bocatoma Canelon
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &     CenNom, NCentral, Objeto, ULog)
      ParMaule%IQCanelon = NumCen

!     Costo Civil asociado a Canelon
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%CostoCanelon

!     Numero de extraccion
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%IExtr425

!     Volumen Cota Disponibildad 425
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParMaule%Vol425

!     Economias Invernada: Uso en Reserva, Acumulables, Costo Almac
      READ(URead, '(A1)') AuxVar
      READ(URead, *) &
     &     ParMaule%EconInvernUsoEnReserva, &
     &     ParMaule%EconInvernAcumAnual, &
     &     ParMaule%EconInvernCosto

!     Costo de embalsar (rstriccion por apertura maule)
      READ(URead, '(A1)') AuxVar
      READ(URead, *) &
     &     ParMaule%CostoEmbalsar, &
     &     ParMaule%CostoNoEmbalsar

      IF (ParMaule%UsaCorteOptim) THEN
         ParMaule%PDLDAcNFila = ParMaule%DimPDLDAcFila
         ParMaule%PDLDAcNCol = ParMaule%DimPDLDAcCol
      ELSE
         ParMaule%PDLDAcNFila = 0
         ParMaule%PDLDAcNCol = 0
      ENDIF


!     configuracion de riego
      ICen = 0
      DO Idx=1, NumRetRiego
         IF (ICenRetRiego(Idx) .GT. 0) THEN
            ICen = ICen + 1
            ParMaule%ICenRetRiego(ICen) = ICenRetRiego(Idx)
            ParMaule%PRetRiego(ICen) = 0.01d0*PRetRiego(Idx)
            ParMaule%HolgRiego(ICen) = HolgRiego(Idx)
         ENDIF
      ENDDO
      ParMaule%NumRetRiego = ICen

!     Configura riego
      ICol = ParMaule%NumColEta
      IFil = ParMaule%NumFilEta
      DO Idx=1, ParMaule%NumRetRiego
         CVolB = ' '

         CALL Num2Char(CenInd(ParMaule%ICenRetRiego(Idx)), CVolB, No, DimLargo)

         ICol = ICol + 1
         ParMaule%IQRIHC(Idx) = ICol
         ParMaule%VarEtaNames(ICol) = 'qri' // CVolB

         IF (ParMaule%HolgRiego(Idx)) THEN
            ICol = ICol + 1
            ParMaule%IQRHHC(Idx) = ICol
            ParMaule%VarEtaNames(ICol) = 'qrh' // CVolB
         ELSE
            ParMaule%IQRHHC(Idx) = 0
         ENDIF

         IFil = IFil + 1
         ParMaule%IQRIHF(Idx) = IFil

         ParMaule%NumColEta = ICol
         ParMaule%NumFilEta = IFil
      ENDDO

      DO Idx = 1, ParMaule%NumColEta
         ParMaule%VarEtaVol(Idx) = (Idx .LE. ParMaule%NumVarVol)
      ENDDO

!        Suponemos que los datos de Meses y Agnos refieren a
!        calendario hidrologico, es decir, el mes uno corresponde a
!        abril y el agno calendario comienza en el mes 10.
!
!        Tipo de Etapa para gasto medio
!        INICIOANO:  Etapa coincide con el inicio de mes y de agno
!        INICIOMES:  Etapa coincide con el inicio de mes
!        INTRAETA:  Etapa dentro de un mes

      IF (Mes(1) .EQ. ENEROHID) THEN
         ParMaule%TipoEtaDE(1) = INICIOANO
      ELSE
         ParMaule%TipoEtaDE(1) = INICIOMES
      ENDIF

      DO IEta = 2, NEtapa
         IF (Mes(IEta) .EQ. Mes(IEta-1)) THEN
!           Estamos dentro de un mes
            ParMaule%TipoEtaDE(IEta) = INTRAETA
         ELSE
            IF (Mes(IEta) .EQ. ENEROHID) THEN
               ParMaule%TipoEtaDE(IEta) = INICIOANO
            ELSE
               ParMaule%TipoEtaDE(IEta) = INICIOMES
            ENDIF
         ENDIF
      ENDDO

!     Mes inicio temporada de riego
      ParMaule%MesRiegoIni = 1
      ParMaule%MesRiegoFin = 12
      DO I =2, 12
         IF (DxAEQy(ParMaule%PRiegoMaule(I-1), 0.0d0, 0.0d0) &
     &        .AND. .NOT. DxAEQy(ParMaule%PRiegoMaule(I), 0.0d0, 0.0d0)) THEN
            ParMaule%MesRiegoIni = I
            EXIT
         ENDIF
      ENDDO

      DO I =1, 11
         IF (.NOT. DxAEQy(ParMaule%PRiegoMaule(I), 0.0d0, 0.0d0) &
     &        .AND. DxAEQy(ParMaule%PRiegoMaule(I+1), 0.0d0, 0.0d0)) THEN
            ParMaule%MesRiegoFin = I
            EXIT
         ENDIF
      ENDDO


      DO IEta = 1, NEtapa
         EnRiego = (Mes(IEta) .GE. ParMaule%MesRiegoIni) .OR.           &
     &        (Mes(IEta) .LE. ParMaule%MesRiegoFin)

         IF (EnRiego) THEN
            ParMaule%TipoEtaDR(IEta) = INTRARIE
         ELSE
            ParMaule%TipoEtaDR(IEta) = 0
         ENDIF

         IF (ParMaule%TipoEtaDE(IEta) .NE. INTRAETA) THEN
            IF (Mes(IEta) .EQ. ParMaule%MesRiegoIni) THEN
               ParMaule%TipoEtaDR(IEta) = INICIOANO
            ENDIF
         ENDIF
      ENDDO

      DEALLOCATE(PRetRiego)
      DEALLOCATE(HolgRiego)
      DEALLOCATE(ICenRetRiego)

      RETURN
      END


!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDMauleN(NArcNom, ISimul,                         &
     &     NBloque, BloDur, BloEta, TipoEtapa, Mes,                      &
     &     EmbFEsc, ParMaule, Dim, ULog)
!
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
      TYPE(PAR_MAULE) ParMaule
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
         WRITE(UWrite, '(A, '','', $)') 'TipoEtaDE'
         WRITE(UWrite, '(A, '','', $)') 'TipoEtaDR'
         WRITE(UWrite, '(A, '','', $)') 'HorasAcum'
         WRITE(UWrite, '(A, '','', $)') 'EmbFac'
         DO Idx = 1, ParMaule%NumColEta
            WRITE(UWrite, '(A, '', '', $)') ParMaule%VarEtaNames(Idx)
         ENDDO
         DO Idx = 1, ParMaule%NumColBlo
            WRITE(UWrite, '(A, '', '', $)') ParMaule%VarBloNames(Idx)
         ENDDO
         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      ICen = ParMaule%IEmbMaule
      VolScale = 1D3/EmbFEsc(ICen)
      Time = 0.0d0
      DO IBlo = 1, NBloque
         IEta = BloEta(IBlo)
         IF (ISimul .EQ. 0) THEN
            WRITE(UWrite, '(A, $)') 'MEDIA, '
         ELSE
            WRITE(UWrite, '(A, I3, '', '', $)') 'Sim', ISimul
         ENDIF
         WRITE(UWrite, '(I4, '', '', $)') IBlo
         WRITE(UWrite, '(I4, '', '', $)') IEta
         WRITE(UWrite, '(I3, '', '', $)') Mes(IEta)
         WRITE(UWrite, '(A, '', '', $)')  TipoEtapa(IEta)
         WRITE(UWrite, '(I3, '', '', $)') ParMaule%TipoEtaDE(IEta)
         WRITE(UWrite, '(I3, '', '', $)') ParMaule%TipoEtaDR(IEta)
         WRITE(UWrite, '(F7.1, '', '', $)') Time
         WRITE(UWrite, '(E9.2, '', '', $)') EmbFEsc(ICen)

         DO Idx = 1, ParMaule%NumColEta
            IF (.NOT. ParMaule%VarEtaVol(Idx)) THEN
               WRITE(UWrite, '(F8.3, '', '', $)')                       &
     &              ParMaule%DataEta(Idx, IEta)
            ELSE
               WRITE(UWrite, '(F10.7, '', '', $)')                       &
     &              ParMaule%DataEta(Idx, IEta)*VolScale
            ENDIF
         ENDDO
         DO Idx = 1, ParMaule%NumColBlo
            WRITE(UWrite, '(F8.3, '', '', $)')                          &
     &           ParMaule%DataBlo(Idx, IBlo)
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

      LOGICAL FUNCTION IsVarCorteMaule(ISimul, IEtapaOri, ParMaule, Ind)
      USE PLP

      INTEGER, INTENT(IN):: Ind
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: IEtapaOri
      TYPE(PAR_MAULE), INTENT(IN):: ParMaule

      LOGICAL ReservaOrdinariaActiva
      LOGICAL EnRiego


      IF (Ind .GT. ParMaule%DimPDLDAcCol) THEN
         IsVarCorteMaule = .False.
         RETURN
      ENDIF

      IsVarCorteMaule = .TRUE.

      EnRiego = ParMaule%TipoEtaDR(IEtapaOri) .NE. 0
      ReservaOrdinariaActiva = .not. ParMaule%EnRegimenNormal(ISimul, IEtapaOri)

!     cuando no estamos en riego
      IF (.NOT. EnRiego) THEN
         IF (Ind .EQ. ParMaule%IVMGRTF) THEN
            IsVarCorteMaule = .FALSE.
         ENDIF
      ENDIF

      IF (ParMaule%EnRegimenNormal(ISimul, IEtapaOri)) THEN
!        Anulamos los volumenes acumulados de la reservas
         IF ((Ind .EQ. ParMaule%IVMGOEF) &
     &        .OR. (Ind .EQ. ParMaule%IVMGORF)  &
     &        .OR. (Ind .EQ. ParMaule%IVMDOEF) &
     &        .OR. (Ind .EQ. ParMaule%IVMDORF)) THEN
            IsVarCorteMaule = .FALSE.
         ENDIF

      ENDIF


!     revisamos condiciones de etapas de derechos electricas
      IF (ParMaule%TipoEtaDE(IEtapaOri) .NE. INTRAETA) THEN
         IF (Ind .EQ. ParMaule%IVMGEMF) THEN
            IsVarCorteMaule = .FALSE.
         ENDIF
      ENDIF

!     Reiniciamos contador de los gasto anuales electricos
      IF ((ParMaule%TipoEtaDE(IEtapaOri) .EQ. INICIOANO)) THEN
         IF (Ind .EQ. ParMaule%IVMGEAF)  THEN
            IsVarCorteMaule = .FALSE.
         ENDIF

         IF (ParMaule%CompElecMaxed(ISimul, IEtapaOri)) THEN
            IF (Ind .EQ. ParMaule%IVMDCEF) THEN
               IsVarCorteMaule = .FALSE.
            ENDIF
         ENDIF
      ENDIF


!     Reiniciamos contador de los gasto de riego de temporada
      IF ((ParMaule%TipoEtaDR(IEtapaOri) .EQ. INICIOANO)) THEN
         IF (Ind .EQ. ParMaule%IVMGRTF)  THEN
            IsVarCorteMaule = .FALSE.
         ENDIF
      ENDIF

!     inicio de agno electrico/calendario
      IF ((ParMaule%TipoEtaDE(IEtapaOri) .EQ. INICIOANO)) THEN
         IF (ReservaOrdinariaActiva) THEN
            IF ((Ind .EQ. ParMaule%IVMGOEF)   &
     &           .OR. (Ind .EQ. ParMaule%IVMGORF)) THEN
               IsVarCorteMaule = .FALSE.
            ENDIF
         ENDIF

         IF (.NOT. ParMaule%EconInvernAcumAnual) THEN
!     Si no se pueden acumular economias anuales, se pierden
            IF (Ind .EQ. ParMaule%IVMDEIF) THEN
               IsVarCorteMaule = .FALSE.
            ENDIF
         ENDIF

      ENDIF


      RETURN
      END
