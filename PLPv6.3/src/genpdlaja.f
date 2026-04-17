!*********************************
!     Matriz Invariante A Vol Embalses
!*********************************
      SUBROUTINE GenPDLajaBloA(IEta, IBloque, blodur, etadur,           &
     &     ParLaja, CenOffset, FiltVarColInd,                           &
     &     FEmbOffset, COffset, FOffset,                                &
     &     A, PDNCol, PDNombre, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_LAJA, DimLargo, No
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!******************
      INTEGER, INTENT(IN) :: PDNCol
      INTEGER, INTENT(IN) :: IEta
      INTEGER, INTENT(IN) :: CenOffset
      INTEGER, INTENT(IN) :: FEmbOffset
      INTEGER, INTENT(IN) :: IBloque
      TYPE(PAR_LAJA), INTENT(IN) :: ParLaja
      DOUBLE PRECISION, INTENT(IN) :: blodur
      DOUBLE PRECISION, INTENT(IN) :: etadur
      INTEGER, INTENT(IN) :: FiltVarColInd(Dim%EmbFilt, Dim%Eta)
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
      INTEGER FiltVarIndLaja
      INTEGER IEmbLaja
      INTEGER FOffseti
      INTEGER Idx
      INTEGER IVol
      INTEGER FCentRiego
      
      IVol = ParLaja%IEmbLaja
      
      CALL Num2Char(IBloque, CVolB, No, DimLargo)
      
      DO Idx = 1, ParLaja%NumColBlo
         Nombre = ParLaja%VarBloNames(Idx)
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
      !    qg_i_j = l_qgap_j + l_qgdg_j + l_qges_j + l_qger_j 
      !
      ! donde 
      !       'i' es el indice de la central El Toro
      !       'j' es el indice del bloque
      !
      !       lqgap_j Q Turbinado a cargo de las captaciones de alto polcura
      !       lqgdg_j Q Turbinado a cargo de las derechos de gastos medios
      !       lqges_j Q Turbinado a cargo de las economias normales de endesa
      !       lqger_j Q Turbinado a cargo de las economias de reserva
      ! 
      IEmbLaja = CenOffset + ParLaja%IEmbLaja
      CALL Am_set(A, IEmbLaja, FOffseti + 1, -1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQGES, FOffseti + 1, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQGER, FOffseti + 1, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQGDG, FOffseti + 1, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQG50, FOffseti + 1, 1.0d0)

      IF (ParLaja%IAflAltoPolcura .GT. 0) THEN
         CALL Am_set(A, COffset + ParLaja%IQGAP, FOffseti + 1, 1.0d0)
      ENDIF

      FOffseti = FOffseti + 1
      
      ! Definicion de caudal de gasto medio total considerando
      !
      !  lqdgn_j = lqgdg_j + qf_i 
      !
      ! donde 
      !       'i' es el indice de la central El Toro
      !       'k' es el indice de la central Alto Polcura
      !       'j' es el indice del bloque
      !
      IF (ParLaja%IFiltLaja .GT. 0) THEN
         FiltVarIndLaja = FiltVarColInd(ParLaja%IFiltLaja, IEta)
         IF (FiltVarIndLaja .GT. 0) THEN
            CALL Am_set(A, FiltVarIndLaja, FOffseti + 1, -1.0d0)
         ENDIF
      ENDIF
      CALL Am_set(A, COffset + ParLaja%IQGDG, FOffseti + 1, -1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQDGN, FOffseti + 1, 1.0d0)

      FOffseti = FOffseti + 1


!     Agrega el gasto contabilizado al acumulado gasto horario 
      CALL Am_set(A, COffset + ParLaja%IQDGN, ParLaja%FilIndEta(ParLaja%IQDGNH_F, IEta), -blodur/etadur)

!     Agrega el uso de economias al acumulado horario 
      CALL Am_set(A, COffset + ParLaja%IQGES, ParLaja%FilIndEta(ParLaja%IQGESH_F, IEta), -blodur/etadur)

!     Agrega el uso de economias al acumulado horario 
      CALL Am_set(A, COffset + ParLaja%IQGDG, ParLaja%FilIndEta(ParLaja%IQGDGH_F, IEta), -blodur/etadur)

!     Agrega el uso de economias de reserva al acumulado horario 
      CALL Am_set(A, COffset + ParLaja%IQGER, ParLaja%FilIndEta(ParLaja%IQGERH_F, IEta), -blodur/etadur)

!     Agrega el caudal usado de alto polcura al acumulado horario 
      CALL Am_set(A, COffset + ParLaja%IQGAP, ParLaja%FilIndEta(ParLaja%IQGAPH_F, IEta), -blodur/etadur)

!     Agrega el caudal usado de regla 50cm al acumulado horario 
      CALL Am_set(A, COffset + ParLaja%IQG50, ParLaja%FilIndEta(ParLaja%IQG50H_F, IEta), -blodur/etadur)

!     Agrega el caudal turbinado al acumulado horario
      CALL Am_set(A, IEmbLaja, ParLaja%FilIndEta(ParLaja%IQGH_F, IEta), -blodur/etadur)

!     Agrega caudal de riego extraido a la ecuacion de la central de riego
      DO Idx = 1, ParLaja%NumRetRiego
         FCentRiego = FEmbOffset + ParLaja%ICenRetRiego(Idx)
         CALL Am_set(A, COffset + ParLaja%IQRI(Idx), FCentRiego, 1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQRI(Idx),                         &
     &        ParLaja%FilIndEta(ParLaja%IQRIHF(Idx), IEta),            &
     &        -blodur/etadur)
      ENDDO         

!***************
!     Offsets Finales
!***************
      COffset = COffset + ParLaja%NumColBlo
      FOffset = FOffset + ParLaja%NumFilBlo
      
      RETURN
      END


!**************************
!     FO y Limites Vol Embalses
!**************************
      SUBROUTINE GenPDLajaBloFO(ParLaja,                                &
     &     BloDur, FPhi,                                                &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)

      USE PLP, ONLY : PAR_DIMS, PAR_LAJA

      USE OSI

!     Variables Globales
!******************
      DOUBLE PRECISION BloDur
      DOUBLE PRECISION FPhi

      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)
      TYPE(PAR_LAJA) ParLaja
      !
      INTEGER I

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + ParLaja%NumColBlo) = 0.0d0

!     costos variables para caudales de generacion que permiten
!     ordenarlos segun preferencias de operacion
      DO I = 1, ParLaja%IQG50
         FO(COffset + I) = ParLaja%CQVar(I) * BloDur/FPhi
      ENDDO

 
!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + 1 : COffset + ParLaja%NumColBlo) = DINFTY
      LowBnd(COffset + 1 : COffset + ParLaja%NumColBlo) = 0.0d0
      
      RETURN
      END

!**********************************


      SUBROUTINE GenPDLajaEtaA(IEta, ParLaja, etadur,                   &
     &     FiltVarColInd, VolOffSet, COffset, FOffset,                  &
     &     A, PDNCol, PDNFila, PDNombre, Sentido, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_LAJA
      USE A_MATRIX

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Variables Globales
!******************
      INTEGER PDNCol, PDNFila
      TYPE(PAR_LAJA) ParLaja
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
      INTEGER FiltVarIndLaja
      DOUBLE PRECISION etadursc

      IVol = ParLaja%IEmbLaja
     
      DO Idx = 1, ParLaja%NumColEta
         Nombre = ParLaja%VarEtaNames(Idx)
         Nombre = fconcat('l_', Nombre)
         PDNombre(COffset + Idx) = Nombre
      ENDDO

      DO Idx = 1, ParLaja%NumColEta
         ParLaja%ColIndEta(Idx, IEta) = COffset + Idx
      ENDDO
      
      DO Idx = 1, ParLaja%NumFilEta
         ParLaja%FIlIndEta(Idx, IEta) = FOffset + Idx
      ENDDO

      etadursc = etadur/ParLaja%ScaleVol
      ! Gasto mensual
      CALL Am_set(A, COffset + ParLaja%IQDGNH, FOffset + ParLaja%IVDGMF_F, -etadursc)

      CALL Am_set(A, COffset + ParLaja%IVDGMF, FOffset + ParLaja%IVDGMF_F, 1.0d0)

      ! Gasto anual
      CALL Am_set(A, COffset + ParLaja%IQDGNH, FOffset + ParLaja%IVDGAF_F, -etadursc)

      CALL Am_set(A, COffset + ParLaja%IVDGAF, FOffset + ParLaja%IVDGAF_F, 1.0d0)
      
      ! Economias anualues
      CALL Am_set(A, COffset + ParLaja%IQGESH, FOffset + ParLaja%IVESF_F, etadursc)

      IF (ParLaja%FVertEcoAct) THEN
         CALL Am_set(A, COffset + ParLaja%IVRBES, FOffset + ParLaja%IVESF_F, 1.0d0)
      ENDIF
      CALL Am_set(A, COffset + ParLaja%IVESN, FOffset + ParLaja%IVESF_F, -1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVESF, FOffset + ParLaja%IVESF_F, 1.0d0)

      ! Economias reserva totales
      CALL Am_set(A, COffset + ParLaja%IQGERH, FOffset + ParLaja%IVERF_F, etadursc)

      CALL Am_set(A, COffset + ParLaja%IVERN, FOffset + ParLaja%IVERF_F, -1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVERF, FOffset + ParLaja%IVERF_F, 1.0d0)

      ! Economias alto polcura totales
      CALL Am_set(A, COffset + ParLaja%IQGAPH, FOffset + ParLaja%IVAPF_F, etadursc)

      CALL Am_set(A, COffset + ParLaja%IVAPN, FOffset + ParLaja%IVAPF_F, -1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVAPF, FOffset + ParLaja%IVAPF_F, 1.0d0)

      ! Volumen de derechos anuales
      CALL Am_set(A, COffset + ParLaja%IVDAF, FOffset + ParLaja%IVDAF_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVDAD, FOffset + ParLaja%IVDAF_F, 1.0d0)

      ! Volumen de derechos mensuales
      CALL Am_set(A, COffset + ParLaja%IVDMF, FOffset + ParLaja%IVDMF_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVDMD, FOffset + ParLaja%IVDMF_F, 1.0d0)

      ! Vertimiento de Economias totales
      CALL Am_set(A, COffset + ParLaja%IVRB, FOffset + ParLaja%IVRB_F, 1.0d0)

      IF (ParLaja%IQRebLaja .GT. 0) THEN
         CALL Am_set(A, VolOffset + ParLaja%IQRebLaja, FOffset + ParLaja%IVRB_F, -etadursc)

         IF (ParLaja%FVertEcoAct) THEN
            CALL Am_set(A, COffset + ParLaja%IVRBES, FOffset + ParLaja%IVRB_F, 1.0d0)
         ENDIF
      ENDIF


      ! Volumen util
      CALL Am_set(A, VolOffset + ParLaja%IEmbLaja, FOffset + ParLaja%IVU_F, 1.0d0)

      IF (ParLaja%FVolUtilEcoEnd) THEN
         CALL Am_set(A, COffset + ParLaja%IVESF, FOffset + ParLaja%IVU_F, -1.0d0)
      ENDIF
      IF (ParLaja%FVolUtilEcoAP) THEN
         CALL Am_set(A, COffset + ParLaja%IVAPF, FOffset + ParLaja%IVU_F, -1.0d0)
      ENDIF
      CALL Am_set(A, COffset + ParLaja%IVU, FOffset + ParLaja%IVU_F, -1.0d0)

      ! riego suplido
      CALL Am_set(A, COffset + ParLaja%IQRS, FOffset + ParLaja%IQRS_F,  1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQHI, FOffset + ParLaja%IQRS_F,  -1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQDEFM, FOffset + ParLaja%IQRS_F,  -1.0d0)

      IF (ParLaja%FHolgTurRiego) THEN
         CALL Am_set(A, COffset + ParLaja%IQDR, FOffset + ParLaja%IQRS_F, 1.0d0)
      ENDIF

      ! primeros regantes
      CALL Am_set(A, COffset + ParLaja%IQPR, FOffset + ParLaja%IQPR_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQRS, FOffset + ParLaja%IQPR_F, -0.8d0)
      ! nuevos regantes
      CALL Am_set(A, COffset + ParLaja%IQNR, FOffset + ParLaja%IQNR_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQRS, FOffset + ParLaja%IQNR_F, -0.2d0)

      ! caudal total del laja
      CALL Am_set(A, COffset + ParLaja%IQLAJA, FOffset + ParLaja%IQLAJA_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQGH, FOffset + ParLaja%IQLAJA_F, -1.0d0)

      IF (ParLaja%IFiltLaja .GT. 0) THEN
         FiltVarIndLaja = FiltVarColInd(ParLaja%IFiltLaja, IEta)
         IF (FiltVarIndLaja .GT. 0) THEN
            CALL Am_set(A, FiltVarIndLaja, FOffset + ParLaja%IQLAJA_F, -1.0d0)
         ENDIF
      ENDIF
      IF (ParLaja%IQRebLaja .GT. 0) THEN
         CALL Am_set(A, VolOffset + ParLaja%IQRebLaja, FOffset + ParLaja%IQLAJA_F, -1.0d0)
      ENDIF
         

      ! Volumen util superior positivo
      IF (ParLaja%FVolUtilSupAct) THEN
      ! Volumen util superior negativo
         CALL Am_set(A, COffset + ParLaja%IVU, FOffset + ParLaja%IVUN_F,  1.0d0)

         CALL Am_set(A, COffset + ParLaja%IVUN, FOffset + ParLaja%IVUN_F,  1.0d0)
         
         CALL Am_set(A, COffset + ParLaja%IVUP, FOffset + ParLaja%IVUN_F, -1.0d0)

         CALL Am_set(A, COffset + ParLaja%IVUP, FOffset + ParLaja%IVUP_F, -ParLaja%FactVolUtilAct)

         CALL Am_set(A, COffset + ParLaja%IQGESH, FOffset + ParLaja%IVUP_F, etadursc)

         Sentido(FOffset + ParLaja%IVUP_F) = 'L'
      ENDIF

      ! Volumen economizable positivo
      IF (ParLaja%FQEconEndAct) THEN
      ! Volumen economizable negativo
         CALL Am_set(A, COffset + ParLaja%IQDGNH, FOffset + ParLaja%IQEN_F,  1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQEN, FOffset + ParLaja%IQEN_F, -1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQEP, FOffset + ParLaja%IQEN_F,  1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQEP, FOffset + ParLaja%IQEP_F, -1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQEA, FOffset + ParLaja%IQEP_F, 1.0d0)
         
         Sentido(FOffset + ParLaja%IQEP_F) = 'L'

         CALL Am_set(A, COffset + ParLaja%IQEA, FOffset + ParLaja%IVESF_F, -etadursc)

         CALL Am_set(A, COffset + ParLaja%IVUP, FOffset + ParLaja%IQEA_F, -ParLaja%FactVolUtilAct)

         CALL Am_set(A, COffset + ParLaja%IQEA, FOffset + ParLaja%IQEA_F, etadursc)

         Sentido(FOffset + ParLaja%IQEA_F) = 'L'
      ENDIF


      ! Gasto horario
      CALL Am_set(A, COffset + ParLaja%IQDGNH, FOffset + ParLaja%IQDGNH_F, 1.0d0)

      ! Economias horarias
      CALL Am_set(A, COffset + ParLaja%IQGESH, FOffset + ParLaja%IQGESH_F, 1.0d0)

      ! Derechos horarias
      CALL Am_set(A, COffset + ParLaja%IQGDGH, FOffset + ParLaja%IQGDGH_F, 1.0d0)

      ! Economias reservas horarias
      CALL Am_set(A, COffset + ParLaja%IQGERH, FOffset + ParLaja%IQGERH_F, 1.0d0)

      ! Gasto horario Alto Polcura
      CALL Am_set(A, COffset + ParLaja%IQGAPH, FOffset + ParLaja%IQGAPH_F, 1.0d0)

      ! Gasto horario regla 50cm
      CALL Am_set(A, COffset + ParLaja%IQG50H, FOffset + ParLaja%IQG50H_F, 1.0d0)

      ! Caudal horario turbinado
      CALL Am_set(A, COffset + ParLaja%IQGH, FOffset + ParLaja%IQGH_F, 1.0d0)

      ! Caudal de deficit de riego
      IF (ParLaja%FHolgTurRiego) THEN
         CALL Am_set(A, COffset + ParLaja%IQDR, FOffset + ParLaja%IQDR_F, -1.0d0)
      ENDIF
      CALL Am_set(A, COffset + ParLaja%IQDEFM, FOffset + ParLaja%IQDR_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IQLAJA, FOffset + ParLaja%IQDR_F, -1.0d0)

      Sentido(FOffset + ParLaja%IQDR_F) = 'L'

      ! Gastos medios anuales versus derechos anluale
      CALL Am_set(A, COffset + ParLaja%IVDGAF, FOffset + ParLaja%IGDA_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVDAF, FOffset + ParLaja%IGDA_F, -1.0d0)

      Sentido(FOffset + ParLaja%IGDA_F) = 'L'

      ! Gastos medios anuales versus derechos anluale
      CALL Am_set(A, COffset + ParLaja%IVDGMF, FOffset + ParLaja%IGDM_F, 1.0d0)

      CALL Am_set(A, COffset + ParLaja%IVDMF, FOffset + ParLaja%IGDM_F, -1.0d0)

      Sentido(FOffset + ParLaja%IGDM_F) = 'L'

      ! Agrega caudal de riego extraido 
      DO Idx = 1, ParLaja%NumRetRiego
         CALL Am_set(A, COffset + ParLaja%IQRIHC(Idx), FOffset + ParLaja%IQRIHF(Idx), 1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQRIHC(Idx), FOffset + ParLaja%IQRFHF(Idx), 1.0d0)

         IF (ParLaja%FHolgRetRiego) THEN
            CALL Am_set(A, COffset + ParLaja%IQRFHC(Idx), FOffset + ParLaja%IQRFHF(Idx), 1.0d0)
         ENDIF

         CALL Am_set(A, COffset + ParLaja%IQRDHC(Idx), FOffset + ParLaja%IQRFHF(Idx), -1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQRDHC(Idx), FOffset + ParLaja%IQRDHF(Idx), 1.0d0)

         CALL Am_set(A, COffset + ParLaja%IQNR, FOffset + ParLaja%IQRDHF(Idx), -ParLaja%CNRiego(IEta, Idx))

         CALL Am_set(A, COffset + ParLaja%IQPR, FOffset + ParLaja%IQRDHF(Idx), -ParLaja%CPRiego(IEta, Idx))
      ENDDO

      RETURN
      END

!******************
      SUBROUTINE GenPDLajaEtaFO(ParLaja, edur, FPhi,                    &
     &     COffset, PDNCol, FO, LowBnd, UppBnd)
      USE PLP, ONLY : PAR_DIMS, PAR_LAJA
      USE OSI

!     Variables Globales
!******************
      TYPE(PAR_LAJA) ParLaja

      DOUBLE PRECISION FPhi

      INTEGER PDNCol
      INTEGER COffset
      DOUBLE PRECISION edur
      DOUBLE PRECISION FO(PDNCol)
      DOUBLE PRECISION LowBnd(PDNCol)
      DOUBLE PRECISION UppBnd(PDNCol)

      INTEGER Idx
      INTEGER IVol

      DOUBLE PRECISION FVol 
      DOUBLE PRECISION FCau

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()


      IVol = ParLaja%IEmbLaja

!     Funcion Objetivo
!****************
      FO(COffset + 1 : COffset + ParLaja%NumColEta) = 0.0d0

      FVol = ParLaja%ScaleVol / FPhi
      FCau = edur / FPhi

      IF (ParLaja%FVertEcoAct) THEN
         FO(COffset + ParLaja%IVRB) = ParLaja%CVertEcon * FVol
      ENDIF
      IF (ParLaja%FVolUtilSupAct) THEN
         FO(COffset + ParLaja%IVUN) = ParLaja%CVolUtilNeg * FVol
      ENDIF

      IF (ParLaja%FHolgTurRiego) THEN
         FO(COffset + ParLaja%IQDR) = ParLaja%CRiegoNS * FCau
      ENDIF
      
      IF (ParLaja%FHolgRetRiego) THEN
         DO Idx=1, ParLaja%NumRetRiego
            FO(COffset + ParLaja%IQRFHC(Idx)) = ParLaja%CRiegoNS * FCau
         ENDDO
      ENDIF

      IF (ParLaja%FQEconEndAct) THEN
         FO(COffset + ParLaja%IQEN) = ParLaja%CSubEconAnu * FCau
      ENDIF


      FO(COffset + ParLaja%IVAPF) = ParLaja%CostoVAP * FVol

!     Restricciones de tipo 'x < = ' y 'x > = '
!***********************************
      UppBnd(COffset + 1 : COffset + ParLaja%NumColEta) = DINFTY
      LowBnd(COffset + 1 : COffset + ParLaja%NumColEta) = 0.0d0

      LowBnd(COffset + ParLaja%IVU) = -DINFTY

      RETURN
      END

!*********************

      DOUBLE PRECISION FUNCTION QAfluEta(IEta, NBloque, BloInd, BloDur, &
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

      QAfluEta = 0.0d0
      DSum = 0.0d0
      DO IBlo = 1, NBloque(IEta)
         IBInd = BloInd(IBlo, IEta)
         DSum = DSum + BloDur(IBInd) 
         QAfluEta = QAfluEta +                                          &
     &        BloDur(IBInd)*EstocRHSP(IAfl, IBInd, IClaseFila(IAfl))
      ENDDO
      QAfluEta = QAfluEta/DSum

      RETURN
      END

!*******************

      SUBROUTINE GetQsLaja(IEta, NBloque, BloInd, BloDur, Mes,          &
     &     EstocRHSP, IClaseFila, ParLaja, VolUtil,                     &
     &     QAltoPolc, QHoyaInter,                                       &
     &     QDefAbanico, QDefTucapel, QTLajaMin,                         &
     &     QPRiego, QNRiego, Dim)

      USE PLP, ONLY : PAR_DIMS, PAR_LAJA
      TYPE(PAR_DIMS), INTENT(IN)::  Dim


      INTEGER IEta
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER Mes(Dim%Eta)
      INTEGER IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION VolUtil
      TYPE(PAR_LAJA) ParLaja

      DOUBLE PRECISION QAltoPolc
      DOUBLE PRECISION QHoyaInter
      DOUBLE PRECISION QDefAbanico
      DOUBLE PRECISION QDefTucapel
      DOUBLE PRECISION QTLajaMin
      DOUBLE PRECISION QNRiego
      DOUBLE PRECISION QPRiego

!
      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION friego_mes
      INTEGER IAfl

      QAltoPolc = QAfluEta(IEta, NBloque, BloInd, BloDur,               &
     &     EstocRHSP, IClaseFila, ParLaja%IAflAltoPolcura, Dim)

      QHoyaInter = 0.0d0
      DO IAfl = 1, ParLaja%NumAflHoyaInt         
         IF (ParLaja%IAflHoyaInt(IAfl) .GT. 0) THEN
            QHoyaInter = QHoyaInter + QAfluEta(IEta, NBloque, BloInd,   &
     &           BloDur, EstocRHSP, IClaseFila,                         & 
     &           ParLaja%IAflHoyaInt(IAfl), Dim)
         ENDIF
      ENDDO

      friego_mes = 0.01d0*ParLaja%PGastosMesHid(Mes(IEta))
      QPRiego = friego_mes*ParLaja%QRiegoHist
!
!     Riego correspondiente a los nuevos regantes, que depende del
!     volumen util y la modulacion mensual
!
      IF (VolUtil .GE. ParLaja%VolUtilColMed) THEN
         QNRiego = ParLaja%QNRiegoCSup
      ELSEIF (VolUtil .GE. ParLaja%VolUtilColInf) THEN
         QNRiego = ParLaja%QNRiegoCMed
      ELSE
         QNRiego = ParLaja%QNRiegoCInf
      ENDIF
      QNRiego = friego_mes*QNRiego

!
!     Deficits
!
      QDefAbanico = ParLaja%QFiltHist
      IF (ParLaja%MetodoCalcDef .LE. 1) THEN
         QDefTucapel = QNRiego + QPRiego - QHoyaInter
         QTLajaMin = MIN(QDefTucapel, QDefAbanico)
      ELSE IF (ParLaja%MetodoCalcDef .EQ. 2) THEN
         QDefTucapel = QPRiego - QHoyaInter
         QTLajaMin = MIN(QDefTucapel, QDefAbanico) + QNRiego
      ENDIF

      
      RETURN
      END

!***************************************

      SUBROUTINE FijaLaja(IEta,                                         &
     &     NBloque, BloInd, BloDur, EtaDur, Mes, FactTiempo,            &
     &     EstocRHSP, IClaseFila,                                       &
     &     ParLaja, VolIni, NumEmb,                                     &
     &     ISimul,                                                      &
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

      TYPE(PAR_LAJA), INTENT(INOUT) :: ParLaja
      INTEGER, INTENT(IN) :: NumEmb
      DOUBLE PRECISION, INTENT(IN):: VolIni(NumEmb)
      INTEGER, INTENT(IN) :: ISimul

      INTEGER, INTENT(IN) :: IClaseFila(Dim%EstocFila)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: EtaDur(Dim%Eta)
!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT) :: lp


!     locals
      DOUBLE PRECISION VolLaja
      DOUBLE PRECISION VolUtil
      DOUBLE PRECISION VarLajaPrev(ParLaja%NumColEta)
      DOUBLE PRECISION LDResLaja(ParLaja%NumFilEta)
      DOUBLE PRECISION VarLaja(ParLaja%NumColEta)
      INTEGER Idx
      INTEGER IdxU
      INTEGER IdxL
      INTEGER IdxE
      DOUBLE PRECISION QAltoPolc
      DOUBLE PRECISION QHoyaInter
      DOUBLE PRECISION QDefAbanico
      DOUBLE PRECISION QDefTucapel
      DOUBLE PRECISION QTLajaMin
      DOUBLE PRECISION QPRiego
      DOUBLE PRECISION QNRiego
      DOUBLE PRECISION QAltoPolcPrev
      DOUBLE PRECISION QHoyaInterPrev
      DOUBLE PRECISION QDefAbanicoPrev
      DOUBLE PRECISION QDefTucapelPrev
      DOUBLE PRECISION QTLajaMinPrev
      DOUBLE PRECISION QPRiegoPrev
      DOUBLE PRECISION QNRiegoPrev      
      DOUBLE PRECISION Caudal2Vol
      DOUBLE PRECISION Caudal2VolPrev
      DOUBLE PRECISION EconNuevas
      DOUBLE PRECISION GastoMedioHorMin
      DOUBLE PRECISION GastoMedioHorMax
      DOUBLE PRECISION GastoEcoEndHorMax
      DOUBLE PRECISION GastoEcoResHorMax
      DOUBLE PRECISION GastoRegla50HorMax
      DOUBLE PRECISION CaudalEconAntMax
      DOUBLE PRECISION QEconEndesPrev
      INTEGER IEtaPrev
      INTEGER IVol
      LOGICAL ColchonInferiorActivo
      INTEGER IFil
      INTEGER ICol
      DOUBLE PRECISION pcoef
      DOUBLE PRECISION ncoef
      DOUBLE PRECISION QRiego

      INTEGER MCUppInd (ParLaja%NumColEta)
      DOUBLE PRECISION MCUppVal (ParLaja%NumColEta)
      INTEGER MCLowInd (ParLaja%NumColEta)
      DOUBLE PRECISION MCLowVal (ParLaja%NumColEta)


      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!     LOGICAL Regla50cmActiva

      VarLaja(1: ParLaja%NumColEta) = 0.0d0
      LDResLaja(1: ParLaja%NumFilEta) = 0.0d0
!
!     Obtiene el valor de las variables previas del laja, que pueden
!     resultar de las condiciones iniciales o de la etapa previa 
!     
      CALL GetVarLajaPrev(IEta, ISimul,                                 &
     &     ParLaja, VolIni(ParLaja%IEmbLaja), VarLajaPrev)
!
!     Las restricciones de economias, gastos y derechos asumen en valor
!     de las variables de la etapa previa, que se consideran como valor
!     inicial
!
      LDResLaja(ParLaja%IVDGMF_F) = VarLajaPrev(ParLaja%IVDGMF)
      LDResLaja(ParLaja%IVDGAF_F) = VarLajaPrev(ParLaja%IVDGAF)
      LDResLaja(ParLaja%IVESF_F) = VarLajaPrev(ParLaja%IVESF)
      LDResLaja(ParLaja%IVERF_F) = VarLajaPrev(ParLaja%IVERF)
      LDResLaja(ParLaja%IVAPF_F) = VarLajaPrev(ParLaja%IVAPF)
      LDResLaja(ParLaja%IVDAF_F) = VarLajaPrev(ParLaja%IVDAF)
      LDResLaja(ParLaja%IVDMF_F) = VarLajaPrev(ParLaja%IVDMF)

!
!     Volumen y filtraciones actuales y utiles del laja
!
      IVol = ParLaja%IEmbLaja
      VolLaja = VolIni(IVol)
      VolUtil = VarLajaPrev(ParLaja%IVU)
      ColchonInferiorActivo = VolUtil .LE. ParLaja%VolUtilColInf
      ParLaja%ColchonInfActivo(ISimul, IEta) = ColchonInferiorActivo
!
!     Calcula flujos relevantes 
!
      Caudal2Vol = FactTiempo * EtaDur(IEta)
      CALL GetQsLaja(IEta, NBloque, BloInd, BloDur, Mes,                &
     &     EstocRHSP, IClaseFila, ParLaja, VolUtil,                     &
     &     QAltoPolc, QHoyaInter,                                       &
     &     QDefAbanico, QDefTucapel, QTLajaMin,                         &
     &     QPRiego, QNRiego, Dim)

      LDResLaja(ParLaja%IQNR_F) = 0.0d0
      LDResLaja(ParLaja%IQPR_F) = 0.0d0
      QRiego = QNRiego + QPRiego

      pcoef = 0.0d0
      ncoef = 0.0d0
      IF (QRiego .GT. 0.0d0) THEN
         IF (ParLaja%MetodoCalcDef .LE. 1) THEN
            ncoef = -QNRiego/QRiego
            pcoef = -(1.0d0 + ncoef)
         ELSE IF (ParLaja%MetodoCalcDef .EQ. 2) THEN
            pcoef = -1.0d0
            ncoef = 0.0d0
            LDResLaja(ParLaja%IQNR_F) = QNRiego
            LDResLaja(ParLaja%IQPR_F) = -QNRiego
         ENDIF
      ENDIF
      
      ICol = ParLaja%ColIndEta(ParLaja%IQRS, IEta) - 1
      IFil = ParLaja%FilIndEta(ParLaja%IQNR_F, IEta) - 1
      CALL osi_lp_setcoefficient(lp, IFIl, ICol, ncoef)
      IFil = ParLaja%FilIndEta(ParLaja%IQPR_F, IEta) - 1
      CALL osi_lp_setcoefficient(lp, IFIl, ICol, pcoef)

      DO Idx=1, ParLaja%NumRetRiego
         LDResLaja(ParLaja%IQRDHF(Idx)) = ParLaja%DRExtra(IEta, Idx)
      ENDDO
      

!     Fijamos el Volumen util minimo absoluto y el volumen util del
!     colchon inferior
      LDResLaja(ParLaja%IVU_F) = ParLaja%VolUtilMin
      IF (ParLaja%FVolUtilSupAct) THEN
         LDResLaja(ParLaja%IVUN_F) = ParLaja%VolUtilColInf
      ENDIF
      IF (ParLaja%FQEconEndAct) THEN
         IF (ColchonInferiorActivo) THEN
            LDResLaja(ParLaja%IQEN_F) = ParLaja%GastoResMax
         ELSE
            LDResLaja(ParLaja%IQEN_F) = ParLaja%GastoAnuMax
         ENDIF
      ENDIF

     
!
!     Consideraciones para inicio de mes y de agno
!
      IF (IEta .GT. 1) THEN
         IF (ParLaja%TipoEtaGM(IEta) .NE. INTRAETA) THEN
!           Reiniciamos contador de gasto mensual
            LDResLaja(ParLaja%IVDGMF_F) = 0.0d0
            LDResLaja(ParLaja%IVDMF_F) = ParLaja%VGastoMenMax(IEta)
         ENDIF
         IF (ParLaja%TipoEtaGM(IEta) .EQ. INICIOANO) THEN
!           Reiniciamos contador de los derechos anuales de gastos
            LDResLaja(ParLaja%IVDGAF_F) = 0.0d0
            LDResLaja(ParLaja%IVDAF_F) = ParLaja%VGastoAnuMax(IEta)
         ENDIF         
      ENDIF
         
!
!     Reglas para calcular las nuevas economias de endesa y de reserva 
! 
      IF (IEta .GT. 1) THEN
!        
!        Calcula flujos relevantes  en la etapa anterior
!
         IEtaPrev = IEta - 1
         Caudal2VolPrev = FactTiempo * EtaDur(IEtaPrev)
         CALL GetQsLaja(IEtaPrev, NBloque, BloInd, BloDur, Mes,         &
     &        EstocRHSP, IClaseFila, ParLaja, VolUtil,                  &
     &        QAltoPolcPrev, QHoyaInterPrev,                            &
     &        QDefAbanicoPrev, QDefTucapelPrev, QTLajaMinPrev,          &
     &        QPRiegoPrev, QNRiegoPrev, Dim)

         IF (ColchonInferiorActivo) THEN
!
!           En el colchon inferior no se generan nuevas economias normales
!           
            VarLaja(ParLaja%IVESN) =  0.0d0
!
!           Cuando lo que se genera para cubrir el deficit de riego en
!           tucapel es menor que el deficit de filtraciones abanico, se
!           generan economias de reserva a cuenta de los ahorros de la
!           etapa anterior. Verificamos entonces que hay necesidad de
!           riego.
!
            IF (QPRiegoPrev .GT. 0) THEN
               VarLaja(ParLaja%IVERN) = Caudal2VolPrev                     &
     &              * MAX(QDefAbanicoPrev                                  &
     &              - VarLajaPrev(ParLaja%IQLAJA), 0d0)
            ENDIF
         ELSE 
!           Las economias se contabilizan segun se obtienen en la etapa
!           anterior, si es que existen
!
            QEconEndesPrev = ParLaja%GastoAnuMax                       &
     &           - VarLajaPrev(ParLaja%IQDGNH) 
!           calculamos el valor positivo de las economias
            QEconEndesPrev = MAX(QEconEndesPrev, 0.0d0)

            IF (ParLaja%FQEconEndAct) THEN
!              Si es que esta activo el contar las economias
!              anticipadas, estas se restan por estar ya realizadas en
!              la etapa anterior. Notese que aunque se cuenten
!              anticipadamente las economias, como depende de una
!              penalizacion, es posible que no se hayan contado todas
!              las posibles economias
               QEconEndesPrev = QEconEndesPrev -                        & 
     &              VarLajaPrev(ParLaja%IQEA)
            ENDIF
            VarLaja(ParLaja%IVESN) = Caudal2VolPrev * QEconEndesPrev
!
!           No hay nuevas economias de reserva fuera del colchon inferior
!
            VarLaja(ParLaja%IVERN) = 0.0d0
         ENDIF
!        
!        Vertimientos de economias
!        
!        Si se ha vertido por sobre la variable de vertimientos de
!        economias definido por IVRBES, es decir, usando la variable de
!        holgura de vertimiento IVRB, revisamos entonces si todavia es
!        posible vertir economias, dado que la prioridad entre VRB y VRBES
!        esta dado solo por penazilacion
         IF (VarLajaPrev(ParLaja%IVRB) .GT. 0.0d0) THEN
            EconNuevas = MAX(0.0d0,                                  &
     &           VarLajaPrev(ParLaja%IVESF)                           &
     &           + VarLaja(ParLaja%IVESN)                             & 
     &           - VarLajaPrev(ParLaja%IVRB))
            
            VarLaja(ParLaja%IVESN) = EconNuevas -                     &
     &           VarLajaPrev(ParLaja%IVESF)
         ENDIF
      ENDIF

!
!     Economias de reserva se pierden todas si se esta en el colchon
!     inferior
!
      IF (.NOT. ColchonInferiorActivo) THEN
         LDResLaja(ParLaja%IVERF_F) = 0.0d0
      ENDIF

!     
!     El caudal de Alto Polcura se suma inmediatamente a los derechos
!     
      VarLaja(ParLaja%IVAPN) = Caudal2Vol * QAltoPolc

!
!     Reglas de descuento de derechos anuales y mensuales
!      
      IF (ColchonInferiorActivo) THEN
!        Se reducen los derechos de gastos maximos anuales de
!        GastoAnuMax (57 m3/s) a GastoResMax (47 m3/s)
         VarLaja(ParLaja%IVDAD) =  Caudal2Vol *     &
     &        (ParLaja%GastoAnuMax - ParLaja%GastoResMax)
!        Igualmente se reducen los derechos mensuales
         VarLaja(ParLaja%IVDMD) =  Caudal2Vol *     &
     &        (ParLaja%GastoMenMax - ParLaja%GastoResMax)
      ENDIF

!
!     Regla para definir el caudal minimo turbinado en Laja para cumplir
!     con los convenios de riego
!
      VarLaja(ParLaja%IQDEFM) = QTLajaMin
      VarLaja(ParLaja%IQHI) = QHoyaInter


!
!     Regla de gasto medio horario a 50cm de rebalse
!     
!     Regla50cmActiva = VolLaja .GE. ParLaja%VolAcumA50cmReb
      IF (VolLaja .GE. ParLaja%VolAcumA50cmReb) THEN
!        Si el volumen esta sobre los ultimos 50cm a la cota de rebalse,
!        se puede vertir sin restricciones, pero se contabilizan
!        GastoResMax para los gastos anuales. Esto se hace forzando el
!        gasto medio horario a este ultimo valor, y liberando q50h
         GastoRegla50HorMax = DINFTY
         GastoMedioHorMin = 0.0d0
         GastoMedioHorMax = ParLaja%GastoResMax
      ELSE 
         GastoMedioHorMin = 0.0d0
         GastoMedioHorMax = ParLaja%GastoHorMax
         GastoRegla50HorMax = 0.0d0
      ENDIF

!     Regla de gasto permanente de reserva en caso de estar en colchon inferior
      IF (ColchonInferiorActivo) THEN
!        Si estamos dentro del colchon inferior, se aplica el gasto
!        maximo permanente de reserva de 47 m3/s menos las filtraciones
         GastoMedioHorMax = ParLaja%GastoResMax
      ENDIF

!
!     Regla Uso de economias para generacion dentro y fuera de colchon
!     inferior
!
      IF (ColchonInferiorActivo) THEN
!        Si el volumen esta bajo la cota del colchon de reserva, no se
!        pueden utilizar la economias normales ni el alto polcura, pero si
!        las de reserva
         GastoEcoEndHorMax = 0.0d0
         GastoEcoResHorMax = DINFTY
      ELSE
         GastoEcoEndHorMax = DINFTY
         GastoEcoResHorMax = 0.0d0
      ENDIF

      IF (ColchonInferiorActivo) THEN
         CaudalEconAntMax = 0.0d0
      ELSE
         CaudalEconAntMax = DINFTY
      ENDIF
      
!
!     Upper boundaries simples
!
      IdxU = 0
      ! Gasto de economias endesa horarios
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQGESH, IEta)
      MCUppVal(IdxU) = GastoEcoEndHorMax
      ! Gasto de economias de reserva horarios
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQGERH, IEta)
      MCUppVal(IdxU) = GastoEcoResHorMax
      ! Gastos medios horarios
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQDGNH, IEta)
      MCUppVal(IdxU) = GastoMedioHorMax
      ! Gastos regla de 50cm horarios
      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQG50H, IEta)
      MCUppVal(IdxU) = GastoRegla50HorMax

      ! Caudal de economias anticipadas
      IF (ParLaja%FQEconEndAct) THEN
         IdxU = IdxU + 1
         MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQEA, IEta)
         MCUppVal(IdxU) = CaudalEconAntMax
      ENDIF
      
!
!     Asigna igualdades
!
      IdxE = IdxU

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IVESN, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IVESN) / ParLaja%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IVAPN, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IVAPN) / ParLaja%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IVERN, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IVERN) / ParLaja%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IVDAD, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IVDAD) / ParLaja%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IVDMD, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IVDMD) / ParLaja%ScaleVol

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQDEFM, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IQDEFM) 

      IdxU = IdxU + 1
      MCUppInd(IdxU) = ParLaja%ColIndEta(ParLaja%IQHI, IEta)
      MCUppVal(IdxU) = VarLaja(ParLaja%IQHI) 

!     
!     Low boundaries simples
!
      IdxL = 0
      ! Gastos medios horarios minimos
      IdxL = IdxL + 1
      MCLowInd(IdxL) = ParLaja%ColIndEta(ParLaja%IQDGNH, IEta)
      MCLowVal(IdxL) = GastoMedioHorMin

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
      DO Idx = 1, ParLaja%NumResVol
         LDResLaja(Idx) = LDResLaja(Idx) / ParLaja%ScaleVol
      ENDDO

      CALL ModifBrdRhs(ParLaja%NumFilEta,                 &
     &     ParLaja%FilIndEta(1, IEta), LDResLaja,         &
     &     lp)

      CALL ModifUpp(IdxU, MCUppInd,  MCUppVal, lp)
      CALL ModifLow(IdxL, MCLowInd,  MCLowVal, lp)

      RETURN
      END

!***************************

      SUBROUTINE GetVarLajaPrev(IEtapa, ISimul,                         &
     &     ParLaja, VolIniLaja, VarLajaPrev)
      USE PLP

      TYPE(PAR_LAJA), INTENT(IN) :: ParLaja

      INTEGER, INTENT(IN) :: IEtapa
      INTEGER, INTENT(IN) :: ISimul
      DOUBLE PRECISION, INTENT(IN) :: VolIniLaja

      DOUBLE PRECISION, INTENT(OUT) :: VarLajaPrev(ParLaja%NumColEta)
!
      DOUBLE PRECISION VolLaja
      DOUBLE PRECISION VolUtil
      INTEGER IVol
!
      IVol = ParLaja%IEmbLaja

      IF (IEtapa .EQ. 1) THEN
         VarLajaPrev(1:ParLaja%NumColEta) = 0.0d0
!        Usamos valores de archivo. Se supone que no hay que resolver si
!        estamos al principo de un mes o ano, esto viene implicito en el
!        valor inicial

         VarLajaPrev(ParLaja%IVDGMF) = ParLaja%GastoMenIni
         VarLajaPrev(ParLaja%IVDGAF) = ParLaja%GastoAnuIni
         VarLajaPrev(ParLaja%IVESF) = ParLaja%EcoEndAnuIni
         VarLajaPrev(ParLaja%IVERF) = ParLaja%EcoResTotIni
         VarLajaPrev(ParLaja%IVAPF) = ParLaja%EcoAltoPolTotIni
         VarLajaPrev(ParLaja%IVDAF) = ParLaja%VDerechosAnuIni
         VarLajaPrev(ParLaja%IVDMF) = ParLaja%VDerechosMenIni
         
         VolLaja = VolIniLaja

!        Definicion de volumen util neto
         VolUtil = VolLaja - ParLaja%VolUtilMin
         IF (ParLaja%FVolUtilEcoEnd) THEN
!           Descuenta las economias de endesa
            VolUtil = VolUtil - VarLajaPrev(ParLaja%IVESF)
         ENDIF
         IF (ParLaja%FVolUtilEcoAP) THEN
!           Descuenta las economias de AP
            VolUtil = VolUtil - VarLajaPrev(ParLaja%IVAPF)
         ENDIF

         VarLajaPrev(ParLaja%IVU) = VolUtil
         
         RETURN
      ENDIF

      VarLajaPrev(1:ParLaja%NumColEta) =                                &
     &     ParLaja%VarEtaPrev(1:ParLaja%NumColEta, ISimul, IEtapa)
      
      RETURN
      END

!******************

!******************
      SUBROUTINE AgrFactLaja(NFila, NCol, ISimul,                       &
     &     IEtapaOri, IEtapaDest, deps,  ParLaja,                       &  
     &     nzcnt, rmatval, rmatind, pi, LD, rhs,                        &
     &     lpi, FactDBL, ULogCF)

      USE PLP
      USE OSI
      INCLUDE 'machcons.fpp'

      INTEGER, INTENT(IN):: NFila
      INTEGER, INTENT(IN):: NCol

      TYPE(PAR_LAJA) ParLaja

      INTEGER ISimul
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
      LOGICAL IsVarCorteLaja


      DO Ind = 1, ParLaja%NumVarEst
         IF ( .NOT. IsVarCorteLaja(ISimul, IEtapaOri, ParLaja, Ind)) THEN
            CYCLE
         ENDIF

         IFila = ParLaja%FilIndEta(Ind, IEtapaOri)
         IF (DxAEQy(pi(IFila), 0.0d0, deps)) THEN
            CYCLE
         ENDIF

         rhs = rhs - pi(IFila)*LD(IFila)
            
         IF (FactDBL .NE. 0) THEN
            ICol = ParLaja%ColIndEta(Ind, IEtapaOri)
            name = ' '
            CALL osi_lp_getcolname(lpi, ICol - 1, sizeof(name), loc(name))
            WRITE(ULogCF,*) 'cfl: ', name, IFila, pi(IFila), LD(IFila)
         ENDIF
         
         ICol = ParLaja%ColIndEta(Ind, IEtapaDest)
         nzcnt = nzcnt + 1
         rmatval(nzcnt) = -pi(IFila)
         rmatind(nzcnt) = ICol  - 1
      ENDDO

      RETURN
      END

!**********************************

      SUBROUTINE InitParLaja(ParLaja, Dim)
      USE PLP, ONLY : PAR_LAJA, PAR_DIMS

      TYPE(PAR_LAJA) ParLaja
      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!

      ALLOCATE(ParLaja%VarEtaVol(ParLaja%DimColEta))
      ParLaja%VarEtaVol = .FALSE.

      ALLOCATE(ParLaja%VarBloNames(ParLaja%DimColBlo))
      ALLOCATE(ParLaja%VarEtaNames(ParLaja%DimColEta))


      ALLOCATE(ParLaja%ColIndEta(ParLaja%DimColEta, Dim%Eta))
      ALLOCATE(ParLaja%FilIndEta(ParLaja%DimFilaEta, Dim%Eta))

      ALLOCATE(ParLaja%VGastoMenMax(Dim%Eta))
      ALLOCATE(ParLaja%VGastoAnuMax(Dim%Eta))     

      ALLOCATE(ParLaja%TipoEtaGM(Dim%Eta))
         
      ALLOCATE(ParLaja%DataEta(ParLaja%DimColEta, Dim%Eta))
      ALLOCATE(ParLaja%DataBlo(ParLaja%DimColBlo, Dim%Blo))

      ALLOCATE(ParLaja%VarEtaPrev(ParLaja%DimColEta, Dim%Simul, Dim%Eta))
      ALLOCATE(ParLaja%ColchonInfActivo(Dim%Simul, Dim%Eta))

      RETURN
      
      END

      SUBROUTINE LeeLaja(NArcLajaN, FConvLaja,                          &
     &     NCenEmb, NCenSer,                                            &  
     &     CenNom, NEtapa, Mes, FactTiempo,                             &
     &     FiltNCen, FiltEmbInd, EmbMaxA,                               & 
     &     FVertReb, NEmbVReb, EmbVRebInd,                              &
     &     ScaleVol, ParLaja, ULog, Dim)

      USE PLP, ONLY : PAR_DIMS, PAR_LAJA, FactTiempoH, HorasMesHid, &
     &     INICIOANO, INTRAETA, ENEROHID, INICIOMES, DimLargo, No

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      INCLUDE 'machcons.fpp'

      CHARACTER*(*) NArcLajaN
      INTEGER FConvLaja
      CHARACTER*48 CenNom(Dim%Cen)
      TYPE(PAR_LAJA) ParLaja
      INTEGER NEtapa
      INTEGER Mes(Dim%Eta)
      INTEGER ULog
      INTEGER FiltNCen
      INTEGER FiltEmbInd(Dim%EmbFilt)
      DOUBLE PRECISION FactTiempo

      LOGICAL FVertReb
      INTEGER EmbVRebInd(Dim%EmbVReb)
      INTEGER NEmbVReb

      DOUBLE PRECISION EmbMaxA(Dim%Emb)
      DOUBLE PRECISION ScaleVol(Dim%Emb)

!     variables locales
      CHARACTER*12 AuxVar
      CHARACTER*48 NomCentral
      CHARACTER*42 Objeto

      INTEGER URead
      LOGICAL FStop
      LOGICAL FWarning

      INTEGER NumCen
      INTEGER IFilt
      INTEGER IEta
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER NCenHidSPP
      DOUBLE PRECISION GastoDiaMax
      INTEGER Idx
      INTEGER I
      INTEGER ICen
      
      EXTERNAL Abrir
      INTEGER Abrir

      DOUBLE PRECISION PRetPRiego(12)
      DOUBLE PRECISION PRetNRiego(12)
      DOUBLE PRECISION RiegoExtra(12)
      DOUBLE PRECISION VRemA50cmReb

      CHARACTER*(DimLargo) CVolB
      INTEGER ICol
      INTEGER IFil

      CALL InitParLaja(ParLaja, Dim)

      FStop = .FALSE.

      ParLaja%VarBloNames(ParLaja%IQGES) = 'qges' 
      ParLaja%VarBloNames(ParLaja%IQGDG) = 'qgdg' 
      ParLaja%VarBloNames(ParLaja%IQGER) = 'qger' 
      ParLaja%VarBloNames(ParLaja%IQGAP) = 'qgap' 
      ParLaja%VarBloNames(ParLaja%IQG50) = 'qg50' 
      ParLaja%VarBloNames(ParLaja%IQDGN) = 'qdgn' 
      
      ParLaja%VarEtaNames(ParLaja%IVDGMF) = 'vdgmf' 
      ParLaja%VarEtaNames(ParLaja%IVDGAF) = 'vdgaf' 
      ParLaja%VarEtaNames(ParLaja%IVESF)  = 'vesf' 
      ParLaja%VarEtaNames(ParLaja%IVERF)  = 'verf' 
      ParLaja%VarEtaNames(ParLaja%IVAPF)  = 'vapf' 
      ParLaja%VarEtaNames(ParLaja%IVDAF)  = 'vdaf' 
      ParLaja%VarEtaNames(ParLaja%IVDMF)  = 'vdmf' 
      ParLaja%VarEtaNames(ParLaja%IVESN)  = 'vesn' 
      ParLaja%VarEtaNames(ParLaja%IVAPN)  = 'vapn'
      ParLaja%VarEtaNames(ParLaja%IVERN)  = 'vern' 
      ParLaja%VarEtaNames(ParLaja%IVDAD)  = 'vdad' 
      ParLaja%VarEtaNames(ParLaja%IVDMD)  = 'vdmd' 
      ParLaja%VarEtaNames(ParLaja%IVU)    = 'vu' 
      ParLaja%VarEtaNames(ParLaja%IVRB)   = 'vrb' 
      ParLaja%VarEtaNames(ParLaja%IQDGNH) = 'qdgnh' 
      ParLaja%VarEtaNames(ParLaja%IQGESH) = 'qgesh' 
      ParLaja%VarEtaNames(ParLaja%IQGDGH) = 'qgdgh' 
      ParLaja%VarEtaNames(ParLaja%IQGERH) = 'qgerh' 
      ParLaja%VarEtaNames(ParLaja%IQGAPH) = 'qgaph' 
      ParLaja%VarEtaNames(ParLaja%IQG50H) = 'qg50h' 
      ParLaja%VarEtaNames(ParLaja%IQGH)   = 'qgh' 
      ParLaja%VarEtaNames(ParLaja%IQDEFM) = 'qdefm' 
      ParLaja%VarEtaNames(ParLaja%IQRS)   = 'qrs' 
      ParLaja%VarEtaNames(ParLaja%IQHI)   = 'qhi' 
      ParLaja%VarEtaNames(ParLaja%IQPR)   = 'qpr' 
      ParLaja%VarEtaNames(ParLaja%IQNR)   = 'qnr' 
      ParLaja%VarEtaNames(ParLaja%IQLAJA) = 'qlaja' 
      
      
      ! Apertura de archivo
      URead = Abrir(NArcLajaN, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(6, '(3A)') 'leelaja: Error, no existe archivo ',           &
     &        NArcLajaN, '.'
         WRITE(ULog, '(3A)') 'leelaja: Error, no existe archivo ',        &
     &        NArcLajaN, '.'
         STOP 1
      ENDIF


      ParLaja%UsaCorteOptim = FConvLaja .GT. 1
      ! Comentario
      READ(URead, '(A1)') AuxVar
      ! Central El Toro
      Objeto ='embalse Laja'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FStop, NomCentral,                  &
     &     CenNom, NCenEmb, Objeto, ULog)
      IF (FStop) THEN
         STOP 1
      ENDIF

      ParLaja%IEmbLaja = NumCen
      ParLaja%ScaleVol = ScaleVol(NumCen)
      ParLaja%IQRebLaja = 0
      IF (FVertReb) THEN
         DO Idx = 1, NEmbVReb
            IF (NumCen .EQ. EmbVRebInd(Idx)) THEN
               ParLaja%IQRebLaja = Idx + 2*NCenEmb
            ENDIF
         ENDDO
      ENDIF


      ! buscamos indice de variable de filtracion
      ParLaja%IFiltLaja = 0
      DO IFilt = 1, FiltNCen
         IF (FiltEmbInd(IFilt) .EQ. ParLaja%IEmbLaja) THEN
            ParLaja%IFiltLaja = IFilt
         ENDIF
      ENDDO

      NCenHidSPP = NCenEmb + NCenSer

      ! Afluente AltoPolcura
      Objeto ='afluente Alto Polcura'
      ParLaja%IAflAltoPolcura = 0
      READ(URead, '(A1)') AuxVar
      READ(URead, *) NomCentral
      CALL NomCen2NumCen(NumCen, FWarning, NomCentral,                  &
     &     CenNom, NCenHidSPP, Objeto, ULog)
      IF (NumCen .GT. 0) THEN
         ParLaja%IAflAltoPolcura = NumCen
      ENDIF
      
      ! Definicion hoya intemedia
      Objeto ='afluente hoya intermedia Laja'
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%NumAflHoyaInt      
      READ(URead, '(A1)') AuxVar

      ALLOCATE(ParLaja%IAflHoyaInt(ParLaja%NumAflHoyaInt))

      ICen = 0
      DO Idx=1, ParLaja%NumAflHoyaInt
         READ(URead, *) NomCentral
         CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &        CenNom, NCenHidSPP, Objeto, ULog)
         IF (NumCen .GT. 0) THEN
            ICen = ICen + 1
            ParLaja%IAflHoyaInt(ICen) = NumCen
         ENDIF
      ENDDO
      ParLaja%NumAflHoyaInt = ICen

      ! GastoDiaMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) GastoDiaMax
      ! El gasto diario maximo se modela con el gasto horario prom max,
      ! que se puede entender como caudal promedio de la etapa
      ParLaja%GastoHorMax = GastoDiaMax

      ! GastoMenMax
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%GastoMenMax
      ! El gasto mensual medio se convierte en volumen, dependiendo de las
      ! horas del mes hidrologico, y la constante de tiempo 
      DO IEta=1, NEtapa
         ParLaja%VGastoMenMax(IEta) = ParLaja%GastoMenMax &
     &        *HorasMesHid(Mes(IEta))*FactTiempoH
      ENDDO

      ! GastoAnuMax y DerGastoAnu
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%GastoAnuMax
      ! El gasto anual medio se convierte en volumen usando el numero de
      ! horas del agno, que en este caso es constante 
      ! ** no hay agnos bisiestos **
      DO IEta=1, NEtapa         
         ParLaja%VGastoAnuMax(IEta) = ParLaja%GastoAnuMax &
     &        *(24*365)*FactTiempoH
      ENDDO

      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%GastoResMax

      ! GastoMenIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%GastoMenIni

      ! GastoAnuIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%GastoAnuIni

      ! EcoEndAnuIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%EcoEndAnuIni

      ! DiasAnuCInfIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%DiasAnuCInfIni

      ParLaja%VDerechosAnuIni = ParLaja%VGastoAnuMax(1) -               &
     &     FactTiempoH * (24 * ParLaja%DiasAnuCInfIni) *                &
     &     (ParLaja%GastoAnuMax - ParLaja%GastoResMax)      

      ! DiasMesCInfIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%DiasMesCInfIni

      ParLaja%VDerechosMenIni = ParLaja%VGastoMenMax(1) -               &
     &     FactTiempoH * (24 * ParLaja%DiasMesCInfIni) *                &
     &     (ParLaja%GastoMenMax - ParLaja%GastoResMax)

      ! EcoResTotIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%EcoResTotIni

      ! EcoResTotIni
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%EcoAltoPolTotIni

      ! Vol utili minimo
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%VolUtilMin

      ! Vol Colchon Inferior
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%VOlUtilColInf

      ! Vol Colchon Intermedio
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%VolUtilColMed

      ! Vol a 50cm de rebalse
      READ(URead, '(A1)') AuxVar
      READ(URead, *) VRemA50cmReb
      ParLaja%VolAcumA50cmReb = EmbMaxA(ParLaja%IEmbLaja) - VRemA50cmReb

      ! FIltraciones Historicas
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%QFiltHist

      ! Caudal de Riego Historico Tucapel
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%QRiegoHist

      ! Caudales Nuevos Regantes
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%QNRiegoCSup
      READ(URead, *) ParLaja%QNRiegoCMed
      READ(URead, *) ParLaja%QNRiegoCInf

!     Porcentajes de riego mensual
      READ(URead, '(A1)') AuxVar
      READ(URead, *) (ParLaja%PGastosMesHid(I), I=1, 12)

!     Definicion retiros de riego
      Objeto ='retiro de riego Laja'
      ParLaja%NumRetRiego = 0
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%NumRetRiego


      ALLOCATE(ParLaja%IQRI(ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%IQRIHC(ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%IQRDHC(ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%IQRFHC(ParLaja%NumRetRiego))

      ALLOCATE(ParLaja%IQRIHF(ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%IQRFHF(ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%IQRDHF(ParLaja%NumRetRiego))
      
      ALLOCATE(ParLaja%ICenRetRiego(ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%PRetPRiego(12, ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%PRetNRiego(12, ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%RiegoExtra(12, ParLaja%NumRetRiego))
      
      ALLOCATE(ParLaja%CPRiego(Dim%Eta, ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%CNRiego(Dim%Eta, ParLaja%NumRetRiego))
      ALLOCATE(ParLaja%DRExtra(Dim%Eta, ParLaja%NumRetRiego))


      ICen = 0
      DO Idx=1, ParLaja%NumRetRiego
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCentral
         READ(URead, '(A1)') AuxVar
         READ(URead, *) (PRetPRiego(I), I=1, 12)
         READ(URead, '(A1)') AuxVar
         READ(URead, *) (PRetNRiego(I), I=1, 12)
         READ(URead, '(A1)') AuxVar
         READ(URead, *) (RiegoExtra(I), I=1, 12)
         CALL NomCen2NumCen(NumCen, FWarning, NomCentral,               &
     &        CenNom, NCenHidSPP, Objeto, ULog)
         IF (NumCen .GT. 0) THEN
            ICen = ICen + 1
            ParLaja%ICenRetRiego(ICen) = NumCen
            ParLaja%PRetPRiego(1:12, ICen) = PRetPRiego(1:12)
            ParLaja%PRetNRiego(1:12, ICen) = PRetNRiego(1:12)
            ParLaja%RiegoExtra(1:12, ICen) = RiegoExtra(1:12)

            DO IEta=1, NEtapa
               ParLaja%CPRiego(IEta, ICen) = 1d-2*PRetPRiego(Mes(IEta))
               ParLaja%CNRiego(IEta, ICen) = 1d-2*PRetNRiego(Mes(IEta))
               ParLaja%DRExtra(IEta, ICen) = RiegoExtra(Mes(IEta))
            ENDDO
         ENDIF
      ENDDO
      ParLaja%NumRetRiego = ICen

!     Flags del modelo
      READ(URead, '(A1)') AuxVar

!     Costos variables no combustibles
      READ(URead, '(A1)') AuxVar      
      READ(URead, *) (ParLaja%CQVar(I), I=1, ParLaja%IQG50)

!     Costo almacenamiento volumen alto polcura
      READ(URead, '(A1)') AuxVar      
      READ(URead, *) ParLaja%CostoVAP
     
      ParLaja%CQVar(1:ParLaja%IQG50) =  FactTiempo *                    &
     &     ParLaja%CQVar(1:ParLaja%IQG50)

!     Flag de holgura de turbinamiento de riego
      READ(URead, '(A1)') AuxVar
      READ(URead, *)                                                    &
     &     ParLaja%FHolgTurRiego, ParLaja%FHolgRetRiego,                &
     &     ParLaja%FVertEcoAct, ParLaja%FVolUtilSupAct,                 & 
     &     ParLaja%FVolUtilEcoEnd, ParLaja%FVolUtilEcoAP,               &
     &     ParLaja%FQEconEndAct

!     Costo de riego no servido y otros
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ParLaja%CRiegoNS, ParLaja%CVertEcon,               &
     &     ParLaja%CVolUtilNeg, ParLaja%CSubEconAnu,                    &
     &     ParLaja%FactVolUtilAct, ParLaja%MetodoCalcDef

      ParLaja%CRiegoNS = FactTiempo * ParLaja%CRiegoNS
      ParLaja%CVertEcon = FactTiempo * ParLaja%CVertEcon
      ParLaja%CSubEconAnu = FactTiempo * ParLaja%CSubEconAnu


      IF (ParLaja%UsaCorteOptim) THEN
         ParLaja%PDLDAcNFila = ParLaja%DimPDLDAcFila
         ParLaja%PDLDAcNCol = ParLaja%DimPDLDAcCol
      ELSE
         ParLaja%PDLDAcNFila = 0
         ParLaja%PDLDAcNCol = 0
      ENDIF
!     
      DO Idx = 1, ParLaja%NumColEta
         ParLaja%VarEtaVol(Idx) = (Idx .LE. ParLaja%NumVarVol) 
      ENDDO

!     variables opcionales
      IF (ParLaja%FVertEcoAct) THEN
         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IVRBES = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IVRBES) = 'vrbes' 
         ParLaja%VarEtaVol(ParLaja%IVRBES) = .TRUE.
      ENDIF

      IF (ParLaja%FHolgTurRiego) THEN
         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IQDR = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IQDR) = 'qdr' 
      ENDIF

      IF (ParLaja%FVolUtilSupAct) THEN
         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IVUP = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IVUP) = 'vup' 
         ParLaja%VarEtaVol(ParLaja%IVUP) = .TRUE.

         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IVUN = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IVUN) = 'vun' 
         ParLaja%VarEtaVol(ParLaja%IVUN) = .TRUE.

         ParLaja%NumFilEta = ParLaja%NumFilEta + 1
         ParLaja%IVUP_F = ParLaja%NumFilEta

         ParLaja%NumFilEta = ParLaja%NumFilEta + 1
         ParLaja%IVUN_F = ParLaja%NumFilEta
      ENDIF

      IF (ParLaja%FQEconEndAct) THEN
         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IQEA = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IQEA) = 'qea'
 
         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IQEN = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IQEN) = 'qen' 

         ParLaja%NumColEta = ParLaja%NumColEta + 1
         ParLaja%IQEP = ParLaja%NumColEta
         ParLaja%VarEtaNames(ParLaja%IQEP) = 'qep' 

         ParLaja%NumFilEta = ParLaja%NumFilEta + 1
         ParLaja%IQEA_F = ParLaja%NumFilEta

         ParLaja%NumFilEta = ParLaja%NumFilEta + 1
         ParLaja%IQEN_F = ParLaja%NumFilEta

         ParLaja%NumFilEta = ParLaja%NumFilEta + 1
         ParLaja%IQEP_F = ParLaja%NumFilEta
      ENDIF

      ICol = ParLaja%NumColBlo
      DO Idx=1, ParLaja%NumRetRiego
         CVolB = ' '
         CALL Num2Char(Idx, CVolB, No, DimLargo)

         ICol = ICol + 1
         ParLaja%IQRI(Idx) = ICol
         ParLaja%VarBloNames(ICol) = 'qri' // CVolB

         ParLaja%NumColBlo = ICol
      ENDDO


      ICol = ParLaja%NumColEta
      IFil = ParLaja%NumFilEta
      DO Idx=1, ParLaja%NumRetRiego
         CVolB = ' '
         CALL Num2Char(Idx, CVolB, No, DimLargo)

         ICol = ICol + 1
         ParLaja%IQRIHC(Idx) = ICol
         ParLaja%VarEtaNames(ICol) = 'qrih' // CVolB

         ICol = ICol + 1
         ParLaja%IQRDHC(Idx) = ICol
         ParLaja%VarEtaNames(ICol) = 'qrdh' // CVolB

         IF (ParLaja%FHolgRetRiego) THEN         
            ICol = ICol + 1
            ParLaja%IQRFHC(Idx) = ICol
            ParLaja%VarEtaNames(ICol) = 'qrfh' // CVolB
         ENDIF

         IFil = IFil + 1
         ParLaja%IQRIHF(Idx) = IFil
         IFil = IFil + 1
         ParLaja%IQRFHF(Idx) = IFil
         IFil = IFil + 1
         ParLaja%IQRDHF(Idx) = IFil

         ParLaja%NumColEta = ICol
         ParLaja%NumFilEta = IFil
      ENDDO

!        Suponemos que los datos de Meses y Agnos refieren a
!        calendario hidrologico, es decir, el mes uno corresponde a
!        abril y el agno calendario comienza en el mes 10.
!        
!        Tipo de Etapa para gasto medio
!        INICIOANO:  Etapa coincide con el inicio de mes y de agno
!        INICIOMES:  Etapa coincide con el inicio de mes 
!        INTRAETA:  Etapa dentro de un mes

      ParLaja%TipoEtaGM(1) = INICIOANO

      DO IEta = 2, NEtapa
         IF (Mes(IEta) .EQ. Mes(IEta-1)) THEN
!           Estamos dentro de un mes
            ParLaja%TipoEtaGM(IEta) = INTRAETA
         ELSE
            IF (Mes(IEta) .EQ. ENEROHID) THEN
               ParLaja%TipoEtaGM(IEta) = INICIOANO 
            else
               ParLaja%TipoEtaGM(IEta) = INICIOMES
            ENDIF
         ENDIF
      ENDDO

      RETURN 
      END


!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDLajaN(NArcNom, ISimul,                         & 
     &     NBloque, BloDur, BloEta, TipoEtapa,                          &
     &     EmbFEsc, ParLaja, Dim, ULog)
!
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim


!
      TYPE(PAR_LAJA) ParLaja
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
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
         WRITE(UWrite, '(A, '','', $)') 'Etapa'
         WRITE(UWrite, '(A, '','', $)') 'TipoEtapa'
         WRITE(UWrite, '(A, '','', $)') 'HorasAcum'
         WRITE(UWrite, '(A, '','', $)') 'EmbFac'
         DO Idx = 1, ParLaja%NumColEta
            WRITE(UWrite, '(A, '', '', $)') ParLaja%VarEtaNames(Idx)
         ENDDO
         DO Idx = 1, ParLaja%NumColBlo
            WRITE(UWrite, '(A, '', '', $)') ParLaja%VarBloNames(Idx)
         ENDDO
         WRITE(UWrite, *)
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      ICen = ParLaja%IEmbLaja
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
         WRITE(UWrite, '(A, '', '', $)')  TipoEtapa(IEta)
         WRITE(UWrite, '(F7.1, '', '', $)') Time
         WRITE(UWrite, '(E9.2, '', '', $)') EmbFEsc(ICen)

         DO Idx = 1, ParLaja%NumColEta
            IF (.NOT. ParLaja%VarEtaVol(Idx)) THEN
               WRITE(UWrite, '(F9.3, '', '', $)')                       &
     &              ParLaja%DataEta(Idx, IEta)
            ELSE
               WRITE(UWrite, '(F10.7, '', '', $)')                       &
     &              ParLaja%DataEta(Idx, IEta)*VolScale
            ENDIF
         ENDDO
         DO Idx = 1, ParLaja%NumColBlo
            WRITE(UWrite, '(F7.3, '', '', $)')                          &
     &           ParLaja%DataBlo(Idx, IBlo)
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

      LOGICAL FUNCTION IsVarCorteLaja(ISimul, IEtapaOri, ParLaja, Ind)
      USE PLP

      INTEGER, INTENT(IN):: Ind
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: IEtapaOri
      TYPE(PAR_LAJA), INTENT(IN):: ParLaja

      IF (Ind .GT. ParLaja%DimPDLDAcCol) THEN
         IsVarCorteLaja = .False.
         RETURN
      ENDIF

      IsVarCorteLaja = .TRUE.

!     Gasto Medio Mensual al inicio del mes no es variable de estado
      IF (ParLaja%TipoEtaGM(IEtapaOri) .NE. INTRAETA) THEN
         IF (((Ind .EQ. ParLaja%IVDGMF) .OR. &
     &        (Ind .EQ. ParLaja%IVDMF))) THEN
            IsVarCorteLaja = .False.
         ENDIF
      ENDIF
      
! Gasto Medio Anual al inicio del mes no es variable de estado
      IF (ParLaja%TipoEtaGM(IEtapaOri) .EQ. INICIOANO) THEN
         IF (((Ind .EQ. ParLaja%IVDGAF) .OR. &
     &        (Ind .EQ. ParLaja%IVDAF))) THEN
            IsVarCorteLaja = .False.
         ENDIF
      ENDIF
      
!     Economias de reserva se pierden todas si se esta en el colchon
!     inferior
      IF (.NOT. ParLaja%ColchonInfActivo(ISimul, IEtapaOri)) THEN
         IF ( Ind .EQ. ParLaja%IVERF) THEN
            IsVarCorteLaja = .False.
         ENDIF
      ENDIF
      
      
      RETURN
      END
