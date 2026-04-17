!****************************************************************
!     asigna al lado derecho los caudales de cada muestra, sea esta de
!     simulacion o de apertura; en el caso de los embalses, los
!     caudales se suman al volumen preexistente
!****************************************************************
      SUBROUTINE FijaMues(IEtapa, NBloque, CenPMax, CenRen,             &
     &     BloInd, BloDur, FactTiempo, FScaleQs,                        &
     &     EstocNFila, EstocFilaInd, EstocNCol, EstocColInd,            &
     &     EstocRHSP, EstocUBP,                                         &
     &     IClaseFila, IClaseCol,                                       &
     &     FBaterias,ParBaterias,                                       &
     &     lp, Dim)
      USE PLP, ONLY : PAR_DIMS, PAR_BATERIAS, C_SIZE_T
      TYPE(PAR_DIMS), INTENT(IN):: Dim


      INTEGER, INTENT(IN):: EstocColInd(Dim%EstocCol, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: EstocFilaInd(Dim%EstocFila, Dim%IBlo, Dim%Eta)
      INTEGER, INTENT(IN):: EstocNCol
      INTEGER, INTENT(IN):: EstocNFila
      INTEGER, INTENT(IN):: IClaseFila(EstocNFila)
      INTEGER, INTENT(IN):: IClaseCol(EstocNCol)
      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: NBloque
      INTEGER, INTENT(IN):: BloInd(NBloque)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: FactTiempo
      LOGICAL, INTENT(IN):: FScaleQs
      DOUBLE PRECISION, INTENT(IN):: CenPMax(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION, INTENT(IN):: CenRen(Dim%Cen)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(IN):: EstocUBP(Dim%EstocCol, Dim%Blo, Dim%Clase)
      LOGICAL, INTENT(IN):: FBaterias
      TYPE(PAR_BATERIAS), INTENT(IN):: ParBaterias
      
!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp
!     locals
      INTEGER IEstocCol
      INTEGER IEstocFila
      INTEGER IBEstoc
      INTEGER IBlo
      INTEGER IBInd
      INTEGER ICen
      INTEGER IInyPasada

      INTEGER MFRhsInd (EstocNFila*NBloque)
      DOUBLE PRECISION MFRhsVal (EstocNFila*NBloque)

      INTEGER MCUppInd (EstocNCol*NBloque)
      DOUBLE PRECISION MCUppVal (EstocNCol*NBloque)

      INTEGER MFRhsInd_Bat(ParBaterias%NInyPasada*NBloque)
      DOUBLE PRECISION MFRhsVal_Bat(ParBaterias%NInyPasada*NBloque)
      INTEGER IInyPasada_to_IEstocCol(ParBaterias%NInyPasada)
!     codigo:

!
!     rows, se modifican los RHS con los alfuentes de los balances de agua de embalses y serie
!
      IBEstoc = 0
      DO IEstocFila = 1, EstocNFila
         DO IBlo = 1, NBloque
            IBInd = BloInd(IBlo)            
            IBEstoc = IBEstoc + 1
            MFRhsInd(IBEstoc) = EstocFilaInd(IEstocFila, IBlo, IEtapa)
            IF (FScaleQs) THEN
               MFRhsVal(IBEstoc) =                                      &
     &              EstocRHSP(IEstocFila, IBInd, IClaseFila(IEstocFila))                
            ELSE
               MFRhsVal(IBEstoc) = FactTiempo*BloDur(IBInd)*            &
     &              EstocRHSP(IEstocFila, IBInd, IClaseFila(IEstocFila))                
            ENDIF
         ENDDO
      ENDDO
      CALL ModifBrdRhs(IBEstoc, MFRhsInd, MFRhsVal, lp)

!
!     cols, se modifican los afluentes de las centrales de pasada
!
      IBEstoc = 0
      DO IEstocCol = 1, EstocNCol
         ICen = EstocNFila + IEstocCol
         IF (FBaterias) THEN
            DO IInyPasada=1, ParBaterias%NInyPasada
                  IF (ICen .EQ. ParBaterias%ICen_Pasada(IInyPasada)) THEN
                        IInyPasada_to_IEstocCol(IInyPasada)=IEstocCol
                  ENDIF
            ENDDO
         ENDIF
         DO IBlo = 1, NBloque
            IBInd = BloInd(IBlo)            
            IBEstoc = IBEstoc + 1
            MCUppInd(IBEstoc) =                                         &
     &           EstocColInd(IEstocCol, IBlo, IEtapa)
            MCUppVal(IBEstoc) =                                         &
     &           MIN(EstocUBP(IEstocCol, IBInd, IClaseCol(IEstocCol)),  &
     &           CenPMax(ICen, IBInd)/CenRen(ICen))
         ENDDO            
      ENDDO
      CALL ModifUpp(IBEstoc, MCUppInd, MCUppVal, lp)

      IF (FBaterias) THEN
            IBEstoc=0
            DO IInyPasada=1, ParBaterias%NInyPasada
                  ICen= ParBaterias%ICen_Pasada(IInyPasada)
                  IEstocCol= IInyPasada_to_IEstocCol(IInyPasada)
                  DO IBlo = 1, NBloque
                        IBInd = BloInd(IBlo)
                        IBEstoc = IBEstoc + 1
                        MFRhsInd_Bat(IBEstoc)= ParBaterias%ECg_pfil_pasada(IInyPasada,IBlo,IEtapa)
                        MFRhsVal_Bat(IBEstoc)= MIN(EstocUBP(IEstocCol, IBInd, IClaseCol(IEstocCol)),  &
     &           CenPMax(ICen, IBInd)/CenRen(ICen))
                  ENDDO
            ENDDO
            IF (IBEstoc .NE. 0) THEN
                  CALL ModifBrdRhs(IBEstoc, MFRhsInd_Bat, MFRhsVal_Bat, lp)
            ENDIF
      ENDIF
      RETURN
      END
