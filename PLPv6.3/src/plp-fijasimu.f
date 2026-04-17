!**********************************************************
!     asigna el lado derecho las cotas iniciales de cada embalse
!     segun simulacion
!**********************************************************
      SUBROUTINE FijaSimu(IEtapa, ScaleVol,                             &
     &     VolIni,                                                      &
     &     FVertReb, NEmbVReb, EmbVRebInd, EmbVRebFilInd, EmbVReb,      &
     &     NumEmb,                                                      & 
     &     PDLDAcFilaInd,                                               &
     &     lp, Dim)
      USE PLP, ONLY : PAR_DIMS, C_SIZE_T
      TYPE(PAR_DIMS), INTENT(IN):: Dim

      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: PDLDAcFilaInd(Dim%PDLDAcFila, Dim%Eta)
      LOGICAL, INTENT(IN):: FVertReb
      INTEGER, INTENT(IN):: NumEmb
      INTEGER, INTENT(IN):: NEmbVReb
      INTEGER, INTENT(IN):: EmbVRebInd(NEmbVReb)
      INTEGER, INTENT(IN):: EmbVRebFilInd(Dim%EmbVReb, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: EmbVReb(NEmbVReb)
      DOUBLE PRECISION, INTENT(IN):: VolIni(Dim%Emb)
      DOUBLE PRECISION, INTENT(IN):: ScaleVol(Dim%Emb)

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp

!     locals
      INTEGER IFilaAcop
      INTEGER IReb
      INTEGER IEmb

      DOUBLE PRECISION MFRhsVal (NumEmb)
      DOUBLE PRECISION MFRebVal (NEmbVReb)

!     codigo:

      DO IFilaAcop = 1, NumEmb
         MFRhsVal(IFilaAcop) = VolIni(IFilaAcop) / ScaleVol(IFilaAcop)
      ENDDO
      CALL ModifBrdRhs(NumEmb, PDLDAcFilaInd(1, IEtapa),    &
     &     MFRhsVal, lp)

      IF (FVertReb) THEN
         DO IReb = 1, NEmbVReb
            IEmb = EmbVRebInd(IReb)
            MFRebVal(IReb) = (EmbVReb(IReb)                      &
     &           - VolIni(IEmb)) / ScaleVol(IEmb)
!            MFRebVal(IReb) = MAX(MFRebVal(IReb), 0.0d0)
         ENDDO
         CALL ModifBrdRhs(NEmbVReb, EmbVRebFilInd(1, IEtapa),    &
     &        MFRebVal, lp)
      ENDIF

      RETURN
      END

      SUBROUTINE FijaVar(IEtapa,                                        &
     &     ScaleObj, ScalePhi,                                          &
     &     PDNCol,                                                      &
     &     ISimul, NSimul, PDNumIte, NEtapa,                            &
     &     FOnePhi, FSeparaFCF, FDepHidEta,                             &
     &     lp)
      
      USE PLP
      
      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: NEtapa
      INTEGER, INTENT(IN):: NSimul
      INTEGER, INTENT(IN):: PDNCol(NEtapa)
      INTEGER, INTENT(IN):: PDNumIte
      LOGICAL, INTENT(IN):: FOnePhi
      LOGICAL, INTENT(IN):: FDepHidEta(NEtapa)
      LOGICAL, INTENT(IN):: FSeparaFCF
      DOUBLE PRECISION, INTENT(IN):: ScaleObj
      DOUBLE PRECISION, INTENT(IN):: ScalePhi

!     outs
      INTEGER(C_SIZE_T), INTENT(INOUT):: lp
      
!     locals
      INTEGER ICol
      INTEGER ICol2

      INTEGER ModifNObj
      INTEGER MCObjInd (2)
      DOUBLE PRECISION MCObjVal (2)


!
!     PELIGRO, WILL ROBINSON, PELIGRO!(1)
!     El unico criterio para decidir si la matriz de transicion es igual
!     la identidad o igual a 1/NVarphi consiste en revisar si la etapa
!     corresponde al invierno o al deshielo.
      IF (                                                              &
     &     (PDNumIte .GT. 0) .AND.                                      &
     &     (.NOT. FOnePhi) .AND.                                        &
     &     (IEtapa .LT. NEtapa)) THEN
         IF ((FSeparaFCF) .OR.                                          &
     &        (FDepHidEta(IEtapa))) THEN
            ICol  = ISimul
            ICol2 = MOD(NSimul - 2 + ICol, NSimul) + 1
            ICol  = ICol  + PDNCol(IEtapa)
            ICol2 = ICol2 + PDNCol(IEtapa)
            MCObjInd(1) = ICol
            MCObjVal(1) = ScalePhi/ScaleObj
            MCObjInd(2) = ICol2
            MCObjVal(2) = 0.0d0
            ModifNObj = 2
            CALL ModifObj(ModifNObj, MCObjInd, MCObjVal, lp)
         ENDIF
      ENDIF
      RETURN
      END
