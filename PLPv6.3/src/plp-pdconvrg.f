      LOGICAL FUNCTION PDConvrg(PDNumIte, PDMaxIte, ZSPFPromBest,       &
     &     NSimul, ZSPF, ZSDF, UmbIntConf, PDError,                     &
     &     FConvPGradx, FConvPVar, UmbGradX, UmbZSPF,                   &
     &     GradxPhi, GradxPhiECF, NumEtaCF, PDLDAcNCol, NEtapa, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

      INCLUDE 'machcons.fpp'
      EXTERNAL Promedio
      EXTERNAL Varianza

      INTEGER NSimul

      DOUBLE PRECISION epsilon
      DOUBLE PRECISION Error
      DOUBLE PRECISION PDError
      DOUBLE PRECISION Promedio
      DOUBLE PRECISION Suma
      DOUBLE PRECISION UmbIntConf
      DOUBLE PRECISION UmbGradX
      DOUBLE PRECISION UmbZSPF
      DOUBLE PRECISION Varianza
      DOUBLE PRECISION x
      DOUBLE PRECISION y
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION ZSPF(NSimul)
      DOUBLE PRECISION ZSPFProm
      DOUBLE PRECISION ZSPFPromBest
      DOUBLE PRECISION ZSPFVar

      INTEGER IColAcop
      INTEGER IEtapa
      INTEGER ISimul
      INTEGER NEtapa
      INTEGER NumEtaCF
      INTEGER PDLDAcNCol
      INTEGER PDMaxIte
      INTEGER PDNumIte
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      DOUBLE PRECISION GradxPhiECF(Dim%PDLDAcCol, Dim%Simul)

      LOGICAL DxAEQy
      LOGICAL FConvPGradX
      LOGICAL FConvPVar
      LOGICAL PDConvrgVal
      LOGICAL USerStop
      EXTERNAL DxAEQy

!     codigo:
      IF (USerStop()) THEN
         PDConvrgVal = .TRUE.
      ELSE
         PDConvrgVal = .FALSE.
         IF (FConvPGradX .AND. (NumEtaCF .LT. NEtapa)) THEN
            IEtapa = NumEtaCF + 1
            IF (PDNumIte .GE. 2) THEN
               Suma = 0d0
               DO ISimul = 1, NSimul
                  DO IColAcop = 1, PDLDAcNCol
                     x = GradxPhiECF(IColAcop, ISimul)
                     y = GradxPhi(IColAcop, ISimul, IEtapa)
                     Suma = MAX(Suma, DABS(x - y))
                  ENDDO
               ENDDO
               
               IF (Suma .LT. UmbGradX) THEN
                  PDConvrgVal = .TRUE.
               ENDIF
            ENDIF
            GradxPhiECF(1:PDLDAcNCol, 1:NSimul) =  &
     &           GradxPhi(1:PDLDAcNCol, 1:NSimul, IEtapa)
         ENDIF
         ZSPFProm = Promedio (ZSPF, NSimul)
         ZSPFVar = Varianza (ZSPF, ZSPFProm, NSimul)
         IF (                                                           &
     &        (FConvPVar) .AND.                                         &
     &        (DABS(ZSPFVar - ZSPFProm) .LT.                            &
     &        DABS(UmbZSPF*ZSPFProm))                                   &
     &        ) THEN
            PDConvrgVal = .TRUE.
         ENDIF
         IF (NSimul .EQ. 1) THEN
            Error = ZSPFPromBest - ZSDF
            epsilon = DABS(ZSPFPromBest)*PDError/100.d0
         ELSE
            Error = DABS(ZSPFPromBest - ZSDF)
            epsilon = DABS (SQRT (ZSPFVar) )*UmbIntConf
         ENDIF
         IF (Error .LE. epsilon) THEN
            PDConvrgVal = .TRUE.
         ELSE IF (PDNumIte .GE. PDMaxIte) THEN
            PDConvrgVal = .TRUE.
         ENDIF
      ENDIF
      PDConvrg = PDConvrgVal
      RETURN
      END
