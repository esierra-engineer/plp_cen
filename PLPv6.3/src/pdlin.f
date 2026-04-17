!***************
!     Subrutina PDLIN
!***************
      SUBROUTINE PDLin(PriProgDin, PDNumIte, ZSPF, ZSPFBest, ZSDF, kappa, &
     &     NSimul, HorPro, MinPro, SegPro, IUnit)
!
      USE OSI
      INCLUDE 'machcons.fpp'

!
      DOUBLE PRECISION Promedio
      DOUBLE PRECISION Varianza
!
      INTEGER HorPro
      INTEGER MinPro
      INTEGER SegPro

      INTEGER DimLargo
      PARAMETER (DimLargo = 10)

      CHARACTER*(DimLargo) CHorPro
      CHARACTER*(DimLargo) CMinPro
      CHARACTER*(DimLargo) CSegPro
      INTEGER IUnit
      INTEGER NSimul
      INTEGER PDNumIte
      LOGICAL PriProgDin
      DOUBLE PRECISION ErrRel
      DOUBLE PRECISION sigma
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION ZSPFProm
      DOUBLE PRECISION ZSPFVar
      DOUBLE PRECISION ZSPF(NSimul)
      DOUBLE PRECISION ZSPFBest
      DOUBLE PRECISION kappa

      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!
      CALL Num2Char (HorPro, CHorPro, .TRUE., 2)
      CALL Num2Char (MinPro, CMinPro, .TRUE., 2)
      CALL Num2Char (SegPro, CSegPro, .TRUE., 2)
      IF (PriProgDin) THEN
         ZSPFProm = Promedio (ZSPF, NSimul)
         IF (NSimul .EQ. 1) THEN
            ErrRel = DABS(ZSPFBest - ZSDF)/                             &
     &           MAX(DAbs(ZSPFBest), DAbs(ZSDF), DEPSILON)*100.d0
            WRITE(IUnit, 98) 'pdlin:', PDNumIte, ZSDF, ZSPFProm,        &
     &           ErrRel, kappa, CHorPro, CMinPro, CSegPro
         ELSE
            ZSPFVar = Varianza (ZSPF, ZSPFProm, NSimul)
            IF (ABS(ZSPFVar) .LT. SEPSILON) THEN
               sigma = DINFTY
            ELSE
               sigma = ABS(ZSPFBest - ZSDF)/ABS(sqrt(ZSPFVar))
            ENDIF
            WRITE(IUnit, 99) 'pdlin:', PDNumIte, ZSDF, ZSPFProm,        &
     &           sigma, kappa, CHorPro, CMinPro, CSegPro
         ENDIF
         CALL Vomit(IUnit)
      ENDIF
 98   FORMAT(A6, 1x, I5, 2x, E15.8, 2x, E15.8, 2x, F12.8, 2x, E11.4,    &
     &     1x, 2x, A2, ':', A2, ':', A2)
 99   FORMAT(A6, 1x, I5, 2x, E15.8, 2x, E15.8, 2x, F12.8, 2x, E11.4,    &
     &     1x, 2x, A2, ':', A2, ':', A2)
      RETURN
      END
