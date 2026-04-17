!***************
!     Subrutina PDFin
!***************
      SUBROUTINE PDFin(PriProgDin, PDNumIte, ZSPF, ZSPFBest, ZSDF,      &
     &     kappa, NSimul, IUnit)
!
      USE OSI
      INCLUDE 'machcons.fpp'
!
      DOUBLE PRECISION Promedio
      DOUBLE PRECISION Varianza
!
      INTEGER IUnit
      INTEGER NSimul
      INTEGER PDNumIte
      LOGICAL PriProgDin
      DOUBLE PRECISION ErrRel
      DOUBLE PRECISION sigma
      DOUBLE PRECISION ZSDF
      DOUBLE PRECISION ZSPF(NSimul)
      DOUBLE PRECISION ZSPFBest
      DOUBLE PRECISION ZSPFProm
      DOUBLE PRECISION ZSPFVar
      DOUBLE PRECISION kappa


      DOUBLE PRECISION DINFTY
      DINFTY = osi_getinfty()

!
      IF (PriProgDin) THEN
         ZSPFProm = Promedio (ZSPF, NSimul)
         IF (NSimul .EQ. 1) THEN
            ErrRel = DABS(ZSPFBest - ZSDF)/                             &
     &           MAX(DAbs(ZSPFBest), DAbs(ZSDF), DEPSILON)*100.d0
            WRITE(IUnit, 98) 'pdfin:', PDNumIte, ZSDF, ZSPFProm,        &
     &           ErrRel, kappa
         ELSE
            ZSPFVar = Varianza (ZSPF, ZSPFProm, NSimul)
            IF (ABS(ZSPFVar) .LT. SEPSILON) THEN
               sigma = DINFTY
            ELSE
               sigma = ABS(ZSPFBest - ZSDF)/ABS(sqrt(ZSPFVar))
            ENDIF
            WRITE(IUnit, 99) 'pdfin:', PDNumIte, ZSDF, ZSPFProm,        &
     &           sigma, kappa
         ENDIF
         CALL Vomit(IUnit)
      ENDIF
   98 FORMAT(A6, 1x, I5, 2x, E15.8, 2x, E15.8, 2x, F12.8, 2x, E11.4)
   99 FORMAT(A6, 1x, I5, 2x, E15.8, 2x, E15.8, 2x, F12.8, 2x, E11.4)
      RETURN
      END
