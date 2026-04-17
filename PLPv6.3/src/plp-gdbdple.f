      SUBROUTINE GraDatPlaEmb(IPDNumIte, UWritePhi, ScaleVol, GradxPhi, LDPhiPrv, &
     &     NSimul, PDLDAcNCol, NEtapa, Dim, ULog)
      USE PLP, ONLY : PAR_DIMS, NArcPlaEmbO

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog


      EXTERNAL DxAEQy
      DOUBLE PRECISION LDPhiPrv(Dim%Simul, Dim%Eta)
      DOUBLE PRECISION GradxPhi(Dim%PDLDAcCol, Dim%Simul, Dim%Eta)
      INTEGER IPDNumIte
      INTEGER UWritePhi

      DOUBLE PRECISION ScaleVol(Dim%PDLDAcCol)
      INTEGER Abrir
      INTEGER IEtapa
      INTEGER ISimul
      INTEGER NEtapa
      INTEGER NSimul
      INTEGER PDLDAcNCol
      LOGICAL DxAEQy
      INTEGER IColAcop
!
!
!     Imprime la funcion de costo futuro el final de la primera etapa
!
      IF (UWritePhi .EQ. 0) THEN 
         UWritePhi = Abrir(NArcPlaEmbO, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWritePhi, '(A, $)')                                        &
     &        '#IPDNumIte, IEtapa, ISimul, LDPhiPrv'
         DO IColAcop = 1, PDLDAcNCol
            WRITE(UWritePhi, '('', Emb'', I3, $)') IColAcop
         ENDDO 
         WRITE (UWritePhi, *)
      ENDIF

      DO IEtapa = 2, NEtapa
         DO ISimul = 1, NSimul
            WRITE (UWritePhi, '(3(I3, '',''), E15.8, $)')            &
     &           IPDNumIte, IEtapa - 1, ISimul,                      &
     &           LDPhiPrv(ISimul, IEtapa)
            DO IColAcop = 1, PDLDAcNCol
               WRITE (UWritePhi, '('','', E15.8, $)') &
     &              GradxPhi(IColAcop, ISimul, IEtapa)/ScaleVol(IColAcop)
            ENDDO
            WRITE (UWritePhi, *)
         ENDDO
      ENDDO

      RETURN
      END
