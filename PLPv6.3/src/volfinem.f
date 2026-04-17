!**********************************
!     Subrutina Vol Final Embalses
!**********************************
      SUBROUTINE VolFinEmb(NEtapa, NCenEmb, FVolFinEmb,                &
     &     EmbCFUE, EmbVFin, EmbVMin, EmbVMax, Dim)
      USE PLP, ONLY : PAR_DIMS

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!     Variables Globales
!***********************
      INTEGER NEtapa
      INTEGER NCenEmb
      LOGICAL FVolFinEmb
      LOGICAL EmbCFUE(Dim%Emb)
      DOUBLE PRECISION EmbVMin(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVMax(Dim%Emb, Dim%Eta)
      DOUBLE PRECISION EmbVFin(Dim%Emb)
!     Variables Locales
!**********************
      INTEGER IEmb
      DO IEmb = 1, NCenEmb
         IF (FVolFinEmb .OR. (.NOT. EmbCFUE(IEmb))) THEN
            EmbVMin(IEmb, NEtapa) = EmbVFin(IEmb)
            EmbVMax(IEmb, NEtapa) = MAX(EmbVMax(IEmb, NEtapa),        &
     &           EmbVFin(IEmb))
         ENDIF
      ENDDO
      RETURN
      END
