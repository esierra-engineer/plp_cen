      SUBROUTINE MatIncid(AIncid, NHidSPP, A,          &
     &     NCentral, NCenEmb, NCenSer, NBarra, NFlujo)
!
!     Archivos comunes a todas las rutinas.
      USE A_MATRIX
      TYPE(AMatrix) A
      INTEGER NHidSPP
      INTEGER NCenEmb
      INTEGER NCenSer
      INTEGER AIncid(NHidSPP, 0:NHidSPP)
      INTEGER AIncid0(NCenEmb + NCenSer, NCenEmb + NCenSer)
      INTEGER AIncid1(NCenEmb + NCenSer, NCenEmb + NCenSer)
      INTEGER AIncid2(NCenEmb + NCenSer, NCenEmb + NCenSer)
      INTEGER AIncid3(NCenEmb + NCenSer, NCenEmb + NCenSer)
      INTEGER COffSet
      INTEGER FOffSet
      INTEGER ICen
      INTEGER ICenI
      INTEGER ICenJ
      INTEGER ICenK
      INTEGER NBarra
      INTEGER NCenHidSPP
      INTEGER NCenIncid
      INTEGER NCentral
      INTEGER NFlujo

      FOffSet = NBarra
      NCenHidSPP = NCenEmb + NCenSer
      COffSet = NCentral + 2*NFlujo
      DO ICenJ = 1, NCenHidSPP
         DO ICenI = 1, NCenHidSPP
            AIncid1(ICenJ, ICenI) = -INT(MIN(                           &
     &           Am_get(A, ICenJ, FOffSet + ICenI),                             &
     &           Am_get(A, COffSet + ICenJ, FOffSet + ICenI)))
            AIncid2(ICenJ, ICenI) = AIncid1(ICenJ, ICenI)
            AIncid0(ICenJ, ICenI) = AIncid1(ICenJ, ICenI)
         ENDDO
         AIncid1(ICenJ, ICenJ) = 0
         AIncid2(ICenJ, ICenJ) = 0
         AIncid0(ICenJ, ICenJ) = 1
      ENDDO
      DO ICen = 2, NCenHidSPP
         DO ICenJ = 1, NCenHidSPP
            DO ICenI = 1, NCenHidSPP
               AIncid3(ICenI, ICenJ) = 0
               DO ICenK = 1, NCenHidSPP
                  AIncid3(ICenI, ICenJ) = AIncid3(ICenI, ICenJ) +       &
     &                 AIncid2(ICenI, ICenK)*AIncid1(ICenK, ICenJ)
               ENDDO
            ENDDO
         ENDDO
         DO ICenJ = 1, NCenHidSPP
            DO ICenI = 1, NCenHidSPP
               AIncid2(ICenI, ICenJ) = AIncid3(ICenI, ICenJ)
               AIncid0(ICenI, ICenJ) = AIncid0(ICenI, ICenJ) +          &
     &              AIncid2(ICenI, ICenJ)
            ENDDO
         ENDDO
      ENDDO
      DO ICenI = 1, NCenHidSPP
         NCenIncid = 0
         DO ICenJ = 1, NCenHidSPP
            IF (AIncid0(ICenI, ICenJ) .NE. 0) THEN
               NCenIncid = NCenIncid + 1
               AIncid(ICenI, NCenIncid) = ICenJ
            ENDIF
            AIncid(ICenI, 0) = NCenIncid
         ENDDO
      ENDDO
      RETURN
      END
