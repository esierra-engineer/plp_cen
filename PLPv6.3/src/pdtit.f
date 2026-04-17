!***************
!     Subrutina PDTIT
!***************
      SUBROUTINE PDTit(PriProgDin, NSimul, IUnit)
      IMPLICIT NONE
      INTEGER IUnit
      INTEGER NSimul
      LOGICAL PriProgDin
      IF (PriProgDin) THEN
         IF (NSimul .EQ. 1) THEN
            WRITE(IUnit, '(4A)') 'pdtit:pditer',                        &
     &           '       fase dual',                                    &
     &           '     fase primal',                                    &
     &           '       gap        kappa     tiempo'
         ELSE
            WRITE(IUnit, '(4A)') 'pdtit:pditer',                        &
     &           '       fase dual',                                    &
     &           '     fase primal',                                    &
     &           '       gap        kappa     tiempo'
         ENDIF
         CALL Vomit(IUnit)
      ENDIF
      RETURN
      END
