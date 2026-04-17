      SUBROUTINE UpperCase(Cadena, CadMay)
      IMPLICIT NONE
      CHARACTER*(*) Cadena
      CHARACTER*(*) CadMay
      INTEGER LCadena
      INTEGER i
      INTEGER j
      INTEGER L
!
      CadMay = Cadena
      L = LCadena(CadMay)
      DO i = 1, L
         j = ICHAR(CadMay(i:i))
         IF ((j .GE. 97) .AND.                                          &
     &        (j .LE. 122)) THEN
            CadMay(i:i) = CHAR(j - 32)
         ENDIF
      ENDDO
      RETURN
      END
