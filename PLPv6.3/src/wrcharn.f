      SUBROUTINE WriteCharN(NBackSpace, CChar)
      IMPLICIT NONE
      CHARACTER CChar
      INTEGER NBackSpace, I
      DO I = 1, NBackSpace
         WRITE(6, '(A, $)') CChar
      ENDDO
      RETURN
      END
