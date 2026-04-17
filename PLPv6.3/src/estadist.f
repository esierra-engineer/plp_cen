      DOUBLE PRECISION FUNCTION promedio (x, n)
      IMPLICIT NONE
      DOUBLE PRECISION x (*)
      INTEGER n
      INTEGER i
      promedio = 0.0d0
      DO i = 1, n
         promedio = promedio + x(i)
      ENDDO
      promedio = promedio/DBLE(n)
      RETURN
      END
      DOUBLE PRECISION FUNCTION varianza (x, mux, n)
      IMPLICIT NONE
      DOUBLE PRECISION x (*)
      DOUBLE PRECISION mux
      INTEGER n
      INTEGER i
      varianza = 0.0d0
      DO i = 1, n
         varianza = varianza + x (i)*x (i)
      ENDDO
      varianza = varianza/DBLE (n) - mux*mux
      RETURN
      END
