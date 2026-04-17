      SUBROUTINE Num2Char(Numero, CNumero, Relleno, Largo)
      USE PLP, ONLY : DimLargo
      CHARACTER*(DimLargo) CNumero
      INTEGER CopiaNumero
      INTEGER ILargo
      INTEGER Largo
      INTEGER Numero
      INTEGER NCeros
      INTEGER ICero
      INTEGER Resto
      LOGICAL Relleno
      IF ((Largo .GT. DimLargo) .OR. (Largo .LT. 0)) THEN
         WRITE(6, '(A, I3, A)')                                         &
     &        'num2char: Error, largo fuera de rango ', Largo, '.'
      ELSE
!     los numeros mayores que 10**Largo pierden las cifras mas significa
         Resto = Numero
         DO ILargo = Largo, 1, -1
            CopiaNumero = Resto - (Resto/10)*10
            CNumero(ILargo:ILargo) = char(CopiaNumero + 48)
            Resto = Resto/10
         ENDDO
         NCeros = 1
         IF (.NOT. Relleno) THEN
!     el string CNumero se cambia de '00123' a '123'
            DO WHILE ((NCeros .LE. Largo) .AND.                         &
     &           (CNumero(NCeros:NCeros) .EQ. '0'))
               NCeros = NCeros + 1
            ENDDO
            NCeros = NCeros - 1
            DO ICero = 1, Largo - NCeros
               CNumero(ICero:ICero) =                                   &
     &              CNumero(ICero + NCeros:ICero + NCeros)
            ENDDO
            DO ICero = Largo - NCeros + 1, Largo
               CNumero(ICero:ICero) = ' '
            ENDDO
         ENDIF
      ENDIF
      DO ICero = Largo + 1, DimLargo
         CNumero(ICero:ICero) = CHAR(0)
      ENDDO
      RETURN
      END
