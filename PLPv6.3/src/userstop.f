      LOGICAL FUNCTION UserStop()
!     nombre archivos
      INCLUDE 'pxp.fpp'
      CHARACTER ParoONoParo
      INTEGER Abrir
      INTEGER URead
!
      UserStop = .FALSE.
      URead = Abrir(NArcUStop, 'UNKNOWN', 'SEQUENTIAL', 0)
      REWIND (URead)
      READ (URead, '(A)', err = 10) ParoONoParo
      IF ((ParoONoParo .EQ. 'P') .OR. (ParoONoParo .EQ. 'p')) THEN
         UserStop = .TRUE.
      ENDIF
   10 CALL Cerrar(URead)
      RETURN
      END
