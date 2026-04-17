!***********************
      SUBROUTINE LeeTmp(Tmp)
!***********************
      INCLUDE 'pxp.fpp'
!     Lee tiempo
      INTRINSIC date_and_time
      INTEGER values(8)

      INTEGER Tmp(DimTmp)
      CHARACTER fecha*20
      CHARACTER fechatmp*30
      CHARACTER*3 NomMes(12)
      PARAMETER (NomMes = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',    &
     &     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'])
      CHARACTER*3 NumMes(12)
      PARAMETER (NumMes = [ '01', '02', '03', '04', '05', '06',         &
     &     '07', '08', '09', '10', '11', '12'])
      INTEGER I
      LOGICAL NotFound
      LOGICAL PrimeraVez
      DATA PrimeraVez /.TRUE./
      SAVE PrimeraVez
!
      INTEGER Abrir
      INTEGER UWrite
      INTEGER ULog

!
      ULog = 6
      IF (PrimeraVez) THEN
         CALL fdate(fechatmp)
         fecha(1:4) = fechatmp(21:24)
         fecha(5:5) = '.'
         I = 1
         NotFound = .TRUE.
         DO WHILE ((I .LE. 12)                                          &
     &        .AND. NotFound)
            IF (NomMes(I) .EQ. fechatmp(5:7)) THEN
               fecha(6:7) = NumMes(I)
               NotFound = .FALSE.
            ENDIF
            I = I + 1
         ENDDO
         IF (NotFound) THEN
            WRITE(6, '(A)') 'leetmp: Error en rutina'
            STOP 1
         ENDIF
         fecha(8:8) = '.'
         fecha(9:10) = fechatmp(9:10)
         IF (fecha(9:9) .EQ. ' ') fecha(9:9) = '0'
         fecha(11:11) = '.'
         fecha(12:19) = fechatmp(12:19)
         fecha(14:14) = 'h'
         fecha(17:17) = 'm'
         fecha(20:20) = 's'
         UWrite = Abrir(NArcFecha, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(A)') fecha
!$$$  WRITE(UWrite, '(I4, 5('':'', I2))') (Tmp(i), i = 1, 6)
         CALL Cerrar(UWrite)
         PrimeraVez = .FALSE.
      ENDIF
      CALL date_and_time(VALUES=values)
      Tmp(1) = values(3)
      Tmp(2) = values(2)
      Tmp(3) = values(1)
      Tmp(4) = values(5)
      Tmp(5) = values(6)
      Tmp(6) = values(7)
      Tmp(7) = 0
      RETURN
      END
