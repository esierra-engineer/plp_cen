!     Rutina que convierte nombre de central a su número
      SUBROUTINE NomCen2NumCen(NumCen, FWarning, NomCen,                &
     &     CenNom, NCentral, Objeto, ULog)
!     NumCen: El número que devuelve
!     NomCen: El nombre que se entrega
!     CenNom: Array de donde busca los nombres
!     NCentral: Numero del total de centrales que se busca
!     Objeto: Archivo que se esta utilizando (para el log solamente)
!     ULog: El Log.
      CHARACTER*48 NomCen
      CHARACTER*(*) Objeto
      INTEGER NCentral
      CHARACTER*48 CenNom(NCentral)
      INTEGER ICen
      INTEGER NumCen
      INTEGER ULog
      LOGICAL FWarning
!
      NumCen = 0
      ICen = 1
      DO WHILE ((NumCen .EQ. 0) .AND. (ICen .LE. NCentral))
         IF (CenNom(ICen) .EQ. NomCen) THEN
            NumCen = ICen
         ENDIF
         ICen = ICen + 1
      ENDDO
      IF (NumCen .EQ. 0) THEN
         WRITE(6, '(6A)') 'nomcnumc: warning, ', &
     &        trim(Objeto), " '", trim(NomCen), "'", ' no existe.'
         IF (ULog .gt. 0) THEN
            WRITE(ULog, '(6A)') 'nomcnumc: warning, ', &
     &           trim(Objeto), " '", trim(NomCen), "'", ' no existe.'
         ENDIF
         FWarning = .TRUE.
      ELSE
         FWarning = .FALSE.
      ENDIF
      RETURN
      END
