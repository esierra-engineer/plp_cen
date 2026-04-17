      CHARACTER*3 FUNCTION ExtClave(Clave,label)
      INCLUDE 'pxp.fpp'
      CHARACTER*1 Clave
      CHARACTER*3 label
      LOGICAL claveok
      claveok = .FALSE.
      IF (Clave .EQ. PCenTipEmb) THEN
         ExtClave = 'Emb'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipEmbAux) THEN
         ExtClave = 'Aux'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipPas) THEN
         ExtClave = 'Pas'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipSer) THEN
         ExtClave = 'Ser'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipTer) THEN
         ExtClave = 'Ter'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipMod) THEN
         ExtClave =  label
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipBat) THEN
         ExtClave = 'BAT'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipRie) THEN
         ExtClave = 'Rie'
         claveok = .TRUE.
      ENDIF
      IF (Clave .EQ. PCenTipFal) THEN
         ExtClave = 'Fal'
         claveok = .TRUE.
      ENDIF
      IF (.NOT. claveok) THEN
         WRITE(6, '(A)') 'extclave: Error en la clave.'
         STOP 1
      ENDIF
      RETURN
      END
