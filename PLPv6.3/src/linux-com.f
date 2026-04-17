!     Estas rutinas dependen del Sistema Operativo
!     Esta es la version para Fortran Linux
!***************************
      INTEGER FUNCTION lcadena(S)
!***************************
!     Determina largo de una cadena
      IMPLICIT NONE
      CHARACTER*(*) S
      INTRINSIC len_trim
!     INTEGER len_trim
      INTEGER L
      INTEGER L0
      L0 = len_trim (S)
      L = 0
      DO WHILE ((L .LT. L0))
         if (S(L + 1:L + 1) .EQ. char(0)) then
            exit
         endif
         L = L + 1
      ENDDO
      DO WHILE (L .GT. 0)
         if (S(L:L) .NE. ' ') then
            exit
         endif
         L = L - 1
      ENDDO
      lcadena = L
      RETURN
      END
!**************************************
      recursive FUNCTION fConcat(S1, S2)
!**************************************
!     Concatena dos string
      IMPLICIT NONE
      CHARACTER*(*) S1, S2
!     CHARACTER*(*) fConcat
      CHARACTER*80 fConcat
      CHARACTER*80 buffer
      INTEGER LT, L1, L2
      INTEGER I
      INTEGER lcadena
      L1 = lcadena(S1)
      L2 = lcadena(S2)
      LT = L1 + L2
      IF (LT .EQ. 0) THEN
         buffer(1:1) = char(0)
      ELSE IF (L1 .EQ. 0) THEN
         buffer(1:LT) = S2(1:L2)
      ELSE IF (L2 .EQ. 0) THEN
         buffer(1:LT) = S1(1:L1)
      ELSE
         buffer(1:L1) = S1(1:L1)
         buffer(L1+1:L1+L2) = S2(1:L2)
      ENDIF
      LT = LT + 1
      DO I = LT, LEN(buffer)
         buffer(I:I) = char(0)
      ENDDO
      fconcat = buffer
      RETURN
      END
!***********************************
      INTEGER FUNCTION fPosChar(Cha, Str)
!***********************************
!     Busca caracter dentro string
      CHARACTER*(*) Str
      CHARACTER*1 Cha
      EXTERNAL lcadena
      INTEGER fPosCharVal
      INTEGER lcadena
      INTEGER ICar
      INTEGER NCar
      NCar = lcadena(Str)
      fPosCharVal = 0
      ICar = 0
      DO WHILE (                                                        &
     &     (fPosCharVal .EQ. 0) .AND.                                   &
     &     (ICar .LT. NCar))
         ICar = ICar + 1
         IF (Str(ICar:ICar) .EQ. Cha) THEN
            fPosCharVal = ICar
         ENDIF
      ENDDO
      fPosChar = fPosCharVal
      RETURN
      END
!*****************************************
      SUBROUTINE DifTmp(TFin, TIni, Hor, MIN, Seg)
!*****************************************
!     Determina dif. tiempo en segundos
!     No vale para mas de un dia en general
      INTEGER i

      INTEGER DimTmp
      PARAMETER (DimTmp = 7)

      INTEGER TFin(DimTmp)
      INTEGER TIni(DimTmp)
      INTEGER STFin(DimTmp)
      INTEGER STIni(DimTmp)
      INTEGER Hor, MIN, Seg
      INTEGER NFin, NIni
      INTEGER NFecha
      DO i = 1, DimTmp
         STIni(i) = TIni(i)
         STFin(i) = TFin(i)
      ENDDO
      NIni = NFecha(STIni(1), STIni(2), STIni(3))
      NFin = NFecha(STFin(1), STFin(2), STFin(3))
      TFin(3) = NFin - NIni
      IF (TFin(7) .LT. TIni(7)) THEN
         TFin(7) = TFin(7) + 10
         TFin(6) = TFin(6) - 1
      ENDIF
      IF (TFin(6) .LT. TIni(6)) THEN
         TFin(6) = TFin(6) + 60
         TFin(5) = TFin(5) - 1
      ENDIF
      IF (TFin(5) .LT. TIni(5)) THEN
         TFin(5) = TFin(5) + 60
         TFin(4) = TFin(4) - 1
      ENDIF
      IF (TFin(4) .LT. TIni(4)) THEN
         TFin(4) = TFin(4) + 24
         TFin(3) = TFin(3) - 1
      ENDIF
      DO i = 4, DimTmp
         TFin(i) = TFin(i) - TIni(i)
      ENDDO
      IF (TFin(7) .GT. 50) THEN
         TFin(6) = TFin(6) + 1
      ENDIF
      Hor = TFin(3)*24 + TFin(4)
      MIN= TFin(5)
      Seg = TFin(6)
      RETURN
      END
!****************
!     calcula la fecha
!****************
      INTEGER FUNCTION NFecha(Dia, Mes, Ano)
      INTEGER NDiasAcuBis, NDiasAcuAno, NDiasAcuMes
      INTEGER AnoBis
      INTEGER NDiasMes
      INTEGER DiasMes(12)
      INTEGER DIasAcuMes(13)
      INTEGER Ano
      INTEGER Mes
      INTEGER Dia
      INTEGER NDias
      DATA DiasMes/                                                     &
     &     031, 028, 031, 030, 031, 030,                                &
     &     031, 031, 030, 031, 030, 031/
      DATA DiasAcuMes/000, 031, 059, 090, 120, 151,                     &
     &     181, 212, 243, 273, 304, 334, 365/
      IF((Ano .GT. 2099) .OR. (Ano .LT. 1900) .OR.                      &
     &     (Mes .GT. 12) .OR. (Mes .LT. 1)) THEN
         NDias = 0
      ELSE
         NDias = 0
         NDiasAcuAno = 365*(Ano - 1900)
         NDiasAcuBis = INT ((Ano - 1900 + 3)/4)
         NDiasAcuAno = NDiasAcuAno + NDiasAcuBis
         IF (Mod(Ano - 1900 + 4, 4) .NE. 0) THEN
            AnoBis = 0
         ELSE
            AnoBis = 1
         ENDIF
         NDiasMes = DiasMes(Mes)
         IF (Mes .EQ. 2) THEN
            NDiasMes = NDiasMes + AnoBis
         ENDIF
         IF (Dia .GT. NDiasMes) THEN
            NDias = 0
         ELSE
            NDiasAcuMes = DiasAcuMes(Mes)
            IF (Mes .GT. 2) THEN
               NDiasAcuMes = NDiasAcuMes + AnoBis
            ENDIF
            NDias = NDiasAcuAno + NDiasAcuMes + Dia
         ENDIF
      ENDIF
      NFecha = NDias
      RETURN
      END
!*******************************************************
!     vomita lo que hay en el buffer de la unidad a la unidad
!*******************************************************
      SUBROUTINE Vomit(IUnit)
      IMPLICIT NONE
      INTEGER IUnit
      CALL flush(IUnit)
      RETURN
      END
!*************************************************************
!     Obtiene el nombre del archivo que va a grabar la rutina salva
!*************************************************************
      SUBROUTINE name4salva(filename, iapert, ietapa, isimul, pdnumite, &
     &     pmnumite, ext, FPLP, iciclo)
      INCLUDE 'pxp.fpp'
!
      CHARACTER*80       fconcat
!
      CHARACTER*(*) ext
      CHARACTER*80 filename
      CHARACTER*(DimLargo) ciapert
      CHARACTER*(DimLargo) cietapa
      CHARACTER*(DimLargo) cisimul
      CHARACTER*(DimLargo) cpdnumite
      CHARACTER*(DimLargo) cpmnumite
      CHARACTER*(DimLargo) ciciclo
      INTEGER iapert
      INTEGER ietapa
      INTEGER isimul
      INTEGER pdnumite
      INTEGER pmnumite
      INTEGER iciclo
      LOGICAL FPLP
!
      ciapert = ' '
      cietapa = ' '
      cisimul = ' '
      cpdnumite = ' '
      cpmnumite = ' '
      ciciclo = ' '
 
      CALL Num2Char(pdnumite, cpdnumite, Si, 2)
      CALL Num2Char(ietapa, cietapa, Si, 3)
      CALL Num2Char(isimul, cisimul, Si, 2)
      IF (FPLP) THEN
         CALL Num2Char(iapert, ciapert, Si, 2)
         filename = fconcat(cpdnumite, '-')
         filename = fconcat(filename, ext)
         filename = fconcat(filename, '-')
         filename = fconcat(filename, cietapa)
         filename = fconcat(filename, '-')
         filename = fconcat(filename, cisimul)
         filename = fconcat(filename, '-')
         filename = fconcat(filename, ciapert)
      ELSE
         CALL Num2Char(pmnumite, cpmnumite, Si, 2)
         filename = fconcat(cpmnumite, '-')
         filename = fconcat(filename, cpdnumite)
         filename = fconcat(filename, '-')
         filename = fconcat(filename, ext)
         filename = fconcat(filename, '-')
         filename = fconcat(filename, cietapa)
         filename = fconcat(filename, '-')
         filename = fconcat(filename, cisimul)
      ENDIF

      IF (iciclo .GT. 0) THEN
         CALL Num2Char(iciclo, ciciclo, Si, 4)
         filename = fconcat(filename, '_')
         filename = fconcat(filename, ciciclo)
      ENDIF
         

!$$$      IF (FPLP) THEN
!$$$         CALL Num2Char(iapert, ciapert, Si, 2)
!$$$         filename = ext(1:2)
!$$$         filename = fconcat(filename, cietapa)
!$$$         filename = fconcat(filename, cisimul)
!$$$         filename = fconcat(filename, ciapert)
!$$$      ELSE
!$$$         CALL Num2Char(pmnumite, cpmnumite, Si, 2)
!$$$         filename = cpmnumite
!$$$         filename = fconcat(filename, cpdnumite)
!$$$         filename = fconcat(filename, ext(1:2))
!$$$         filename = fconcat(filename, cietapa)
!$$$      ENDIF
      RETURN
      END
      SUBROUTINE ReporteProgreso2(                                      &
     &     MensajeProgreso, NWriteCharN, IDeltaProgreso, NProgreso,     &
     &     InicioReporte, FinReporte)
      IMPLICIT NONE
      INTEGER LargoBarra
      PARAMETER (LargoBarra = 39)
      CHARACTER*(*) MensajeProgreso
      INTEGER I
      INTEGER IDeltaProgreso
      INTEGER IIProgreso1
      INTEGER IIProgreso2
      INTEGER IIProgreso3
      INTEGER IProgreso
      INTEGER NWriteCharN
      INTEGER NProgreso
      INTEGER NNProgreso
      LOGICAL InicioReporte
      LOGICAL FinReporte
      SAVE IIProgreso1
      SAVE IProgreso
      SAVE NNProgreso
      IF (InicioReporte) THEN
         WRITE(6,                                                       &
     &        '(A, ''  0% ['', $)')                                     &
     &        MensajeProgreso
         DO I = 1, LargoBarra
            WRITE(6, '('' '', $)')
         ENDDO
         WRITE(6, '('']'', $)')
         CALL WriteCharN(NWriteCharN, CHAR(8))
         IIProgreso1 = 0
         IProgreso = 0
         NNProgreso = NProgreso
      ELSEIF (FinReporte) THEN
         CALL WriteCharN(LargoBarra + 7, CHAR(8))
         WRITE(6,                                                       &
     &        '(''100% ['', $)')
         DO I = 1, LargoBarra
            WRITE(6, '(''#'', $)')
         ENDDO
         WRITE(6, '('']'', $)')
         CALL WriteCharN(NWriteCharN - 3, CHAR(8))
      ELSE
         IProgreso = IProgreso + IDeltaProgreso
         IIProgreso2 =                                                  &
     &        INT(ANINT(100.0*REAL(IProgreso)/REAL(NNProgreso)))
         IF (IIProgreso2 .GT. IIProgreso1) THEN
            CALL WriteCharN(LargoBarra + 7, CHAR(8))
            WRITE(6, '(I3, ''% ['', $)')                                &
     &           IIProgreso2
            IIProgreso3 =                                               &
     &           INT(ANINT(REAL(LargoBarra)*REAL(IProgreso)/            &
     &           REAL(NNProgreso)))
            DO I = 1, IIProgreso3
               WRITE(6, '(''#'', $)')
            ENDDO
            DO I = IIProgreso3 + 1, LargoBarra
               WRITE(6, '('' '', $)')
            ENDDO
            WRITE(6, '('']'', $)')
            CALL WriteCharN(NWriteCharN, CHAR(8))
            IIProgreso1 = IIProgreso2
         ENDIF
      ENDIF
      CALL Vomit(6)
      RETURN
      END
