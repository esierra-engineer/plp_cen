      SUBROUTINE GraDatBDSer0(UPreSuf)
      INTEGER UPreSuf

      WRITE(UPreSuf, '(A)') '"seriesFieldNames"'
      WRITE(UPreSuf, '(2A)') '"QGenerado [m3/s]","PGenerado [MW]",',    &
     & '"QVertido [m3/s]","Valor Agua [US$/MWh]","VA [US$/dm3]"'
      WRITE(UPreSuf, '(A)') '"seriesFormat"'
      WRITE(UPreSuf, '(2A)') '"%10.2f","%10.2f",',                      &
     &     '"%10.2f","%10.2f","%10.2f"'     
      WRITE(UPreSuf, '(A)') '"seriesPrefixNames"'
      WRITE(UPreSuf, '(A)')                                             &
     &     '"Serie","Simulacion","Bloque","Numero Serie"'
      END

!*************************************************
!     Graba Archivo Datos Generacion Series Diario
!*************************************************
      SUBROUTINE GraDatBDSer2(NBloque, BloEta,                          &
     &     NCentral, NCenEmb, CenTipo,                                  &
     &     CenGBar, CenPGen, EmbDat, SerDat, FPhi,                      &
     &     RenCen, AIncid, FactTiempo, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*1 CenTipo(Dim%Cen)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION Factor
      DOUBLE PRECISION FactRendim
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION SerDat(Dim%Ser, DimDatSer, Dim%Blo)
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER CenGBar(Dim%Cen)
      INTEGER I
      INTEGER IEta
      INTEGER IBlo
      INTEGER ICen
      INTEGER ICenJ
      INTEGER IREC
      INTEGER LRECL
      INTEGER NBloque
      INTEGER BloEta(Dim%Blo)
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER UWrite
      INTEGER fPosChar
      LOGICAL SeSuma
      CHARACTER*4 InvRD
      DATA IREC /0/
      DOUBLE PRECISION D0
      DOUBLE PRECISION D1
      DOUBLE PRECISION D2
      DOUBLE PRECISION D3
      DOUBLE PRECISION D4
      INTEGER AbrirDirecto
      EXTERNAL AbrirDirecto
!
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      LRECL = 20
      UWrite = AbrirDirecto('plpser.res', 'UNKNOWN', LRECL, 'NATIVE', ULog)
      DO IBlo = 1, NBloque
         IEta = BloEta(IBlo)
         DO ICen = 1, NCentral
            IF (fPosChar(CenTipo(ICen), STipoSer) .GT. 0) THEN
               FactRendim = 0.0d0
               DO I = 1, AIncid(ICen, 0)
                  ICenJ = AIncid(ICen, I)
                  SeSuma = No
                  IF (fPosChar(CenTipo(ICenJ), STipoSer) .GT. 0) THEN
                     IF (SerDat(ICenJ - NCenEmb, PSerDatCMg, IBlo)      &
     &                    .GT. 0.001d0) THEN
                        SeSuma = Si
                     ENDIF
                  ENDIF
                  IF (fPosChar(CenTipo(ICenJ), STipoEmb) .GT. 0) THEN
                     IF (EmbDat(ICenJ, PEmbDatCMg, IEta)                &
     &                    .GT. 0.001d0) THEN
                        SeSuma = Si
                     ENDIF
                  ENDIF
                  IF (SeSuma) THEN
                     IF (CenGBar(ICenJ) .GT. 0) THEN
                        FactRendim = FactRendim + RenCen(ICenJ, IBlo)
                     ENDIF
                  ENDIF
               ENDDO
               IF (FactRendim .GT. 0.001d0) THEN
                  Factor = FactTiempo/FactRendim
               ELSE
                  Factor = 0.0d0
               ENDIF
               IREC = IREC + 1
               D0 = CenPGen(ICen, IBlo)
               D1 = CenPGen(ICen, IBlo)*RenCen(ICen, IBlo)
               D2 = SerDat(ICen - NCenEmb, PSerDatVer, IBlo)
               D3 = SerDat(ICen - NCenEmb, PSerDatCMg, IBlo)*           &
     &              FPhi(IEta)*Factor
               D4 = SerDat(ICen - NCenEmb, PSerDatCMg, IBlo)*           &
     &              FPhi(IEta)
               WRITE(UWrite, REC = IREC)                                &
     &              InvRD(D0),                                          &
     &              InvRD(D1),                                          &
     &              InvRD(D2),                                          &
     &              InvRD(D3),                                          &
     &              InvRD(D4)
            ENDIF
         ENDDO
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END
