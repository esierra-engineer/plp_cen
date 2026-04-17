      SUBROUTINE GraDatBDEmb0(UPreSuf)

      INTEGER UPreSuf
      WRITE(UPreSuf, '(A)') '"reservoirFieldNames"'
      WRITE(UPreSuf, '(7A)')                                            &
     &     '"Vol.Inicial [Gm3]","Cot.Inicial [m]",',                    &
     &     '"Vol.Final [Gm3]","Cot.Final [m]",',                        &
     &     '"QGenerado [m3/s]",',                                       &
     &     '"QFiltrado [m3/s]","QExtraccion [m3/s]",',                  &
     &     '"QVertido [m3/s]","QRebalse [m3/s]",',                      &
     &     '"QAfluente [m3/s]","QDeficit [m3/s]",',                     &
     &     '"Valor Agua [US$/MWh]","VA [US$/dm3]"'
      WRITE(UPreSuf, '(A)') '"reservoirFormat"'
      WRITE(UPreSuf, '(7A)')                                            &
     &     '"%10.4f","%10.2f",',                                        &
     &     '"%10.4f","%10.2f",',                                        &
     &     '"%10.2f",',                                                 &
     &     '"%10.2f","%10.2f",',                                        &
     &     '"%10.2f","%10.2f",',                                        &
     &     '"%10.2f","%10.2f",',                                        &
     &     '"%10.2f","%10.2f"'
      WRITE(UPreSuf, '(A)') '"reservoirPrefixNames"'
      WRITE(UPreSuf, '(A)')                                             &
     &   '"Embalse","Simulacion","Bloque","Numero Embalse"'
      END

!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDEmb2(NBloques, BloEta, ISimul,                 &
     &     NCentral, NCenEmb, CenTipo,                          &
     &     CenGBar, CenPGen, EmbFEsc, EmbVIni, EmbDat, SerDat, FPhi,    &
     &     EstocRHSP, SimulInd, NSimul,                                 &
     &     ExtrNCen, ExtrCenInd, ExtrDat,                               &
     &     RenCen, AIncid, FactTiempo, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
!

      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*1 CenTipo(Dim%Cen)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION Factor
      DOUBLE PRECISION FactRendim
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION SerDat(Dim%Ser, DimDatSer, Dim%Blo)
      DOUBLE PRECISION VolIni
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER CenGBar(Dim%Cen)
      INTEGER I
      INTEGER IBlo
      INTEGER IEta
      INTEGER ICen
      INTEGER ICenJ
      INTEGER ISimul
      INTEGER NSimul
      INTEGER IClaseFila, IS, Idx
      INTEGER IREC
      INTEGER LRECL
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      INTEGER ExtrNCen
      INTEGER ICentral
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER UWrite
      INTEGER fPosChar
      INTEGER ExtrCenInd(Dim%Extr)
      LOGICAL SeSuma
      CHARACTER*4 InvRD
      DATA IREC /0/
      DOUBLE PRECISION D0
      DOUBLE PRECISION D1
      DOUBLE PRECISION D2
      DOUBLE PRECISION D3
      DOUBLE PRECISION D4
      DOUBLE PRECISION D5
      DOUBLE PRECISION D6
      DOUBLE PRECISION D7
      DOUBLE PRECISION D8
      DOUBLE PRECISION D9
      DOUBLE PRECISION D10
      DOUBLE PRECISION QAflu
      DOUBLE PRECISION QExt
      DOUBLE PRECISION ExtrDat(Dim%Extr, Dim%Blo)
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)

      INTEGER AbrirDirecto
      EXTERNAL AbrirDirecto
!
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      LRECL = 44
      UWrite = AbrirDirecto('plpemb.res', 'UNKNOWN', LRECL, 'NATIVE', ULog)
      DO IBlo = 1, NBloques
         IEta = BloEta(IBlo)
         DO ICen = 1, NCentral
            IF(fPosChar(CenTipo(ICen), STipoEmb) .GT. 0) THEN
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
               IF((IEta .EQ. 1)) THEN
                  VolIni = EmbVIni(ICen)
               ELSE
                  VolIni = EmbDat(ICen, PEmbDatVol, IEta - 1)
               ENDIF
               IF (ISimul .EQ. 0) THEN                  
                  QAflu = 0.0d0
                  DO IS = 1, NSimul
                     IClaseFila = SimulInd(IS, IEta)
                     QAflu = QAflu + EstocRHSP(ICen, IBlo, IClaseFila)
                  ENDDO
                  QAflu = QAflu / NSimul                     
               ELSE
                  IClaseFila = SimulInd(ISimul, IEta)
                  QAflu = EstocRHSP(ICen, IBlo, IClaseFila)
               ENDIF    
               QExt = 0.0d0
               DO Idx = 1, ExtrNCen
                  ICentral = ExtrCenInd(Idx)
                  IF (ICen .EQ. ICentral) THEN
                     QExt = ExtrDat(Idx, IBlo) + QExt
                  END IF
               ENDDO 
               IREC = IREC + 1
               D0 = VolIni*1D3/EmbFEsc(ICen)
               D1 = EmbDat(ICen, PEmbDatVol, IEta)*1D3/EmbFEsc(ICen)
               D2 = CenPGen(ICen, IBlo)
               D3 = EmbDat(ICen, PEmbDatFil, IEta)
               D4 = QExt
               D5 = EmbDat(ICen, PEmbDatVer, IBlo)
               D6 = EmbDat(ICen, PEmbDatReb, IEta)
               D7 = QAflu
               D8 = EmbDat(ICen, PEmbDatDef, IBlo)
               D9 = EmbDat(ICen, PEmbDatCMg, IEta)*FPhi(IEta)*Factor
               D10 = EmbDat(ICen, PEmbDatCMg, IEta)*FPhi(IEta)
               WRITE(UWrite, REC = IREC)                                &
     &              InvRD(D0),                                          &
     &              InvRD(D1),                                          &
     &              InvRD(D2),                                          &
     &              InvRD(D3),                                          &
     &              InvRD(D4),                                          &
     &              InvRD(D5),                                          &
     &              InvRD(D6),                                          &
     &              InvRD(D7),                                          &
     &              InvRD(D8),                                          &
     &              InvRD(D9),                                          &
     &              InvRD(D10)
            ENDIF
         ENDDO
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END
