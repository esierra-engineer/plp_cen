!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDEmb(NArcNom, ISimul, & 
     &     NBloque, BloEta, TipoEtapa,       &
     &     NCentral, NCenEmb, CenNom, CenTipo,                          &
     &     CenGBar, CenPGen, EmbFEsc, EmbVIni, EmbDat, SerDat, FPhi,    &
     &     EstocRHSP, SimulInd, NSimul,                                 &
     &     RenCen, AIncid, FactTiempo, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog


!
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION EmbVIni(Dim%Emb)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION FactRendim
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION SerDat(Dim%Ser, DimDatSer, Dim%Blo)
      INTEGER Abrir
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER CenGBar(Dim%Cen)
      INTEGER I
      INTEGER IEta
      INTEGER IBlo
      INTEGER ICen
      INTEGER ICenJ
      INTEGER ISimul
      INTEGER NBloque
      INTEGER BloEta(Dim%Blo)
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER UWrite
      INTEGER fPosChar
      LOGICAL SeSuma
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER NSimul
      INTEGER IClaseFila, IS

      DOUBLE PRECISION VIni(NBloque)
      DOUBLE PRECISION VFin(NBloque)
      DOUBLE PRECISION PSom(NBloque)
      DOUBLE PRECISION PSom2(NBloque)
      DOUBLE PRECISION QAflu(NBloque)

 100  FORMAT((A6,",",I4,",",A,",",I4,",",A,",",E9.2,2(",",F10.5),8(",",F10.2)))
      
!
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(4A)') 'Hidro,Bloque,TipoEtapa,',               &
     &        'EmbNum,EmbNom,EmbFac,EmbVini,EmbVfin,',                  &
     &        'EmbQgen,EmbQver,EmbQdef,',                               &
     &        'EmbPsom,EmbPsom2,EmbAflu,EmbQFil,EmbQReb'   
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      DO ICen = 1, NCentral
         IF(fPosChar(CenTipo(ICen), STipoEmb) .GT. 0) THEN
            DO IBlo = 1, NBloque
               IEta = BloEta(IBlo)
               ! vini y vfin
               IF((IEta .EQ. 1)) THEN
                  VIni(IBlo) = EmbVIni(ICen)*1D3/EmbFEsc(ICen)
               ELSE
                  VIni(IBlo) =                                          &
     &                 EmbDat(ICen, PEmbDatVol, IEta - 1)*              &
     &                 1D3/EmbFEsc(ICen)
               ENDIF
               VFin(IBlo) = EmbDat(ICen, PEmbDatVol, IEta)*             &
     &              1D3/EmbFEsc(ICen)
               
               ! precio sombra
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
                  PSom(IBlo) =                                          &
     &                 EmbDat(ICen, PEmbDatCMg, IEta)*                  &
     &                 FPhi(IEta)*FactTiempo/FactRendim
               ELSE
                  PSom(IBlo) = 0.0
               ENDIF
               PSom2(IBlo) =                                            &
     &              EmbDat(ICen, PEmbDatCMg, IEta)*                     &
     &              FPhi(IEta)
               
               ! caudal afluente
               IF (ISimul .EQ. 0) THEN                  
                  QAflu(IBlo) = 0.0d0
                  DO IS = 1, NSimul
                     IClaseFila = SimulInd(IS, IEta)
                     QAflu(IBlo) = QAflu(IBlo) + EstocRHSP(ICen, IBlo, IClaseFila)
                  ENDDO
                  QAflu(IBlo) = QAflu(IBlo) / NSimul                     
               ELSE
                  IClaseFila = SimulInd(ISimul, IEta)
                  QAflu(IBlo) = EstocRHSP(ICen, IBlo, IClaseFila)
               ENDIF
            ENDDO
            
            IF (ISimul .EQ. 0) THEN
               NomSimul = 'MEDIA'
            ELSE
               WRITE(NomSimul,'("Sim", I3)') ISimul
            ENDIF
               
            WRITE(UWrite, 100) &
     &           (NomSimul, &
     &           IBlo, &
     &           TipoEtapa(BloEta(IBlo)), &
     &           ICen, &
     &           CenNom(ICen), &
     &           EmbFEsc(ICen), &
     &           VIni(IBlo), &
     &           VFin(IBlo), &
     &           CenPGen(ICen, IBlo), &
     &           EmbDat(ICen, PEmbDatVer, IBlo), &
     &           EmbDat(ICen, PEmbDatDef, IBlo), &
     &           PSom(IBlo), &
     &           PSom2(IBlo), &
     &           QAflu(IBlo), &
     &           EmbDat(ICen, PEmbDatFil, BloEta(IBlo)), &
     &           EmbDat(ICen, PEmbDatReb, BloEta(IBlo)), &
     &           IBlo = 1, NBloque)
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

!!!
!******************************************
!     Graba Archivo Datos Centrales Embalse
!******************************************
      SUBROUTINE GraDatBDEmbE(NArcNom, ISimul,                          & 
     &     NBloques, BloEta, TipoEtapa,                                 &
     &     NCentral, CenNom, CenTipo,                                   &
     &     EmbFEsc, EmbDat, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!
!
      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      INTEGER Abrir
      INTEGER IBlo
      INTEGER ICen
      INTEGER ISimul
      INTEGER NBloques
      INTEGER BloEta(Dim%Blo)
      INTEGER NCentral
      INTEGER UWrite
      INTEGER fPosChar
      
 100  FORMAT((A5,",",I4,",",A,",",I4,","A,",",E9.2,2(",",F7.2),2(",",F8.5)))
      
!
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(4A)') 'Hidro,Bloque,TipoEtapa,',                &
     &        'EmbNum,EmbNom,EmbFac,',                                  &
     &        'EmbQFilt,EmbQReb,',                                      &
     &        'EmbVRebP,EmbVRebN'
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      DO ICen = 1, NCentral
         IF (fPosChar(CenTipo(ICen), STipoEmb) .GT. 0) THEN
               IF (ISimul .EQ. 0) THEN
                  NomSimul = 'MEDIA'
               ELSE
                  WRITE(NomSimul,'("Sim", I3)') ISimul
               ENDIF

            WRITE(UWrite, 100) &
     &           (NomSimul, &
     &           IBlo, &
     &           TipoEtapa(BloEta(IBlo)), &
     &           ICen, &
     &           CenNom(ICen), &
     &           EmbFEsc(ICen), &
     &           EmbDat(ICen, PEmbDatFil, BloEta(IBlo)), &
     &           EmbDat(ICen, PEmbDatReb, BloEta(IBlo)), &
     &           EmbDat(ICen, PEmbDatRebP, BloEta(IBlo))*1D3/EmbFEsc(ICen), &
     &           EmbDat(ICen, PEmbDatRebN, BloEta(IBlo))*1D3/EmbFEsc(ICen), &
     &           IBlo = 1, NBloques)
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END

