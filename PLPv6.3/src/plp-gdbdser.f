!*************************************************
!     Graba Archivo Datos Generacion Series Diario
!*************************************************
      SUBROUTINE GraDatBDSer(NArcNom, ISimul, & 
     &     NBloques, BloEta, TipoEtapa,       &
     &     NCentral, NCenEmb, CenNom, CenTipo,                          &
     &     CenGBar, CenPGen, EmbDat, SerDat, NBarra, BarNom, FPhi,      &
     &     EstocRHSP, SimulInd, NSimul,                                 &
     &     RenCen, AIncid, FactTiempo, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!

      CHARACTER*8 STipoEmb
      CHARACTER*8 STipoSer
      CHARACTER*1 CenTipo(Dim%Cen)
      CHARACTER*48 BarNom(Dim%Bar)
      CHARACTER*48 CenNom(Dim%Cen)
      CHARACTER*24 NArcNom
      CHARACTER*48 BarNom2
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Eta)
      DOUBLE PRECISION FactRendim
      DOUBLE PRECISION FactTiempo
      DOUBLE PRECISION FPhi(Dim%Eta)
      DOUBLE PRECISION RenCen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION SerDat(Dim%Ser, DimDatSer, Dim%Blo)
      INTEGER Abrir
      INTEGER AIncid(Dim%HidSPP, 0:Dim%HidSPP)
      INTEGER CenGBar(Dim%Cen)
      INTEGER I
      INTEGER IBlo
      INTEGER IEta
      INTEGER ICen
      INTEGER ICenJ
      INTEGER ISimul
      INTEGER NBarra
      INTEGER NBloques
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

      DOUBLE PRECISION QAflu(NBloques)
      DOUBLE PRECISION PSom(NBloques)
      DOUBLE PRECISION PSom2(NBloques)
      
 100  FORMAT((A6,",",I4,","A,",",I4,",",A,",",I4,",",A,7(",",F10.2)))
      
!
      STipoEmb = PCenTipEmb//PCenTipEmbAux
      STipoEmb(3:3) = Char(0)
      STipoSer = PCenTipRie//PCenTipSer
      STipoSer(3:3) = Char(0)
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(5A)') 'Hidro,Bloque,TipoEtapa,',               &
     &        'SerNum,SerNom,SerBar,BarNom,',                           &
     &        'SerQGen,SerQVer,',                                       &
     &        'SerPSom,SerPSom2,',                                      &
     &        'SerPGen,SerAflu,SerRend'
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      DO ICen = 1, NCentral
         IF (fPosChar(CenTipo(ICen), STipoSer) .GT. 0) THEN
            DO IBlo = 1, NBloques
               IEta = BloEta(IBlo)
               
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
     &                 SerDat(ICen - NCenEmb, PSerDatCMg, IBlo)*        &
     &                 FPhi(IEta)*FactTiempo/FactRendim
               ELSE
                  PSom(IBlo) = 0.0
               ENDIF
               PSom2(IBlo) = SerDat(ICen - NCenEmb, PSerDatCMg, IBlo)*  &
     &              FPhi(IEta)
               
               ! caudal afluente
               IF (ISimul .EQ. 0) THEN                  
                  QAflu(IBlo) = 0.0d0
                  DO IS = 1, NSimul
                     IClaseFila = SimulInd(IS, IEta)
                     QAflu(IBlo) = QAflu(IBlo) +                        &
     &                       EstocRHSP(ICen - NCenEmb, IBlo, IClaseFila)
                  ENDDO
                  QAflu(IBlo) = QAflu(IBlo) / NSimul                     
               ELSE
                  IClaseFila = SimulInd(ISimul, IEta)
                  QAflu(IBlo) = EstocRHSP(ICen - NCenEmb, IBlo, IClaseFila)
               ENDIF
            ENDDO

            IF (ISimul .EQ. 0) THEN
               NomSimul = 'MEDIA'
            ELSE
               WRITE(NomSimul,'("Sim", I3)') ISimul
            ENDIF

            IF (NBarra .GT. 1) THEN
               IF (CenGBar(ICen) .EQ. 0) THEN
                  BarNom2 = 'BarraAux = 0  '
               ELSE
                  BarNom2 = BarNom(CenGBar(ICen))
               ENDIF
            ELSE
               BarNom2 = 'Uninodal'
            ENDIF
            
            WRITE(UWrite, 100) &
     &           (NomSimul, &
     &           IBlo, &
     &           TipoEtapa(BloEta(IBlo)), &
     &           ICen, &
     &           CenNom(ICen), &
     &           CenGBar(ICen), &
     &           BarNom2, &
     &           CenPGen(ICen, IBlo), &
     &           SerDat(ICen - NCenEmb, PSerDatVer, IBlo), &
     &           PSom(IBlo), &
     &           PSom2(IBlo), &
     &           CenPGen(ICen, IBlo)*RenCen(ICen, IBlo), &
     &           QAflu(IBlo), &     
     &           RenCen(ICen, IBlo), &
     &           IBlo = 1, NBloques)
         ENDIF
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END
