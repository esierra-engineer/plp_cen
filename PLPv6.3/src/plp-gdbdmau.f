!**********************
!c     Graba Archivo Maul
!**********************
      SUBROUTINE GraDatBDMaul(NArcNom, ISimul,                          &
     &     NEtapa, TipoEtapa, Mes, Cau2Vol,                             &
     &     CauConMauEta, CauRes105Eta,                                  &
     &     CenPGen, EmbFEsc, EmbDat,                                    &
     &     MauleIPar, MauleRPar, Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog

!     variables globales
!***********************
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauConMauEta(Dim%Eta)
      DOUBLE PRECISION CauRes105Eta(Dim%Eta)
      DOUBLE PRECISION CenPGen(Dim%Cen, Dim%Blo)
      DOUBLE PRECISION EmbDat(Dim%Emb, DimDatEmb, Dim%Blo)
      DOUBLE PRECISION EmbFEsc(Dim%Emb)
      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      INTEGER ISimul
      INTEGER MauleIPar(DimIMaule)
      INTEGER Mes(Dim%Eta)
      INTEGER NEtapa
!     variables locales
!**********************
      INTEGER Abrir
      INTEGER IEta
      INTEGER ICen
      INTEGER UWrite

      DOUBLE PRECISION riecmel
      DOUBLE PRECISION riecmna
      DOUBLE PRECISION riecmnb
      DOUBLE PRECISION ries123
      DOUBLE PRECISION rieoreg
      DOUBLE PRECISION rieomau
      DOUBLE PRECISION cauconv
      DOUBLE PRECISION caureso
      DOUBLE PRECISION rietotal
      DOUBLE PRECISION volaflarme
      DOUBLE PRECISION volaflinve
      DOUBLE PRECISION volfiltinv
      DOUBLE PRECISION volresmaur
      DOUBLE PRECISION qgenclbn
      DOUBLE PRECISION qgeninve
      DOUBLE PRECISION qgenmaul
      DOUBLE PRECISION qverclbn
      DOUBLE PRECISION qverinve
      DOUBLE PRECISION qvermaul
      DOUBLE PRECISION qficclbn
      DOUBLE PRECISION qficinve
      DOUBLE PRECISION qficmaul
      DOUBLE PRECISION volclbn
      DOUBLE PRECISION volinve
      DOUBLE PRECISION volmaul
      
      
      CHARACTER*55 FMT
      FMT = '(2A,I4,A,2A,I4,A,F10.2,A,25(F7.2,A))'
!
      IF (MauleIPar(IIUsoConvMaule) .EQ. 0) RETURN
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(8A)') 'Hidro,Etapa,TipoEtapa,Mes,Cau2Vol,',    &
     &        'RieCMel,RieCMNA,RieCMNB,RieS123,RieOReg,RieOpci,',       &
     &        'ConvMaule,Res105,RieTotal,',                             &
     &        'VolAflArme,VolAflInv,VolFiltInv,VolResMauRie,',          &
     &        'ExtColbun,ExtInve,ExtMaule,',                            &
     &        'VertColbun,VertInve,VertMaule,',                         &
     &        'AffFictColbun,AffFictInve,AffFictMaule,',                &
     &        'VolColbun,VolInve,VolMaule'
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      DO IEta = 1, NEtapa
         IF (ISimul .EQ. 0) THEN
            NomSimul = 'MEDIA'
         ELSE
            WRITE(NomSimul,'("Sim", I3)') ISimul
         ENDIF

         riecmel = MauleRPar(IRieCMel, ISimul, IEta)
         riecmnb = MauleRPar(IRieCMNA, ISimul, IEta)
         riecmna = MauleRPar(IRieCMNB, ISimul, IEta)
         ries123 = MauleRPar(IRieS123, ISimul, IEta)
         rieoreg = MauleRPar(IRieOReg, ISimul, IEta)
         rieomau = MauleRPar(IRieOpcionalMaule, ISimul, IEta)
         cauconv = CauConMauEta(IEta)
         caureso = CauRes105Eta(IEta)
         rietotal =                                                     &
     &        MauleRPar(IRieCMel, ISimul, IEta) +                       &
     &        MauleRPar(IRieCMNA, ISimul, IEta) +                       &
     &        MauleRPar(IRieCMNB, ISimul, IEta) +                       &
     &        MauleRPar(IRieS123, ISimul, IEta) +                       &
     &        MauleRPar(IRieOReg, ISimul, IEta) +                       &
     &        MauleRPar(IRieOpcionalMaule, ISimul, IEta)
         volaflarme = MauleRPar(IVolAflArme, ISimul, IEta)/1.0D3
         volaflinve = MauleRPar(IVolAflInv, ISimul, IEta)/1.0D3
         volfiltinv = MauleRPar(IVolFiltInv, ISimul, IEta)/1.0D3
         volresmaur = MauleRPar(IVolResMauRie, ISimul, IEta)/1.0D3
         ICen = MauleIPar(IICenColbun)
         qgenclbn = CenPGen(ICen, IEta)
         ICen = MauleIPar(IICenInve)
         qgeninve = CenPGen(ICen, IEta)
         ICen = MauleIPar(IICenMaule)
         qgenmaul = CenPGen(ICen, IEta)
         ICen = MauleIPar(IICenColbun)
         qverclbn = EmbDat(ICen, PEmbDatVer, IEta)
         ICen = MauleIPar(IICenInve)
         qverinve = EmbDat(ICen, PEmbDatVer, IEta)
         ICen = MauleIPar(IICenMaule)
         qvermaul = EmbDat(ICen, PEmbDatVer, IEta)
         ICen = MauleIPar(IICenColbun)
         qficclbn = EmbDat(ICen, PEmbDatDef, IEta)
         ICen = MauleIPar(IICenInve)
         qficinve = EmbDat(ICen, PEmbDatDef, IEta)
         ICen = MauleIPar(IICenMaule)
         qficmaul = EmbDat(ICen, PEmbDatDef, IEta)
         ICen = MauleIPar(IICenColbun)
         volclbn = EmbDat(ICen, PEmbDatVol, IEta)*1D3/EmbFEsc(ICen)
         ICen = MauleIPar(IICenInve)
         volinve = EmbDat(ICen, PEmbDatVol, IEta)*1D3/EmbFEsc(ICen)
         ICen = MauleIPar(IICenMaule)
         volmaul = EmbDat(ICen, PEmbDatVol, IEta)*1D3/EmbFEsc(ICen)

         WRITE(UWrite, FMT) NomSimul,',',IEta,',',TipoEtapa(IEta),',',  &
     &   Mes(IEta),',',Cau2Vol(IEta),',',                               &
     &   riecmel,',',riecmna,',',riecmnb,',',ries123,',',               &
     &   rieoreg,',',rieomau,',',cauconv,',',caureso,',',rietotal,',',  &
     &   volaflarme,',',volaflinve,',',volfiltinv,',',volresmaur,',',   &
     &   qgenclbn,',',qgeninve,',',qgenmaul,',',                        &
     &   qverclbn,',',qverinve,',',qvermaul,',',                        &
     &   qficclbn,',',qficinve,',',qficmaul,',',                        &
     &   volclbn,',',volinve,',',volmaul
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END
