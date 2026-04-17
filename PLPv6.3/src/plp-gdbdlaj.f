!**********************
!c     Graba Archivo Laja
!**********************
      SUBROUTINE GraDatBDLaja(NArcNom, ISimul,                          &
     &     NEtapa, TipoEtapa, Mes, Cau2Vol, LajaLPar, LajaRPar,         &
     &     Dim, ULog)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
      INTEGER ULog
!
!     variables globales
!***********************
      CHARACTER*24 NArcNom
      CHARACTER*12 TipoEtapa(Dim%Eta)
      CHARACTER*8 NomSimul
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      INTEGER ISimul
      INTEGER Mes(Dim%Eta)
      INTEGER NEtapa
      LOGICAL LajaLPar(DimLLaja)
!     variables locales
!**********************
      INTEGER Abrir
      INTEGER IEta
      INTEGER UWrite

      DOUBLE PRECISION riezaco
      DOUBLE PRECISION rieccce
      DOUBLE PRECISION rieladi
      DOUBLE PRECISION rieopla
      DOUBLE PRECISION rietotal
      DOUBLE PRECISION cauextmin
      DOUBLE PRECISION cauriedefa
      DOUBLE PRECISION cauriedeft
      DOUBLE PRECISION cauaflcapap
      DOUBLE PRECISION apol
      DOUBLE PRECISION filt
      DOUBLE PRECISION abhi
      DOUBLE PRECISION antp
      DOUBLE PRECISION tuhi
      DOUBLE PRECISION q1ab
      DOUBLE PRECISION q1tu
      DOUBLE PRECISION q2tu

      CHARACTER*55 FMT
      FMT = '(2A,I4,A,2A,I4,A,F10.2,A,17(F7.1,A))'
!
      IF (.NOT. LajaLPar(IUsoConvLaja)) THEN
         RETURN
      ENDIF
      IF (ISimul .EQ. 1) THEN
         UWrite = Abrir(NArcNom, 'UNKNOWN', 'SEQUENTIAL', ULog)
         WRITE(UWrite, '(5A)') 'Hidro,Etapa,TipoEtapa,Mes,Cau2Vol,',    &
     &      'RieZaCo,RieCCCE,RieLaDi,RieOpciLaja,RieTotal',             &
     &      'CauExtrMin,CauRieDefAban,CauRieDefTuca,CauAflCaptAltoPolc',&
     &      'APOL_CAP,FILT_GEN,ABAN_HI,ANTU_PAS,TUCA_HI,',              &
     &      'Q1RABAN,Q1RTUCA,Q2RTUCA'
      ELSE
         UWrite = Abrir(NArcNom, 'OLD', 'APPEND', ULog)
      END IF
      DO IEta = 1, NEtapa
         IF (ISimul .EQ. 0) THEN
            NomSimul = 'MEDIA'
         ELSE
            WRITE(NomSimul,'("Sim", I3)') ISimul            
         ENDIF

         riezaco = LajaRPar(IRieZaCo, ISimul, IEta)
         rieccce = LajaRPar(IRieCCCE, ISimul, IEta)
         rieladi = LajaRPar(IRieLaDi, ISimul, IEta)
         rieopla = LajaRPar(IRieOpcionalLaja, ISimul, IEta)
         rietotal =                                                     &
     &        LajaRPar(IRieZaCo, ISimul, IEta) +                        &
     &        LajaRPar(IRieCCCE, ISimul, IEta) +                        &
     &        LajaRPar(IRieLaDi, ISimul, IEta) +                        &
     &        LajaRPar(IRieOpcionalLaja, ISimul, IEta)
         cauextmin = LajaRPar(ICauExtrMin, ISimul, IEta)
         cauriedefa = LajaRPar(ICauRieDefAban, ISimul, IEta)
         cauriedeft = LajaRPar(ICauRieDefTuca, ISimul, IEta)
         cauaflcapap = LajaRPar(ICauAflCaptAltoPolc, ISimul, IEta)
         apol = LAJARPAR(IAPOL_CAP, ISIMUL, IEta)
         filt = LAJARPAR(IFILT_GEN, ISIMUL, IEta)
         abhi = LAJARPAR(IABAN_HI, ISIMUL, IEta)
         antp = LAJARPAR(IANTU_PAS, ISIMUL, IEta)
         tuhi = LAJARPAR(ITUCA_HI, ISIMUL, IEta)
         q1ab = LAJARPAR(IQ1RABAN, ISIMUL, IEta)
         q1tu = LAJARPAR(IQ1RTUCA, ISIMUL, IEta)
         q2tu = LAJARPAR(IQ2RTUCA, ISIMUL, IEta)

         WRITE(UWrite, FMT) NomSimul,',', IEta,',',                     &
     &        TipoEtapa(IEta),',',Mes(IEta),',',Cau2Vol(IEta),',',      &
     &        riezaco,',',rieccce,',',rieladi,',',rieopla,',',          &
     &        rietotal,',',cauextmin,',',cauriedefa,',',cauriedeft,',', &
     &        cauaflcapap,',',apol,',',filt,',',abhi,',',antp,',',      &
     &        tuhi,',',q1ab,',',q1tu,',',q2tu
      ENDDO
      CALL Cerrar(UWrite)
      RETURN
      END
