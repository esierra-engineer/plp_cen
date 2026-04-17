!     Actualiza las economias de ENDESA y de los nuevos regantes de acue
!     al convenio del Maule.
      SUBROUTINE Maule2(IEtapa, NEtapa, NBloque, BloInd, BloDur,        &
     &     Cau2Vol, MauleIPar, MauleCPar, MauleLPar, MauleRPar, Mes,    &
     &     FFasePrimal, ISimul, SimulInd, EstocRHSP, SimPrimal, PDNCol, &
     &     VolFiltInv, VolDefRie, Dim)
!     Archivo comun a todas las rutinas.
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN)::  Dim


      DOUBLE PRECISION pp
!
      INTEGER PDNCol
      DOUBLE PRECISION Cau2Vol(Dim%Eta)
      DOUBLE PRECISION CauAflFictInve
      DOUBLE PRECISION CauGenCip
      DOUBLE PRECISION CauExtMauEND
      DOUBLE PRECISION CauExtMauRie
      DOUBLE PRECISION DeltaEcoNeg
      DOUBLE PRECISION DeltaEcoPos
      DOUBLE PRECISION EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION MauleRPar(DimRMaule, Dim%Simul, 0:Dim%Eta)
      DOUBLE PRECISION SimPrimal(PDNCol)
      DOUBLE PRECISION VolAflFictInve
      DOUBLE PRECISION VolAflInv
      DOUBLE PRECISION VolAflMau
      DOUBLE PRECISION VolAflMau20PC
      DOUBLE PRECISION VolAflMau80PC
      DOUBLE PRECISION VolCompEND
      DOUBLE PRECISION VolCompENDAnt
      DOUBLE PRECISION VolDefRie
      DOUBLE PRECISION VolDisResOrdEND
      DOUBLE PRECISION VolDisResOrdRie
      DOUBLE PRECISION VolEcoInv
      DOUBLE PRECISION VolEcoInvAnt
      DOUBLE PRECISION VolGenCip
      DOUBLE PRECISION VolExtInv
      DOUBLE PRECISION VolExtMau
      DOUBLE PRECISION VolExtMauEND
      DOUBLE PRECISION VolExtMauRie
      DOUBLE PRECISION VolFiltInv
      DOUBLE PRECISION VolInv
      DOUBLE PRECISION VolMau
      DOUBLE PRECISION VolExtMaxMauEND
      DOUBLE PRECISION VolExtMaxMauRie
      DOUBLE PRECISION VolResCuoExt
      DOUBLE PRECISION VolResMauEND
      DOUBLE PRECISION VolResMauRie
      DOUBLE PRECISION VolVertInv
      DOUBLE PRECISION VolVertMau
      INTEGER IClase(Dim%EstocFila)
      INTEGER IEtapa
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION BloDur(Dim%Blo)
      INTEGER ISimul
      INTEGER MauleIPar(DimIMaule)
      TYPE(PAR_MAULEC) MauleCPar
      INTEGER Mes(Dim%Eta)
      INTEGER NEtapa
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER UWrite1
      INTEGER UWrite2
      INTEGER UWrite3
      INTEGER UWrite4
      LOGICAL FFasePrimal
      LOGICAL FPasoPorResOrd
      LOGICAL FPrevisionDeshielo
      LOGICAL FVieneDePorSup
      LOGICAL FVieneDeResOrd
      LOGICAL MauleLPar(DimLMaule, Dim%Simul, 0:Dim%Eta)

      DOUBLE PRECISION CauVertMaule
      DOUBLE PRECISION CauVertCip 
      DOUBLE PRECISION DSum
      DOUBLE PRECISION QAfluEta
      INTEGER IBlo

      IF (MauleIPar(IIUsoConvMaule) .EQ. 0) RETURN
!     Se considera que nunca hay una prevision de deshielo
!     que supere los 500Hm3 o que la direccion de aguas no
!     permite a ENDESA la cuota extraordinaria de 50Hm3.
      UWrite1 = MauleIPar(IIMauleWrite1)
      UWrite2 = MauleIPar(IIMauleWrite2)
      UWrite3 = MauleIPar(IIMauleWrite3)
      UWrite4 = MauleIPar(IIMauleWrite4)
      FPrevisionDeshielo = .FALSE.
!
!     Extraccion de datos de MauleIPar.
      FPasoPorResOrd = MauleLPar(IFPasoPorResOrd, ISimul, IEtapa)
      FVieneDePorSup = MauleLPar(IFVieneDePorSup, ISimul, IEtapa)
      FVieneDeResOrd = MauleLPar(IFVieneDeResOrd, ISimul, IEtapa)
!     Extraccion de datos de MauleRPar.
      VolCompEND = MauleRPar(IVolCompEND, ISimul, IEtapa)
      VolDisResOrdEND = MauleRPar(IVolDisResOrdEND, ISimul, IEtapa)
      VolDisResOrdRie = MauleRPar(IVolDisResOrdRie, ISimul, IEtapa)
      VolEcoInv = MauleRPar(IVolEcoInv, ISimul, IEtapa)
      VolInv = MauleRPar(IVolInv, ISimul, IEtapa)
      VolMau = MauleRPar(IVolMau, ISimul, IEtapa)
      VolResCuoExt = MauleRPar(IVolResCuoExt, ISimul, IEtapa)
      VolResMauEND = MauleRPar(IVolResMauEND, ISimul, IEtapa)
      VolResMauRie = MauleRPar(IVolResMauRie, ISimul, IEtapa)
      VolExtMaxMauEND = MauleRPar(IVolExtMaxMauEND, ISimul, IEtapa)
      VolExtMaxMauRie = MauleRPar(IVolExtMaxMauRie, ISimul, IEtapa)
!
      IClase = SimulInd(ISimul, IEtapa)
!     VolAflMau = EstocRHSP(MauleIPar(IIAflMaule), IEtapa, IClase)
      VolAflMau = QAfluEta(IEtapa, NBloque, BloInd,                 &
     &     BloDur, EstocRHSP, IClase,                               & 
     &     MauleIPar(IIAflMaule), Dim) * Cau2Vol(IEtapa)

!     VolAflInv = EstocRHSP(MauleIPar(IIAflInve), IEtapa, IClase)
      VolAflInv = QAfluEta(IEtapa, NBloque, BloInd,                  &
     &     BloDur, EstocRHSP, IClase,                                & 
     &     MauleIPar(IIAflInve), Dim) * Cau2Vol(IEtapa)

      DSum = 0.0d0
      CauAflFictInve = 0.0d0
      CauExtMauRie = 0.0d0
      CauExtMauEND = 0.d0
      CauGenCip = 0.0d0
      CauVertMaule = 0.0d0
      CauVertCip = 0.0d0
      DO IBlo = 1, NBloque(IEtapa)
         DSum = DSum + BloDur(BloInd(IBlo, IEtapa)) 
         CauAflFictInve = CauAflFictInve + BloDur(BloInd(IBlo, IEtapa)) &
     &        * SimPrimal(MauleCPar%IBloInd(IIAflFictInve, IBlo, IEtapa))
         CauExtMauRie = CauExtMauRie + BloDur(BloInd(IBlo, IEtapa))     &
     &        * SimPrimal(MauleCPar%IBloInd(IIExtMauRie, IBlo, IEtapa))
         CauExtMauEND = CauExtMauEND + BloDur(BloInd(IBlo, IEtapa))     &
     &        * SimPrimal(MauleCPar%IBloInd(IIExtMauEND, IBlo, IEtapa))
         CauGenCip = CauGenCip +   BloDur(BloInd(IBlo, IEtapa))         &
     &        * SimPrimal(MauleCPar%IBloInd(IIGenCip, IBlo, IEtapa))
         CauVertMaule = CauVertMaule + BloDur(BloInd(IBlo, IEtapa))     &
     &        * SimPrimal(MauleCPar%IBloInd(IIVertMaule, IBlo, IEtapa))
         CauVertCIp = CauVertCip + BloDur(BloInd(IBlo, IEtapa))         &
     &        * SimPrimal(MauleCPar%IBloInd(IIVertCip, IBlo, IEtapa))
         
      ENDDO
      CauAflFictInve = CauAflFictInve / DSum
      CauExtMauRie = CauExtMauRie / DSum
      CauExtMauEND = CauExtMauEND / DSum
      CauGenCip = CauGenCip / DSum
      CauVertMaule = CauVertMaule / DSum
      CauVertCip = CauVertCip / DSum

      VolAflFictInve = Cau2Vol(IEtapa)*CauAflFictInve
      VolExtMau = Cau2Vol(IEtapa)*(CauExtMauEND + CauExtMauRie)
      VolGenCip = Cau2Vol(IEtapa)*CauGenCip
!
!     Volumenes de extraccion.
      VolExtMauRie = Cau2Vol(IEtapa)*CauExtMauRie
      VolExtMauEND = Cau2Vol(IEtapa)*CauExtMauEND
!     Vertimientos.
      VolVertMau = Cau2Vol(IEtapa)*CauVertMaule
      VolVertInv = Cau2Vol(IEtapa)*CauVertCip
      VolExtInv = VolGenCip + VolFiltInv + VolVertInv
      IF (FFasePrimal .AND.                                             &
     &     (ISimul .EQ.                                                 &
     &     MauleIPar(IIndSimImpMaule))) THEN
!$$$     $     (SimulInd(ISimul, IEtapa) .EQ.
!
!     Impresion de datos y resultados
         WRITE(UWrite1,                                                 &
     &        '(I3, '','', I4, 21('','', F6.1), 4('',    '', L1))')     &
     &        IEtapa, Mes(IEtapa),                                      &
     &        VolInv/1D3, VolGenCip/1D3, VolFiltInv/1D3, VolVertInv/1D3,&
     &        VolAflInv/1D3,                                            &
     &        VolMau/1D3, VolExtMau/1D3, VolVertMau/1D3, VolAflMau/1D3, &
     &        VolEcoInv/1D3, VolCompEND/1D3, VolResMauEND/1D3,          &
     &        VolExtMauEND/1D3 , VolResMauRie/1D3, VolExtMauRie/1D3,    &
     &        VolDisResOrdEND/1D3, VolDisResOrdRie/1D3,                 &
     &        VolDefRie/1D3,                                            &
     &        VolExtMaxMauEND/1D3, VolExtMaxMauRie/1D3,                 &
     &        VolResCuoExt/1D3,                                         &
     &        FVieneDePorSup, FVieneDeResOrd,                           &
     &        FPasoPorResOrd, FPrevisionDeshielo
         WRITE(UWrite2, '(I3, '','', I4, 13('','', F6.1))')             &
     &        IEtapa, Mes(IEtapa),                                      &
     &        VolInv/1D3, VolGenCip/1D3, VolFiltInv/1D3, VolVertInv/1D3,&
     &        VolAflInv/1D3,                                            &
     &        VolMau/1D3, VolExtMau/1D3, VolVertMau/1D3, VolAflMau/1D3, &
     &        VolGenCip/Cau2Vol(IEtapa), VolVertInv/Cau2Vol(IEtapa),    &
     &        VolExtMau/Cau2Vol(IEtapa), VolVertMau/Cau2Vol(IEtapa)
      ENDIF
!
!     Se comienza la actualizacion de variables
!     Antes de actualizar la etapa, hay que recordar el estado de
!     la laguna del Maule.
      IF (VolMau .GE. MauleCPar%VolResOrdMax + MauleCPar%VolResExtMax) THEN
!     Esta en la porcion superior.
         FVieneDeResOrd = No
         FVieneDePorSup = Si
      ELSE IF ((MauleCPar%VolResOrdMax + MauleCPar%VolResExtMax .GE. VolMau) .AND.          &
     &        (VolMau .GE. MauleCPar%VolResExtMax)) THEN
!     Esta en la reserva ordinaria.
         FVieneDeResOrd = Si
         FVieneDePorSup = No
      ELSE
!     Esta en la reserva extraordinaria.
         FVieneDeResOrd = No
         FVieneDePorSup = No
      ENDIF
!     Actualizacion del volumen de las lagunas.
      VolMau = pp(VolMau - (VolExtMau + VolVertMau) + VolAflMau)
      VolInv = pp(VolInv - VolExtInv + VolAflInv)
!     Actualizacion de las economias.
!     Se completo riego deficitario con desembalse de la Invernada.
      DeltaEcoPos = pp(MIN(VolExtMaxMauRie - VolExtMauRie,              &
     &     VolGenCip + VolFiltInv - VolAflInv - VolAflFictInve))
!     Lo primero que se consume ante una extraccion de ENDESA o
!     un vertimiento son las economias de la Invernada.
      DeltaEcoNeg = MIN(VolEcoInv, VolExtMauEND + VolVertMau)
      VolEcoInvAnt = VolEcoInv
      VolEcoInv = pp(VolEcoInvAnt + DeltaEcoPos - DeltaEcoNeg)
!     Volumenes de compensacion y residuales.
      VolResMauRie = pp(VolResMauRie - VolExtMauRie)
      IF (FVieneDePorSup) THEN
         VolCompENDAnt = VolCompEND
         VolCompEND = pp(VolCompENDAnt -                                &
     &        pp(VolExtMauEND - VolEcoInvAnt))
         VolResMauEND = pp(VolResMauEND -                               &
     &        pp(VolExtMauEND - VolEcoInvAnt - VolCompENDAnt))
      ELSE IF (FVieneDeResOrd) THEN
         VolResMauEND = pp(VolResMauEND -                               &
     &        pp(VolExtMauEND - VolEcoInvAnt))
      ENDIF
!     Cuotas de la reserva ordinaria.
      IF ((VolMau .LE. MauleCPar%VolResOrdMax + MauleCPar%VolResExtMax) .AND.               &
     &     (VolMau .GE. MauleCPar%VolResExtMax)) THEN
!     Esta en la reserva ordinaria.
         VolAflMau20PC = 0.2D0*VolAflMau
         VolAflMau80PC = 0.8D0*VolAflMau
         IF (FVieneDePorSup) THEN
            IF (.NOT. FPasoPorResOrd) THEN
!     Se ha pasado de la porcion superior a la reserva ordinaria, y
!     hay que salvar el volumen residual de la reserva ordinaria.
               VolDisResOrdEND = 0.2D0*pp(VolMau - MauleCPar%VolResExtMax)
               VolDisResOrdRie = 0.8D0*pp(VolMau - MauleCPar%VolResExtMax)
            ELSE
               VolDisResOrdEND = pp(VolDisResOrdEND                     &
     &              - pp(VolExtMauEND - VolAflMau20PC))
               VolDisResOrdRie = pp(VolDisResOrdRie                     &
     &              - pp(VolExtMauRie - VolAflMau80PC))
            ENDIF
            FPasoPorResOrd = Si
         ELSE IF (FVieneDeResOrd) THEN
            VolDisResOrdEND = pp(VolDisResOrdEND                        &
     &           - pp(VolExtMauEND - VolAflMau20PC))
            VolDisResOrdRie = pp(VolDisResOrdRie                        &
     &           - pp(VolExtMauRie - VolAflMau80PC))
         ELSE
!     Viene de la reserva extraordinaria
            VolDisResOrdEND = 0.0d0
            VolDisResOrdRie = 0.0d0
            FPasoPorResOrd = Si
         ENDIF
      ENDIF
!     Balances.
      IF (IEtapa .LT. NEtapa) THEN
!     Se analiza la siguiente etapa. El chequeo se hace para no salirse
!     de los limites de los arreglos.
         IF (Mes(IEtapa) .NE. Mes(IEtapa + 1)) THEN
!     Cambio de mes.
            IF ((Mes(IEtapa) .LT. Enero) .AND.                          &
     &           (Mes(IEtapa + 1) .GE. Enero)) THEN
               VolCompEND = MIN(VolCompEND + VolResMauEND,              &
     &              MauleCPar%VolCompENDMax)
               VolResMauEND = MauleCPar%VolMaxEND
               FPasoPorResOrd = No
               IF ((VolMau .LE. MauleCPar%VolResOrdMax + MauleCPar%VolResExtMax) .AND.      &
     &              (VolMau .GE. MauleCPar%VolResExtMax)) THEN
!     Esta en la reserva ordinaria.
                  VolDisResOrdEND = 0.2D0*pp(VolMau - MauleCPar%VolResExtMax)
               ENDIF
            ELSE IF ((Mes(IEtapa) .LT. Junio) .AND.                     &
     &              (Mes(IEtapa + 1) .GE. Junio)) THEN
!     La temporada de riego termina en mayo, como se deduce a partir
!     de los factores de modulacion del riego en el convenio.
               VolResCuoExt = MauleCPar%VolCuoExtMau
               VolResMauRie = MauleCPar%VolMaxRie
            ENDIF
         ENDIF
!
!     Insercion de datos de MauleLPar.
         MauleLPar(IFPasoPorResOrd, ISimul, IEtapa + 1) = FPasoPorResOrd
         MauleLPar(IFVieneDePorSup, ISimul, IEtapa + 1) = FVieneDePorSup
         MauleLPar(IFVieneDeResOrd, ISimul, IEtapa + 1) = FVieneDeResOrd
!     Insercion de datos de MauleRPar.
         MauleRPar(IVolCompEND, ISimul, IEtapa + 1) = VolCompEND
         MauleRPar(IVolDisResOrdEND, ISimul, IEtapa + 1) =              &
     &        VolDisResOrdEND
         MauleRPar(IVolDisResOrdRie, ISimul, IEtapa + 1) =              &
     &        VolDisResOrdRie
         MauleRPar(IVolEcoInv, ISimul, IEtapa + 1) = VolEcoInv
         MauleRPar(IVolInv, ISimul, IEtapa + 1) = VolInv
         MauleRPar(IVolMau, ISimul, IEtapa + 1) = VolMau
         MauleRPar(IVolResCuoExt, ISimul, IEtapa + 1) = VolResCuoExt
         MauleRPar(IVolResMauEND, ISimul, IEtapa + 1) = VolResMauEND
         MauleRPar(IVolResMauRie, ISimul, IEtapa + 1) = VolResMauRie
!
      ENDIF
      RETURN
      END
