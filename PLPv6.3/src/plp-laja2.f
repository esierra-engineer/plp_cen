!     Actualiza las economias de ENDESA y de los nuevos regantes de acue
!     al convenio del Laja.
      SUBROUTINE Laja2(IEtapa, NEtapa, NBloque, BloInd, BloDur,         &
     &     ScaleVol, CauDemNuReEta,                                     &
     &     Cau2Vol, LajaIPar, LajaCPar, LajaLPar, LajaRPar,             &
     &     Mes, MesBal,                                                 &
     &     FFasePrimal, ISimul, SimulInd, EstocRHSP, SimPrimal, PDNCol, &
     &     VolFilt, lp, Dim)
      USE PLP

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     Archivo comun a todas las rutinas.
!     Convenio del Laja.
!
      DOUBLE PRECISION pp

      INTEGER(C_SIZE_T), INTENT(IN) :: lp

!
      DOUBLE PRECISION, INTENT(IN):: Cau2Vol(Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: EstocRHSP(Dim%EstocFila, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION, INTENT(OUT):: LajaRPar(DimRLaja, Dim%Simul, 0:Dim%Eta + 1)
      TYPE(PAR_LAJAC), INTENT(IN):: LajaCPar
      INTEGER, INTENT(IN):: PDNCol
      DOUBLE PRECISION, INTENT(IN):: SimPrimal(PDNCol)
      DOUBLE PRECISION, INTENT(IN):: VolFilt
      INTEGER, INTENT(IN):: IEtapa
      INTEGER, INTENT(IN):: NBloque(Dim%Eta)
      INTEGER, INTENT(IN):: BloInd(Dim%IBlo, Dim%Eta)
      DOUBLE PRECISION, INTENT(IN):: BloDur(Dim%Blo)
      INTEGER, INTENT(IN):: ISimul
      INTEGER, INTENT(IN):: LajaIPar(DimILaja)
      INTEGER, INTENT(IN):: Mes(Dim%Eta)
      INTEGER, INTENT(IN):: NEtapa
      INTEGER, INTENT(IN):: SimulInd(Dim%Simul, Dim%Eta)
      LOGICAL, INTENT(IN):: FFasePrimal
      LOGICAL, INTENT(IN):: LajaLPar(DimLLaja)
      LOGICAL, INTENT(IN):: MesBal(Dim%Eta)
!     Locales
      DOUBLE PRECISION CauDemNuReEta(Dim%Eta)
      DOUBLE PRECISION CauExtr
      DOUBLE PRECISION CauVert
      DOUBLE PRECISION EcoEND
      DOUBLE PRECISION EcoENDGlo
      DOUBLE PRECISION EcoENDLoc
      DOUBLE PRECISION EcoNuRe
      DOUBLE PRECISION VolAflLaja
      DOUBLE PRECISION VolDelta
      DOUBLE PRECISION VolExtr
      DOUBLE PRECISION VolExtrAnuEND
      DOUBLE PRECISION VolExtrMasVert
      DOUBLE PRECISION VolExtrMenEND
      DOUBLE PRECISION VolExtrMenNuRe
      DOUBLE PRECISION VolRealMax
      DOUBLE PRECISION VoluLaja
      DOUBLE PRECISION VolVert
      DOUBLE PRECISION VolVertEND
      DOUBLE PRECISION VolVertNuRe
      INTEGER IClase(Dim%EstocFila)
      INTEGER IComp
      INTEGER UWrite1
      LOGICAL Imprime

      INTEGER IBlo
      DOUBLE PRECISION DSum
      DOUBLE PRECISION QAfluEta
      DOUBLE PRECISION ScaleVol(Dim%Emb)

      DOUBLE PRECISION GetUppBnd
!
      IF (.NOT. LajaLPar(IUsoConvLaja)) THEN
         RETURN
      ENDIF
!     IComp = LajaIPar(IIVolLaja)
      IComp = LajaCPar%IEtaInd(IIVolLaja, IEtapa)
      VolRealMax = GetUppBnd(IComp, lp)
      UWrite1 = LajaIPar(IILajaWrite1)
      IF (FFasePrimal .AND.                                             &
     &     (ISimul .EQ. LajaIPar(IIndSimImpLaja))) THEN
!$$$      IF (FFasePrimal .AND.
!$$$     $     (SimulInd(ISimul, IEtapa) .EQ.
!$$$     $     LajaIPar(IIndSimImpLaja))) THEN
         Imprime = Si
      ELSE
         Imprime = No
      ENDIF
!     Extraccion de datos de LajaRPar.
      EcoENDGlo = LajaRPar(IEcoENDGlo, ISimul, IEtapa)
      EcoENDLoc = LajaRPar(IEcoENDLoc, ISimul, IEtapa)
      EcoNuRe = LajaRPar(IEcoNuRe, ISimul, IEtapa)
      VolExtrAnuEND = LajaRPar(IVolExtrAnuEND, ISimul, IEtapa)
      VolExtrMenEND = LajaRPar(IVolExtrMenEND, ISimul, IEtapa)
      VolExtrMenNuRe = LajaRPar(IVolExtrMenNuRe, ISimul, IEtapa)
!
      IClase = SimulInd(ISimul, IEtapa)
!      VolAflLaja = EstocRHSP(LajaIPar(IIAflLaja), IClase, IEtapa)
      VolAflLaja = QAfluEta(IEtapa, NBloque, BloInd,                    &
     &     BloDur, EstocRHSP, IClase,                                   & 
     &     LajaIPar(IIAflLaja), Dim) * Cau2Vol(IEtapa)
!     CauExtr = SimPrimal(LajaIPar(IIGenElToro))
      CauExtr = 0.0d0
      CauVert = 0.0d0
      DSum = 0.0d0
      DO IBlo = 1, NBloque(IEtapa)
         DSum = DSum + BloDur(BloInd(IBlo, IEtapa)) 
         CauExtr = CauExtr + BloDur(BloInd(IBlo, IEtapa))               &
     &        * SimPrimal(LajaCPar%IBloInd(IIGenElToro, IBlo, IEtapa))
         CauVert = CauVert + BloDur(BloInd(IBlo, IEtapa))                 &
     &        * SimPrimal(LajaCPar%IBloInd(IIVertLaja, IBlo, IEtapa))
      ENDDO
      CauExtr = CauExtr / DSum
      CauVert = CauVert / DSum

!     r: VolMax < VoluLaja + VolAflLaja - VolFilt
      VolExtr = Cau2Vol(IEtapa)*CauExtr
!     VolExtr < VoluLaja + VolAflLaja - VolFilt
!     0 < VolExtr
!     r: 0 < VolExtr
!     VolExtr > 0
!     r: VolExtr < VoluLaja + VolAflLaja - VolFilt
      IComp = LajaCPar%IEtaInd(IIVolLaja, IEtapa)
      VoluLaja = SimPrimal(IComp) * ScaleVol(LajaIPar(IICenElToro))
!     0 < VoluLaja
!      VolVert = Cau2Vol(IEtapa)*SimPrimal(LajaIPar(IIVertLaja)) -   &
!     &     VolFilt
      VolVert = Cau2Vol(IEtapa)*CauVert - VolFilt

!     VoluLaja < VolRealMax
      EcoEND = EcoENDGlo + EcoENDLoc
      IF (EcoEND + EcoNuRe .GT. 0.0d0) THEN
         VolVertEND = EcoEND*VolVert/(EcoEND + EcoNuRe)
         VolVertNuRe = VolVert - VolVertEND
      ELSE
         VolVertEND = 0.0d0
         VolVertNuRe = 0.0d0
      ENDIF
      VolDelta = MIN(VolExtrMenEND, VolExtrAnuEND, VolExtr)
      VolExtrMenEND = VolExtrMenEND - VolDelta
      IF (VolRealMax - VoluLaja .LT. LajaCPar%Vol50cm) THEN
         VolExtrAnuEND = VolExtrAnuEND - MIN(LajaCPar%Gasto50cm*Cau2Vol(IEtapa), &
     &        VolDelta)
      ELSE
         VolExtrAnuEND = VolExtrAnuEND - VolDelta
      ENDIF
!     0 < VolExtrAnuEND
      IF ( (                                                            &
     &     (VolExtrAnuEND .LE. 0d0) .OR.                                &
     &     (VolExtrMenEND .LE. 0d0)                                     &
     &     ) .AND. (                                                    &
     &     (VolDelta .LE. 0d0)                                          &
     &     )) THEN
         VolExtr = VolExtr + VolFilt - VolDelta
      ELSE
         VolExtr = VolExtr - VolDelta
      ENDIF
      VolExtrMasVert = VolExtr + VolVertEND
      VolDelta = MIN(EcoENDLoc, VolExtrMasVert)
      EcoENDLoc = EcoENDLoc - VolDelta
      VolExtrMasVert = VolExtrMasVert - VolDelta
      VolDelta = MIN(EcoENDGlo, VolExtrMasVert)
      EcoENDGlo = EcoENDGlo - VolDelta
      EcoENDGlo = MIN(EcoENDGlo, pp(VoluLaja - EcoENDLoc))
      EcoENDLoc = MIN(EcoENDLoc, VoluLaja)
      EcoNuRe = EcoNuRe - VolVertNuRe
      IF (Imprime) THEN
         WRITE(UWrite1, '(I4, '','', $)') IEtapa
         WRITE(UWrite1, '(I3, '','', $)') Mes(IEtapa)
         WRITE(UWrite1, '(F6.1, '','', $)') CauExtr
         WRITE(UWrite1, '(F6.1, '','', $)') Cau2Vol(IEtapa)
         WRITE(UWrite1, '(F6.1, '','', $)') VolFilt/Cau2Vol(IEtapa)
         WRITE(UWrite1, '(F6.1, '','', $)') CauExtr*Cau2Vol(IEtapa)/1d3
         WRITE(UWrite1, '(F6.1, '','', $)') VolFilt/1d3
         WRITE(UWrite1, '(F6.1, '','', $)') VolAflLaja/1d3
         WRITE(UWrite1, '(F6.1, '','', $)') VolExtrAnuEND/1d3
         WRITE(UWrite1, '(F6.1, '','', $)') VolExtrMenEND/1d3
         WRITE(UWrite1, '(F6.1, '','', $)') VolVert/1d3
         WRITE(UWrite1, '(F6.1, '','', $)') VoluLaja/1d3
         WRITE(UWrite1, '(F6.1, '','', $)')                             &
     &        LajaRPar(ICauExtrMin, ISimul, IEtapa)
         WRITE(UWrite1, '(F6.1, '','', $)')                             &
     &        LajaRPar(ICauExtrMax, ISimul, IEtapa)
         WRITE(UWrite1, '(F7.1, '','', $)') EcoNuRe/1d3
      ENDIF
      IF (IEtapa .LT. NEtapa) THEN
!     Se analiza la siguiente etapa. El chequeo se hace para no salirse
!     de los limites de los arreglos.
         IF (Mes(IEtapa) .NE. Mes(IEtapa + 1)) THEN
!     Cambio de mes.
            VolExtrMenEND = MAX(LajaCPar%VolDerMenEND,                           &
     &           LajaCPar%CauDerMenEND*Cau2Vol(IEtapa + 1))
            EcoNuRe = EcoNuRe + VolExtrMenNuRe
            VolExtrMenNuRe =                                            &
     &           MAX(3.6d0*24.0d0*30.0d0, Cau2Vol(IEtapa + 1))*         &
     &           CauDemNuReEta(IEtapa + 1)
            IF (VoluLaja .LT. LajaCPar%VolColSup) THEN
               VolExtrMenNuRe = VolExtrMenNuRe*LajaCPar%PCenLimSupCol
            ENDIF
         ENDIF
         IF (MesBal(IEtapa + 1)) THEN
!     Cambio de year. Llego la etapa del balance anual.
            EcoENDGlo = MIN(EcoENDGlo + VolExtrAnuEND,                  &
     &           VoluLaja - EcoENDLoc)
            VolExtrAnuEND = LajaCPar%VolDerAnuEND
         ENDIF
!
!     Insercion de datos de LajaRPar.
         LajaRPar(IEcoENDGlo, ISimul, IEtapa + 1) = EcoENDGlo
         LajaRPar(IEcoENDLoc, ISimul, IEtapa + 1) = EcoENDLoc
         LajaRPar(IEcoNuRe, ISimul, IEtapa + 1) = EcoNuRe
         LajaRPar(IVoluLaja, ISimul, IEtapa + 1) = VoluLaja
         LajaRPar(IVolExtrAnuEND, ISimul, IEtapa + 1) = VolExtrAnuEND
         LajaRPar(IVolExtrMenEND, ISimul, IEtapa + 1) = VolExtrMenEND
         LajaRPar(IVolExtrMenNuRe, ISimul, IEtapa + 1) = VolExtrMenNuRe
!
      ENDIF
      EcoEND = EcoENDGlo + EcoENDLoc
      IF (Imprime) THEN
         WRITE(UWrite1, '(4(F6.1, '',''), F6.1, $)')                    &
     &        LajaCPar%VolDerAnuEND/1d3, VolExtrAnuEND/1d3, EcoENDLoc/1d3,       &
     &        EcoENDGlo/1d3, EcoEND/1d3
      ENDIF
      RETURN
      END
