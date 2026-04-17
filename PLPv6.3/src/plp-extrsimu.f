!*********************************
!     extrae la simulacion*ISimul*
!*********************************
      SUBROUTINE ExtrSimu (SimulInd, ISimul, NEtapa, NBloque, BloInd,   &
     &     NCenPas, NSimul, Kit,                                        &
     &     NCols, NFilas, PDNCol, PDNFila,                              &
     &     EtaDual, EtaPrimal,                                          &
     &     PasQAfl, PasQAflSim, Dim, ULog)
      USE PLP, ONLY : PAR_DIMS, ISDFA,  ISPFA

      TYPE(PAR_DIMS), INTENT(IN) :: Dim

!
      INTEGER ULog
      INTEGER ICenPas
      INTEGER IClase
      INTEGER IEtapa
      INTEGER IBind
      INTEGER IBlo
      INTEGER SimulInd(Dim%Simul, Dim%Eta)
      INTEGER ISimul
      INTEGER NEtapa
      INTEGER NBloque(Dim%Eta)
      INTEGER BloInd(Dim%IBlo, Dim%Eta)
      INTEGER NCenPas
      INTEGER NSimul
      INTEGER IPtrD
      INTEGER IPtrP
      INTEGER USimDual
      INTEGER USimPrimal
      INTEGER Kit(Dim%Simul, Dim%Kit)
      
      INTEGER NCols, NFilas 
      INTEGER PDNCol(Dim%Eta)
      INTEGER PDNFila(Dim%Eta)
      DOUBLE PRECISION EtaDual(NFilas)
      DOUBLE PRECISION EtaPrimal(NCols)

      DOUBLE PRECISION PasQAfl(Dim%Pas, Dim%Blo, Dim%Clase)
      DOUBLE PRECISION PasQAflSim(Dim%Pas, Dim%Blo)

      INTEGER IEtaCOff
      INTEGER IEtaFOff
      INTEGER NCol
      INTEGER NFila
!
      USimDual = Kit(ISimul, ISDFA)
      USimPrimal = Kit(ISimul, ISPFA)

      IEtaCOff = 1
      IEtaFOff = 1

      DO IEtapa = 1, NEtapa
         DO ICenPas = 1, NCenPas
            IClase = SimulInd(ISimul, IEtapa)
            DO IBlo = 1, NBloque(IEtapa)
               IBind = BloInd(IBlo, IEtapa)
               PasQAflSim(ICenPas, IBind) =                             &
     &              PasQAfl(ICenPas, IBind, IClase)
            ENDDO
         ENDDO
         IPtrD = NSimul*SUM(PDNFila(1:IEtapa-1))                        &
     &        + (ISimul - 1)*PDNFila(IEtapa) + 1
         IPtrP = NSimul*SUM(PDNCol(1:IEtapa-1))                         &
     &        + (ISimul - 1)*PDNCol(IEtapa) + 1  
         

         NCol = PDNCol(IEtapa)
         NFila = PDNFila(IEtapa)
         CALL AAD2RAMd(USimPrimal, IPtrP, EtaPrimal(IEtaCOff), NCol, ULog)
         CALL AAD2RAMd(USimDual, IPtrD, EtaDual(IEtaFOff), NFila, ULog)

         IEtaCOff = IEtaCOff + NCol
         IEtaFOff = IEtaFOff + NFila
      ENDDO
      RETURN
      END
