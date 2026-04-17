      SUBROUTINE PDInic(NEtapa, NSimul, PDNCol, PDNFila, PXNFila,    &
     &     PXNCol, lp, Dim)
      USE PLP, ONLY : PAR_DIMS, C_SIZE_T
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN)::  Dim

!     constantes de la maquina
      INCLUDE 'machcons.fpp'
!
!
      INTEGER IEtapa
      INTEGER NEtapa
      INTEGER NSimul
      INTEGER(C_SIZE_T) lp(Dim%Simul, NEtapa)
      INTEGER NCol
      INTEGER NFilaFinal
      INTEGER NFilaInicial
      INTEGER PDNCol(NEtapa)
      INTEGER PDNFila(NEtapa)
      INTEGER PXNCol(NEtapa)
      INTEGER PXNFila(NEtapa)
      DOUBLE PRECISION bd

      INTEGER(C_SIZE_T) lpi
      INTEGER ISimul
      INTEGER I, NVarPhi


      DO ISimul = 1, NSimul
         DO IEtapa = 1, NEtapa
!     elimina todas las aproximaciones de la funcion varphi(.)
            lpi = lp(ISimul, IEtapa)
            NFilaInicial = PDNFila(IEtapa) + PXNFila(IEtapa) + 1
            NFilaFinal = IINFTY
            CALL BorResPD(lpi, NFilaInicial, NFilaFinal)
!     fija la variable*VarPhi*en 0.
            NCol = PDNCol(IEtapa) + PXNCol(IEtapa)
            bd = 0.0d0
            NVarPhi = osi_lp_getnumcols(lpi) - NCol
            DO I=1, NVarPhi
               CALL osi_lp_setcollower (lpi, NCol + I - 1, bd)
               CALL osi_lp_setcolupper (lpi, NCol + I - 1, bd)
            ENDDO
         ENDDO
      ENDDO
      RETURN
      END
