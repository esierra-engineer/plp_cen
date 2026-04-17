      SUBROUTINE GenMatX(PXNCol, PXNFila, NEtapa,                       &
     &     PlaCFNCol, PlaCFNFila)
!     comun a todas las rutinas:
!     parametros y variables:
      INTEGER NEtapa
      INTEGER PlaCFNCol
      INTEGER PlaCFNFila
      INTEGER PXNCol(NEtapa)
      INTEGER PXNFila(NEtapa)
!
!
!
      PXNCol(1:NEtapa) = 0
      PXNFila(1:NEtapa) = 0

      PXNCol(NEtapa) = PlaCFNCol
      PXNFila(NEtapa) = PlaCFNFila


      RETURN
      END


