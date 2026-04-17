!**********************************
!>     Subrutina Inicializa Matrices P.D.
!**********************************
      SUBROUTINE PDMatIni(A, PDNCol, PDNFila, PXNFila, LD, FO, Sentido)
!
      USE A_MATRIX
      INTEGER PDNCol, PDNFila
      INTEGER PXNFila
      CHARACTER*1 Sentido(PDNFila)
      TYPE(AMatrix) A
      DOUBLE PRECISION LD(PDNFila)
      DOUBLE PRECISION FO(PDNCol)

      CALL Am_init(A, PDNCol, PDNFila + PXNFila)
      LD(1:PDNFila) = 0.0d0
      FO(1:PDNCol) = 0.0d0
      Sentido(1:PDNFila) = 'E'

      RETURN
      END
