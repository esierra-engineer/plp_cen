      SUBROUTINE PDBestSoln(ZSPF, ZSPFPromBest, NSimul, FZSPFBest)

      INTEGER NSimul
      DOUBLE PRECISION ZSPF(NSimul)
      DOUBLE PRECISION ZSPFPromBest
      DOUBLE PRECISION Promedio
      DOUBLE PRECISION ZSPFProm
      LOGICAL FZSPFBest
      ZSPFProm = Promedio (ZSPF, NSimul)
      IF ((ZSPFProm .LT. ZSPFPromBest) .OR. (.NOT. FZSPFBest)) THEN
         ZSPFPromBest = ZSPFProm
      ENDIF
      RETURN
      END
