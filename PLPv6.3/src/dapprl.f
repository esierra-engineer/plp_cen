      LOGICAL FUNCTION DxAEQy(x, y, ErrAbs)
      IMPLICIT NONE
      DOUBLE PRECISION x
      DOUBLE PRECISION y
      DOUBLE PRECISION ErrAbs
      DxAEQy = (DABS(x - y) .LE. ErrAbs)
      RETURN
      END
      LOGICAL FUNCTION DxALTy(x, y, ErrAbs)
      IMPLICIT NONE
      DOUBLE PRECISION x
      DOUBLE PRECISION y
      DOUBLE PRECISION ErrAbs
      DxALTy = (x - y .LT. -ErrAbs)
      RETURN
      END
      LOGICAL FUNCTION DxAGTy(x, y, ErrAbs)
      IMPLICIT NONE
      DOUBLE PRECISION x
      DOUBLE PRECISION y
      DOUBLE PRECISION ErrAbs
      DxAGTy = (x - y .GT. ErrAbs)
      RETURN
      END
