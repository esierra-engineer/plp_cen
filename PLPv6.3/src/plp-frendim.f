!***********************************************************************
!     funcion que calcula los rendimientos de acuerdo al volumen del emb
!***********************************************************************
      DOUBLE PRECISION FUNCTION FRendimientos(NTramo, Bordes,           &
     &     Pendientes, Constantes, Vol)

      INTEGER IRendTramo
      INTEGER NTramo
      DOUBLE PRECISION Bordes(NTramo)
      DOUBLE PRECISION Constantes(NTramo)
      DOUBLE PRECISION Pendientes(NTramo)
      DOUBLE PRECISION ValFRendimientos
      DOUBLE PRECISION Vol
!
      IRendTramo = 0
      ValFRendimientos = 1000.d0
      DO IRendTramo = 1, NTramo
         ValFRendimientos = MIN(ValFRendimientos,                       &
     &        Constantes(IRendTramo) +                                  &
     &        Pendientes(IRendTramo)*(Vol - Bordes(IRendTramo)))
         IF ((ValFRendimientos .LT. 0d0)) ValFRendimientos = 0d0
      ENDDO
      FRendimientos = ValFRendimientos
      RETURN
      END
