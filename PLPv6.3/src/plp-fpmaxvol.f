!***********************************************************************
!     funcion que calcula pmax de acuerdo al volumen del emb
!***********************************************************************
      DOUBLE PRECISION FUNCTION FPmaxvol(NTramo, Bordes,           &
     &     Pendientes, Constantes, Vol)

      INTEGER IPmaxTramo
      INTEGER NTramo
      DOUBLE PRECISION Bordes(NTramo)
      DOUBLE PRECISION Constantes(NTramo)
      DOUBLE PRECISION Pendientes(NTramo)
      DOUBLE PRECISION ValFPmax
      DOUBLE PRECISION Vol
!
      IPmaxTramo = 0
      ValFPmax = 1000.d0
      DO IPmaxTramo = 1, NTramo - 1
         IF ((Bordes(IPmaxTramo) .le. Vol) &
     &        .and. (Vol .lt. Bordes(IPmaxTramo+1) )) THEN
            ValFPmax = Constantes(IPmaxTramo) + &
     &           Pendientes(IPmaxTramo) * Vol
            EXIT
         ENDIF
      ENDDO
      FPmaxvol = ValFPmax
      RETURN
      END
