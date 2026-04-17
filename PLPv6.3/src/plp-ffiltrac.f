!***********************************************************************
!     funcion que calcula las filtraciones de acuerdo al volumen del emb
!***********************************************************************
      DOUBLE PRECISION FUNCTION FFiltraciones(NTramo, Bordes,           &
     &     Pendientes, Constantes, Vol)

      INTEGER NTramo
      DOUBLE PRECISION Bordes(NTramo)
      DOUBLE PRECISION Constantes(NTramo)
      DOUBLE PRECISION Pendientes(NTramo)
      DOUBLE PRECISION Vol
      DOUBLE PRECISION FFiltracionesi
!
      INTEGER IFTramo

      FFiltraciones = FFiltracionesi(NTramo, Bordes,           &
     &     Pendientes, Constantes, Vol, IFTramo)
      RETURN
      END


      DOUBLE PRECISION FUNCTION FFiltracionesi(NTramo, Bordes,           &
     &     Pendientes, Constantes, Vol, IFTramo)

      INTEGER IFiltTramo
      INTEGER NTramo
      INTEGER IFTramo
      DOUBLE PRECISION Bordes(NTramo)
      DOUBLE PRECISION Constantes(NTramo)
      DOUBLE PRECISION Pendientes(NTramo)
      DOUBLE PRECISION ffilt
      DOUBLE PRECISION Vol
!
      IFTramo = 0
      ffilt = 0d0
      DO IFiltTramo = 1, NTramo - 1
         IF ((Bordes(IFiltTramo) .le. Vol) &
     &        .and. (Vol .lt. Bordes(IFiltTramo+1) )) THEN         
            ffilt = Constantes(IFiltTramo) +                               &
     &           Pendientes(IFiltTramo)*Vol 
            IFtramo = IFiltTramo
            EXIT
         ENDIF
      ENDDO
      FFiltracionesi = ffilt
      RETURN
      END


