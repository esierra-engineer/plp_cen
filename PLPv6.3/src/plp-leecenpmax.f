      SUBROUTINE LeeCenPMaxDim(ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCenPMax

      TYPE(PAR_DIMS) Dim

      INTEGER ULog

      CHARACTER*80 AuxVar
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      INTEGER PMaxNCen
!
      Dim%EmbPMax = 0
      Dim%PMaxParam = 0
      Dim%PMaxTramo = 0

      URead = Abrir(NArcCenPMax, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leecenpmax: No existe archivo ',           &
     &        NArcCenPMax, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PMaxNCen

      Dim%EmbPMax = PMaxNCen
      Dim%PMaxParam = 3
      Dim%PMaxTramo = 20 + 1

      CALL Cerrar(URead)

      RETURN
      END

!*******************************************
!     Subrutina Datos Centrales PMaximientos
!*******************************************
      SUBROUTINE LeeCenPMax(NCentral, NCenEmb, CenNom,                   &
     &     PMaxCenInd, PMaxEmbInd,                                       &
     &     PMaxNTramo, PMaxParam, PMaxNCen,                              &
     &     ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcCenPMax, &
     &      PPMaxVol, PPMaxPend, PPMaxConst
      USE OSI

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!     Variables Globales
!***********************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCentral
      INTEGER NCenEmb
      INTEGER PMaxCenInd(Dim%EmbPMax)
      INTEGER PMaxEmbInd(Dim%EmbPMax)
      INTEGER PMaxNTramo(Dim%EmbPMax)
      INTEGER ULog
      DOUBLE PRECISION PMaxParam(Dim%PMaxTramo, Dim%EmbPMax, Dim%PMaxParam)
      DOUBLE PRECISION GetInfty
!     Variables Locales
!**********************
      CHARACTER*48 NomCen
      CHARACTER*48 NomEmb
      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER IPmax
      INTEGER ITra
      INTEGER IPar
      INTEGER PMaxNCen
      INTEGER NumCen
      INTEGER NumEmb
      INTEGER URead

      CHARACTER*42 Objeto
      LOGICAL FStop


!****************************
!     Lee datos plpcenpmax.dat
!****************************
      PMaxNCen = 0
      URead = Abrir(NArcCenPMax, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leecenpmax: No existe archivo ',           &
     &        NArcCenPMax, '.'
         RETURN
      ENDIF
      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) PMaxNCen
      IF (PMaxNCen .GT. Dim%EmbPMax) THEN
         WRITE(6, '(2A, I3, A, I3, A)')                                 &
     &        'leecenpmax: Numero embalses ',              &
     &        ' = ', PMaxNCen, ' > ', Dim%EmbPMax, '.'
         WRITE(ULog, '(2A, I3, A, I3, A)')                              &
     &        'leecenpmax: Numero embalses ',              &
     &        ' = ', PMaxNCen, ' > ', Dim%EmbPMax, '.'
         STOP 1
      ENDIF
      DO IPmax = 1, PMaxNCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb

         Objeto = 'embalse'
         CALL NomCen2NumCen(NumCen, FStop, NomCen,                      &
     &        CenNom, NCentral, Objeto, ULog)
         IF (FStop) THEN
            STOP 1
         ENDIF
         PMaxCenInd(IPmax) = NumCen

         Objeto = 'central'
         CALL NomCen2NumCen(NumEmb, FStop, NomEmb,                      &
     &        CenNom, NCenEmb, Objeto, ULog)

         IF (FStop) THEN
            STOP 1
         ENDIF
         PMaxEmbInd(IPmax) = NumEmb
         
         READ(URead, '(A1)') AuxVar
         READ(URead, *) PMaxNTramo(IPmax)
         READ(URead, '(A1)') AuxVar
         IF (PMaxNTramo(IPmax) .GT. Dim%PMaxTramo - 1) THEN
            WRITE(6, '(3A, I3, A, I3, A)')                              &
     &           'leecenpmax: Numero tramos muy grande   ',        &
     &           NomCen, ' = ', PMaxNTramo(IPmax), ' > ',                &
     &           Dim%PMaxTramo - 1, '.'
            WRITE(ULog, '(3A, I3, A, I3, A)')                           &
     &           'leecenpmax: Numero tramos muy grande   ',        &
     &           NomCen, ' = ', PMaxNTramo(IPmax), ' > ',                &
     &           Dim%PMaxTramo - 1, '.'
            STOP 1
         ENDIF

         DO ITra = 1, PMaxNTramo(IPmax)
            READ(URead, *)                                               &
     &           (PMaxParam(ITra, IPmax, IPar), IPar = 1, Dim%PMaxParam)
            PMaxParam(ITra, IPmax, PPMaxVol) =                           &
     &           PMaxParam(ITra, IPmax, PPMaxVol)*1.0D3
            PMaxParam(ITra, IPmax, PPMaxPend) =                          &
     &           PMaxParam(ITra, IPmax, PPMaxPend)/1.0D3
         ENDDO
         PMaxParam(PMaxNTramo(IPmax) + 1, IPmax, PPMaxConst) = &
     &        PMaxParam(PMaxNTramo(IPmax), IPmax, PPMaxConst)
         PMaxParam(PMaxNTramo(IPmax) + 1, IPmax, PPMaxPend) = &
     &        PMaxParam(PMaxNTramo(IPmax), IPmax, PPMaxPend)
         PMaxParam(PMaxNTramo(IPmax) + 1, IPmax, PPMaxVol) = GetInfty()

         PMaxNTramo(IPmax) = PMaxNTramo(IPmax) + 1
!     rendimiento inicial, que depende de la cota inicial y por lo
!     tanto es dato.
      ENDDO
      CALL Cerrar(URead)
      RETURN
      END
