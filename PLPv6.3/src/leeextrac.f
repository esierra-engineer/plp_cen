      SUBROUTINE LeeExtrDim(FExtrac, ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcExtr

      TYPE(PAR_DIMS) Dim
      LOGICAL FExtrac
      INTEGER ULog

      CHARACTER*80 AuxVar
      EXTERNAL Abrir
      INTEGER Abrir
      INTEGER URead
      INTEGER ExtrNCen


      URead = Abrir(NArcExtr, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leeextrac: No existe archivo ',           &
     &        NArcExtr, '.'
         
         Dim%Extr = 0
         FExtrac = .FALSE.
         RETURN
      ENDIF

      FExtrac = .TRUE.

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ExtrNCen

      Dim%Extr = ExtrNCen

      CALL Cerrar(URead)

      RETURN
      END



!**************************************
!     Subrutina Datos de filtraciones de embalses
!**************************************
      SUBROUTINE LeeExtr(NCentral, CenNom,                              &
     &     FExtrac, ExtrNCen, ExtrMax, ExtrCenInd, CenXHid,             &
     &     ULog, Dim)
      USE PLP, ONLY : PAR_DIMS, NArcExtr

      TYPE(PAR_DIMS), INTENT(IN) :: Dim
!
!     Variables Globales
!******************
      CHARACTER*48 CenNom(Dim%Cen)
      INTEGER NCentral

      LOGICAL FExtrac
      INTEGER CenXHid(Dim%Extr)
      INTEGER ExtrNCen
      INTEGER ExtrCenInd(Dim%Extr)
      DOUBLE PRECISION ExtrMax(Dim%Extr)

      INTEGER ULog
!     Variables Locales
!******************
      CHARACTER*48 NomCen
      CHARACTER*48 NomEmb
      CHARACTER*80 AuxVar
      INTEGER Abrir
      INTEGER IExt
      INTEGER NumCen
      INTEGER NumEmb
      INTEGER URead
!
      DOUBLE PRECISION MaxExtr
      LOGICAL FStop
      CHARACTER*42 Objeto

!************************
!     Lee datos plpextrac.dat
!************************
      URead = Abrir(NArcExtr, 'OLD', 'SEQUENTIAL', ULog)
      IF (URead .EQ. 0) THEN
         WRITE(ULog, '(3A)') 'leeextrac: No existe archivo ',           &
     &        NArcExtr, '.'
         
         FExtrac = .FALSE.
         RETURN
      ENDIF

      FExtrac = .TRUE.

      READ(URead, '(A1)') AuxVar
      READ(URead, '(A1)') AuxVar
      READ(URead, *) ExtrNCen
      IF (ExtrNCen .GT. Dim%Extr) THEN
         WRITE(6, '(2A, I3, A, I3, A)')                                 &
     &        'leeextrac: Numero embalses filtraciones  ',              &
     &        ' = ', ExtrNCen, ' > ', Dim%Extr, '.'
         WRITE(ULog, '(2A, I3, A, I3, A)')                              &
     &        'leeextrac: Numero embalses filtraciones  ',              &
     &        ' = ', ExtrNCen, ' > ', Dim%Extr, '.'
         STOP 1
      ENDIF
      DO IExt = 1, ExtrNCen
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomEmb
         READ(URead, '(A1)') AuxVar
!     Filtracion promedio de la etapa 1 (Se pone en la 2da componente).
         READ(URead, *) MaxExtr
         IF (MaxExtr .LT. 0d0) MaxExtr = 0.0d0
         Objeto = 'embalse'
         CALL NomCen2NumCen(NumEmb, FStop, NomEmb,                      &
     &        CenNom, NCentral, Objeto, ULog)
         IF (FStop) THEN
            STOP 1
         ENDIF
!        lee central donde caen las filtraciones
         READ(URead, '(A1)') AuxVar
         READ(URead, *) NomCen
         Objeto = 'central'
         CALL NomCen2NumCen(NumCen, FStop, NomCen,                      &
     &        CenNom, NCentral, Objeto, ULog)
         IF (FStop) THEN
            STOP 1
         ENDIF

         ExtrCenInd(IExt) = NumEmb
         ExtrMax(IExt) = MaxExtr
         CenXHid(IExt) = NumCen
      ENDDO

      CALL Cerrar(URead)
      RETURN
      END
