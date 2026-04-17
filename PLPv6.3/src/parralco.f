

!
!     Estructura de parametros de Ralco
!
      TYPE PAR_RALCO


      INTEGER IEmbRalco
      INTEGER NSegRalco
      DOUBLE PRECISION VCotRalco(10)
      DOUBLE PRECISION ARalco(10)
      DOUBLE PRECISION BRalco(10)

      DOUBLE PRECISION ScaleVol

      ! Ecuaciones/variables de bloque
      INTEGER :: NumColBlo = 0
      INTEGER :: NumFilBlo = 0

      ! Ecuaciones/variables de etapa
      INTEGER :: NumColEta = 1
      INTEGER :: NumFilEta = 2

      INTEGER, ALLOCATABLE :: VRalcoColInd(:)
      INTEGER, ALLOCATABLE :: ColIndEta(:, :)
      INTEGER, ALLOCATABLE :: FilIndEta(:, :)

      ! Varaibles
      INTEGER :: IQEH   = 1 ! Caudal medio horario

      ! Restricciones
      INTEGER :: IQEV_F = 1 ! Restriccion Volumen Caudal
      INTEGER :: IQEH_F = 2 ! Caudal medio horario

      END TYPE
