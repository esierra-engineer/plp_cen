!
!     Estructura de parametros de Gnl
!
      TYPE TermGNL

      CHARACTER*48 Nombre
      INTEGER :: Id
      
      DOUBLE PRECISION :: VMax
      DOUBLE PRECISION :: Vini
      DOUBLE PRECISION :: CGnl
      DOUBLE PRECISION :: CVer
      DOUBLE PRECISION :: CReg
      DOUBLE PRECISION :: CAlm
      DOUBLE PRECISION :: GnlRen
      DOUBLE PRECISION, ALLOCATABLE :: VolInpEta(:)

      ! Ecuaciones/variables de bloque
      INTEGER :: NumColBlo = 0
      INTEGER :: NumFilBlo = 0

      CHARACTER*8, ALLOCATABLE :: VarEtaNames(:)
      DOUBLE PRECISION, ALLOCATABLE :: VolEtaPrev(:,:)

      ! Ecuaciones/variables de etapa
      INTEGER :: NumColEta = 3
      INTEGER :: NumFilEta = 2
      INTEGER :: NumVarEst = 1

      INTEGER :: NumCen = 0
      INTEGER, ALLOCATABLE :: CenInd(:)
      DOUBLE PRECISION, ALLOCATABLE  :: CenRen(:)


      INTEGER, ALLOCATABLE :: VGnlColInd(:)
      INTEGER, ALLOCATABLE :: ColIndEta(:, :)
      INTEGER, ALLOCATABLE :: FilIndEta(:, :)

      DOUBLE PRECISION, ALLOCATABLE :: DataEta(:, :)
      DOUBLE PRECISION, ALLOCATABLE :: DualEta(:, :)

      ! Variables
      INTEGER :: IVGNLF = 1 ! Volumen de GNL final
      INTEGER :: IVGNLE = 2 ! Consumo/insumo de Volumen de GNL de la etapa
      INTEGER :: IVGNLV = 3 ! Volumen vertido de GNL de la etapa

      ! Restricciones
      INTEGER :: IVGNLF_F = 1 ! Volumen de GNL final
      INTEGER :: IVGNLE_F = 2 ! Consumo/insumo de GNL

      INTEGER :: DimColEta = 3
      INTEGER :: DimFilEta = 2

      !  Acoplamiento
      INTEGER :: DimPDLDAcCol = 1
      INTEGER :: DimPDLDAcFila = 1
      INTEGER :: PDLDAcNFila = 0
      INTEGER :: PDLDAcNCol = 0
      LOGICAL :: UsaCorteOptim = .FALSE.
      END TYPE


!
!  Estructura de gnl
!

      TYPE PAR_GNL

      CHARACTER*48 :: NArcGnlO  = 'plpgnl.csv'
      

      INTEGER :: NumTGNL = 0
      TYPE(TermGNL), ALLOCATABLE :: TGNL(:)


      END TYPE


