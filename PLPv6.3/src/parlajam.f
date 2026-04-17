

!
!     Estructura de parametros del Laja
!
      TYPE PAR_LAJAM

      CHARACTER*24 :: NArcLajaO  = 'plplajam.csv'

      INTEGER :: DimColEta = 30      
      INTEGER :: DimFilaEta = 35
      INTEGER :: DimColBlo = 18
      INTEGER :: DimFilaBlo = 1
      INTEGER :: DimAflHI = 10
      INTEGER :: DimCenRetRiego = 10

      ! Indices relevantes
      
      INTEGER :: IEmbLaja  ! Embalse El Toro      
      REAL(8) :: ScaleVol  ! Escala de El Toro
      INTEGER :: IFiltLaja ! Indice de variable de filtracion de El Toro

      DOUBLE PRECISION, ALLOCATABLE :: VarEtaPrev(:, :, :)
      
      ! Indices de afluentes Hoya Intermedia
      INTEGER NumAflHoyaInt
      INTEGER, ALLOCATABLE :: IAflHoyaInt(:)


      REAL(8) :: VolMaxLaja  ! Volumen Maximo Laja

      INTEGER :: NumColchon ! Numero de colchones del convenio
      REAL(8), ALLOCATABLE :: VolColchon(:)
      REAL(8), ALLOCATABLE :: FactDerRiegoColchon(:)
      REAL(8), ALLOCATABLE :: FactDerElectColchon(:)
      REAL(8), ALLOCATABLE :: FactDerMixtoColchon(:)
      REAL(8), ALLOCATABLE :: FactGasAnticColchon(:)


      REAL(8) :: DerRiegoBase, DerElectBase, DerMixtoBase
      REAL(8) :: DerRiegoMax, DerElectMax, DerMixtoMax, GasAnticMax
      REAL(8) :: DerRiegoIni, DerElectIni, DerMixtoIni, GasAnticIni

      REAL(8) :: MesIniTempRiego, MesIniTempAntic

      REAL(8) :: CaudalMaxRiego, CaudalMaxElect, CaudalMaxMixto, CaudalMaxAntic
      
      REAL(8) :: FactMenMaxRiego(12)
      REAL(8) :: FactMenMaxElect(12)
      REAL(8) :: FactMenMaxMixto(12)
      REAL(8) :: FactMenMaxAntic(12)


      INTEGER NumRetRiego
      INTEGER, ALLOCATABLE :: ICenRetRiego(:)
      INTEGER, ALLOCATABLE :: ICenInyRiego(:)
      REAL(8), ALLOCATABLE :: FRiegoCost(:)
      REAL(8), ALLOCATABLE :: FRiegoPrim(:)
      REAL(8), ALLOCATABLE :: FRiegoNuev(:)
      REAL(8), ALLOCATABLE :: FRiegoEmer(:)
      REAL(8), ALLOCATABLE :: FRiegoSalt(:)

      DOUBLE PRECISION, ALLOCATABLE :: DRExtra(:, :)
      
      REAL(8) :: CaudalDefPrim, CaudalDefNuev
      REAL(8) :: CaudalDefEmer, CaudalDefSalt
      REAL(8) :: QFiltHist

      REAL(8) :: VolMuerto
      
      REAL(8) :: CRiegoNS

      REAL(8), ALLOCATABLE :: CRiegoNSEta(:)

      REAL(8) :: FactMenCRiegoNS(12)
      REAL(8) :: FactMenRiegoPrim(12)
      REAL(8) :: FactMenRiegoNuev(12)
      REAL(8) :: FactMenRiegoEmer(12)
      REAL(8) :: FactMenRiegoSalt(12)

      INTEGER :: NumRetiros
      INTEGER, ALLOCATABLE :: EtaRetiro(:)
      REAL(8), ALLOCATABLE :: RetPrimReg(:) 
      REAL(8), ALLOCATABLE :: RetNuevReg(:)
      REAL(8), ALLOCATABLE :: RetEmerReg(:) 
      REAL(8), ALLOCATABLE :: RetSaltReg(:)

      
      INTEGER :: NumCaudalToro
      REAL(8), ALLOCATABLE :: EtaCaudalToro(:)
      REAL(8), ALLOCATABLE :: CaudalToro(:)
      
      ! Ecuaciones/variables de bloque
      INTEGER :: NumColBlo = 4
      INTEGER :: NumFilBlo = 1

      INTEGER :: IQDR = 1 ! Caudal Generado a cuenta de Derechos de riego
      INTEGER :: IQDE = 2 ! Caudal Generado a cuenta de Derechos Electricos
      INTEGER :: IQDM = 3 ! Caudal Generado a cuenta de Derechos mixto
      INTEGER :: IQGA = 4 ! Caudal Generado a cuenta de Gasto Anticipado
      
      REAL(8) CQVar(4)
      REAL(8) :: FactMenCQVar(4, 12)
      REAL(8), ALLOCATABLE :: CQVarEta(:,:)

      CHARACTER*8, ALLOCATABLE :: VarBloNames(:)

      ! Ecuaciones/variables de etapa
      INTEGER :: NumColEta = 17
      INTEGER :: NumFilEta = 12
      INTEGER :: NumVarEst = 4
      INTEGER :: NumResVol = 4
      INTEGER :: NumVarVol = 4

      INTEGER :: IVDRF  = 1     ! Volumen Derechos Riego 
      INTEGER :: IVDEF  = 2     ! Volumen Derechos Electrico 
      INTEGER :: IVDMF  = 3     ! Volumen Derechos Mixto
      INTEGER :: IVGAF  = 4     ! Volumen Gasto Anticipado Riego
      INTEGER :: IQDRH  = 5     ! Caudal Derechos Riego
      INTEGER :: IQDEH  = 6     ! Caudal Derechos Electrico
      INTEGER :: IQDMH  = 7     ! Caudal Derechos Mixro
      INTEGER :: IQGAH  = 8     ! Caudal Derechos Mixro
      INTEGER :: IQDEFM = 9     ! Deficit minimmo a 
      INTEGER :: IQGTH  = 10    ! Caudal turbinado horario
      INTEGER :: IQRS   = 11    ! Caudal de riego total asegurado
      INTEGER :: IQHI   = 12    ! Caudal hoya intermedia
      INTEGER :: IQPR   = 13    ! Caudal de riego primeros regantes
      INTEGER :: IQNR   = 14    ! Caudal de riego nuevos regantes
      INTEGER :: IQER   = 15    ! Caudal de riego emergencia
      INTEGER :: IQSR   = 16    ! Caudal de riego salto
      INTEGER :: IQLAJA = 17    ! Caudal total laja

!     restricciones
      INTEGER :: IVDRF_F  = 1    ! Volumen Derechos Riegi
      INTEGER :: IVDEF_F  = 2    ! Volumen Derechos Electrico
      INTEGER :: IVDMF_F  = 3    ! Volumen Derechos Mixto
      INTEGER :: IVGAF_F  = 4    ! Volumen Gasto Anticipado
      INTEGER :: IQDRH_F  = 5    ! Caudal horario promedio de riego
      INTEGER :: IQDEH_F  = 6    ! Caudal horario promedio electrico
      INTEGER :: IQDMH_F  = 7     ! Caudal horario promedio mixto
      INTEGER :: IQGAH_F  = 8    ! Caudal horario promedio mixto
      INTEGER :: IQGTH_F  = 9    ! Caudal horario promedio generado
      INTEGER :: IQRSB_F  = 10   ! Balance de riego
      INTEGER :: IQLAJA_F = 11   ! Caudal total del laja
      INTEGER :: IQDRT_F  = 12   ! balance de deficit de riego total


      INTEGER, ALLOCATABLE :: TipoEtaGM(:)

      REAL(8), ALLOCATABLE :: DataEta(:, :)
      REAL(8), ALLOCATABLE :: DataBlo(:, :)

      INTEGER, ALLOCATABLE :: ColchonActivo(:, :)

      
      INTEGER, ALLOCATABLE :: IQRI(:) ! Caudal de riego debloque
      INTEGER, ALLOCATABLE :: IQRIHC(:) ! Caudal de riego retirado horario
      INTEGER, ALLOCATABLE :: IQRDHC(:) ! Caudal de demanda de riego horario
      INTEGER, ALLOCATABLE :: IQRFHC(:) ! Caudal de riego falla horario         

      INTEGER, ALLOCATABLE :: IQRIHF(:) ! Restriccion de riego retirado horario
      INTEGER, ALLOCATABLE :: IQRFHF(:) ! Restriccion de riego falla horario
      INTEGER, ALLOCATABLE :: IQRDHF(:) ! Caudal de demanda de riego horario
      
!     restriccioes opcionales

      LOGICAL, ALLOCATABLE :: VarEtaVol(:)
      CHARACTER*8, ALLOCATABLE :: VarEtaNames(:)

      INTEGER, ALLOCATABLE :: ColIndEta(:, :)
      INTEGER, ALLOCATABLE :: FilIndEta(:, :)


      !  Acoplamiento
      INTEGER :: DimPDLDAcCol = 4
      INTEGER :: DimPDLDAcFila = 4

      INTEGER :: PDLDAcNFila = 4
      INTEGER :: PDLDAcNCol = 4

      END TYPE

